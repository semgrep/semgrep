(* Cooper Pierce
 *
 * Copyright (c) 2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

module In = Input_to_core_j

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* This module implements the logic for performing extractions dictated by any
   extract mode rules.

   This entails:
    - finding any matches in the provided targets generated by the given
      extract rules
    - combining (or not) these matches depending on the settings in the extract
      rule
    - producing new targets from the combined/processed matches
    - producing a mechanism for the caller to map matches found in the
      generated targets to matches in the original file
*)

let ( let* ) = Option.bind

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* A type for nonempty lists *)
type 'a nonempty = Nonempty of 'a * 'a list

let ( @: ) x (Nonempty (y, xs)) = Nonempty (x, y :: xs)
let nonempty_to_list (Nonempty (x, xs)) = x :: xs

(* from Run_semgrep *)
let mk_rule_table rules =
  let rule_pairs = Common.map (fun r -> (fst r.Rule.id, r)) rules in
  Common.hash_of_list rule_pairs

(** Collects a list into a list of equivalence classes (themselves nonempty
    lists) according to the given equality predicate. `eq` must be an
    equivalence relation for correctness.
*)
let collect eq l =
  List.fold_left
    (fun collected x ->
      match
        List.fold_left
          (fun (checked, to_add) candidate_class ->
            match (to_add, candidate_class) with
            | None, _ -> (candidate_class :: checked, None)
            | Some x, Nonempty (y, _) ->
                if eq x y then ((x @: candidate_class) :: checked, None)
                else (candidate_class :: checked, Some x))
          ([], Some x) collected
      with
      | collected, None -> collected
      | collected, Some new_class -> Nonempty (new_class, []) :: collected)
    [] l

let extract_of_match erule_table match_ =
  Common.find_some_opt
    (fun (x, mvar) ->
      match Hashtbl.find_opt erule_table match_.Pattern_match.rule_id.id with
      | None -> None
      | Some r ->
          let (`Extract { Rule.extract; _ }) = r.Rule.mode in
          if x = extract then Some (r, Some mvar) else Some (r, None))
    match_.Pattern_match.env

type extract_range = {
  (* Offsets from start of file from which the extraction occured *)
  start_line : int;
  start_col : int;
  (* Byte index of start/end *)
  start_pos : int;
  end_pos : int;
}

let count_lines_and_trailing =
  String.fold_left
    (fun (n, c) b ->
      match b with
      | '\n' -> (n + 1, 0)
      | _ -> (n, c + 1))
    (0, 0)

let offsets_of_mval extract_mvalue =
  Metavariable.mvalue_to_any extract_mvalue
  |> Visitor_AST.range_of_any_opt
  |> Option.map (fun (start_loc, end_loc) ->
         let end_len = String.length end_loc.Parse_info.str in
         {
           start_pos = start_loc.charpos;
           (* subtract 1 because lines are 1-indexed, so the
               offset is one less than the current line *)
           start_line = start_loc.Parse_info.line - 1;
           start_col = start_loc.column;
           end_pos = end_loc.charpos + end_len;
         })

let mk_extract_target dst_lang contents rule_ids =
  let dst_lang =
    match dst_lang with
    | Xlang.LGeneric -> "generic"
    | Xlang.LRegex -> "regex"
    | Xlang.L (x, _) -> Lang.show x
  in
  let f : Common.dirname = Common.new_temp_file "extracted" dst_lang in
  Common2.write_file ~file:f contents;
  {
    In.path = f;
    language = dst_lang;
    rule_nums = List.mapi (fun i _ -> i) rule_ids;
  }

(*****************************************************************************)
(* Error reporting *)
(*****************************************************************************)

let report_unbound_mvar ruleid mvar m =
  let { Range.start; end_ } = Pattern_match.range m in
  logger#warning
    "The extract metavariable for rule %s (%s) wasn't bound in a match; \
     skipping extraction for this match [match was at bytes %d-%d]"
    ruleid mvar start end_

let report_no_source_range erule =
  logger#error
    "In rule %s the extract metavariable (%s) did not have a corresponding \
     source range"
    (fst erule.Rule.id)
    (let (`Extract { Rule.extract; _ }) = erule.mode in
     extract)

(*****************************************************************************)
(* Result mapping helpers *)
(*****************************************************************************)

let map_loc pos line col file (loc : Parse_info.token_location) =
  (* this _shouldn't_ be a fake location *)
  {
    loc with
    charpos = loc.charpos + pos;
    line = loc.line + line;
    column = (if loc.line = 1 then loc.column + col else loc.column);
    file;
  }

let map_taint_trace map_loc { Pattern_match.source; tokens; sink } =
  let lift_map_loc f x =
    let token =
      match x.Parse_info.token with
      | Parse_info.OriginTok loc -> Parse_info.OriginTok (f loc)
      | Parse_info.ExpandedTok (pp_loc, v_loc, i) ->
          Parse_info.ExpandedTok (f pp_loc, v_loc, i)
      | x -> x
    in
    { x with token }
  in
  let map_loc = lift_map_loc map_loc in
  let rec map_taint_call_trace trace =
    match trace with
    | Pattern_match.Toks tokens ->
        Pattern_match.Toks (Common.map map_loc tokens)
    | Pattern_match.Call { call_toks; intermediate_vars; call_trace } ->
        Pattern_match.Call
          {
            call_toks = Common.map map_loc call_toks;
            intermediate_vars = Common.map map_loc intermediate_vars;
            call_trace = map_taint_call_trace call_trace;
          }
  in
  {
    Pattern_match.source = map_taint_call_trace source;
    tokens = Common.map map_loc tokens;
    sink = map_taint_call_trace sink;
  }

let map_res map_loc tmpfile file
    (mr : Report.partial_profiling Report.match_result) =
  let matches =
    Common.map
      (fun (m : Pattern_match.t) ->
        {
          m with
          file;
          range_loc = Common2.pair map_loc m.range_loc;
          taint_trace =
            Option.map (Lazy.map_val (map_taint_trace map_loc)) m.taint_trace;
        })
      mr.matches
  in
  let errors =
    Report.ErrorSet.map
      (fun (e : Semgrep_error_code.error) -> { e with loc = map_loc e.loc })
      mr.errors
  in
  let extra =
    match mr.extra with
    | Debug { skipped_targets; profiling } ->
        let skipped_targets =
          Common.map
            (fun (st : Output_from_core_t.skipped_target) ->
              { st with path = (if st.path = tmpfile then file else st.path) })
            skipped_targets
        in
        Report.Debug
          { skipped_targets; profiling = { profiling with Report.file } }
    | Time { profiling } -> Time { profiling = { profiling with Report.file } }
    | No_info -> No_info
  in
  { Report.matches; errors; extra; explanations = [] }

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let extract_and_concat erule_table xtarget rule_ids matches =
  matches
  (* Group the matches within this file by rule id *)
  |> collect (fun m m' -> m.Pattern_match.rule_id = m'.Pattern_match.rule_id)
  |> Common.map (fun matches -> nonempty_to_list matches)
  (* Convert matches to the extract metavariable / bound value *)
  |> Common.map
       (List.filter_map (fun m ->
            match extract_of_match erule_table m with
            | Some ({ mode = `Extract { Rule.extract; _ }; id = id, _; _ }, None)
              ->
                report_unbound_mvar id extract m;
                None
            | Some (r, Some mval) -> Some (r, mval)
            | None -> None))
  (* Factor out rule *)
  |> List.filter_map (function
       | [] -> None
       | (r, _) :: _ as xs -> Some (r, Common.map snd xs))
  (* Convert mval match to offset of location in file *)
  |> Common.map (fun (r, mvals) ->
         ( r,
           List.filter_map
             (fun mval ->
               let offsets = offsets_of_mval mval in
               if Option.is_none offsets then report_no_source_range r;
               offsets)
             mvals ))
  (* For every rule ... *)
  |> Common.map (fun (r, offsets) ->
         (* Sort matches by start position for merging *)
         List.fast_sort (fun x y -> Int.compare x.start_pos y.start_pos) offsets
         |> List.fold_left
              (fun acc curr ->
                match acc with
                | [] -> [ curr ]
                | last :: acc ->
                    (* Keep both when disjoint *)
                    if last.end_pos < curr.start_pos then curr :: last :: acc
                      (* Filter out already contained range; start of
                           last is before start of curr from sorting *)
                    else if curr.end_pos <= last.end_pos then last :: acc
                      (* Merge overlapping ranges *)
                    else { last with end_pos = curr.end_pos } :: acc)
              []
         |> List.rev
         (* Read the extracted text from the source file *)
         |> Common.map (fun { start_pos; start_line; start_col; end_pos } ->
                let source_file = open_in_bin xtarget.Xtarget.file in
                let extract_size = end_pos - start_pos in
                seek_in source_file start_pos;
                let contents = really_input_string source_file extract_size in
                logger#trace
                  "Extract rule %s extracted the following from %s at bytes \
                   %d-%d\n\
                   %s"
                  (fst r.Rule.id) xtarget.file start_pos end_pos contents;
                (contents, map_loc start_pos start_line start_col xtarget.file))
         (* Combine the extracted snippets *)
         |> List.fold_left
              (fun (consumed_loc, contents, map_contents) (snippet, map_snippet) ->
                Buffer.add_string contents snippet;
                let len = String.length snippet in
                let snippet_lines, snippet_trailing =
                  count_lines_and_trailing snippet
                in
                ( {
                    consumed_loc with
                    start_pos = consumed_loc.start_pos + len;
                    start_line = consumed_loc.start_line + snippet_lines;
                    start_col = snippet_trailing;
                  },
                  contents,
                  (* Map results generated by running queries against the temp
                     file to results with ranges corresponding to the position
                     in the original source file.

                     For a tempfile generated from the concatenation of several
                     extracted snippets we accomplish this we by chaining
                     functions together, successively moving the offset along
                     from snippet to snippet if the position to map isn't in
                     the current one

                     In the event that a match spans multiple snippets it will
                     start at the correct start location, but the length with
                     dictate the end, so it may not exactly correspond.
                  *)
                  fun ({ Parse_info.charpos; _ } as loc) ->
                    if charpos < consumed_loc.start_pos then map_contents loc
                    else
                      let line = loc.line - consumed_loc.start_line in
                      map_snippet
                        {
                          loc with
                          charpos = loc.charpos - consumed_loc.start_pos;
                          line;
                          column =
                            (if line = 1 then
                             loc.column - consumed_loc.start_col
                            else loc.column);
                        } ))
              ( { start_pos = 0; end_pos = 0; start_line = 0; start_col = 0 },
                Buffer.create 0,
                fun _ ->
                  (* cannot reach here because charpos of matches
                     cannot be negative and above length starts at 0 *)
                  raise Common.Impossible )
         |> fun (_, buf, map_loc) ->
         let contents = Buffer.contents buf in
         logger#trace
           "Extract rule %s combined matches from %s resulting in the following:\n\
            %s"
           (fst r.Rule.id) xtarget.file contents;
         (* Write out the extracted text in a tmpfile *)
         let (`Extract { Rule.dst_lang; _ }) = r.mode in
         let target = mk_extract_target dst_lang contents rule_ids in
         (target, map_res map_loc target.path xtarget.file))

let extract_as_separate erule_table xtarget rule_ids matches =
  matches
  |> List.filter_map (fun m ->
         match extract_of_match erule_table m with
         | Some (erule, Some extract_mvalue) ->
             (* Note: char/line offset should be relative to the extracted
              * portion, _not_ the whole pattern!
              *)
             let* {
                    start_pos = start_extract_pos;
                    start_line = line_offset;
                    start_col = col_offset;
                    end_pos = end_extract_pos;
                  } =
               match offsets_of_mval extract_mvalue with
               | Some x -> Some x
               | None ->
                   report_no_source_range erule;
                   None
             in
             (* Read the extracted text from the source file *)
             let source_file = open_in_bin m.file in
             let extract_size = end_extract_pos - start_extract_pos in
             seek_in source_file start_extract_pos;
             let contents = really_input_string source_file extract_size in
             logger#trace
               "Extract rule %s extracted the following from %s at bytes %d-%d\n\
                %s"
               m.rule_id.id m.file start_extract_pos end_extract_pos contents;
             (* Write out the extracted text in a tmpfile *)
             let (`Extract { Rule.dst_lang; _ }) = erule.mode in
             let target = mk_extract_target dst_lang contents rule_ids in
             let map_loc =
               map_loc start_extract_pos line_offset col_offset
                 xtarget.Xtarget.file
             in
             Some (target, map_res map_loc target.path xtarget.file)
         | Some ({ mode = `Extract { Rule.extract; _ }; id = id, _; _ }, None)
           ->
             report_unbound_mvar id extract m;
             None
         | None ->
             (* Cannot fail to lookup rule in hashtable just created from rules
                used for query *)
             raise Common.Impossible)

(** This is the main function which performs extraction of the matches
   generated by extract mode rules.

   The resulting extracted regions will be combined appropiate to the rule's
   settings, and (a) target(s) along with a function to translate results back
   to the original file will be produced.
 *)
let extract_nested_lang ~match_hook ~timeout ~timeout_threshold
    (erules : Rule.extract_rule list) xtarget rule_ids =
  let erule_table = mk_rule_table erules in
  let xconf =
    {
      Match_env.config = Config_semgrep.default_config;
      equivs = [];
      matching_explanations = false;
    }
  in
  let res =
    Match_rules.check ~match_hook ~timeout ~timeout_threshold xconf
      (erules :> Rule.rules)
      xtarget
  in
  let separate_matches, combine_matches =
    Common.partition_either
      (fun (m : Pattern_match.t) ->
        let erule = Hashtbl.find erule_table m.rule_id.id in
        let (`Extract { Rule.reduce; _ }) = erule.mode in
        match reduce with
        | Separate -> Left m
        | Concat -> Right m)
      res.matches
  in
  let separate =
    extract_as_separate erule_table xtarget rule_ids separate_matches
  in
  let combined =
    extract_and_concat erule_table xtarget rule_ids combine_matches
  in
  separate @ combined

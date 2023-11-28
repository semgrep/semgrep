(* Cooper Pierce, Yoann Padioleau
 *
 * Copyright (c) 2022-2023 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open File.Operators
open Extract

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* This module implements the logic for performing extractions dictated by
   extract mode rules.

   Note that this module is calling Match_rules.check().

   This entails:
    - finding any matches in the provided targets generated by the given
      extract rules
    - combining (or not) these matches depending on the settings in the extract
      rule
    - producing new targets from the combined/processed matches
    - producing a mechanism for the caller to map matches found in the
      generated targets to matches in the original file (a.k.a., "adjuster")
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type ehrules = (Rule_ID.t, Rule.extract_rule) Hashtbl.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let extract_rule_and_mvar_of_match_opt (ehrules : ehrules) (m : Pattern_match.t)
    : (Rule.extract_rule * Metavariable.mvalue option) option =
  m.Pattern_match.env
  |> Common.find_some_opt (fun (x, mvar) ->
         match Hashtbl.find_opt ehrules m.Pattern_match.rule_id.id with
         | None -> None
         | Some r ->
             let (`Extract { Rule.extract; _ }) = r.Rule.mode in
             if x = extract then Some (r, Some mvar) else Some (r, None))

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
      | __else__ -> (n, c + 1))
    (0, 0)

let offsets_of_mval extract_mvalue =
  Metavariable.mvalue_to_any extract_mvalue
  |> AST_generic_helpers.range_of_any_opt
  |> Option.map (fun ((start_loc : Tok.location), (end_loc : Tok.location)) ->
         let end_len = String.length end_loc.Tok.str in
         {
           start_pos = start_loc.pos.bytepos;
           (* subtract 1 because lines are 1-indexed, so the
               offset is one less than the current line *)
           start_line = start_loc.pos.line - 1;
           start_col = start_loc.pos.column;
           end_pos = end_loc.pos.bytepos + end_len;
         })

let mk_extract_target (dst_lang : Xlang.t) (contents : string) :
    extracted_target =
  let suffix = Xlang.informative_suffix dst_lang in
  let f = Common.new_temp_file "extracted" suffix in
  Common2.write_file ~file:f contents;
  Extracted (Fpath.v f)

(* Unquote string *)
(* TODO: This is not yet implemented *)
let convert_from_unquote_to_string quoted_string =
  logger#error "unquote_string unimplemented";
  quoted_string

(* Unescapes JSON array string *)
let convert_from_json_array_to_string json =
  let json' = "{ \"semgrep_fake_payload\": " ^ json ^ "}" in
  Yojson.Basic.from_string json'
  |> Yojson.Basic.Util.member "semgrep_fake_payload"
  |> Yojson.Basic.Util.to_list
  |> Common.map Yojson.Basic.Util.to_string
  |> String.concat "\n"

(*****************************************************************************)
(* Error reporting *)
(*****************************************************************************)

let report_unbound_mvar (ruleid : Rule_ID.t) mvar m =
  let { Range.start; end_ } = Pattern_match.range m in
  logger#warning
    "The extract metavariable for rule %s (%s) wasn't bound in a match; \
     skipping extraction for this match [match was at bytes %d-%d]"
    (Rule_ID.to_string ruleid) mvar start end_

let report_no_source_range erule =
  logger#error
    "In rule %s the extract metavariable (%s) did not have a corresponding \
     source range"
    (Rule_ID.to_string (fst erule.Rule.id))
    (let (`Extract { Rule.extract; _ }) = erule.mode in
     extract)

(*****************************************************************************)
(* Result mapping helpers *)
(*****************************************************************************)

let map_loc bytepos line col file (loc : Tok.location) =
  (* this _shouldn't_ be a fake location *)
  let pos = loc.pos in
  let pos =
    Pos.make ~file ~line:(pos.line + line)
      ~column:(if pos.line =|= 1 then pos.column + col else pos.column)
      (pos.bytepos + bytepos)
  in
  { loc with pos }

let map_taint_trace map_loc traces =
  let lift_map_loc f x =
    match x with
    | Tok.OriginTok loc -> Tok.OriginTok (f loc)
    | Tok.ExpandedTok (pp_loc, v_loc) -> Tok.ExpandedTok (f pp_loc, v_loc)
    | x -> x
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
  Common.map
    (fun { Pattern_match.source_trace; tokens; sink_trace } ->
      {
        Pattern_match.source_trace = map_taint_call_trace source_trace;
        tokens = Common.map map_loc tokens;
        sink_trace = map_taint_call_trace sink_trace;
      })
    traces

let map_bindings map_loc bindings =
  let map_tokens map_loc mval =
    let mmval =
      AST_generic_helpers.fix_token_locations_any map_loc
        (Metavariable.mvalue_to_any mval)
      |> Metavariable.mvalue_of_any
    in
    match mmval with
    | Some m -> m
    | None -> raise Common.Impossible
  in
  let map_binding (mvar, mval) = (mvar, map_tokens map_loc mval) in
  Common.map map_binding bindings

let map_res map_loc (Extracted tmpfile) (Original file) :
    match_result_location_adjuster =
 fun (mr : Core_result.matches_single_file) : Core_result.matches_single_file ->
  let matches =
    mr.matches
    |> Common.map (fun (m : Pattern_match.t) ->
           {
             m with
             file = !!file;
             range_loc = Common2.pair map_loc m.range_loc;
             taint_trace =
               Option.map (Lazy.map_val (map_taint_trace map_loc)) m.taint_trace;
             env = map_bindings map_loc m.env;
           })
  in
  let errors =
    Core_error.ErrorSet.map
      (fun (e : Core_error.t) -> { e with loc = map_loc e.loc })
      mr.errors
  in
  let extra =
    match mr.extra with
    | Core_profiling.Debug { skipped_targets; profiling } ->
        let skipped_targets =
          Common.map
            (fun (st : Semgrep_output_v1_t.skipped_target) ->
              { st with path = (if st.path =*= tmpfile then file else st.path) })
            skipped_targets
        in
        Core_profiling.Debug
          { skipped_targets; profiling = { profiling with file } }
    | Core_profiling.Time { profiling } ->
        Core_profiling.Time { profiling = { profiling with file } }
    | Core_profiling.No_info -> Core_profiling.No_info
  in
  { Core_result.matches; errors; extra; explanations = [] }

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let extract_and_concat (ehrules : ehrules) (xtarget : Xtarget.t)
    (matches : Pattern_match.t list) : extracted_target_and_adjuster list =
  matches
  (* Group the matches within this file by rule id.
   * TODO? dangerous use of =*= ?
   *)
  |> Common2.group (fun m m' ->
         m.Pattern_match.rule_id =*= m'.Pattern_match.rule_id)
  |> Common.map (fun matches -> Common2.nonempty_to_list matches)
  (* Convert matches to the extract metavariable / bound value *)
  |> Common.map
       (Common.map_filter (fun m ->
            match extract_rule_and_mvar_of_match_opt ehrules m with
            | Some ({ mode = `Extract { Rule.extract; _ }; id = id, _; _ }, None)
              ->
                report_unbound_mvar id extract m;
                None
            | Some (r, Some mval) -> Some (r, mval)
            | None -> None))
  (* Factor out rule *)
  |> Common.map_filter (function
       | [] -> None
       | (r, _) :: _ as xs -> Some (r, Common.map snd xs))
  (* Convert mval match to offset of location in file *)
  |> Common.map (fun (r, mvals) ->
         ( r,
           Common.map_filter
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
                let contents_raw =
                  Common.with_open_infile !!(xtarget.Xtarget.file) (fun chan ->
                      let extract_size = end_pos - start_pos in
                      seek_in chan start_pos;
                      really_input_string chan extract_size)
                in
                (* Convert from JSON to plaintext, if required *)
                let contents =
                  let (`Extract { Rule.transform; _ }) = r.Rule.mode in
                  match transform with
                  | ConcatJsonArray ->
                      convert_from_json_array_to_string contents_raw
                  | Unquote -> convert_from_unquote_to_string contents_raw
                  | __else__ -> contents_raw
                in
                logger#trace
                  "Extract rule %s extracted the following from %s at bytes \
                   %d-%d\n\
                   %s"
                  (Rule_ID.to_string (fst r.Rule.id))
                  !!(xtarget.file) start_pos end_pos contents;
                ( contents,
                  map_loc start_pos start_line start_col !!(xtarget.file) ))
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
                  fun ({ Tok.pos = { bytepos; _ }; _ } as loc) ->
                    if bytepos < consumed_loc.start_pos then map_contents loc
                    else
                      (* For some reason, with the concat_json_string_array option, it needs a fix to point the right line *)
                      (* TODO: Find the reason of this behaviour and fix it properly *)
                      let line =
                        let (`Extract { Rule.transform; _ }) = r.Rule.mode in
                        match transform with
                        | ConcatJsonArray ->
                            loc.pos.line - consumed_loc.start_line - 1
                        | __else__ -> loc.pos.line - consumed_loc.start_line
                      in
                      map_snippet
                        {
                          loc with
                          pos =
                            {
                              loc.pos with
                              bytepos = loc.pos.bytepos - consumed_loc.start_pos;
                              line;
                              column =
                                (if line =|= 1 then
                                   loc.pos.column - consumed_loc.start_col
                                 else loc.pos.column);
                            };
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
           (Rule_ID.to_string (fst r.Rule.id))
           !!(xtarget.file) contents;
         (* Write out the extracted text in a tmpfile *)
         let (`Extract { Rule.dst_lang; _ }) = r.mode in
         let extracted_target = mk_extract_target dst_lang contents in
         let original_target = Original xtarget.file in
         {
           extracted = extracted_target;
           original = original_target;
           analyzer = dst_lang;
           adjuster = map_res map_loc extracted_target original_target;
         })

let extract_as_separate (ehrules : ehrules) (xtarget : Xtarget.t)
    (matches : Pattern_match.t list) : extracted_target_and_adjuster list =
  matches
  |> Common.map_filter (fun m ->
         match extract_rule_and_mvar_of_match_opt ehrules m with
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
             let contents_raw =
               Common.with_open_infile m.file (fun chan ->
                   let extract_size = end_extract_pos - start_extract_pos in
                   seek_in chan start_extract_pos;
                   really_input_string chan extract_size)
             in
             (* Convert from JSON to plaintext, if required *)
             let contents =
               let (`Extract { Rule.transform; _ }) = erule.mode in
               match transform with
               | ConcatJsonArray ->
                   convert_from_json_array_to_string contents_raw
               | Unquote -> convert_from_unquote_to_string contents_raw
               | __else__ -> contents_raw
             in
             logger#trace
               "Extract rule %s extracted the following from %s at bytes %d-%d\n\
                %s"
               (Rule_ID.to_string m.rule_id.id)
               m.file start_extract_pos end_extract_pos contents;
             (* Write out the extracted text in a tmpfile *)
             let (`Extract { Rule.dst_lang; Rule.transform; _ }) = erule.mode in
             let extracted_target = mk_extract_target dst_lang contents in
             let original_target = Original xtarget.file in
             (* For some reason, with the concat_json_string_array option, it
              * needs a fix to point the right line
              * TODO: Find the reason of this behaviour and fix it properly
              *)
             let map_loc =
               match transform with
               | ConcatJsonArray ->
                   map_loc start_extract_pos (line_offset - 1) col_offset
                     !!(xtarget.Xtarget.file)
               | __else__ ->
                   map_loc start_extract_pos line_offset col_offset
                     !!(xtarget.Xtarget.file)
             in
             Some
               {
                 extracted = extracted_target;
                 original = original_target;
                 analyzer = dst_lang;
                 adjuster = map_res map_loc extracted_target original_target;
               }
         | Some ({ mode = `Extract { Rule.extract; _ }; id = id, _; _ }, None)
           ->
             report_unbound_mvar id extract m;
             None
         | None ->
             (* Cannot fail to lookup rule in hashtable just created from rules
                used for query *)
             raise Common.Impossible)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* This is the main function which performs extraction of the matches
   generated by extract mode rules.

   The resulting extracted regions will be combined appropiate to the rule's
   settings, and (a) target(s) along with a function to translate results back
   to the original file will be produced.

   old: was called extract_nested_lang.
*)
let extract ~match_hook ~timeout ~timeout_threshold
    (erules : Rule.extract_rule list) (xtarget : Xtarget.t) :
    extracted_target_and_adjuster list =
  let ehrules = Rule.hrules_of_rules erules in
  let xconf = Match_env.default_xconfig in
  let res =
    (* !! Calling Match_rules !! *)
    Match_rules.check ~match_hook ~timeout ~timeout_threshold xconf
      (erules :> Rule.rules)
      xtarget
  in
  let separate_matches, combine_matches =
    res.matches
    |> Common.partition_either (fun (m : Pattern_match.t) ->
           match Hashtbl.find_opt ehrules m.rule_id.id with
           | Some erule -> (
               let (`Extract { Rule.reduce; _ }) = erule.mode in
               match reduce with
               | Separate -> Left m
               | Concat -> Right m)
           | None -> raise Impossible)
  in
  let separate = extract_as_separate ehrules xtarget separate_matches in
  let combined = extract_and_concat ehrules xtarget combine_matches in
  separate @ combined

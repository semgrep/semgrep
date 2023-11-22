module OutJ = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  Partially translated from nosemgrep.py

  See https://semgrep.dev/docs/ignoring-files-folders-code/ for documentation
  about nosemgrep.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* python: All the regexps were in constants.py before, but
 * better separation of concern to put it here.
 *)

let rule_id_re_str = {|(?:[:=][\s]?(?P<ids>([^,\s](?:[,\s]+)?)+))?|}

(*
   Inline 'noqa' implementation modified from flake8:
   https://github.com/PyCQA/flake8/blob/master/src/flake8/defaults.py
   We're looking for items that look like this:
   ' nosem'
   ' nosemgrep: example-pattern-id'
   ' nosem: pattern-id1,pattern-id2'
   ' NOSEMGREP:pattern-id1,pattern-id2'

   * We do not want to capture the ': ' that follows 'nosem'
   * We do not care about the casing of 'nosem'
   * We want a comma-separated list of ids
   * We want multi-language support, so we cannot strictly look for
     Python comments that begin with '# '
   * nosem and nosemgrep should be interchangeable
*)
let nosem_inline_re_str = {| nosem(?:grep)?|} ^ rule_id_re_str
let nosem_inline_re = SPcre.regexp nosem_inline_re_str ~flags:[ `CASELESS ]

(*
   A nosemgrep comment alone on its line.
   Since we don't know the comment syntax for the particular language, we
   assume it's enough that there isn't any word or number character before
   'nosemgrep'.
   The following will not match:
     hello(); // nosemgrep
     + 42 // nosemgrep
   The following will match:
     # nosemgrep
     print('nosemgrep');
*)
let nosem_previous_line_re =
  SPcre.regexp
    ({|^[^a-zA-Z0-9]* nosem(?:grep)?|} ^ rule_id_re_str)
    ~flags:[ `CASELESS ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
   Try to recognise the [rex] (a regex) into the given [line] and returns an
   array IDs (["ids"]) collected during this recognition.
*)
let recognise_and_collect ~rex (line_num, line) =
  SPcre.exec_all ~rex line
  |> Result.map
       (Array.map (fun subst ->
            match SPcre.get_named_substring_and_ofs rex "ids" subst with
            | Ok (Some (s, (begin_ofs, _end_ofs))) ->
                Some (line_num, s, begin_ofs)
            | Ok None
            | Error _ ->
                (* TODO: log something? *)
                None))
  |> Result.to_option

(*
   Try to recognize a possible [nosem] tag into the given [match].
   If [strict:true], we returns possible errors when [nosem] is used with an
   ID which is not equal to the rule's ID.
*)
let rule_match_nosem (pm : Pattern_match.t) : bool * Core_error.t list =
  let lines =
    (* Minus one, because we need the preceding line. *)
    let start, end_ = pm.range_loc in
    let start_line = max (start.pos.line - 1) 1 in
    let end_line = end_.pos.line in
    File.lines_of_file (start_line, end_line) (Fpath.v pm.file)
    |> Common.mapi (fun idx x -> (start_line + idx, x))
  in

  let path = pm.file in
  let linecol_to_bytepos_fun =
    (Pos.full_converters_large path).linecol_to_bytepos_fun
  in

  let previous_line, line =
    match lines with
    | line0 :: line1 :: _ when (fst pm.range_loc).pos.line > 0 ->
        (Some line0, Some line1)
    | line :: _ -> (None, Some line)
    | [] (* XXX(dinosaure): is it possible? *) -> (None, None)
  in

  let no_ids = Array.for_all Option.is_none in

  let ids_line =
    match line with
    | None -> None
    | Some line -> recognise_and_collect ~rex:nosem_inline_re line
  in
  let ids_previous_line =
    match previous_line with
    | None -> None
    | Some previous_line ->
        recognise_and_collect ~rex:nosem_previous_line_re previous_line
  in

  match
    ( Option.value ~default:[||] ids_line,
      Option.value ~default:[||] ids_previous_line )
  with
  | [||], [||] ->
      (* no lines or no [nosemgrep] occurrences found, keep the [rule_match]. *)
      (false, [])
  | ids_line, ids_previous_line when no_ids ids_line && no_ids ids_previous_line
    ->
      (* [nosemgrep] occurrences found but no [ids]. *)
      (true, [])
  | ids_line, ids_previous_line ->
      let ids = Array.append ids_line ids_previous_line in
      let ids =
        Array.to_list ids |> Common.map_filter Fun.id
        |> Common.map (fun (line_num, s, col) ->
               (* [String.split_on_char] can **not** return an empty list. *)
               ( line_num,
                 List.hd (String.split_on_char ' ' s) (* nosemgrep: list-hd *),
                 col ))
      in
      (* check if the id specified by the user is the [rule_match]'s [rule_id]. *)
      let nosem_matches id =
        (* TODO: id should be a Rule_ID.t too *)
        let res =
          Rule_ID.ends_with pm.rule_id.id ~suffix:(Rule_ID.of_string id)
        in
        res
      in

      List.fold_left
        (fun (result, errors) (line_num, id, col) ->
          (* strip quotes from the beginning and end of the id. this allows
             use of nosem as an HTML attribute inside tags.
             HTML comments inside tags are not allowed by the spec.
          *)
          let id = Common2.strip '"' id in
          let loc =
            Tok.
              {
                str = id;
                pos =
                  Pos.
                    {
                      bytepos = linecol_to_bytepos_fun (line_num, col);
                      line = line_num;
                      column = col;
                      file = path;
                    };
              }
          in
          let errors =
            (* If the rule-id is 'foo.bar.my-rule' we accept 'foo.bar.my-rule' as well as
             * any suffix of it such as 'my-rule' or 'bar.my-rule'. *)
            if not (nosem_matches id) then
              let msg =
                Format.asprintf
                  "found 'nosem' comment with id '%s', but no corresponding \
                   rule trying '%s'"
                  id
                  (Rule_ID.to_string pm.rule_id.id)
              in
              let core_error =
                {
                  Core_error.rule_id = None;
                  typ = SemgrepWarning;
                  msg;
                  loc;
                  details = None;
                }
              in
              core_error :: errors
            else errors
          in
          (nosem_matches id || result, errors))
        (false, []) ids

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let produce_ignored (matches : Core_result.processed_match list) :
    Core_result.processed_match list * Core_error.t list =
  (* filters [rule_match]s by the [nosemgrep] tag. *)
  let matches, wide_errors =
    Common.map
      (fun (pm : Core_result.processed_match) ->
        let is_ignored, errors = rule_match_nosem pm.pm in
        ({ pm with is_ignored }, errors))
      matches
    |> List.split
  in
  (matches, List.concat wide_errors)

let filter_ignored ~keep_ignored (matches : OutJ.core_match list) =
  matches
  |> List.filter (fun (m : OutJ.core_match) ->
         keep_ignored || not m.extra.is_ignored)

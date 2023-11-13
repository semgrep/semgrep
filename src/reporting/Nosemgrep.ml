module Out = Semgrep_output_v1_j

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
            | Ok (Some (s, ofs)) -> Some (line_num, s, ofs)
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
let rule_match_nosem ~strict (rule_match : Out.core_match) :
    bool * Out.core_error list =
  let lines =
    (* Minus one, because we need the preceding line. *)
    let start_line = rule_match.Out.start.line - 1 in
    let end_line = rule_match.Out.end_.line in
    File.lines_of_file (start_line, end_line) rule_match.Out.path
    |> Common.mapi (fun idx x -> (start_line + idx, x))
  in

  let path = rule_match.path in
  let linecol_to_bytepos_fun =
    (Pos.full_converters_large (Fpath.to_string path)).linecol_to_bytepos_fun
  in

  let previous_line, line =
    match lines with
    | line0 :: line1 :: _ when rule_match.Out.start.line > 0 ->
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

  match (ids_line, ids_previous_line) with
  | None, None
  | Some [||], Some [||] ->
      (* no lines or no [nosemgrep] occurrences found, keep the [rule_match]. *)
      (false, [])
  | Some ids_line, Some ids_previous_line
    when no_ids ids_line && no_ids ids_previous_line ->
      (* [nosemgrep] occurrences found but no [ids]. *)
      (true, [])
  | __else__ ->
      let ids =
        Array.append
          (Option.value ~default:[||] ids_line)
          (Option.value ~default:[||] ids_previous_line)
      in
      let ids =
        Array.to_list ids |> Common.map_filter Fun.id
        |> Common.map (fun (line_num, s, ids) ->
               (* [String.split_on_char] can **not** return an empty list. *)
               ( line_num,
                 List.hd (String.split_on_char ' ' s) (* nosemgrep: list-hd *),
                 ids ))
      in
      (* check if the id specified by the user is the [rule_match]'s [rule_id]. *)
      let nosem_matches id =
        (* TODO: id should be a Rule_ID.t too *)
        Rule_ID.ends_with rule_match.Out.check_id ~suffix:(Rule_ID.of_string id)
      in
      List.fold_left
        (fun (result, errors) (line_num, id, (col1, col2)) ->
          let location =
            let start =
              Out.
                {
                  line = line_num;
                  col = col1;
                  offset = linecol_to_bytepos_fun (line_num, col1);
                }
            in
            let end_ =
              Out.
                {
                  line = line_num;
                  col = col2;
                  offset = linecol_to_bytepos_fun (line_num, col2);
                }
            in
            Out.{ path; start; end_ }
          in
          let errors =
            (* If the rule-id is 'foo.bar.my-rule' we accept 'foo.bar.my-rule' as well as
             * any suffix of it such as 'my-rule' or 'bar.my-rule'. *)
            if strict && not (nosem_matches id) then
              let message =
                Format.asprintf
                  "found 'nosem' comment with id '%s', but no corresponding \
                   rule trying '%s'"
                  id
                  (Rule_ID.to_string rule_match.Out.check_id)
              in
              let core_error : Out.core_error =
                {
                  Out.rule_id = None;
                  error_type = SemgrepError;
                  severity = `Warning;
                  message;
                  location;
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

let process_ignores ~keep_ignored ~strict (out : Out.core_output) :
    Out.core_output =
  let results, errors =
    (* filters [rule_match]s by the [nosemgrep] tag. *)
    Common.map_filter
      (fun (rule_match : Out.core_match) ->
        let is_ignored, errors = rule_match_nosem ~strict rule_match in
        let rule_match =
          { rule_match with extra = { rule_match.extra with is_ignored } }
        in
        if not is_ignored then Some (rule_match, errors)
        else if keep_ignored then Some (rule_match, errors)
        else None)
      out.Out.results
    |> List.split
  in
  { out with results; errors = List.concat (out.Out.errors :: errors) }

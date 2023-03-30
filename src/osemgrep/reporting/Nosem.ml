open Common
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  Partially translated from nosemgrep.py
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
let recognise_and_collect ~rex line =
  SPcre.exec_all ~rex line
  |> Result.map
       (Array.map (fun subst ->
            try Some (Pcre.get_named_substring rex "ids" subst) with
            | _ -> None))
  |> Result.to_option

(*
   Try to recognize a possible [nosem] tag into the given [match].
   If [strict:true], we returns possible errors when [nosem] is used with an
   ID which is not equal to the rule's ID.
*)
let rule_match_nosem ~strict (rule_match : Out.cli_match) :
    bool * Out.cli_error list =
  let lines =
    File.lines_of_file
      (max 0 (rule_match.Out.start.line - 1), rule_match.Out.end_.line)
      (Fpath.v rule_match.Out.path)
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
      let ids = List.filter_map Fun.id (Array.to_list ids) in
      (* check if the id specified by the user is the [rule_match]'s [rule_id]. *)
      List.fold_left
        (fun (result, errors) id ->
          let errors =
            if strict && id <> rule_match.Out.check_id then
              let msg =
                Format.asprintf
                  "found 'nosem' comment with id '%s', but no corresponding \
                   rule trying '%s'"
                  id rule_match.Out.check_id
              in
              let cli_error : Out.cli_error =
                {
                  Out.code = 0;
                  level =
                    "warn"
                    (* XXX(dinosaure): use [Severity.string_of_basic_severity]? *);
                  type_ = "nosemgrep error" (* TODO(dinosaure): correct? *);
                  rule_id = Some rule_match.Out.check_id;
                  message = Some msg;
                  path = Some rule_match.Out.path;
                  long_msg = None;
                  short_msg = Some msg;
                  spans = None;
                  help = None;
                }
              in
              cli_error :: errors
            else errors
          in
          (id = rule_match.Out.check_id || result, errors))
        (true, []) ids

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let process_ignores ~strict (out : Out.cli_output) : Out.cli_output =
  let results, errors =
    (* filters [rule_match]s by the [nosemgrep] tag. *)
    List.filter_map
      (fun rule_match ->
        let to_ignore, errors = rule_match_nosem ~strict rule_match in
        if not to_ignore then Some (rule_match, errors) else None)
      out.Out.results
    |> List.split
  in
  { out with results; errors = List.concat (out.Out.errors :: errors) }

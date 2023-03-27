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
let _nosem_inline_re = SPcre.regexp nosem_inline_re_str ~flags:[ `CASELESS ]

(*
   As a hack adapted from semgrep-agent,
   we assume comment markers are one of these special characters
*)
let _nosem_inline_comment_re =
  SPcre.regexp (spf {|[:#/]+%s$|} nosem_inline_re_str) ~flags:[ `CASELESS ]

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
let _nosem_previous_line_re =
  SPcre.regexp
    ({|^[^a-zA-Z0-9]* nosem(?:grep)?|} ^ rule_id_re_str)
    ~flags:[ `CASELESS ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* TODO: probably need to pass some flags from the CLI *)
let process_ignores (out : Out.cli_output) : Out.cli_output =
  (* TODO: port nosemgrep.py, filter in out.matches the
     matches where the match has a corresponding nosem: annotation in
     the corresponding file, and add new errors if nosem: has the wrong
     format in the file.
  *)
  out

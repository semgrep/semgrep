(* python: was in constants.py, but those are type defs, not constants ...
 *
 * LATER: factorize code with Matching_report.match_format
 * and Runner_config.output_format.
 *)
type t =
  | Text
  | Json
  | Gitlab_sast
  | Gitlab_secrets
  | Junit_xml
  | Sarif
  | Emacs
  | Vim
  (* used to disable the final display of match results because
   * we displayed them incrementally instead
   *)
  | TextIncremental
[@@deriving show]

let _output_format_is_json = function
  | Json
  | Sarif ->
      true
  | Text
  | Gitlab_sast
  | Gitlab_secrets
  | Junit_xml
  | Emacs
  | Vim
  | TextIncremental ->
      false

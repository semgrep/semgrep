(* python: was in constants.py, but those are type defs, not constants ...
 *
 * LATER: factorize code with Matching_report.match_format
 * and Runner_config.output_format.
 *)
type t =
  | Text
  | Json
  | Emacs
  | Vim
  | Sarif
  | Gitlab_sast
  | Gitlab_secrets
  | Junit_xml
  (* used to disable the final display of match results because
   * we displayed them incrementally instead
   *)
  | TextIncremental
[@@deriving show]

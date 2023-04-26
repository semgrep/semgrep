type t = Semgrep_output_v1_t.core_match * Rule.rule
(** Match and corresponding rule *)

val of_matches :
  ?only_git_dirty:bool ->
  Pattern_match.t list ->
  Rule.hrules ->
  string list ->
  (t list * string list) Lwt.t
(** Postprocess semgrep run results given rules and files scanned, populating fixes, and messages. Filters out matches depending on git status *)

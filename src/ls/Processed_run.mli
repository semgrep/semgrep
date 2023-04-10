type t = Semgrep_output_v1_t.core_match * Rule.rule

val of_matches :
  ?only_git_dirty:bool ->
  Pattern_match.t list ->
  Rule.hrules ->
  string list ->
  (t list * string list) Lwt.t

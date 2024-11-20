type t = Reachable | Undetermined | Unreachable

val string_of : t -> string
val of_cli_match_opt : Semgrep_output_v1_t.cli_match -> t option

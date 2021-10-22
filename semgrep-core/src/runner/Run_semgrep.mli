open Runner_common

val semgrep_with_patterns_file : config -> string list -> unit

val semgrep_with_rules_file : config -> string list -> unit

val semgrep_with_one_pattern : config -> string list -> unit

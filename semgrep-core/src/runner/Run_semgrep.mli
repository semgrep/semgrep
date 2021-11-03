open Runner_common

(*
   Copy named pipes created with <(echo 'foo') on the command line
   into a regular file to avoid illegal seeks when reporting match results
   or parsing errors.
*)
val replace_named_pipe_by_regular_file : Common.filename -> Common.filename

val parse_pattern : Lang.t -> string -> AST_generic.any

val semgrep_with_patterns_file : config -> string list -> unit

(*
   Run semgrep and return (success, result, targets).
   The targets are all the files that were considered valid targets for the
   semgrep scan. This excludes files that were filtered out on purpose
   due to being in the wrong language, too big, etc.
   It includes targets that couldn't be scanned, for instance due to
   a parsing error.
*)
val run_semgrep_with_rules :
  config -> string list -> exn option * Report.rule_result * string list

val semgrep_with_rules_file : config -> string list -> unit

val semgrep_with_one_pattern : config -> string list -> unit

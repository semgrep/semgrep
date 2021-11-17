val run_semgrep_with_rules :
  Runner_common.config ->
  Common.filename list ->
  exn option * Report.rule_result * Common.filename list
(** [run_semgrep_with_rules config roots] runs the semgrep engine
    starting from a list of [roots] and returns (success, result, targets).
    The targets are all the files that were considered valid targets for the
    semgrep scan. This excludes files that were filtered out on purpose
    due to being in the wrong language, too big, etc.
    It includes targets that couldn't be scanned, for instance due to
    a parsing error.
*)

val semgrep_with_rules_file :
  Runner_common.config -> Common.filename list -> unit
(** [semgrep_with_rules_file config roots] calls [run_semgrep_with_rules] and
    format the results on stdout either in a JSON or Textual format
    (depending on the value in config.output_format)
*)

val semgrep_with_one_pattern :
  Runner_common.config -> Common.filename list -> unit
(** this is the function used when running semgrep with -e or -f *)

(* internal functions *)

val replace_named_pipe_by_regular_file : Common.filename -> Common.filename
(**
   Copy named pipes created with <(echo 'foo') on the command line
   into a regular file to avoid illegal seeks when reporting match results
   or parsing errors.
   Used outside Run_semgrep in Main.ml for -dump_pattern.
*)

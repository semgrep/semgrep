(* There is currently no 'semgrep validate' subcommand. Rule validations are run via
 * 'semgrep scan --validate ...' but internally it's quite similar to
 * a subcommand.
 *)

(* a slice of Scan_CLI.conf *)
type conf = {
  rules_source : Rule_fetching.rules_source;
  core_runner_conf : Core_runner.conf;
  logging_level : Logs.level option;
}

(* LATER: at some point we may want a Validate_CLI.conf instead of
 * abusing Scan_CLI.conf *)
val run : conf -> Exit_code.t

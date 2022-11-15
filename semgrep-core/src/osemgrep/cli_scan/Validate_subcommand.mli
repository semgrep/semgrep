(* There is currently no 'semgrep validate' subcommand. Rule validations are run via
 * 'semgrep scan --validate ...' but internally it's quite similar to
 * a subcommand.
 *)

(* LATER: at some point we may want a Validate_CLI.conf instead of
 * abusing Scan_CLI.conf *)
val run : Scan_CLI.conf -> Exit_code.t

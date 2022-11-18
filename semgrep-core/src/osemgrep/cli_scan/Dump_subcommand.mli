(* There is currently no 'semgrep dump' subcommand. Dumps are run via
 * 'semgrep scan --dump-ast ...' but internally it's quite similar to
 * a subcommand.
 *)

(* LATER: at some point we may want a Dump_CLI.conf instead of
 * abusing Scan_CLI.conf *)
val run : Scan_CLI.conf -> Exit_code.t

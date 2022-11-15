(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* There is currently no 'semgrep test' subcommand. Tests are run via
 * 'semgrep scan --test ...' but internally it's quite similar to
 * a subcommand.
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* LATER: at some point we may want a Test_CLI.conf instead of
 * abusing Scan_CLI.conf *)
let run (_conf : Scan_CLI.conf) : Exit_code.t = failwith "TODO"

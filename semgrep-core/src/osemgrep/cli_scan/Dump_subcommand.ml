(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* There is currently no 'semgrep dump' subcommand. Dumps are run via
 * 'semgrep scan --dump-ast ...' but internally it's quite similar to
 * a subcommand.
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* LATER: at some point we may want a Dump_CLI.conf instead of
 * abusing Scan_CLI.conf *)
let run (_conf : Scan_CLI.conf) : Exit_code.t = failwith "TODO"
(* TOPORT:
   dump_parsed_ast(json, __validate_lang("--dump-ast", lang), pattern, targets)
*)

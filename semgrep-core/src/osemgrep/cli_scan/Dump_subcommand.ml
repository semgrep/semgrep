(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* There is currently no 'semgrep dump' subcommand. Dumps are run via
 * 'semgrep scan --dump-ast ...' but internally it's quite similar to
 * a subcommand.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* a slice of Scan_CLI.conf *)
type conf = { language : Lang.t; json : bool; target : target_kind }

and target_kind = Pattern of string | File of Common.filename
[@@deriving show]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run (_conf : conf) : Exit_code.t = failwith "TODO"
(* TOPORT:
   dump_parsed_ast(json, __validate_lang("--dump-ast", lang), pattern, targets)
*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* There is currently no 'semgrep test' subcommand. Tests are run via
 * 'semgrep scan --test ...' but internally it's quite similar to
 * a subcommand.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type conf = {
  target : target_kind;
  ignore_todo : bool;
  (* TODO? do we need those options? people use the JSON output?
   * the playground? and the optimizations and strict?
   *)
  json : bool;
  (* take the whole core_runner_conf? like for validate? *)
  optimizations : bool;
  strict : bool;
}

(* alt: we could accept multiple dirs, and multiple files *)
and target_kind =
  | Dir of Common.filename * string option (* optional --config *)
  | File of Common.filename * string (* mandatory --config *)
[@@deriving show]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run (_conf : conf) : Exit_code.t = failwith "TODO"

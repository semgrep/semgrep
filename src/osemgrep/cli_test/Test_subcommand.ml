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

(* alt: we could accept multiple dirs, and multiple files
 * TODO? should we restrict the config_str to File or Dir?
 *)
and target_kind =
  | Dir of Fpath.t * Rules_config.config_string option (* optional --config *)
  | File of Fpath.t * Rules_config.config_string (* mandatory --config *)
[@@deriving show]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run (_conf : conf) : Exit_code.t = failwith "TODO: Test_subcommand.run"

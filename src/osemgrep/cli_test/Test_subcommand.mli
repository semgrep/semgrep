(* There is currently no 'semgrep test' subcommand. Tests are run via
 * 'semgrep scan --test ...' but internally it's quite similar to
 * a subcommand.
 *)

type conf = {
  target : target_kind;
  ignore_todo : bool;
  json : bool;
  optimizations : bool;
  strict : bool;
}

and target_kind =
  | Dir of Fpath.t * Rules_config.config_string option (* optional --config *)
  | File of Fpath.t * Rules_config.config_string (* mandatory --config *)
[@@deriving show]

val run : conf -> Exit_code.t

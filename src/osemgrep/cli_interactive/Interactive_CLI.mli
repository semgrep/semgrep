(*
   'semgrep interactive' command-line parsing.
*)

(*
   The result of parsing a 'semgrep interactive' command.
*)

(* a subset of Scan_CLI.conf *)
type conf = {
  lang : Lang.t; (* use Xlang.t at some point? or even Xlang option? *)
  target_roots : Fpath.t list;
  targeting_conf : Find_targets.conf;
  core_runner_conf : Core_runner.conf;
  (* nosem? *)
  logging_level : Logs.level option;
}
[@@deriving show]

(*
   Usage: parse_argv [| "semgrep-interactive"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf

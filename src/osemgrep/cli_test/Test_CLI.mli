(*
   'semgrep test' command-line parsing.
*)

(* The result of parsing a 'semgrep test' command. This is also used in
 * Scan_CLI.ml to transform legacy commands such as 'semgrep scan --tests <dir>'
 * into the new 'semgrep test <dir>'
 *)
type conf = {
  target : target_kind;
  pro : bool;
  ignore_todo : bool;
  json : bool;
  optimizations : bool;
  strict : bool;
  (* Whether to emit "matching diagnosis", which analyzes failing
     test annotation cases and matching explanations to determine
     why a rule did or did not match.
  *)
  matching_diagnosis : bool;
  common : CLI_common.conf;
}

and target_kind =
  | Dir of Fpath.t * Rules_config.config_string option (* optional --config *)
  | File of Fpath.t * Rules_config.config_string (* mandatory --config *)
[@@deriving show]

(*
   Usage: parse_argv [| "semgrep-test"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf

(* used also in cli_scan for --tests *)
val o_test_ignore_todo : bool Cmdliner.Term.t

val target_kind_of_roots_and_config :
  Fpath.t list -> Rules_config.config_string list -> target_kind

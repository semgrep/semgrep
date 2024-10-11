(*
   'semgrep show' command-line parsing.
*)

(*
   The result of parsing a 'semgrep show' command.
   This is also used in Scan_CLI.ml to transform legacy
   commands such as 'semgrep scan --show-supported-languages' into the
   new 'semgrep show supported-languages'
*)
type conf = {
  common : CLI_common.conf;
  (* mix of --dump-ast/--dump-rule/... *)
  show_kind : show_kind;
  json : bool;
}

and show_kind =
  | Version
  | SupportedLanguages
  | Identity
  | Deployment
  (* dumpers *)
  | DumpPattern of string * Lang.t
  | DumpCST of Fpath.t * Lang.t
  | DumpAST of Fpath.t * Lang.t
  | DumpConfig of Rules_config.config_string
  | DumpRuleV2 of Fpath.t
  | DumpEnginePath of bool (* pro = true *)
  | DumpCommandForCore
[@@deriving show]

(*
   Usage: parse_argv [| "semgrep-show"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf

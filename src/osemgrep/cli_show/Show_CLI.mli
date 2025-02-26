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
  | Debug of debug_settings  (** Open an interactive debugging view. *)

and debug_settings = {
  output_dir : Fpath.t;
      (** Directory to save the output to. If not specified on command line,
          defaults to a temporary directory. *)
  targeting_conf : Find_targets.conf;
      (** Configuration for finding target files to debug. Built from the root
          argument with default settings. *)
  rules_source : Rules_source.t;
      (** Configuration for rules to use when debugging *)
}
[@@deriving show]

(*
   Usage: parse_argv [| "semgrep-show"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : < Cap.tmp ; .. > -> string array -> conf

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
  (* mix of --dump-ast/--dump-rule/... *)
  target : target_kind;
  json : bool;
}

and target_kind =
  | Pattern of string * Lang.t
  | File of Fpath.t * Lang.t
  | Config of Rules_config.config_string
  | EnginePath of bool (* pro = true *)
  | CommandForCore
  | SupportedLanguages
[@@deriving show]

(*
   Usage: parse_argv [| "semgrep-show"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf

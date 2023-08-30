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
  (* alt: we could accept XLang.t to dump extended patterns *)
  | Pattern of string * Lang.t
  (* alt: we could accept multiple Files via multiple target_roots *)
  | File of Fpath.t * Lang.t
  | Config of Semgrep_dashdash_config.config_string
  (* LATER: get rid of it? *)
  | EnginePath of bool (* pro = true *)
  (* LATER: get rid of it *)
  | CommandForCore
[@@deriving show]

(*
   Usage: parse_argv [| "semgrep-show"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf

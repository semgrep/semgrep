module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep show' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
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

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)

(* ------------------------------------------------------------------ *)
(* Flags *)
(* ------------------------------------------------------------------ *)

let o_json : bool Term.t =
  let info = Arg.info [ "json" ] ~doc:{|Output results in JSON format.|} in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* Positional arguments *)
(* ------------------------------------------------------------------ *)
let o_args : string list Term.t =
  let info =
    Arg.info [] ~docv:"STRINGS"
      ~doc:{|Commands used to show internal information.|}
  in
  Arg.value (Arg.pos_all Arg.string [] info)

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)
let cmdline_term : conf Term.t =
  (* !The parameters must be in alphabetic orders to match the order
   * of the corresponding '$ o_xx $' further below! *)
  let combine _args _json = failwith "TODO" in
  Term.(const combine $ o_args $ o_json)

let doc = "Show various information"

let man : Cmdliner.Manpage.block list =
  [ `S Cmdliner.Manpage.s_description; `P "Display various information" ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep show" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd

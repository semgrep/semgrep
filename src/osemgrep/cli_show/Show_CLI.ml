open Common
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

(* coupling: if you add a command you probably need to modify [combine]
 * below and also the doc in [man] further below
 *)
and target_kind =
  (* 'semgrep show ???'
   * accessible also as 'semgrep scan --dump-ast -e <pattern>'
   * alt: we could accept XLang.t to dump extended patterns *)
  | Pattern of string * Lang.t
  (* 'semgrep show ???'
   * accessible also as 'semgrep scan --lang <lang> --dump-ast <target>
   * alt: we could accept multiple Files via multiple target_roots *)
  | File of Fpath.t * Lang.t
  (* 'semgrep show dump-config <config_str>' *)
  | Config of Rules_config.config_string
  (* 'semgrep show ???'
   * accessible also as 'semgrep scan --dump-engine-path
   * LATER: get rid of it? *)
  | EnginePath of bool (* pro = true *)
  (* 'semgrep show ???'
   * accessible also as 'semgrep scan --dump-command-for-core' (or just '-d')
   * LATER: get rid of it *)
  | CommandForCore
  (* 'semgrep show supported-languages'
   * accessible also as `semgrep scan --show-supported-languages
   *)
  | SupportedLanguages
  | Identity
  | Deployment
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
  let combine args json =
    let target =
      (* coupling: if you add a command here, update also the man page
       * further below
       *)
      match args with
      | [ "dump-config"; config_str ] -> Config config_str
      | [ "supported-languages" ] -> SupportedLanguages
      | [ "identity" ] -> Identity
      | [ "deployment" ] -> Deployment
      | _ ->
          Error.abort
            (spf "show command not supported: %s" (String.concat " " args))
    in
    { target; json }
  in

  Term.(const combine $ o_args $ o_json)

let doc = "Show various information"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P "Display various information";
    `P "Here are the different subcommands";
    `Pre "semgrep show dump-config <STRING>";
    `P "Dump the internal representation of the result of --config=<STRING>";
    `Pre "semgrep show supported-languages";
    (* coupling: Scan_CLI.o_show_supported_languages help *)
    `P "Print a list of languages that are currently supported by Semgrep.";
    `Pre "semgrep show deployment";
    `P "Print the current logged-in deployment";
    `Pre "semgrep show identity";
    `P "Print the current logged-in token identity";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep show" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd

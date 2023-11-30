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
  show_kind : show_kind;
  json : bool;
  logging_level : Logs.level option;
}

(* coupling: if you add a command you probably need to modify [combine]
 * below and also the doc in [man] further below
 *)
and show_kind =
  | Version
  (* 'semgrep show supported-languages'
   * accessible also as `semgrep scan --show-supported-languages
   *)
  | SupportedLanguages
  (* a.k.a whoami *)
  | Identity
  | Deployment
  | Bucket
  (* 'semgrep show ???'
   * accessible also as 'semgrep scan --dump-ast -e <pattern>'
   * alt: we could accept XLang.t to dump extended patterns *)
  | DumpPattern of string * Lang.t
  (* 'semgrep show ???'
   * accessible also as 'semgrep scan --lang <lang> --dump-ast <target>
   * alt: we could accept multiple Files via multiple target_roots *)
  | DumpAST of Fpath.t * Lang.t
  | DumpConfig of Rules_config.config_string
  (* 'semgrep show ???'
   * accessible also as 'semgrep scan --dump-engine-path
   * LATER: get rid of it? *)
  | DumpEnginePath of bool (* pro = true *)
  (* 'semgrep show ???'
   * accessible also as 'semgrep scan --dump-command-for-core' (or just '-d')
   * LATER: get rid of it *)
  | DumpCommandForCore
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
  let combine args json common =
    Logs_helpers.setup_logging ~force_color:false
      ~level:common.CLI_common.logging_level ();

    let show_kind =
      (* coupling: if you add a command here, update also the man page
       * further below
       *)
      match args with
      | [ "version" ] -> Version
      | [ "dump-config"; config_str ] -> DumpConfig config_str
      | [ "supported-languages" ] -> SupportedLanguages
      | [ "identity" ] -> Identity
      | [ "deployment" ] -> Deployment
      | [ "bucket" ] -> Bucket
      | _ ->
          Error.abort
            (spf "show command not supported: %s" (String.concat " " args))
    in
    {
      show_kind;
      json;
      logging_level =
        Option.value (Some common.CLI_common.logging_level)
          ~default:(Some Logs.Info);
    }
  in

  Term.(const combine $ o_args $ o_json $ CLI_common.o_common)

let doc = "Show various information"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P "Display various information";
    `P "Here are the different subcommands";
    (* the sub(sub)commands *)
    `Pre "semgrep show version";
    `P "Print the Semgrep version";
    `Pre "semgrep show identity";
    `P "Print the current logged-in token identity";
    `Pre "semgrep show deployment";
    `P "Print the current logged-in deployment";
    `Pre "semgrep show supported-languages";
    (* coupling: Scan_CLI.o_show_supported_languages help *)
    `P "Print a list of languages that are currently supported by Semgrep.";
    `Pre "semgrep show dump-config <STRING>";
    `P "Dump the internal representation of the result of --config=<STRING>";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep show" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd

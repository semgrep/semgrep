module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep logout' command-line arguments processing.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type conf = { common : CLI_common.conf } [@@deriving show]

(*****************************************************************************)
(* Cmdline flags *)
(*****************************************************************************)

(*****************************************************************************)
(* Turn argv into a conf *)
(*****************************************************************************)

let cmdline_term : conf Term.t =
  let combine common = { common } in
  Term.(const combine $ CLI_common.o_common)

let doc = "Remove locally stored credentials to semgrep.dev"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P "Remove locally stored credentials to semgrep.dev";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep logout" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd

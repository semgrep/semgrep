module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep lsp' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type conf = { common : CLI_common.conf } [@@deriving show]

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

let cmdline_term : conf Term.t =
  let combine common = { common } in
  Term.(const combine $ CLI_common.o_common)

let doc = "Language server mode!!"

let man : Cmdliner.Manpage.block list =
  [ `S Cmdliner.Manpage.s_description; `P "Language server mode!!" ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep lsp" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* At some point we should support --stdio, --socket etc.
   https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#implementationConsiderations *)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  Fmt_tty.setup_std_outputs ?style_renderer:None ();
  CLI_common.eval_value ~argv cmd

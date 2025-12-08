(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd
module H = Cmdliner_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep lsp' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type conf = { common : CLI_common.conf; x_eio_ls : bool } [@@deriving show]

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

(* TODO: deprecate this once we are ready to switch to Eio finally *)
let o_x_eio_ls : bool Term.t =
  H.negatable_flag [ "x-eio-ls" ] ~neg_options:[ "no-x-eio-ls" ] ~default:false
    ~docs:CLI_common.experimental_section_title
    ~doc:
      {|Run with '--x-eio-ls' to use the new, experimental `Eio`-based language server.|}

let cmdline_term : conf Term.t =
  let combine common x_eio_ls = { common; x_eio_ls } in
  Term.(const combine $ CLI_common.o_common $ o_x_eio_ls)

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

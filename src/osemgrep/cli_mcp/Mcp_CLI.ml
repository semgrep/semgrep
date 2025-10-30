(*
   Copyright (c) 2023-2025 Semgrep Inc.

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
   'semgrep mcp' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type conf = { common : CLI_common.conf; session_id : string } [@@deriving show]

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

let o_session_id : string Term.t =
  let open Cmdliner in
  let doc = "Session ID to use for this MCP daemon instance (required)" in
  Arg.(required & opt (some string) None & info [ "session-id" ] ~doc)

let cmdline_term : conf Term.t =
  let combine common session_id = { common; session_id } in
  Term.(const combine $ CLI_common.o_common $ o_session_id)

let doc = "(experimental) MCP server mode!!"

let man : Cmdliner.Manpage.block list =
  [ `S Cmdliner.Manpage.s_description; `P doc ] @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep mcp" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* LSP specific but maybe relevant:
   At some point we should support --stdio, --socket etc.
   https://microsoft.github.io/language-server-protocol/specifications/mcp/3.17/specification/#implementationConsiderations *)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  Fmt_tty.setup_std_outputs ?style_renderer:None ();
  CLI_common.eval_value ~argv cmd

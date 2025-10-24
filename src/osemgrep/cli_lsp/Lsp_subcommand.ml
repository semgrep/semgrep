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
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-lsp command, execute it and exit.

*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type caps =
  < Core_scan.caps ; Cap.random ; Cap.network ; Cap.tmp ; Cap.readdir >

let hook_pro_language_server :
    (caps -> Eio_unix.Stdenv.base -> Lsp_CLI.conf -> unit) option Hook.t =
  Hook.create None

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (caps : < caps ; .. >) (conf : Lsp_CLI.conf) : Exit_code.t =
  CLI_common.with_logging ~color:Auto ~level:conf.common.logging_level
  @@ fun () ->
  Logs.debug (fun m -> m "Starting semgrep-lsp");
  match Hook.get hook_pro_language_server with
  | Some run_pro_language_server when conf.x_eio_ls ->
      Eio_main.run (fun env ->
          run_pro_language_server (caps :> Legacy_language_server.caps) env conf;
          Exit_code.ok ~__LOC__)
  | None when conf.x_eio_ls ->
      Logs.err (fun m ->
          m
            "Eio-based language server is not configured--make sure you are \
             using the proprietary semgrep binary.");
      Exit_code.fatal ~__LOC__
  | _ ->
      Legacy_language_server.start (caps :> Legacy_language_server.caps);
      Exit_code.ok ~__LOC__

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Lsp_CLI.parse_argv argv in
  run_conf caps conf

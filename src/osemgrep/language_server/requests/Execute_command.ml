(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
(* Commentary *)
(* Commands executed by the client that we process server side. These commands *)
(* must also be added in LS.ml *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)
(* Coupling: Must add the command here for the client to understand to send the *)
(*  command to the LS  *)
let supported_commands = [ Ignore_cmd.command; Autofix_cmd.command ]

let handle_execute_request (session : Session.t) (command : string) arg_list :
    Session.t * Lsp_.Reply.t option =
  match
    [
      (Ignore_cmd.command, Ignore_cmd.command_handler);
      (Autofix_cmd.command, Autofix_cmd.command_handler);
    ]
    |> List.assoc_opt command
  with
  | Some handler -> handler session arg_list
  | None ->
      (* TODO: Log to client *)
      Logs.err (fun m -> m "Command %s not supported by the server" command);
      (session, None)

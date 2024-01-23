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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let handle_execute_request (server : RPC_server.t) command arg_list :
    Yojson.Safe.t option * RPC_server.t =
  match command with
  | "semgrep/ignore" ->
      let session = server.session in
      let session =
        match arg_list with
        | `Assoc
            [ ("path", `String path); ("fingerprint", `String fingerprint) ]
          :: [] ->
            let session = Session.add_skipped_fingerprint session fingerprint in
            let server = { server with session } in
            Scan_helpers.scan_file server Lsp__Uri0.(of_path path);
            session
        | _ -> session
      in
      RPC_server.(None, { server with session })
  | _ ->
      Logs.debug (fun m -> m "Unknown command: %s" command);
      Logs.debug (fun m ->
          m "Args: %s"
            (String.concat ", " (List_.map Yojson.Safe.to_string arg_list)));
      (None, server)

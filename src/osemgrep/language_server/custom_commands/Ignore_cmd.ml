(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
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
(* Ignore command takes in a match based id, and then won't display them. these *)
(* ignored findings are stored in .semgrep/cache/fingerprint_ignored_findings *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
open Lsp
open Types

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
let command = "semgrep/ignore"

type t = { path : string; fingerprint : string } [@@deriving yojson]

let create ~path ~fingerprint =
  let arguments = [ to_yojson { path; fingerprint } ] in
  Command.create ~arguments ~title:"Ignore finding" ~command ()

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let command_handler (session : Session.t) (arg_list : Yojson.Safe.t list) =
  let args =
    match arg_list with
    | args :: _ -> args
    | [] -> `Null
  in
  match of_yojson args with
  | Ok { path; fingerprint } ->
      let session =
        let session = Session.add_skipped_fingerprint session fingerprint in
        let metrics =
          {
            session.metrics with
            ignore_count = session.metrics.ignore_count + 1;
          }
        in
        { session with metrics }
      in
      (session, Some (Scan_helpers.scan_file session Lsp.Uri.(of_path path)))
  | Error e ->
      Logs.warn (fun m -> m "Error parsing ignore command: %s" e);
      (session, None)

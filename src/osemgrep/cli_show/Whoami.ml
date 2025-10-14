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
module OutJ = Semgrep_output_v1_j

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

type identity_kind = Identity | Deployment

let print (caps : < Cap.network ; Cap.stdout >) (kind : identity_kind) :
    Exit_code.t =
  let settings = Semgrep_settings.load () in
  let api_token = settings.Semgrep_settings.api_token in
  match api_token with
  | Some token -> (
      let caps = Auth.cap_token_and_network token caps in
      match kind with
      | Identity ->
          (* get_identity_async returns the identity string or empty on failure *)
          let id = Lwt_platform.run (Semgrep_App.get_identity_async caps) in
          if id = "" then (
            Logs.app (fun m ->
                m "%s Failed to determine identity" (Console.error_tag ()));
            Exit_code.fatal ~__LOC__)
          else (
            Logs.app (fun m ->
                m "%s You are logged in as %s" (Console.success_tag ()) id);
            Exit_code.ok ~__LOC__)
      | Deployment -> (
          let (x : OutJ.deployment_config option) =
            Lwt_platform.run (Semgrep_App.deployment_config_async caps)
          in
          match x with
          | None ->
              Logs.app (fun m ->
                  m "%s Failed to determine deployment" (Console.error_tag ()));
              Exit_code.fatal ~__LOC__
          | Some x ->
              (* TODO? return just x.name? *)
              let str = OutJ.string_of_deployment_config x in
              Logs.app (fun m ->
                  m "%s Your deployment info is %s" (Console.success_tag ()) str);
              Exit_code.ok ~__LOC__))
  | None ->
      Logs.err (fun m ->
          m
            "%s You are not logged in! Run `semgrep login` before using \
             `semgrep show identity`"
            (Console.warning_tag ()));
      Exit_code.fatal ~__LOC__

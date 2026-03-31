(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
let meth = "semgrep/loginStatus"

let on_request (session : Legacy_session.t) id _params :
    Legacy_session.t * Legacy_lsp_.Reply.t =
  let settings = Semgrep_settings.load () in
  match settings.api_token with
  | None -> (session, Legacy_lsp_.Reply.now (Legacy_lsp_.respond_json id `Null))
  | Some token ->
      ( session,
        Legacy_lsp_.Reply.later (fun send ->
            let%lwt deployment =
              Semgrep_App.deployment_config_result_async token
            in
            match deployment with
            | Ok deployment ->
                send
                  (Legacy_lsp_.respond_json id
                     (Legacy_loginfinish.mk_login_response deployment token))
            | Error `Unauthorized ->
                (* Returning null here means the token is invalid, and the user needs to login again *)
                Logs.warn (fun m ->
                    m
                      "Invalid Semgrep token detected responding with null to \
                       LoginStatus request");
                Legacy_lsp_.Reply.apply send
                  (Legacy_lsp_.Reply.now (Legacy_lsp_.respond_json id `Null))
            | Error (`Other msg) ->
                let err =
                  Printf.sprintf
                    "Failed to sign in: invalid token not cleaned up properly \
                     or network error: %s"
                    msg
                in
                Legacy_lsp_.Reply.apply send
                  (Legacy_lsp_.Reply.now
                     (Legacy_lsp_.respond_json_error id
                        (Jsonrpc.Response.Error.make
                           ~code:Jsonrpc.Response.Error.Code.InternalError
                           ~message:err ())))) )

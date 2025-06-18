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

open Lsp
open Types
open Yojson.Safe.Util
open RPC_server
module CN = Client_notification
module CR = Client_request
module Conv = Convert_utils
module OutJ = Semgrep_output_v1_t

(*****************************************************************************)
(* Server *)
(*****************************************************************************)

let initialize_server server
    ({ rootUri; workspaceFolders; initializationOptions; _ } :
      InitializeParams.t) =
  Logs.app (fun m -> m "Initializing server");
  let initializationOptions =
    match initializationOptions with
    | Some json -> json
    | None -> `Assoc []
  in
  Logs.debug (fun m ->
      m "Initializing server with initializationOptions:\n%s"
        (Yojson.Safe.pretty_to_string initializationOptions));
  let user_settings =
    let scan_options = initializationOptions |> member "scan" in
    let do_hover =
      initializationOptions |> member "doHover" |> to_bool_option
      |> Option.value ~default:false
    in
    let pro_intrafile =
      scan_options |> member "pro_intrafile" |> to_bool_option
      |> Option.value ~default:false
    in
    let res =
      scan_options |> User_settings.t_of_yojson
      |> Result.value ~default:server.session.user_settings
    in
    { res with do_hover; pro_intrafile }
  in
  (* Semgrep scanning roots *)
  let workspace_folders =
    match (workspaceFolders, rootUri) with
    | Some (Some folders), _ -> Conv.workspace_folders_to_paths folders
    | _, Some uri -> [ Uri.to_path uri |> Fpath.v ]
    | Some None, None
    | None, None ->
        Logs.warn (fun m -> m "No workspace folders or rootUri provided");
        []
  in
  let is_intellij =
    match initializationOptions |> member "metrics" with
    | `Assoc l when Option.is_some (List.assoc_opt "extensionType" l) ->
        String.equal
          (Option.value ~default:""
             (to_string_option (List.assoc "extensionType" l)))
          "intellij"
    | _ -> false
  in
  (* We're using preemptive threads here as when semgrep scans run, they don't utilize Lwt at all,
      and so block the Lwt scheduler, meaning it cannot properly respond to requests until
      a scan is finished. With preemptive threads, the threads are guaranteed to run concurrently.
     This means we can process IO while a scan is running. *)
  Logs.debug (fun m -> m "Initializing preemptive threads");
  Lwt_platform.init_preemptive 1 user_settings.jobs (fun msg ->
      (* Ocsigen, WTF is this logging thing??? *)
      Logs.debug (fun m -> m "[Language Server Threads]: %s" msg));
  let metrics =
    let client_metrics =
      initializationOptions |> member "metrics"
      |> LS_metrics.client_metrics_of_yojson
      |> Result.value ~default:LS_metrics.client_metrics_default
    in
    { server.session.metrics with client_metrics }
  in
  let server =
    {
      session =
        {
          server.session with
          workspace_folders;
          user_settings;
          is_intellij;
          metrics;
        };
      state = Lsp_.State.Running;
    }
  in
  Logs.app (fun m -> m "Caching workspace targets");
  Session.cache_workspace_targets server.session;
  Logs.app (fun m -> m "Finished caching workspace targets");
  Logs.debug (fun m ->
      m "Initialized server with session:\n%s" (Session.show server.session));
  Logs.app (fun m -> m "Server initialized");
  server

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let on_request server (params : InitializeParams.t) =
  (* There's rootPath, rootUri, and workspaceFolders. First two are
      deprecated, so let's split the diffrence and support last two *)
  let init =
    InitializeResult.
      {
        capabilities = server.session.capabilities;
        serverInfo = Some { name = "Semgrep"; version = Some Version.version };
      }
  in
  let server = initialize_server server params in
  (* TODO we should create a progress symbol before calling initialize server! *)
  (server, init)

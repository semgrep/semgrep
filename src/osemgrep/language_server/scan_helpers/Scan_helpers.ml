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
open Lsp
open RPC_server
module CN = Client_notification
module CR = Client_request
module Conv = Convert_utils
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Semgrep helpers *)
(*****************************************************************************)

let wrap_with_detach f = Lwt.async (fun () -> Lwt_preemptive.detach f ())
(* Relevant here means any matches we actually care about showing the user.
    This means like some matches, such as those that appear in committed
    files/lines, will be filtered out*)

(** This is the entry point for scanning, returns /relevant/ matches, and all files scanned*)
let run_semgrep ?(targets = None) ?(rules = None) ?(git_ref = None)
    ({ session; _ } : RPC_server.t) =
  let rules = Option.value ~default:session.cached_session.rules rules in
  Logs.debug (fun m -> m "Running Semgrep with %d rules" (List.length rules));
  (* !!!Dispatch to the Semgrep engine!!! *)
  let res =
    let targets = Option.value ~default:(Session.targets session) targets in
    let runner_conf = Session.runner_conf session in
    let scan_func =
      Core_runner.mk_scan_func_for_osemgrep Core_scan.scan_with_exn_handler
    in
    scan_func ~respect_git_ignore:true ~file_match_results_hook:None runner_conf
      rules [] targets
    |> Core_runner.create_core_result rules
  in
  (* Collect results. *)
  let scanned = res.scanned |> Set_.elements in
  Logs.debug (fun m -> m "Scanned %d files" (List.length scanned));
  let matches =
    let only_git_dirty = session.user_settings.only_git_dirty in
    Processed_run.of_matches ~git_ref ~only_git_dirty res
  in
  Logs.debug (fun m -> m "Found %d matches" (List.length matches));
  (matches, scanned)

(** Scan all folders in the workspace *)
let scan_workspace server =
  let f () =
    let token =
      create_progress server "Semgrep Scan in Progress" "Scanning Workspace"
    in
    let results, files = run_semgrep server in
    Session.record_results server.session results files;
    (* LSP expects empty diagnostics to clear problems *)
    let diagnostics =
      let files = Session.scanned_files server.session in
      Diagnostics.diagnostics_of_results ~is_intellij:server.session.is_intellij
        results files
    in
    end_progress server token;
    batch_notify server diagnostics
  in
  wrap_with_detach f

(** Scan a single file. Passing [content] will write it to a temp file,
   and scan that temp file, then return results as if [uri] was scanned *)
let scan_file ?(content = None) server uri =
  let f () =
    let file_path = Uri.to_path uri in
    let file = Fpath.v file_path in
    let targets, git_ref =
      match content with
      | None -> ([ file ], None)
      | Some content ->
          let name = Fpath.basename file in
          let ext = Fpath.get_ext file in
          let tmp_file = Common.new_temp_file name ext in
          Common.write_file tmp_file content;
          ([ Fpath.v tmp_file ], Some Fpath.(to_string file))
    in
    let session_targets = Session.targets server.session in
    let targets = if List.mem file session_targets then targets else [] in
    let targets = Some targets in
    let results, _ = run_semgrep ~git_ref ~targets server in
    let results =
      match content with
      | Some _ ->
          let existing_results, _ =
            run_semgrep ~targets:(Some [ file ]) server
          in
          results @ existing_results
      | None -> results
    in
    let results =
      Common.map
        (fun (m : Out.cli_match) -> { m with path = file_path })
        results
    in
    let files = [ file ] in
    Session.record_results server.session results files;
    let diagnostics =
      Diagnostics.diagnostics_of_results ~is_intellij:server.session.is_intellij
        results files
    in
    batch_notify server diagnostics
  in
  wrap_with_detach f

let refresh_rules server =
  let token = create_progress server "Semgrep" "Refreshing Rules" in
  Lwt.async (fun () ->
      let%lwt () = Session.cache_session server.session in
      end_progress server token;
      scan_workspace server;
      Lwt.return_unit)

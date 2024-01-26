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
module OutJ = Semgrep_output_v1_t

(*****************************************************************************)
(* Semgrep helpers *)
(*****************************************************************************)

(** [wrap_with_detach f] runs f in a separate, preemptive thread, in order to
    not block the Lwt event loop *)
let wrap_with_detach f = Lwt.async (fun () -> Lwt_platform.detach f ())
(* Relevant here means any matches we actually care about showing the user.
    This means like some matches, such as those that appear in committed
    files/lines, will be filtered out*)

(** This is the entry point for scanning, returns /relevant/ matches, and all files scanned*)
let run_semgrep ?(targets = None) ?(rules = None) ?(git_ref = None)
    ({ session; _ } : RPC_server.t) =
  let rules = Option.value ~default:session.cached_session.rules rules in
  match (rules, targets) with
  | [], _ ->
      Logs.debug (fun m -> m "No rules to run! Not scanning anything.");
      ([], [])
  | _, Some [] ->
      Logs.debug (fun m -> m "No targets to scan! Not scanning anything.");
      ([], [])
  | _ ->
      Logs.debug (fun m ->
          m "Running Semgrep with %d rules" (List.length rules));
      (* !!!Dispatch to the Semgrep engine!!! *)
      let res =
        let targets = Option.value ~default:(Session.targets session) targets in
        let runner_conf = Session.runner_conf session in
        (* This is currently just ripped from Scan_subcommand. *)
        let scan_func =
          let pro_intrafile = session.user_settings.pro_intrafile in
          match !Core_runner.hook_pro_scan_func_for_osemgrep with
          | Some pro_scan_func when pro_intrafile ->
              (* THINK: files or folders? *)
              let roots = targets in
              (* For now, we're going to just hard-code it at a whole scan, and
                 using the intrafile pro engine.
                 Interfile would likely be too intensive (and require us to target
                 folders, not the affected files)
              *)
              let diff_config = Differential_scan_config.WholeScan in
              pro_scan_func roots ~diff_config
                Engine_type.(
                  PRO
                    {
                      (* TODO: this probably needs better product detection. *)
                      extra_languages = true;
                      analysis = Interprocedural;
                      secrets_config = None;
                      code_config = Some ();
                      supply_chain_config = None;
                    })
          | _ ->
              (* TODO: improve this error message depending on what the
               * instructions should be *)
              if pro_intrafile then
                RPC_server.notify_show_message Lsp.Types.MessageType.Error
                  "You have requested running semgrep with a setting that \
                   requires the pro engine, but do not have the pro engine. \
                   You may need to acquire a different binary."
                |> ignore;
              Core_runner.mk_scan_func_for_osemgrep
                Core_scan.scan_with_exn_handler
        in

        let res =
          scan_func ~respect_git_ignore:true ~file_match_results_hook:None
            runner_conf rules [] targets
        in
        Core_runner.create_core_result rules res
      in
      let errors =
        res.core.errors
        |> List_.map (fun (e : Semgrep_output_v1_t.core_error) -> e.message)
        |> String.concat "\n"
      in
      let skipped =
        res.core.skipped_rules
        |> List_.map (fun (r : Semgrep_output_v1_t.skipped_rule) ->
               Rule_ID.to_string r.rule_id)
        |> String.concat "\n"
      in
      Logs.debug (fun m -> m "Semgrep errors: %s" errors);
      Logs.debug (fun m -> m "Semgrep skipped rules: %s" skipped);
      (* Collect results. *)
      let scanned = res.scanned |> Set_.elements in
      Logs.debug (fun m -> m "Scanned %d files" (List.length scanned));
      Logs.debug (fun m ->
          m "Found %d matches before processing" (List.length res.core.results));
      let skipped_fingerprints = Session.skipped_fingerprints session in
      let matches =
        let only_git_dirty = session.user_settings.only_git_dirty in
        Processed_run.of_matches ~skipped_fingerprints ~git_ref ~only_git_dirty
          res
      in
      Logs.debug (fun m -> m "Found %d matches" (List.length matches));
      Session.send_metrics session;
      (matches, scanned)

(** Scan all folders in the workspace *)
let scan_workspace server =
  let f () =
    let token =
      create_progress "Semgrep Scan in Progress" "Scanning Workspace"
    in
    let results, files = run_semgrep server in
    Session.record_results server.session results files;
    (* LSP expects empty diagnostics to clear problems *)
    let diagnostics =
      let files = Session.scanned_files server.session in
      Diagnostics.diagnostics_of_results ~is_intellij:server.session.is_intellij
        results files
    in
    end_progress token;
    batch_notify diagnostics
  in
  (* Scanning is blocking, so run in separate preemptive thread *)
  wrap_with_detach f

let scan_open_documents server =
  Logs.debug (fun m ->
      m "Scanning open documents with %s" (Session.show server.session));

  let f () =
    let open_documents = server.session.cached_session.open_documents in
    let session_targets = Session.targets server.session in
    let open_documents =
      List.filter (fun doc -> List.mem doc open_documents) session_targets
    in
    let token =
      create_progress "Semgrep Scan in Progress" "Scanning Open Documents"
    in
    let results, files = run_semgrep ~targets:(Some open_documents) server in
    Session.record_results server.session results files;
    (* LSP expects empty diagnostics to clear problems *)
    let diagnostics =
      let files = Session.scanned_files server.session in
      Diagnostics.diagnostics_of_results ~is_intellij:server.session.is_intellij
        results files
    in
    end_progress token;
    batch_notify diagnostics
  in
  wrap_with_detach f

(** Scan a single file *)
let scan_file server uri =
  let f () =
    let file_path = Uri.to_path uri in
    let file = Fpath.v file_path in
    let targets = [ file ] in
    let session_targets = Session.targets server.session in
    (* If the file opened isn't a target, try updating targets just in case *)
    (* This feels fine since if it is an actual target, we need to do this, and if not *)
    (* then the user won't see results either way. *)
    if not (List.mem file session_targets) then (
      Logs.warn (fun m ->
          m
            "File %a is not in the session targets recalculating targets just \
             in case"
            Fpath.pp file);
      Session.cache_workspace_targets server.session);
    let targets = if List.mem file session_targets then targets else [] in
    let targets = Some targets in
    let results, _ = run_semgrep ~targets server in
    let results =
      List_.map (fun (m : OutJ.cli_match) -> { m with path = file }) results
    in
    let files = [ file ] in
    Session.record_results server.session results files;
    let diagnostics =
      Diagnostics.diagnostics_of_results ~is_intellij:server.session.is_intellij
        results files
    in
    batch_notify diagnostics
  in
  (* Scanning is blocking, so run in separate preemptive thread *)
  wrap_with_detach f

let refresh_rules server =
  let token = create_progress "Semgrep" "Refreshing Rules" in
  Lwt.async (fun () ->
      let%lwt () = Session.cache_session server.session in
      end_progress token;
      (* We used to scan ALL files in the workspace *)
      (* Now we just scan open documents so we aren't killing *)
      (* CPU cycles for no reason *)
      scan_open_documents server;
      Lwt.return_unit)

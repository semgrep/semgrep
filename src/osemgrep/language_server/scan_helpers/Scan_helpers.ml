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
(* Helpers *)
(*****************************************************************************)

let parse_and_resolve_name (lang : Lang.t) (fpath : Fpath.t) :
    AST_generic.program * Tok.location list =
  let { Parsing_result2.ast; skipped_tokens; _ } =
    Parse_target.parse_and_resolve_name lang fpath
  in
  (ast, skipped_tokens)

(*****************************************************************************)
(* Semgrep helpers *)
(*****************************************************************************)

(** [wrap_with_detach f] runs f in a separate, preemptive thread, in order to
    not block the Lwt event loop *)
let wrap_with_detach f = Lwt.async (fun () -> Lwt_platform.detach f ())
(* Relevant here means any matches we actually care about showing the user.
    This means like some matches, such as those that appear in committed
    files/lines, will be filtered out*)

(* This is the entry point for scanning, returns /relevant/ matches,
   and all files scanned. *)
let run_semgrep ?(targets : Fpath.t list option) ?rules ?git_ref
    ({ session; _ } : RPC_server.t) =
  let rules = Option.value ~default:session.cached_session.rules rules in
  match (rules, targets) with
  (* This can happen if rules aren't loaded yet. It seems to alarm users if
     they see it even though it's not problematic. TODO have a better way of
     doing this? *)
  | _, Some []
  | [], _
    when not session.cached_session.initialized ->
      Logs.app (fun m -> m "Rules not loaded yet, not scanning anything.");
      ([], [])
  | [], _ ->
      Logs.warn (fun m -> m "No rules to run! Not scanning anything.");
      ([], [])
  | _, Some [] ->
      Logs.warn (fun m -> m "No targets to scan! Not scanning anything.");
      ([], [])
  | _ ->
      let profiler = Profiler.make () in
      Profiler.start profiler ~name:"total_time";
      (* !!!Dispatch to the Semgrep engine!!! *)
      let res =
        let targets =
          (fun () ->
            match targets with
            | None -> Session.targets session
            | Some targets -> targets)
          |> Profiler.record profiler ~name:"session_targets"
        in
        Logs.info (fun m -> m "Scanning %d targets" (List.length targets));
        let runner_conf = Session.runner_conf session in
        (* This is currently just ripped from Scan_subcommand. *)
        let core_run_func =
          let pro_intrafile = session.user_settings.pro_intrafile in
          match !Core_runner.hook_mk_pro_core_run_for_osemgrep with
          | Some pro_scan_func when pro_intrafile ->
              (* THINK: files or folders?
                 Note that converting many target files to scanning roots
                 is expensive due to having to find the project root
                 for each of them. If they're all regular files, we might
                 want to create a way to pass them directly as "target files"
                 rather than "scanning roots".
              *)
              let roots = List_.map Scanning_root.of_fpath targets in
              (* For now, we're going to just hard-code it at a whole scan, and
                 using the intrafile pro engine.
                 Interfile would likely be too intensive (and require us to
                 target folders, not the affected files)
              *)
              pro_scan_func
                {
                  roots;
                  diff_config = Differential_scan_config.WholeScan;
                  engine_type =
                    Engine_type.(
                      PRO
                        {
                          (* TODO: this probably needs better product detection. *)
                          extra_languages = true;
                          analysis = Interprocedural;
                          secrets_config = None;
                          code_config = Some ();
                          supply_chain_config = None;
                          path_sensitive = false;
                        });
                }
          | _ ->
              (* TODO: improve this error message depending on what the
               * instructions should be *)
              if pro_intrafile then
                RPC_server.notify_show_message Lsp.Types.MessageType.Error
                  "You have requested running semgrep with a setting that \
                   requires the pro engine, but do not have the pro engine. \
                   You may need to acquire a different binary."
                |> ignore;
              Core_runner.mk_core_run_for_osemgrep
                (Core_scan.scan (session.caps :> < Cap.tmp >))
        in
        Logs.debug (fun m ->
            m "Running Semgrep with %d rules" (List.length rules));
        let res_or_exn =
          (fun () ->
            core_run_func.run ~file_match_hook:None runner_conf
              Find_targets.default_conf (rules, []) targets)
          |> Profiler.record profiler ~name:"core_run"
        in
        match res_or_exn with
        | Error exn ->
            (* TODO? should this just be logged instead *)
            Exception.reraise exn
        | Ok res -> Core_runner.mk_result rules res
      in
      (* Collect results. *)
      let scanned = res.scanned |> Set_.elements in
      Logs.info (fun m ->
          m "Found %d matches before processing" (List.length res.core.results));
      let skipped_fingerprints = Session.skipped_fingerprints session in
      let matches =
        let only_git_dirty = session.user_settings.only_git_dirty in
        (fun () ->
          Processed_run.of_matches ~skipped_fingerprints ~git_ref
            ~only_git_dirty res)
        |> Profiler.record profiler ~name:"process_run"
      in
      (* Do reporting *)
      let (cli_output : Out.cli_output) =
        Output.preprocess_result ~fixed_lines:false res
      in
      let errors =
        cli_output.errors
        |> List_.filter_map (fun (e : Out.cli_error) -> e.message)
        |> String.concat "\n"
      in
      Logs.app (fun m -> m "Semgrep errors: %s" errors);
      Logs.app (fun m ->
          m "Semgrep skipped %d rules" (List.length cli_output.skipped_rules));
      Logs.app (fun m -> m "Scanned %d files" (List.length scanned));
      Logs.app (fun m -> m "Found %d matches" (List.length matches));
      Session.send_metrics session ?core_time:res.core.time ~profiler
        ~cli_output;
      (matches, scanned)

(* This function runs a search by hooking into Match_search_mode, which bypasses
   some of the CLI.
   We do this instead of using run_semgrep above. Why?
   We want to support streaming searches. This means that we want a hook which
   can activate on a certain granularity, in our case, the matches associated to
   each file.
   This is cool, because we have a file_match_results_hook. The problem is that
   for some reason, when sending mass notifications from each invocation of the
   file_match_results_hook, there is a massive delay (maybe about six seconds
   on django) before the notifications are received by the extension.
   In the interim, we will just continue to hook into core.
*)
let run_core_search xconf rule (file : Fpath.t) =
  let hook = Fun.id in
  let xlang = rule.Rule.target_analyzer in
  (* We have to look at all the initial files again when we do this.
     TODO: Maybe could be better to infer languages from each file,
     so we only have to look at each file once.
  *)
  if Filter_target.filter_target_for_xlang xlang file then
    let xtarget =
      Xtarget.resolve parse_and_resolve_name
        (Target.mk_regular xlang Product.all (File file))
    in
    try
      let is_relevant_rule =
        Match_rules.is_relevant_rule_for_xtarget
          (rule :> Rule.rule)
          xconf xtarget
      in
      if is_relevant_rule then
        (* !!calling the engine!! *)
        let ({ Core_result.matches; _ } : _ Core_result.match_result) =
          Match_search_mode.check_rule rule hook xconf xtarget
        in
        let matches_with_fixes =
          matches
          |> List_.map Core_result.mk_processed_match
          |> Autofix.produce_autofixes
        in
        Some matches_with_fixes
      else None
    with
    | Parsing_error.Syntax_error _ -> None
  else None

(** Scan all folders in the workspace *)
let scan_workspace server =
  Logs.app (fun m -> m "Scanning workspace");
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
    Logs.app (fun m -> m "Scanned workspace");
    batch_notify diagnostics
  in
  (* Scanning is blocking, so run in separate preemptive thread *)
  wrap_with_detach f

let scan_open_documents server =
  Logs.app (fun m -> m "Scanning open documents");
  let f () =
    let open_documents = server.session.cached_session.open_documents in
    let session_targets = Session.targets server.session in
    let open_documents =
      List.filter (fun doc -> List.mem doc open_documents) session_targets
    in
    let token =
      create_progress "Semgrep Scan in Progress" "Scanning Open Documents"
    in
    let results, files = run_semgrep ~targets:open_documents server in
    Session.record_results server.session results files;
    (* LSP expects empty diagnostics to clear problems *)
    let diagnostics =
      let files = Session.scanned_files server.session in
      Diagnostics.diagnostics_of_results ~is_intellij:server.session.is_intellij
        results files
    in
    end_progress token;
    Logs.app (fun m -> m "Scanned open documents");
    batch_notify diagnostics
  in
  wrap_with_detach f

(** Scan a single file *)
let scan_file server uri =
  Logs.app (fun m -> m "Scanning single file");
  let f () =
    let file_path = Uri.to_path uri in
    let file = Fpath.v file_path in
    let targets = [ file ] in
    let session_targets = Session.targets server.session in
    (* If the file opened isn't a target, try updating targets just in case *)
    (* This feels fine since if it is an actual target, we need to do this, and if not *)
    (* then the user won't see results either way. *)
    if not (List.mem file session_targets) then (
      Logs.debug (fun m ->
          m
            "File %a is not in the session targets recalculating targets just \
             in case"
            Fpath.pp file);
      Session.cache_workspace_targets server.session);
    let targets = if List.mem file session_targets then targets else [] in
    let results, _ = run_semgrep ~targets server in
    let results =
      List_.map (fun (m : Out.cli_match) -> { m with path = file }) results
    in
    let files = [ file ] in
    Session.record_results server.session results files;
    let diagnostics =
      Diagnostics.diagnostics_of_results ~is_intellij:server.session.is_intellij
        results files
    in
    Logs.app (fun m -> m "Scanned single file");
    batch_notify diagnostics
  in
  (* Scanning is blocking, so run in separate preemptive thread *)
  wrap_with_detach f

let refresh_rules server =
  let token = create_progress "Semgrep" "Refreshing Rules" in
  Lwt.async (fun () ->
      Logs.app (fun m -> m "Refreshing rules");
      let%lwt () = Session.cache_session server.session in
      end_progress token;
      RPC_server.notify_custom "semgrep/rulesRefreshed";
      Logs.app (fun m -> m "Rules refreshed");

      (* We used to scan ALL files in the workspace *)
      (* Now we just scan open documents so we aren't killing *)
      (* CPU cycles for no reason *)
      scan_open_documents server;
      Lwt.return_unit)

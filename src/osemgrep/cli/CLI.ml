(* Martin Jambon, Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
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
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   (O)Semgrep command-line entry point.

   This module determines the subcommand invoked on the command line
   and has another module handle it as if it was an independent command.
   Exceptions are caught and turned into an appropriate exit code
   (unless you used --debug).

   We don't use Cmdliner to dispatch subcommands because it's too
   complicated and anywant we want full control on main help message.

   Translated from cli.py and commands/wrapper.py
*)

module Env = Semgrep_envvars

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let default_subcommand = "scan"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Placeholder for adaptation of pysemgrep state.terminal.init_for_cli() *)
(* TOPORT:
     1. GITHUB_ACTIONS specific output requirements
     2. Any NO_COLOR / SEMGREP_FORCE_NO_COLOR behavior
*)
(* let init_for_cli () : unit =
   ()
*)

let metrics_init () : unit =
  let () =
    match Env.v.user_agent_append with
    | Some str -> Metrics_.add_user_agent_tag ~str
    | _ -> ()
  in
  let settings = Semgrep_settings.load () in
  let api_token = settings.Semgrep_settings.api_token in
  let anonymous_user_id = settings.Semgrep_settings.anonymous_user_id in
  Metrics_.init ~anonymous_user_id ~ci:Env.v.is_ci;
  Metrics_.add_token (Some api_token);
  ()

let log_cli_feature flag : unit =
  Metrics_.add_feature "cli-flag"
    (flag
    |> Base.String.chop_prefix_if_exists ~prefix:"-"
    |> Base.String.chop_prefix_if_exists ~prefix:"-")

(* NOTE: We could implement send_metrics in Metrics_.ml,
   but we would need to add a dependency on Http_helpers.
*)
let send_metrics () : unit =
  Metrics_.prepare_to_send ();
  (* Populates the sent_at timestamp *)
  let user_agent = Metrics_.string_of_user_agent () in
  let metrics = Metrics_.string_of_metrics () in
  let url = Env.v.metrics_url in
  let headers =
    [ ("Content-Type", "application/json"); ("User-Agent", user_agent) ]
  in
  Logs.debug (fun m -> m "Metrics: %s" metrics);
  Logs.debug (fun m -> m "userAgent: '%s'" user_agent);
  match Http_helpers.post ~body:metrics ~headers url with
  | Ok body -> Logs.debug (fun m -> m "Metrics Endpoint response: %s" body)
  | Error (status_code, err) ->
      Logs.warn (fun m -> m "Metrics Endpoint error: %d %s" status_code err);
      ()

(*****************************************************************************)
(* Subcommands dispatch *)
(*****************************************************************************)

(* This is used to determine if we should fall back to assuming 'scan'.
 * coupling: if you modify the set of subcommands, you probably need to
 * update Help.ml messages.
 *)
let known_subcommands =
  [
    "ci";
    "install-semgrep-pro";
    "login";
    "logout";
    "lsp";
    "publish";
    "scan";
    (* osemgrep-only *)
    "install-ci";
    "interactive";
    "show";
  ]

(* Exit with a code that a proper semgrep implementation would never return.
   Uncaught OCaml exception result in exit code 2.
   This is to ensure that the tests that expect error status 2 fail. *)
let missing_subcommand () =
  Logs.err (fun m -> m "This semgrep subcommand is not implemented\n%!");
  Exit_code.not_implemented_in_osemgrep

let dispatch_subcommand argv =
  match Array.to_list argv with
  (* impossible because argv[0] contains the program name *)
  | [] -> assert false
  (* new: without any argument, we default to the help message now that
   * we removed support for the .semgrep.yml implicit config in pysemgrep.
   *)
  | [ _ ]
  | [ _; "--experimental" ] ->
      Help.print_help ();
      Migration.abort_if_use_of_legacy_dot_semgrep_yml ();
      Exit_code.ok
  | [ _; ("-h" | "--help") ]
  (* ugly: this --experimental management here is a bit ugly, to allow the
   * different combination.
   * alt: remove --experimental from argv, but it's useful to pass it down
   * to the subcommands too.
   *)
  | [ _; ("-h" | "--help"); "--experimental" ]
  | [ _; "--experimental"; ("-h" | "--help") ] ->
      Help.print_semgrep_dashdash_help ();
      Exit_code.ok
  | argv0 :: args -> (
      let subcmd, subcmd_args =
        match args with
        | [] -> (default_subcommand, [])
        | arg1 :: other_args ->
            if List.mem arg1 known_subcommands then (arg1, other_args)
            else
              (* No valid subcommand was found.
                 Assume the 'scan' subcommand was omitted and insert it. *)
              (default_subcommand, arg1 :: other_args)
      in
      let subcmd_argv =
        let subcmd_argv0 = argv0 ^ "-" ^ subcmd in
        subcmd_argv0 :: subcmd_args |> Array.of_list
      in
      let experimental = Array.mem "--experimental" argv in
      (* coupling: with known_subcommands if you add an entry below.
       * coupling: with Help.ml if you add an entry below.
       *)
      Metrics_.add_feature "subcommand" subcmd;
      Metrics_.add_user_agent_tag ~str:(spf "command/%s" subcmd);
      let flags =
        subcmd_argv |> Array.to_list
        |> exclude (fun x -> not (Base.String.is_prefix ~prefix:"-" x))
        |> uniq_by ( =*= )
      in
      List.iter log_cli_feature flags;
      try
        match subcmd with
        (* TODO: gradually remove those 'when experimental' guards as
         * we progress in osemgrep port (or use Pysemgrep.Fallback further
         * down when we know we don't handle certain kind of arguments).
         *)
        | "ci" when experimental -> Ci_subcommand.main subcmd_argv
        | "install-semgrep-pro" when experimental -> missing_subcommand ()
        | "login" when experimental -> Login_subcommand.main subcmd_argv
        | "logout" when experimental -> Logout_subcommand.main subcmd_argv
        | "publish" when experimental -> missing_subcommand ()
        (* TODO: next target for not requiring the 'when experimental' guard!*)
        | "lsp" when experimental -> Lsp_subcommand.main subcmd_argv
        (* partial support, still use Pysemgrep.Fallback in it *)
        | "scan" -> Scan_subcommand.main subcmd_argv
        (* osemgrep-only: and by default! no need experimental! *)
        | "install-ci" -> Install_subcommand.main subcmd_argv
        | "interactive" -> Interactive_subcommand.main subcmd_argv
        | "show" -> Show_subcommand.main subcmd_argv
        (* LATER: "test" *)
        | _else_ ->
            if experimental then
              (* this should never happen because we default to 'scan',
               * but better to be safe than sorry.
               *)
              Error.abort (spf "unknown semgrep command: %s" subcmd)
            else raise Pysemgrep.Fallback
      with
      | Pysemgrep.Fallback -> Pysemgrep.pysemgrep argv)
  [@@profiling]

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* Wrapper that catches exceptions and turns them into an exit code.
 * TOPORT?
    Adds the following functionality to our subcommands:
    - Enforces that exit code 1 is only for findings
    - Handles metric sending before exit
 *)
let safe_run ~debug f : Exit_code.t =
  if debug then f ()
  else
    try f () with
    | Error.Semgrep_error (s, opt_exit_code) -> (
        Logs.err (fun m -> m "%s" s);
        match opt_exit_code with
        | None -> Exit_code.fatal
        | Some code -> code)
    | Error.Exit code -> code
    (* should never happen, you should prefer Error.Exit to Common.UnixExit
     * but just in case *)
    | Common.UnixExit i -> Exit_code.of_int i
    (* TOPORT: PLEASE_FILE_ISSUE_TEXT for unexpected exn *)
    | Failure msg ->
        Logs.err (fun m -> m "Error: %s%!" msg);
        Exit_code.fatal
    | e ->
        let trace = Printexc.get_backtrace () in
        Logs.err (fun m ->
            m "Error: exception %s\n%s%!" (Printexc.to_string e) trace);
        Exit_code.fatal

let before_exit ~profile () : unit =
  (* alt: could be done in Main.ml instead, just before the call to exit() *)
  !Hooks.exit |> List.iter (fun f -> f ());
  (* mostly a copy of Profiling.main_boilerplate finalize code *)
  if profile then Profiling.print_diagnostics_and_gc_stats ();
  (* alt: could use Logs.debug, but --profile would require then --debug *)
  Common.erase_temp_files ();
  ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* called from ../../main/Main.ml *)
let main argv : Exit_code.t =
  Printexc.record_backtrace true;
  let debug = Array.mem "--debug" argv in
  let profile = Array.mem "--profile" argv in

  (* LATER: move this function from Core_CLI to here at some point.
   * alt: we could have each module defining exns register exns from the
   * toplevel, but it's better to avoid toplevel inits so we have better
   * control and can better monitor the startup time.
   *)
  Core_CLI.register_exception_printers ();

  (* Some copy-pasted code from Core_CLI.ml *)
  (* SIGXFSZ (file size limit exceeded)
   * ----------------------------------
   * By default this signal will kill the process, which is not good. If we
   * would raise an exception from within the handler, the exception could
   * appear anywhere, which is not good either if you want to recover from it
   * gracefully. So, we ignore it, and that causes the syscalls to fail and
   * we get a `Sys_error` or some other exception. Apparently this is standard
   * behavior under both Linux and MacOS:
   *
   * > The SIGXFSZ signal is sent to the process. If the process is holding or
   * > ignoring SIGXFSZ, continued attempts to increase the size of a file
   * > beyond the limit will fail with errno set to EFBIG.
   *)
  Sys.set_signal Sys.sigxfsz Sys.Signal_ignore;

  (* TODO? We used to tune the garbage collector but from profiling
     we found that the effect was small. Meanwhile, the memory
     consumption causes some machines to freeze. We may want to
     tune these parameters in the future/do more testing, but
     for now just turn it off.
     (* if !Flag.gc_tuning && config.max_memory_mb = 0
        then Core_CLI.set_gc ();
     *)
  *)

  (* The precise Logs_helpers.setup_logging() is done in Scan_subcommand.ml,
   * because that's when we have a conf object which contains
   * the --quiet/--verbose/--debug options. In the mean time we still
   * enable some default basic logging so you can call logging functions
   * even before we fully parse the command-line arguments.
   * alt: we could analyze [argv] and do it sooner for all subcommands here.
   *)
  Logs_helpers.enable_logging ();
  (* pad poor's man profiler *)
  if profile then Profiling.profile := Profiling.ProfAll;

  (* TOADAPT: profile_start := Unix.gettimeofday (); *)
  (* hacks for having a smaller engine.js file *)
  Parsing_init.init ();
  Data_init.init ();
  metrics_init ();

  (* TOPORT: maybe_set_git_safe_directories() *)

  (*TOADAPT? adapt more of Common.boilerplate? *)
  let exit_code = safe_run ~debug (fun () -> dispatch_subcommand argv) in
  Metrics_.add_exit_code exit_code;
  send_metrics ();

  (* TODO(dinosaure): currently, even if we record the [exit_code], we will
   * never send the final report **with** the exit code to the server. We
   * send it before this call. At some point, we should handle correctly
   * the [exit_code] and properly send the report with it.
   *)
  before_exit ~profile ();
  exit_code

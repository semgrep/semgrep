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
module Http_helpers_ = Http_helpers
module Env = Semgrep_envvars

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   (O)Semgrep command-line entry point.

   This module determines the subcommand invoked on the command line
   and has another module handle it as if it was an independent command.
   Exceptions are caught and turned into an appropriate exit code
   (unless you used --debug).

   alt: we don't use Cmdliner to dispatch subcommands because it's too
   complicated and anyway we want full control on the main help message.

   Translated from cli.py and commands/wrapper.py and parts of metrics.py
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.exec
  ; Cap.random
  ; Cap.signal
  ; Cap.tmp
  ; Cap.chdir
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

let default_subcommand = "scan"

(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)
(* alt: define our own Pro_CLI.ml in semgrep-pro
 * old: was Interactive_subcommand.main
 *)
let hook_semgrep_interactive =
  ref (fun _argv ->
      failwith "semgrep interactive not available (requires --pro)")

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

(*****************************************************************************)
(* Metrics start and end *)
(*****************************************************************************)

let metrics_init (caps : < Cap.random >) : unit =
  let settings = Semgrep_settings.load () in
  let api_token = settings.Semgrep_settings.api_token in
  let anonymous_user_id = settings.Semgrep_settings.anonymous_user_id in
  Metrics_.init caps ~anonymous_user_id ~ci:!Env.v.is_ci;
  api_token
  |> Option.iter (fun (_token : Auth.token) ->
         Metrics_.g.payload.environment.isAuthenticated <- true);
  !Env.v.user_agent_append |> Option.iter Metrics_.add_user_agent_tag;
  Metrics_.g.payload.environment.integrationName <- !Env.v.integration_name;
  ()

(* For debugging customer issues, we append the CLI flags for each subcommand,
   handling the logic in this base CLI entry point pre- subcommand dispatch.
*)
let log_cli_feature (flag : string) : unit =
  Metrics_.add_feature "cli-flag"
    (flag (* TODO: don't use Base unless there's some agreement about it. *)
    |> Base.String.chop_prefix_if_exists ~prefix:"-"
    |> Base.String.chop_prefix_if_exists ~prefix:"-")

(* CLI subcmds need to call Metrics_.configure conf.metrics
   otherwise metrics will not be send as Metrics.g.config default
   to Off
*)
let send_metrics (caps : < Cap.network ; .. >) : unit =
  if Metrics_.is_enabled () then Semgrep_Metrics.send caps
  else Logs.info (fun m -> m "Metrics not enabled, skip sending metrics")

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
    "show";
    "test";
    "validate";
    (* pro-only and osemgrep-only *)
    "interactive";
  ]

let dispatch_subcommand (caps : caps) (argv : string array) =
  match Array.to_list argv with
  (* impossible because argv[0] contains the program name *)
  | [] -> assert false
  (* new: without any argument, we default to the help message now that
   * we removed support for the .semgrep.yml implicit config in pysemgrep.
   *)
  | [ _ ]
  | [ _; "--experimental" ] ->
      Help.print_help caps#stdout;
      Migration.abort_if_use_of_legacy_dot_semgrep_yml ();
      Exit_code.ok ~__LOC__
  | [ _; ("-h" | "--help") ]
  (* ugly: this --experimental management here is a bit ugly, to allow the
   * different combination.
   * alt: remove --experimental from argv, but it's useful to pass it down
   * to the subcommands too.
   *)
  | [ _; ("-h" | "--help"); "--experimental" ]
  | [ _; "--experimental"; ("-h" | "--help") ] ->
      Help.print_semgrep_dashdash_help caps#stdout;
      Exit_code.ok ~__LOC__
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
      (* basic metrics on what was the command *)
      Metrics_.add_feature "subcommand" subcmd;
      (* osemgrep-only: *)
      Metrics_.add_user_agent_tag "osemgrep";
      Metrics_.add_user_agent_tag (Printf.sprintf "command/%s" subcmd);
      subcmd_argv |> Array.to_list
      |> List_.exclude (fun x ->
             (* TODO: don't use JaneStreet Base until we agree to do so *)
             not (Base.String.is_prefix ~prefix:"-" x))
      |> List.iter log_cli_feature;
      (* coupling: with known_subcommands if you add an entry below.
       * coupling: with Help.ml if you add an entry below.
       *)
      try
        match subcmd with
        (* TODO: gradually remove those 'when experimental' guards as
         * we progress in osemgrep port (or use Pysemgrep.Fallback further
         * down when we know we don't handle certain kind of arguments).
         *)
        | "publish" when experimental ->
            Publish_subcommand.main caps subcmd_argv
        | "login" when experimental -> Login_subcommand.main caps subcmd_argv
        (* partial support, still use Pysemgrep.Fallback in it *)
        | "scan" -> Scan_subcommand.main caps subcmd_argv
        | "ci" -> Ci_subcommand.main caps subcmd_argv
        | "install-semgrep-pro" ->
            Install_semgrep_pro_subcommand.main caps subcmd_argv
        (* osemgrep-only: and by default! no need experimental! *)
        | "lsp" -> Lsp_subcommand.main caps subcmd_argv
        | "logout" ->
            Logout_subcommand.main (caps :> < Cap.stdout >) subcmd_argv
        | "install-ci" -> Install_ci_subcommand.main caps subcmd_argv
        | "interactive" -> !hook_semgrep_interactive subcmd_argv
        | "show" -> Show_subcommand.main caps subcmd_argv
        | "test" -> Test_subcommand.main caps subcmd_argv
        | "validate" -> Validate_subcommand.main caps subcmd_argv
        | _ ->
            if experimental then
              (* this should never happen because we default to 'scan',
               * but better to be safe than sorry.
               *)
              Error.abort (Printf.sprintf "unknown semgrep command: %s" subcmd)
            else raise Pysemgrep.Fallback
      with
      | Pysemgrep.Fallback -> Pysemgrep.pysemgrep (caps :> < Cap.exec >) argv)
[@@profiling]

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* Wrapper that catches exceptions and turns them into an exit code
 * TOPORT? "Enforces that exit code 1 is only for findings"
 *)
let safe_run ~debug f : Exit_code.t =
  if debug then f ()
  else
    try f () with
    | Error.Semgrep_error (s, opt_exit_code) -> (
        Logs.err (fun m -> m "%s" s);
        match opt_exit_code with
        | None -> Exit_code.fatal ~__LOC__
        | Some code -> code)
    | Error.Exit_code code -> code
    (* should never happen, you should prefer Error.Exit to Common.UnixExit
     * but just in case *)
    | Common.UnixExit i ->
        Exit_code.of_int ~__LOC__ ~code:i ~description:"rogue UnixExit"
    (* TOPORT: PLEASE_FILE_ISSUE_TEXT for unexpected exn *)
    | Failure msg ->
        Logs.err (fun m -> m "Error: %s%!" msg);
        Exit_code.fatal ~__LOC__
    | e ->
        let trace = Printexc.get_backtrace () in
        Logs.err (fun m ->
            m "Error: exception %s\n%s%!" (Printexc.to_string e) trace);
        Exit_code.fatal ~__LOC__

let before_exit ~profile caps : unit =
  (* alt: could be done in Main.ml instead, just before the call to exit() *)
  !Hooks.exit |> List.iter (fun f -> f ());
  (* mostly a copy of Profiling.main_boilerplate finalize code *)
  if profile then Profiling.log_diagnostics_and_gc_stats ();
  (* alt: could use Logs.debug, but --profile would require then --debug *)
  CapTmp.erase_temp_files caps#tmp;
  ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Called from ../../main/Main.ml. main() mostly does some preliminary logging,
 * profiling, debugging, and metrics initializations before calling
 * dispatch_subcommand().
 *)
let main (caps : caps) (argv : string array) : Exit_code.t =
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
  if Sys.unix then CapSys.set_signal caps#signal Sys.sigxfsz Sys.Signal_ignore;

  (* TODO? We used to tune the garbage collector but from profiling
     we found that the effect was small. Meanwhile, the memory
     consumption causes some machines to freeze. We may want to
     tune these parameters in the future/do more testing, but
     for now just turn it off.
     (* if !Flag.gc_tuning && config.max_memory_mb = 0
        then Core_CLI.set_gc ();
     *)
  *)

  (* The precise Logs_.setup() is done in CLI_Common.setup_logging()
   * called from the different Xxx_subcommand.ml
   * because that's when we have a conf object which contains
   * the --quiet/--verbose/--debug options. In the mean time, we still
   * enable some basic logging so you can call logging functions
   * even before we fully parse the command-line arguments.
   * alt: we could analyze [argv] and do it sooner for all subcommands here.
   *)
  Logs_.setup_basic ();
  (* TOADAPT: profile_start := Unix.gettimeofday (); *)
  (* pad poor's man profiler *)
  if profile then Profiling.profile := Profiling.ProfAll;

  (* hacks for having a smaller engine.js file *)
  Parsing_init.init ();
  Data_init.init ();
  Http_helpers_.set_client_ref (module Cohttp_lwt_unix.Client);

  metrics_init (caps :> < Cap.random >);

  (* TOPORT: maybe_set_git_safe_directories() *)
  (* TOADAPT? adapt more of Common.boilerplate? *)

  (* !The main call! dispatching a subcommand *)
  let exit_code = safe_run ~debug (fun () -> dispatch_subcommand caps argv) in

  Metrics_.add_exit_code exit_code;
  send_metrics (caps :> < Cap.network >);
  before_exit ~profile (caps :> < Cap.tmp >);
  exit_code

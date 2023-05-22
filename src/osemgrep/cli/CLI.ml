(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Semgrep command-line entry point.

   This module determines the subcommand invoked on the command line
   and has another module handle it as if it was an independent command.
   Exceptions are caught and turned into an appropriate exit code
   (unless you used --debug).

   We don't use Cmdliner to dispatch subcommands because it's too
   complicated and we never show a help page for the whole command anyway
   since we fall back to the 'scan' subcommand if none is given.

   Translated from cli.py and commands/wrapper.py
*)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)
(* python: the help message was automatically generated by Click
 * based on the docstring and the subcommands. In OCaml we have to
 * generate it manually unfortunately.
 * LATER: add interactive, test, validate, dump NEW osemgrep-only subcommands.
 * Not added for now to avoid regressions intests/e2e/test_help.py.
 *)
let main_help_msg =
  {|Usage: semgrep [OPTIONS] COMMAND [ARGS]...

  To get started quickly, run `semgrep scan --config auto`

  Run `semgrep SUBCOMMAND --help` for more information on each subcommand

  If no subcommand is passed, will run `scan` subcommand by default

Options:
  -h, --help  Show this message and exit.

Commands:
  ci                   The recommended way to run semgrep in CI
  install-semgrep-pro  Install the Semgrep Pro Engine
  login                Obtain and save credentials for semgrep.dev
  logout               Remove locally stored credentials to semgrep.dev
  lsp                  [EXPERIMENTAL] Start the Semgrep LSP server
  publish              Upload rule to semgrep.dev
  scan                 Run semgrep rules on files
  shouldafound         Report a false negative in this project.
|}

let default_subcommand = "scan"

(*****************************************************************************)
(* TOPORT *)
(*****************************************************************************)

(* TOPORT:
      def maybe_set_git_safe_directories() -> None:
          """
          Configure Git to be willing to run in any directory when we're in Docker.

          In docker, every path is trusted:
          - the user explicitly mounts their trusted code directory
          - r2c provides every other path

          More info:
          - https://github.blog/2022-04-12-git-security-vulnerability-announced/
          - https://github.com/actions/checkout/issues/766
          """
          env = get_state().env
          if not env.in_docker:
              return

          try:
              # "*" is used over Path.cwd() in case the user targets an absolute path instead of setting --workdir
              git_check_output(["git", "config", "--global", "--add", "safe.directory", "*"])
          except Exception as e:
              logger.info(
                  f"Semgrep failed to set the safe.directory Git config option. Git commands might fail: {e}"
              )

   def abort_if_linux_arm64() -> None:
       """
       Exit with FATAL_EXIT_CODE if the user is running on Linux ARM64.
       Print helpful error message.
       """
       if platform.machine() in {"arm64", "aarch64"} and platform.system() == "Linux":
           logger.error("Semgrep does not support Linux ARM64")
           sys.exit(FATAL_EXIT_CODE)
*)

(*****************************************************************************)
(* Subcommands dispatch *)
(*****************************************************************************)

(* This is used to determine if we should fall back to assuming 'scan'. *)
let known_subcommands =
  [
    "ci";
    "install-semgrep-pro";
    "login";
    "logout";
    "lsp";
    "publish";
    "scan";
    "shouldafound";
    (* osemgrep-only *)
    "interactive";
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
  | [ _; ("-h" | "--help") ] ->
      print_string main_help_msg;
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
      (* coupling: with known_subcommands if you add an entry below.
       * coupling: with the main_help_msg if you add an entry below.
       *)
      match subcmd with
      | "ci" -> Ci_subcommand.main subcmd_argv
      | "install-semgrep-pro" -> missing_subcommand ()
      | "login" -> Login_subcommand.main subcmd_argv
      | "logout" -> Logout_subcommand.main subcmd_argv
      | "lsp" -> missing_subcommand ()
      | "publish" -> missing_subcommand ()
      | "scan" -> Scan_subcommand.main subcmd_argv
      | "shouldafound" -> missing_subcommand ()
      (* osemgrep-only *)
      | "interactive" -> Interactive_subcommand.main subcmd_argv
      (* LATER: "dump", "test", "validate" *)
      | _else_ -> (* should have defaulted to 'scan' above *) assert false)
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
  (* TOPORT:
     finally:
      metrics = get_state().metrics
      metrics.add_exit_code(exit_code)
      metrics.send()

      error_handler = get_state().error_handler
      exit_code = error_handler.send(exit_code)
  *)
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

(* called from ../main/Main.ml *)
let main argv : Exit_code.t =
  Printexc.record_backtrace true;
  let debug = Array.mem "--debug" argv in
  let profile = Array.mem "--profile" argv in

  (* LATER: move this function from Core_CLI to here at some point,
   * or have each module defining exns register exns from the toplevel.
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

  (* TOADAPT
     profile_start := Unix.gettimeofday ();
     if config.lsp then LSP_client.init ();
  *)
  (* hacks for having a smaller engine.js file *)
  Parsing_init.init ();
  Data_init.init ();

  (* TOPORT:
      state.terminal.init_for_cli()
      abort_if_linux_arm64()
      state.app_session.authenticate()
      state.app_session.user_agent.tags.add(f"command/{subcommand}")
      state.metrics.add_feature("subcommand", subcommand)
      maybe_set_git_safe_directories()
  *)
  (*TOADAPT? adapt more of Common.boilerplate? *)
  let exit_code = safe_run ~debug (fun () -> dispatch_subcommand argv) in
  before_exit ~profile ();
  exit_code

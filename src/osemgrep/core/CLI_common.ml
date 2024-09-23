open Cmdliner

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   Shared CLI flags, CLI processing helpers, and help messages for the
   semgrep CLI.
*)

(*************************************************************************)
(* Types and constants *)
(*************************************************************************)

type conf = {
  (* mix of --debug, --quiet, --verbose *)
  logging_level : Logs.level option;
  (* osemgrep-only: pad poor's man profiling info for now *)
  profile : bool;
  (* osemgrep-only: mix of --experimental, --legacy, --develop *)
  maturity : Maturity.t;
}
[@@deriving show]

let blurb_pro =
  "Requires Semgrep Pro Engine. See https://semgrep.dev/products/pro-engine/ \
   for more."

(*************************************************************************)
(* Verbosity options (mutually exclusive) *)
(*************************************************************************)

(* alt: we could use Logs_cli.level(), but by defining our own flags
 * we can give better ~doc:. We lose the --verbosity=Level though.
 * TODO: maybe "findings" below is to cli_scan specific
 *)
let o_quiet : bool Term.t =
  let info = Arg.info [ "q"; "quiet" ] ~doc:{|Only output findings.|} in
  Arg.value (Arg.flag info)

(* TODO: same, maybe we should take the doc as a paramter so each
 * cli_xxx command can give a different help
 *)
let o_verbose : bool Term.t =
  let info =
    Arg.info [ "v"; "verbose" ]
      ~doc:
        {|Show more details about what rules are running, which files
failed to parse, etc.
|}
  in
  Arg.value (Arg.flag info)

let o_debug : bool Term.t =
  let info =
    Arg.info [ "debug" ]
      ~doc:{|All of --verbose, but with additional debugging information.|}
  in
  Arg.value (Arg.flag info)

let o_logging : Logs.level option Term.t =
  let combine debug quiet verbose =
    match (verbose, debug, quiet) with
    | false, false, false -> (* default *) Some Logs.Warning
    | true, false, false -> (* --verbose *) Some Logs.Info
    | false, true, false -> (* --debug *) Some Logs.Debug
    | false, false, true -> (* --quiet *) None
    | _ ->
        (* TOPORT: list the possibilities *)
        Error.abort "mutually exclusive options --quiet/--verbose/--debug"
  in
  Term.(const combine $ o_debug $ o_quiet $ o_verbose)

let setup_logging ~force_color ~level =
  Log_semgrep.setup ~force_color ~level ();
  Logs.debug (fun m ->
      m "Logging setup for osemgrep: force_color=%B level=%s" force_color
        (Logs.level_to_string level));
  (* TOPORT
        # Setup file logging
        # env.user_log_file dir must exist
        env.user_log_file.parent.mkdir(parents=True, exist_ok=True)
        file_handler = logging.FileHandler(env.user_log_file, "w")
        file_formatter = logging.Formatter(
            "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
        )
        file_handler.setLevel(logging.DEBUG)
        file_handler.setFormatter(file_formatter)
        logger.addHandler(file_handler)
  *)
  Logs.debug (fun m ->
      m "Executed as: %s" (Sys.argv |> Array.to_list |> String.concat " "))

(*************************************************************************)
(* Profiling options *)
(*************************************************************************)

(* osemgrep-only:  *)
let o_profile : bool Term.t =
  let info = Arg.info [ "profile" ] ~doc:{|<undocumented>|} in
  Arg.value (Arg.flag info)

(*************************************************************************)
(* Term for all common CLI flags *)
(*************************************************************************)

let o_common : conf Term.t =
  let combine logging profile maturity =
    { logging_level = logging; profile; maturity }
  in
  Term.(const combine $ o_logging $ o_profile $ Maturity.o_maturity)

(*************************************************************************)
(* Misc *)
(*************************************************************************)

let help_page_bottom =
  [
    `S Manpage.s_authors;
    `P "Semgrep Inc. <support@semgrep.com>";
    `S Manpage.s_bugs;
    `P
      "If you encounter an issue, please report it at\n\
      \      https://github.com/semgrep/semgrep/issues";
  ]

(* Small wrapper around Cmdliner.Cmd.eval_value.
 * Note that I didn't put this helper function in Cmdliner_helpers.ml because
 * it's using Exit_code.ml and Error.ml which are semgrep-specific.
 *)
let eval_value ~argv cmd =
  (* the ~catch:false is to let non-cmdliner exn (e.g., Error.Semgrep_error)
   * to bubble up; those exns will then be caught in CLI.safe_run.
   *)
  match Cmd.eval_value ~catch:false ~argv cmd with
  (* alt: could define a new Exit_code for those kinds of errors *)
  | Error (`Term | `Parse) -> Error.exit_code_exn (Exit_code.fatal ~__LOC__)
  (* this should never happen, because of the ~catch:false above *)
  | Error `Exn -> assert false
  | Ok ok -> (
      match ok with
      | `Ok config -> config
      | `Version
      | `Help ->
          Error.exit_code_exn (Exit_code.ok ~__LOC__))

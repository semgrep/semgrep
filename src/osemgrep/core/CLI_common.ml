(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open Cmdliner
module H = Cmdliner_

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
  simple_profiling : bool;
  profile : bool;
  (* osemgrep-only: mix of --experimental, --legacy, --develop *)
  maturity : Maturity.t;
  x_parmap : bool;
  (* Telemetry *)
  (* currently only used by `semgrep lsp` *)
  telemetry : Telemetry.config option;
}
[@@deriving show]

let blurb_pro =
  "Requires Semgrep Pro Engine. See https://semgrep.dev/products/pro-engine/ \
   for more."

(* Coupling: these need to be kept in sync with tracing.py *)
let default_trace_endpoint = Uri.of_string "https://telemetry.semgrep.dev"
let default_dev_endpoint = Uri.of_string "https://telemetry.dev2.semgrep.dev"
let default_local_endpoint = Uri.of_string "http://localhost:4318"

(*
   The --help section where all the --x-... options appear:
*)
let experimental_section_title = "EXPERIMENTAL OPTIONS"

(*************************************************************************)
(* Verbosity options (mutually exclusive) *)
(*************************************************************************)

(* alt: we could use Logs_cli.level(), but by defining our own flags
 * we can give better ~doc:. We lose the --verbosity=Level though.
 * TODO: maybe "findings" below is to cli_scan specific
 *)
let o_quiet : bool Term.t =
  let info =
    Arg.info [ "q"; "quiet" ] ~docs:Cmdliner.Manpage.s_common_options
      ~doc:{|Only output findings.|}
  in
  Arg.value (Arg.flag info)

(* TODO: same, maybe we should take the doc as a paramter so each
 * cli_xxx command can give a different help
 *)
let o_verbose : bool Term.t =
  let info =
    Arg.info [ "v"; "verbose" ] ~docs:Cmdliner.Manpage.s_common_options
      ~doc:
        {|Show more details about what rules are running, which files
failed to parse, etc.
|}
  in
  Arg.value (Arg.flag info)

let o_debug : bool Term.t =
  let info =
    Arg.info [ "debug" ] ~docs:Cmdliner.Manpage.s_common_options
      ~doc:{|All of --verbose, but with additional debugging information.|}
  in
  Arg.value (Arg.flag info)

let o_eio : bool Term.t =
  let info =
    Arg.info [ "x-eio" ] ~docs:experimental_section_title
      ~doc:"[INTERNAL] <deprecated>"
  in
  Arg.value (Arg.flag info)

let o_parmap : bool Term.t =
  let info =
    Arg.info [ "x-parmap" ] ~docs:experimental_section_title
      ~doc:"[INTERNAL] Rely on legacy Parmap-based parallelism"
  in
  Arg.value (Arg.flag info)

let o_rule_validation : string Term.t =
  let info =
    Arg.info [ "x-rule-validation" ] ~docs:experimental_section_title
      ~doc:
        "[INTERNAL] Control rule pre-validation. 'full' (default) runs Python \
         jsonschema + semgrep-core RPC validation. 'core-only' runs only the \
         RPC validation. 'none' skips both; rule errors surface from the scan \
         subprocess instead."
  in
  Arg.value (Arg.opt Arg.string "full" info)

(* Deprecated; superseded by --x-rule-validation. Kept as a hidden no-op so
   existing scripts don't break; pysemgrep logs a deprecation warning. *)
let o_no_python_schema_validation : bool Term.t =
  let info =
    Arg.info
      [ "x-no-python-schema-validation" ]
      ~docs:experimental_section_title
      ~doc:
        "[DEPRECATED] No-op alias kept for backward compatibility. Use \
         --x-rule-validation=core-only instead."
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

let with_logging ~color ~level func =
  Log_semgrep.with_setup ~color ~level (fun () ->
      Logs.debug (fun m ->
          m "Logging setup for osemgrep: color=%s level=%s"
            (Console.show_highlight_setting color)
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
          m "Executed as: %s" (Sys.argv |> Array.to_list |> String.concat " "));
      func ())

(*************************************************************************)
(* Profiling options *)
(*************************************************************************)

let o_simple_profiling : bool Term.t =
  let info =
    Arg.info [ "x-simple-profiling" ] ~docs:experimental_section_title
      ~doc:
        "Upon exit, print on stderr a report showing how long certain \
         operations took, in an unspecified text format."
  in
  Arg.value (Arg.flag info)

(* for --profile *)
(* TODO: --no-profile using H.negatable_flag *)
let o_profile : bool Term.t =
  let info =
    Arg.info [ "profile" ] ~docs:Cmdliner.Manpage.s_common_options
      ~doc:
        {|Record profiles via Pyro Caml. By default sends them to localhost:4040|}
  in
  Arg.value (Arg.flag info)

(*************************************************************************)
(* Telemetry options *)
(*************************************************************************)

let o_trace : bool Term.t =
  H.negatable_flag [ "trace" ] ~neg_options:[ "no-trace" ] ~default:true
    ~docs:Cmdliner.Manpage.s_common_options
    ~doc:
      {|Record traces from Semgrep scans to help debugging. This feature is
meant for internal use and may be changed or removed without warning.|}

let o_trace_endpoint : string option Term.t =
  let info =
    Arg.info [ "trace-endpoint" ] ~docs:Cmdliner.Manpage.s_common_options
      ~doc:
        {|Endpoint to send OpenTelemetry traces to, if `--trace` is present.
The value may be `semgrep-prod` (default), `semgrep-dev`,
`semgrep-local`, or any valid URL.  This feature is meant for
internal use and may be changed or removed without warning.|}
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let o_telemetry : Telemetry.config option Term.t =
  let combine trace trace_endpoint =
    let env = Sys.getenv_opt "SEMGREP_DEPLOYMENT_ENV" in
    match (trace, trace_endpoint) with
    | true, Some url ->
        let endpoint =
          match url with
          (* coupling: cli/src/semgrep/telemetry.py _OTEL_ENDPOINT_ALIASES *)
          | "semgrep-prod" -> default_trace_endpoint
          | "semgrep-dev" -> default_dev_endpoint
          | "semgrep-local" -> default_local_endpoint
          | _ -> Uri.of_string url
        in
        Some { Telemetry.endpoint; top_level_scope = None; env }
    | true, None ->
        Some { endpoint = default_trace_endpoint; top_level_scope = None; env }
    | false, Some _ ->
        Logs.warn (fun m ->
            m
              "The --trace-endpoint flag or SEMGREP_OTEL_ENDPOINT environment \
               variable is specified without --trace.\n\
               If you intend to enable tracing, please also add the --trace \
               flag.");
        None
    | false, None -> None
  in
  Term.(const combine $ o_trace $ o_trace_endpoint)

(*************************************************************************)
(* Term for all common CLI flags *)
(*************************************************************************)

let o_common : conf Term.t =
  (* keep the arguments in alphabetic order please *)
  let combine logging maturity profile simple_profiling x_eio x_parmap
      rule_validation no_python_schema_validation telemetry =
    (* user-facing flag only used by pysemgrep *)
    ignore rule_validation;
    (* deprecated no-op; pysemgrep logs the warning *)
    ignore no_python_schema_validation;
    (* --x-eio will be passed to pysemgrep, which will report a deprecation
     * warning. *)
    ignore x_eio;
    {
      logging_level = logging;
      profile;
      simple_profiling;
      maturity;
      x_parmap;
      telemetry;
    }
  in
  Term.(
    const combine $ o_logging $ Maturity.o_maturity $ o_profile
    $ o_simple_profiling $ o_eio $ o_parmap $ o_rule_validation
    $ o_no_python_schema_validation $ o_telemetry)

(*************************************************************************)
(* Misc *)
(*************************************************************************)

let help_page_bottom =
  [
    (* Since 'o_common' contains at least one experimental option,
       we'll have a nonempty experimental section for all semgrep
       subcommands. *)
    `S experimental_section_title;
    `P
      "Any option starting with '--x-' is experimental and may be removed from \
       semgrep without notice.";
    (* the documentation for each experimental option identified with
       a matching ~docs:experimental_section will be inserted here
       by cmdliner *)
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

let exits =
  Exit_code.all
  |> List.map (fun (x : Exit_code.t) -> Cmd.Exit.info x.code ~doc:x.description)

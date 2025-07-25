(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-mcp command, execute it and exit.

*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type caps =
  < Core_scan.caps ; Cap.random ; Cap.network ; Cap.tmp ; Cap.readdir >

let hook_run_mcp : (caps -> Mcp_CLI.conf -> unit) option Hook.t =
  Hook.create None

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (caps : < caps ; .. >) (conf : Mcp_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  Logs.debug (fun m -> m "Starting semgrep-mcp");
  (* let's go! *)
  match Hook.get hook_run_mcp with
  | Some run_mcp ->
      run_mcp (caps :> caps) conf;
      Exit_code.ok ~__LOC__
  | None ->
      Logs.err (fun m ->
          m
            "MCP subcommand requires Pro Engine--make sure you are using the \
             proprietary semgrep binary.");
      Exit_code.fatal ~__LOC__

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Mcp_CLI.parse_argv argv in
  run_conf caps conf

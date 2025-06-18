(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-lsp command, execute it and exit.

*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type caps = Session.caps

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (caps : < caps ; .. >) (conf : Lsp_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  Logs.debug (fun m -> m "Starting semgrep-lsp");
  Lwt_platform.run (LS.start caps);
  Exit_code.ok ~__LOC__

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Lsp_CLI.parse_argv argv in
  run_conf caps conf

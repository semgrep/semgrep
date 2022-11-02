(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-ci command, execute it and exit.

   Translated from ci.py
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (_conf : Ci_CLI.conf) : Exit_code.t =
  (* TODO:
     Setup_logging.setup config;
     logger#info "Executed as: %s" (Sys.argv |> Array.to_list |> String.concat " ");
     logger#info "Version: %s" config.version;
  *)
  Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let res = Ci_CLI.parse_argv argv in
  match res with
  | Ok conf -> CLI_common.safe_run run conf
  | Error exit_code -> exit_code

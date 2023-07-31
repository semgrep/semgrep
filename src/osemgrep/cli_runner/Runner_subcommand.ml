(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-runner command, execute it and exit.

*)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Runner_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  match () with
  | _ when conf.client ->
    Runner_client.run_client();
    Exit_code.ok
  | _else_ -> (
    Runner_server.run_server()
  )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Runner_CLI.parse_argv argv in
  run conf

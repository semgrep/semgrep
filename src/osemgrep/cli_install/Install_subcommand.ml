(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This is a command to install semgrep (in CI) for a given repository *)

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let run (conf : Install_CLI.conf) : Exit_code.t =
  Logs.app (fun m ->
      m "Running install command with env %s"
        (Install_CLI.show_ci_env_flavor conf.ci_env));
  Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (argv : string array) : Exit_code.t =
  let conf = Install_CLI.parse_argv argv in
  run conf

open Common

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Temporary module while migrating code to osemgrep to fallback to
 * pysemgrep when osemgrep does not handle yet certain options.
 *)

(*************************************************************************)
(* Types *)
(*************************************************************************)
exception Fallback

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(* Windows-specific helper to spawn pysemgrep with the given arguments. *)
let win_spawn_pysemgrep (caps : < Cap.exec >) args =
  let cmd = (Cmd.Name "pysemgrep", args) in
  let env = Some { Cmd.vars = []; inherit_parent_env = true } in
  match CapExec.run_subprocess ?env caps#exec cmd with
  | Ok (`Exited n)
  | Ok (`Signaled n) ->
      let msg = spf "pysemgrep signaled with code %d" n in
      raise (UnixExit (n, msg))
  | Error (`Msg msg) ->
      Logs.err (fun m -> m "executing pysemgrep failed: %s" msg);
      raise (UnixExit (127, msg))

(* dispatch back to pysemgrep! *)
let pysemgrep (caps : < Cap.exec >) argv =
  Logs.debug (fun m ->
      m "execute pysemgrep: %s"
        (argv |> Array.to_list
        |> List_.map (fun arg -> spf "%S" arg)
        |> String.concat " "));
  (* pysemgrep should be in the PATH, thx to the code in
     ../../../cli/bin/semgrep *)
  let cmd_name = "pysemgrep" in
  (* execvp does not work on Windows: the C Runtime simply spawns a new process
     and exits the current one, breaking CLI interactivity. *)
  if Sys.win32 then
    (* argv.(0) is the program name (e.g., ["osemgrep"] or ["osemgrep-pro"]).
       [CapUnix.execvp] via [Unix.execvp] allows specifying a different process
       name from the executable name ["pysemgrep"], in this case. The
       [Bos.OS.Cmd.run_status] function used via [CapExec.run_subprocess]
       doesn't allow specifying a program name separately.
       But, the ["pysemgrep"] code doesn't seem to be using this program name,
       and it should be safe to drop it to use the [Bos] library. *)
    let args =
      match Array.to_list argv with
      | [] -> invalid_arg (__FUNCTION__ ^ ": empty argv")
      | _program_name :: args -> args
    in
    win_spawn_pysemgrep caps args
  else CapUnix.execvp caps#exec cmd_name argv

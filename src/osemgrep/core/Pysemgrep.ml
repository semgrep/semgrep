(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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
let win_spawn_pysemgrep args : Exit_code.t =
  let cmd = (Cmd.Name "pysemgrep", args) in
  let env = Some { Cmd.vars = []; inherit_parent_env = true } in
  match UCmd.run_subprocess ?env cmd with
  | Ok (`Exited n) ->
      Exit_code.of_int ~__LOC__ ~code:n
        ~description:(spf "pysemgrep exited with code %d" n)
  (* On Windows, WSIGNALED is never returned by Unix.waitpid; signal-like
     termination is reported via WEXITED with negative NTSTATUS codes.
     See: https://ocaml.org/manual/5.3/api/Unix.html#TYPEprocess_status *)
  | Ok (`Signaled _) -> assert false
  | Error (`Msg msg) ->
      Logs.err (fun m -> m "executing pysemgrep failed: %s" msg);
      Exit_code.of_int ~__LOC__ ~code:127 ~description:msg

(* dispatch back to pysemgrep! *)
let pysemgrep argv : Exit_code.t =
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
       [Unix.execvp] allows specifying a different process
       name from the executable name ["pysemgrep"], in this case. The
       [Bos.OS.Cmd.run_status] function used via [UCmd.run_subprocess]
       doesn't allow specifying a program name separately.
       But, the ["pysemgrep"] code doesn't seem to be using this program name,
       and it should be safe to drop it to use the [Bos] library. *)
    let args =
      match Array.to_list argv with
      | [] -> invalid_arg (__FUNCTION__ ^ ": empty argv")
      | _program_name :: args -> args
    in
    win_spawn_pysemgrep args
  else Unix.execvp cmd_name argv

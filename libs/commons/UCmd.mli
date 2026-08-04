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
(* Wrappers around Bos.OS.Cmd to run external commands. *)

type eio_env =
  < clock : float Eio.Time.clock_ty Eio.Std.r
  ; process_mgr : Eio_unix.Process.mgr_ty Eio.Std.r >
(** The Eio environment required for [string_of_run_with_timeout].
    Any [Eio_unix.Stdenv.base] satisfies this type via coercion. *)

val run_subprocess :
  ?env:Cmd.env -> Cmd.t -> (Bos.OS.Cmd.status, [> `Msg of string ]) result
(** Like status_of_run but does not capture stdout or stderr of the process
    running. Useful to replicate CLI behavior similar to execv* commands, which
    don't work on Windows. *)

(*
   The following functions capture the error output of the command being run
   and logs it as the info level, allowing it to be silenced by adjusting
   the log level.
*)
val string_of_run :
  trim:bool ->
  ?env:Cmd.env ->
  Cmd.t ->
  (string * Cmd.run_status, [> `Msg of string ]) result

val string_of_run_with_stderr :
  trim:bool ->
  ?env:Cmd.env ->
  Cmd.t ->
  (string * Cmd.run_status, [> `Msg of string ]) result * string
(** Like string_of_run but instead of logging the stderr output, it captures it and returns it (in both success and failure cases). *
   The first part of the return type matches the return value of `string_of_run`; the last string part contains the stderr contents *)

val eio_env_of_base : Eio_unix.Stdenv.base -> eio_env
(** Convert an [Eio_unix.Stdenv.base] to an [eio_env].
    Useful in callers that have access to the Eio base environment but do not
    want to depend on [eio.unix] directly. *)

val string_of_run_with_timeout :
  eio_env ->
  timeout_seconds:float ->
  trim:bool ->
  ?env:Cmd.env ->
  Cmd.t ->
  (string * Bos.OS.Cmd.status, [ `Msg of string | `Timeout ]) result * string
(** Like [string_of_run_with_stderr] but kills the subprocess after
    [timeout_seconds] seconds and returns [Error `Timeout].
    Returns [Bos.OS.Cmd.status] (i.e. [`Exited of int | `Signaled of int])
    rather than [Cmd.run_status] since [Bos.OS.Cmd.run_info] cannot be
    constructed independently of Bos.
    On timeout, the child process receives SIGKILL via Eio switch cancellation.
    Must be called from within an Eio event loop ([Eio_main.run]). *)

val lines_of_run :
  trim:bool ->
  ?env:Cmd.env ->
  Cmd.t ->
  (string list * Cmd.run_status, [> `Msg of string ]) result

val status_of_run :
  ?quiet:bool ->
  ?env:Cmd.env ->
  Cmd.t ->
  (Bos.OS.Cmd.status, [> `Msg of string ]) result

(* old style *)
exception CmdError of Unix.process_status * string

val cmd_to_list : ?verbose:bool -> string -> string list

val quote_command_for_bash : string list -> string
(** Convert a list of arguments into a valid Bash command without excessive
    quoting. This is intended primarily for printing out
    commands to be read and copy-pasted by humans. *)

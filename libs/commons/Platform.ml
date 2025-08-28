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
type arch = Arm | Arm64 | X86_64 | OtherArch of string
type kernel = Darwin | Linux | Windows | OtherKernel of string

module Log = Log_commons.Log

let arch (caps : < Cap.exec >) =
  (Cmd.Name "uname", [ "-m" ]) |> CapExec.string_of_run ~trim:true caps#exec
  |> function
  | Ok (output, (_, `Exited 0)) -> (
      match String.lowercase_ascii output with
      | "arm" -> Arm
      | "arm64" -> Arm64
      | "x86_64" -> X86_64
      | s -> OtherArch s)
  | Ok (error, (_, _))
  | Error (`Msg error) ->
      Log.warn (fun m -> m "Running `uname -m` failed with an error: %s" error);
      OtherArch "unknown"

(* We want to check for [Sys.win32] rather than [Sys.cygwin] because we
   target native Windows executables without a cygwin dependency. *)
let is_windows = Sys.win32

let kernel (caps : < Cap.exec >) =
  if is_windows then Windows
  else
    (Cmd.Name "uname", []) |> CapExec.string_of_run ~trim:true caps#exec
    |> function
    | Ok (output, (_, `Exited 0)) -> (
        match String.lowercase_ascii output with
        | "darwin" -> Darwin
        | "linux" -> Linux
        | s -> OtherKernel s)
    | Ok (error, (_, _))
    | Error (`Msg error) ->
        Log.warn (fun m -> m "Running uname failed with an error: %s" error);
        OtherKernel "unknown"

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

(* note that Sys.os_type uses "unix" for both Darwin and Linux *)
type kernel = Darwin | Linux | Windows | OtherKernel of string

(* We need Cap.exec because both functions are calling 'uname' internally.
 * You should avoid using those functions and prefer if possible
 * Sys.os_type, Sys.{unix,win32,cygwin}
 *)

val arch : < Cap.exec > -> arch
val kernel : < Cap.exec > -> kernel

(* alias for Sys.win32 *)
val is_windows : bool

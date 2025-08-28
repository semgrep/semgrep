(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Manipulation of Semgrep version info.

   The actual Semgrep version is in a generated file of its own.
   Use the Semver library to parse, print, and compare versions.
*)

(* The current Semgrep version (the parsed form of Version.version) *)
val version : Semver.t
val major : int
val minor : int
val patch : int

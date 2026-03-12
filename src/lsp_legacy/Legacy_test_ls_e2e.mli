(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val tests : unit -> Testo.t list
val lwt_tests : unit -> Testo_lwt.t list

(* Shared with the main test suite.
   TODO: relocate to a more logical library? *)
val project_root : unit -> Fpath.t

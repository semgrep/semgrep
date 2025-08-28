(*
   Copyright (c) 2021-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Returns the testsuite for dataflow analyses. To be concatenated by
 * the caller (e.g. in Test.ml ) with other testsuites and
 * run via Alcotest.run.
 *)
val tests :
  < Cap.time_limit > -> (Fpath.t -> AST_generic.program) -> Testo.t list

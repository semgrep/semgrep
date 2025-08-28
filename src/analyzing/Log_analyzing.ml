(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
let src = Logs.Src.create "semgrep.analyzing"

module Log = (val Logs.src_log src : Logs.LOG)

(*****************************************************************************)
(* Debugging tags, select with SEMGREP_LOG_TAGS *)
(*****************************************************************************)

let svalue_tag = Logs_.create_tags [ "svalue" ]

(* Prints 'Deep_dataflow_util.uses_table'. *)
let fdeps_tag = Logs_.create_tags [ "fdeps" ]

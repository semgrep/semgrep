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
let src = Logs.Src.create "semgrep.tainting"

module Log = (val Logs.src_log src : Logs.LOG)

(*****************************************************************************)
(* Debugging tags, select with SEMGREP_LOG_TAGS *)
(*****************************************************************************)

(* A debug log with the tag "bad" was meant to be a warning or error, but the
 * issue that is being logged happens too often and adds a lot of noise. *)
let bad_tag = Logs_.create_tags [ "bad" ]

(* Taint signatures *)
let sigs_tag = Logs_.create_tags [ "sigs" ]

(* Taint transfer function *)
let transfer_tag = Logs_.create_tags [ "transfer" ]

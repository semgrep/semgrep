(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* See also CapSys and CapUnix in TCB/.

   See also Cmd.ml which contains the "pure" stuff.
*)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let run_subprocess _caps ?env cmd = UCmd.run_subprocess ?env cmd
let string_of_run _caps ~trim ?env cmd = UCmd.string_of_run ~trim ?env cmd

let string_of_run_with_stderr _caps ~trim ?env cmd =
  UCmd.string_of_run_with_stderr ~trim ?env cmd

let lines_of_run _caps ~trim ?env cmd = UCmd.lines_of_run ~trim ?env cmd
let status_of_run _caps ?quiet ?env cmd = UCmd.status_of_run ?quiet ?env cmd

(*****************************************************************************)
(* Deprecated *)
(*****************************************************************************)

(* DEPRECATED: you should use lines_of_run *)
let cmd_to_list _caps ?verbose cmd_str = UCmd.cmd_to_list ?verbose cmd_str

(* you should probably use of the xxx_of_run function above *)
let with_open_process_in _caps cmd_str fchan =
  UCmd.with_open_process_in cmd_str fchan

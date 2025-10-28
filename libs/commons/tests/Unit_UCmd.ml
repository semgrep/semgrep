(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Unit tests for the UCmd module *)

let test_quote_command_for_bash () =
  let args = [ "ls"; ""; "a b"; "a>b"; "AB"; "42"; "_.-"; "do"; "time" ] in
  let cmd_str = UCmd.quote_command_for_bash args in
  Alcotest.(check string)
    "equal" "ls '' 'a b' 'a>b' AB 42 _.- 'do' time" cmd_str

let skipped = if Sys.win32 then Some "OS-dependent quoting" else None

let tests =
  [ Testo.create "quote command for bash" ?skipped test_quote_command_for_bash ]

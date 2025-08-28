(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_parse _xs = raise Todo
let test_dump _file = raise Todo

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () =
  [
    ("-parse_json", "   <file or dir>", Arg_.mk_action_n_arg test_parse);
    (* -dump_json uses the json-wheel pretty printer *)
    ("-dump_ast_json", "   <file>", Arg_.mk_action_1_arg test_dump);
  ]

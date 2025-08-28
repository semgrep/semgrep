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
(* The unescape_strings is used when we use Json_to_generic to parse
 * a semgrep rule written in JSON (instead of YAML) in which case
 * we need to do the same thing that Yojson does and unescape strings.
 *)
val program : ?unescape_strings:bool -> AST_json.program -> AST_generic.program
val any : AST_json.any -> AST_generic.any
val value_to_generic : AST_json.value -> AST_generic.expr

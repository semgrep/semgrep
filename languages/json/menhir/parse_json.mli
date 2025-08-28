(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* AST_json.program is actually an alias to AST_js.expr *)
val parse_program : Fpath.t -> AST_json.program

(* for semgrep pattern parsing *)
val any_of_string : string -> AST_json.any

(* return a precise AST this time (see AST_json.value) *)
val parse : Fpath.t -> AST_json.value

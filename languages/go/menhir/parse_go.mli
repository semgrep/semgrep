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
val parse : Fpath.t -> (Ast_go.program, Parser_go.token) Parsing_result.t
val parse_program : Fpath.t -> Ast_go.program
val any_of_string : string -> Ast_go.any
val program_of_string : string -> Ast_go.program

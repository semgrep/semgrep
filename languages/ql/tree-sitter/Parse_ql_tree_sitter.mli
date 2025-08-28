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
val parse : Fpath.t -> (AST_ql.program, unit) Tree_sitter_run.Parsing_result.t

val parse_string :
  file:string (* filename *) ->
  contents:string ->
  (AST_ql.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (AST_ql.any, unit) Tree_sitter_run.Parsing_result.t

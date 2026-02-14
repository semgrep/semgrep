(*
   Copyright (c) 2020-2026 Alex Useche (hex0punk).
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.
   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val parse :
  Fpath.t -> (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (AST_generic.any, unit) Tree_sitter_run.Parsing_result.t

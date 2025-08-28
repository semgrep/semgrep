(*
   Copyright (c) 2020-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Map tree-sitter-dockerfile CST to an AST.

   Note that this relies on Parse_bash_tree_sitter.ml to parse
   the bash constructs inside the Dockerfile.
*)

val parse :
  Fpath.t -> (AST_dockerfile.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (AST_dockerfile.program, unit) Tree_sitter_run.Parsing_result.t

(*
   The input can be a sequence of dockerfile instructions
   or a bash snippet.
*)
val parse_docker_or_bash_pattern :
  string -> (AST_generic.any, unit) Tree_sitter_run.Parsing_result.t

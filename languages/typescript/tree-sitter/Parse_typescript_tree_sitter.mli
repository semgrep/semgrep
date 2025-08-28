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
   Parse a typescript program into a javascript AST.

   The plan is to enrich the javascript AST progressively so as to support
   the full typescript language.

   We also want to support tsx (React syntax for typescript) which comes
   as a slightly different grammar and CST than typescript.
*)

type dialect = [ `Typescript | `TSX ]

(*
   Parse a file as pure typescript or as TSX. If unspecified, the
   dialect is guessed from the file extension. Pure typescript is the fallback
   if the extension is unknown.
*)
val parse :
  ?dialect:dialect ->
  Fpath.t ->
  (Ast_js.a_program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (Ast_js.any, unit) Tree_sitter_run.Parsing_result.t

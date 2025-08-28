(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Pattern parser.

   It doesn't use menhir of ocamlyacc because those tools work with
   a Lexing.lexbuf, which we don't use since we don't use ocamllex.
   The implementation isn't very hard since all we do is match braces.
*)

val parse : Pat_lexer.token list -> Pat_AST.t

(* Shortcut for lexing + parsing *)
val from_string : ?source_name:string -> Conf.t -> string -> Pat_AST.t

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
(* This is the main function. See flag_parsing for settings. *)
val parse : Fpath.t -> (AST_scala.program, Parser_scala.token) Parsing_result.t
val parse_program : Fpath.t -> AST_scala.program

(* for semgrep *)
val any_of_string : string -> AST_scala.any

(* internal *)
val tokens : Parsing_helpers.input_source -> Parser_scala.token list

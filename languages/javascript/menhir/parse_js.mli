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
(* This is the main function. It may raise
 *  - Parse_info.Parsing_error if Flag_parsing.error_recovery is false
 *  - Parse_info.Lexical_error if Flag_parsing.exn_when_lexical_error is true.
 *)
val parse :
  ?timeout:float ->
  Fpath.t ->
  (Ast_js.a_program, Parser_js.token) Parsing_result.t

val parse_program : Fpath.t -> Ast_js.a_program

(* other parsers *)

(* for semgrep *)
val any_of_string : string -> Ast_js.any

(* for lsif *)
val type_of_string : string -> Ast_js.type_

(* to help write test code *)
val program_of_string : string -> Ast_js.a_program

(* internal *)
val tokens : Parsing_helpers.input_source -> Parser_js.token list

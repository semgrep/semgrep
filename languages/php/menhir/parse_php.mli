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
(* This is the main function. raise Parse_error when not Flag.error_recovery.*)
val parse : Fpath.t -> (Cst_php.program, Parser_php.token) Parsing_result.t
val parse_program : Fpath.t -> Cst_php.program

(* for sgrep/spatch patterns *)
val any_of_string : string -> Cst_php.any

val tokens :
  ?init_state:Lexer_php.state_mode ->
  Parsing_helpers.input_source ->
  Parser_php.token list

(* useful in tests *)
val program_of_string : string -> Cst_php.program

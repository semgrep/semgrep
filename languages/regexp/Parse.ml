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
(*
   Public entry point for parsing regexps.
*)
open Fpath_.Operators

let from_lexbuf conf lexbuf =
  try Parser.main (Lexer.token conf) lexbuf with
  | Parsing.Parse_error ->
      let tok = Tok.tok_of_lexbuf lexbuf in
      raise (Parsing_error.Syntax_error tok)

let channel conf ic = Lexing.from_channel ic |> from_lexbuf conf

let file ?(conf = Dialect.default_conf) path =
  UFile.Legacy.with_open_infile !!path (fun ic -> channel conf ic)

let string ?(conf = Dialect.default_conf) s =
  Lexing.from_string s |> from_lexbuf conf

let parse = file

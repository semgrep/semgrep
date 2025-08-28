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
val is_eof : Parser_php.token -> bool
val is_comment : Parser_php.token -> bool
val is_just_comment : Parser_php.token -> bool
val token_kind_of_tok : Parser_php.token -> Lib_ast_fuzzy.token_kind
val info_of_tok : Parser_php.token -> Tok.t

val visitor_info_of_tok :
  (Tok.t -> Tok.t) -> Parser_php.token -> Parser_php.token

val line_of_tok : Parser_php.token -> int

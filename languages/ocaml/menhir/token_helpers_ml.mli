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
val is_eof : Parser_ml.token -> bool
val is_comment : Parser_ml.token -> bool
val token_kind_of_tok : Parser_ml.token -> Lib_ast_fuzzy.token_kind
val info_of_tok : Parser_ml.token -> Tok.t
val visitor_info_of_tok : (Tok.t -> Tok.t) -> Parser_ml.token -> Parser_ml.token
val line_of_tok : Parser_ml.token -> int

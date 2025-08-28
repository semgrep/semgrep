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
val is_eof : Parser_go.token -> bool
val is_irrelevant : Parser_go.token -> bool
val is_comment_or_space : Parser_go.token -> bool
val info_of_tok : Parser_go.token -> Tok.t
val visitor_info_of_tok : (Tok.t -> Tok.t) -> Parser_go.token -> Parser_go.token
val token_kind_of_tok : Parser_go.token -> Lib_ast_fuzzy.token_kind

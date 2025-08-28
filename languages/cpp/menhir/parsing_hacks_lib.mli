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
val pr2_pp : string -> unit

val set_as_comment :
  Token_cpp.cppcommentkind -> Token_views_cpp.token_extended -> unit

val msg_context : Parser_cpp.token -> Token_views_cpp.context -> unit
val change_tok : Token_views_cpp.token_extended -> Parser_cpp.token -> unit
val fresh_tok : Parser_cpp.token -> Parser_cpp.token
val regexp_ns_decl_like : Str.regexp
val regexp_macro : Str.regexp
val regexp_declare : Str.regexp

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
val find_ifdef_funheaders : Token_views_cpp.ifdef_grouped list -> unit
val find_ifdef_bool : Token_views_cpp.ifdef_grouped list -> unit
val find_ifdef_mid : Token_views_cpp.ifdef_grouped list -> unit
val find_define_init_brace_paren : Token_views_cpp.paren_grouped list -> unit
val find_string_macro_paren : Token_views_cpp.paren_grouped list -> unit

val find_macro_lineparen :
  Token_views_cpp.paren_grouped Token_views_cpp.line_grouped list -> unit

val find_macro_paren : Token_views_cpp.paren_grouped list -> unit

val filter_pp_or_comment_stuff :
  Token_views_cpp.token_extended list -> Token_views_cpp.token_extended list

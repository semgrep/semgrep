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
val find_template_inf_sup : Token_views_cpp.token_extended list -> unit
val find_template_commentize : Token_views_cpp.multi_grouped list -> unit
val find_qualifier_commentize : Token_views_cpp.token_extended list -> unit
val find_constructor_outside_class : Token_views_cpp.token_extended list -> unit
val find_constructor : Token_views_cpp.token_extended list -> unit

val find_constructed_object_and_more :
  Token_views_cpp.token_extended list -> unit

val reclassify_tokens_before_idents_or_typedefs :
  Token_views_cpp.multi_grouped list -> unit

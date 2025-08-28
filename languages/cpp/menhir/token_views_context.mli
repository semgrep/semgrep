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
val set_context_tag_cplus : Token_views_cpp.multi_grouped list -> unit
val set_context_tag_multi : Token_views_cpp.multi_grouped list -> unit

(* todo: could be moved *)
val look_like_typedef : string -> bool

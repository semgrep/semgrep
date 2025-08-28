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
(* will among other things interally call pp_token.ml to expand some macros *)
val fix_tokens :
  macro_defs:(string, Pp_token.define_body) Hashtbl.t ->
  Flag_parsing_cpp.language ->
  Parser_cpp.token list ->
  Parser_cpp.token list

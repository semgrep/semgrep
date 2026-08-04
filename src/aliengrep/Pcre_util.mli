(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   PCRE2-related code used both for parsing patterns and for scanning targets.
*)

(* Return a PCRE2-compatible character class from a list of characters.
   (only supports ASCII characters)

   If contents_only is set to true, the enclosing brackets are omitted.
*)
val char_class_of_list : ?contents_only:bool -> char list -> string

(*
   Produce a pattern that matches a sequence of characters literally.

   You must use this instead of Pcre2.quote if you're using the `EXTENDED flag.
   It's safe to always use this instead of Pcre2.quote.
*)
val quote : string -> string

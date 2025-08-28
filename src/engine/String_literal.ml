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
(* Evaluate the contents of string literals *)

(*
   Assume:
   \\ -> \
   \' -> '
   \" -> "
*)
let approximate_unescape =
  let rex = Pcre2_.regexp "\\\\[\\\\'\"]" in
  fun s ->
    Pcre2_.substitute ~rex
      ~subst:(fun s ->
        assert (String.length s = 2);
        String.sub s 1 1)
      s

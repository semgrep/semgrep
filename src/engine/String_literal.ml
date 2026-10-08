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
  let re = Pcre2_.compile_exn {|\\[\\'"]|} in
  fun s ->
    match
      Pcre2_.replace_fn re
        (fun s ->
          assert (String.length s = 2);
          String.sub s 1 1)
        s
    with
    | Ok x -> x
    | Error _ -> s

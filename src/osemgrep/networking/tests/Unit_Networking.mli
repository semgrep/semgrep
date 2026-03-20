(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* OCaml's TLS implementation, whether bindings to OpenSSL or the pure ocaml
 * version sometimes breaks. Hours gone and we don't know why.
 * These are some tests to try to catch it.
 *)
val tests : Testo.t list

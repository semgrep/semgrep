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
   Tags used to filter tests.
*)

(* A test that sometimes fails for unknown reasons *)
val flaky : Testo.Tag.t

(* End-to-end semgrep tests *)
val e2e : Testo.Tag.t

(* This is used to exclude all the tests involving this or that language. *)
val tags_of_lang : Lang.t -> Testo.Tag.t list
val tags_of_langs : Lang.t list -> Testo.Tag.t list

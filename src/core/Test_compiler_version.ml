(*
 * Copyright (C) 2025 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

open Common

let t = Testo.create

let tests : Testo.t list =
  [
    t "OCaml compiler version starts with 5.3.0+semgrep-fork" (fun () ->
        let ocaml_version = Sys.ocaml_version in
        let expected_prefix = "5.3.0+semgrep-fork" in
        if not (String.starts_with ~prefix:expected_prefix ocaml_version) then
          failwith
            (spf "Expected OCaml version to start with '%s', but got '%s'"
               expected_prefix ocaml_version));
  ]

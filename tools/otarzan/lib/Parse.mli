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
(*
   Parse an ml file and extract the type definitions
*)
val extract_typedefs_from_ml_file :
  Fpath.t -> AST_ocaml.type_declaration list list

(* helpers used also in Test_otarzan.ml *)
val parse : Fpath.t -> AST_ocaml.program

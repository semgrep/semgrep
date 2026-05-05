(*
   Copyright (c) 2021-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val program : AST_scala.program -> AST_generic.program
val any : AST_scala.any -> AST_generic.any

val merge_chained_packages : AST_generic.stmt list -> AST_generic.stmt list
(** Merge consecutive top-level [Package] directives into a single [Package]
    with concatenated identifiers (e.g. [package a; package b.c] becomes
    [package a.b.c]). Shared with [Parse_scala_tree_sitter]. *)

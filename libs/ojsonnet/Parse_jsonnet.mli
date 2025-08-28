(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* This should really be in ../parsing/ but that would lead to circular
 * dependencies because we would use Parse_jsonnet_tree_sitter.ml which
 * itself use AST_jsonnet.ml.
 *)
val parse_program : Fpath.t -> AST_jsonnet.program

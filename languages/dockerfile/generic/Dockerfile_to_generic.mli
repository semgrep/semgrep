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
   Convert Dockerfile-specific AST to generic AST.

   This relies on Bash_to_generic to convert the bash
   constructs inside the Dockerfile.
*)

(* may raise AST_generic.Error *)
val program : AST_dockerfile.program -> AST_generic.program
val any : AST_dockerfile.program -> AST_generic.any

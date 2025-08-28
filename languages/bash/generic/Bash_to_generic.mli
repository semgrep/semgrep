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
(*
   Convert Bash-specific AST to generic AST.
*)

(*
   Convert a target program to the generic AST.
   May raise AST_generic.Error.
*)
val program : AST_bash.program -> AST_generic.program
val any : AST_bash.program -> AST_generic.any

(* internal function used also in Dockerfile_to_generic *)
val program_with_env :
  AST_bash.input_kind -> AST_bash.program -> AST_generic.program

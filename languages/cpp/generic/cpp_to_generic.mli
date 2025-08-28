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
(* may raise AST_generic.Error *)
val program : Ast_cpp.program -> AST_generic.program

(* refer to `Rule_options.atd` for more details. *)
type cpp_parsing_option = [ `AsFunDef | `AsVarDefWithCtor ]

val any : ?parsing_opt:cpp_parsing_option -> Ast_cpp.any -> AST_generic.any

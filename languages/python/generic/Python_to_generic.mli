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
val program :
  ?assign_to_vardef:bool -> AST_python.program -> AST_generic.program

val any : AST_python.any -> AST_generic.any
val type_for_lsif : AST_python.type_ -> AST_generic.type_
val parameters_for_lsif : AST_python.parameters -> AST_generic.parameter list

(* exception Error of string * Parse_info.info *)
(* may raise Error *)

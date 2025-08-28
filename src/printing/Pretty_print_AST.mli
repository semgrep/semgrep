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
(* This module is currently deprecated. You probably should rely
 * on Ugly_print_AST.ml instead.
 *)

val expr_to_string : Lang.t -> AST_generic.expr -> string
val svalue_to_string : Lang.t -> AST_generic.svalue -> string
val stmt_to_string : Lang.t -> AST_generic.stmt -> string
val arguments_to_string : Lang.t -> AST_generic.argument list -> string

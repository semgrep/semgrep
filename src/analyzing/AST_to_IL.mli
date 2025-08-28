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
type ctx

val empty_ctx : ctx
val add_entity_name : ctx -> AST_generic.ident -> ctx

val function_definition :
  Lang.t ->
  ?ctx:ctx ->
  AST_generic.function_definition ->
  IL.function_definition

val stmt : Lang.t -> AST_generic.stmt -> IL.stmt list
val expr : Lang.t -> AST_generic.expr -> IL.exp
val name_of_entity : AST_generic.entity -> IL.name option
val var_of_name : AST_generic.name -> IL.name
val var_of_id_info : AST_generic.ident -> AST_generic.id_info -> IL.name

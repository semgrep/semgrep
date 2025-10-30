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
type env

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

type compile_pattern_matching_fn =
  env ->
  cond_with_pre_stmts:(env -> AST_generic.condition -> IL.stmt list * IL.exp) ->
  stmt_expr_with_pre_stmts:(env -> AST_generic.stmt -> IL.stmt list * IL.exp) ->
  AST_generic.condition ->
  AST_generic.case_and_body list ->
  IL.stmt list * IL.exp
(** Hook for Pro-only pattern matching compilation *)

val hook_compile_pattern_matching : compile_pattern_matching_fn option Hook.t
val fresh_var : ?str:string -> env -> Tok.t -> IL.name
val mk_e : IL.exp_kind -> IL.orig -> IL.exp
val mk_i : IL.instr_kind -> IL.orig -> IL.instr
val mk_s : IL.stmt_kind -> IL.stmt
val add_stmts : env -> IL.stmt list -> unit
val add_instr : env -> IL.instr -> unit
val with_pre_stmts : env -> (env -> IL.exp) -> IL.stmt list * IL.exp
val locate : ?tok:Tok.t -> string -> string
val lval_of_base : IL.base -> IL.lval

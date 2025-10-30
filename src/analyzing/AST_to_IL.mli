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
(** Context of the translation. Holds a set of entity names. *)

type env
(** Translation environment. Holds language, statements, etc. *)

val empty_ctx : ctx
(** Creates empty context *)

val add_entity_name : ctx -> AST_generic.ident -> ctx
(** Appends the string representation of the {!AST_generic.ident} to
    the set of entity names in {!ctx}  *)

val function_definition :
  Lang.t ->
  ?ctx:ctx ->
  AST_generic.function_definition ->
  IL.function_definition
(** Translates a Generic AST function definition into IL. *)

val stmt : Lang.t -> AST_generic.stmt -> IL.stmt list
(** Translates a Generic AST statement into IL statements. *)

val expr : Lang.t -> AST_generic.expr -> IL.exp
(** Translates a Generic AST expression into an IL expression. *)

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

val hook_compile_pattern_matching : compile_pattern_matching_fn option Hook.t
(** Hook for Pro-only pattern matching compilation. Defaults to None. *)

val fresh_var : ?str:string -> env -> Tok.t -> IL.name
val mk_e : IL.exp_kind -> IL.orig -> IL.exp
val mk_i : IL.instr_kind -> IL.orig -> IL.instr
val mk_s : IL.stmt_kind -> IL.stmt
val add_stmts : env -> IL.stmt list -> unit
val add_instr : env -> IL.instr -> unit

val with_pre_stmts : env -> (env -> IL.exp) -> IL.stmt list * IL.exp
(** Apply a transformation [f] to the environment [env].

    N.B. The side-effects of the transformation (i.e. produces pre-statements),
    are _not_ stored in the environment.
    @return (side_effects, transformed_env)
 *)

val locate : ?tok:Tok.t -> string -> string
(** Prepend the token location to [s], if found. *)

val lval_of_base : IL.base -> IL.lval

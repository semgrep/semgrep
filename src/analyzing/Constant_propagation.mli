(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val constant_propagation_and_evaluate_literal :
  ?lang:Lang.t -> AST_generic.expr -> AST_generic.svalue option
(** Partially evaluate a Generic expression. *)

type propagate_basic_visitor_funcs = {
  visit_definition :
    Eval_generic_partial.env * Iter_with_context.context ->
    AST_generic.definition ->
    unit;
}

val add_constant_env :
  AST_generic.ident ->
  AST_generic.sid * AST_generic.svalue ->
  Eval_generic_partial.env ->
  unit

(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some constant propagation algorithm may be
 * specific to a language.
 *)
(* !Note that this assumes Naming_AST.resolve has been called before! *)
val propagate_basic : Lang.t -> AST_generic.program -> unit

(* This is exposed for DeepSemgrep, so it can control the order
 * in which functions are analyzed. Generally will perform better
 * if propagate_basic is called first *)
val propagate_dataflow_one_function :
  Lang.t -> Fun_CFG.t (* function CFG *) -> unit

val propagate_dataflow : Lang.t -> AST_generic.program -> unit

(* pro-scan hook *)
val hook_propagate_basic_visitor : propagate_basic_visitor_funcs option Hook.t

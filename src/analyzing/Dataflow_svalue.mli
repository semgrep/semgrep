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
(** Dataflow S-value analysis (constant and symbolic propagation) *)

type mapping = AST_generic.svalue Dataflow_var_env.mapping

val is_symbolic_expr : AST_generic.expr -> bool

val fixpoint : Lang.t -> Fun_CFG.t -> mapping
(** Flow-sensitive constant-propagation.
   !Note that this assumes Naming_AST.resolve has been called before!
*)

val set_svalue_ref : AST_generic.id_info -> AST_generic.svalue -> unit

val update_env_with :
  AST_generic.svalue Dataflow_var_env.t ->
  IL.name ->
  AST_generic.svalue ->
  AST_generic.svalue Dataflow_var_env.t

val update_svalue : IL.cfg -> mapping -> unit
(**
   Updates the [IL.lval.svalue] refs according to the mapping.
   Note that the svalue refs in IL are shared with the Generic AST, so
   running this analysis also updates svalue info in the Generic AST.
   The update respects previous constant propagation passes, updating
   svalue info when we have deduced more specific facts, but leaving it
   untouched otherwise.
*)

(* deep-scan hook *)
val hook_constness_of_function :
  (AST_generic.expr -> AST_generic.svalue option) option Hook.t

(* pro-scan hook *)
val hook_transfer_of_assume :
  (bool ->
  IL.exp_kind ->
  AST_generic.svalue Dataflow_var_env.t ->
  AST_generic.svalue Dataflow_var_env.t)
  option
  Hook.t

(* pro-scan hook: replaces the default flow-sensitive fixpoint with an
 * alternative implementation. The argument is [lang -> enter_env -> fun_cfg].
 * Unset in CE, where the default must-analysis fixpoint runs. *)
val hook_fixpoint_with_env :
  (Lang.t -> AST_generic.svalue Dataflow_var_env.t -> Fun_CFG.t -> mapping)
  option
  Hook.t

val transfer_node :
  lang:Lang.t ->
  fun_cfg:Fun_CFG.t ->
  IL.node ->
  AST_generic.svalue Dataflow_var_env.t ->
  AST_generic.svalue Dataflow_var_env.t
(** Per-node transfer used by the default fixpoint. Exposed so that an
    alternative fixpoint installed via {!hook_fixpoint_with_env} can reuse the
    exact same per-node constant-propagation semantics. *)

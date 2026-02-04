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
type mapping = OSS_taint_lval_env.t Dataflow_core.mapping
(** Mapping from variables to taint sources (if the variable is tainted).
  * If a variable is not in the map, then it's not tainted. *)

(** When we encounter getters/setters without a definition, we need to resolve them
  * to their corresponding property, we cache the results here. *)

val fixpoint :
  Taint_rule_inst.t ->
  ?in_env:OSS_taint_lval_env.t ->
  ?name:IL.name ->
  Fun_CFG.t ->
  Shape_and_sig.Effects.t * mapping
(** Main entry point, [fixpoint taint_inst cfg] returns a mapping (effectively a set)
  * containing all the tainted variables in [cfg]. Besides, if it infers any taint
  * 'findings', it will invoke [config.handle_findings] which can perform any
  * side-effectful action.
  *
  * @param in_env are the assumptions made on the function's parameters.
  * @param name is the name of the function being analyzed, if it has a name.
  * *)

val must_drop_taints_if_bool_or_number : Rule_options.t -> 'a Type.t -> bool
(** 'must_drop_taints_if_bool_or_number options typ' is 'true' iff given the
  `taint_assume_safe_*` options we need to sanitize expressions of type 'typ'.

  For example, if `taint_assume_safe_numbers` is set and 'typ' is an integer
  type, then 'must_drop_taints_if_bool_or_number' will evaluate to 'true'.

  THINK: Move to module 'Taint' or somewhere else? *)

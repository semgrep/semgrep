(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)

(** Lambda-environment for taint analysis. *)

type env
(** Environment, keeps track of the lambdas under analysis. *)

val new_env : Fun_CFG.t -> env

val push : env -> IL.name (** lambda's name *) -> Fun_CFG.t -> env
(** Push a new lambda to be analyzed. *)

val find_lambda_cfg_in_current_scope :
  env -> IL.lval -> (IL.name * Fun_CFG.t) option

val find_lambdas_to_analyze_in_node :
  env -> IL.node -> (IL.name * Fun_CFG.t) list
(** Given a CFG node, finds the lambdas to be analyzed:

      - If the node declares a lambda, and this lambda is not used anywhere.
      - Any other lambda being referenced/used in the node.
 *)

val live_vars_needed_for_taint : env -> IL.NameSet.t
(** The set of variables that must be tracked across lambdas.

    This is used to filter what tainted variables, of those discovered while
    analyzing a lambda, may be relevant for the enclosing function.

    This is a rough flow-insensitive and cheap approximation of a liveness analysis.

    In the example below, `y` can be discarded after `foo(...)` but `x` must be kept
    because it is later passed into a sink:

        let x;
        foo(() => { y = taint; x = taint; });
        sink(x);

    ALT: We could just do liveness analysis (and we tried) but that seems to be slower
        overall, the cost of the liveness analysis may be higher than the benefit of
        the added precision.
 *)

(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type lambdas_cfgs
type t = { params : IL.param list; cfg : IL.cfg; lambdas : lambdas_cfgs }

val reachable_nodes : t -> IL.node Seq.t
(** Get the reachable nodes from function's CFG, including the nodes in the lambdas' CFGs. *)

val empty_lambdas : lambdas_cfgs
val record_lambda : lambdas_cfgs -> IL.name -> t -> lambdas_cfgs
val seq_of_lambdas : lambdas_cfgs -> (IL.name * t) Seq.t
val find_lambda : lambdas_cfgs -> IL.name -> t option
val is_lambda : lambdas_cfgs -> IL.lval -> (IL.name * t) option
val union_lambdas : base:lambdas_cfgs -> lambdas_cfgs -> lambdas_cfgs

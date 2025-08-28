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
(** Dataflow environments where the key is a simple variable name (a string) *)

(* The comparison function uses only the name of a variable (a string), so
 * two variables at different positions in the code will be agglomerated
 * correctly in the Set or Map.
 *)
type var = string

module VarMap : Map.S with type key = String.t
module VarSet : Set.S with type elt = String.t

type 'a t = 'a VarMap.t
type 'a env = 'a t
type 'a inout = 'a env Dataflow_core.inout
type 'a mapping = 'a env Dataflow_core.mapping
type 'a transfn = 'a mapping -> Dataflow_core.nodei -> 'a inout

val empty_env : unit -> 'a env
val empty_inout : unit -> 'a inout
val eq_env : ('a -> 'a -> bool) -> 'a env -> 'a env -> bool
val varmap_union : ('a -> 'a -> 'a) -> 'a env -> 'a env -> 'a env
val varmap_diff : ('a -> 'a -> 'a) -> ('a -> bool) -> 'a env -> 'a env -> 'a env
val env_to_str : ('a -> string) -> 'a env -> string

(** {2 Variable to [Dataflow_core.NodeiSet] environments} *)

val add_var_and_nodei_to_env :
  var ->
  Dataflow_core.nodei ->
  Dataflow_core.NodeiSet.t env ->
  Dataflow_core.NodeiSet.t env

val add_vars_and_nodei_to_env :
  VarSet.t ->
  Dataflow_core.nodei ->
  Dataflow_core.NodeiSet.t env ->
  Dataflow_core.NodeiSet.t env

val union_env :
  Dataflow_core.NodeiSet.t env ->
  Dataflow_core.NodeiSet.t env ->
  Dataflow_core.NodeiSet.t env

val diff_env :
  Dataflow_core.NodeiSet.t env ->
  Dataflow_core.NodeiSet.t env ->
  Dataflow_core.NodeiSet.t env

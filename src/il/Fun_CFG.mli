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
(** Function CFG, and handling of lambdas. *)

type pos = Pos.t option
(** Source position of a lambda declaration.

  If a lambda is has a name coming from the sources, it will have a position.
  If a lambda was given a fresh "tmp" name, it won't have a source position,
  so this will be 'None'.
 *)

type lambdas_cfgs
(** A map from lambda names to their CFGs. *)

type t = { params : IL.param list; cfg : IL.cfg; lambdas : lambdas_cfgs }
(** The params and CFG of a function, plus its top-level lambdas.

  If there are lambdas nested wihtin lambdas, the nested lambdas will not be in
  'lambdas' here. Each lambda has its own 'Fun_CFG.t', with its top-lvel lambdas.
 *)

val reachable_nodes : t -> IL.node Seq.t
(** Get the reachable nodes from function's CFG, including the nodes in the lambdas' CFGs. *)

val empty_lambdas : lambdas_cfgs

val record_lambda : lambdas_cfgs -> IL.name -> t -> lambdas_cfgs
(** Record a top-level lambda declaration in a function (or lambda), the 'name'
    should be the one taken from the declaration. *)

val seq_of_lambdas : lambdas_cfgs -> (IL.name * pos * t) Seq.t
(** List top-level lambdas in the CFG and the source positions where they
    are declared. *)

val find_lambda :
  lambdas_cfgs -> IL.name -> (t, [> `NotLambda | `Multi ]) result
(** Find a lambda by name and/or pos. *)

val is_lambda :
  lambdas_cfgs ->
  IL.lval ->
  (IL.name * t, [> `NotVar | `NotLambda | `Multi ]) result
(** Check if an 'lval' is a lambda and fetch its CFG, see 'find_lambda'. *)

val lambdas_names : lambdas_cfgs -> IL.NameSet.t
(** Domain of 'lambdas_cfgs', all the top-level lambda names. *)

val union_lambdas : base:lambdas_cfgs -> lambdas_cfgs -> lambdas_cfgs

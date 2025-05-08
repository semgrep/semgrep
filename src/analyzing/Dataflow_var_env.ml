(* Iain Proctor, Yoann Padioleau, Jiao Li
 *
 * Copyright (C) 2009-2010 Facebook
 * Copyright (C) 2019-2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

module C = Dataflow_core
module VarMap = Maps.String_map
module VarSet = Sets.String_set

(* The comparison function uses only the name of a variable (a string), so
 * two variables at different positions in the code will be agglomerated
 * correctly in the Set or Map.
 *)
type var = string
type 'a t = 'a VarMap.t
type 'a env = 'a t
type 'a inout = 'a env C.inout
type 'a mapping = 'a env C.mapping
type 'a transfn = 'a mapping -> C.nodei -> 'a inout

let empty_env () = VarMap.empty
let empty_inout () = { C.in_env = empty_env (); out_env = empty_env () }

(* the environment is polymorphic, so we require to pass an eq for 'a *)
let eq_env eq env1 env2 = VarMap.equal eq env1 env2

let (varmap_union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t) =
 fun union_op env1 env2 ->
  let union _ x y = Some (union_op x y) in
  VarMap.union union env1 env2

let (varmap_diff : ('a -> 'a -> 'a) -> ('a -> bool) -> 'a t -> 'a t -> 'a t) =
 fun diff_op is_empty env1 env2 ->
  let merge _ opt_x opt_y =
    match (opt_x, opt_y) with
    | None, _ -> None
    | Some x, None -> Some x
    | Some x, Some y ->
        let diff = diff_op x y in
        if is_empty diff then None else Some diff
  in
  VarMap.merge merge env1 env2

let (env_to_str : ('a -> string) -> 'a t -> string) =
 fun val2str env ->
  VarMap.fold (fun dn v s -> s ^ dn ^ ":" ^ val2str v ^ " ") env ""

(*****************************************************************************)
(* NodeiSet envs *)
(*****************************************************************************)

let (add_var_and_nodei_to_env :
      var -> C.nodei -> C.NodeiSet.t t -> C.NodeiSet.t t) =
 fun var ni env ->
  let set =
    try C.NodeiSet.add ni (VarMap.find var env) with
    | Not_found -> C.NodeiSet.singleton ni
  in
  VarMap.add var set env

let (add_vars_and_nodei_to_env :
      VarSet.t -> C.nodei -> C.NodeiSet.t t -> C.NodeiSet.t t) =
 fun varset ni env ->
  let acc = env in
  VarSet.fold (fun var acc -> add_var_and_nodei_to_env var ni acc) varset acc

(* useful helpers when the environment maps to a set of Nodes, e.g.,
 * for reaching definitions.
 *)
let (union_env : C.NodeiSet.t t -> C.NodeiSet.t t -> C.NodeiSet.t t) =
 fun env1 env2 ->
  let union _ x y = Some (C.NodeiSet.union x y) in
  VarMap.union union env1 env2

let (diff_env : C.NodeiSet.t t -> C.NodeiSet.t t -> C.NodeiSet.t t) =
 fun env1 env2 ->
  let merge _ opt_x opt_y =
    match (opt_x, opt_y) with
    | None, _ -> None
    | Some x, None -> Some x
    | Some x, Some y ->
        let diff = C.NodeiSet.diff x y in
        if C.NodeiSet.is_empty diff then None else Some diff
  in
  VarMap.merge merge env1 env2

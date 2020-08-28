(* Julien Verlaguet, Yoann Padioleau
 *
 * Copyright (C) 2011, 2012 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

open Env_typing_php
open Typing_helpers_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module implements the unification of PHP types. It's using
 * a tricky technique. TODO explain.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* mixed means contain both arrays and objects *)
let rec mixed has_array has_object l =
  match l with
  | [] -> has_array && has_object
  | (Tobject _ | Tclosed _) :: rl -> mixed has_array true rl
  | (Trecord _ | Tarray _) :: rl -> mixed true has_object rl
  | _ :: rl -> mixed has_array has_object rl
let mixed l = mixed false false l

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* Unify types (polymorphic type or union types). Will modify
 * env.subst and returned the unified type.
 *)
let rec unify env t1 t2 =
  if t1 == t2 
  then t1 
  else
    match t1, t2 with
    (* CONFIG: null get absorbed :( could change that to find null bugs? *)
    | Tsum ([] | [Tabstr "null"]), x | x, Tsum ([] | [Tabstr "null"]) -> 
        x
          (* pad: ??? redundant with t1 == t2 above no ?*)
    | x, y when x == y -> x

    | Tvar x, Tvar y when x = y -> 
        t1
    | Tvar n1, Tvar n2 -> 
        (* tricky *)
        unify_vars env n1 n2
    | (Tvar _n as x), y | y, (Tvar _n as x) ->
        (* TODO Why using a fresh type variable? what is wrong with n? *)
        let n' = fresh() in
        TEnv.set env n' y;
        unify env x (Tvar n')
    | Tsum x, Tsum y ->
        let l = unify_sum env x y in
        (match () with
        (* CONFIG: let's not grow too much, abstract things quite fast *)
        | _ when mixed l           -> Tsum []
        | _ when List.length l > 3 -> Tsum []
        | _                        -> Tsum l
        )

(* 
 * function($x: 'a, $y: 'b) {
 *   // here we need to unify 'a and 'b
 *   return (true)? $x : $y;
 * }
 * at the end, in subst we will have 'a -> 'c, 'b -> 'c, 
 * and in tenv we will have 'c --> Tany, and we will
 * return 'c here.
 * 
 * TODO This is very subtle code.
 *)
and unify_vars env n1 n2 =
  let n1' = Subst.get env n1 in
  let n2' = Subst.get env n2 in
  if n1' = n2' 
  then Tvar n1' 
  else (* TODO: begin here no? *)
    let t1 = TEnv.get env n1' in
    let t2 = TEnv.get env n2' in
    let n = fresh() in
    Subst.replace env n1 n;
    Subst.replace env n2 n;
    let t = unify env t1 t2 in
    let n = Subst.get env n in
    TEnv.set env n t;
    (Tvar n)

(* unify prim simple types *)
and unify_ env (t1: prim_ty) (t2: prim_ty) =
  match t1, t2 with
  | Tsstring s1, Tsstring s2 when s1 == s2 -> 
      t1
  | Tsstring s1, Tsstring s2 ->
      let s = SSet.union s1 s2 in
      (* CONFIG: absorb *)
      if SSet.cardinal s > 200
      then Tabstr "string"
      else Tsstring s1
        (* TODO: ??? why not unify_ env t (Tabstr "string") ? *)
  | (Tsstring _ as t), _ | _, (Tsstring _ as t) -> 
      t

  | Tienum s1, Tienum s2 when s1 == s2 ->  t1
  | Tienum s1, Tienum s2               ->  Tienum (SSet.union s1 s2)
  | Tienum _, t | t, Tienum _          ->  unify_ env t (Tabstr "int")

  | Tsenum s1, Tsenum s2 when s1 == s2 -> t1
  | Tsenum s1, Tsenum s2               -> Tsenum (SSet.union s1 s2)
  | Tsenum _, t | t, Tsenum _          -> unify_ env (Tabstr "string") t

  (* We want them in this order bool < int < string < html. Absorb. 
   * TODO: CONFIG: can be desactivated?
   *)
  | Tabstr "bool", t | t, Tabstr "bool" -> t
  | Tabstr "int", t  | t, Tabstr "int" -> t
  | Tabstr "string", t | t, Tabstr "string" -> t
  | Tabstr _, _ -> t1

  | Trecord s1, Trecord s2 ->
      if s1 == s2 
      then t1 
      else Trecord (unify_map env s1 s2)
  | Trecord x, t | t, Trecord x -> (*records get changed into arrays! need pi
  for records too*)
      if SMap.is_empty x
      then t
      else
        let v = SMap.fold (fun _ t acc -> unify env t acc) x any in
        let s = SMap.fold (fun x _ acc -> SSet.add x acc) x SSet.empty in
        unify_ env (Tarray (s, string, v)) t

  | Tarray (s1, t1, t2), Tarray (s2, t3, t4) ->
      let s = SSet.union s1 s2 in
      Tarray (s, unify env t1 t3, unify env t2 t4)

  | Tfun (l1, t1), Tfun (l2, t2) ->
      let l = unifyl env l1 l2 in
      let t = unify env t1 t2 in
      Tfun (l, t)

  | Tobject o, Tclosed (s, c) | Tclosed (s, c), Tobject o ->
      let o = unify_map env o c in
      Tclosed (s, o)
  | Tobject o1, Tobject o2 ->
      if o1 == o2 then t1 else
        Tobject (unify_map env o1 o2)
  | Tclosed (s1, c1), Tclosed (s2, c2) ->
      let c1 = SMap.fold (
        fun x t acc ->
          if SMap.mem x c2
          then SMap.add x t acc
          else acc
      ) c1 SMap.empty in
      let c2 = SMap.fold (
        fun x t acc ->
          if SMap.mem x c1
          then SMap.add x t acc
          else acc
      ) c2 SMap.empty in
      Tclosed (SSet.union s1 s2, unify_map env c1 c2)

  | _ -> assert false

and unifyl env l1 l2 =
  match l1, l2 with
  | [], l | l, [] -> l
  | (s1, x1) :: rl1, (s2, x2) :: rl2 ->
      let s = 
        if s1 = s2 then s1 else ""
      in
      (s, unify env x1 x2) :: unifyl env rl1 rl2

and unify_map env s1 s2 =
  SMap.fold (fun x t acc ->
    if SMap.mem x s2
    then
      let t = unify env t (SMap.find x s2) in
      SMap.add x t acc
    else SMap.add x t acc
  ) s1 s2

and unify_sum env l1 l2 =
  match l1, l2 with
  | [], l | l, [] -> 
      l
  | x1 :: rl1, x2 :: rl2 ->
      let c1 = Env_typing_php.proj x1 in
      let c2 = Env_typing_php.proj x2 in
      (* todo: use Cmp *)
      let c = c1 - c2 in
      if c < 0
      then x1 :: unify_sum env rl1 l2
      else 
        if c > 0
        then x2 :: unify_sum env l1 rl2
      else unify_ env x1 x2 :: unify_sum env rl1 rl2

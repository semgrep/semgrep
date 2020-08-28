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
open Ast_php
open Env_interpreter_php (* IMap, ptrs field, etc *)
module A = Ast_php
module Env = Env_interpreter_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: could be put in common.ml *)
module Utils = struct

  let fresh =
    let i = ref 0 in
    fun () -> incr i; !i

  let rec lfold f env l =
    match l with
    | [] -> env, []
    | x :: rl ->
        let env, rl = lfold f env rl in
        let env, x = f env x in
        env, x :: rl

  let opt f acc x =
    match x with
    | None -> acc, None
    | Some v ->
        let acc, v = f acc v in
        acc, Some v
end

(*****************************************************************************)
(* Ptr and globals *)
(*****************************************************************************)

(* Heap access *)
module Ptr = struct

  (* in PHP, variables are pointer to pointer of values *)
  let new_val_ ptrs val_ =
    let x = Utils.fresh() in
    let v = val_ in
    let ptrs = IMap.add x v ptrs in
    let y = Utils.fresh() in
    let v = Vptr x in
    let ptrs = IMap.add y v ptrs in
    ptrs , Vptr y

  let new_val heap val_ =
    let ptrs, v = new_val_ heap.ptrs val_ in
    { ptrs }, v

  let new_ heap =
    new_val heap Vnull


  let get_ heap ptr =
    try
      let v = IMap.find ptr heap.ptrs in
      heap, v
    with Not_found ->
      (* todo: throw exn when in strict? *)
      heap, Vnull

  and set_ heap ptr v =
    { ptrs = IMap.add ptr v heap.ptrs }

  (* 'get' dereference a pointer when the value is a pointer.
   *
   * todo: should have different functions when we know it's always
   * a pointer. Same when we know it's not already in the heap.
   *)
  let get heap ptr =
    match ptr with
    | Vptr n -> get_ heap n
    | Vref s -> get_ heap (ISet.choose s)
    (* Throw exception? no
     * When we do '2 + $x' where $x = 3, the abstract interpreter will
     * have for values for the operand of plus a (Vint 2) and a (Vptr 1)
     * where 1 points to a (Vint 3). The Vint of $x is kinda boxed.
     * But in the code of binaryOp we don't want to handle the many
     * variations of having a Vptr or not, so in many places we
     * call Ptr.get which will dereference the pointer when
     * the value is a boxed integers, or return the value where we
     * already have a "final" value
     *)
    | _ -> heap, ptr

  let set heap ptr v =
    match ptr with
    | Vptr n -> set_ heap n v
    | Vref shared ->
        (* todo: when need that? when have multiple elements here? *)
        let l = ISet.elements shared in
        List.fold_left (fun heap x -> set_ heap x v) heap l
    | _ -> heap

end

(* env.vars access *)
module Var = struct

  let super_globals =
    let l = [
      "$GLOBALS";
      "$_SERVER";
      "$_GET";
      "$_POST";
      "$_FILES";
      "$_COOKIE";
      "$_SESSION";
      "$_REQUEST";
      "$_ENV";

      (* pad: ??? why they are considered as superglobals?
       * todo: use A.special "self" ?
       *)
      "self";
      "parent";
      (* todo? why consider this super globals? why not
       * set $this in env.vars instead?
       *)
      "$this";
    ] in
    let h = Hashtbl.create 23 in
    List.iter (fun x -> Hashtbl.add h x true) l;
    h

  let set_global env s v =
    env.globals := SMap.add s v !(env.globals)

  let unset_global env s =
    env.globals := SMap.remove s !(env.globals)

  let set env s v =
    if Hashtbl.mem super_globals s
    then set_global env s v
    else env.vars := SMap.add s v !(env.vars)

  let unset env s =
    env.vars := SMap.remove s !(env.vars)

  (* get or create a variable in env.vars or env.globals.
   * Returns whether the variable was created.
   * It creates a new variable because that's the semantic
   * of PHP which does not have construction to declare
   * variable. The first use of the variable is its declaration.
   *
   * todo? again have different function when know it's not
   * already in env.vars ?
   *)
  let get env heap str =
    try
      let vars =
        if Hashtbl.mem super_globals str
        then !(env.globals)
        else !(env.vars)
      in
      let v = SMap.find str vars in
      heap, false, v
    with Not_found ->
      let heap, x = Ptr.new_ heap in
      set env str x;
      heap, true, x

  let get_global env heap str =
    try
      let vars = !(env.globals) in
      let v = SMap.find str vars in
      heap, false, v
    with Not_found ->
      let heap, x = Ptr.new_ heap in
      set_global env str x;
      heap, true, x

end

(*****************************************************************************)
(* Unification *)
(*****************************************************************************)

module Order = struct

  let rec value x =
    match x with
    | Vtaint _ -> 32
    | Vany -> 31
    | Vnull -> 0
    | Vabstr ty -> type_ ty

    (* project on corresponding type_ *)
    | Vint _ -> 1
    | Vbool _ -> 2
    | Vfloat _ -> 3
    | Vstring _ -> 4

    | Vptr _ -> 5
    | Vref _ -> 5

    | Vrecord _ -> 6
    | Varray _ -> 6
    | Vmap _ -> 6

    | Vmethod _ -> 39
    | Vobject _ -> 40
    | Vsum _ -> raise Common.Impossible

  and type_ x =
    match x with
    | Tint -> 1
    | Tbool -> 2
    | Tfloat -> 3
    | Tstring -> 4

    | Txhp -> 30

end

(* Should be called more "generalize" or "abstractize" because
 * it just tries to give a more general value, e.g. in
 * $x = true ? 3: 4; the goal is to return a Vabstr Tint which
 * generalize the Vint 3 and Vint 4.
 *)
module Unify = struct

  (* todo: this whole code is complicated and mysterious,
   * need more examples
   *)

  let make_ref ptrs x =
    match x with
    | Vptr n ->
        (match IMap.find n ptrs with
        | Vptr n' ->
            IMap.add n (Vref (ISet.singleton n')) ptrs
        | _ -> ptrs
        )
    | _ -> ptrs

  (*
    The stack is used to keep track of cyclic data structures.
    We need to stop when this is the case.
    Otherwise what we do is we dereference the left pointer
    then dereference the right pointer, unify the two values
    that we found (let's call this new value v).

    Vptr 0 --> Vint
    Vptr 1 --> Vbool
    pointers stack ptrs 0 1

    if(true) {
      $x = 42; &2&1 Vint 42
    } else {
      $x = 55; &2&1 Vint 55
    }
     // the pointer in $x must point to the same abstract value,
     // in this case Vint
     // $x &2&1 Vint
     mais attention
     &3&4 Vint
  *)
  let rec pointers stack ptrs n1 n2 =
    if ISet.mem n1 stack && ISet.mem n2 stack || n1 = n2
    then ptrs, n1
    else
      let v1 = IMap.find n1 ptrs in
      let v2 = IMap.find n2 ptrs in
      let stack = ISet.add n1 stack in
      let stack = ISet.add n2 stack in
      let ptrs, v = value stack ptrs v1 v2 in
      let ptrs = IMap.add n1 v ptrs in
      let ptrs = IMap.add n2 v ptrs in
      ptrs, n1

  and fold_array stack ptrs vl =
    match vl with
    | [] -> ptrs, Vany
    | [x] -> ptrs, x
    | x :: rl ->
        List.fold_left (
        fun (ptrs, v) x ->
          value stack ptrs v x
       ) (ptrs, x) rl

  (* Why not use the order and then do value a b = value b a
   * to factorize some patterns below?
   *)
  and value stack ptrs v1 v2 =
    match v1, v2 with
    | Vany, _x | _x, Vany ->
        ptrs, Vany
    | Vtaint s, _ | _, Vtaint s ->
        ptrs, Vtaint s
    | Vptr n, Vref s | Vref s, Vptr n ->
        value stack ptrs (Vref s) (Vref (ISet.singleton n))
    | Vref s1, Vref s2 ->
        let s = ISet.union s1 s2 in
        let l = ISet.elements s in
        let stack = List.fold_left (fun acc x -> ISet.add x acc) stack l in
        (match l with
        | [] -> raise Common.Impossible
        | [_] -> ptrs, Vref s
        | x :: rl ->
            let ptrs, _ = List.fold_left (fun (ptrs, acc) x ->
                pointers stack ptrs acc x
             ) (ptrs, x) rl
            in
            ptrs, Vref s
        )
    | Vptr n1, Vptr n2 ->
        let ptrs, _n = pointers stack ptrs n1 n2 in
        ptrs, Vptr n1
    | _, _ when v1 == v2 ->
        ptrs, v1
    | Vnull, Vnull ->
        ptrs, Vnull
    | Vabstr ty1, Vabstr ty2 when ty1 = ty2 ->
        ptrs, v1

    | Vbool b1, Vbool b2 when b1 = b2 ->
        ptrs, v1
    | Vbool _, Vabstr Tbool | Vabstr Tbool, Vbool _ | Vbool _, Vbool _ ->
        ptrs, Vabstr Tbool

    | Vint n1, Vint n2 when n1 = n2 ->
        ptrs, v1
    | Vint _, Vabstr Tint | Vabstr Tint, Vint _ | Vint _, Vint _ ->
        ptrs, Vabstr Tint

    | Vfloat f1, Vfloat f2 when f1 = f2 ->
        ptrs, v1
    | Vfloat _, Vabstr Tfloat | Vabstr Tfloat, Vfloat _ | Vfloat _, Vfloat _ ->
        ptrs, Vabstr Tfloat

    | Vstring s1, Vstring s2 when s1 = s2 ->
        ptrs, v1
    | Vstring _, Vabstr Tstring | Vabstr Tstring, Vstring _ | Vstring _, Vstring _ ->
        ptrs, Vabstr Tstring

    | Vrecord m1, Vrecord m2 ->
        let ptrs, m = record stack ptrs m1 m2 in
        ptrs, Vrecord m
    | (Varray _ | Vmap _ as x), Vrecord m
    | Vrecord m, (Varray _ | Vmap _ as x) ->
        let ptrs, k = Ptr.new_val_ ptrs (Vabstr Tstring) in
        let vl = SMap.fold (fun _ y acc -> y :: acc) m [] in
        let ptrs, v = fold_array stack ptrs vl in
        value stack ptrs x (Vmap (k, v))
    | Varray vl1, Varray vl2 ->
        array stack ptrs vl1 vl2
    | Varray l, (Vmap _ as x)
    | (Vmap _ as x), Varray l ->
        let ptrs, k = Ptr.new_val_ ptrs (Vabstr Tint) in
        let ptrs, v = fold_array stack ptrs l in
        value stack ptrs x (Vmap (k, v))
    | Vmap (v1, v2), Vmap (v3, v4) ->
        let ptrs, v1 = value stack ptrs v1 v3 in
        let ptrs, v2 = value stack ptrs v2 v4 in
        let v1 = Vmap (v1, v2) in
        ptrs, v1
    (* this is the only place where we use the 'this' of Vmethod,
     * but julien is not sure it's still useful
     *)
    | Vmethod (st1, m1), Vmethod (st2, m2) ->
        let ptrs, st = value stack ptrs st1 st2 in
        let m = IMap.fold IMap.add m1 m2 in
        ptrs, Vmethod (st, m)
    | Vobject m1, Vobject m2 ->
        let ptrs, m = record stack ptrs m1 m2 in
        ptrs, Vobject m
    | Vsum vl1, Vsum vl2 ->
        let ptrs, vl1 = list stack ptrs vl1 vl2 in
        let v = Vsum vl1 in
        ptrs, v
    | x, Vsum vl2 ->
        let ptrs, vl = list stack ptrs [x] vl2 in
        let v = Vsum vl in
        ptrs, v
    | Vsum vl1, x ->
        let ptrs, vl = list stack ptrs vl1 [x] in
        let v = Vsum vl in
        ptrs, v
    (* pad: dangerous :( lose exhaustive check *)
    | x, y ->
        let ptrs, vl = list stack ptrs [x] [y] in
        let v = Vsum vl in
        ptrs, v

  and record stack ptrs r1 r2 =
    let ptrs, r2 = record_left stack ptrs r1 r2 in
    let ptrs, r1 = record_left stack ptrs r2 r1 in
    ptrs, r1

  and record_left stack ptrs r1 r2 =
    SMap.fold (
    fun x ty (ptrs, acc) ->
      let ptrs, ty' =
        try ptrs, SMap.find x r2
        with Not_found -> Ptr.new_val_ ptrs Vnull
      in
      let ptrs, ty = value stack ptrs ty ty' in
      ptrs, SMap.add x ty acc
   ) r1 (ptrs, r2)

  and array stack ptrs a1 a2 =
    let ptrs, a = arrayl stack ptrs [] (List.rev a1) (List.rev a2) in
    ptrs, Varray a

  and arrayl stack ptrs acc a1 a2 =
    match a1, a2 with
    | [], [] -> ptrs, acc
    | [], x :: rl
    | x :: rl, [] ->
        let ptrs, y = Ptr.new_val_ ptrs Vnull in
        let ptrs, v = value stack ptrs x y in
        arrayl stack ptrs (v :: acc) [] rl
    | x1 :: rl1, x2 :: rl2 ->
        let ptrs, v = value stack ptrs x1 x2 in
        arrayl stack ptrs (v :: acc) rl1 rl2

  and list stack ptrs l1 l2 =
    match l1, l2 with
    | [], l
    | l, [] -> ptrs, l
    | x1 :: rl1, x2 :: rl2 ->
        let o1 = Order.value x1 in
        let o2 = Order.value x2 in
        if o1 < o2
        then
          let ptrs, l = list stack ptrs rl1 l2 in
          ptrs, x1 :: l
        else if o1 > o2
        then
          let ptrs, l = list stack ptrs l1 rl2 in
          ptrs, x2 :: l
        else
          let ptrs, x1 = value stack ptrs x1 x2 in
          let ptrs, rl1 = list stack ptrs rl1 rl2 in
          let l1 = x1 :: rl1 in
          ptrs, l1

  let value heap v1 v2 =
    let ptrs, v = value ISet.empty heap.ptrs v1 v2 in
    { ptrs }, v
end

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* Because assignements can be in conditionals ... have to extract them.
 *
 * todo: could delete that. It was used in a first version of the
 * interpreter where we were unifying heaps. But we don't unify
 * heaps anymore. Moreover it's incomplete.
 *)
module NullNewVars = struct

  let rec stmtl env heap stl =
    List.fold_left (stmt env) heap stl

  and stmt env heap x =
    match x with
    | Block (_, stl, _) -> stmtl env heap stl
    (* todo? enough? what about deeply nested assignements?
     * what if had some 'global' directives in previous statements?
     *)
    | Expr (Assign (Id [s,_], _tok, _), _) when not (SMap.mem s !(env.vars)) ->
        let heap, _, _ = Var.get env heap s in
        heap
    | Expr (AssignOp (Id [s,_], _op, _), _) when not (SMap.mem s !(env.vars)) ->
        let heap, _, _ = Var.get env heap s in
        heap
    | _ -> heap

end

module Copy = struct

  let rec value ptrs x =
    match x with
    | Vtaint _
    | Vany
    | Vnull
    | Vabstr _
    | Vint _  | Vbool _ | Vfloat _| Vstring _
    | Vref _
    | Vsum _
    | Vmap _
    | Vobject _ | Vmethod _
      ->
        ptrs, x
    | Vptr n ->
        let ptrs, v' = value ptrs (IMap.find n ptrs) in
        let n' = Utils.fresh() in
        let ptrs = IMap.add n' v' ptrs in
        ptrs, Vptr n'
    | Vrecord m ->
        let ptrs, m' = SMap.fold (fun k v (ptrs, acc) ->
            let ptrs, v' = value ptrs v in
            ptrs, SMap.add k v' acc
         ) m (ptrs, SMap.empty) in
        ptrs, Vrecord m'
    | Varray vl ->
        let ptrs, vl = Utils.lfold value ptrs vl in
        ptrs, Varray vl

  let value heap v =
    let ptrs, v = value heap.ptrs v in
    { ptrs = ptrs }, v

end

module IsLvalue = struct

  let expr e =
    match e with
    | Id _
    | Var _
    | IdSpecial (This, _)
    | Infix _
    | Postfix _
    | Array_get _
    | Obj_get _
    | Class_get _
    | List _
      -> true

    | Lambda _
    | (Cast (_, _)|CondExpr (_, _, _)|InstanceOf (_, _, _)|New (_, _, _)|ConsArray _
      | Collection _ |Arrow _
      | Xhp _|Ref _|Call (_, _)|Unop (_, _)|Binop (_, _, _)|Assign (_, _, _)
      | AssignOp _
      | Guil _|String _|Double _|Int _|Unpack _
      | IdSpecial (Eval, _)
      ) ->
        false
end

(*****************************************************************************)
(* Toplevel functions *)
(*****************************************************************************)

(* shortcuts *)
let unw = Ast_php.unwrap
let w = Ast_php.wrap_fake
let fake s = Parse_info.fake_info s

(* Allez ... pour faire plaisir a yoyo.
 * pad: not sure we need that.
 *)
let save_excursion env heap f e =
  let env = { Env_interpreter_php.
     file    = ref !(env.Env.file);
     stack   = env.Env.stack;
     vars    = ref !(env.Env.vars);
     globals = ref !(env.Env.globals);
     cfun    = env.Env.cfun;
     safe = ref !(env.Env.safe);
     path = ref !(env.Env.path);
     db = env.db;
  } in
  ignore (f env heap e)

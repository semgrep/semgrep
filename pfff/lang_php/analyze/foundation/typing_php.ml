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
open Common

open Ast_php
module A = Ast_php

open Env_typing_php
open Typing_helpers_php
module Unify = Typing_unify_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module implements a bottom-up type-inference for PHP.
 * It's using union types, polymorphic types, and object types
 * (see Env_typing_php.t)
 * Every functions/classes are sorted in their topological order,
 * and are then typed independently (hence the term "bottom-up").
 *
 * This module is also (ab)used to provide autocompletion.
 *
 * TODO: explain
 *
 *  - the subsitution technique, and why it's better than the alternatives:
 *     * W: simple gauss like, but slow fixpoint, bad error message (too late)
 *     * compose_subst: nice algo, but strong invariant which makes it easy
 *       to make mistake. One has to "thread" the subsitution carefully
 *       while visiting the AST.
 *     * use_ref_a_la_prolog: may not handle cyclic types (necessary for
 *       objects according to julien)
 *     * constraints: complex solver
 *     * leroy levels: ??
 *
 *    Typical algorithms for type inference use equality,
 *    but here we manage sets that grow, so not pure equality but
 *    set inclusion (a constraint). $y = $x means all types of $x
 *    should go in $y.
 *    Julien's techniques makes it easy to manage union types.
 *
 *  - absorbtion (string -> int -> bool -> null)
 *
 *  - it's not really bottom-up, in the sense that a call site
 *    can influence globally the type of the called function or method
 *
 * pad's notes:
 *  - abused strings:
 *    * "$;return"
 *    * "$;tmp"
 *
 * history:
 *  - pad wanted to do type inference for PHP a long time ago, like many
 *    other people at Facebook such as iproctor, yiding, etc. He reads
 *    a few papers on it:
 *     * soft typing for scheme,
 *     * inferring types for dynamic languages javascript/ruby/python/php/...,
 *     * the cartesian product inference algorithm of ole agesen,
 *     * constraint-based type inference of pottier
 *     * didier's remy extensible record typing
 *     * ...
 *    He also reread chapters in general books on typing (pierce), but failed
 *    to read something that explains simply how to handle union types.
 *    The traditional algorithms have a strong equality model, not a
 *    set model, which is required for union types.
 *
 *  - pad did some type inference by cheating, by abusing xdebug to
 *    extract type information from traces
 *
 *  - julien wanted first to (ab)use the (top-down) abstract interpreter to
 *    also do type inference, but the interpreter is kinda hacky already
 *    and full of heuristics. Pad had the idea of trying
 *    a bottom-up approach, but failed to know how to use W or
 *    compose_subst with union types that grows. Julien did it.
 *
 *  - algo unification and managing subsitution a la ?? coq?
 *
 * todo: each time we use 'any' below, it's probably a todo
 *
 * todo: https://github.com/tomprimozic/type-systems
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
exception UnknownEntity of string

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* stuff that is used by chiara's code, to remove. I think
 * The AEnv stuff should be removed too.
 *)
let pi = Some (Cst_php.fakeInfo "todo")
let pi_loc = Cst_php.fakeInfo "todo"

let fake s = Parse_info.fake_info s
let fb = AST_generic.fake_bracket
let unbracket (_, x, _) = x

(*****************************************************************************)
(* Preparing work *)
(*****************************************************************************)
(* for the topological order and bottom-up approach *)
let add_defs_code_database_and_update_dependencies env stl =
  List.iter (function
  | ClassDef def ->
      Graph.class_def env.graph def;
      Classes.add env (A.unwrap def.c_name) def
  | FuncDef def ->
      Graph.func_def env.graph def;
      Functions.add env (A.unwrap def.f_name) def
  | ConstantDef _ ->
      raise Common.Todo
  | _ -> ()
  ) stl

(*****************************************************************************)
(* Collect *)
(*****************************************************************************)

(* The substitution grows and grows, so we need to do stuff? *)
module Collect = struct

  type mem = {
      tys: (t, t) Hashtbl.t;
      prims: (prim_ty, prim_ty) Hashtbl.t;
    }

  let rec ty env subst tenv mem depth t =
(*    try Hashtbl.find mem.tys t
    with Not_found ->
      let t' = *)
        match t with
        | Tvar n ->
            let n = Subst.get env n in
            if IMap.mem n !subst then Tvar (IMap.find n !subst) else
            let v = fresh() in
            subst := IMap.add n v !subst;
            let t = ty env subst tenv mem depth (TEnv.get env n) in
            tenv := IMap.add v t !tenv;
            Tvar v
        | Tsum l -> Tsum (List.map (prim_ty env subst tenv mem depth) l)
(*      in
      Hashtbl.add mem.tys t t';
      t' *)

  and prim_ty env subst tenv mem depth t =
    try Hashtbl.find mem.prims t
    with Not_found ->
      if depth > 3 then t else
      let t' =
        match t with
        | Tabstr _
        | Tienum _
        | Tsstring _
        | Tsenum _ as x -> x
        | Trecord m -> Trecord (SMap.map (ty env subst tenv mem depth) m)
        | Tarray (s, t1, t2) ->
            Tarray (s,
                   ty env subst tenv mem depth t1,
                   ty env subst tenv mem depth t2)
        | Tfun (tl, t) -> Tfun (
            List.map (fun (s, x) -> s, ty env subst tenv mem depth x) tl,
            ty env subst tenv mem depth t)
        | Tobject m ->
            let m = SMap.map (ty env subst tenv mem depth) m in
            Tobject m
        | Tclosed (s, m) ->
            Tclosed (s, SMap.map (ty env subst tenv mem depth) m)
      in
      Hashtbl.add mem.prims t t';
      t'

  let ty count env subst tenv mem t =
    incr count;
    if !count mod 100 = 0
    then (Printf.printf "Collected [%d/%d]\n" !count !(env.count); flush stdout)
    else ();
    ty env subst tenv mem 0 t

  let run env =
    Printf.printf "Collecting [Cumulated: %f]: " !(env.cumul); flush stdout;
    let t = Sys.time() in
    let subst = ref IMap.empty in
    let tenv = ref IMap.empty in
    let ty = ty (ref 0) in
    let mem = { tys = Hashtbl.create 1024; prims = Hashtbl.create 1024 } in
    let lenv = SMap.map (ty env subst tenv mem) !(env.vars) in
    let genv = SMap.map (ty env subst tenv mem) !(env.globals) in
    env.tenv := !tenv;
    env.subst := !subst;
    env.globals := genv;
    env.vars := lenv;
    Printf.printf "Compacting: "; flush stdout;
    Gc.compact();
    Printf.printf "DONE\n"; flush stdout;
    let t = Sys.time() -. t in
    env.cumul := t +. !(env.cumul);
    env.collect_count := 0;
    Printf.printf "%f\n" t; flush stdout

  let collect env =
    incr env.collect_count;
    if !(env.collect_count) >= 100000
    then run env
    else ()
end

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

let rec infer_type_definition env str =
  match () with
  | _ when Classes.mem env str && not (GEnv.mem_class env str) ->
      class_id env str
  | _ when Functions.mem env str && not (GEnv.mem_fun env str) ->
      func_id env str
  | _ when SSet.mem ("^Fun:" ^ str) !(env.builtins) -> ()
  | _ ->
      (* TODO:
      if env.strict
      then failwith ("infer_type_definition, unknown def: " ^ str)
      *)
      ()

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
and stmtl env l =
  List.iter (stmt env) l

and stmt env= function
  | TypeDef _ -> failwith "no support for typedefs in type inferencer"
  | NamespaceDef _ | NamespaceUse _ -> failwith "no support for namespace yet"
  | Expr (e, _) -> iexpr env e
  | Block (_,stl,_) -> stmtl env stl
  | If (_, e, st1, st2) ->
      (* todo? should we unify e with bool? *)
      iexpr env e;
      stmt env st1;
      stmt env st2
  | While (_, e, stl) ->
      iexpr env e;
      stmtl env stl
  | Do (_, stl, e) ->
      stmtl env stl;
      iexpr env e
  | For (_, el1, el2, el3, stl) ->
      iexprl env el1;
      iexprl env el2;
      iexprl env el3;
      stmtl env stl
  | Switch (_, e, cl) ->
      let t = expr env e in
      casel env t cl
  | Foreach (_, e1, _, pat, stl) ->
      let a = expr env e1 in
      let a' =
        match pat with
          (*
        | None -> array (Tvar (fresh()), expr env e2)
        | Some v -> array (expr env e2, expr env v)
          *)
        | _ -> raise Todo
      in
      let _ = Unify.unify env a a' in
      stmtl env stl
  | Return (_, None) -> ()
  | Return (_, Some (ConsArray (avl))) ->
      let e = ConsArray(avl) in
      let id = AEnv.create_ai env e in
      let ti = (pi, Env_typing_php.ReturnValue) in
      let _ = AEnv.set env id ti in
      iexpr env (Assign (Id [(wrap_fake "$;return")], fake "=", e))
  | Return (_, Some e) ->
      let id = AEnv.create_ai env e in
      let ti = (pi, Env_typing_php.ReturnValue) in
      let _ = AEnv.set env id ti in
      iexpr env (Assign (Id [(wrap_fake "$;return")], fake "=", e))
  | Break (_, eopt) | Continue (_, eopt) -> expr_opt env eopt
  | Throw (_, e) -> iexpr env e
  | Try (_, stl, cl, fl) ->
      stmtl env stl;
      catchl env cl;
      finallyl env fl
  | StaticVars (_, svarl) ->
      List.iter (fun (s, e) ->
        match e with
        | None -> ()
        | Some e ->
            iexpr env (Assign (Id [s], fake "=", e))
     ) svarl
  | Global (_, el) ->
      List.iter (function
        | Id [(x, tok)] ->
            let gid = A.remove_first_char x in
            let gl = Array_get (Id [(wrap_fake "$GLOBALS")],Some(String (gid,tok)))in
            let assign = Assign (Id [(x, tok)], fake "=", gl) in
            iexpr env assign
        | e -> iexpr env e
     ) el
  | ClassDef cd ->
      class_def env cd
  | FuncDef fd ->
      func_def env fd
  | ConstantDef _ ->
      raise Common.Todo

and expr_opt env = function
  | None -> ()
  | Some x -> iexpr env x

and casel env t l = List.iter (case env t) l

and case env t = function
  | Case (_, e, stl) ->
      let t' = expr env e in
      let _ = Unify.unify env t t' in
      stmtl env stl
  | Default (_, stl) -> stmtl env stl

and catchl env l = List.iter (catch env) l
and catch env (_, _, _, stl) = stmtl env stl

and finallyl env l = List.iter (finally env) l
and finally env (_, stl) = stmtl env stl

and exprl env l = List.map (expr env) l
and iexprl env l = ignore (exprl env l)

and iexpr env e = ignore (expr env e)

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env e =
  expr_ env false e

and expr_ env _lv = function

  | IdSpecial (Eval, _) -> raise Todo

  | Id [(("true" | "false"),_)] -> bool
  | Int _ -> int
  | Double _ -> float
  | String (s, _) ->
      (match () with
      | _ when env.auto_complete && has_marker env s ->
          let t = Tvar (fresh()) in
          env.show := Sauto_complete (s, t);
          t
      | _ when String.contains s '<' -> thtml
      | _                            -> Tsum [Tsstring (SSet.singleton s)]
      )
  | Guil (_, el, _) ->
      List.iter (encaps env) el;
      string

  | Binop (e1, (bop, _), e2) ->
      let t1 = expr env e1 in
      let t2 = expr env e2 in
      binaryOp env t1 t2 bop
  | Unop ((uop, _), e) ->
      let _ = expr env e in
      unaryOp uop
  | Infix (_, e) -> expr env e
  | Postfix (_, e) -> expr env e

  | CondExpr (e1, e2, e3) ->
      iexpr env e1;
      let e2 = expr env e2 in
      let e3 = expr env e3 in
      Unify.unify env e2 e3
  | Cast (pty, e) ->
      iexpr env e;
      ptype env pty

  | Ref (_, e) -> expr env e

  | Unpack _ -> raise Todo

  | (Id [(s, tok)] | Var (s, tok)) as elt ->
      let is_marked = has_marker env s in
      (match elt with
      | _ when env.infer_types && is_marked ->
          let s = get_marked_id env s in
          let t = expr env (Id [(s, tok)]) in
          env.show := Stype_infer t;
          t
      | _ when env.auto_complete && is_marked ->
          (match elt with
          | Var _ ->
            let locals =
              SMap.fold (fun x _ acc -> SSet.add x acc) !(env.vars) SSet.empty
            in
            env.show := Slocal (get_marked_id env s, locals)
          | Id _ ->
            env.show := Sglobal (get_marked_id env s);
          | _ -> raise Common.Impossible
          );
        any

      (* a local variable, lookup in env. This can create a new
       * variable and assigns it a fresh type variable. This is
       * how PHP works ... there is no variable declaration.
       *)
      | Var _ ->
          Env.get env s
      | _ when Env.mem env s ->
          Env.get env s

      (* this covers functions but also builtin constants such as null *)
      | _ when GEnv.mem_fun env s -> GEnv.get_fun env s
      | _ when GEnv.mem_class env s -> GEnv.get_class env s
      | _ when Classes.mem env s || Functions.mem env s ->
          infer_type_definition env s;
          expr env (Id [(s, tok)])
      | _ ->
          if env.strict
          then raise (UnknownEntity s);
          any
      )
  | IdSpecial (This, tok) -> let name = "$this", tok in expr env (Var (name))

  (*Array_get returns the type of the values of the array*)
  (* Array access without a key *)
  | Array_get (e, None) ->
      let id = AEnv.create_ai env e in
      let t1 = expr env e in
      let v = Tvar (fresh()) in
      let t2 = array (int, v) in
      let ti = (pi, Env_typing_php.NoIndex (v)) in
      let _ = Unify.unify env t1 t2 in
      let _ = AEnv.set env id ti in
      v
  (* Array access with const as key *)
  | Array_get (e, Some (Id [(s,tok)])) when s.[0] <> '$' ->
      let id = AEnv.create_ai env e in
      let v = expr env (Array_get (e, Some (String (s,tok)))) in
      let ti = (pi, Env_typing_php.Const v) in
      let _ = AEnv.set env id ti in
      v

  | Array_get (Id [(s,y)], Some (String (x, _)))
      when Hashtbl.mem Builtins_typed_php.super_globals s ->

      let id = AEnv.create_ai env (Id [(s, y)]) in
      let ti = (pi, Env_typing_php.UnhandledAccess) in
      let _ = AEnv.set env id ti in
      let marked = env.auto_complete && has_marker env x in
      let t1 = GEnv.get_global env s in
      if marked then (env.show := Sauto_complete (x, t1); any) else
      let v = Tvar (fresh()) in
      let t2 = srecord (x, v) in
      let _ = Unify.unify env t1 t2 in
      let v = Instantiate.approx env ISet.empty v in
      v

  | Array_get (e, Some (String (s, _)))->
      let id = AEnv.create_ai env e in
      let marked = env.auto_complete && has_marker env s in
      let t1 = expr env e in
      if marked then (env.show := Sauto_complete (s, t1); any) else
      let v = Tvar (fresh()) in
      let t2 = srecord (s, v) in
      let ti = (pi, Env_typing_php.ConstantString (v)) in
      let _ = Unify.unify env t1 t2 in
      let _ = AEnv.set env id ti in
      v

  (* disguised array access *)
  | Call (Id [(("idx" | "edx" | "adx" | "sdx"),_)], (_,(e :: k :: r),_)) ->
      let e = expr env (Array_get (e, Some k)) in
      (match r with
      | [] -> e
      | x :: _ -> Unify.unify env (expr env x) e
      )
  (* Array access with variable or constant integer *)
  | Array_get (e, Some k) ->
      let id = AEnv.create_ai env e in
      let t1 = expr env e in
      let k = expr env k in
      let v = Tvar (fresh()) in
      let t2 = array (k, v) in
      let ti = (pi, Env_typing_php.VarOrInt(k, v)) in
      let _ = Unify.unify env t1 t2 in
      let _ = AEnv.set env id ti in
      v

  (* ?? why this special case? the code handling Id() should do the
   * GEnv.get_class so no need this special case.
   *)
  | Class_get (Id [(c,_)], _, Id [(x,_)])
      when c <> special "self" && c <> special "parent" ->
      let marked = env.auto_complete && has_marker env x in
      let t1 = GEnv.get_class env c in
      if marked then (env.show := Sauto_complete (x, t1); any) else
      let v = Tvar (fresh()) in
      let t2 = sobject (x, v) in
      let _ = Unify.unify env t1 t2 in
      v
  | Class_get (e, _, Id [(x,_)]) | Obj_get (e, _, Id [(x,_)]) ->
      let marked = env.auto_complete && has_marker env x in
      let t1 = expr env e in
      if marked then (env.show := Sauto_complete (x, t1); any) else
      let v = Tvar (fresh()) in
      let t2 = sobject (x, v) in
      let _ = Unify.unify env t1 t2 in
      v
  | Class_get _ | Obj_get _ ->
      any


  | Assign (Array_get(e, a), _, e2) ->
    let e1 = Array_get(e, a) in
    let id = AEnv.create_ai env e in
    let t1 = expr env e1 in
    let t2 = expr env e2 in
    let ti = (pi, Env_typing_php.Value(t2)) in
    let _ = AEnv.set env id ti in
    let t = Unify.unify env t1 t2 in
    t

  | Assign (Id [("$;return", tok)], _, ConsArray(avl))  -> (
    let e1 = Id [("$;return", tok)] in
    let e2 = ConsArray(avl) in
      let t1 = expr env e1 in
      let t2 = expr env e2 in
      let t = Unify.unify env t1 t2 in
      match pi with
      | Some _p ->
          let e = ConsArray(avl) in
          let id = AEnv.create_ai env e in
          let tl = List.map (array_declaration env id pi) (unbracket avl) in
          let ti = (pi, Env_typing_php.Declaration(tl)) in
          let _ = AEnv.set env id ti in
          t
      | None -> t
  )

  | Assign (e1, _, ConsArray(avl)) ->
      let e2 = ConsArray (avl) in
      let t1 = expr env e1 in
      let t2 = expr env e2 in
      let t = Unify.unify env t1 t2 in
      t

  | Assign (e1, _, e2) ->
      let t1 = expr env e1 in
      let t2 = expr env e2 in
      let t = Unify.unify env t1 t2 in
      t

  | AssignOp (e1, bop, e2) ->
      expr env (Assign (e1, fake "=", Binop (e1, bop, e2)))

  | ConsArray (avl) ->
      let t = Tvar (fresh()) in
      let t = List.fold_left (array_value env) t (unbracket avl) in
      (match pi with
      | Some _p ->
          let e = ConsArray(avl) in
          let id = AEnv.create_ai env e in
          let tl  = List.map (array_declaration env id pi) (unbracket avl) in
	  let ti = (pi, Env_typing_php.Declaration(tl)) in
          let _ = AEnv.set env id ti in
          t
	| None -> t
      )

(* old code of julien's intern? still needed?
      (match id with
      | None ->(
	match pi with
	| Some(p) ->
          let e = ConsArray(id, avl) in
          let id = AEnv.create_ai env e in
          let tl  = List.map (array_declaration env id pi) avl in
	  let ti = (pi, Env_typing_php.Declaration(tl)) in
          let _ = AEnv.set env id ti in
          t
	| None -> t )
      | Some(Id [("$;return", _)]) -> t
      | Some(e) ->
        let id = AEnv.create_ai env e in
        let tl  = List.map (array_declaration env id pi) avl in
	let ti = (pi, Env_typing_php.Declaration(tl)) in
        let _ = AEnv.set env id ti in
        t)
*)
  | Collection _ -> failwith "Collection is not implemented - complain to pieter@"

  | Arrow (_e1,_,  _e2) -> failwith "Todo: Arrow"
  | List el ->
      let t = Tvar (fresh()) in
      let el = List.map (expr env) (unbracket el) in
      let t = List.fold_left (Unify.unify env) t el in
      array (int, t)

  | Call (e, (_,[Id [("JUJUMARKER",_)] ],_)) ->
      env.show := Sargs (expr env e);
      any
  | Call (e, (_,el,_)) ->
      let f = expr env e in
      let f = Instantiate.ty env ISet.empty f in
      let v = Tvar (fresh()) in
      let f' = fun_ (exprl env el) v in
      let _ = Unify.unify env f f' in
      v

  | Xhp x ->
      xml env x;
      let t = expr env (New (fake "new", Id [(x.xml_tag)], [])) in
      t

  | New (_, Id [(x,_)], _) when env.auto_complete && has_marker env x ->
      env.show := Sglobal (get_marked_id env x);
      any
  | New (_, x, el) ->
      let v = "$;tmp"^(string_of_int (fresh())) in
      let obj = Class_get (x, fake "::", Id [(wrap_fake "__obj")]) in
      iexpr env (Assign (Var (wrap_fake v), fake "=", obj));
      iexpr env (Call (Obj_get (obj, fake ".", Id [(wrap_fake "__construct")]), fb el));
      let t = expr env (Id [(wrap_fake v)]) in
      let set = match x with
        | Id [(c,_)] when c.[0] <> '$' -> SSet.singleton c
        | _ -> SSet.empty
      in
      Unify.unify env t (Tsum [Tclosed (set, SMap.empty)])

  | InstanceOf (_, e1, e2) ->
      iexpr env e1;
      iexpr env e2;
      bool
  (* TODO *)
  | Lambda _ ->
      any

  | Id _ -> failwith "no support for namespace yet"

and encaps env e =
  let t = expr env e in
  ignore (Unify.unify env t string)

and array_value env t = function
  | Arrow (String (s,_), _, e) ->
      let t' = srecord (s, (expr env e)) in
      Unify.unify env t t'
  | Arrow (e1, _, e2) ->
      let t' = array (expr env e1, expr env e2) in
      Unify.unify env t t'
  | e ->
      let t' = array (int, expr env e) in
      Unify.unify env t t'

and array_declaration env id pi = function
  | Arrow (e1, _, e2) ->
      let t1 = expr env e1 in
      let t2 = expr env e2 in
      let aa = (pi, DeclarationKValue(t1, t2)) in
      AEnv.set env id aa;
      t2
  | e ->
      let t = expr env e in
      let aa = (pi, DeclarationValue (t)) in
      AEnv.set env id aa;
      t

and ptype _env (x, _tok) =
  match x with
  | Cst_php.BoolTy -> bool
  | Cst_php.IntTy -> int
  | Cst_php.DoubleTy -> float
  | Cst_php.StringTy -> string
  | Cst_php.ArrayTy -> Tsum [Trecord SMap.empty]
  | Cst_php.ObjectTy -> Tsum [Tobject SMap.empty]

and binaryOp env t1 t2 = function
  | ArithOp op -> 
     if G.is_boolean_operator op
     then begin
       logicalOp env t1 t2 op;
       bool
     end else Unify.unify env t1 t2
  | BinaryConcat ->
      Unify.unify env t1 t2
  | CombinedComparison ->
      Unify.unify env t1 t2 |> ignore;
      int

and logicalOp env t1 t2 = function
  | G.Lt | G.Gt | G.LtE | G.GtE
  | G.Eq | G.NotEq
  | G.PhysEq | G.NotPhysEq ->
      ignore (Unify.unify env t1 t2)
  | G.And | G.Or | G.Xor
  | _ ->
      (* ?? why nothing there? *)
      ()

and unaryOp = function
  | G.Plus | G.Minus | G.BitNot -> int
  | G.Not -> bool
  | _ -> raise Impossible


and xhp env = function
  | XhpText _ -> ()
  | XhpExpr e -> ignore (expr env e)
  | XhpXml x -> xml env x

and xml env x =
  List.iter (fun (_, x) -> xhp_attr env x) x.xml_attrs;
  List.iter (xhp env) x.xml_body

and xhp_attr env = function
  | Guil (_, el, _) -> List.iter (encaps env) el
  | e ->
      let t = expr env e in
      ignore (Unify.unify env t string)

(* ---------------------------------------------------------------------- *)
(* Functions *)
(* ---------------------------------------------------------------------- *)
and func_id env fname =
  if GEnv.mem_fun env fname then ()
  else
    try func_def env (Functions.get env fname)
    with Not_found ->
      if env.strict
      then raise (UnknownEntity fname);
      GEnv.set_fun env fname (Tvar (fresh()))

and func_def env fd =
  if not (GEnv.mem_fun env (A.unwrap fd.f_name)) && env.verbose then begin
    incr env.count;
    pr (spf "Typing function(%d/%d)[%d]: %s"
           !(env.count) !(env.total) env.depth (A.unwrap fd.f_name));
  end;
  Collect.collect env;
  let env = { env with vars = ref SMap.empty } in
  let pl = List.map (parameter env) fd.f_params in
  let ret = fresh() in
  let return = Tvar ret in
  let f = Tsum [Tfun (pl, return)] in
  GEnv.set_fun env (A.unwrap fd.f_name) f;
  (* Set the function name in aenv_fun for the purpose of guessing arrays *)
  ignore(AEnv.set_fun env (A.unwrap fd.f_name));
  ignore(AEnv.create_ai_params env (fd.f_params));
  ignore(AEnv.set_funs env ((AEnv.get_fun env), (AEnv.get_class env),pi_loc));
  (* todo? do we need that? if the toplogical sort has been done
   * correctly we should not need that no?
   * We can have some cycles, so the topological sort is not
   * enough. We need to process the dependencies. The topological
   * sort is just some kind of optimisations (to converge more quicky??)
   *)
  List.iter (infer_type_definition env)
    (Graph.get_deps !(env.graph) (A.unwrap fd.f_name));
  Env.set env "$;return" return;
  stmtl env fd.f_body;
  make_return env ret;
  GEnv.set_fun env (A.unwrap fd.f_name) (Generalize.ty env ISet.empty f)

and make_return env r =
  match TEnv.get env r with
  (* ??? *)
  | Tsum _ -> TEnv.set env r null
  | _ -> ()

and parameter env p =
  let rec hint = function
    | None -> Tvar (fresh())
    | Some (Hint [(x, tok)]) ->
        (try get_hard_object env x
        with Not_found ->
          expr env (New (fake "new", Id [(x, tok)], [])))
    | Some (Hint _) -> failwith "no support for namespace yet"
    | Some (HintArray _) ->
        expr env (ConsArray (fb []))

    (* don't handle type extensions *)
    | Some (HintQuestion _)
    | Some (HintTuple _)
    | Some (HintCallback _) -> Tvar (fresh())
    | Some (HintShape _) -> failwith "no support for shape yet"
    | Some (HintTypeConst _) -> failwith "no support for type consts"
    | Some (HintVariadic (_, x)) -> array (int, hint x)
  in
  let pval = hint p.p_type
  in
  (match p.p_default with
  | None -> ()
  | Some (ConsArray(avl)) ->
      let e = ConsArray(avl) in
      ignore (Unify.unify env pval (expr env e))
  | Some e -> ignore (Unify.unify env pval (expr env e))
  );
  Env.set env (A.unwrap p.p_name) pval;
  (A.unwrap p.p_name), pval

(* ---------------------------------------------------------------------- *)
(* Classes *)
(* ---------------------------------------------------------------------- *)
and class_id env x =
  if GEnv.mem_class env x then () else
  if not (Classes.mem env x) then () else
  class_def env (Classes.get env x)

and get_hard_object env c =
  let class_ = GEnv.get_class env c in
  let class_ = match class_ with Tsum [Tobject o] -> o | _ -> raise Not_found in
  SMap.find "__obj" class_

(* The type of an object is always in the field __obj of the class;
 * all the other fields are static methods/vars.
 *)
and get_object = function
  | Tsum [Tobject o] when SMap.mem "__obj" o ->
      (match SMap.find "__obj" o with Tsum [Tobject o] -> o
      | _ -> SMap.empty)
  | _ -> SMap.empty
and get_class_bis = function
  | Tsum [Tobject o] -> o
  | _ -> SMap.empty

and get_class env x =
  class_id env x;
  GEnv.get_class env x

and class_def env c =
  (* Printf.printf "Class: %s\n" (A.unwrap c.c_name);*)
  if GEnv.mem_class env (A.unwrap c.c_name) then () else begin
  GEnv.set_class env (A.unwrap c.c_name) any;
  ignore(AEnv.set_class env (A.unwrap c.c_name));
  Collect.collect env;
  let parent, parent_name =
    match c.c_extends with
    | Some ht ->
      let (x, _) = name_of_class_name ht in
      get_class env x, x
    | None -> Tvar (fresh()), "" in
  if env.verbose then begin
    incr env.count;
    Printf.printf "Typing class(%d/%d)[%d]: %s\n"
      !(env.count) !(env.total) env.depth (A.unwrap c.c_name);
    flush stdout;
  end;
  let env = { env with vars = ref SMap.empty } in
  let class_ = get_class_bis parent in
  let obj_parent = get_object parent in

  (* Adding traits *)
  let traits =
    List.map (fun ht ->
      let  (x, _) = name_of_class_name ht in
      get_object (get_class env x)
    ) c.c_uses in
  let obj_parent = List.fold_right (SMap.fold SMap.add) traits obj_parent in

  (* Declarations *)
  ignore(AEnv.set_fun env "");
  let is_enum = c.c_variables = [] && c.c_methods = [] in
  let ien, sen = List.fold_left (constant_enum is_enum c.c_name) (SSet.empty, SSet.empty) c.c_constants in
  let class_ = List.fold_left (constant is_enum env ien sen) class_ c.c_constants in
  let class_ = List.fold_left (class_vars true env) class_ c.c_variables in
  let class_ = List.fold_left (method_decl true env) class_ c.c_methods in

  let obj = List.fold_left (class_vars false env) obj_parent c.c_variables in
  let obj = List.fold_left (method_decl false env) obj c.c_methods in

  let this = Tsum [Tclosed (SSet.singleton (A.unwrap c.c_name), obj)] in
  let self = Unify.unify env parent (Tsum [Tobject class_]) in

  GEnv.set_class env (A.unwrap c.c_name) self;
  Env.set env (special "self") self;
  Env.set env (special "parent") (Tsum [Tobject obj_parent]);
  Env.set env "$this" this;

  let class_ = List.fold_left (method_def true env) class_ c.c_methods in
  let obj = List.fold_left (method_def false env) obj c.c_methods in

  let obj = SMap.map (fun t -> Generalize.ty env ISet.empty t) obj in
  let privates = List.fold_left private_vars SSet.empty c.c_variables in
  let privates = List.fold_left private_methods privates c.c_methods in
  let obj = filter_privates privates obj in

  (* CHEATING :-) *)
  let obj = SMap.map (cheat_method env parent_name this) obj in

  let obj = Tsum [Tobject obj] in
(*  let obj = Generalize.ty env obj in
  let obj = Tlazy obj in *)
(* XXX
 let class_ = SMap.map (fun t -> Generalize.ty env ISet.empty t) class_ in
*)
  let class_ = filter_privates privates class_ in
  let class_ = SMap.add "__obj" obj class_ in
  let class_ = (Tsum [Tobject class_]) in
(*  let class_ = Generalize.ty env class_ in
  make_globals env; *)
  GEnv.set_class env (A.unwrap c.c_name) class_
  end

and private_vars privates cv =
  if is_private cv.cv_modifiers
  then SSet.add (A.unwrap cv.cv_name) privates
  else privates

and private_methods privates m =
  if is_private m.m_modifiers
  then SSet.add (A.unwrap m.f_name) privates
  else privates

and filter_privates privates obj =
  SMap.fold (fun x t acc ->
    if SSet.mem x privates
    then acc
    else SMap.add x t acc
 ) obj SMap.empty

and constant_enum _is_enum cname (ien, sen) cst =
  let x = A.unwrap cst.cst_name in
  let e:(expr option) = cst.cst_body in
  match e with
  | Some (Int _) -> SSet.add (A.unwrap cname^"::"^x) ien, sen
  | Some (String _) -> ien, SSet.add (A.unwrap cname^"::"^x) sen
  | Some _ | None -> ien, sen

and constant is_enum env ien sen acc cst =
  let x = A.unwrap cst.cst_name in
  match cst.cst_body with
  | Some (Int _) when is_enum -> SMap.add x (Tsum [Tienum ien]) acc
  | Some (String _) when is_enum -> SMap.add x (Tsum [Tsenum sen]) acc
  | Some e -> SMap.add x (expr env e) acc
  | None -> acc

and class_vars static env acc c =
  if static <> is_static c.cv_modifiers
  then acc
  else
    let ((s, _tok), e) = (c.cv_name, c.cv_value) in
    let t = match e with
      | None -> Tvar (fresh())
      | Some (ConsArray(avl)) ->
          expr env (ConsArray(avl))
      | Some x -> expr env x
    in
    let s = if static then s else A.remove_first_char s in
    SMap.add s t acc

and method_decl static env acc m =
  if static <> is_static m.m_modifiers
  then acc
  else
    let pl = List.map (parameter env) m.f_params in
    let ret = fresh() in
    let f = afun pl (Tvar ret) in
    SMap.add (A.unwrap m.f_name) f acc

(* TODO: factorize with func_def ? *)
and method_def static env acc m =
  if static <> is_static m.m_modifiers
  then acc
  else
    let env_cpy = !(env.vars) in
    let pl = List.map (parameter env) m.f_params in
    let ret = fresh() in
    let return = Tvar ret in
    Env.set env "$;return" return;
    stmtl env m.f_body;
    make_return env ret;
    let f = afun pl (Env.get env "$;return") in
    let _ = Unify.unify env (SMap.find (A.unwrap m.f_name) acc) f in
    env.vars := env_cpy;
    SMap.add (A.unwrap m.f_name) f acc

(*
 * When we have:
 *   class A { function foo() { return $this; } }
 *   class B extends A { }
 * without doing anything special, the return type of B::foo would
 * be A, but we actually want B. This is useful for completion purpose,
 * and returning $this is a very frequent idiom in our codebase.
 * Hence this cheat_method function below.
 *)
and cheat_method env parent this m =
  match m with
  | Tsum [Tfun (x, Tsum [Tclosed (s, _)])] when SSet.mem parent s ->
      let v = Tvar (fresh()) in
      let v = Unify.unify env v this in
      Tsum [Tfun (x, v)]
  | x -> x

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let infer_using_topological_sort_dependencies env =
  let l = Dependencies_toposort_php.TopoSort.sort env.graph in
  List.iter (infer_type_definition env) l;
  ()

let infer_using_topological_sort_dependencies_and_save_typingbin env =
  Printf.printf "Topological sort:  "; flush stdout;
  let l = Dependencies_toposort_php.TopoSort.sort env.graph in
  Printf.printf "DONE\n"; flush stdout;
  env.total := List.length l;
  List.iter (infer_type_definition env) l;
  if env.debug then Print2.penv env;
  let oc = open_out "typing_env.bin" in
  Printf.printf "Saving environment (typing_env.bin): "; flush stdout;
  Collect.run env;
  GEnv.save env oc;
  close_out oc;
  Printf.printf "DONE\n"

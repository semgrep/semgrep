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
open Env_interpreter_php
open Abstract_interpreter_php_helpers

module A = Ast_php
module G = AST_generic
module Env = Env_interpreter_php
module H = Abstract_interpreter_php_helpers
module CG = Callgraph_php2
module Trace = Tracing_php

exception ForeachWithList of string

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Abstract interpreter for PHP, with hooks for tainting analysis
 * (to find XSS holes), and hooks for callgraph generation.
 *
 * An abstract interpreter kinda mimics a normal interpreter by
 * also executing a program, in a top-down manner, modifying a heap,
 * managing local and global variables, etc, but maintains "abstract"
 * values for variables instead of concrete values as in a regular
 * interpreter. See env_interpreter_php.ml.
 *
 * For instance on 'if(cond()) { $x = 42; } else { $x = 3;}'
 * the abstract interpreter will actually execute both branches and
 * merge/unify the different values for the variable in a more
 * abstract value. So, while processing the first branch the interpreter
 * will add a new local variable $x, allocate space in the abstract
 * heap, and sets its value to the precise (Vint 42). But after
 * both branches, the abstract interpreter will unify/merge/abstract
 * the different values for $x to a (Vabstr Tint) and from now on,
 * the value for $x will be that abstract.
 *
 * Two key concepts are the different level of abstractions
 * (resulting from unification/generalization values), and
 * reaching a fixpoint.
 *
 * References:
 *  - http://en.wikipedia.org/wiki/Abstract_interpretation
 *
 * Actually the unify/merge/abstract in the example above will happen
 * as soon as processing the else branch in the current algorithm.
 * So there will be no (Vint 3) in the heap. See tests/php/ia/if.php.
 *
 * The algorithm is kinda:
 *  - flow insensitive, because??
 *  - path insensitive, because we don't look for instance
 *    at the resulting value of a condition in a 'if' to determine
 *    unreachable path
 *  - BUT context sensitive, because we treat different calls
 *    to the same function differently (we unroll each call).
 *    There is a limit on the depth of the call stack though,
 *    see max_depth.
 *
 * For a bottom-up approach see typing_php.ml.
 *
 * To help you debug the interpreter you can put some
 * 'var_dump($x)' in the PHP file to see the abstract
 * value of a variable at a certain point in the program.
 *
 * pad's notes:
 *  - strings are sometimes (ab)used to not only represent
 *    variables and entities but also special variables:
 *    * "*return*", to communicate the return value to the caller
 *    * "*array*, to build an array
 *    * "*myobj*, to build an object
 *    * "*BUILD*", to call the 'new' method of a class
 *    * special "self"/"parent", in env.globals
 *    * "$this", also in env.globals
 *  - How the method lookup mechanism works? there is no lookup,
 *    instead at the moment where we build the object, we put
 *    all the methods of the parents in the new object. But then
 *    what about the use of self:: or parent:: when executing the
 *    code of a parent method? The references to self and parent
 *    are in the closure of the method and are pointers to
 *    the fake object that represents the class.
 *  - the id() function semantic is hardcoded
 *
 * TODO:
 *  - ask juju about the many '??' in this file
 *  - the places where expect a VPtr, and so need to call Ptr.get,
 *    or even a VptrVptr and so where need to call Ptr.get two times,
 *    and the places where expect a final value is not clear.
 *  - before processing the file, maybe should update the code database
 *    with all the entities in the file, cos when one process a script,
 *    many scripts have a main() or usage() but the code database
 *    stores only one.
 *  - $x++ is ignored (we don't really care about int for now)
 *  - many places where play with $ in s.(0)
 *  - C-s for Vany, it's usually a Todo
 *
 * TODO long term:
 *  - we could use the ia also to find bugs that my current
 *    checkers can't find (e.g. undefined methods in $o->m() because
 *    of the better interprocedural class analysis, wrong type,
 *    passing null, use of undeclared field in $o->fld, etc).
 *    But the interpreter first needs to be correct
 *    and to work on www/ without so many exceptions.
 *    TODO just go through all constructs and find opportunities
 *    to detect bugs?
 *  - It could also be used for program understanding purpose
 *    by providing a kind of tracer.
 *  - maybe it could be combined with the type inference to give
 *    more precise results and find even more bugs.
 *
 * history:
 *  - basic values (Vint 2, Vstring "foo"), abstract value with types
 *    (Vabstr Tint), also range for ints. Special care for PHP references
 *    by using pointer to pointer to val via the Vptr, as in Zend.
 *    Basically handling simple sequences of assignements.
 *  - loop, recursive functions, by fixpoint on the heap. Could configure
 *    the number of times we run the loop to allow to converge
 *    to a fixpoint after more than two iterations. When the fixpoint
 *    is not reached for certain variables, then set a more abstract
 *    value (for instance if the range for ints grows, then turn it into
 *    a Vabstr Tint). Manage branches such as ifthenelse by unifying/merging
 *    heaps
 *  - unfiying heaps was too costly, so just unify what
 *    is needed (unifying pointers), process ifthenelse "sequentially"
 *    not independently.
 *  - fixpoint was too costly, and when '$i = 1;for() { $i=$i+1 }' it does
 *    not converge if use range so make it in such a way to reach the fixpoint
 *    in one step when unify. In the end we don't unify heaps,
 *    we don't do fixpoint. So for the loop example before,
 *    first have $i = Vint 1, but as soon as have $i=$i+1, we say it's a
 *    Vabstr Tint (and this is the fixpoint, in just one step).
 *  - handle objects and other constructs.
 *
 *  - all comments added by pad, split files, add mli, add unit tests,
 *    add tests/ia/*.php, fixed bugs, added strict mode, etc
 *)

(*****************************************************************************)
(* Configuration *)
(*****************************************************************************)

(* Generating a callgraph or not. Could be put in the environment, but
 * it's more about a configuration option than a local information in
 * an environment.
 *)
let extract_paths = ref true

(* Number of function calls to handle.
 * Julien thinks 6 is the value above which there is diminushing return
 * regarding the callgraph. The size of the callgraph does not grow that
 * much when goes from 6 to 7.
 *)
let max_depth = ref 4

(* throw exn instead of passing-over silently unhandled constructs *)
let strict = ref true

let show_vardump = ref false

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* used by unit testing when encountering the 'checkpoint()' function call *)
let _checkpoint_heap = ref
  (None: (Env_interpreter_php.heap * value SMap.t (* local vars *)) option)

(* for callgraph generation *)
let (graph: Callgraph_php2.callgraph ref) = ref Map_.empty

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* less: could maybe factorize in Unknown of Database_code.entity_kind,
 *  but for members for instance we also want to show also some
 *  extra information like the available methods and fields.
 *)
exception UnknownFunction of string
exception UnknownClass    of string
exception UnknownConstant of string

exception UnknownMember of string * string * string list
exception UnknownObject

(* Exception thrown when a call to a function or method is not known.
 * This happens when the code is intrisically far too dynamic or
 * when we have done too aggressive approximations in the interpreter
 * on certain values.
 *)
exception LostControl

type field = Static | NonStatic

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* add an edge in callgraph *)
let save_path env target =
  if !extract_paths
  then graph := CG.add_graph (List.hd !(env.path)) target !graph

(* used in extract_path mode to fake function/method calls *)
and make_fake_params l =
  List.map (fun p ->
    match p.p_type with
    | Some (Hint name) -> New (fake "new", Id (name), [])
    | None | Some (HintArray _) -> Id [w "null"]
    | _ -> failwith "fake params not implemented for extended types"
  ) l

let exclude_toplevel_defs xs =
  List.filter (function
  | ClassDef _ | FuncDef _ | ConstantDef _ -> false
  | _ -> true
  ) xs

let methods_and_fields members =
  List.map fst (SMap.bindings members)

let fb = AST_generic.fake_bracket

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

module Interp = functor (Taint: Env_interpreter_php.TAINT) -> struct

let rec program env heap program =

  env.path := [CG.File !(env.file)];
  (* Ok, let's interpret the toplevel statements. env.db must
   * be populated with all the necessary functions/classes/constants.
   *
   * Note that Include/Require are transformed into __builtin__require()
   * calls in ast_php and we will silently skip them below
   * (because the definitions of those __builtin__ are empty in
   * pfff/data/php_stdlib/pfff.php).
   *)
  Trace.process_entity !(env.file);
  let finalheap = stmtl env heap (exclude_toplevel_defs program) in

  if !extract_paths
  then begin
    (* Normally the abstract interpreter needs a starting point, like
     * a toplevel call to 'main();' to start interpret. A file with just
     * functions can't really be interpreted. But some code may not be
     * easily reachable, so for callgraph and tracing purposes, it's
     * good to fake calls to the toplevel functions and classes/methods
     * in a file. *)
    env.path := [CG.FakeRoot];
    List.iter (fake_root env heap) program;
  end;
  (* we return the heap so people can start from this heap to
   * analyze another file, like the .phpt of a .php, so there
   * is a continuity.
   *)
  finalheap


(* used only when generating the callgraph *)
and fake_root env heap =
  (* we use save_excursion because the 'force_class' below will
   * modify by side effect env.globals, but then when we would
   * process FuncDef, env.globals would think previous classes are in the
   * heap when they are actually not.
   * todo?? why it matters?
   *)
  H.save_excursion env heap (fun env heap x ->
    match x with
    | ClassDef c ->
      Trace.process_entity (unw c.c_name);
      let heap = force_class env heap (unw c.c_name) in
      (* pad: julien was first processing all static methods, not sure why *)
      List.iter (fun m ->
        let params = make_fake_params m.f_params in
        let e =
          if is_static m.m_modifiers
          then (Call (Class_get (Id [c.c_name], fake "::", Id [m.f_name]), 
                      fb params))
          else (Call (Obj_get (New (fake "new", Id [c.c_name], []), fake ".", Id [m.f_name]), fb params))
        in
        ignore(expr env heap e)
      ) c.c_methods
    | FuncDef fd ->
        Trace.process_entity (unw fd.f_name);
        let params = make_fake_params fd.f_params in
        ignore (call_fun fd env heap params)
    | ConstantDef f ->
        Trace.process_entity (unw f.cst_name);
        (* the body of a constant definition is a static scalar
         * so there is not much interesting things to do on it
         *)
        ()
    | _ -> ()
  )

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)

and stmt env heap x =
  match x with
  | TypeDef _x -> failwith "no support for typedefs for abstract interpreter"
  | NamespaceDef _ | NamespaceUse _ -> failwith "no support for namespace yet"
  (* special keywords in the code to debug the abstract interpreter state.
   * I've added var_dump() so that one can easily run a PHP test file
   * with php or aphp and get both run working (show() is an
   * undefined function in HPHP).
   *)
  | Expr (Call (Id [(("show" | "var_dump"),_)], (_,[e],_)), _) ->
      let heap, v = expr env heap e in
      if !show_vardump then begin
        Env.print_locals_and_globals print_string env heap;
        pr (Env.string_of_value heap v);
      end;
      heap
  (* used by unit testing *)
  | Expr (Call (Id [("checkpoint",_)], (_,[],_)), _) ->
      _checkpoint_heap := Some (heap, !(env.vars));
      heap

  | Expr (e, _) ->
      let heap, _ = expr env heap e in
      heap

  (* With 'if(true) { $x = 1; } else { $x = 2; }'
   * we will endup with a heap with $x = &2{&1{int}}.
   * Going in first branch will populate the heap
   * and env.vars with an entry for $x, and when visiting
   * the second branch the second assignment will
   * cause a generalization for $x to an int.
   *)
  | If (_, c, st1, st2) ->
      (* todo: warn type error if value/type of c is not ok? *)
      let heap, _ = expr env heap c in
      (* Some variables may be defined only in one branch.
       * To simplify the unifier we create some fake $x=null; before
       * processing the statements.
       * todo: dead code apparently
       *)
      let heap = NullNewVars.stmt env heap st1 in
      let heap = NullNewVars.stmt env heap st2 in
      (* not that we are not doing any path sensitivity here ...
       * even if we can statically determine that c is always true,
       * we just process both branches, and we actually process them
       * sequentially (we used to process them independently
       * and then merge/unify the resulting heaps).
       *)
      let heap = stmt env heap st1 in
      let heap = stmt env heap st2 in
      heap
  | Block (_,stl,_) ->
      stmtl env heap stl
  | Return (_, e) ->
      let e =
        match e with
        | None -> Id [(w "null")]
        | Some e -> e
      in
      (* the special "*return*" variable is used in call_fun() below *)
      let heap, _ = expr env heap (Assign (Var (w "*return*"), fake "=", e)) in
      heap

  (* this may seem incorrect to treat 'do' and 'while' in the same way,
   * because the evaluation of e does not happen at the same time.
   * But here we care about the pointfix of the values, and so
   * the order does not matter.
   * todo: but need to process the stmts 2 times at least to get a fixpoint?
   *)
  | Do (_, stl, e) | While (_, e, stl) ->
      let heap, _ = expr env heap e in
      let heap = stmtl env heap stl in
      heap
  | For (_, el1, el2, el3, stl) ->
      let heap, _ = Utils.lfold (expr env) heap el1 in
      let heap, _ = Utils.lfold (expr env) heap el2 in
      let heap, _ = Utils.lfold (expr env) heap el3 in
      stmtl env heap stl
  | Switch (_, e, cl) ->
      let heap, _ = expr env heap e in
      let heap = List.fold_left (case env) heap cl in
      heap
  (* todo: explain *)
  | Foreach (_, a, _, pattern, stl) ->
     let heap, a = expr env heap a in
     (match pattern with
      | Var _ ->
          let heap, _, v = lvalue env heap pattern in
          let heap, kint = Ptr.new_val heap (Vabstr Tint) in
          let heap, a' = Ptr.new_val heap (Vmap (kint, v)) in
          let heap, a' = Ptr.get heap a' in
          let heap, _a = Unify.value heap a a' in
          let heap = stmtl env heap stl in
          heap
      | Arrow (lhs, _, rhs) ->
         let heap, _, k = lvalue env heap lhs in
         let heap, _, v = lvalue env heap rhs in
         let heap, a' = Ptr.new_val heap (Vmap (k, v)) in
         let heap, a' = Ptr.get heap a' in
         let heap, _a = Unify.value heap a a' in
         let heap = stmtl env heap stl in
         heap

      | _ -> raise (ForeachWithList !(env.file)))
     | Continue (_, e) | Break (_, e) ->
      let heap, _ = Utils.opt (expr env) heap e in
      heap
  | Throw (_, e) ->
      let heap, _ = expr env heap e in
      heap
  | Try (_, stl, cl, fl) ->
      let heap = stmtl env heap stl in
      let heap = List.fold_left (catch env) heap cl in
      let heap = List.fold_left (finally env) heap fl in
      heap

  | Global (_, idl) -> List.fold_left (global env) heap idl
  | StaticVars (_, sl) -> List.fold_left (static_var env) heap sl

  | ClassDef _ | FuncDef _ ->
      if !strict
      then failwith "nested classes/functions";
      heap
  | ConstantDef _ ->
      (* see exclude_toplevel_defs above and parser_php.mly which
       * shows we can't have nested constants by construction
       *)
      raise Common.Impossible

(* What if break/continue/return/throw in the middle of the list of stmts?
 * Do we still abstract interpret the rest of the code? Yes because
 * we care about the pointfix of the values, and so we don't really
 * care about the control flow. The analysis is kinda of flow insensitive.
 *)
and stmtl env heap stl = List.fold_left (stmt env) heap stl

and global env heap v =
  match v with
  | Id [(x,_)] ->
      let heap, _new_, gv = Var.get_global env heap x in
      Var.set env x gv;
      heap
  | _ ->
      failwith "global: rest of global"

and static_var env heap (var, eopt) =
  let gvar = env.cfun ^ "**" ^ (unw var) in
  let heap, new_, gval = Var.get_global env heap gvar in
  let heap, _, v = Var.get env heap (unw var) in
  let heap, _ = assign env heap new_ v gval in
  match eopt with
  | None -> heap
  (* first time see this variable, so evaluate expression (once) *)
  | Some e when new_ ->
      let heap, e = expr env heap e in
      (* todo: why care passing new_ here? *)
      let heap, _ = assign env heap new_ gval e in
      heap
  | Some _ -> heap

and case env heap x =
  match x with
  | Case (_, e, stl) ->
      let heap, _ = expr env heap e in
      (* pad: still useful? toremove now? *)
      let heap = NullNewVars.stmtl env heap stl in

      let heap = stmtl env heap stl in
      heap
  | Default (_, stl) ->
      (* pad: useful? reremove now *)
      let heap = NullNewVars.stmtl env heap stl in
      let heap = stmtl env heap stl in
      heap

and catch env heap (_, _, _, stl) =
  stmtl env heap stl
and finally env heap (_, stl) =
  stmtl env heap stl

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env heap x =
  if !Taint.taint_mode
  then Taint.taint_expr env heap
    (expr_, lvalue, get_dynamic_function, call_fun, call, assign) !(env.path) x
  else expr_ env heap x

(* will return a "concrete" value, or a pointer to a concrete value,
 * or a pointer to a pointer to a concrete value when Ref.
 *)
and expr_ env heap x =
  match x with
  | Id [("true",_)]  -> heap, Vbool true
  | Id [("false",_)] -> heap, Vbool false
  | Int (s, _)     -> heap, Vint   (int_of_string s)
  | Double (s, _)  -> heap, Vfloat (float_of_string s)

  | String (s, _)  -> heap, Vstring s
  (* pad: ugly special case.
   * todo: fix Taint.fold_slist to not return a
   * a 'Vabstr Tstring' but instead a precise 'Vstring xxx'
   *)
  | Guil (_, [String (s,_)], _) -> heap, Vstring s
  | Guil (_, el, _) ->
      let heap, vl = Utils.lfold (encaps env) heap el in
      let heap, vl = Utils.lfold Ptr.get heap vl in
      let v = Taint.fold_slist vl in
      heap, v

  | Id [("null",_)]  -> heap, Vnull
  | Id [("NULL",_)]  -> heap, Vnull

  | Id [(s,_)]  ->
      (* Must be a constant. Functions and classes are not in the heap;
       * they are managed through the env.db instead and we handle
       * them at the Call (Id ...) and New (Id ...) cases.
       *)
       (try
           let def = env.db.constants s in
           (match def.cst_body with
             | Some b -> expr env heap b
             | None -> heap, Vany (* abstract constant? *)
           )
        with Not_found ->
          (* todo: when used in an instanceof context, as in
           * $x instanceof A, then Id will contain actually
           * the name of a class. *)
          if !strict then raise (UnknownConstant s);
          heap, Vany
       )

  | Id ((_s,tok)::_) -> raise (Cst_php.TodoNamespace (tok))
  | Id [] -> raise Impossible

  (* will probably return some Vabstr (Tint|Tbool|...) *)
  | Binop (e1, (bop, _), e2) ->
      let heap, v1 = expr env heap e1 in
      let heap, v2 = expr env heap e2 in
      (* we do some Ptr.get to get the final value, to "normalize" variables *)
      let heap, v1 = Ptr.get heap v1 in
      let heap, v2 = Ptr.get heap v2 in
      heap, binaryOp env heap bop v1 v2
  | Unop ((uop, _), e) ->
      let heap, v = expr env heap e in
      let heap, v = Ptr.get heap v in
      heap, unaryOp uop v

  (* todo: just desugar in $x = $x + 1 ? *)
  | Infix _ | Postfix _ ->
      if !strict then failwith "todo: handle Infix/Postfix";
      heap, Vany

  | Cast (ty, e) ->
      let heap, v = expr env heap e in
      heap, cast env heap ty v


  (* with '$x = (true)? 42 : "foo"' we will return a
   * Tsum(Vint 42, Vstring "foo")
   *)
  | CondExpr (e1, e2, e3) ->
      let heap, _ = expr env heap e1 in
      let heap, v1 = expr env heap e2 in
      let heap, v2 = expr env heap e3 in
      let heap, v = Unify.value heap v1 v2 in
      heap, v

  (* list($a, $b) = $y  where $y is an array *)
  | Assign (List (_, l, _), _, e) ->
      let n = ref 0 in
      let heap =
        List.fold_left (fun heap x ->
          let v = Array_get (e, Some (Int (w (string_of_int !n)))) in
          let heap, _ = expr env heap (Assign (x, fake "=", v)) in
          incr n;
          heap
        ) heap l in
      let heap, e = expr env heap e in
      heap, e
  | List _ -> failwith "List outside assignement?"

  (* code for $x = ..., $o->fld = ..., etc *)
  | Assign (e1, _, e2) ->
      let heap, new_var_created, lval = lvalue env heap e1 in
      let heap, rval = expr env heap e2 in
      assign env heap new_var_created lval rval
  | AssignOp (e1, (op, tok), e2) ->
      expr env heap (Assign (e1, tok, Binop (e1, (op, tok), e2)))

  (* we will return the pointer to pointer here *)
  | Ref (_, e) ->
      let heap, _, x = lvalue env heap e in
      heap, x

  | Unpack _ -> raise Todo

  | InstanceOf (_, e1, e2) ->
      let heap, _ = expr env heap e1 in
      let heap, _ = expr env heap e2 in
      (* pad: why vnull? *)
      heap, Vsum [Vnull; Vabstr Tbool]

  | ConsArray (_, [], _) ->
      heap, Varray []
  | ConsArray ((_, avl, _))
  | Collection (_, (_, avl, _)) ->
      let id = Id [(w "*array*")] in
      let heap = List.fold_left (array_value env id) heap avl in
      let heap, _, v = Var.get env heap "*array*" in
      let heap, v = Ptr.get heap v in
      Var.unset env "*array*";
      heap, v

  | Arrow _ -> failwith "should be handled in caller, in array_value"

  (* hardcoded special case, not sure why we need that *)
  | Call (Id [("id",_)], (_,[x],_)) -> expr env heap x

  | Call (Id [("call_user_func", tok)], (_, e :: el, _)) ->
      let heap, v = expr env heap e in
      Taint.check_danger env heap "call_user_func" tok !(env.path) v;
      (try
          (* todo: fname can also reference a static method *)
          let heap, def = get_dynamic_function env heap v in
          call_fun def env heap el
        with (LostControl | UnknownFunction _) ->
          if !strict then failwith "call_user_func unknown function";
          heap, Vany
      )

  | Call (Obj_get (lhs, _, Id [(s,_)]), _) when List.mem s ["toArray"; "toValuesArray"; "toKeysArray";
        "toVector"; "toImmVector"; "toMap"; "toImmMap"; "toSet"; "toImmSet"; "values"; "keys";
        "lazy"] ->
     let heap, v = expr env heap lhs in
     let heap, v' = Ptr.get heap v in
     heap, v'

  (* simple function call or $x() call *)
  | Call (Id [(s,_)], (_, el,_)) ->
      (try
        let heap, def = get_function env heap s in
        call_fun def env heap el
     (* pad: other? *)
      with (LostControl | UnknownFunction _) as exn  ->
        if !strict then raise exn;
        let heap, vl = Utils.lfold (expr env) heap el in
        let res = Taint.when_call_not_found heap vl in
        heap, res
      )
  (* expression call or method call (Call (Obj_get...)) or
   * static method call (Call (Class_get ...))
   *)
  | Call (e, (_, el,_)) ->
      let heap, v = expr env heap e in
      call env heap v el

  | New (_, e, el) ->
      new_ env heap e el
  | Xhp x ->
      let heap, v = xml env heap x in
      let v = if !Taint.taint_mode then Vabstr Txhp else v in
      heap, v

  | Lambda _ ->
      (* todo? could try to process its body? return a Vfun ? *)
      if !strict then failwith "todo: handle Lambda";
      heap, Vany
  | IdSpecial (Eval, _) -> raise Todo
  | Array_get _ | Class_get (_, _, _) | Obj_get (_, _, _)
  | Var _ | (IdSpecial (This, _)) as lv ->
      (* The lvalue will contain the pointer to pointer, e.g. &2{&1{...}}
       * so someone can modify it. See also assign() below.
       * But in an expr context, we actually want the value, hence
       * the Ptr.get dereference, so we will return &1{...}
       *)
      let heap, _, x = lvalue env heap lv in
      (* could probably do another call to Ptr.get here *)
      let heap, x = Ptr.get heap x in
      heap, x

(* related to Unify *)
and binaryOp env heap bop v1 v2 =
  match bop with
  | ArithOp op -> 
    if G.is_boolean_operator op
    then Vabstr Tbool
    else
      (match v1, v2 with
      | (Vint _ | Vabstr Tint), (Vint _ | Vabstr Tint) -> Vabstr Tint
      (* todo: warn on type error? why vnull? *)
      | _ -> Vsum [Vnull; Vabstr Tint]
      )

  | BinaryConcat ->
      (* Vabstr Tstring by default *)
      Taint.binary_concat env heap v1 v2 !(env.path)
  | CombinedComparison -> Vabstr Tint

and unaryOp uop v =
  match uop, v with
  | G.Plus, Vint n       -> Vint n
  | G.Plus, Vabstr Tint  -> Vabstr Tint
  | G.Plus, _            -> Vsum [Vnull; Vabstr Tint]
  | G.Minus, Vint n      -> Vint (-n)
  | G.Minus, Vabstr Tint -> Vabstr Tint
  | G.Minus, _           -> Vsum [Vnull; Vabstr Tint]
  | G.Not, Vbool b      -> Vbool (not b)
  | G.Not, Vabstr Tbool -> Vabstr Tbool
  | G.Not, _            -> Vsum [Vnull; Vabstr Tbool]
  | G.BitNot, Vint n      -> Vint (lnot n)
  | G.BitNot, Vabstr Tint -> Vabstr Tint
  | G.BitNot, _           -> Vsum [Vnull; Vabstr Tint]
  | ((G.Mult|G.Div|G.Mod|G.Pow|G.FloorDiv|G.MatMult
     |G.LSL|G.LSR|G.ASR|G.BitOr|G.BitXor|G.BitAnd|G.BitClear
     |G.And|G.Or|G.Xor|G.Eq|G.NotEq|G.PhysEq|G.NotPhysEq
     |G.Range|G.RegexpMatch|G.NotMatch
     |G.Lt|G.LtE|G.Gt|G.GtE|G.Cmp|G.Concat|G.Nullish),_) -> raise Impossible

and cast _env _heap (ty, _) v =
  match ty, v with
  | Cst_php.BoolTy, (Vbool _ | Vabstr Tbool) -> v
  | Cst_php.IntTy, (Vint _ | Vabstr Tint) -> v
  | Cst_php.DoubleTy, (Vfloat _ | Vabstr Tfloat) -> v
  | Cst_php.StringTy, (Vstring _ | Vabstr Tstring) -> v
  | Cst_php.ArrayTy, (Varray _ | Vrecord _) -> v
  (* pad: ?? should be more ty? raise exception? warn type error? *)
  | _ -> v

(* ---------------------------------------------------------------------- *)
(* Lvalue *)
(* ---------------------------------------------------------------------- *)
(* will return a boolean indicating whether a variable was created
 * and will return the lvalue, that is the pointer to pointer, and not
 * the actual value, so that the caller can modify it.
 * todo: Why do we care to return if a variable was created?
 *)
and lvalue env heap x =
  match x with
  (* for taiting *)
  | Var ("$_POST" | "$_GET" | "$_REQUEST" as s, _) ->
      let heap, k = Ptr.new_val heap (Vtaint s) in
      let heap, v = Ptr.new_val heap (Vtaint s) in
      heap, false, Vmap (k, v)

  (* Most common case. When we interpret '$x = 42', we will end up here
   * and return the pointer to pointer for $x from env.vars.
   * See also assign() below.
   *)
  | Id [(s,_)] ->
      if !strict
      then failwith ("lvalue should be variables, not Id: " ^ s);
      (* this may create a new variable in env.vars, which is
       * PHP semantic since there is no variable declaration in PHP.
       *)
      Var.get env heap s
  | Var (s,_) ->
      Var.get env heap s

  | IdSpecial (This, tok) ->
      (* $this is present in env.globals (see make_method())
       * todo: so with this actually look for the value of $this in
       * env.globals??
      *)
      let name = "$this", tok in
      lvalue env heap (Var (name))

  | Array_get (e, k) ->
      array_get env heap e k

  | ConsArray _ as e ->
      let heap, a = expr env heap e in
      let heap, v = Ptr.new_ heap in
      let heap, _ = assign env heap true v a in
      heap, true, v

  (* will return the field reference or Vmethod depending on s *)
  | Obj_get (e, _, Id [(s,_)]) ->
      let heap, v = expr env heap e in
      let heap, v' = Ptr.get heap v in
      let members = obj_get_members ISet.empty env heap [v'] in
      (try heap, false, SMap.find s members
      with Not_found ->
        (* This will actually try access to a static class variables
         * See class_vars() below.
         * todo: We should throw an exception here in strict mode,
         * people should not access static member via $o->.
         * I don't even know how to do that actually.
         *)
        try heap, false, SMap.find ("$"^s) members
        with Not_found ->
          (match s with
          (* it's ok to not have a __construct method *)
          | "__construct" -> ()
          | _ ->
              if !strict then begin
                let xs = methods_and_fields members in
                if Common.null xs
                then raise UnknownObject
                else raise (UnknownMember (s, "?", xs))
              end
          );
          (* argh, PHP allow to access an undeclared field.
           * todo: should warn error
           *)
          let heap, k = Ptr.new_val heap Vnull in
          let heap = Ptr.set heap v' (Vobject (SMap.add s k members)) in
          heap, true, k
      )
  (* will return a classvar reference or Vmethod depending on s *)
  | Class_get (e, _, Id [(s,_)]) ->
      let str = get_class env heap e in
      let heap = lazy_class env heap str in
      let heap, _, v = Var.get_global env heap str in
      let heap, v = Ptr.get heap v in
      let heap, v = Ptr.get heap v in
      (try
          match v with
          | Vobject members when SMap.mem s members ->
              heap, false, SMap.find s members
          | Vobject members ->
              if !strict
              then raise (UnknownMember (s, str, methods_and_fields members));
              heap, false, Vany
          | _ ->
              if !strict then failwith "Class_get not a Vobject";
              heap, false, Vany
      with Not_found ->
        if !strict then failwith "Class_get not found";
        heap, false, Vany
      )
  (* TODO *)
  | Class_get (_, _, e) ->
      let heap, _ = expr env heap e in
      if !strict then failwith "Class_get general case not handled";
      heap, false, Vany

  | List _ -> failwith "List should be handled in caller"
  | _e ->
      if !strict then failwith "lvalue not handled";
      heap, false, Vany

(* ---------------------------------------------------------------------- *)
(* Assign *)
(* ---------------------------------------------------------------------- *)
(*
 * When doing $x = ... lots of things can happen in PHP. For instance
 * PHP can copy the value of the right to the left. If the right
 * part is a reference (as in $x = &$y), then it will create instead
 * a shared reference.
 *
 * root is a pointer to pointer resulting from '$x = ...'
 * but also from 'A::$x = ...', or '$o->x = ....'
 *
 * TODO explain. Why need is_new?
 * note:could be moved in helper
 *)
and assign _env heap is_new root(*lvalue*) v_root(*rvalue*) =
  let heap, ptr = Ptr.get heap root in
  let heap, v = Ptr.get heap v_root in
  match v with
  (* v_root comes from an expr, so after a Ptr.get if it's still a Vptr,
   * that means the expr was a Ref.
   *)
  | Vptr _ | Vref _ ->
      let vr = match v with Vptr n -> Vref (ISet.singleton n) | x -> x in
      if is_new
      then
        let heap = Ptr.set heap root vr in
        let heap = Ptr.set heap v_root vr in
        heap, v
      else
        let heap, v = Unify.value heap v vr in
        let heap = Ptr.set heap root v in
        let heap = Ptr.set heap v_root v in
        heap, v
  (* a regular rvalue *)
  | _ ->
      if is_new
      then
        (* PHP has a copy-on-write semantic, so for '$y = $x'
         * we will copy the value of $x in $y. There will be
         * no sharing.
         *)
        let heap, v' = Copy.value heap v in
        let heap = Ptr.set heap ptr v' in
        heap, v
      else
        let heap, v' = Ptr.get heap ptr in
        let heap, v = Unify.value heap v v' in
        let heap = Ptr.set heap ptr v in
        heap, v

(* ---------------------------------------------------------------------- *)
(* Call *)
(* ---------------------------------------------------------------------- *)

(* Getting the func_def corresponding to the Id.
 * We would not need this if we had a Vfun.
 *)
and get_function env heap id =
  if not (id.[0] = '$') then
    try heap, env.db.funs id
    with Not_found -> raise (UnknownFunction id)
  else
    let heap, v = expr env heap (Id [(w id)]) in
    get_dynamic_function env heap v

(* in PHP functions are passed via strings *)
and get_dynamic_function env heap v =
  let heap, v = Ptr.get heap v in
  match v with
  (* todo: this could be a static method too *)
  | Vstring s ->
      (try heap, env.db.funs s
       with Not_found -> raise (UnknownFunction s)
      )
  (* when will this happen? usually either have a Vstring or
   * a Vabstr Tstring.
   *)
  | Vsum l -> get_function_list env heap l
  | _ -> raise LostControl
and get_function_list env heap = function
  | [] -> raise LostControl
  | Vstring s :: _ ->
      (try heap, env.db.funs s
      with Not_found -> raise (UnknownFunction s)
      )
  | _ :: rl -> get_function_list env heap rl

(* call_fun and func_def is abused to also call methods *)
and call_fun (def: A.func_def) env heap (el: A.expr list) =
  Trace.call (unw def.f_name) !(env.path);

  let n = try SMap.find (unw def.f_name) env.stack with Not_found -> 0 in
  let env = { env with stack = SMap.add (unw def.f_name) (n+1) env.stack } in
  (* pad: ugly, call_fun should also accept method_def *)
  save_path env (CG.node_of_string (unw def.f_name));

  let is_clean =
    let _, vl = Utils.lfold (expr env) heap el in
    List.fold_left (fun acc x -> Taint.GetTaint.value heap x = None && acc)
      true vl
  in

  (* stop when recurse in same function twice or when depth stack > 6 *)
  if n >= 2 || List.length !(env.path) >= !max_depth && is_clean
  (* || Sys.time() -. !time >= 1.0|| SMap.mem def.f_name !(env.safe) *)
  then
    (* ??? why not just return Vany instead? *)
    let heap, v = Ptr.new_ heap in
    let heap, _ = assign env heap true v Vany in
    heap, v
  else begin
    (* we evaluate parameters in the caller environment *)
    let env = { env with vars = ref !(env.vars); cfun = unw def.f_name } in
    let heap = parameters env heap def.f_params el in
    (* we keep just the parameter variables created in the previous step *)
    let vars = fun_nspace def !(env.vars) in
    let env = { env with vars = ref vars } in
    env.path := (CG.node_of_string (unw def.f_name)) :: !(env.path);
    let heap = stmtl env heap def.f_body in
    env.path := List.tl !(env.path);
    let heap, _, r = Var.get env heap "*return*" in
    let heap, r = Ptr.get heap r in

    if Taint.GetTaint.value heap r = None
    then env.safe := SMap.add (unw def.f_name) r !(env.safe);

    heap, r
  end


and parameters env heap l1 l2 =
  match l1, l2 with
  | [], _ -> heap
  | p :: rl, [] ->
      (match p.p_default with
      | None -> parameters env heap rl []
      | Some e ->
          let e = if p.p_ref <> None then make_ref e else e in
          let heap, v = expr env heap e in
          Var.unset env (unw p.p_name);
          let heap, _, lv = lvalue env heap (Var p.p_name) in
          let heap, _ = assign env heap true lv v in
          parameters env heap rl []
      )
  | p :: rl, e :: rl2 ->
      let e = if p.p_ref <> None then make_ref e else e in
      let heap, v = expr env heap e in
      (* in recursive calls we have parameters equal
       * to variables used in the caller context.
       * we must not confuse them.
       * todo: what about if this parameter name in used
       * with the other parameters????
       *)
      Var.unset env (unw p.p_name);
      let heap, _, lv = lvalue env heap (Var p.p_name) in
      let heap, _ = assign env heap true lv v in
      parameters env heap rl rl2

(* could be in helper *)
and make_ref e =
  match e with
  | Ref _ -> e
  | _ when IsLvalue.expr e -> Ref (fake "&", e)
  | _ -> e

(* could be moved in helper*)
and fun_nspace f roots =
  List.fold_left (
    fun acc p ->
      (try SMap.add (unw p.p_name) (SMap.find (unw p.p_name) roots) acc
        with Not_found -> acc
      )
  ) SMap.empty f.f_params


and call env heap v el =
  match v with
  | Vsum l -> sum_call env heap l   el
  | x      -> sum_call env heap [x] el

and sum_call env heap v el =
  (match v with
  (* todo: if strict then exception? too dynamic code? *)
  | [] -> heap, Vany
  (* in PHP we pass functions as strings *)
  | Vstring s :: _ ->
      (* todo: 's' could reference a static method as in
       * $x = 'A::foo'; $x();
       *)
      let heap, r = expr env heap (Call (Id [(w s)], fb el)) in
      heap, r
  | Vmethod (_, fm) :: _ ->
      let fl = IMap.fold (fun _ y acc -> y :: acc) fm [] in
      call_methods env heap fl el
  | Vtaint _ as v :: _ ->
      if !strict then failwith "sum_call Vtaint";
      heap, v
  | _ :: rl -> sum_call env heap rl el
  )

and call_methods env heap fl el =
  match fl with
  | [] -> assert false
  | [f] -> f env heap el
  | f1 :: rl ->
      let heap, v = f1 env heap el in
      List.fold_left (call_method env el) (heap, v) rl

and call_method env el (heap, v) f =
  let heap, v' = f env heap el in
  let heap, v = Unify.value heap v v' in
  heap, v

(* ---------------------------------------------------------------------- *)
(* Arrays *)
(* ---------------------------------------------------------------------- *)
and array_value env id heap x =
  let heap, new_, ar = lvalue env heap id in
  let heap, a = Ptr.get heap ar in
  match x with
  | Arrow (e1, _, e2) ->
     let heap, k = expr env heap e1 in
     let heap, k = Ptr.get heap k in
     (match a, k with
      | _, Vstring k when new_ ->
         let heap, v = array_new_entry env heap ar a k SMap.empty in
         let heap, e2 = expr env heap e2 in
         let heap, _ = assign env heap true v e2 in
         heap
      | Vrecord m, Vstring k ->
         let heap, v = array_new_entry env heap ar a k m in
         let heap, e2 = expr env heap e2 in
         let heap, _ = assign env heap true v e2 in
         heap
      | _ ->
         let heap, _ =
           expr env heap (Assign (Array_get (id, Some e1), fake "=", e2)) in
         heap
     )
  | _ ->
     (match a with
      | _ when new_ ->
         let l = [] in
         let heap, v = Ptr.new_ heap in
         let l = v :: l in
         let heap = Ptr.set heap ar (Varray l) in
         let heap, e = expr env heap x in
         let heap, _ = assign env heap true v e in
         heap

      | Varray l ->
         let heap, v = Ptr.new_ heap in
         let l = v :: l in
         let heap = Ptr.set heap ar (Varray l) in
         let heap, e = expr env heap x in
         let heap, _ = assign env heap true v e in
         heap

      | _ ->
         let heap, _ = expr env heap (Assign (Array_get (id, None), fake "=", x))
         in
         heap
     )


(* could be moved in helper *)
and array_new_entry _env heap ar _a k m =
  let heap, v = Ptr.new_ heap in
  let m = SMap.add k v m in
  let heap = Ptr.set heap ar (Vrecord m) in
  heap, v

and array_get env heap e k =
  let heap, _new_, ar = lvalue env heap e in
  let heap, ar = Ptr.get heap ar in
  let heap, a = Ptr.get heap ar in
  let heap, k = Utils.opt (expr env) heap k in
  let heap, k = Utils.opt Ptr.get heap k in
  match a, k with
  | Vrecord m, Some (Vstring k) when SMap.mem k m ->
      heap, false, SMap.find k m
  | Vrecord m, Some (Vstring k) ->
      let heap, v = array_new_entry env heap ar a k m in
      heap, false, v
  | Varray l, Some (Vint k) when k >= 0 && k < List.length l ->
      heap, false, List.nth (List.rev l) k
  | Vmap (k, v), Some k' ->
      let heap, _ = Unify.value heap k k' in
      heap, false, v
  | Vmap (_, v), None ->
      heap, false, v
  | _, kval ->
      (* todo? strict mode? *)
      let kval = match kval with None -> Vabstr Tint | Some v -> v in
      let heap, kr = Ptr.new_ heap in
      let heap, k = Ptr.get heap kr in
      let heap = Ptr.set heap k kval in
      let heap, v = Ptr.new_ heap in
      let a' = Vmap (kr, v) in
      let heap, a = Unify.value heap a a' in
      let heap = Ptr.set heap ar a in
      heap, false, v

(* ---------------------------------------------------------------------- *)
(* Xhp, strings *)
(* ---------------------------------------------------------------------- *)

and xhp env heap x =
  match x with
  | XhpText _ -> heap
  | XhpExpr e ->
      let heap, _ = expr env heap e in
      heap
  | XhpXml x ->
      (* todo: should set the children field of the enclosing xhp? *)
      let heap, _v = xml env heap x in
      heap

and xhp_attr env heap x =
  match x with
  | Guil (_, el, _) ->
      let heap, vl = Utils.lfold (encaps env) heap el in

      let heap, vl = Utils.lfold Ptr.get heap vl in
      let v = Taint.fold_slist vl in
      Taint.check_danger env heap "xhp attribute" ((Cst_php.fakeInfo ""))
        !(env.path) v;

      heap
  | e -> fst (expr env heap e)

and xml env heap x =
  let heap = List.fold_left (fun heap (_, x) ->
    xhp_attr env heap x
  ) heap x.xml_attrs in
  let heap = List.fold_left (xhp env) heap x.xml_body in
  (* todo? args = ? *)
  let args = [] in
  new_ env heap (Id [x.xml_tag]) args

and encaps env heap x = expr env heap x

(* ---------------------------------------------------------------------- *)
(* Class *)
(* ---------------------------------------------------------------------- *)

and new_ env heap e el =
  let str = get_class env heap e in
  (* todo? not necessary I think. the class loading will be done
   * when processing the Class_get below in lvalue()
   *)
  let heap = lazy_class env heap str in
  let stl = [
  (* *myobj* = str::*BUILD*();
   * *myobj->__construct(el);
   *)
    Expr (Assign (Var (w "*myobj*"), fake "=",
                 Call (Class_get (Id [(w str)], fake "::", Id [(w "*BUILD*")]), fb [])), G.sc);
    Expr (Call (Obj_get (Var (w "*myobj*"), fake ".", Id [(w "__construct")]), fb el), G.sc);
  ] in
  let heap = stmtl env heap stl in
  let heap, _, v = Var.get env heap "*myobj*" in
  Var.unset env "*myobj*";
  heap, v

(* could be put in helper.
 * todo: what mem is for?
 *)
and obj_get_members mem env heap v =
  (match v with
  | [] -> SMap.empty
  | Vref a :: rl ->
      let l = ISet.fold (fun x acc -> x :: acc) a [] in
      let vl = List.map (fun x -> Vptr x) l in
      obj_get_members mem env heap (vl @ rl)
  | Vobject m :: _ -> m
  | Vsum l :: l' ->
      obj_get_members mem env heap (l@l')
  | Vptr n :: rl when ISet.mem n mem ->
      obj_get_members mem env heap rl
  | Vptr n as x :: rl ->
      let mem = ISet.add n mem in
      let heap, x = Ptr.get heap x in
      obj_get_members mem env heap (x :: rl)
  | _x :: rl -> obj_get_members mem env heap rl
  )

(* todo: make it mimic more get_function and get_dynamic_function ? *)
and get_class env heap e =
  match e with
  (* pad: ???? *)
  | Id [("",_)] -> ""

  | Id [(s,_)] when s.[0] <> '$' -> s
  | Id _ ->
      let _env, v = expr env heap e in
      let _heap, v = Ptr.get heap v in
      get_string [v]
  | _ -> ""
and get_string = function
  | [] -> ""
  | Vstring s :: _ -> s
  | Vsum l' :: rl ->
      (match get_string l' with
      | "" -> get_string rl
      | x -> x
      )
  | _ :: rl -> get_string rl

and lazy_class env heap classname =
  if (SMap.mem classname !(env.globals))
  then heap
  else force_class env heap classname

and force_class env heap classname =
  try
    let def = env.db.classes classname in

    let heap, null = Ptr.new_ heap in
    (* pad: ??? there is an overriding set_global below, so why create this?
     * because in class_def we can reference the class name and
     * want to find its pointer?
     *)
    Var.set_global env (unw def.c_name) null;
    let heap, v = class_def env heap def in
    (* def.c_name should be the same than classname no? *)
    Var.set_global env (unw def.c_name) v;
    heap
  with Not_found ->
    if !strict then raise (UnknownClass classname);
    heap

and class_def env heap (c: Ast.class_def) =
  let heap, self = Ptr.new_ heap in
  let heap, pname, parent =
    match c.c_extends with
    | Some ht ->
        let (p, _tok) = name_of_class_name ht in
        let heap = lazy_class env heap p in
        let heap, _, ptr = Var.get_global env heap p in
        heap, p, ptr
    (* no parents *)
    | None ->
        let heap, ptr = Ptr.new_ heap in
        (* todo: return a None *)
        heap, "", ptr
  in
  let heap, ddparent = Ptr.get heap parent in
  let heap, ddparent = Ptr.get heap ddparent in
  (* 'm' for members, not only methods *)
  let m =
    match ddparent with
    (* we inherit all previous members *)
    | Vobject m -> m
    (* todo: strict, exn/? hmm can happen when have no parents so Vnull
     * so put | Vnull->Smap.empty | _ -> raise Impossible?
     *)
    | _ -> SMap.empty
  in
  let heap, m = List.fold_left (cconstants env) (heap,m) c.c_constants in
  let heap, m = List.fold_left (class_vars env Static) (heap,m) c.c_variables in
  let heap, m = List.fold_left (method_def env c.c_name parent self None)
    (heap, m) c.c_methods in
  (* todo: handle traits! pure inlining, so have same self and parent *)

  (* *BUILD* is a special method that will be called when creating
   * new object as in 'new A()' which will be translated in
   * a '*myobj* = A::*BUILD*; *myobj->__construct().
   * "New est une methode comme une autre" :)
   *)
  let m = SMap.add "*BUILD*" (build_new env heap pname parent self c m) m in
  let v = Vobject m in
  let heap, _ = assign env heap true self v in
  heap, self


and build_new env heap pname parent self c m =
  let mid = Utils.fresh() in
  let closure = build_new_ env heap pname parent self c m in
  Vmethod (Vnull (* no this, BUILD is static method *),
          IMap.add mid closure IMap.empty)

(* "new est une method comme les autres".
 * m contains all the methods, static vars, and constants of this
 * class and parents.
 *)
and build_new_ _env _heap _pname parent self c m =
 fun env heap args ->
  (* we should always call *BUILD* without arguments *)
  if args <> [] 
  then raise Common.Impossible;

  let heap, dparent = Ptr.get heap parent in
  let heap, dparent = Ptr.get heap dparent in
  let heap, ptr =
    match dparent with
    | Vobject x ->
        (* recursivly call new on the parent *)
        (match SMap.find "*BUILD*" x with
        | Vmethod (_, f) ->
            let fl = IMap.fold (fun _ y acc -> y :: acc) f [] in
            let heap, x = call_methods env heap fl [] in
            heap, x
        | _ -> assert false
        )
    (* no parents, ok let's allocate a new naked object (->Vnull) *)
    | _ -> Ptr.new_ heap
  in
  let heap, up = Ptr.get heap ptr in
  let heap, up = Ptr.get heap up in
  (* get all members of the parent *)
  let m =
    match up with
    | Vobject m' -> SMap.fold SMap.add m' m
    | _ -> m
  in
  let heap, m' =
    List.fold_left (class_vars env NonStatic) (heap, m) c.c_variables in
  (* this will naturally override previous methods as the last binding
   * in the SMap of the members is what matters.
   *)
  let heap, m' =
    List.fold_left (method_def env c.c_name parent self (Some ptr))
      (heap, m') c.c_methods in
  let heap, _ = assign env heap true ptr (Vobject m') in
  heap, ptr


and cconstants env (heap, m) cst =
  let name = A.unwrap cst.cst_name in
  let heap, v = (match cst.cst_body with
    | None -> Ptr.new_ heap
    | Some b -> expr env heap b
  ) in
  heap, SMap.add name v m

(* static is to indicate if we want create members for static variables. *)
and class_vars env static (heap, m) cv =
  match static, is_static cv.cv_modifiers with
  | Static, true
  | NonStatic, false ->
      (class_var env static) (heap, m) (cv.cv_name, cv.cv_value)
  | _ -> heap, m

and class_var env static (heap, m) ((s, _tok), e) =
  (* static variables keep their $, regular fields don't *)
  let s =
    match static with
    | Static -> s
    | NonStatic -> String.sub s 1 (String.length s - 1)
  in
  match e with
  | None ->
      let heap, v = Ptr.new_ heap in
      heap, SMap.add s v m
  | Some e ->
      let heap, v1 = Ptr.new_ heap in
      let heap, v2 = expr env heap e in
      let heap, _ = assign env heap true v1 v2 in
      heap, SMap.add s v1 m

(* todo: factorize with func_def? *)
and method_def _env cname parent self this (heap, acc) def =
  let fdef = {
    f_ref = false;
    (* pad: this is ugly, but right now call_fun accepts only
     * func_def, not method_def, so have to do that.
     * There is a (ugly) corresponding call to node_of_string in
     * call_fun().
     *)
    f_name = w (CG.string_of_node (CG.Method (unw cname, unw def.f_name)));
    f_params = def.f_params;
    f_return_type = def.f_return_type;
    f_body = def.f_body;
    f_kind = Function; m_modifiers = [];
    l_uses = [];
    f_attrs = def.f_attrs;
  } in
  let cls = make_method def.f_name parent self this fdef in
  let mid = Utils.fresh() in
  let v = match this with None -> Vnull | Some v -> v in
  let v = Vmethod (v, IMap.add mid cls IMap.empty) in
  heap, SMap.add (unw def.f_name) v acc

(* we use OCaml closures to deal with self/parent scoping issues *)
and make_method mname parent self this fdef =
  fun env heap el ->
    let self_ = A.special "self" in
    let parent_ = A.special "parent" in
    let old_self =
      try Some (SMap.find self_ !(env.globals)) with Not_found -> None in
    let old_parent =
      try Some (SMap.find parent_ !(env.globals)) with Not_found -> None in
    let old_this =
      try Some (SMap.find "$this" !(env.globals)) with Not_found -> None in
    Var.set_global env self_ self;
    Var.set_global env parent_ parent;
    (match this with
    | None -> ()
    | Some v -> Var.set_global env "$this" v
    );
    let heap, res =
      (* the actual call! *)
      call_fun fdef env heap el in

    (* tainting special code *)
    let heap, res' = Ptr.get heap res in
    let heap, res' = Ptr.get heap res' in
    if unw mname = "render"
    then Taint.check_danger env heap "return value of render" (snd mname)
      !(env.path) res';

    (match old_self with Some x -> Var.set_global env self_ x | None -> ());
    (match old_parent with Some x -> Var.set_global env parent_ x | None ->());
    (match old_this with Some x -> Var.set_global env "$this" x | None -> ());
    heap, res

end

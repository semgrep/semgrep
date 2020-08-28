(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2012 Facebook
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
module E = Error_php
module S = Scope_code
module Ent = Entity_code
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A variable checker and local name resolver for PHP.
 *
 * This module helps find stupid PHP mistakes related to variables. See
 * tests/php/scheck/variables.php for examples of bugs currently
 * detected by this checker. This module not only checks but also annotates
 * the AST with scoping information as a side effect. This is useful
 * in codemap to display differently references to parameters, local vars,
 * global vars, etc.
 *
 * This file mostly deals with scoping issues. Scoping is different
 * from typing! Those are two orthogonal programming language concepts.
 * old: This file is concerned with variables, that is Ast_php.dname
 *  entities, so for completness C-s for dname in ast_php.ml and
 *  see if all uses of it are covered. Other files are more concerned
 *  about checks related to entities, that is Ast_php.name.
 *
 * The errors detected here are mostly:
 *  - UseOfUndefinedVariable
 *  - UnusedVariable
 * Some similar checks are done by JSlint.
 *
 * Some issues:
 *  - detecting variable-related mistakes is made slightly more complicated
 *    by PHP because of the lack of declaration in the language;
 *    the first assignment "declares" the variable (on the other side
 *    the PHP language forces people to explicitly declared
 *    the use of global variables (via the 'global' statement) which
 *    makes certain things easier).
 *
 *  - variables passed by reference can look like UseOfUndefinedVariable
 *    bugs but they are not. One way to fix it is to do a global analysis that
 *    remembers what are all the functions taking arguments by reference
 *    and whitelist them here. But it has a cost. One can optimize a little
 *    this by using an entity_finder computed semi lazily a la cmf multi
 *    level approach (recursive a la cpp, flib-map, git-grep, git head).
 *    Another way is to force programmers to actually declare such variables
 *    before those kinds of function calls (this is what Evan advocated).
 *
 *  - people abuse assignements in function calls to emulate "keyword arguments"
 *    as in 'foo($color = "red", $width = 10)'. Such assignments looks
 *    like UnusedVariable but they are not. One can fix that by detecting
 *    such code patterns.
 *
 *  - functions like extract(), param_get(), param_post()
 *    or variable variables like $$x introduce some false positives.
 *    Regarding the param_get/param_post(), one way to fix it is to just
 *    not analyse toplevel code. Another solution is to hardcode a few
 *    analysis that recognizes the arguments of those functions. Finally
 *    for the extract() and $$x one can just bailout of such code or
 *    as Evan did remember the first line where such code happen and
 *    don't do any analysis pass this point.
 *
 *  - any analysis will probably flag lots of warnings on an existing PHP
 *    codebase. Some programmers may find legitimate certain things,
 *    for instance having variables declared in a foreach to escape its
 *    foreach scope. This would then hinder the whole analysis because
 *    people would just not run the analysis. You need the approval of
 *    the PHP developers on such analysis first and get them ok to change
 *    their coding styles rules. One way to fix this problem is to have
 *    a strict mode where only certain checks are enabled. A good
 *    alternative is also to rank errors. A final trick is to report
 *    only new errors.
 *
 * Here are some extra notes by Evan in his own variable linter:
 *
 * "These things declare variables in a function":
 * - DONE Explicit parameters
 * - DONE Assignment (pad: variable mentionned for the first time)
 * - DONE Assignment via list()
 * - DONE Static, Global
 * - DONE foreach()
 * - DONE catch
 * - DONE Builtins ($this)
 * - DONE Lexical vars, in php 5.3 lambda expressions
 * pad: forgot pass by reference variables
 *
 * "These things make lexical scope unknowable":
 * - DONE Use of extract()
 * - DONE Assignment or Global with variable variables ($$x)
 *   (pad: so just bailout on such code)
 * pad: forgot eval()? but eval can introduce new variables in scope?
 *
 * These things don't count as "using" a variable:
 * - DONE isset() (pad: this should be forbidden, it's a bad way to program)
 * - DONE empty()
 * - DONE Static class variables (pad: check done in check_classes instead)
 *
 * Here are a few additional checks and features of this checker:
 *  - when the strict_scope flag is set, check_variables will
 *    emulate a block-scoped language as in JSLint and flags
 *    variables used outside their "block".
 *  - when passed the find_entity hook, check_variables will
 *    know about functions taking parameters by refs, which removes
 *    some false positives
 *
 * history:
 *  - sgrimm had the simple idea of just counting the number of occurences
 *    of a variable in a program, at the token level. If only 1, then
 *    probably a typo. But sometimes variable names are mentionned in
 *    interface signatures in which case they occur only once. So you need
 *    some basic analysis; the token level is not enough. You may not
 *    need the CFG but at least you need the AST to differentiate the
 *    different kinds of unused variables.
 *  - Some of the scoping logic was previously in another file, scoping_php.ml
 *    but we were kind of duplicating the logic that is in scoping_php.ml.
 *    PHP has bad scoping rule allowing variables declared through a foreach
 *    to be used outside the foreach, which is probably wrong.
 *    Unfortunately, knowing from scoping_php.ml just the status of a
 *    variable (local, global, or param) is not good enough to find bugs
 *    related to those weird scoping rules. So I've put all variable scope
 *    related stuff in this file and removed the duplication in scoping_php.ml.
 *  - I was using ast_php.ml and a visitor approach but then I rewrote it
 *    to use ast_php and an "env" approach because the code was
 *    getting ugly and was containing false positives that were hard to fix.
 *    As a side effect of the refactoring, some bugs disappeared (nested
 *    assigns in if, list() not at the toplevel of an expression, undefined
 *    access to an array), and code regarding lexical variables became
 *    more clear because localized in one place.
 *
 * TODO OTHER:
 *  - factorize code for the shared_ref thing and create_new_local_if_necessary
 *  - the old checker was handling correctly globals? was it looking up
 *    in the top scope? add some unit tests.
 *  - put back strict block scope
 *  - factorize in add_var() that adds in env.vars and
 *    update env.scoped_vars_used too. Removed refs to scope_ref in this file
 *)

(*****************************************************************************)
(* Types, constants *)
(*****************************************************************************)
type env = {
  (* The ref in the tuple is to remember the number of uses of the variable,
   * for the UnusedVariable check.
   * The ref for the Map.t is to avoid threading the env, because
   * any stmt/expression can introduce new variables.
   *
   * todo? use a list of Map.t to represent nested scopes?
   * (globals, methods/functions, nested blocks)? when in strict/block mode?
   *)
  vars: (string, (Cst_php.tok * Scope_code.t * int ref)) Map_.t ref;

  (* to remember how to annotate Var in ast_php.ml *)
  scope_vars_used: (Cst_php.tok, Scope_code.t) Hashtbl.t;

  (* todo: have a globals:? *)

  (* we need to access the definitions of functions/methods to know
   * if an argument was passed by reference, in which case what looks
   * like a UseOfUndefinedVariable is actually not (but it would be
   * better for them to fix the code to introduce/declare this variable
   * before ...).
   *)
  db: Entity_php.entity_finder option;
  (* when analyze $this->method_call(), we need to know the enclosing class *)
  in_class: ident option;

  (* for better error message when the variable was inside a long lambda *)
  in_long_lambda: bool;

  (* when the body of a function contains eval/extract/... we bailout
   * because we don't want to report false positives
   *)
  bailout: bool ref;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let unused_ok s =
  s =~ "\\$_.*" ||
  s =~ "\\$ignore.*" ||
  List.mem s ["$unused";"$dummy";"$guard"] ||
  (if !E.strict
   then false
   else
    List.mem s [
      "$res"; "$retval"; "$success"; "$is_error"; "$rs"; "$ret";
      "$e"; "$ex"; "$exn"; (* exception *)
    ]
  )

let lookup_opt s vars =
  Common2.optionise (fun () -> Map_.find s vars)

let s_tok_of_ident name =
  A.str_of_ident name, A.tok_of_ident name

(* to help debug *)
let str_of_any = Ast_php.show_any

let fb = AST_generic.fake_bracket

(*****************************************************************************)
(* Vars passed by ref *)
(*****************************************************************************)
(*
 * Detecting variables passed by reference is complicated in PHP because
 * one does not have to use &$var at the call site (one can though). This is
 * ugly. So to detect variables passed by reference, we need to look at
 * the definition of the function/method called, hence the need for a
 * entity_finder in env.db.
 *
 * note that it currently returns an Cst_php.func_def, not
 * an Ast_php.func_def because the database currently
 * stores concrete ASTs, not simple ASTs.
 *)
let funcdef_of_call_or_new_opt env e =
  match env.db with
  | None -> None
  | Some find_entity ->
      (match e with
      | Id [name] ->
          (* simple function call *)
            let (s, tok) = s_tok_of_ident name in
            (match find_entity (Ent.Function, s) with
            | [Cst_php.FunctionE def] -> Some def
            (* normally those errors should be triggered in
             * check_functions_php.ml, but right now this file uses
             * ast_php.ml and not ast_php.ml, so there are some
             * differences in the logic so we double check things here.
             *)
            | [] ->
                E.fatal tok (E.UndefinedEntity (Ent.Function, s));
                None
            | _x::_y::_xs ->
                E.fatal tok (E.MultiDefinedEntity (Ent.Function, s, ("","")));
                None
            | _ -> raise Impossible
            )
      (* dynamic function call *)
      | Var _ -> None

      (* static method call *)
      | Class_get (Id name1, _, Id name2) ->
          (* todo: name1 can be self/parent in traits, or static: *)
          let aclass = A.str_of_name name1 in
          let amethod = A.str_of_name name2 in
          (try
              Some (Class_php.lookup_method ~case_insensitive:true
                       (aclass, amethod) find_entity)
              (* could not find the method, this is bad, but
               * it's not our business here; this error will
               * be reported anyway in check_classes_php.ml anyway
               *)
            with
            | Not_found | Multi_found
            | Class_php.Use__Call|Class_php.UndefinedClassWhileLookup _ ->
                None
          )
      (* simple object call. If 'this->...()' then we can use lookup_method.
       * Being complete and handling any method calls like $o->...()
       * requires to know what is the type of $o which is quite
       * complicated ... so let's skip that for now.
       *
       * todo: special case also id(new ...)-> ?
       *)
      | Obj_get (IdSpecial (This, _), _, Id [name2]) ->
          (match env.in_class with
          | Some name1 ->
              let aclass = A.str_of_ident name1 in
              let amethod = A.str_of_ident name2 in
              (try
                Some (Class_php.lookup_method ~case_insensitive:true
                    (aclass, amethod) find_entity)
               with
               | Not_found | Multi_found
               | Class_php.Use__Call|Class_php.UndefinedClassWhileLookup _ ->
                   None
              )
          (* wtf? use of $this outside a class? *)
          | None -> None
          )

      (* dynamic call, not much we can do ... *)
      | _ -> None
      )

(*****************************************************************************)
(* Checks *)
(*****************************************************************************)

let check_defined env name ~incr_count =
  let (s, tok) = s_tok_of_ident name in
  match lookup_opt s !(env.vars) with
  | None ->
      (* todo? could still issue an error but with the information that
       * there was an extract/eval/... around?
       *)
      if !(env.bailout)
      then ()
      else
        let err =
          if env.in_long_lambda
          then E.UseOfUndefinedVariableInLambda s
          else
            let allvars =
              !(env.vars) |> Map_.to_list |> List.map fst in
            let suggest = Suggest_fix_php.suggest s allvars in
            E.UseOfUndefinedVariable (s, suggest)
        in
        E.fatal (A.tok_of_ident name) err

  | Some (_tok, scope, access_count) ->
      Hashtbl.add env.scope_vars_used tok scope;
      if incr_count then incr access_count

let check_used env vars =
  vars |> Map_.iter (fun s (tok, scope, aref) ->
    if !aref = 0
    then
      if unused_ok s
      then ()
      else
        (* if you use compact(), some variables may look unused but
         * they can actually be used. See variables_fp.php.
         *)
        if !(env.bailout)
        then ()
        else E.fatal tok (E.UnusedVariable (s, scope))
  )

let create_new_local_if_necessary ~incr_count env name =
  let (s, tok) = s_tok_of_ident name in
  match lookup_opt s !(env.vars) with
  (* new local variable implicit declaration.
   * todo: add in which nested scope? I would argue to add it
   * only in the current nested scope. If someone wants to use a
   * var outside the block, he should have initialized the var
   * in the outer context. Jslint does the same.
   *)
  | None ->
      env.vars := Map_.add s (tok, S.Local, ref 0) !(env.vars);
      Hashtbl.add env.scope_vars_used tok S.Local;

  | Some (_tok, scope, access_count) ->
      Hashtbl.add env.scope_vars_used tok scope;
      if incr_count then incr access_count

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* For each introduced variable (parameter, foreach variable, exception, etc),
 * we add the binding in the environment with a counter, a la checkModule.
 * We then check at use-time if something was declared before. We then
 * finally check when we exit a scope that all variables were actually used.
 *)
let rec program env prog =
  List.iter (stmt env) prog;

  (* we must check if people used the variables declared at the toplevel
   * context or via the param_post/param_get calls.
   * todo: check env.globals instead?
   *)
  check_used env !(env.vars)

(* ---------------------------------------------------------------------- *)
(* Functions/Methods *)
(* ---------------------------------------------------------------------- *)
and func_def env def =

  (* should not contain variables anyway, but does not hurt to check *)
  def.f_params |> List.iter (fun p -> Common.opt (expr env) p.p_default);

  let access_cnt =
    match def.f_kind with
    | Function  | AnonLambda | ShortLambda -> 0
    (* Don't report UnusedParameter for parameters of methods;
     * people sometimes override a method and don't use all
     * the parameters, hence the 1 value below.
     *
     * less: we we don't want parameters just in method interface
     *  to not be counted as unused Parameter
     * less: one day we will have an @override annotation in which
     *  case we can reconsider the above design decision.
     *)
    | Method -> 1
  in
  let oldvars = !(env.vars) in

  let enclosing_vars =
    match def.f_kind with
    (* for ShortLambda enclosing variables can be accessed  *)
    | ShortLambda -> Map_.to_list oldvars
    (* fresh new scope *)
    | _ ->
      (Env_php.globals_builtins |> List.map (fun s ->
       "$" ^ s, (Cst_php.fakeInfo s, S.Global, ref 1)
      )) @
      (* $this is now implicitly passed in use() for closures *)
      (try ["$this", Map_.find "$this" oldvars]
       with Not_found -> []
      )
  in

  let env = { env with
    vars = ref ((
      (def.f_params |> List.map (fun p ->
        let (s, tok) = s_tok_of_ident p.p_name in
        s, (tok, S.Param, ref access_cnt)
      )) @
      enclosing_vars
      ) |> Map_.of_list);

    (* reinitialize bailout for each function/method *)
    bailout = ref false;
  }
  in
  def.l_uses |> List.iter (fun (_is_ref, name) ->
    let (s, tok) = s_tok_of_ident name in
    check_defined ~incr_count:true { env with vars = ref oldvars} name;
    (* don't reuse same access count reference; the variable has to be used
     * again in this new scope.
     *)
    env.vars := Map_.add s (tok, S.Closed, ref 0) !(env.vars);
  );
  (* We put 1 as 'use_count' below because we are not interested
   * in error message related to $this. It's ok to not use $this
   * in a method.
   *)
  if def.f_kind = Method && not (A.is_static def.m_modifiers)
  then begin
     let tok = (Cst_php.fakeInfo "$this") in
     env.vars := Map_.add "$this" (tok, S.Class, ref 1) !(env.vars);
  end;

  List.iter (stmt env) def.f_body;
  let newvars =
    enclosing_vars |> List.fold_left (fun acc (x, _) -> Map_.remove x acc)
      !(env.vars)
  in
  check_used env newvars

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
and stmt env = function
  | FuncDef def -> func_def env def
  | ClassDef def -> class_def env def
  | ConstantDef def -> constant_def env def
  | TypeDef def -> typedef_def env def
  | NamespaceDef (_, qu, _) | NamespaceUse (_, qu, _) ->
    raise (Cst_php.TodoNamespace (A.tok_of_name qu))

  | Expr (e, _) -> expr env e
  (* todo: block scope checking when in strict mode? *)
  | Block (_, xs, _) -> stmtl env xs

  | If (_, e, st1, st2) ->
      expr env e;
      stmtl env [st1;st2]

  | Switch (_, e, xs) ->
      expr env e;
      casel env xs

  | While (_, e, xs) ->
      expr env e;
      stmtl env xs
  | Do (_, xs, e) ->
      stmtl env xs;
      expr env e
  | For (_, es1, es2, es3, xs) ->
      exprl env (es1 @ es2 @ es3);
      stmtl env xs

  | Foreach (_, e1, _, pattern, xs) ->
      expr env e1;
      foreach_pattern env pattern;
      stmtl env xs

  | Return (_, eopt)
  | Break (_, eopt) | Continue (_, eopt) ->
      Common.opt (expr env) eopt

  | Throw (_, e) -> expr env e
  | Try (_, xs, cs, fs) ->
      stmtl env xs;
      catches env (cs);
      finallys env (fs)

  | StaticVars (_, xs) ->
      xs |> List.iter (fun (name, eopt) ->
        Common.opt (expr env) eopt;
        let (s, tok) = s_tok_of_ident name in
        (* less: check if shadows something? *)
        env.vars := Map_.add s (tok, S.Static, ref 0) !(env.vars);
      )
  | Global (_, xs) ->
      xs |> List.iter (fun e ->
        (* should be an Var most of the time.
         * todo: should check in .globals that this variable actually exists
         *)
        match e with
        | Var name ->
            let (s, tok) = s_tok_of_ident name in
            env.vars := Map_.add s (tok, S.Global, ref 0) !(env.vars);
        (* todo: E.warning tok E.UglyGlobalDynamic *)
        | _ ->
            pr2 (str_of_any (Expr2 e));
            raise Todo
      )

(* The scope of catch is actually also at the function level in PHP ...
 *
 * todo: but for this one it is so ugly that I introduce a new scope
 * even outside strict mode. It's just too ugly.
 * todo: check unused
 * todo? could use local ? could have a UnusedExceptionParameter ?
 * less: could use ref 1, the exception is often not used
 *)
and catch env (_t, _hint_type, name, xs) =
  let (s, tok) = s_tok_of_ident name in
  env.vars := Map_.add s (tok, S.LocalExn, ref 0) !(env.vars);
  stmtl env xs

and finally env (_t, xs) =
  stmtl env xs

and case env = function
  | Case (_, e, xs) ->
      expr env e;
      stmtl env xs
  | Default (_, xs) ->
      stmtl env xs

and stmtl env xs = List.iter (stmt env) xs
and casel env xs = List.iter (case env) xs
and catches env xs = List.iter (catch env) xs
and finallys env xs = List.iter (finally env) xs

and foreach_pattern env pattern =

 (* People often use only one of the iterator when
  * they do foreach like   foreach(... as $k => $v).
  * We want to make sure that at least one of
  * the iterator variables is used, hence this trick to
  * make them share the same access count reference.
  *)
  let shared_ref = ref 0 in

  let rec aux e =
    (* look Cst_php.foreach_pattern to see the kinds of constructs allowed *)
    match e with
    | Var name ->
      let (s, tok) = s_tok_of_ident name in
      (* todo: if already in scope? shadowing? *)
      (* todo: if strict then introduce new scope here *)
      (* todo: scope_ref := S.LocalIterator; *)
      env.vars := Map_.add s (tok, S.LocalIterator, shared_ref) !(env.vars)
    | Ref (_, x) -> aux x
    | Arrow (e1, _, e2) -> aux e1; aux e2
    | List (_, xs, _) -> List.iter aux xs
    (* other kinds of lvalue are permitted too, but it's a little bit wierd
     * and very rarely used in www
     *)
    | Array_get (e, eopt) ->
      aux e;
      eopt |> Common.do_option (expr env)
    (* todo: E.warning tok E.WeirdForeachNoIteratorVar *)
    | _ ->
      failwith ("Warning: unexpected `foreach` value " ^
                   (str_of_any (Expr2 pattern)))
  in
  aux pattern


(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env e =
  match e with
  | IdSpecial (Eval, _) -> raise Todo
  | Int _ | Double _ | String _ -> ()

  | Var name ->
      check_defined ~incr_count:true env name

  | Id _name -> ()

  | Assign (e1, _, e2) ->
      (* e1 should be an lvalue *)
      (match e1 with
      | Var name ->
          (* Does an assignation counts as a use? If you only
           * assign and never use a variable what is the point?
           * This should be legal only for parameters passed by reference.
           * (note that here I talk about parameters, not arguments)
           *
           * TODO: hmm if you take a reference to something, then
           *  assigning something to it should be considered as a use too.
           *)
          create_new_local_if_necessary ~incr_count:false env name;

      (* extract all vars, and share the same reference *)
      | List (_, xs, _) ->
          (* Use the same trick than for LocalIterator *)
          let shared_ref = ref 0 in

          let rec aux = function
            (* should be an lvalue again *)
            | Var name ->
                let (s, tok) = s_tok_of_ident name in
                (match lookup_opt s !(env.vars) with
                | None ->
                    env.vars := Map_.add s (tok, S.ListBinded, shared_ref)
                      !(env.vars);
                | Some (_tok, _scope, _access_cnt) ->
                    ()
                )
            | ((Array_get _ | Obj_get _ | Class_get _) as e) ->
                expr env e
            | List (_, xs, _) -> List.iter aux xs
            | _ ->
                raise Todo
          in
          List.iter aux xs

      | Array_get (e_arr, e_opt) ->
          (* make sure the array is declared *)
          expr env e_arr;
          Common.opt (expr env) e_opt

      (* checks for use of undefined member should be in check_classes *)
      | Obj_get _ | Class_get _ ->
          (* just recurse on the whole thing so the code for Obj_get/Class_get
           * below will be triggered
           *)
          expr env e1

      | Call (Id[(("__builtin__eval_var", _) as name)], _args) ->
          env.bailout := true;
          E.warning (tok_of_ident name) E.DynamicCode

      (* can we have another kind of lvalue? *)
      | e ->
          pr2 (str_of_any (Expr2 e));
          failwith "WrongLvalue"
      );
      expr env e2

  | AssignOp (e1, _, e2) ->
      exprl env [e1;e2]

  | List _xs ->
      let tok = raise Todo in (* Meta_ast_php.toks_of_any (Expr2 e) |> List.hd *)
      failwith (spf "list(...) should be used only in an Assign context at %s"
                  (PI.string_of_info tok))
  (* Arrow used to be allowed only in Array and Foreach context, but now
   * can we also have code like yield $k => $v, so this is really a pair.
   *)
  | Arrow (e1, _, e2) ->
      exprl env [e1; e2]

  (* A mention of a variable in a unset() should not be really
   * considered as a use of variable. There should be another
   * statement in the function that actually uses the variable.
   *)
  | Call (Id[ ("__builtin__unset", _tok)], (_, args, _)) ->
      args |> List.iter (function
        (* should be an lvalue again *)
        (* less: The use of 'unset' on a variable is still not clear to me. *)
        | Var name ->
            check_defined ~incr_count:false env name
        (* Unsetting a field, seems like a valid use.
         * Unsetting a prop, not clear why you want that.
         * Unsetting a class var, not clear why you want that either
         *)
        | (Array_get (_, _) | Obj_get _ | Class_get _) as e ->
            (* make sure that the array used is actually defined *)
            expr env e
        | e ->
            pr2 (str_of_any (Expr2 e));
            raise Todo
      )
  (* special case, could factorize maybe with pass_var_by_ref *)
  | Call (Id[ ("sscanf", _tok)], (_, x::y::vars, _)) ->
      (* what if no x and y? wrong number of arguments, not our business here*)
      expr env x;
      expr env y;
      vars |> List.iter (function
      | Var name ->
          create_new_local_if_necessary ~incr_count:false env name
      (* less: wrong, it should be a variable? *)
      | e -> expr env e
      )

  (* The right fix is to forbid people to use isset/empty or unset on var.
   * todo: could have if(isset($x)) { ... code using $x}.
   *  maybe we should have a bailout_vars and skip further errors on $x.
   * todo: could have isset(Array_get(...) there too no?
   *)
  | Call (Id[ ("__builtin__isset", _tok)], (_,[Var _name],_)) ->
      ()
  (* http://php.net/manual/en/function.empty.php
   * "empty() does not generate a warning if the variable does not exist."
   *)
  | Call (Id[ ("__builtin__empty", _tok)], (_, [Var _name], _)) ->
      ()


  | Call (Id[ ((("__builtin__eval" | "__builtin__eval_var" |
               "extract" | "compact"
       ), _) as name)], _args) ->
      env.bailout := true;
      E.warning (tok_of_ident name) E.DynamicCode

      (* facebook specific? should be a hook instead to visit_prog? *)
  | Call(Id[("param_post"|"param_get"|"param_request"|"param_cookie"as kind,_)],
        (_,((ConsArray (_, array_args, _))::rest_param_xxx_args),_)) ->

      (* have passed a 'prefix' arg, or nothing *)
      if List.length rest_param_xxx_args <= 1
      then begin
        let prefix_opt =
          match rest_param_xxx_args with
          | [String(str_prefix, _tok_prefix)] ->
              Some str_prefix
          | [] ->
              (match kind with
              | "param_post" -> Some "post_"
              | "param_get" -> Some "get_"
              | "param_request" -> Some "req_"
              | "param_cookie" -> Some "cookie_"
              | _ -> raise Impossible
              )
          | _ ->
              (* less: display an error? weird argument to param_xxx func?*)
              None
        in
        prefix_opt |> Common.do_option (fun prefix ->
          array_args |> List.iter (function
          | Arrow(String(param_string, tok_param), _, _typ_param) ->
              let s = "$" ^ prefix ^ param_string in
              let tok = A.tok_of_ident (param_string, tok_param) in
              env.vars := Map_.add s (tok, S.Local, ref 0) !(env.vars);
              (* less: display an error? weird argument to param_xxx func? *)
          | _ -> ()
          )
        )
      end

  | Call (e, (_, es, _)) ->
      expr env e;

      (* getting the def for args passed by ref false positives fix *)
      let def_opt = funcdef_of_call_or_new_opt env e in
      let es_with_parameters =
        match def_opt with
        | None ->
            es |> List.map (fun e -> e, None)
        | Some def ->
            let params =
              def.Cst_php.f_params |> Cst_php.unparen |> Cst_php.uncomma_dots
            in
            let rec zip args params =
              match args, params with
              | [], [] -> []
              (* more params than arguments, maybe because default parameters *)
              | [], _y::_ys -> []
              (* more arguments than params, maybe because func_get_args() *)
              | x::xs, [] -> (x, None)::zip xs []
              | x::xs, y::ys -> (x, Some y)::zip xs ys
            in
            zip es params
      in

      es_with_parameters |> List.iter (fun (arg, param_opt) ->
        match arg, param_opt with
        (* keyword argument; do not consider this variable as unused.
         * We consider this variable as a pure comment here and just pass over.
         * todo: could make sure they are not defined in the current
         * environment in strict mode? and if they are, shout because of
         * bad practice?
         *)
        | Assign (Var _name, _, e2), _ ->
            expr env e2
        (* a variable passed by reference, this can considered a new decl *)
        | Var name, Some {Cst_php.p_ref = Some _;_} ->

            (* if was already assigned and passed by refs,
             * increment its use counter then.
             * less: or should we increase only if inout param?
             *)
            create_new_local_if_necessary ~incr_count:true env name

        | _ -> expr env arg
      )


  | IdSpecial (This,tok) ->
      let name = "$this", tok in
      (* when we do use($this) in closures, we create a fresh $this variable
       * with a refcount of 0, so we need to increment it here.
       *)
      check_defined ~incr_count:true env name

  (* array used as an rvalue; the lvalue case should be handled in Assign. *)
  | Array_get (e, eopt) ->
      expr env e;
      Common.opt (expr env) eopt

  | Obj_get (e1, _, e2) ->
      expr env e1;
      (match e2 with
      (* with 'echo $o->$v' we have a dynamic field, we need to visit
       * e2 to mark $v as used at least.
       *)
      | Id _name  -> ()
      | _ -> expr env e2
      )

  | Class_get (e1, _, e2) ->
      expr env e1;
      (match e2 with
      (* with 'echo A::$v' we should not issue a UseOfUndefinedVariable,
       * check_classes_php.ml will handle this case.
       *)
      | Var _  -> ()
      | _ -> expr env e2
      )

  | New (tok, e, es) ->
      expr env (Call (Class_get(e, tok, Id[ (wrap_fake "__construct")]), fb es))

  | InstanceOf (_, e1, e2) -> exprl env [e1;e2]

  | Infix (_, e) | Postfix (_, e) | Unop (_, e) -> expr env e
  | Binop (e1, _, e2) -> exprl env [e1; e2]
  | Guil (_, xs, _) -> exprl env xs

  | Ref (_, e) | Unpack e -> expr env e

  | ConsArray (_, xs, _) -> array_valuel env xs
  | Collection (_n, (_, xs, _)) ->
    array_valuel env xs
  | Xhp x -> xml env x

  | CondExpr (e1, e2, e3) -> exprl env [e1; e2; e3]
  | Cast (_, e) -> expr env e

  | Lambda def ->
      let in_long_lambda =
        match def.f_kind with ShortLambda -> false | _ -> true
      in
      func_def { env with in_long_lambda } def

and array_value env x =
  match x with
  | Arrow (e1, _, e2) -> exprl env [e1; e2]
  | e -> expr env e

and xml env x =
  x.xml_attrs |> List.iter (fun (_name, xhp_attr) -> expr env xhp_attr);
  x.xml_body |> List.iter (xhp env)

and xhp env = function
  | XhpText _s -> ()
  | XhpExpr e -> expr env e
  | XhpXml x -> xml env x

and exprl env xs = List.iter (expr env) xs
and array_valuel env xs = List.iter (array_value env) xs

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

(* checks for use of undefined members should be in check_classes, but
 * it's hard to do locally.
 *
 * todo: inline the code of traits?
 *)
and class_def env def =
  let env = { env with in_class = Some def.c_name } in
  List.iter (constant_def env) def.c_constants;
  List.iter (class_var env) def.c_variables;
  List.iter (method_def env) def.c_methods

(* cst_body should be a static scalar so there should not be any
 * variable in it so in theory we don't need to check it ... doesn't
 * hurt though, one day maybe this will change.
 *)
and constant_def env def =
  Common.opt (expr env) def.cst_body

(* type definitions do not contain any variables *)
and typedef_def _env _def =
  ()

and class_var env v =
  Common.opt (expr env) v.cv_value

and method_def env x = func_def env x

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check_and_annotate_program2 find_entity prog =
  let env = {
    (* less: should be a in globals field instead? *)
    vars = ref (Env_php.globals_builtins |> List.map (fun s ->
       "$" ^ s, (Cst_php.fakeInfo s, S.Global, ref 1)
       ) |> Map_.of_list);
    db = find_entity;
    in_class = None;
    in_long_lambda = false;
    bailout = ref false;
    scope_vars_used = Hashtbl.create 101;
  }
  in
  let ast = Ast_php_build.program prog in
  program env ast;

  (* annotating the scope of Var *)
  (Cst_php.Program prog) |>
    Visitor_php.mk_visitor { Visitor_php.default_visitor with
    Visitor_php.kexpr = (fun (k, _) x ->
      match x with
      | Cst_php.IdVar (dname, aref) ->
          let tok = Cst_php.info_of_dname dname in
          (try
            aref := Hashtbl.find env.scope_vars_used tok
          (* keep NoScope *)
          with Not_found -> ()
          )
      | _ -> k x
    );
  };
  ()

let check_and_annotate_program a b =
  Common.profile_code "Checker.variables" (fun () ->
    check_and_annotate_program2 a b)

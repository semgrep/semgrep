(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
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

open AST_generic
module H = AST_generic_helpers
module V = Visitor_AST

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Various helper functions to extract subparts of AST elements.
 *
 *)

let go_really_deeper_stmt = ref true

(*****************************************************************************)
(* Sub-expressions and sub-statements *)
(*****************************************************************************)

let subexprs_of_any_list xs =
  xs
  |> List.fold_left
       (fun x -> function
         | E e -> e :: x
         | _ -> x)
       []

(* used for really deep statement matching *)

let subexprs_of_stmt_kind = function
  (* 1 *)
  | ExprStmt (e, _)
  | DoWhile (_, _, e)
  | DefStmt (_, VarDef { vinit = Some e; _ })
  | DefStmt (_, FieldDefColon { vinit = Some e; _ })
  | For (_, ForEach (_, _, e), _)
  | Continue (_, LDynamic e, _)
  | Break (_, LDynamic e, _)
  | Throw (_, e, _) ->
      [ e ]
  | While (_, cond, _)
  | If (_, cond, _, _) ->
      [ H.cond_to_expr cond ]
  (* opt *)
  | Switch (_, condopt, _) -> (
      match condopt with
      | None -> []
      | Some cond -> [ H.cond_to_expr cond ])
  | Return (_, eopt, _) -> Option.to_list eopt
  (* n *)
  | For (_, MultiForEach es, _) ->
      es
      |> List.filter_map (function
           | FE (_, _, e) -> Some [ e ]
           | FECond ((_, _, e1), _, e2) -> Some [ e1; e2 ]
           | FEllipsis _ -> None)
      |> List.concat
  | For (_, ForClassic (xs, eopt1, eopt2), _) ->
      (xs
      |> Common.map_filter (function
           | ForInitExpr e -> Some e
           | ForInitVar (_, vdef) -> vdef.vinit))
      @ Option.to_list eopt1 @ Option.to_list eopt2
  | Assert (_, (_, args, _), _) ->
      args
      |> Common.map_filter (function
           | Arg e -> Some e
           | _ -> None)
  | For (_, ForIn (_, es), _) -> es
  | OtherStmt (_op, xs) -> subexprs_of_any_list xs
  | OtherStmtWithStmt (_, xs, _) -> subexprs_of_any_list xs
  (* 0 *)
  | DirectiveStmt _
  | Block _
  | For (_, ForEllipsis _, _)
  | Continue _
  | Break _
  | Label _
  | Goto _
  | Try _
  | DisjStmt _
  | DefStmt _
  | WithUsingResource _ ->
      []

let subexprs_of_stmt st = subexprs_of_stmt_kind st.s

let subexprs_of_args args =
  args |> unbracket
  |> Common.map_filter (function
       | Arg e
       | ArgKwd (_, e)
       | ArgKwdOptional (_, e) ->
           Some e
       | ArgType _
       | OtherArg _ ->
           None)

(* used for deep expression matching *)
let subexprs_of_expr with_symbolic_propagation e =
  match e.e with
  | N (Id (_, { id_svalue = { contents = Some (Sym e1) }; _ }))
    when with_symbolic_propagation ->
      [ e1 ]
  | L _
  | N _
  | IdSpecial _
  | Ellipsis _
  | TypedMetavar _ ->
      []
  | DotAccess (e, _, _)
  | Await (_, e)
  | Cast (_, _, e)
  | Ref (_, e)
  | DeRef (_, e)
  | DeepEllipsis (_, e, _)
  | DotAccessEllipsis (e, _) ->
      [ e ]
  | Assign (e1, _, e2)
  | AssignOp (e1, _, e2)
  | ArrayAccess (e1, (_, e2, _))
  (* not sure we always want to return 'e1' here *) ->
      [ e1; e2 ]
  | Conditional (e1, e2, e3) -> [ e1; e2; e3 ]
  | Seq xs -> xs
  | Record (_, flds, _) ->
      flds |> Common2.map_flatten (function F st -> subexprs_of_stmt st)
  | Container (_, xs) -> unbracket xs
  | Comprehension (_, (_, (e, xs), _)) ->
      e
      :: (xs
         |> Common.map (function
              | CompFor (_, _pat, _, e) -> e
              | CompIf (_, e) -> e))
  | New (_, _t, args) -> subexprs_of_args args
  | Call (e, args) ->
      (* not sure we want to return 'e' here *)
      e :: subexprs_of_args args
  | SliceAccess (e1, e2) ->
      e1
      :: (e2 |> unbracket
         |> (fun (a, b, c) -> [ a; b; c ])
         |> Common.map Option.to_list |> List.flatten)
  | Yield (_, eopt, _) -> Option.to_list eopt
  | StmtExpr st -> subexprs_of_stmt st
  | OtherExpr (_, anys) ->
      (* in theory we should go deeper in any *)
      subexprs_of_any_list anys
  | Alias (_, e1) -> [ e1 ]
  | Lambda def -> subexprs_of_stmt (H.funcbody_to_stmt def.fbody)
  (* TODO? or call recursively on e? *)
  | ParenExpr (_, e, _) -> [ e ]
  | Xml { xml_attrs; xml_body; _ } ->
      List.filter_map
        (function
          | XmlAttr (_, _, e)
          | XmlAttrExpr (_, e, _) ->
              Some e
          | _ -> None)
        xml_attrs
      @ List.filter_map
          (function
            | XmlExpr (_, Some e, _) -> Some e
            | XmlXml xml -> Some (Xml xml |> AST_generic.e)
            | _ -> None)
          xml_body
  | RegexpTemplate ((_l, e, _r), _opt) -> [ e ]
  (* currently skipped over but could recurse *)
  | Constructor _
  | AnonClass _
  | LetPattern _ ->
      []
  | DisjExpr _ -> raise Common.Impossible
  [@@profiling]

(* Need this wrapper because [@@profiling] has the side-effect of removing labels. *)
let subexprs_of_expr ?(symbolic_propagation = false) e =
  subexprs_of_expr symbolic_propagation e

(* This is similar to subexprs_of_expr, but used for the
 * *implicit* deep expression matching. Here we should not go as deep.
 *
 * For example, we should allow patterns like 'foo();'
 * to also match code like 'x = foo();' or even 'print(foo());'
 * but not necessarily any expressions like 'bar() || foo();'.
 * See tests/ts/deep_exprtmt.ts for more examples.
 *)
let subexprs_of_expr_implicit with_symbolic_propagation e =
  match e.e with
  | N (Id (_, { id_svalue = { contents = Some (Sym e1) }; _ }))
    when with_symbolic_propagation ->
      [ e1 ]
  (* cases where we extract a subexpr *)
  | Assign (_e1, _, e2)
  | AssignOp (_e1, _, e2) ->
      [ e2 ]
  (* TODO? special case for Bash and Dockerfile to prevent
   * 'RUN b' to also match 'RUN a && b' (but still allowing
   *  'RUN a' to match 'RUN a && b'?)
   * | Call ({ e = IdSpecial (Op And, _); _}, (_, Arg e1::_, _)) -> [e1]
   *)
  | Call (e, args) ->
      (* TODO: ugly we add 'e' also here for cases like
       * bar().then(stuff) which is parsed as (bar().then)(stuff)
       * and we need to go in left part.
       *)
      e :: subexprs_of_args args
  | Cast (_, _, e)
  | ParenExpr (_, e, _)
  | Await (_, e) ->
      [ e ]
  | Yield (_, eopt, _) -> Option.to_list eopt
  | StmtExpr st -> subexprs_of_stmt st
  (* TODO: ugly, but we have pattern like 'db.find(...)' that we
   * also want to match code like 'db.find().then(stuff).
   *)
  | DotAccess (e, _, _) -> [ e ]
  (* TODO: ugly but in semgrep-rules/python/.../flush.yaml there is
   * '$F.name' that is matching cmd = [stuff, fout.name, otherstuff].
   * They should rewrite the rule and use '... <... $F.name ...>' there.
   *)
  | Container (_, xs) -> unbracket xs
  (* TODO: ugly but in semgrep-rules/terraform/.../missing-athena...yaml
   * we look for '{ ... encryption_configuration {...} ...}' and
   * the encryption_configuration can actually be nested deeper.
   * They should rewrite the rule.
   *)
  | Record (_, flds, _) ->
      flds |> Common2.map_flatten (function F st -> subexprs_of_stmt st)
  (* cases where we should not extract a subexpr *)
  | L _
  | N _
  | IdSpecial _
  | Ellipsis _ ->
      []
  | Ref (_, _e)
  | DeRef (_, _e) ->
      []
  | Conditional (_e1, _e2, _e3) -> []
  | Seq _xs -> []
  | ArrayAccess (_e1, (_, _e2, _)) -> []
  | SliceAccess (_e1, _e2) -> []
  | Comprehension (_, (_, (_e, _xs), _)) -> []
  | New (_, _t, _args) -> []
  | OtherExpr (_, _anys) -> []
  | Alias (_, _e1) -> []
  | Xml _xmlbody -> []
  | Constructor _ -> []
  | RegexpTemplate _ -> []
  | AnonClass _def -> []
  | Lambda _def -> []
  | LetPattern _ -> []
  | TypedMetavar _
  | DeepEllipsis _
  | DotAccessEllipsis _
  | DisjExpr _ ->
      raise Common.Impossible
  [@@profiling]

(* Need this wrapper because [@@profiling] has the side-effect of removing labels. *)
let subexprs_of_expr_implicit ?(symbolic_propagation = false) e =
  subexprs_of_expr_implicit symbolic_propagation e

(* used for deep statement matching *)
let substmts_of_stmt st =
  match st.s with
  (* 0 *)
  | DirectiveStmt _
  | ExprStmt _
  | Return _
  | Continue _
  | Break _
  | Goto _
  | Throw _
  | Assert _
  | OtherStmt _ ->
      []
  (* 1 *)
  | While (_, _, st)
  | DoWhile (_, st, _)
  | For (_, _, st)
  | Label (_, st)
  | OtherStmtWithStmt (_, _, st) ->
      [ st ]
  (* 2 *)
  | If (_, _, st1, st2) -> st1 :: Option.to_list st2
  | WithUsingResource (_, st1, st2) -> [ st1; st2 ]
  (* n *)
  | Block (_, xs, _) -> xs
  | Switch (_, _, xs) ->
      xs
      |> Common.map (function
           | CasesAndBody (_, st) -> [ st ]
           | CaseEllipsis _ -> [])
      |> List.flatten
  | Try (_, st, xs, opt) -> (
      [ st ]
      @ (xs |> Common.map Common2.thd3)
      @
      match opt with
      | None -> []
      | Some (_, st) -> [ st ])
  | DisjStmt _ -> raise Common.Impossible
  (* this may slow down things quite a bit *)
  | DefStmt (_ent, def) -> (
      if not !go_really_deeper_stmt then []
      else
        match def with
        | VarDef _
        | FieldDefColon _
        | EnumEntryDef _
        | TypeDef _
        | MacroDef _
        | Signature _
        | UseOuterDecl _
        (* recurse? *)
        | ModuleDef _
        | OtherDef _ ->
            []
        (* this will add lots of substatements *)
        | FuncDef def -> [ H.funcbody_to_stmt def.fbody ]
        | ClassDef def ->
            def.cbody |> unbracket |> Common.map (function F st -> st))

(*****************************************************************************)
(* Visitors  *)
(*****************************************************************************)
(* TODO: move in pfff at some point *)
let do_visit_with_ref mk_hooks any =
  let res = ref [] in
  let hooks = mk_hooks res in
  let vout = V.mk_visitor hooks in
  vout any;
  List.rev !res

let lambdas_in_expr e =
  do_visit_with_ref
    (fun aref ->
      {
        V.default_visitor with
        V.kexpr =
          (fun (k, _) e ->
            match e.e with
            | Lambda def -> Common.push def aref
            | _ -> k e);
      })
    (E e)
  [@@profiling]

(* opti: using memoization speed things up a bit too
 * (but again, this is still slow when called many many times).
 * todo? note that this is not the optimal memoization we can do because
 * using Hashtbl where the key is a full expression can be slow (hashing
 * huge expressions still takes some time). It would be better to
 * return a unique identifier to each expression to remove the hashing cost.
 *)
let hmemo = Hashtbl.create 101

let lambdas_in_expr_memo a =
  Common.memoized hmemo a (fun () -> lambdas_in_expr a)
  [@@profiling]

(*****************************************************************************)
(* Really substmts_of_stmts *)
(*****************************************************************************)

let flatten_substmts_of_stmts xs =
  (* opti: using a ref, List.iter, and Common.push instead of a mix of
   * List.map, List.flatten and @ below speed things up
   * (but it is still slow when called many many times)
   *)
  let res = ref [] in
  let changed = ref false in

  let rec aux x =
    (* return the current statement first, and add substmts *)
    Common.push x res;

    (* this can be really slow because lambdas_in_expr() below can be called
     * a zillion times on big files (see tests/PERF/) if we do the
     * matching naively in m_stmts_deep.
     *)
    (if !go_really_deeper_stmt then
     let es = subexprs_of_stmt x in
     (* getting deeply nested lambdas stmts *)
     let lambdas = es |> Common.map lambdas_in_expr_memo |> List.flatten in
     lambdas
     |> Common.map (fun def -> H.funcbody_to_stmt def.fbody)
     |> List.iter aux);

    let xs = substmts_of_stmt x in
    match xs with
    | [] -> ()
    | xs ->
        changed := true;
        xs |> List.iter aux
  in
  xs |> List.iter aux;
  if !changed then
    match !res with
    | [] -> None
    | last :: _ ->
        (* Return the last element of the list as a pair.
           This is used as part of the caching optimization. *)
        Some (List.rev !res, last)
  else None
  [@@profiling]

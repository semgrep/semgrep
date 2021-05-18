(*s: semgrep/matching/SubAST_generic.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
(*e: pad/r2c copyright *)

open AST_generic
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
  xs |> List.fold_left (fun x -> function E e -> e :: x | _ -> x) []

(* used for really deep statement matching *)
let subexprs_of_stmt st =
  match st.s with
  (* 1 *)
  | ExprStmt (e, _)
  | If (_, e, _, _)
  | While (_, e, _)
  | DoWhile (_, _, e)
  | DefStmt (_, VarDef { vinit = Some e; _ })
  | DefStmt (_, FieldDefColon { vinit = Some e; _ })
  | For (_, ForEach (_, _, e), _)
  | Continue (_, LDynamic e, _)
  | Break (_, LDynamic e, _)
  | Throw (_, e, _) ->
      [ e ]
  (* opt *)
  | Switch (_, eopt, _) | Return (_, eopt, _) | OtherStmtWithStmt (_, eopt, _)
    ->
      Common.opt_to_list eopt
  (* n *)
  | For (_, ForClassic (xs, eopt1, eopt2), _) ->
      ( xs
      |> Common.map_filter (function
           | ForInitExpr e -> Some e
           | ForInitVar (_, vdef) -> vdef.vinit) )
      @ Common.opt_to_list eopt1 @ Common.opt_to_list eopt2
  | Assert (_, e1, e2opt, _) -> e1 :: Common.opt_to_list e2opt
  | For (_, ForIn (_, es), _) -> es
  | OtherStmt (_op, xs) -> subexprs_of_any_list xs
  (* 0 *)
  | DirectiveStmt _ | Block _
  | For (_, ForEllipsis _, _)
  | Continue _ | Break _ | Label _ | Goto _ | Try _ | DisjStmt _ | DefStmt _
  | WithUsingResource _ ->
      []

(*s: function [[SubAST_generic.subexprs_of_expr]] *)
(* used for deep expression matching *)
let subexprs_of_expr e =
  match e with
  | L _ | N _ | IdSpecial _ | Ellipsis _ | TypedMetavar _ -> []
  | DotAccess (e, _, _)
  | Await (_, e)
  | Cast (_, e)
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
  | Tuple (_, xs, _) | Seq xs -> xs
  | Record (_, flds, _) ->
      flds
      |> Common2.map_flatten (function
           | FieldStmt st -> subexprs_of_stmt st
           | FieldSpread (_, e) -> [ e ])
  | Container (_, xs) -> unbracket xs
  | Call (e, args) ->
      (* not sure we want to return 'e' here *)
      e
      :: ( args |> unbracket
         |> Common.map_filter (function
              | Arg e | ArgKwd (_, e) -> Some e
              | ArgType _ | ArgOther _ -> None) )
  | SliceAccess (e1, e2) ->
      e1
      :: ( e2 |> unbracket
         |> (fun (a, b, c) -> [ a; b; c ])
         |> List.map Common.opt_to_list
         |> List.flatten )
  | Yield (_, eopt, _) -> Common.opt_to_list eopt
  | OtherExpr (_, anys) ->
      (* in theory we should go deeper in any *)
      subexprs_of_any_list anys
  | Lambda def -> subexprs_of_stmt def.fbody
  (* currently skipped over but could recurse *)
  | Constructor _ | AnonClass _ | Xml _ | LetPattern _ | MatchPattern _ -> []
  | DisjExpr _ -> raise Common.Impossible
  [@@profiling]

(*e: function [[SubAST_generic.subexprs_of_expr]] *)

(*s: function [[SubAST_generic.subexprs_of_stmt]] *)
(*e: function [[SubAST_generic.subexprs_of_stmt]] *)

(*s: function [[SubAST_generic.substmts_of_stmt]] *)
(* used for deep statement matching *)
let substmts_of_stmt st =
  match st.s with
  (* 0 *)
  | DirectiveStmt _ | ExprStmt _ | Return _ | Continue _ | Break _ | Goto _
  | Throw _ | Assert _ | OtherStmt _ ->
      []
  (* 1 *)
  | While (_, _, st)
  | DoWhile (_, st, _)
  | For (_, _, st)
  | Label (_, st)
  | OtherStmtWithStmt (_, _, st) ->
      [ st ]
  (* 2 *)
  | If (_, _, st1, st2) -> st1 :: Common.opt_to_list st2
  | WithUsingResource (_, st1, st2) -> [ st1; st2 ]
  (* n *)
  | Block (_, xs, _) -> xs
  | Switch (_, _, xs) ->
      xs
      |> List.map (function
           | CasesAndBody (_, st) -> [ st ]
           | CaseEllipsis _ -> [])
      |> List.flatten
  | Try (_, st, xs, opt) -> (
      [ st ]
      @ (xs |> List.map Common2.thd3)
      @ match opt with None -> [] | Some (_, st) -> [ st ] )
  | DisjStmt _ -> raise Common.Impossible
  (* this may slow down things quite a bit *)
  | DefStmt (_ent, def) -> (
      if not !go_really_deeper_stmt then []
      else
        match def with
        | VarDef _ | FieldDefColon _ | TypeDef _ | MacroDef _ | Signature _
        | UseOuterDecl _
        (* recurse? *)
        | ModuleDef _ | OtherDef _ ->
            []
        (* this will add lots of substatements *)
        | FuncDef def -> [ def.fbody ]
        | ClassDef def ->
            def.cbody |> unbracket
            |> Common.map_filter (function
                 | FieldStmt st -> Some st
                 | FieldSpread _ -> None) )

(*e: function [[SubAST_generic.substmts_of_stmt]] *)

(*****************************************************************************)
(* Visitors  *)
(*****************************************************************************)
(*s: function [[SubAST_generic.do_visit_with_ref]] *)
(* TODO: move in pfff at some point *)
let do_visit_with_ref mk_hooks any =
  let res = ref [] in
  let hooks = mk_hooks res in
  let vout = V.mk_visitor hooks in
  vout any;
  List.rev !res

(*e: function [[SubAST_generic.do_visit_with_ref]] *)

(*s: function [[SubAST_generic.lambdas_in_expr]] *)
let lambdas_in_expr e =
  do_visit_with_ref
    (fun aref ->
      {
        V.default_visitor with
        V.kexpr =
          (fun (k, _) e ->
            match e with Lambda def -> Common.push def aref | _ -> k e);
      })
    (E e)
  [@@profiling]

(*e: function [[SubAST_generic.lambdas_in_expr]] *)

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

(*s: function [[SubAST_generic.flatten_substmts_of_stmts]] *)
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
    ( if !go_really_deeper_stmt then
      let es = subexprs_of_stmt x in
      (* getting deeply nested lambdas stmts *)
      let lambdas = es |> List.map lambdas_in_expr_memo |> List.flatten in
      lambdas |> List.map (fun def -> def.fbody) |> List.iter aux );

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

(*e: function [[SubAST_generic.flatten_substmts_of_stmts]] *)

(*e: semgrep/matching/SubAST_generic.ml *)

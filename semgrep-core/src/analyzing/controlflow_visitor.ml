(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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
module G = AST_generic
open Controlflow
module F = Controlflow

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A smaller wrapper around the AST generic visitor to also handles
 * control flow nodes.
 *
 * This file is deprecated. You should use the IL and its CFG in IL.cfg.
 *
 * Less useful now that we have Controlflow.exprs_of_node and
 * Controlflow.fold_on_node_and_expr.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type visitor_in = Visitor_AST.visitor_in

type visitor_out = Controlflow.node -> unit

(*****************************************************************************)
(* Entry Points *)
(*****************************************************************************)

let mk_visitor vin =
  let visitor = Visitor_AST.mk_visitor vin in
  fun node ->
    match node.F.n with
    (* Nothing is needed if the node has no expr information*)
    | F.Enter
    | F.Exit
    | F.TrueNode
    | F.FalseNode
    | F.DoHeader
    | F.ForHeader
    | F.SwitchEnd
    | F.Case
    | F.Default
    | F.TryHeader
    | F.CatchStart
    | F.Catch
    | F.TryEnd
    | F.Join
    | F.Continue
    | F.Break
    | F.Return None
    | F.SwitchHeader None
    | F.OtherStmtWithStmtHeader (_, None) ->
        ()
    (* cond *)
    | F.IfHeader cond
    | F.SwitchHeader (Some cond) -> (
        match cond with
        | G.Cond e -> visitor (G.E e)
        (* TODO *)
        | G.OtherCond _ -> ())
    (* expr *)
    | F.WhileHeader expr
    | F.DoWhileTail expr
    | F.Throw expr
    | F.Return (Some expr)
    | F.OtherStmtWithStmtHeader (_, Some expr) ->
        visitor (G.E expr)
    | F.ForeachHeader (pat, e) -> visitor (G.E (G.LetPattern (pat, e) |> G.e))
    | F.SimpleNode x ->
        let any = F.any_of_simple_node x in
        visitor any

(*****************************************************************************)
(* Alternative visitor *)
(*****************************************************************************)

let exprs_of_node node =
  match node.n with
  | Enter
  | Exit
  | TrueNode
  | FalseNode
  | DoHeader
  | ForHeader
  | SwitchEnd
  | Case
  | Default
  | TryHeader
  | CatchStart
  | Catch
  | TryEnd
  | Join
  | Continue
  | Break
  | Return None
  | SwitchHeader None
  | OtherStmtWithStmtHeader (_, None) ->
      []
  (* cond *)
  | IfHeader cond
  | SwitchHeader (Some cond) -> (
      match cond with
      | Cond e -> [ e ]
      | OtherCond _ -> [])
  (* expr *)
  | WhileHeader expr
  | DoWhileTail expr
  | Throw expr
  | Return (Some expr)
  | OtherStmtWithStmtHeader (_, Some expr) ->
      [ expr ]
  | ForeachHeader (pat, expr) -> [ G.LetPattern (pat, expr) |> G.e ]
  | SimpleNode x -> (
      match x with
      | ExprStmt e -> [ e ]
      | Assert (_, (_, args, _)) ->
          args
          |> Common.map_filter (function
               | G.Arg e -> Some e
               | _ -> None)
      (* TODO: should transform VarDef in it in Assign *)
      | DefStmt _ -> []
      | DirectiveStmt _ -> []
      (* TODO: should use visitor! *)
      | OtherStmt _ -> []
      (* TODO: should transform in Assign *)
      | Parameter _p -> [])

(* this can also be used as an iter; just pass () to acc *)
let fold_on_node_and_expr hook (flow : flow) acc =
  flow.graph#nodes#fold
    (fun acc (ni, node) ->
      let xs = exprs_of_node node in
      xs |> List.fold_left (fun acc e -> hook (ni, node) e acc) acc)
    acc

(* Yoann Padioleau
 *
 * Copyright (C) 2009, 2010, 2011 Facebook
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

open AST_generic
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A Control Flow Graph (CFG) for AST generic.
 *
 * Note that this is just for intra-procedural analysis. The CFG covers
 * just one function. For inter-procedural analysis you may want to look
 * at pfff/graph_code/ (or invest in learning Datalog).
 *
 * history:
 *  - CFG for C for coccinelle
 *  - CFG for PHP for checkModule at Facebook
 *  - CFG for AST generic for scheck at r2c
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type node = {
  (* later: For now we just have node_kind, but with some data-flow
   *  analysis or with temporal logic we may want to add extra information
   *  in each CFG nodes.
   * alt: We could also record such extra information in an external table
   *  that maps Ograph_extended.nodei, that is nodeid, to some information.
   *)
  n : node_kind;
  (* for error report *)
  i : Parse_info.t option;
}

and node_kind =
  (* special fake cfg nodes *)
  | Enter
  | Exit
  (* alt: An alternative is to store such information in the edges, but
   * experience shows it's easier to encode it via regular nodes
   *)
  | TrueNode
  | FalseNode
  | Join
  | IfHeader of expr
  | WhileHeader of expr
  | DoHeader
  | DoWhileTail of expr
  | ForHeader
  | ForeachHeader of pattern * expr
  | OtherStmtWithStmtHeader of other_stmt_with_stmt_operator * expr option
  | SwitchHeader of expr option
  | SwitchEnd
  | Case
  | Default
  | Return of expr option
  | Break
  | Continue
  | TryHeader
  | CatchStart
  | Catch
  | TryEnd
  | Throw of expr
  (* statements without multiple outgoing or ingoing edges, such as
   * expression statements.
   *)
  | SimpleNode of simple_node

(* not used for now, was used in coccinelle:
   | BlockStart of tok (* { *)
   | BlockEnd of tok (* } *)
   | Else
   | Elsif
*)

(* mostly a copy-paste of a subset of Ast.stmt *)
and simple_node =
  (* later: some expr includes Exit, Eval, Include, etc which
   * also have an influence on the control flow ...
   * We may want to uplift those constructors here and have
   * a better expr type
   *)
  | ExprStmt of expr
  | DefStmt of definition
  | DirectiveStmt of directive
  | Assert of tok * expr * expr option
  | OtherStmt of other_stmt_operator * any list
  (* not part of Ast.stmt but useful to have in CFG for
   * dataflow analysis purpose *)
  | Parameter of parameter

(* For now there is just one kind of edge.
 * later: we may have more, see the ShadowNode idea of Julia Lawall?
 *)
type edge = Direct

type flow = (node, edge) CFG.t

type nodei = Ograph_extended.nodei

(*****************************************************************************)
(* String of *)
(*****************************************************************************)

(* This is useful in graphviz and in dataflow analysis result tables
 * to just get a quick idea of what a node is (without too much details).
 *)
let short_string_of_node_kind nkind =
  match nkind with
  | Enter -> "<enter>"
  | Exit -> "<exit>"
  | TrueNode -> "<TRUE path>"
  | FalseNode -> "<FALSE path>"
  | Join -> "<join>"
  | IfHeader _ -> "if(...)"
  | WhileHeader _ -> "while(...)"
  | DoHeader -> "do"
  | DoWhileTail _ -> "while(...);"
  | ForHeader -> "for(...)"
  | ForeachHeader _ -> "foreach(...)"
  | OtherStmtWithStmtHeader _ -> "<otherstmtheader>"
  | Return _ -> "return ...;"
  | Continue -> "continue ...;"
  | Break -> "break ...;"
  | SwitchHeader _ -> "switch(...)"
  | SwitchEnd -> "<endswitch>"
  | Case -> "case: ..."
  | Default -> "default:"
  | TryHeader -> "try"
  | CatchStart -> "<catchstart>"
  | Catch -> "catch(...)"
  | TryEnd -> "<endtry>"
  | Throw _ -> "throw ...;"
  | SimpleNode x -> (
      match x with
      | ExprStmt _ -> "<expt_stmt>;"
      | DefStmt _ -> "<def>"
      | DirectiveStmt _ -> "<directive>"
      | Assert _ -> "<assert>"
      | Parameter _ -> "<param>"
      | OtherStmt _ -> "<other_stmt>")

let short_string_of_node node = short_string_of_node_kind node.n

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)
let simple_node_of_stmt_opt stmt =
  match stmt.s with
  | G.ExprStmt (e, _) -> Some (ExprStmt e)
  | G.Assert (t, e1, e2, _sc) -> Some (Assert (t, e1, e2))
  | G.DefStmt x -> Some (DefStmt x)
  | G.DirectiveStmt x -> Some (DirectiveStmt x)
  | G.OtherStmt (a, b) -> Some (OtherStmt (a, b))
  | G.Block _
  | G.If (_, _, _, _)
  | G.While (_, _, _)
  | G.DoWhile (_, _, _)
  | G.For (_, _, _)
  | G.Switch (_, _, _)
  | G.Match (_, _, _)
  | G.Return _
  | G.Continue _
  | G.Break _
  | G.Label (_, _)
  | G.Goto _
  | G.Throw _
  | G.Try (_, _, _, _)
  | G.WithUsingResource (_, _, _)
  | G.OtherStmtWithStmt _
  | G.DisjStmt _ ->
      None

let any_of_simple_node = function
  | ExprStmt e -> G.S (G.ExprStmt (e, G.sc) |> G.s)
  | Assert (t, e1, e2) -> G.S (G.Assert (t, e1, e2, G.sc) |> G.s)
  | DefStmt x -> G.S (G.DefStmt x |> G.s)
  | DirectiveStmt x -> G.S (G.DirectiveStmt x |> G.s)
  | OtherStmt (a, b) -> G.S (G.OtherStmt (a, b) |> G.s)
  | Parameter x -> G.Pa x

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let find_node f (cfg : flow) =
  cfg.graph#nodes#tolist
  |> Common.find_some (fun (nodei, node) -> if f node then Some nodei else None)

let find_exit cfg = find_node (fun node -> node.n = Exit) cfg

let find_enter cfg = find_node (fun node -> node.n = Enter) cfg

(* using internally graphviz dot and ghostview on X11 *)
let (display_flow : flow -> unit) =
 fun flow ->
  flow.graph
  |> Ograph_extended.print_ograph_mutable_generic
       ~s_of_node:(fun (_nodei, node) ->
         (short_string_of_node_kind node.n, None, None))

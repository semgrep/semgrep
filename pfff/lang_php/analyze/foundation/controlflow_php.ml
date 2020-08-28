(*s: controlflow_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009, 2010, 2011 Facebook
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
(*e: Facebook copyright *)

open Common 

open Cst_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type node *)
type node = {
  (* For now we just have node_kind, but later if we want to do some data-flow
   * analysis or use temporal logic, we may want to add extra information
   * in each CFG nodes. We could also record such extra
   * information in an external table that maps Ograph_extended.nodei, 
   * that is nodeid, to some information.
   *)
  n: node_kind;
  (* for error report *)
  i: Cst_php.info option;
} 
(*e: type node *)
(*s: type node_kind *)
  and node_kind = 
  (*s: node_kind constructors *)
      (* special fake cfg nodes *)
      | Enter
      | Exit 
  (*x: node_kind constructors *)
      (* An alternative is to store such information in the edges, but
       * experience shows it's easier to encode it via regular nodes
       *)
      | TrueNode
      | FalseNode
  (*x: node_kind constructors *)
    (* not used for now
      | BlockStart of tok (* { *)
      | BlockEnd of tok (* } *)
    *)
  (*x: node_kind constructors *)
      | IfHeader of expr
      (* not used for now
      | Else
      | Elsif
      *) 
  (*x: node_kind constructors *)
      | WhileHeader of expr
      | DoHeader
      | DoWhileTail of expr
      | ForHeader
      | ForeachHeader (* TODO  of foreach_variable list *)

  (*x: node_kind constructors *)
      | SwitchHeader of expr
      | SwitchEnd
      | Case
      | Default
  (*x: node_kind constructors *)
      | Return of Cst_php.expr option

  (*x: node_kind constructors *)
      | Break
      | Continue

      | TryHeader
      | CatchStart
      | Catch
      | TryEnd
      | Throw of expr
  (*x: node_kind constructors *)
      | Join
      | Parameter of dname
      (* statements without multiple outgoing or ingoing edges, such
       * as echo, expression statements, etc.
       *)
      | SimpleStmt of simple_stmt
  (*e: node_kind constructors *)
  (*s: node_kind aux types *)
     and simple_stmt = 
         | ExprStmt of Cst_php.expr * use_status
         | TodoSimpleStmt
         (* TODO? expr includes Exit, Eval, Include, etc which
          * also have an influence on the control flow ...
          * We may want to uplift those constructors here and have
          * a better expr type
          *)
       and use_status = 
       | Normal
       | SpecialMaybeUnused
  (*e: node_kind aux types *)
(*e: type node_kind *)

(*s: type edge *)
(* For now there is just one kind of edge. Later we may have more, 
 * see the ShadowNode idea of Julia Lawall.
 *)
type edge = Direct 
(*e: type edge *)

(*s: type flow *)
type flow = (node, edge) Ograph_extended.ograph_mutable
(*e: type flow *)

(*****************************************************************************)
(* String of *)
(*****************************************************************************)

(*s: function short_string_of_node *)
let short_string_of_node_kind nkind = 
  match nkind with
  | Enter -> "<enter>"
  | Exit -> "<exit>"
  | SimpleStmt _ -> "<simplestmt>"
  | Parameter _ -> "<parameter>"
  | WhileHeader _ -> "while(...)"

  | TrueNode -> "TRUE path"
  | FalseNode -> "FALSE path"

  | IfHeader _ -> "if(...)"
  | Join -> "<join>"

  | Return _ -> "return ...;"

  | DoHeader -> "do"
  | DoWhileTail _ -> "while(...);"

  | Continue -> "continue;"
  | Break -> "break;"

  | ForHeader -> "for(...)"
  | ForeachHeader  -> "foreach(...)"

  | SwitchHeader _ -> "switch(...)"
  | SwitchEnd -> "<endswitch>"

  | Case -> "case: ..."
  | Default -> "default:"

  | TryHeader -> "try"
  | CatchStart -> "<catchstart>"
  | Catch -> "catch(...)"
  | TryEnd -> "<endtry>"

  | Throw _ -> "throw ...;"
(*e: function short_string_of_node *)
let short_string_of_node node =
  short_string_of_node_kind node.n 
(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let find_node f cfg =
  cfg#nodes#tolist |> Common.find_some (fun (nodei, node) ->
    if f node then Some nodei else None
  )

let find_exit cfg = find_node (fun node -> node.n = Exit) cfg
let find_enter cfg = find_node (fun node -> node.n = Enter) cfg


(*s: controlflow_php accessors *)
let (first_node : flow -> Ograph_extended.nodei) = fun _flow ->
  raise Todo

let (mk_node: node_kind -> node) = fun _nk ->
  raise Todo
(*e: controlflow_php accessors *)

(*s: function display_flow *)
(* using internally graphviz dot and ghostview on X11 *)
let (display_flow: flow -> unit) = fun flow ->
  flow |> Ograph_extended.print_ograph_mutable_generic  
    ~s_of_node:(fun (_nodei, node) -> 
      short_string_of_node_kind node.n, None, None
    )
(*e: function display_flow *)
(*e: controlflow_php.ml *)

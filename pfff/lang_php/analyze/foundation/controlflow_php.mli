(*s: controlflow_php.mli *)

(*s: type node *)
type node = {
  (* For now we just have node_kind, but later if we want to do some data-flow
   * analysis or use temporal logic, we may want to add extra information
   * in each CFG nodes. We could also record such extra information in an
   * external table that maps Ograph_extended.nodei to whatever information.
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
      | IfHeader of Cst_php.expr
      (* not used for now
      | Else
      | Elsif
      *) 
  (*x: node_kind constructors *)
      | WhileHeader of Cst_php.expr
      | DoHeader
      | DoWhileTail of Cst_php.expr
      | ForHeader (* the exprs are put in extra nodes around *)
      | ForeachHeader (* TODO  of Cst_php.foreach_variable list *)

  (*x: node_kind constructors *)
      | SwitchHeader of Cst_php.expr
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
      | Throw of Cst_php.expr
  (*x: node_kind constructors *)
      | Join
      (* for the dataflow, it's convenient to have parameters as
       * nodes.
       *)
      | Parameter of Cst_php.dname
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

val find_node: (node -> bool) -> flow -> Ograph_extended.nodei
val find_enter: flow -> Ograph_extended.nodei
val find_exit: flow -> Ograph_extended.nodei

(*s: controlflow helpers signatures *)
val first_node : flow -> Ograph_extended.nodei
val mk_node: node_kind -> node
(*e: controlflow helpers signatures *)

(*s: function display_flow signature *)
(* using internally graphviz 'dot' and ghostview 'gv' on X11 *)
val display_flow: flow -> unit
(*e: function display_flow signature *)

val short_string_of_node_kind: node_kind -> string
val short_string_of_node: node -> string

(*x: controlflow_php.mli *)
(*e: controlflow_php.mli *)

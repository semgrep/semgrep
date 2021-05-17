open AST_generic

type node = { n : node_kind; i : Parse_info.t option }

and node_kind =
  | Enter
  | Exit
  | TrueNode
  | FalseNode
  | Join
  | IfHeader of expr
  | WhileHeader of expr
  | DoHeader
  | DoWhileTail of expr
  | ForHeader
  (*TODO*)
  | ForeachHeader of pattern * expr
  | OtherStmtWithStmtHeader of other_stmt_with_stmt_operator * expr option
  | SwitchHeader of expr option
  | SwitchEnd
  | Case
  (* TODO of expr? *)
  | Default
  | Return of expr option
  | Break
  | Continue
  | TryHeader
  | CatchStart
  | Catch
  (* of pattern? *)
  | TryEnd
  | Throw of expr
  | SimpleNode of simple_node

and simple_node =
  | ExprStmt of expr
  | DefStmt of definition
  | DirectiveStmt of directive
  | Assert of tok * expr * expr option
  (* The 'any' below should not containt stmts, otherwise the CFG will
   * be incomplete. Use other_stmt_with_stmt_operator instead.
   *)
  | OtherStmt of other_stmt_operator * any list
  | Parameter of parameter

(* For now there is just one kind of edge. Later we may have more,
 * see the ShadowNode idea of Julia Lawall.
 *)
type edge = Direct

type flow = (node, edge) Ograph_extended.ograph_mutable

(* an int representing the index of a node in the graph *)
type nodei = Ograph_extended.nodei

val find_node : (node -> bool) -> flow -> nodei

val find_enter : flow -> nodei

val find_exit : flow -> nodei

(* using internally graphviz 'dot' and ghostview 'gv' on X11 *)
val display_flow : flow -> unit

val short_string_of_node_kind : node_kind -> string

val short_string_of_node : node -> string

val simple_node_of_stmt_opt : stmt -> simple_node option

val any_of_simple_node : simple_node -> any

module MV = Metavars_generic

val verbose : bool ref
val debug : bool ref

type tin = MV.metavars_binding
type tout = tin list
type ('a, 'b) matcher = 'a -> 'b -> tin -> tout

val empty_environment : unit -> 'a list

(* entry points, used in the sgrep_generic visitors *)
val m_expr : (Ast.expr, Ast.expr) matcher
val m_stmt : (Ast.stmt, Ast.stmt) matcher
val m_stmts_deep : (Ast.stmt list, Ast.stmt list) matcher

(* used only for unit testing *)
val m_any : (Ast.any, Ast.any) matcher

(* so can be changed when in equivalence matching mode *)
val go_deeper: bool ref
val equivalence_mode: bool ref
val build :
  ?verbose:bool -> Common.dirname -> Common.filename list -> Graph_code.t

(* helpers *)
val kind_of_expr_opt :
  Ast_js.var_kind Ast_js.wrap -> Ast_js.expr option -> Entity_code.entity_kind

(* used to be in ast_js.ml *)
type qualified_name = string

(* deprecated *)
val build_for_ai :
  Common.dirname ->
  Common.filename list ->
  (qualified_name, Ast_js.var) Hashtbl.t
  * (Common.filename (* readable *) * Ast_js.a_program) list

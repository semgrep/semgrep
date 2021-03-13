(*s: pfff/lang_GENERIC/parsing/Map_AST.mli *)
open AST_generic

(*s: type [[Map_AST.visitor_in]] *)
type visitor_in = {
  kexpr: (expr -> expr) * visitor_out -> expr -> expr;
  kstmt: (stmt -> stmt) * visitor_out -> stmt -> stmt;

  kinfo: (tok -> tok) * visitor_out -> tok -> tok;
  kidinfo: (id_info -> id_info) * visitor_out -> id_info -> id_info;
}
(*e: type [[Map_AST.visitor_in]] *)

(*s: type [[Map_AST.visitor_out]] *)
and visitor_out = {
  vitem: item -> item;
  vprogram: program -> program;
  vexpr: expr -> expr;
  vany: any -> any;
}
(*e: type [[Map_AST.visitor_out]] *)

(*s: signature [[Map_AST.default_visitor]] *)
val default_visitor: visitor_in
(*e: signature [[Map_AST.default_visitor]] *)

(*s: signature [[Map_AST.mk_visitor]] *)
val mk_visitor: visitor_in -> visitor_out
(*e: signature [[Map_AST.mk_visitor]] *)
(*e: pfff/lang_GENERIC/parsing/Map_AST.mli *)

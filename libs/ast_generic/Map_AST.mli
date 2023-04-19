open AST_generic

type visitor_in = {
  kexpr : (expr -> expr) * visitor_out -> expr -> expr;
  kstmt : (stmt -> stmt) * visitor_out -> stmt -> stmt;
  kinfo : (tok -> tok) * visitor_out -> tok -> tok;
  kidinfo : (id_info -> id_info) * visitor_out -> id_info -> id_info;
  klit : (literal -> literal) * visitor_out -> literal -> literal;
  kargs :
    (argument list -> argument list) * visitor_out ->
    argument list ->
    argument list;
  kname : (name -> name) * visitor_out -> name -> name;
}

and visitor_out = {
  vitem : item -> item;
  vprogram : program -> program;
  vexpr : expr -> expr;
  vany : any -> any;
}

val default_visitor : visitor_in
val mk_visitor : visitor_in -> visitor_out

val mk_fix_token_locations : (Tok.location -> Tok.location) -> visitor_out
(** Make a visitor that fixes token locations, e.g. when we want to interpret
  * a sub-AST as an independent program. This is used to implement
  * metavariable-pattern in [Match_rules.satisfies_metavar_pattern_condition]. *)

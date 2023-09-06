val parse_rule_xpattern : Parsing_common.env -> string * Tok.t -> Xpattern.t

val parse_formula_from_dict :
  Parsing_common.env -> Parsing_common.dict -> Rule.formula

val parse_formula_old_from_dict :
  Parsing_common.env -> Parsing_common.dict -> Rule.formula

val parse_formula : Parsing_common.env -> AST_generic.expr -> Rule.formula

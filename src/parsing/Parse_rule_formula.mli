val parse_rule_xpattern :
  Parse_rule_helpers.env -> string * Tok.t -> (Xpattern.t, Rule.error) Result.t

val parse_formula_from_dict :
  Parse_rule_helpers.env ->
  Parse_rule_helpers.dict ->
  (Rule.formula, Rule.error) Result.t

val parse_formula_old_from_dict :
  Parse_rule_helpers.env ->
  Parse_rule_helpers.dict ->
  (Rule.formula, Rule.error) Result.t

val parse_formula :
  Parse_rule_helpers.env ->
  AST_generic.expr ->
  (Rule.formula, Rule.error) Result.t

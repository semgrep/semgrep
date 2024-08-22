(* YOU SHOULD NOT USE THESE FUNCTIONS OUTSIDE OF `Parse_rule.ml`!!!
   The `Rule.Error.t` returned has an invariant where its `file` field
   is set on exit from `Parse_rule`
   These helpers are separated from `Parse_rule` for code cleanliness,
   but they are _not_ meant to be exposed to any other file.
*)

val parse_rule_xpattern :
  Parse_rule_helpers.env -> string * Tok.t -> (Xpattern.t, Rule_error.t) result

val parse_formula_from_dict :
  Parse_rule_helpers.env ->
  Parse_rule_helpers.dict ->
  (Rule.formula, Rule_error.t) result

val parse_formula_old_from_dict :
  Parse_rule_helpers.env ->
  Parse_rule_helpers.dict ->
  (Rule.formula, Rule_error.t) result

val parse_formula :
  Parse_rule_helpers.env ->
  AST_generic.expr ->
  (Rule.formula, Rule_error.t) result

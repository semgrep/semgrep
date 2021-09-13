(* may raise all the exns in Rule *)
val parse : Common.filename -> Rule.rules

(* used also by Convert_rule.ml *)
val parse_metavar_cond :
  string Rule.wrap (* enclosing location *) ->
  string (* condition to parse *) ->
  AST_generic.expr

(* internals used by other parsers (e.g., Parse_mini_rule.ml) *)

val parse_severity : id:Rule.rule_id -> string Rule.wrap -> Rule.severity

val parse_pattern :
  id:Rule.rule_id -> lang:Lang.t -> string Rule.loc -> Pattern.t

(*e: semgrep/parsing/Parse_rule.mli *)

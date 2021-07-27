(*s: semgrep/parsing/Parse_rule.mli *)

exception InvalidLanguage of Rule.rule_id * string * Parse_info.t

exception
  InvalidPattern of Rule.rule_id * string * Rule.xlang * string * Parse_info.t * string list

exception InvalidRegexp of Rule.rule_id * string * Parse_info.t

exception InvalidRule of Rule.rule_id * string * Parse_info.t

exception InvalidYaml of string * Parse_info.t

(* may raise all the exns above *)
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

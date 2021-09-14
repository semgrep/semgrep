(* may raise all the exns in Rule *)
val parse : Common.filename -> Rule.rules

(* internals used by other parsers (e.g., Parse_mini_rule.ml) *)

val parse_severity : id:Rule.rule_id -> string Rule.wrap -> Rule.severity

val parse_pattern :
  id:Rule.rule_id -> lang:Lang.t -> string Rule.loc -> Pattern.t

(*e: semgrep/parsing/Parse_rule.mli *)

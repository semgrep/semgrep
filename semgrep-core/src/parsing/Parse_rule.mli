(*s: semgrep/parsing/Parse_rule.mli *)

val parse : Common.filename -> Rule.rules

(* used also by Convert_rule.ml *)
val parse_metavar_cond : string -> AST_generic.expr

(*e: semgrep/parsing/Parse_rule.mli *)

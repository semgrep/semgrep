(*s: semgrep/parsing/Parse_rule.mli *)

val parse: Common.filename -> Rule.rules

(* used also by Convert_rule.ml *)
val parse_metavar_cond: string -> Rule.metavar_cond
(*e: semgrep/parsing/Parse_rule.mli *)

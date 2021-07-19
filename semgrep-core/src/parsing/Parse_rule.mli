(*s: semgrep/parsing/Parse_rule.mli *)

exception InvalidRuleException of string * string

exception InvalidLanguageException of string * string

exception InvalidPatternException of string * string * string * string

exception InvalidRegexpException of string * string

exception UnparsableYamlException of string

exception InvalidYamlException of string

val parse : Common.filename -> Rule.rules

(* used also by Convert_rule.ml *)
val parse_metavar_cond : string -> AST_generic.expr

(* internals used by other parsers (e.g., Parse_mini_rule.ml) *)

val parse_severity : id:string Rule.wrap -> string -> Rule.severity

val parse_pattern : id:string -> lang:Lang.t -> string -> Pattern.t

(*e: semgrep/parsing/Parse_rule.mli *)

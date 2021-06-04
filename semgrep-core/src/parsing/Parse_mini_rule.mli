(*s: semgrep/parsing/Parse_mini_rule.mli *)

(*s: signature [[Parse_rules.parse]] *)
val parse : Common.filename -> Mini_rule.rules

(*e: signature [[Parse_rules.parse]] *)

(*s: exception [[Parse_rules.InvalidRuleException]] *)
exception InvalidRuleException of string * string

(*e: exception [[Parse_rules.InvalidRuleException]] *)
(*s: exception [[Parse_rules.InvalidLanguageException]] *)
exception InvalidLanguageException of string * string

(*e: exception [[Parse_rules.InvalidLanguageException]] *)
(*s: exception [[Parse_rules.InvalidPatternException]] *)
exception InvalidPatternException of string * string * string * string

(*e: exception [[Parse_rules.InvalidPatternException]] *)

exception InvalidRegexpException of string * string

(*s: exception [[Parse_rules.UnparsableYamlException]] *)
exception UnparsableYamlException of string

(*e: exception [[Parse_rules.UnparsableYamlException]] *)
(*s: exception [[Parse_rules.InvalidYamlException]] *)
exception InvalidYamlException of string

(*e: exception [[Parse_rules.InvalidYamlException]] *)

(* internals used by other parsers (e.g., Parse_tainting_rules.ml) *)

(*s: signature [[Parse_rules.parse_languages]] *)
val parse_languages : id:string -> Yaml.value list -> Lang.t list * Lang.t

(*e: signature [[Parse_rules.parse_languages]] *)
(*s: signature [[Parse_rules.parse_severity]] *)
val parse_severity : id:string -> string -> Mini_rule.severity

(*e: signature [[Parse_rules.parse_severity]] *)
(*s: signature [[Parse_rules.parse_pattern]] *)
val parse_pattern : id:string -> lang:Lang.t -> string -> Pattern.t

(*e: signature [[Parse_rules.parse_pattern]] *)

(*e: semgrep/parsing/Parse_mini_rule.mli *)


val parse: Common.filename -> Rule.rules

exception InvalidRuleException of string * string
exception InvalidLanguageException of string * string
exception InvalidPatternException of string * string * string * string
exception UnparsableYamlException of string
exception InvalidYamlException of string

(* internals used by other parsers (e.g., parse_tainting_rules.ml) *)

val parse_languages: id:string -> Yaml.value list -> Lang.t list * Lang.t
val parse_severity: id:string -> string -> Rule.severity
val parse_pattern: id:string -> lang:Lang.t -> string -> Rule.pattern


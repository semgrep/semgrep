
val parse: Common.filename -> Rule.rules

exception InvalidRuleException of string * string
exception InvalidLanguageException of string * string
exception InvalidPatternException of string * string * string * string
exception UnparsableYamlException of string
exception InvalidYamlException of string


type t = AST_generic.any [@@deriving show, eq]

(* a few helpers used mostly in Analyze_pattern.ml *)
val is_special_identifier : ?lang:Lang.t -> string -> bool
val is_special_string_literal : string -> bool
val regexp_regexp_string : string
val is_regexp_string : string -> bool

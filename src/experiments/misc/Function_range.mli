type function_info = { name : string; range : Tok_range.t } [@@deriving show]
type ranges = function_info list [@@deriving show]

val ranges : AST_generic.program -> ranges

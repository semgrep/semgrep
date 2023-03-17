type function_info = { name : string; range : Loc.t } [@@deriving show]
type ranges = function_info list [@@deriving show]

val ranges : AST_generic.program -> ranges

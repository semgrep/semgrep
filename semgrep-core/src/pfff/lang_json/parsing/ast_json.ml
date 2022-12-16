
(* Should include only the literals (Bool, Num, String),
 * collections (Obj, Arr), and IdSpecial Null from Ast_js.expr.
 *
 * related: json-wheel Json_type.t, but does not have the Parse_info.t
 * in it so easier to just reuse the Javascript parser.
*)
type expr = Ast_js.expr
[@@deriving show]

type program = expr
[@@deriving show]

type any =
  | E of Ast_js.expr
  | PartialSingleField of string Ast_js.wrap * Ast_js.tok * Ast_js.expr
[@@deriving show { with_path = false }]

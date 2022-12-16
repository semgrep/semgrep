open Common

(* Transition package after moving AST_generic.ml to semgrep.
 *
 * This is a subset of AST_generic.ml to compile pfff without dependency
 * to AST_generic.ml which is now in semgrep.
*)

type operator =
  | Plus
  | Minus
  | Mult | Div | Mod
  | Pow
  | FloorDiv | MatMult
  | LSL | LSR | ASR
  | BitOr | BitXor | BitAnd | BitNot | BitClear
  | And | Or | Xor | Not
  | Eq
  | NotEq
  | PhysEq
  | NotPhysEq
  | Lt | LtE | Gt | GtE
  | Cmp
  | Concat
  | Append
  | RegexpMatch
  | NotMatch
  | Range
  | RangeInclusive
  | NotNullPostfix
  | Length
  | Elvis
  | Nullish
  | In
  | NotIn
  | Is
  | NotIs
[@@deriving show]

type incr_decr = Incr | Decr
[@@deriving show]
type prefix_postfix = Prefix | Postfix
[@@deriving show]

type class_kind =
  | Class
  | Interface
  | Trait
[@@deriving show]

type function_kind =
  | Function
  | Method
  | LambdaKind
  | Arrow
[@@deriving show]

(* !!You should not use the function below!! You should use instead
 * Metavars_generic.is_metavar_name. If you use the function below,
 * it probably means you have an ugly dependency to semgrep that you
 * should not have.
 * coupling: Metavariable.is_metavar_name
*)
let is_metavar_name s =
  s =~ "^\\(\\$[A-Z_][A-Z_0-9]*\\)$"
(* coupling: Metavariable.is_metavar_ellipsis *)
let is_metavar_ellipsis s =
  s =~ "^\\(\\$\\.\\.\\.[A-Z_][A-Z_0-9]*\\)$"


(* ugly! See AST_generic.ml for more information *)
let special_multivardef_pattern = "!MultiVarDef!"

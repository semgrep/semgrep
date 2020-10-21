
type value =
  | Bool of bool
  | Int of int
  | Float of float
  | String of string (* string without the enclosing '"' *)
  | List of value list
  | AST of string (* any AST, e.g., "x+1" *)

type env
type code = AST_generic.expr

val parse_json: Common.filename -> env * code

exception NotHandled of code

(* raise NotHandled if the code is outside the subset of expressions allowed *)
val eval: env -> code -> value

(* entry point for -eval *)
val eval_json_file: Common.filename -> unit

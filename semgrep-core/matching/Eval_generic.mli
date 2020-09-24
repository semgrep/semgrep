
type value =
  | Int of int
  | Bool of bool
  | String of string (* string without the enclosing '"' *)
  | AST of string (* any AST, e.g., "x+1" *)

type env
type code = AST_generic.expr

val parse_json: Common.filename -> env * code

exception NotHandled

(* raise NotHandled if the code is outside the subset of expressions allowed *)
val eval: env -> code -> value

(* entry point for -eval *)
val eval_json_file: Common.filename -> unit

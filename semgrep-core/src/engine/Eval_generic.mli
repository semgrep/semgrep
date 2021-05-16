type value =
  | Bool of bool
  | Int of int
  | Float of float
  | String of string (* string without the enclosing '"' *)
  | List of value list
  | AST of string

(* any AST, e.g., "x+1" *)

type env

type code = AST_generic.expr

val parse_json : Common.filename -> env * code

exception NotHandled of code

exception NotInEnv of Metavariable.mvar

(* raise NotHandled if the code is outside the subset of expressions allowed,
 * and NotInEnv if a metavariable is not binded in the environment.
 *)
val eval : env -> code -> value

(* entry point for -eval *)
val eval_json_file : Common.filename -> unit

(* for -test_eval *)
val test_eval : Common.filename -> unit

(* for MetavarCond *)
val bindings_to_env : Metavariable.bindings -> env

val bindings_to_env_with_just_strings : Metavariable.bindings -> env

val eval_bool : env -> code -> bool

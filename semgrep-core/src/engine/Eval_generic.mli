type value =
  | Bool of bool
  | Int of int
  | Float of float
  (* the string does not contain the enclosing '"' *)
  | String of string
  | List of value list
  (* any AST, e.g., "x+1" *)
  | AST of string

type env
type code = AST_generic.expr

exception NotHandled of code
exception NotInEnv of Metavariable.mvar

(* raise NotHandled if the code is outside the subset of expressions allowed
 * raise NotInEnv if a metavariable is not binded in the environment.
 *)
val eval : env -> code -> value

(* This function will swallow exns and always return a bool.
 * This is the function called by Match_rules.ml
 *)
val eval_bool : env -> code -> bool

(* for -test_eval *)
val test_eval : Common.filename -> unit

(* used in regression testing code *)
val parse_json : Common.filename -> env * code

(* for MetavarCond *)
val bindings_to_env_just_strings :
  Config_semgrep.t -> Metavariable.bindings -> env

val bindings_to_env_just_strings_const_prop : Metavariable.bindings -> env

(* For entropy analysis and other string analyzers.
   The mvar is only for making an error message. *)
val text_of_binding : Metavariable.mvar -> Metavariable.mvalue -> string option
val bindings_to_env : Config_semgrep.t -> Metavariable.bindings -> env

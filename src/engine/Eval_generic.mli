(* this is the (partially parsed/evaluated) content of a metavariable. *)
type value =
  | Bool of bool
  | Int of int64
  | Float of float
  (* the string does not contain the enclosing '"' *)
  | String of string
  | List of value list
  (* any AST, e.g., "x+1" *)
  | AST of string
[@@deriving show]

type env
type code = AST_generic.expr

exception NotHandled of code
exception NotInEnv of Metavariable.mvar

(* raise NotHandled if the code is outside the subset of expressions allowed
 * raise NotInEnv if a metavariable is not bound in the environment.
 *)
val eval : env -> code -> value

(* swallows exns and returns None if they happen
 *)
val eval_opt : env -> code -> value option

val eval_regexp_matches :
  ?base_offset:int ->
  file:Fpath.t ->
  regexp:string ->
  string ->
  ((Tok.location * Tok.location) * (string * Metavariable.mvalue) list) list

(* This function will swallow exns and always return a bool.
 * This is the function called by Match_rules.ml
 *)
val eval_bool :
  env -> code -> AST_generic.facts -> Metavariable.bindings -> bool

(* for -test_eval *)
val test_eval : string (* filename *) -> unit

(* used in regression testing code *)
val parse_json : string (* filename *) -> env * code

(* for metavariable-comparison and also for metavariable-regex with constant-propagation: true *)
val bindings_to_env_just_strings :
  Rule_options.t -> file:Fpath.t -> Metavariable.bindings -> env

(* For entropy analysis and other string analyzers.
   The mvar is only for making an error message. *)
val text_of_binding : Metavariable.mvar -> Metavariable.mvalue -> string option

val bindings_to_env :
  Rule_options.t -> file:Fpath.t -> Metavariable.bindings -> env

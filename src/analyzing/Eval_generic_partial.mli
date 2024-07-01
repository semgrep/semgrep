type var = string * AST_generic.sid

type env = {
  lang : Lang.t option;
  constants : (var, AST_generic.svalue) Hashtbl.t;
  attributes : (var, AST_generic.attribute list) Hashtbl.t;
}

(* this is the (partially parsed/evaluated) content of a metavariable. This type
 * is used by Eval_generic.ml and Dataflow_when.ml.
 *)
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

val default_env : Language.t option -> env

(* Partially evaluate a Generic expression *)
val eval : env -> AST_generic.expr -> AST_generic.svalue option

(* helpers reused in Constant_propagation.ml *)
val is_lang : env -> Language.t -> bool
val is_js : env -> bool

val find_id :
  env -> AST_generic.ident -> AST_generic.id_info -> AST_generic.svalue option

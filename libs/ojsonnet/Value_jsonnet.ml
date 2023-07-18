(*****************************************************************************)
(* Env *)
(*****************************************************************************)
type env = {
  (* The spec uses a lambda-calculus inspired substitution model, but
   * it is probably simpler and more efficient to use a classic
   * environment where the locals are defined. Jsonnet uses lazy
   * evaluation so we model this by allowing unevaluated expressions in environment below.
   *)
  locals : (local_id, lazy_value) Map_.t;
  (* for call tracing *)
  depth : int;
}

and local_id = LSelf | LSuper | LId of string

(* This used to be wrapped in an explicit "lazy" rather than keeping around an environment
   however, this does not work with the object merge + operator, since we need to be able
   to access the environment in which fields of the object are evaluated in. It is also neccesary
   to keep around and environment even for values, since there could be nested objects/arrays which
   also have lazy semantics themselves, and thus again need to be able to modify a specifc environment *)
and val_or_unevaluated_ = Val of value_ | Unevaluated of Core_jsonnet.expr
and lazy_value = { value : val_or_unevaluated_; env : env }

(*****************************************************************************)
(* Values *)
(*****************************************************************************)
and value_ =
  | Primitive of primitive
  | ObjectVal of object_ AST_jsonnet.bracket
  | Function of Core_jsonnet.function_definition
  | Array of lazy_value array AST_jsonnet.bracket

(* mostly like AST_jsonnet.literal but with evaluated Double instead of
 * Number and a simplified string!
 * Is float good enough? That's what we use in JSON.t so should be good.
 * TODO? string good enough for unicode? codepoints?
 *)
and primitive =
  | Null of AST_jsonnet.tok
  | Bool of bool AST_jsonnet.wrap
  | Double of float AST_jsonnet.wrap
  | Str of string AST_jsonnet.wrap

and object_ = asserts list * value_field list

(* opti? make it a hashtbl of string -> field for faster lookup? *)
and value_field = {
  (* like Str, strictly evaluated! *)
  vfld_name : string AST_jsonnet.wrap;
  vfld_hidden : AST_jsonnet.hidden AST_jsonnet.wrap;
  vfld_value : lazy_value;
}

and asserts = Core_jsonnet.obj_assert * env [@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let empty_obj : value_ =
  let fk = Tok.unsafe_fake_tok "" in
  ObjectVal (fk, ([], []), fk)

let empty_env = { locals = Map_.empty; depth = 0 }

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

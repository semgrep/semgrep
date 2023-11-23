type var = Dataflow_var_env.var
(** A string of the form "<source name>:<sid>". *)

type mapping = Taint_lval_env.t Dataflow_core.mapping
(** Mapping from variables to taint sources (if the variable is tainted).
  * If a variable is not in the map, then it's not tainted. *)

type java_props_cache
(** When we encounter getters/setters without a definition, we need to resolve them
  * to their corresponding property, we cache the results here. *)

val mk_empty_java_props_cache : unit -> java_props_cache

val hook_function_taint_signature :
  (Taint_instance.func ->
  AST_generic.expr ->
  (AST_generic.parameters (* params of function *) * Taint.signature) option)
  option
  ref
(** DEEP *)

val hook_find_attribute_in_class :
  (AST_generic.name -> string -> AST_generic.name option) option ref
(** DEEP *)

val fixpoint : Taint_instance.func -> java_props_cache -> IL.cfg -> mapping
(** Main entry point, [fixpoint instance cfg] returns a mapping (effectively a set)
  * containing all the tainted variables in [cfg]. Besides, if it infers any taint
  * 'findings', it will invoke [instance.handle_findings] which can perform any
  * side-effectful action.
  *
  * @param in_env are the assumptions made on the function's parameters.
  * @param name is the name of the function being analyzed, if it has a name.
  * *)

(* TODO: Move to module 'Taint' maybe. *)
val drop_taints_if_bool_or_number :
  Taint_instance.options -> Taint.Taint_set.t -> 'a Type.t -> Taint.Taint_set.t

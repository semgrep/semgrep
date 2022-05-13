type var = Dataflow_core.var
(** A string of the form "<source name>:<sid>". *)

type config = {
  filepath : Common.filename;  (** File under analysis, for Deep Semgrep. *)
  rule_id : string;  (** Taint rule id, for Deep Semgrep. *)
  is_source : AST_generic.any -> Pattern_match.t list;
      (** Test whether 'any' is a taint source, this corresponds to
      * 'pattern-sources:' in taint-mode. *)
  is_sink : AST_generic.any -> Pattern_match.t list;
      (** Test whether 'any' is a sink, this corresponds to 'pattern-sinks:'
      * in taint-mode. *)
  is_sanitizer : AST_generic.any -> Pattern_match.t list;
      (** Test whether 'any' is a sanitizer, this corresponds to
      * 'pattern-sanitizers:' in taint-mode. *)
  unify_mvars : bool;  (** Unify metavariables in sources and sinks? *)
  handle_findings :
    var option (** function name ('None' if anonymous) *) ->
    Taint.finding list ->
    Taint.taints Dataflow_core.env ->
    unit;
      (** Callback to report findings. *)
}
(** Taint rule instantiated for a given file.
  *
  * For a source to taint a sink, the bindings of both source and sink must be
  * unifiable. See 'unify_meta_envs'. *)

type mapping = Taint.taints Dataflow_core.mapping
(** Mapping from variables to taint sources (if the variable is tainted).
  * If a variable is not in the map, then it's not tainted. *)

type fun_env = (var, Pattern_match.Set.t) Hashtbl.t
(** Set of functions known to act as taint sources (their output is
  * tainted). This is used for a HACK to do some poor-man's intrafile
  * interprocedural taint tracking. TO BE DEPRECATED. *)

val str_of_name : IL.name -> var

val hook_function_taint_signature :
  (config -> AST_generic.expr -> Taint.finding list option) option ref
(** Deep Semgrep *)

val fixpoint :
  ?in_env:Taint.taints Dataflow_core.VarMap.t ->
  ?name:var ->
  ?fun_env:fun_env (** Poor-man's interprocedural HACK (TO BE DEPRECATED) *) ->
  config ->
  IL.cfg ->
  mapping
(** Main entry point, [fixpoint config cfg] returns a mapping (effectively a set)
  * containing all the tainted variables in [cfg]. Besides, if it infers any taint
  * 'findings', it will invoke [config.handle_findings] which can perform any
  * side-effectful action.
  *
  * @param in_env are the assumptions made on the function's parameters.
  * @param name is the name of the function being analyzed, if it has a name.
  * @param fun_env is a set of tainted functions in the same file, it is a HACK
  *    to provide some poor-man's interprocedural capabilities. If the function
  *    being analyzed has a name, and taint reaches a `return` statement, the
  *    function name will be added to this environment by side-effect.
  * *)

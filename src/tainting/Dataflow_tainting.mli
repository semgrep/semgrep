type var = Dataflow_var_env.var
(** A string of the form "<source name>:<sid>". *)

type overlap = float
(** Overlap ratio, only applies to AST nodes that fall in the range of a
 * source/sanitizer/sink annotation. It is a number in [0.0, 1.0], where
 * 1.0 means that the AST node matches the annotation perfectly. For
 * practical purposes we can interpret >0.99 as being the same as 1.0. *)

type 'spec tmatch = {
  spec : 'spec;
      (** The specification on which the match is based, e.g. a taint source.
      * This spec should have a pattern formula. *)
  spec_pm : Pattern_match.t;
      (** A pattern match obtained directly from the spec's pattern formula. *)
  range : Range.t;
      (** The range of this particular match, which will be a subrange of 'spec_pm',
      * see note on 'Taint-tracking via ranges' in 'Match_tainting_mode.ml'. *)
  overlap : overlap;
      (** The overlap of this match ('range') with the spec match ('spec_pm'). *)
}

type a_propagator = {
  kind : [ `From | `To ];
  prop : Rule.taint_propagator;
  var : var; (* REMOVE USE prop.id *)
}

type config = {
  filepath : Common.filename;  (** File under analysis, for Deep Semgrep. *)
  rule_id : Rule_ID.t;  (** Taint rule id, for Deep Semgrep. *)
  is_source : AST_generic.any -> Rule.taint_source tmatch list;
      (** Test whether 'any' is a taint source, this corresponds to
      * 'pattern-sources:' in taint-mode. *)
  is_propagator : AST_generic.any -> a_propagator tmatch list;
      (** Test whether 'any' matches a taint propagator, this corresponds to
       * 'pattern-propagators:' in taint-mode.
       *
       * Propagators allow to specify how taint propagates through side effects.
       *
       * Note that we tried to solve this with a hack in returntocorp/semgrep#5150
       * but it caused a bunch of FPs in semgrep-rules. The hack was essentially
       * to assume that in `x.f(y)` taint always propagated from `y` to `x`.
       *
       * The typical FP was a call that incorrectly tainted an object or module,
       * that also happened to be part of a sink specification. For example, in
       * rule ruby.rails.security.audit.avoid-tainted-shell-call the `Shell` class
       * does not really get tainted even if we call `Shell.cat` on tainted data:
       *
       *     # ruleid: avoid-tainted-shell-call
       *     Shell.cat(params[:filename])
       *
       * But with the hack, `Shell` becomes tainted. Later on, when we call
       * `Shell.cat` on safe data, it triggered an FP. Why? Because the entire
       * `Shell.cat(...)` was marked as a sink, and `Shell` was considered
       * tainted!
       *
       *     # ok: avoid-tainted-shell-call
       *     Shell.cat("/var/log/www/access.log")
       *
       * Most of these FPs could be prevented by fine tuning pattern-sinks. But
       * anyhow it's clearly incorrect to taint `Shell`, so a better solution was
       * needed (hence `pattern-propagators`).
       *)
  is_sink : AST_generic.any -> Rule.taint_sink tmatch list;
      (** Test whether 'any' is a sink, this corresponds to 'pattern-sinks:'
      * in taint-mode. *)
  is_sanitizer : AST_generic.any -> Rule.taint_sanitizer tmatch list;
      (** Test whether 'any' is a sanitizer, this corresponds to
      * 'pattern-sanitizers:' in taint-mode. *)
  unify_mvars : bool;  (** Unify metavariables in sources and sinks? *)
  handle_findings :
    var option (** function name ('None' if anonymous) *) ->
    Taint.finding list ->
    Taint_lval_env.t ->
    unit;
      (** Callback to report findings. *)
}
(** Taint rule instantiated for a given file.
  *
  * For a source to taint a sink, the bindings of both source and sink must be
  * unifiable. See 'unify_meta_envs'. *)

type mapping = Taint_lval_env.t Dataflow_core.mapping
(** Mapping from variables to taint sources (if the variable is tainted).
  * If a variable is not in the map, then it's not tainted. *)

type fun_env = (var, Taint.Taint_set.t) Hashtbl.t
(** Set of functions known to act as taint sources (their output is
  * tainted). This is used for a HACK to do some poor-man's intrafile
  * interprocedural taint tracking. TO BE DEPRECATED. *)

type java_props_cache
(** When we encounter getters/setters without a definition, we need to resolve them
  * to their corresponding property, we cache the results here. *)

val mk_empty_java_props_cache : unit -> java_props_cache
val str_of_name : IL.name -> var

val hook_function_taint_signature :
  (config ->
  AST_generic.expr ->
  (AST_generic.parameters (* params of function *) * Taint.signature) option)
  option
  ref
(** DEEP *)

val hook_find_attribute_in_class :
  (AST_generic.name -> string -> AST_generic.name option) option ref
(** DEEP *)

val fixpoint :
  ?in_env:Taint_lval_env.t ->
  ?name:var ->
  Lang.t ->
  Rule_options.t ->
  config ->
  java_props_cache ->
  IL.cfg ->
  mapping
(** Main entry point, [fixpoint config cfg] returns a mapping (effectively a set)
  * containing all the tainted variables in [cfg]. Besides, if it infers any taint
  * 'findings', it will invoke [config.handle_findings] which can perform any
  * side-effectful action.
  *
  * @param in_env are the assumptions made on the function's parameters.
  * @param name is the name of the function being analyzed, if it has a name.
  * *)

(* TODO: Move to module 'Taint' maybe. *)
val drop_taints_if_bool_or_number :
  Rule_options_t.t -> Taint.Taint_set.t -> 'a Type.t -> Taint.Taint_set.t

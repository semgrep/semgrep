module Lval_env = Taint_lval_env
module G = AST_generic
module R = Rule
module T = Taint
module Taints = T.Taint_set
module Var_env = Dataflow_var_env

type var = Dataflow_var_env.var
(** A string of the form "<source name>:<sid>". *)

type a_propagator = {
  kind : [ `From | `To ];
  prop : Rule.taint_propagator;
  var : var; (* REMOVE USE prop.id *)
}

type options = {
  constant_propagation : bool;
  only_propagate_through_assignments : bool;
  assume_safe_comparisons : bool;
  assume_safe_functions : bool;
  assume_safe_indexes : bool;
  assume_safe_numbers : bool;
  assume_safe_booleans : bool;
  unify_mvars : bool;  (** Unify metavariables in sources and sinks? *)
}

type t = {
  lang : Language.t;
  filepath : string;  (** File under analysis, for Deep Semgrep. *)
  rule_id : Rule_ID.t;  (** Taint rule id, for Deep Semgrep. *)
  track_control : bool;
      (** Whether the rule requires tracking "control taint". *)
  is_source : AST_generic.any -> Rule.taint_source Taint_smatch.t list;
      (** Test whether 'any' is a taint source, this corresponds to
      * 'pattern-sources:' in taint-mode. *)
  is_propagator : AST_generic.any -> a_propagator Taint_smatch.t list;
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
  is_sink : AST_generic.any -> Rule.taint_sink Taint_smatch.t list;
      (** Test whether 'any' is a sink, this corresponds to 'pattern-sinks:'
      * in taint-mode. *)
  is_sanitizer : AST_generic.any -> Rule.taint_sanitizer Taint_smatch.t list;
      (** Test whether 'any' is a sanitizer, this corresponds to
      * 'pattern-sanitizers:' in taint-mode. *)
  (* NOTE [is_sanitizer]:
   * A sanitizer is more "extreme" than you may expect. When a piece of code is
   * "sanitized" Semgrep will just not check it. For example, something like
   * `sanitize(sink(tainted))` will not yield any finding.
   * *)
  options : options;
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

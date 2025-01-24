(* Iago Abal
 *
 * Copyright (C) 2025 Semgrep Inc., All rights reserved
 *)

type var = Dataflow_var_env.var
(** A string of the form "<source name>:<sid>". *)

type a_propagator = {
  kind : [ `From | `To ];
  prop : Rule.taint_propagator;
  var : var; (* TODO: Use prop.id instead ? *)
}

type t = {
  is_source : AST_generic.any -> Rule.taint_source Taint_spec_match.t list;
      (** Test whether 'any' is a taint source, this corresponds to
      * 'pattern-sources:' in taint-mode. *)
  is_propagator : AST_generic.any -> a_propagator Taint_spec_match.t list;
      (** Test whether 'any' matches a taint propagator, this corresponds to
       * 'pattern-propagators:' in taint-mode.
       *
       * Propagators allow to specify how taint propagates through side effects.
       *
       * Note that we tried to solve this with a hack in semgrep/semgrep#5150
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
  is_sanitizer :
    AST_generic.any -> Rule.taint_sanitizer Taint_spec_match.t list;
      (** Test whether 'any' is a sanitizer, this corresponds to
      * 'pattern-sanitizers:' in taint-mode. *)
  is_sink : AST_generic.any -> Rule.taint_sink Taint_spec_match.t list;
      (** Test whether 'any' is a sink, this corresponds to 'pattern-sinks:'
      * in taint-mode. *)
}

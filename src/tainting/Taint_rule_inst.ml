(* Yoann Padioleau, Iago Abal
 *
 * Copyright (C) 2019-2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(* A taint rule inst(ance).
 *
 * This is the "instantiation" of a taint rule for an specific file. A taint rule
 * consists of several pattern formulas that specify what is a source/sink/etc.
 * We instantiate a taint rule by matching those formulas on the file, obtaining
 * a set of "predicates" (see type 'spec_predicates' below) that allow us to label
 * an arbitrary sub-AST in the file as being a source/sink/etc or not. Once we have
 * these predicates, we are then able to run the actual taint analysis.
 *
 * Module 'Match_taint_spec' generates a taint rule instance given a taint rule
 * and a file, and 'Dataflow_tainting' runs the actual taint analysis given a
 * taint rule intance and a function from that file.
 *)

type var = Dataflow_var_env.var
(** A string of the form "<source name>:<sid>". *)

type a_propagator = {
  kind : [ `From | `To ];
  prop : Rule.taint_propagator;
  var : var; (* TODO: Use prop.id instead ? *)
}

type spec_predicates = {
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

type effects_handler =
  IL.name option (** name of the function definition ('None' if anonymous) *) ->
  Shape_and_sig.Effect.t list ->
  Shape_and_sig.Effect.t list

type java_props_cache = (string * AST_generic.SId.t, IL.name) Hashtbl.t

type t = {
  lang : Lang.t;
  file : Fpath.t;  (** File under analysis, for Deep Semgrep. *)
  rule_id : Rule_ID.t;  (** Taint rule id, for Deep Semgrep. *)
  options : Rule_options.t;
  track_control : bool;
      (** Whether the rule requires tracking "control taint". If it does not,
       * then we avoid adding control taint-variables to environment. *)
  preds : spec_predicates;
  handle_effects : effects_handler;  (** Callback to report effects. *)
  java_props_cache : java_props_cache;
      (** Pro should be autogenerating definitions for these getters/setters,
    * but that seems to hurt performance and it's still unclear why, so instead
    * we give taint access to Pro typing info through a hook
    * ('Dataflow_tainting.hook_find_attribute_in_class') and look for the
    * property corresponding to the getter/setter.
    *
    * On very large files, allocating a new name every time could have a perf
    * impact, so we cache them. *)
}
(** Taint rule instantiated for a given file.
  *
  * For a source to taint a sink, the bindings of both source and sink must be
  * unifiable. See 'Dataflow_tainting.unify_meta_envs'. *)

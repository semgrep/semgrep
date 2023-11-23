(* Iago Abal, Yoann Padioleau
 *
 * Copyright (C) 2019-2023 Semgrep Inc.
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

module Lval_env = Taint_lval_env
module G = AST_generic
module R = Rule
module T = Taint
module Taints = T.Taint_set
module TM = Taint_smatch
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

let propagate_through_functions options =
  (not options.assume_safe_functions)
  && not options.only_propagate_through_assignments

let propagate_through_indexes options =
  (not options.assume_safe_indexes)
  && not options.only_propagate_through_assignments

type handle_findings =
  G.entity option (** function name ('None' if anonymous) *) ->
  Taint.finding list ->
  Lval_env.t ->
  unit

type target = {
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
  handle_findings : handle_findings;  (** Callback to report findings. *)
}
(** Taint rule instantiated for a given file.
  *
  * For a source to taint a sink, the bindings of both source and sink must be
  * unifiable. See 'unify_meta_envs'. *)

let orig_is_source target orig = target.is_source (IL.any_of_orig orig)
let orig_is_sanitizer target orig = target.is_sanitizer (IL.any_of_orig orig)
let orig_is_sink target orig = target.is_sink (IL.any_of_orig orig)

type func = {
  target : target;
  entity : G.entity option;
  start_env : Lval_env.t;
  top_matches : Taint_smatch.Top_matches.t;
}

let is_best_match func = TM.is_best_match func.top_matches

let any_is_best_sanitizer func any =
  func.target.is_sanitizer any
  |> List.filter (fun (m : R.taint_sanitizer TM.t) ->
         (not m.spec.sanitizer_exact) || is_best_match func m)

let any_is_best_source func any =
  func.target.is_source any
  |> List.filter (fun (m : R.taint_source TM.t) ->
         (not m.spec.source_exact) || is_best_match func m)

let any_is_best_sink func any =
  func.target.is_sink any |> List.filter (is_best_match func)

let orig_is_best_source func orig : R.taint_source TM.t list =
  any_is_best_source func (IL.any_of_orig orig)

let orig_is_best_sanitizer func orig =
  any_is_best_sanitizer func (IL.any_of_orig orig)

let orig_is_best_sink func orig = any_is_best_sink func (IL.any_of_orig orig)
let lval_is_source func lval = any_is_best_source func (IL.any_of_lval lval)

let lval_is_best_sanitizer func lval =
  any_is_best_sanitizer func (IL.any_of_lval lval)

let lval_is_sink func lval = func.target.is_sink (IL.any_of_lval lval)
let lval_is_best_sink func lval = any_is_best_sink func (IL.any_of_lval lval)

let mk_func target ?entity ?(in_env = Lval_env.empty) flow =
  {
    target;
    entity;
    start_env = in_env;
    top_matches =
      (* Here we compute the "canonical" or "top" sink matches, for each sink we check
       * whether there is a "best match" among the top nodes in the CFG.
       * See NOTE "Top matches" *)
      TM.top_level_matches_in_nodes
        ~matches_of_orig:(fun orig ->
          let sources =
            orig_is_source target orig |> List.to_seq
            |> Seq.filter (fun (m : R.taint_source TM.t) -> m.spec.source_exact)
            |> Seq.map (fun m -> TM.Any m)
          in
          let sanitizers =
            orig_is_sanitizer target orig
            |> List.to_seq
            |> Seq.filter (fun (m : R.taint_sanitizer TM.t) ->
                   m.spec.sanitizer_exact)
            |> Seq.map (fun m -> TM.Any m)
          in
          let sinks =
            orig_is_sink target orig |> List.to_seq
            |> Seq.map (fun m -> TM.Any m)
          in
          sources |> Seq.append sanitizers |> Seq.append sinks)
        flow;
  }

(* Yoann Padioleau, Iago Abal
 *
 * Copyright (C) 2019-2025 Semgrep Inc.
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

open Common
module E = Core_error

type effects_handler =
  IL.name option (** name of the function definition ('None' if anonymous) *) ->
  Shape_and_sig.Effect.poly list ->
  Shape_and_sig.Effect.poly list

type java_props_cache = (string * AST_generic.SId.t, IL.name) Hashtbl.t

type file_timeout_var_stats = {
  first_rule : Rule_ID.t;
  mutable num_rules : int;
}

type file_timeout_stats = (IL.name option, file_timeout_var_stats) Hashtbl.t

(* components of a taint rule that are immutable and sharable. *)
type file = {
  lang : Lang.t;
  path : Fpath.t;  (** File under analysis, for Deep Semgrep. *)
}

(* components of a taint rule that are mutable (and thus can't be passed
   cross-thread during parallel taint signature inference. *)
type mutable_state = {
  handle_effects : effects_handler;
      (** Use 'handle_effects' to e.g. apply hash-consing (see 'Deep_tainting'), or
        * to do some side-effect if needed.

        * old: In the past one had to use 'handle_effects' to record taint
        * effects by side-effect (no pun intended), however this is not needed
        * now because 'Dataflow_tainting.fixpoint' already returns the set of
        * taint effects.

        * SAFETY: Since [handle_effects] closes over a mutable effects cache,
        * it has to be considered "mutable" itself. *)
  java_props_cache : java_props_cache;
      (** Pro should be autogenerating definitions for these getters/setters,
        * but that seems to hurt performance and it's still unclear why, so
        * instead we give taint access to Pro typing info through a hook
        * ('hook_find_attribute_in_class') and look for the property
        * corresponding to the getter/setter.

        * On very large files, allocating a new name every time could have a
        * perf impact, so we cache them. *)
  timeouts : file_timeout_stats;
}

type t = {
  file : file;
  muts : mutable_state;
  rule : Rule_ID.t;
  options : Rule_options.t;
  track_control : bool;
      (** Whether the rule requires tracking "control taint". If it does not,
       * then we avoid adding control taint-variables to environment. *)
  preds : Taint_spec_preds.t;
  stats : Taint_coverage_stats.file_rule_stats;
      (** Per-file per-rule coverage stats, what sources and what sinks have matched?

        TODO: If we just had the raw_spec_matches here we could easily compute
            these stats from the matches later.
        *)
}
(** Taint rule instantiated for a given file.
  *
  * For a source to taint a sink, the bindings of both source and sink must be
  * unifiable. See 'Dataflow_tainting.unify_meta_envs'. *)

let default_effect_handler _fun_name new_effects = new_effects
let mk_file ~lang ~path = { lang; path }

let fresh_muts ~handle_effects =
  {
    handle_effects = handle_effects ||| default_effect_handler;
    java_props_cache = Hashtbl.create 30;
    timeouts = Hashtbl.create 2;
  }

let record_timeout t opt_name =
  match Hashtbl.find_opt t.muts.timeouts opt_name with
  | None ->
      Hashtbl.add t.muts.timeouts opt_name
        { first_rule = t.rule; num_rules = 1 };
      ()
  | Some stats ->
      stats.num_rules <- stats.num_rules + 1;
      ()

let check_timeouts_and_warn ~interfile file caches : E.ErrorSet.t =
  Hashtbl.fold
    (fun opt_name stats errors_acc ->
      (* TODO: Hash 'opt_name' and show it *)
      let loc = IL_helpers.loc_of_name file.path opt_name in
      let msg =
        spf
          "Fixpoint timeout while performing%s taint analysis at %s [rules: \
           %d, first: %s]"
          (if interfile then " inter-file" else "")
          (Pos.string_of_pos loc.pos)
          stats.num_rules
          (Rule_ID.to_string stats.first_rule)
      in
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m -> m "%s" msg);
      let err = E.mk_error ~msg ~loc Semgrep_output_v1_t.FixpointTimeout in
      errors_acc |> E.ErrorSet.add err)
    caches.timeouts E.ErrorSet.empty

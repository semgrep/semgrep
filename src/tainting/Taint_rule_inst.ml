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
  preds : Taint_spec_preds.t;
  pro_hooks : Taint_pro_hooks.t option;
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

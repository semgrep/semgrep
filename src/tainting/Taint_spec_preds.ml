(* Iago Abal
 *
 * Copyright (C) 2025 Semgrep Inc., All rights reserved
 *)
open Common

type var = Dataflow_var_env.var
(** A string of the form "<source name>:<sid>". *)

type prop_orig =
  | Prop of Rule.taint_propagator
  | SinkMultiReq of Rule.taint_sink
      (** A propagation point may originate either from:
          - a taint propagator, the `from` and the `to` in each `pattern-propagators`
            introduce a propagation point each; or,
          - a taint sink with a "multi-requires", a `requires:` item such as
            `- $X: A and B` introduces a propagation point to capture `$X`'s taint,
            see NOTE "Multi-requires as propagators". *)

type propagation_point = {
  kind : [ `From | `To ];
  var : var;
  (* THINK: The label-related stuff in here is only applicable to the `From case.  *)
  prop_by_side_effect : bool;
  prop_requires : Rule.precondition option;
  prop_label : string option;
  prop_replace_labels : string list option;
  prop_orig : prop_orig;
}
(** Taint that arrives at a '`From' point will be propagated to its corresponding
  '`To' point. The taint "flows" through the "propagation variable" 'var', when we
  encounter the '`From' we write to 'var', and when we encounter the '`To' we read
  from it. *)

let get_propagator_precondition { prop_requires; _ } =
  prop_requires ||| Rule.default_propagator_requires

type sink_requires =
  | UniReq of Rule.precondition
  | MultiReq of (var * Rule.precondition) list
      (** Like 'Rule.sink_requires' but we also have the propagation 'var'. *)

type sink = { requires : sink_requires; spec : Rule.taint_sink }

type t = {
  is_source : AST_generic.any -> Rule.taint_source Taint_spec_match.t list;
      (** Test whether 'any' is a taint source, this corresponds to
        'pattern-sources:' in taint-mode. *)
  is_propagator : AST_generic.any -> propagation_point Taint_spec_match.t list;
      (** Test whether 'any' matches a taint propagator, this corresponds to
        'pattern-propagators:' in taint-mode.

        Propagators allow to specify how taint propagates through side effects.

        Note that we tried to solve this with a hack in semgrep/semgrep#5150
        but it caused a bunch of FPs in semgrep-rules. The hack was essentially
        to assume that in `x.f(y)` taint always propagated from `y` to `x`.

        The typical FP was a call that incorrectly tainted an object or module,
        that also happened to be part of a sink specification. For example, in
        rule ruby.rails.security.audit.avoid-tainted-shell-call the `Shell` class
        does not really get tainted even if we call `Shell.cat` on tainted data:

            # ruleid: avoid-tainted-shell-call
            Shell.cat(params[:filename])

        But with the hack, `Shell` becomes tainted. Later on, when we call
        `Shell.cat` on safe data, it triggered an FP. Why? Because the entire
        `Shell.cat(...)` was marked as a sink, and `Shell` was considered
        tainted!

            # ok: avoid-tainted-shell-call
            Shell.cat("/var/log/www/access.log")

        Most of these FPs could be prevented by fine tuning pattern-sinks. But
        anyhow it's clearly incorrect to taint `Shell`, so a better solution was
        needed (hence `pattern-propagators`).
       *)
  is_sanitizer :
    AST_generic.any -> Rule.taint_sanitizer Taint_spec_match.t list;
      (** Test whether 'any' is a sanitizer, this corresponds to
        'pattern-sanitizers:' in taint-mode. *)
  is_sink : AST_generic.any -> sink Taint_spec_match.t list;
      (** Test whether 'any' is a sink, this corresponds to 'pattern-sinks:'
        in taint-mode. *)
}

let show_sink_requires req =
  match req with
  | UniReq precond -> Rule.show_precondition precond
  | MultiReq mvars_w_preconds ->
      mvars_w_preconds
      |> List_.map (fun (prop_var, precond) ->
             spf "%s|%s" prop_var (Rule.show_precondition precond))
      |> String.concat "; "

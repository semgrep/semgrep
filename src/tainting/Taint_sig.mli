(*****************************************************************************)
(* Taint results & signatures *)
(*****************************************************************************)

type sink = { pm : Pattern_match.t; rule_sink : Rule.taint_sink }
(** A sink match with its corresponding sink specification (one of the `pattern-sinks`). *)

type taint_to_sink_item = {
  taint : Taint.taint;
  sink_trace : unit Taint.call_trace;
      (** This trace is from the current calling context of the taint finding,
        to the sink.
        It's a `unit` call_trace because we don't actually need the item at the
        end, and we need to be able to dispatch on the particular variant of taint
        (source or arg).
        *)
}

type taints_to_sink = {
  taints_with_precondition : taint_to_sink_item list * Rule.precondition;
      (** Taints reaching the sink and the precondition for the sink to apply. *)
  sink : sink;
  merged_env : Metavariable.bindings;
      (** The metavariable environment that results of merging the environment from
   * matching the source and the one from matching the sink. *)
}

(** Function-level result.
  *
  * 'ToSink' results where a taint source reaches a sink are candidates for
  * actual Semgrep findings, although some may be dropped by deduplication.
  *
  * Results are computed for each function/method definition, and formulated
  * using 'lval' taints to act as placeholders of the taint that may be passed
  * by an arbitrary caller via the function arguments. Thus the results are
  * polymorphic/context-sensitive, as the 'lval' taints can be instantiated
  * accordingly at each call site.
  *)
type result =
  | ToSink of taints_to_sink  (** Taints reach a sink. *)
  | ToReturn of Taint.taint list * Taint_shape.shape * AST_generic.tok
      (** Taints reach a `return` statement. *)
  | ToLval of Taint.taint list * Taint.lval
      (** Taints reach an l-value in the scope of the function/method. *)

val compare_result : result -> result -> int

module Results : Set.S with type elt = result
module Results_tbl : Hashtbl.S with type key = result

type signature = Results.t
(** A (polymorphic) taint signature: simply a set of results for a function.
 *
 * Note that this signature is polymorphic/context-sensitive given that the
 * potential taints coming into the function via its arguments are represented
 * by 'lval' taints, that can be instantiated as needed.
 *
 * For example given:
 *
 *     def foo(x):
 *         sink(x.a)
 *
 * We infer the signature (simplified):
 *
 *     {ToSink {taints_with_precondition = [(x#0).a]; sink = ... ; ...}}
 *
 * where '(x#0).a' is taint variable that denotes the taint of the offset `.a`
 * of the parameter `x` (where '#0' means it is the first argument) of `foo`.
 * The signature tells us that '(x#0).a' will reach a sink.
 *
 * Given a concrete call `foo(obj)`, Semgrep will instantiate this signature with
 * taint assigned to `obj.a` in that calling context. If it is tainted, then
 * Semgrep will report a finding.
 *
 * Also note that, within each function, if there are multiple paths through
 * which a taint source may reach a sink, we do not keep all of them but only
 * the shortest one.
 *
 * THINK: Could we have a "taint shape" for functions/methods ?
 *)

val show_result : result -> string
val show_signature : signature -> string

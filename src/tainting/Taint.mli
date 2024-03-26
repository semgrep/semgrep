(** Taint, taint traces, taint sets, and taint findings. *)

module LabelSet : Set.S with type elt = string
(** A set of taint labels. *)

(*****************************************************************************)
(* Taint *)
(*****************************************************************************)

type tainted_token = AST_generic.tok [@@deriving show]

type tainted_tokens = tainted_token list [@@deriving show]
(** A list of tokens showing where the taint passed through,
  * at present these represent only code variables. For example,
  * when passing through a statement like `x = tainted`, the token
  * corresponding to `x` will be added to this list. *)

(** A call trace to a source or sink match.
  * E.g. Call('foo()', PM('sink(x)')) tells us that by calling `foo(a)` we reach
  * 'sink(x)' (here `a` is the actual argument passed to `foo`, and `x` could be
  *  the formal argument of `foo`). *)
type 'spec call_trace =
  | PM of Pattern_match.t * 'spec
      (** A direct match. The `'spec` would typically contain the pattern that
        * was used to produce the match, e.g. one of the `pattern-sources`.  *)
  | Call of AST_generic.expr * tainted_tokens * 'spec call_trace
      (** An indirect match through a function call. *)

type arg = { name : string; index : int }
(** A formal argument of a function given by its name and it's index/position. *)

val show_arg : arg -> string

(** Base of an 'lval'. *)
type base =
  | BGlob of IL.name  (** A global variable or a static class field. *)
  | BThis  (** The 'this' or 'self' object. *)
  | BArg of arg  (** A formal parameter in a function/method definition. *)

val show_base : base -> string

(** Offset of an 'lval'. *)
type offset =
  | Ofld of IL.name  (** A field, like `.a` *)
  | Oint of int  (** A constant integer index, like `[42]` *)
  | Ostr of string  (** A constant string index, like `['foo']` *)
  | Oany  (** An arbitrary non-constant index, `[*]` *)

val compare_offset : offset -> offset -> int
val show_offset : offset -> string
val offset_of_IL : IL.offset -> offset

type lval = { base : base; offset : offset list }
(** A restriction of 'IL.lval', the l-values that are in the scope of a
 * function/method, and on which we can track taint:
 *
 * - global variables and static class fields
 * - instance fields of the callee object
 * - formal arguments of the function/method
 *
 * See 'orig' and 'result' / 'signature' for more details.
 *)

val hook_offset_of_IL : (IL.offset -> offset) option ref
(** Pro index sensitivity *)

type source = {
  call_trace : Rule.taint_source call_trace;
  label : string;
      (** The label of this particular taint.
       * This may not agree with the source of the `call_trace`, because
       * this label may have changed by a propagator.
       *)
  precondition : (taint list * Rule.precondition) option;
      (** A precondition is a Boolean formula over taint labels that must
       * hold of a list of "input" taints, which should include at least one
       * taint variable (if there were no taint variables then the precondition
       * can be trivially solved). The precondition expression should be neither
       * 'PBool true' nor 'PBool false'. Trivially true preconditions are
       * represented by 'None', and trivially false preconditions should be
       * droppped from taint sets.
       *
       * A taint with an attached precondition is "conditional", because
       * its existence depends on the taints that arrive through the inputs of
       * a function.  This is spawned by (as of now) sources with an attached
       * `requires`. In the interprocedural case, the presence of a conditional
       * taint label may depend on the particular taint that the taint variables
       * are instantiated with.
       *
       * For example, given:
       *
       *     - label: B
       *       requires: A
       *       pattern: a2b(...)
       *
       * and a function definition:
       *
       *     def foo(x):
       *         return a2b(x)
       *
       * we will determine that the taint of `a2b(x)` is `B` *conditional to*
       * `x` having taint 'A':
       *
       *    (['arg(x#0)'], PLabel A)
       *)
}

(** The origin of taint, where does taint comes from? *)
and orig =
  | Src of source  (** An actual taint source (a `pattern-sources` match). *)
  | Var of lval
      (** Polymorphic taint variable.
       *
       * Taint variables are introduced by variables that are in the scope of
       * a function/method definition, and that are considered like "inputs"
       * or parameters. We identify the variables with the l-value from which
       * they originate. For example given:
       *
       *     def foo(x):
       *       return x.a
       *
       * We assign `x` the taint variable `x` too, that is:
       *
       *     { base = BArg {name = "x"; index = 0}; offset = [] }
       *
       * And `x.a` would be another taint variable resulting from adding the
       * offset `.a` to the taint of `x`, that is:
       *
       *     { base = BArg {name = "x"; index = 0}; offset = [Ofld "a"] }
       *
       * It is then trivial to instantiate taint variables, for a concrete call.
       * If we encounter a call `foo(obj)`, we can easily compute the taint
       * returned by that function by 1) subtituting `x` with `obj` thus obtaining
       * the l-value `obj.a`, and 2) looking up the taint attached to `obj.a` in
       * the environment.
       *)
  | Control  (** Polymorphic taint variable, but for the "control-flow". *)

and taint = { orig : orig; tokens : tainted_tokens }
(** At a given program location, taint is given by its origin (i.e. 'orig') and
 * the path it took from that origin to the current location (i.e. 'tokens'). *)

val trace_of_pm : Pattern_match.t * 'a -> 'a call_trace
val pm_of_trace : 'a call_trace -> Pattern_match.t * 'a

(* TODO: Move 'Dataflow_tainting.subst_in_precondition' here, parameterized
     by 'arg_to_taints'. *)
(* This function just maps, bottom-up, the preconditions in a taint.
   Since this only acts on the children of a taint, the type remains
   the same.
*)
val map_preconditions : (taint list -> taint list) -> taint -> taint option
val show_lval : lval -> string
val show_taint : taint -> string

(*****************************************************************************)
(* Taint sets *)
(*****************************************************************************)

(** A set of taints, where given two pieces of taint that are the same except
 * for "details" such as their call trace, the set picks the "best" one (e.g.
 * the one with the shortest trace). *)
module Taint_set : sig
  type t

  val empty : t
  val is_empty : t -> bool
  val cardinal : t -> int
  val equal : t -> t -> bool
  val singleton : taint -> t
  val add : taint -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val map : (taint -> taint) -> t -> t
  val iter : (taint -> unit) -> t -> unit
  val fold : (taint -> 'a -> 'a) -> t -> 'a -> 'a
  val of_list : taint list -> t
  val to_seq : t -> taint Seq.t
  val elements : t -> taint list
end

type taints = Taint_set.t

val solve_precondition :
  ignore_poly_taint:bool -> taints:taints -> Rule.precondition -> bool option

val taints_satisfy_requires : taint list -> Rule.precondition -> bool

val taints_of_pms :
  incoming:taints -> (Pattern_match.t * Rule.taint_source) list -> taints

val show_taints : taints -> string

(*****************************************************************************)
(* Taint results & signatures *)
(*****************************************************************************)

type sink = { pm : Pattern_match.t; rule_sink : Rule.taint_sink }
[@@deriving show]
(** A sink match with its corresponding sink specification (one of the `pattern-sinks`). *)

type taint_to_sink_item = {
  taint : taint;
  sink_trace : unit call_trace;
      (** This trace is from the current calling context of the taint finding,
        to the sink.
        It's a `unit` call_trace because we don't actually need the item at the
        end, and we need to be able to dispatch on the particular variant of taint
        (source or arg).
        *)
}
[@@deriving show]

type taints_to_sink = {
  taints_with_precondition : taint_to_sink_item list * Rule.precondition;
      (** Taints reaching the sink and the precondition for the sink to apply. *)
  sink : sink;
  merged_env : Metavariable.bindings;
      (** The metavariable environment that results of merging the environment from
   * matching the source and the one from matching the sink. *)
}
[@@deriving show]

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
  | ToReturn of taint list * AST_generic.tok
      (** Taints reach a `return` statement. *)
  | ToLval of taint list * lval
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

module LabelSet : Set.S with type elt = string

type tainted_tokens = AST_generic.tok list [@@deriving show]
(** A list of tokens showing where the taint passed through,
  * at present these represent only code variables. *)

(** A call trace to a source or sink match.
  * E.g. Call('foo(a)', PM('sink(x)')) is an indirect match for 'sink(x)'
  * through the function call 'foo(a)'. *)
type 'a call_trace =
  | PM of Pattern_match.t * 'a  (** A direct match.  *)
  | Call of AST_generic.expr * tainted_tokens * 'a call_trace
      (** An indirect match through a function call. *)
[@@deriving show]

type sink = { pm : Pattern_match.t; rule_sink : Rule.taint_sink }
[@@deriving show]

type arg_pos = string * int [@@deriving show]
type arg = { pos : arg_pos; offset : IL.name list } [@@deriving show]

type source = {
  call_trace : Rule.taint_source call_trace;
  label : string;
      (** The label of this particular taint.
        This may not agree with the source of the `call_trace`, because
        this label may have changed, for instance by being propagated to
        a different label.
      *)
  precondition : (taint list * Rule.precondition) option;
      (** A precondition is a Boolean formula that must hold of a list of
          taints, which may include taint variables.
          A taint with an attached precondition is a "hypothetical taint",
          because it may or may not actually exist. This is spawned by
          (as of now) sources with an attached `requires`, because in the
          interprocedural case, this taint's existence may depend on the
          particular taint that the taint variables are instantiated with.
      *)
}
[@@deriving show]

(** The origin of taint, where does taint comes from? *)
and orig =
  | Src of source  (** An actual taint source (`pattern-sources:` match). *)
  | Arg of arg
      (** A taint variable (potential taint coming through an argument). *)
[@@deriving show]

and taint = { orig : orig; tokens : tainted_tokens } [@@deriving show]

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
  sink : sink;
  merged_env : Metavariable.bindings;
}
[@@deriving show]

(** Function-level finding (not necessarily a Semgrep finding). These may
  * depend on taint variables so they must be interpreted on a specific
  * context.
  *)
type finding =
  | ToSink of taints_to_sink
      (** Taint sources or potentially-tainted arguments inside the function
          reach a sink. *)
  | ToReturn of taint list * AST_generic.tok
      (** Taint sources or potentially-tainted arguments
          would reach a `return` statement. *)
  | ToArg of taint list * arg
[@@deriving show]

type signature = finding list
(** A taint signature, it is simply a list of findings for a function.
 *
 * Note that `ArgToSink` and `ArgToReturn` introduce a form of
 * "taint polymorphism", making the taint analysis context-sensitive.
 *
 * Also note that, within each function, if there are multiple paths through
 * which a taint source may reach a sink, we do not keep all of them but only
 * the shortest one.
 *
 * THINK: We could write this in a way that resembles a function type,
 *   but right now it would probably just add complexity. *)

(** A set of taint sources. *)
module Taint_set : sig
  type t

  val empty : t
  val is_empty : t -> bool
  val equal : t -> t -> bool
  val singleton : taint -> t
  val add : taint -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val map : (taint -> taint) -> t -> t
  val concat_map : (taint -> t) -> t -> t
  val iter : (taint -> unit) -> t -> unit
  val fold : (taint -> 'a -> 'a) -> t -> 'a -> 'a
  val of_list : taint list -> t
  val to_seq : t -> taint Seq.t
  val elements : t -> taint list
end

type taints = Taint_set.t

val trace_of_pm : Pattern_match.t * 'a -> 'a call_trace
val pm_of_trace : 'a call_trace -> Pattern_match.t * 'a

val solve_precondition :
  ?ignore_poly_taint:bool -> taints:taints -> Rule.precondition -> bool option

val taints_satisfy_requires : taint list -> Rule.precondition -> bool

val taints_of_pms :
  incoming:taints -> (Pattern_match.t * Rule.taint_source) list -> taints

(* This function just maps, bottom-up, the preconditions in a taint.
   Since this only acts on the children of a taint, the type remains
   the same.
*)
val map_preconditions : (taint list -> taint list) -> taint -> taint
val show_taints : taints -> string
val _show_arg : arg -> string
val _show_finding : finding -> string
val _show_taint : taint -> string

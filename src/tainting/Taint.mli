(** Taint, taint traces, taint sets, and taint findings. *)

module LabelSet : Set.S with type elt = string
(** A set of taint labels. *)

(*****************************************************************************)
(* Taint *)
(*****************************************************************************)

(* TODO: Use mutual-rec modules as in 'Shape_and_sig' so we can use 'Taint_set.t`
 *   everywhere it is needed, instead of `taint list`s. *)
type tainted_token = Tok.t [@@deriving show]

type tainted_tokens = tainted_token list [@@deriving show]
(** A list of tokens showing where the taint passed through,
  * at present these represent only code variables. For example,
  * when passing through a statement like `x = tainted`, the token
  * corresponding to `x` will be added to this list. *)

type rev_tainted_tokens = tainted_tokens [@@deriving show]
(** Reversed to add new tokens with a simple O(1) cons. *)

(** A call trace to a source or sink match.
  * E.g. Call('foo()', PM('sink(x)')) tells us that by calling `foo(a)` we reach
  * 'sink(x)' (here `a` is the actual argument passed to `foo`, and `x` could be
  *  the formal argument of `foo`). *)
type 'spec call_trace =
  | PM of Core_match.t * 'spec
      (** A direct match. The `'spec` would typically contain the pattern that
        * was used to produce the match, e.g. one of the `pattern-sources`.  *)
  | Call of AST_generic.expr * tainted_tokens * 'spec call_trace
      (** An indirect match through a function call. *)

val show_call_trace : ('spec -> string) -> 'spec call_trace -> string

type arg = { name : IL.name; index : int } [@@deriving eq, ord]
(** A formal argument of a function given by its name and it's index/position. *)

val show_arg : arg -> string

(** Base of an 'lval'. *)
type base =
  | BVar of IL.name  (** A global variable or a static class field. *)
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
val show_offset_list : offset list -> string
val offset_of_IL : IL.offset -> offset
val offset_of_rev_IL_offset : rev_offset:IL.offset list -> offset list

val rev_IL_offset_of_offset : offset list -> IL.offset list option
(** TODO(shapes): This is needed for stuff that is not yet fully adapted to shapes
  *               such as records and dicts.
  *
  * Constructs an reversed IL offset corresponding to a taint offset, if possible
  * (if there is no `Oany`).
  *)

type lval = {
  base : base;
  offset : offset list;
      (** An offset `.a.b.c` is encoded as `[.a; .b; .c]`, note the difference
          wrt 'IL.offset', this offset list is **not** reversed! *)
}
(** A restriction of 'IL.lval', the l-values that are in the scope of a
 * function/method, and on which we can track taint:
 *
 * - global variables and static class fields
 * - instance fields of the callee object
 * - formal arguments of the function/method
 *
 * See 'orig' and 'result' / 'signature' for more details.
 *)

val lval_of_arg : arg -> lval

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
  | Shape_var of lval
      (** A taint shape-variable stands for the taints reachable through the
        * shape of the 'lval', see 'Taint_sig.gather_all_taints_in_shape'. *)
  | Control  (** Polymorphic taint variable, but for the "control-flow". *)

and taint = { orig : orig; rev_tokens : rev_tainted_tokens }
(** At a given program location, taint is given by its origin (i.e. 'orig') and
 * the path it took from that origin to the current location (i.e. 'tokens'). *)

val trace_of_pm : Core_match.t * 'a -> 'a call_trace
val pm_of_trace : 'a call_trace -> Core_match.t * 'a

(* TODO: Move 'Dataflow_tainting.subst_in_precondition' here, parameterized
     by 'arg_to_taints'. *)
(* This function just maps, bottom-up, the preconditions in a taint.
   Since this only acts on the children of a taint, the type remains
   the same.
*)
val map_preconditions : (taint list -> taint list) -> taint -> taint option
val show_lval : lval -> string
val show_taint : taint -> string
val compare_lval : lval -> lval -> int
val compare_taint : taint -> taint -> int

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
  val compare : t -> t -> int
  val singleton : taint -> t
  val add : taint -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val map : (taint -> taint) -> t -> t
  val iter : (taint -> unit) -> t -> unit
  val fold : (taint -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (taint -> bool) -> t -> t
  val of_list : taint list -> t
  val to_seq : t -> taint Seq.t
  val elements : t -> taint list
end

type taints = Taint_set.t

val solve_precondition :
  ignore_poly_taint:bool -> taints:taints -> Rule.precondition -> bool option

val taints_satisfy_requires : taint list -> Rule.precondition -> bool

val taints_of_pms :
  incoming:taints -> (Core_match.t * Rule.taint_source) list -> taints

val show_taints : taints -> string

(*****************************************************************************)
(* Taint-oriented comparison functions for non-taint types *)
(*****************************************************************************)

val compare_matches : Core_match.t -> Core_match.t -> int
val compare_metavar_env : Metavariable.bindings -> Metavariable.bindings -> int

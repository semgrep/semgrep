(** Taint shapes *)

module Fields : Map.S with type key = Taint.offset

(**
 * For example, a record expression #{ a: "tainted", b: "safe" } would have
 * the shape `obj {| a: ref<{"tainted"}>(_|_) |}`. Note that `b` is omitted
 * because it has no taint.
 *
 * 'Obj' shapes are open, if a field is not specified then it has the same
 * taint as its parent 'ref'. *)
type shape =
  | Bot  (** _|_, don't know or don't care *)
  | Obj of obj  (** a struct-like thing *)

and ref =
  | Ref of Xtaint.t * shape
      (** A "reference cell", like a `ref` in OCaml, or a variable in C.
   *
   * A ref may be explicitly tainted ('`Tainted'), not explicitly tainted
   * ('`None' / "0"),  or explicitly clean ('`Clean' / "C").
   *
   * A ref that is not explicitly tainted inherits any taints from "parent"
   * refs. A ref that is explicitly clean it is clean regardless.
   *
   * For example, given a variable `x` and the following statements:
   *
   *     x.a := "tainted";
   *     x.a.u := "clean";
   *
   * We could assign the following shape to `x`:
   *
   *     ref<0>( obj {|
   *             a: ref<"tainted">( obj {|
   *                     u: ref<C>(_|_)
   *                     |} )
   *             |} )
   *
   * We have that `x` itself has no taint directly assigned to it, but `x.a` is
   * tainted (by the string `"tainted"`). Other fields like `x.b` are not tainted.
   * When it comes to `x.a`, we have that `x.a.u` has been explicitly marked clean,
   * so `x.a.u` will be considered clean despite `x.a` being tainted. Any other field
   * of `x.a` such as `x.a.v` will inherit the same taint as `x.a`.
   *
   * INVARIANT(ref): To keep shapes minimal:
   *   1. If the xtaint is '`None', then the shape is not 'Bot' and we can reach
   *      another 'ref' whose xtaint is either '`Tainted' or '`Clean'.
   *   2. If the xtaint is '`Clean', then the shape is 'Bot'.
   *      (If we add aliasing we may need to revisit this, and instead just mark
   *       every reachable 'ref' as clean too.)
   *
   * TODO: We can attach "region ids" to refs and assign taints to regions rather than
   *   to refs directly, then we can have alias analysis.
   *)

and obj = ref Fields.t
(**
 * The "default" taints for non-constant indexes are given by the 'Oany' offset.
 *
 * THINK: Instead of 'Oany' maybe have an explicit field ?
 *)

val equal_ref : ref -> ref -> bool
val show_ref : ref -> string

val union_ref : ref -> ref -> ref
(** Merge refs at JOIN nodes of the CFG. *)

val union_taints_in_ref : ref -> Taint.taints
(** Collect and union all taints reachable via a ref. *)

val find_xtaint_ref : IL.offset list -> ref -> Xtaint.t

val taint_ref : Taint.taints -> IL.offset list -> ref -> ref
(** [taint_ref taints offset ref] adds 'taints' to the 'offset' in 'ref'.  *)

val clean_ref : IL.offset list -> ref -> ref
(** [clean_ref offset ref] marks the 'offset' in 'ref' as clean.  *)

val enum_in_ref : ref -> (Taint.offset list * Taint.taints) Seq.t
(**
 * Enumerate all offsets in a ref and their taint.
 *
 * For example,
 *
 *     enum_in_ref (ref<0>( obj {| a: ref<{"tainted"}>(_|_) |} ))
 *
 * would return a sequence with the pair (.a, "tainted").
 *)

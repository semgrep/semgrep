(** Taint shapes and signatures *)

(*****************************************************************************)
(* Taint shapes *)
(*****************************************************************************)

module Fields : Map.S with type key = Taint.offset

(** A shape approximates an object or data structure, and tracks the taint
 * associated with its fields and indexes.
 *
 * For example, a record expression `{ a: "taint", b: "safe" }` would have
 * the shape `Obj { .a -> Cell({"taint"}, _|_) }`, recording that the field `a`
 * is tainted by the string literal `"taint"`. A field like '.a' (the dot '.'
 * indicates that it's a field) or an index like '[0]' will always have a 'cell'
 * shape, because they denote l-values. The first argument of a 'Cell' is its
 * xtaint or "taint status" (see 'Xtaint.t'). For each field and index, we track
 * its xtaint individually (field- and index-sensitivity). Field '.a' in
 * `Obj { .a -> Cell({"taint"}, _|_) }` has the the taint set {"taint"} attached.
 * The second argument of 'Cell' is the shape of the objects stored in that cell.
 * The shape of field '.a' is '_|_' ("bottom") which is given to primitive types,
 * or whenever we "don't care" (or to act as "to-do" as well).
 *
 * If an 'Obj' shape tracks an 'Oany' offset (see 'Taint.offset'), then the taint
 * and shape given to 'Oany' would also be the taint and shape given to any field
 * that is not being explicitly tracked. If there is no 'Oany' in the 'Obj' shape,
 * then a field that is not explicitly tracked would just have an arbitrary or
 * "don't care" shape, and the taint that it inherits from its "parent" 'cell's.
 *
 * For example, given the assignment `x = { a: "taint", b: "safe" }`, the shape
 * of `x` would be `Cell(`None, Obj { .a -> Cell({"taint"}, _|_) })`. The field
 * `b` is omitted in the shape, and if we ask for it's taint and shape we would
 * get the empty taint set (because `x`'s outermost 'Cell' has no taint), and
 * the shape '_|_' because, given that we are not tracking `b`, it means we don't
 * care about it's shape. In a shape like `{ [*] -> Cell({"taint"}, _|_) }}`
 * where `[*]` denotes 'Oany' (that is, an arbitrary index), the taint and shape
 * of any concrete index would be given by the taint and shape of '[*]'.
 *)
type shape =
  | Bot  (** _|_, don't know or don't care *)
  | Obj of obj
      (** An "object" or struct-like thing.
        *
        * Tuples or lists are also represented by 'Obj' shapes! We just treat
        * constant indexes as if they were fields, and use 'Oany' to capture the
        * non-constant indexes.
        *)
  | Arg of Taint.arg
      (** Represents the yet-unknown shape of a function/method parameter. It is
        * a polymorphic shape variable that is meant to be instantiated at call
        * site. Before adding 'Arg' we assumed parameters had shape 'Bot', and
        * 'Arg' still acts like 'Bot' in some places. *)

and cell =
  | Cell of Xtaint.t * shape
      (** A cell or "reference" represents the "storage" of a value, like
        * a variable in C.
        *
        * A cell may be explicitly tainted ('`Tainted'), not explicitly tainted
        * ('`None' / "0"),  or explicitly clean ('`Clean' / "C").
        *
        * A cell that is not explicitly tainted inherits any taints from "parent"
        * refs. A cell that is explicitly clean it is clean regardless.
        *
        * For example, given a variable `x` and the following statements:
        *
        *     x.a := "taint";
        *     x.a.u := "clean";
        *
        * We could assign the following shape to `x`:
        *
        *     Cell(`None, Obj {
        *             .a -> Cell({"taint"}, Obj {
        *                     .u -> Cell(`Clean, _|_)
        *                     })
        *             })
        *
        * We have that `x` itself has no taint directly assigned to it, but `x.a` is
        * tainted (by the string `"taint"`). Other fields like `x.b` are not tainted.
        * When it comes to `x.a`, we have that `x.a.u` has been explicitly marked clean,
        * so `x.a.u` will be considered clean despite `x.a` being tainted. Any other field
        * of `x.a` such as `x.a.v` will inherit the same taint as `x.a`.
        *
        * INVARIANT(cell): To keep shapes minimal:
        *   1. If the xtaint is '`None', then the shape is not 'Bot' and we can reach
        *      another 'cell' whose xtaint is either '`Tainted' or '`Clean'.
        *   2. If the xtaint is '`Clean', then the shape is 'Bot'.
        *      (If we add aliasing we may need to revisit this, and instead just mark
        *       every reachable 'cell' as clean too.)
        *
        * TODO: We can attach "region ids" to refs and assign taints to regions rather than
        *   to refs directly, then we can have alias analysis.
        *)

and obj = cell Fields.t
(**
 * This a mapping from a 'Taint.offset' to a shape 'cell'.
 *
 * The "default" taints for non-constant indexes are given by the 'Oany' ("*") offset.
 * THINK: Instead of 'Oany' maybe have an explicit field ?
 *)

val equal_cell : cell -> cell -> bool
val equal_shape : shape -> shape -> bool
val compare_cell : cell -> cell -> int
val compare_shape : shape -> shape -> int
val show_cell : cell -> string
val show_shape : shape -> string

val taints_and_shape_are_relevant : Taint.taints -> shape -> bool
(** [true] iff the union of [taints] and [gather_all_taints_in_shape shape]
 * is non-empty, or if [shape] contains a cleaned offset. *)

val tuple_like_obj : (Taint.taints * shape) list -> obj
(** Constructs a 0-indexed tuple-like 'obj' from a list of pairs, taints and shape,
 * for each element in the tuple.  *)

val unify_cell : cell -> cell -> cell
(** Unify two 'cell's into one. *)

val unify_shape : shape -> shape -> shape
(** Unify two 'shapes's into one. *)

val gather_all_taints_in_cell : cell -> Taint.taints
(** Gather and union all taints reachable through a cell. *)

val gather_all_taints_in_shape : shape -> Taint.taints
(** Gather and union all taints reachable through a shape. *)

val find_in_cell : Taint.offset list -> cell -> cell option
val find_in_shape : Taint.offset list -> shape -> cell option

val update_offset_and_unify :
  Taint.taints -> shape -> Taint.offset list -> cell option -> cell option
(** Given a 'cell' and an 'offset', it finds the corresponding sub-'cell'
 * for that 'offset', and it updates its 'taints' and 'shape'. If no 'cell'
 * is given (i.e. 'None'), it creates a fresh one. If 'taints' are empty
 * and 'shape' is 'Bot', it just returns the given 'cell' (or 'None'). *)

val clean_cell : Taint.offset list -> cell -> cell
(** [clean_cell offset cell] marks the 'offset' in 'cell' as clean.  *)

val enum_in_cell : cell -> (Taint.offset list * Taint.taints) Seq.t
(**
 * Enumerate all offsets in a cell and their taint.
 *
 * For example,
 *
 *     enum_in_cell (cell<0>( obj {| a: cell<{"tainted"}>(_|_) |} ))
 *
 * would return a sequence with the pair (.a, "tainted").
 *)

(*****************************************************************************)
(* Taint results *)
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

type taints_to_return = {
  data_taints : Taint.taint list;
      (** The taints of the data being returned (typical data propagated via data flow). *)
  data_shape : shape;  (** The shape of the data being returned. *)
  control_taints : Taint.taint list;
      (** The taints propagated via the control flow (cf., `control: true` sources)
   * used for reachability queries. *)
  return_tok : AST_generic.tok;
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
  | ToSink of taints_to_sink
      (** Taints reach a sink.
        *
        * For example:
        *
        *     def foo(x):
        *         y = x
        *         sink(y)
        *
        * The parameter `x` could be tainted depending on the calling context,
        * so we infer:
        *
        *     ToSink { taints_with_precondition = (["taint"], PBool true);
        *              sink = "sink(y)";
        *              ... }
        *)
  | ToReturn of taints_to_return
      (** Taints reach a `return` statement.
        *
        * For example:
        *
        *     def foo():
        *         x = "taint"
        *         return x
        *
        * We infer:
        *
        *     ToReturn(["taint"], Bot, ...)
        *)
  | ToLval of Taint.taint list * Taint.lval
      (** Taints reach an l-value in the scope of the function/method.
        *
        * For example:
        *
        *     x = ["ok"]
        *
        *     def foo():
        *         global x
        *         x[0] = "taint"
        *
        * We infer:
        *
        *     ToLval(["taint"], "x[0]")
        *
        * TODO: Record taint shapes.
        *)

val compare_result : result -> result -> int
val show_result : result -> string

(*****************************************************************************)
(* Taint signatures *)
(*****************************************************************************)

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

val show_signature : signature -> string

(*****************************************************************************)
(* Instantiation *)
(*****************************************************************************)

val instantiate_taint_var :
  inst_lval:(Taint.lval -> (Taint.taints * shape) option) ->
  inst_ctrl:(unit -> Taint.taints) ->
  Taint.taint ->
  (Taint.taints * shape) option

val instantiate_taint :
  callee:IL.exp ->
  inst_lval:(Taint.lval -> (Taint.taints * shape) option) ->
  inst_ctrl:(unit -> Taint.taints) ->
  Taint.taint ->
  Taint.taints
(** Instantiate taints. Instantiation is meant to replace the taint variables
 * in the taint signature of a callee function, with the taints assigned by
 * the caller. *)

val instantiate_shape :
  callee:IL.exp ->
  inst_lval:(Taint.lval -> (Taint.taints * shape) option) ->
  inst_ctrl:(unit -> Taint.taints) ->
  shape ->
  shape
(** Instantiate a shape. Instantiation is meant to replace the taint variables
 * in the taint signature of a callee function, with the taints assigned by
 * the caller. *)

val subst_in_precondition :
  inst_lval:(Taint.lval -> (Taint.taints * shape) option) ->
  inst_ctrl:(unit -> Taint.taints) ->
  Taint.taint ->
  Taint.taint option
(** Instantiate preconditions. Instantiation is meant to replace the taint variables
 * in the taint signature of a callee function, with the taints assigned by
 * the caller. *)

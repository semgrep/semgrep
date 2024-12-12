(** Lval-to-taints environments used by taint-mode.
 *
 * This environment is field-sensitive, but only for l-values of the form
 * x.a_1. ... . a_N (i.e. a variable followed by field accesses). The main
 * purpose of tracking fields is to remove FPs.
 *
 * L-values of the form this.x.a_1. ... . a_N are normalized as
 * x.a_1. ... . a_N. The `this` base is not important as different variables
 * `x` should have different 'sid's. Same applies to `self`, `super`, etc.
 * We rely on Naming_AST to resolve the variables correctly.
 *
 * L-values of the form x.a_1. ... . a_N [i] o_1...o_M are normalized as
 * x.a_1. ... . a_N. That is, we obtain the longest prefix of dot-offsets
 * possible. See docs of `add` and `clean` below for more details.
 *
 * We track taints per variable, but not per object in memory. There is
 * no alias analysis involved!
 *)

open Shape_and_sig.Shape

type t
type env = t
type taints_to_propagate = Taint.taints Dataflow_var_env.VarMap.t
type pending_propagation_dests = IL.lval Dataflow_var_env.VarMap.t

type prop_fn =
  taints_to_propagate:taints_to_propagate ->
  pending_propagation_dests:pending_propagation_dests ->
  env

type add_fn = IL.lval -> Taint.taints -> env -> env

val hook_propagate_to :
  (Dataflow_var_env.var ->
  Taint.taints ->
  taints_to_propagate:taints_to_propagate ->
  pending_propagation_dests:pending_propagation_dests ->
  prop:prop_fn ->
  add:add_fn ->
  t)
  option
  ref
(** Pro hook, this is a bit complicated to avoid exposing `t`s internals. *)

val empty : env
val empty_inout : env Dataflow_core.inout
val normalize_lval : IL.lval -> (IL.name * Taint.offset list) option

val add_shape :
  IL.name -> Taint.offset list -> Taint.taints -> shape -> env -> env

val add_lval_shape : IL.lval -> Taint.taints -> shape -> env -> env
(** Add taints & shape to an l-value.

    Adding taints to x.a_1. ... .a_N will NOT taint the prefixes
    x.a_1. ... .a_i (i < N) (unless they become tainted separately).
 *)

val add : IL.name -> Taint.offset list -> Taint.taints -> env -> env

val add_lval : add_fn
(** Assign a set of taints (but no specific shape) to an l-value. *)

(* THINK: Perhaps keep propagators outside of this environment? *)
val propagate_to : Dataflow_var_env.var -> Taint.taints -> env -> env

val find_var : env -> IL.name -> cell option
(** Find the 'cell' of a variable. *)

val find_lval : env -> IL.lval -> cell option
(** Find the 'cell' of an l-value. *)

val find_poly :
  env -> IL.name -> Taint.offset list -> (Taint.taints * shape) option
(** Find the taints and shape associated to a variable (name) and an offset.
    If an offset is not being explicitly recorded, then it returns the
    taint associated to the longest offset prefix that is recorded. If that
    taint is polymorphic, then it attaches the remaining offset suffix.

    For example, given this shape (where 't is a taint variable):

        Cell(`None, Obj {
                .a -> Cell({"taint"}, Bot);
                .b -> Cell({'t}, Bot)
                })

    With the offset .a we get:

        Some ({"taint"}, Bot)

    With the offset .b we get:

        Some ({'t}, Bot)

    With the offset .a.u we get:

        Some ({"taint"}, Bot)

    With the offset .b.u we get:

        Some ({'t.u}, Bot)
  *)

val find_lval_poly : env -> IL.lval -> (Taint.taints * shape) option
(** Same as 'find_poly' for l-values. *)

val find_lval_xtaint : env -> IL.lval -> Xtaint.t
(** Look up an l-value on the environemnt and return whether it's tainted, clean,
    or we hold no info about it. It does not check sub-lvalues, e.g. if we record
    that 'x.a' is tainted but had no explicit info about 'x.a.b', checking for
    'x.a.b' would return `None. The way we determine whether an l-value is tainted
    is a "bit" more complex, see Dataflow_tainting.check_tainted_lval. *)

val propagate_from : Dataflow_var_env.var -> env -> Taint.taints option * env
val pending_propagation : Dataflow_var_env.var -> IL.lval -> env -> env

val clean : env -> IL.lval -> env
(** Remove taint from an lvalue.

    Cleaning x.a_1. ... .a_N will clean that l-value as well as all its
    extensions x.a_1. ... .a_N. ... .a_M.

    Crucially, cleaning x.a_1. ... . a_N [i] o_1...o_M  is the same as cleaning
    x.a_1. ... . a_N. So, cleaning an element of an array such as x[1] would
    clean the entire array! This seems drastic but it should help reducing FPs.
 *)

val filter_tainted : (IL.name -> bool) -> env -> env
val add_control_taints : env -> Taint.taints -> env
val get_control_taints : env -> Taint.taints

val union : env -> env -> env
(** Compute the environment for the join of two branches.

     If an lvalue x.a_1. ... .a_N was clean in one branch, we still consider it
     clean in the union unless it is explicitly tainted in the other branch.
     Note that if e.g. x.a_1. ... .a_i (with i < N) were tainted in the other
     branch, then x.a_1. ... . a_N may no longer be clean, but we assume the
     best case scenario to reduce FPs. *)

val union_list : ?default:env -> env list -> env
val equal : env -> env -> bool

val equal_by_lval : env -> env -> IL.lval -> bool
(** Check whether two environments assign the exact same taint to an l-value
 * and each one of its extensions. *)

val to_string : env -> string
val seq_of_tainted : env -> (IL.name * cell) Seq.t

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

type t
type env = t

val empty : env
val empty_inout : env Dataflow_core.inout

val add : env -> IL.lval -> Taint.taints -> env
(** Add taints to an l-value.

    Adding taints to x.a_1. ... .a_N will NOT taint the prefixes
    x.a_1. ... .a_i (i < N) (unless they become tainted separately).

    Adding taints to x.a_1. ... . a_N [i] o_1...o_M is effectively
    the same as adding taint to x.a_1. ... . a_N, since this environment
    is not index-sensitive.
 *)

(* THINK: Perhaps keep propagators outside of this environment? *)
val propagate_to : Dataflow_var_env.var -> Taint.taints -> env -> env

val dumb_find : env -> IL.lval -> [> `Clean | `None | `Tainted of Taint.taints ]
(** Look up an l-value on the environemnt and return whether it's tainted, clean,
    or we hold no info about it. It does not check sub-lvalues, e.g. if we record
    that 'x.a' is tainted but had no explicit info about 'x.a.b', checking for
    'x.a.b' would return `None. The way we determine whether an l-value is tainted
    is a "bit" more complex, see Dataflow_tainting.check_tainted_lval. *)

val propagate_from : Dataflow_var_env.var -> env -> Taint.taints option

val clean : env -> IL.lval -> env
(** Remove taint from an lvalue.

    Cleaning x.a_1. ... .a_N will clean that l-value as well as all its
    extensions x.a_1. ... .a_N. ... .a_M.

    Crucially, cleaning x.a_1. ... . a_N [i] o_1...o_M  is the same as cleaning
    x.a_1. ... . a_N. So, cleaning an element of an array such as x[1] would
    clean the entire array! This seems drastic but it should help reducing FPs.
 *)

val union : env -> env -> env
(** Compute the environment for the join of two branches.

     If an lvalue x.a_1. ... .a_N was clean in one branch, we still consider it
     clean in the union unless it is explicitly tainted in the other branch.
     Note that if e.g. x.a_1. ... .a_i (with i < N) were tainted in the other
     branch, then x.a_1. ... . a_N may no longer be clean, but we assume the
     best case scenario to reduce FPs. *)

val equal : env -> env -> bool
val to_string : (Taint.taints -> string) -> env -> string
val seq_of_tainted : env -> (IL.lval * Taint.taints) Seq.t

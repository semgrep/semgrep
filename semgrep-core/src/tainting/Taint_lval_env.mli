(** Lval-to-taints environments used by taint-mode.
 *
 * Only lvalues of the form x.a_1. ... . a_N (i.e. a variable followed by field
 * accesses) are tracked. The main purpose of tracking fields is to remove FPs.
 *
 * These environments help making taint analysis sensitive to individual fields
 * in records/objects in a limited way. Essentially, they add per variable field
 * sensitivity, but not per object in memory field sensitivity. There is no alias
 * analysis involved!
 *)

type t
type env = t

val empty : env
val empty_inout : env Dataflow_core.inout

val add : IL.lval -> Taint.taints -> env -> env
(** Add taint to an lvalue.

    Note that if we add taint to x.a_1. ... .a_N, the prefixes
    x.a_1. ... .a_i (i < N) will not be considered tainted (unless they become
    tainted separately).
 *)

(* THINK: Perhaps keep propagators outside of this environment? *)
val propagate_to : Dataflow_var_env.var -> Taint.taints -> env -> env

val dumb_find : env -> IL.lval -> [> `Clean | `None | `Tainted of Taint.taints ]
(** Look up an lvalue on the environemnt and return whether it's tainted, clean,
    or we hold no info about it. It does not check sub-lvalues, e.g. if we record
    that 'x.a' is tainted but had no explicit info about 'x.a.b', checking for
    'x.a.b' would return `None. The way we determine whether an lvalue is actually
    tainted is a "bit" more complex, see Dataflow_tainting.check_tainted_lval. *)

val propagate_from : Dataflow_var_env.var -> env -> Taint.taints option

val clean : IL.lval -> env -> env
(** Remove taint from an lvalue.

    Given x.a_1. ... .a_N, it will clean that lvalue as well as all its
    extensions x.a_1. ... .a_N. ... .a_M.  *)

val union : env -> env -> env
(** Compute the environment for the join of two branches.

     If an lvalue x.a_1. ... .a_N was clean in one branch, we still consider it
     clean in the union unless it is explicitly tainted in the other branch.
     Note that if e.g. x.a_1. ... .a_i (with i < N) were tainted in the other
     branch, then x.a_1. ... . a_N may no longer be clean, but we assume the
     best case scenario to reduce FPs. *)

val equal : env -> env -> bool
val to_string : (Taint.taints -> string) -> env -> string

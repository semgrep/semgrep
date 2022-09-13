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

val add_var : IL.name -> Taint.taints -> env -> env

(* THINK: Perhaps keep propagators outside of this environment? *)
val propagate_to : Dataflow_var_env.var -> Taint.taints -> env -> env

val find : IL.lval -> env -> [ `None | `Clean | `Tainted of Taint.taints ]
(** Find whether an lvalue is tainted or not.

    [`None] means no taint.
    [`Clean] means no taint for this particular lvalue x.a_1. ... .a_N, but
    some of its prefixes x.a_1. ... .a_i (i < N) is tainted.

    Given x.a_1. ... . a_N, it recursively checks all the prefixes of the
    lvalue (from longest to shortest) until it finds one that is either
    explicitly tainted (returns [`Tainted]) or explicitly clean (returns
    [`Clean]). If none is found then it returns [`None].

    If x.a.b is tainted with label B and x.a is tainted with label A,
    the taint of x.a.b is still just B. *)

val find_var : IL.name -> env -> Taint.taints option
val propagate_from : Dataflow_var_env.var -> env -> Taint.taints option

val clean : IL.lval -> env -> env
(** Remove taint from an lvalue.

    Given x.a_1. ... .a_N, it will clean that lvalue as well as all its
    extensions x.a_1. ... .a_N. ... .a_M.  *)

val clean_var : IL.name -> env -> env

val union : env -> env -> env
(** Compute the environment for the join of two branches.

     If an lvalue x.a_1. ... .a_N was clean in one branch, we still consider it
     clean in the union unless it is explicitly tainted in the other branch.
     Note that f e.g. x.a_1. ... .a_i (with i <> N) were tainted in the other
     branch, then  x.a_1. ... . a_N may no longer be clean, but we assume the
     best case scenario. *)

val equal : env -> env -> bool
val to_string : (Taint.taints -> string) -> env -> string

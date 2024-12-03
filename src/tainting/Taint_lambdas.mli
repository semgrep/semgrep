(** Taint analysis for lambdas *)

(* TODO: Move most lambda-related code to here. *)

val find_vars_to_track_across_lambdas : IL.fun_cfg -> IL.NameSet.t
(**
Computes a set variables that must be tracked across lambdas: this is used
to filter what tainted variables, of those discovered while analyzing a lambda,
may be relevant for the enclosing function.

This is a rough flow-insensitive and cheap approximation of a liveness analysis.

In the example below, `y` can be discarded after `foo(...)` but `x` must be kept
because it is later passed into a sink:

    let x;
    foo(() => { y = taint; x = taint; });
    sink(x);

ALT: We could just do liveness analysis (and we tried) but that seems to be slower
    overall, the cost of the liveness analysis may be higher than the benefit of
    the added precision.
*)

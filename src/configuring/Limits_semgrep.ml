(*****************************************************************************)
(* Const/sym ("svalue") propagation *)
(*****************************************************************************)

(* TODO: Report these timeouts as errors in 'Report.match_result' *)
(* Timeout in seconds.
 * So e.g. the perf of svalue-prop does not prevent rules from running on a file.
 * Note that 'Time_limit.set_timeout' cannot be nested. *)
let svalue_prop_FIXPOINT_TIMEOUT = 0.1

(*****************************************************************************)
(* Taint analysis *)
(*****************************************************************************)

(* We need to set some limits to prevent taint sets from exploding in some cases.
 * As we root cause these problems and fix them properly, we may be able to raise
 * these limits.
 *
 * These problems became more serious with field-sensitivity: where previously we
 * would just track taint for `x`, now we track taint for `x.a`, `x.b` and `x.c`.
 *
 * In the case of 'WebGoat/src/main/resources/webgoat/static/js/libs/ace.js',
 * for example, it seems that this problem would be greatly reduced if we did
 * not propagate taint for data with Boolean and integer type. Improving some
 * of the data structures involved may help too.
 *)

(* TODO: Report these timeouts as errors in 'Report.match_result' *)
(* Timeout in seconds.
 * So e.g. we limit the amount of time that Pro will spend inferring taint signatures.
 * Note that 'Time_limit.set_timeout' cannot be nested. *)
let taint_FIXPOINT_TIMEOUT = 0.1

(** Bounds the number of l-values we can track. *)
let taint_MAX_TAINTED_LVALS = 100

(** Bounds the number of taints we can track per l-value. *)
let taint_MAX_TAINT_SET_SIZE = 50

(** Bounds the length of the offsets we can track per l-value. *)
let taint_MAX_LVAL_OFFSET = 2

(*****************************************************************************)
(* Dataflow analysis *)
(*****************************************************************************)

(* Minimum number of fixpoint iterations to run before checking for timeouts,
 * see 'svalue_prop_FIXPOINT_TIMEOUT' and 'taint_FIXPOINT_TIMEOUT' below. It
 * appears that when Semgrep is allocating too much memory, perhaps due to the
 * GC having to do heavy work, we start to get fixpoint timeouts on functions
 * that should perfectly run within the timeout. But if we just bump the timeouts
 * then that makes perf worse and in some cases even leads to more rule timeouts. *)
let dataflow_FIXPOINT_MIN_ITERS = 100

(*****************************************************************************)
(* Const/sym ("svalue") propagation *)
(*****************************************************************************)

(* TODO: Report these timeouts as errors in 'Report.match_result' *)
(* Timeout in seconds.
 * So e.g. the perf of svalue-prop does not prevent rules from running on a file.
 * Note that 'Time_limit.set_timeout' cannot be nested. *)
let svalue_prop_FIXPOINT_TIMEOUT = 0.15

(* Bounds the number of times that we will follow an 'id_svalue' during
 * a cycle check. See 'Dataflow_svalue.no_cycles_in_svalue'. *)
let svalue_prop_MAX_VISIT_SYM_IN_CYCLE_CHECK = 1000

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
let taint_FIXPOINT_TIMEOUT = 0.2

(** Bounds the number of variables we can track. *)
let taint_MAX_TAINTED_VARS = 50

(** Bounds the number of fields we can track per l-value, that is, the number of
 * fields in an 'Obj' shape, see 'Taint_sig.shape'. *)
let taint_MAX_OBJ_FIELDS = 10

(** Bounds the number of taints we can track per l-value.
 *
 * The size of the taint sets has a significant impact on performance and most
 * "reasonable" taint rules only require small taint sets to work. When the sets
 * grow large is often due to some bug or "inefficiency". The limit could be
 * insufficient for some pathological cases (e.g. rules that have very liberal
 * source specs that match essentially everything), but those we will not be
 * able to run inter-file, and they should be discouraged anyways.
 *)
let taint_MAX_TAINT_SET_SIZE = 25

(** Bounds the length of the offsets we can track per arg/poly-taint. *)
let taint_MAX_POLY_OFFSET = 1

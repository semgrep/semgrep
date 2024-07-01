open AST_generic
module MV = Metavariable
module Log = Log_analyzing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Support for pattern-when, allowing for control flow-related comparisons.
 *
 * Example usage:
 *
 * rules:
 * - id: patten-when
 *   match:
 *   pattern: foo($X);
 *   where:
 *    - comparison: $X == 0
 * ...
 *
 * if (x == NULL) {
 *    // ok
 *    foo(x);
 * } else {
 *    // ruleid: patten-when
 *    foo(x);
 * }
 *
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
(* coupling: copied from Eval_generic so the metavariable values can
   be used here *)
type value = Eval_generic_partial.value

let annotate_facts (_cfg : IL.cfg) = ()

let facts_satisfy_e (_mvars : (Metavariable.mvar, value) Hashtbl.t)
    (_facts : facts) (_e : expr) =
  false

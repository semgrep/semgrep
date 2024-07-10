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
(* Types *)
(*****************************************************************************)

(* also used in Eval_generic.ml *)
type value = Eval_generic_partial.value

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let hook_annotate_facts = ref None
let hook_facts_satisfy_e = ref None

(* if the -path_sensitive flag is enabled, the ref below will be set to
 * true. the functions in this file will only execute if both
 * -deep_intra_file and -path_sensitive are enabled.
 *)
let hook_path_sensitive = ref false

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let annotate_facts (_cfg : IL.cfg) = if !hook_path_sensitive then ()

let facts_satisfy_e (_mvars : (Metavariable.mvar, value) Hashtbl.t)
    (_facts : facts) (_e : expr) =
  !hook_path_sensitive && false

let with_pro_hooks f =
  Common.save_excursion hook_annotate_facts (Some annotate_facts) (fun () ->
      Common.save_excursion hook_facts_satisfy_e (Some facts_satisfy_e) f)

let setup_pro_hooks () =
  hook_annotate_facts := Some annotate_facts;
  hook_facts_satisfy_e := Some facts_satisfy_e

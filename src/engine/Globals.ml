(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal of this module is mostly to document the "dangerous" globals
 * used inside Semgrep.
 *
 * Ultimately, we want to elimitate all globals. Until now, those globals did
 * not create too many issues because of the use of Parmap and fork
 * in Core_scan.ml (the modifications of those globals in the child process
 * do not affect the state in the parent process), but as soon as we migrate to
 * using domains instead with OCaml 5.0, those globals will haunt us back.
 * Maybe Domain-local-storage globals could help, but even better if we
 * can eliminate them.
 *
 * To find candidates for those "dangerous" globals, you can start with:
 *  $ ./bin/osemgrep --experimental -e 'val $V: $T ref' -l ocaml src/ libs/
 *  $ ./bin/osemgrep --experimental -e 'let $V: $T = ref $X' -l ocaml src/ libs/
 *)

type pro_hook_ref =
  | Pro_hook_ref : 'a option ref -> pro_hook_ref
      (** 'pro_hook_ref' is a GADT, this allows us to do an "existential" quantification
       * over `'a`. That is, you can take any hook with type '... option ref' and wrap it
       * as a 'pro_hook_ref'. Of course, when you pattern match on one of this, you cannot
       * make any assumptions on what that `'a` is, but it is enough to reset the hooks.
       * So we can enumerate Pro hooks in 'pro_hooks_refs' and then write both
       * 'reset_pro_hooks' and 'save_pro_hooks_and_reset' based on that list. *)

let pro_hooks_refs =
  [
    Pro_hook_ref Generic_vs_generic.hook_find_possible_parents;
    Pro_hook_ref Constant_propagation.hook_propagate_basic_visitor;
    Pro_hook_ref Dataflow_svalue.hook_constness_of_function;
    Pro_hook_ref Dataflow_svalue.hook_transfer_of_assume;
    Pro_hook_ref Match_tainting_mode.hook_setup_hook_function_taint_signature;
    Pro_hook_ref Taint_lval_env.hook_propagate_to;
    Pro_hook_ref Dataflow_tainting.hook_function_taint_signature;
    Pro_hook_ref Dataflow_tainting.hook_find_attribute_in_class;
    (* TODO: more Pro hooks ? *)
  ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Prefer 'save_pro_hooks_and_reset'. *)
let reset_pro_hooks () =
  pro_hooks_refs |> List.iter (fun (Pro_hook_ref pro_hook) -> pro_hook := None)

(* Saves current Pro hooks, and temporarily resets them for running a function.
 * When the function has completed, it restores the saved Pro hooks. This confines
 * the reach of the "reset", making it easier to reason about, and causing fewer
 * surprises, thus it is preferred over 'reset_pro_hooks' which has the opposite
 * properties. *)
let save_pro_hooks_and_reset f0 =
  let f =
    pro_hooks_refs
    |> List.fold_left
         (fun f (Pro_hook_ref pro_hook) () ->
           Common.save_excursion pro_hook None f)
         f0
  in
  f ()

(* Useful for defensive programming, especially in tests which may leave
 * bad state behind.
 * Note that it's currently unused, because we should prefer to fix our tests
 * to restore the globals they modified, but as a last resort, you can
 * use this function.
 *)
let reset () =
  Core_error.g_errors := [];
  Core_profiling.mode := Core_profiling.MNo_info;
  AST_generic_equals.busy_with_equal := AST_generic_equals.Not_busy;
  Rule.last_matched_rule := None;
  reset_pro_hooks ();
  (* TODO?
   * - Http_helpers.client_ref ?
   * - many more
   *)
  ()

(* Iago Abal
 *
 * Copyright (C) 2023-2024 Semgrep Inc.
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
(* Gather all pro hooks in one place.
 *
 * alt: maybe we could move this file in src/core/, and actually define
 * the hooks there, but some hooks depends on types outside src/core/
 * so simpler to keep the hook spreaded, but at least reset them here.
 *
 * alt: this could could be moved to semgrep-pro. It's only used by
 * Globals.reset() which itself is not used.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type pro_hook_ref =
  | Pro_hook_ref : 'a option ref -> pro_hook_ref
      (** 'pro_hook_ref' is a GADT, this allows us to do an "existential"
       * quantification over `'a`. That is, you can take any hook with
       * type '... option ref' and wrap it as a 'pro_hook_ref'. Of course,
       * when you pattern match on one of this, you cannot make any assumptions
       * on what that `'a` is, but it is enough to reset the hooks.
       * So we can enumerate Pro hooks in 'pro_hooks_refs' and then write
       * 'reset_pro_hooks' and 'save_pro_hooks_and_reset' based on that list.
       *)

(* TODO: a steps-mode rule to ensure we have all the pro hooks.
 * Do we have all of them?
 *)
let pro_hooks_refs =
  [
    Pro_hook_ref Generic_vs_generic.hook_find_possible_parents;
    Pro_hook_ref Generic_vs_generic.hook_r2c_pro_was_here;
    Pro_hook_ref Constant_propagation.hook_propagate_basic_visitor;
    Pro_hook_ref Dataflow_svalue.hook_constness_of_function;
    Pro_hook_ref Dataflow_svalue.hook_transfer_of_assume;
    Pro_hook_ref Match_tainting_mode.hook_setup_hook_function_taint_signature;
    Pro_hook_ref Taint.hook_offset_of_IL;
    Pro_hook_ref Taint_lval_env.hook_propagate_to;
    Pro_hook_ref Dataflow_tainting.hook_function_taint_signature;
    Pro_hook_ref Dataflow_tainting.hook_find_attribute_in_class;
    Pro_hook_ref Dataflow_tainting.hook_check_tainted_at_exit_sinks;
    Pro_hook_ref Dataflow_when.hook_annotate_facts;
    Pro_hook_ref Dataflow_when.hook_facts_satisfy_e;
    Pro_hook_ref Typing.pro_hook_type_of_expr;
  ]

(*****************************************************************************)
(* Reset *)
(*****************************************************************************)

(* Saves current Pro hooks, and temporarily resets them for running a function.
 * When the function has completed, it restores the saved Pro hooks. This
 * confines the reach of the "reset", making it easier to reason about, and
 * causing fewer surprises, thus it is preferred over 'reset_pro_hooks' which
 * has the opposite properties.
 *)
let save_pro_hooks_and_reset f0 =
  let f =
    pro_hooks_refs
    |> List.fold_left
         (fun f (Pro_hook_ref pro_hook) () ->
           Common.save_excursion pro_hook None f)
         f0
  in
  f ()

let reset_pro_hooks () =
  pro_hooks_refs |> List.iter (fun (Pro_hook_ref pro_hook) -> pro_hook := None)

(* Andre Kuhlenschmidt
 *
 * Copyright (C) 2023 r2c
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
(* A general mechanism to run pre and post processors around
 * a core scan.
 *
 * For now this is used mostly for Secrets (just the post part,
 * for secrets validation).
 *
 * alt: right now this is using first-class modules, but we could
 * probably use simple polymorphic records.
 *)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

module type Processor = sig
  type state

  val pre_process : Core_scan_config.t -> Rule.t list -> Rule.t list * state

  val post_process :
    Core_scan_config.t -> state -> Core_result.t -> Core_result.t
end

(* The default processor is the identity processor which does nothing. *)
module No_Op_Processor : Processor = struct
  type state = unit

  let pre_process _ rules = (rules, ())
  let post_process _ () results = results
end

module Autofix_processor : Processor = struct
  type state = unit

  let pre_process _config rules = (rules, ())

  let post_process (config : Core_scan_config.t) () (res : Core_result.t) =
    (* These edits should all be None, so it's OK to `fst` them out. *)
    let matches_with_fixes =
      Autofix.produce_autofixes (Common.map fst res.matches)
    in
    if config.autofix then Autofix.apply_fixes matches_with_fixes;
    { res with matches = matches_with_fixes }
end

let hook_processor = ref (module Autofix_processor : Processor)

(* quite similar to Core_scan.core_scan_func *)
type 'a core_scan_func_with_rules =
  'a ->
  (Rule.t list * Rule.invalid_rule_error list) * float (* rule parse time *) ->
  Core_result.t

(*****************************************************************************)
(* Composing processors *)
(*****************************************************************************)

(* For internal use, to compose processors.
   This runs A and then B, like a donut.

   A preprocess -> B preprocess -> B postprocess -> A postprocess
*)
module MkPairProcessor (A : Processor) (B : Processor) : Processor = struct
  type state = A.state * B.state

  let pre_process config rules =
    let rules, state_a = A.pre_process config rules in
    let rules, state_b = B.pre_process config rules in
    (rules, (state_a, state_b))

  let post_process config (state_a, state_b) results =
    results |> B.post_process config state_b |> A.post_process config state_a
end

let push_processor (module P : Processor) =
  let module Paired = MkPairProcessor (P) ((val !hook_processor)) in
  hook_processor := (module Paired)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Written with scan_with_rules abstracted to allow reuse across
   semgrep and semgrep-pro
*)
let call_with_pre_and_post_processor f
    (scan_with_rules : 'a core_scan_func_with_rules) :
    'a core_scan_func_with_rules =
 fun config ((rules, rule_errors), rules_parse_time) ->
  let module Processor = (val !hook_processor) in
  let rules', state = Processor.pre_process (f config) rules in
  let res = scan_with_rules config ((rules', rule_errors), rules_parse_time) in
  Processor.post_process (f config) state res

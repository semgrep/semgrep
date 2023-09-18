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

  val pre_process : Rule.t list -> Rule.t list * state
  val post_process : state -> Core_result.t -> Core_result.t
end

(* The default processor is the identity processor which does nothing. *)
module No_Op_Processor : Processor = struct
  type state = unit

  let pre_process rules = (rules, ())
  let post_process () results = results
end

let hook_processor = ref (module No_Op_Processor : Processor)

(* quite similar to Core_scan.core_scan_func *)
type core_scan_func_with_rules =
  (Rule.t list * Rule.invalid_rule_error list) * float (* rule parse time *) ->
  Core_result.t

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Written with scan_with_rules abstracted to allow reuse across
   semgrep and semgrep-pro
*)
let call_with_pre_and_post_processor
    (scan_with_rules : core_scan_func_with_rules) : core_scan_func_with_rules =
 fun ((rules, rule_errors), rules_parse_time) ->
  let module Processor = (val !hook_processor) in
  let rules', state = Processor.pre_process rules in
  let res = scan_with_rules ((rules', rule_errors), rules_parse_time) in
  Processor.post_process state res

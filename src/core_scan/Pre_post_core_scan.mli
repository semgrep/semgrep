(* Pre and Post Processors Hook For Semgrep Pro or other Extensions *)

module type Processor = sig
  (* Each processor can define its own state/environment data structure *)
  type state

  (* pre process the set of rules (example: ??) *)
  val pre_process : Core_scan_config.t -> Rule.t list -> Rule.t list * state

  (* post process the result (example: ??) *)
  val post_process :
    Core_scan_config.t -> state -> Core_result.t -> Core_result.t
end

(* The default processor is the identity processor which does nothing. *)
module No_Op_Processor : Processor

(* The default hook is the No_op_processor *)
val hook_processor : (module Processor) ref

(* quite similar to Core_scan.core_scan_func *)
type 'a core_scan_func_with_rules =
  'a ->
  (Rule.t list * Rule.invalid_rule_error list) * float (* rule parse time *) ->
  Core_result.t

(* [call_with_pre_and_post_processor scan] will return a scan
 * function that will also run the pre/post hooks in
 * hook_processor
 *)
val call_with_pre_and_post_processor :
  ('a -> Core_scan_config.t) ->
  'a core_scan_func_with_rules ->
  'a core_scan_func_with_rules

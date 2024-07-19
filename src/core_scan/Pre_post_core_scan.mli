(* Pre and Post Processors Hook around a core scan *)

module type Processor = sig
  (* Each processor can define its own state/environment data structure *)
  type state

  (* pre process the set of rules (example: ??) *)
  val pre_process : Core_scan_config.t -> Rule.t list -> Rule.t list * state

  (* post process the result (example: ??) *)
  val post_process :
    Core_scan_config.t -> state -> Core_result.t -> Core_result.t
end

(* The no-op processor is the identity processor which does nothing. *)
module No_Op_Processor : Processor

(* Registers a processor for usage.
   This processor will act as an "outer layer", preprocessing before other
   registered processors, and postprocessing after.
*)
val push_processor : (module Processor) -> unit

(* quite similar to Core_scan.core_scan_func *)
type 'config core_scan_func_with_rules =
  'config ->
  Rule_error.rules_and_invalid * float (* rule parse time *) ->
  Core_result.t

(* [call_with_pre_and_post_processor scan] will return a scan
 * function that will also run the pre/post hooks in
 * hook_processor
 *)
val call_with_pre_and_post_processor :
  ('config -> Core_scan_config.t) ->
  'config core_scan_func_with_rules ->
  'config core_scan_func_with_rules

(* Exposed only for testing purposes. These can be used to arbitrarily change
 * the set of pre and post processors. *)

type test_only_processors

(* The default hook is composed of the nosemgrep processor and the autofix
 * processor. *)
val test_only_hook_processors : test_only_processors ref

val test_only_processors_of_processor :
  (module Processor) -> test_only_processors

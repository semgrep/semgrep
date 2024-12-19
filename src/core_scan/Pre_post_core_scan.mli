(* Pre and Post Processors Hook around a core scan *)

(*****************************************************************************)
(* SimpleProcessor *)
(*****************************************************************************)

type post_process_result = {
  (* Could be an option if we want to use the post processor to filter findings.
   * *)
  match_ : Core_result.processed_match;
  (* Must be a persistent sequence. Mainly used to avoid the time complexity of
   * List concatenation *)
  errors : Core_error.t Seq.t;
}

val post_process_result_of_match :
  Core_result.processed_match -> post_process_result

(* Default implementation for `handle_post_process_exn` that passes through the
 * match unmodified and adds the exception to the error list. *)
val default_handle_post_process_exn :
  Core_scan_config.t ->
  'state ->
  Core_result.processed_match ->
  Exception.t ->
  'state * post_process_result

(* A simpler interface for a pre/post processor that leaves less error handling
 * burden on the implementer. *)
module type SimpleProcessor = sig
  type state

  (* TODO Change to a per-rule interface for better default error handling. *)
  val pre_process : Core_scan_config.t -> Rule.t list -> Rule.t list * state

  (* Post process a single match, optionally updating the state. *)
  val post_process :
    Core_scan_config.t ->
    state ->
    Core_result.processed_match ->
    state * post_process_result

  (* What should happen if `post_process` raises an exception? It is recommended
   * to produce an error but salvage the finding. If there is no reasonable
   * alternative, implementers may re-raise and bring down the whole scan. *)
  val handle_post_process_exn :
    Core_scan_config.t ->
    state ->
    Core_result.processed_match ->
    Exception.t ->
    state * post_process_result
end

(*****************************************************************************)
(* Processor *)
(*****************************************************************************)

(* Fully general pre/post processor. Avoid in favor of SimpleProcessor when
 * possible.
 *
 * Implementers of this module type MUST catch and handle exceptions
 * appropriately, containing them to single rules, files, or matches whenever
 * possible. *)
module type Processor = sig
  (* Each processor can define its own state/environment data structure *)
  type state

  (* pre process the set of rules (example: ??) *)
  val pre_process : Core_scan_config.t -> Rule.t list -> Rule.t list * state

  (* post process the result (example: ??) *)
  val post_process :
    Core_scan_config.t -> state -> Core_result.t -> Core_result.t
end

(*****************************************************************************)
(* Entry Points *)
(*****************************************************************************)

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

(*****************************************************************************)
(* TEST ONLY *)
(*****************************************************************************)

(* Exposed only for testing purposes. These can be used to arbitrarily change
 * the set of pre and post processors. *)

type test_only_processors

(* The default hook is composed of the nosemgrep processor and the autofix
 * processor. *)
val test_only_hook_processors : test_only_processors ref

val test_only_processors_of_processor :
  (module Processor) -> test_only_processors

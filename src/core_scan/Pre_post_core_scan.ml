(* Andre Kuhlenschmidt
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
(* A general mechanism to run pre and post processors around
 * a core scan.
 *
 * alt: right now this is using first-class modules, but we could
 * probably use simple polymorphic records.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

module type Processor = sig
  type state

  val pre_process : Core_scan_config.t -> Rule.t list -> Rule.t list * state

  val post_process :
    Core_scan_config.t -> state -> Core_result.t -> Core_result.t
end

(* quite similar to Core_scan.core_scan_func *)
type 'config core_scan_func_with_rules =
  'config ->
  Rule_error.rules_and_invalid * float (* rule parse time *) ->
  Core_result.t

type post_process_result = {
  (* Could be an option if we want to use the post processor to filter findings.
   * *)
  match_ : Core_result.processed_match;
  (* Must be a persistent sequence. Mainly used to avoid the time complexity of
   * List concatenation *)
  errors : Core_error.t Seq.t;
}

module type SimpleProcessor = sig
  type state

  val pre_process : Core_scan_config.t -> Rule.t list -> Rule.t list * state

  val post_process :
    Core_scan_config.t ->
    state ->
    Core_result.processed_match ->
    state * post_process_result

  val handle_post_process_exn :
    Core_scan_config.t ->
    state ->
    Core_result.processed_match ->
    Exception.t ->
    state * post_process_result
end

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let post_process_result_of_match match_ = { match_; errors = Seq.empty }

let default_handle_post_process_exn _config state
    (match_ : Core_result.processed_match) exn =
  let file =
    match match_.pm.path.origin with
    | File file -> Some file
    | GitBlob _ -> None
  in
  let error = Core_error.exn_to_error ?file exn in
  (state, { match_; errors = Seq.return error })

(*****************************************************************************)
(* Processors *)
(*****************************************************************************)

module WrapSimpleProcessor (P : SimpleProcessor) : Processor = struct
  type state = P.state

  let pre_process = P.pre_process

  let post_process config state (res : Core_result.t) =
    let matches = res.processed_matches in
    let _state, matches_rev, new_errors =
      List.fold_left
        (fun (state, matches, errors_acc) match_ ->
          let state, { match_; errors } =
            (* TODO Error handling here! *)
            try P.post_process config state match_ with
            | (Time_limit.Timeout _ | Common.UnixExit _) as e ->
                Exception.catch_and_reraise e
            | exn ->
                let e = Exception.catch exn in
                P.handle_post_process_exn config state match_ e
          in
          (state, match_ :: matches, Seq.append errors_acc errors))
        (state, [], Seq.empty) matches
    in
    let matches = List.rev matches_rev in
    let new_errors = List.of_seq new_errors in
    { res with processed_matches = matches; errors = res.errors @ new_errors }
end

(* The default processor is the identity processor which does nothing. *)
module No_Op_Processor : Processor = struct
  type state = unit

  let pre_process _ rules = (rules, ())
  let post_process _ () results = results
end

(* Autofix is currently implemented as a postprocessor to factorize code
 * between pysemgrep (which uses core_scan) and osemgrep.
 * LATER: once pysemgrep is gone, we can do that directly in the core_scan.
 *)
module Autofix_processor : SimpleProcessor = struct
  type state = unit

  let pre_process _config rules = (rules, ())

  let post_process (_config : Core_scan_config.t) ()
      (res : Core_result.processed_match) =
    Logs_.with_debug_trace ~__FUNCTION__
      begin
        fun () ->
          ((), post_process_result_of_match (Autofix.produce_autofix res))
      end

  let handle_post_process_exn = default_handle_post_process_exn
end

(* Similar motivation than Autofix above *)
module Nosemgrep_processor : SimpleProcessor = struct
  type state = unit

  let pre_process _config rules = (rules, ())

  let post_process (config : Core_scan_config.t) ()
      (res : Core_result.processed_match) =
    Logs_.with_debug_trace ~__FUNCTION__
      begin
        fun () ->
          let processed_match_with_ignores, errors =
            Nosemgrep.produce_single_ignored res
          in
          let errors =
            (* TODO should this check be in call_with_pre_and_post_processor or
             * WrapSimpleProcessor? *)
            if config.strict then errors else []
          in
          ( (),
            {
              match_ = processed_match_with_ignores;
              errors = List.to_seq errors;
            } )
      end

  let handle_post_process_exn = default_handle_post_process_exn
end

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

(*****************************************************************************)
(* Global *)
(*****************************************************************************)

let hook_processor = ref (module No_Op_Processor : Processor)

let push_processor (module P : Processor) =
  let module Paired = MkPairProcessor (P) ((val !hook_processor)) in
  hook_processor := (module Paired)

let push_simple_processor (module SP : SimpleProcessor) =
  let module P = WrapSimpleProcessor (SP) in
  push_processor (module P)

(* In semgrep OSS we just run the nosemgrep and autofix processors. In Pro we
 * also add the secrets post processor (see Secrets.setup()).
 *)
let () =
  push_simple_processor (module Autofix_processor);
  push_simple_processor (module Nosemgrep_processor)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Written with scan_with_rules abstracted to allow reuse across
   semgrep and semgrep-pro
*)
let call_with_pre_and_post_processor fconfig
    (scan_with_rules : 'config core_scan_func_with_rules) :
    'config core_scan_func_with_rules =
 fun config ((rules, rule_errors), rules_parse_time) ->
  let module Processor = (val !hook_processor) in
  let rules', state =
    Logs_.with_debug_trace ~__FUNCTION__:"Pre_post_core_scan.pre_process"
      (fun () ->
        try Processor.pre_process (fconfig config) rules with
        | (Time_limit.Timeout _ | Common.UnixExit _) as e ->
            Exception.catch_and_reraise e
        | exn ->
            let e = Exception.catch exn in
            Logs.err (fun m ->
                m "Uncaught exn in Processor.pre_process: %s"
                  (Exception.to_string e));
            Exception.reraise e)
  in

  let (res : Core_result.t) =
    scan_with_rules config ((rules', rule_errors), rules_parse_time)
  in
  let (res : Core_result.t) =
    Logs_.with_debug_trace ~__FUNCTION__:"Pre_post_core_scan.post_process"
      (fun () ->
        try Processor.post_process (fconfig config) state res with
        | (Time_limit.Timeout _ | Common.UnixExit _) as e ->
            Exception.catch_and_reraise e
        | exn ->
            let e = Exception.catch exn in
            Logs.err (fun m ->
                m "Uncaught exn in Processor.post_process: %s"
                  (Exception.to_string e));
            Exception.reraise e)
  in
  res
(*****************************************************************************)
(* Test enablement code *)
(*****************************************************************************)

type test_only_processors = (module Processor)

let test_only_hook_processors = hook_processor

let test_only_processors_of_processor (module P : Processor) =
  (module P : Processor)

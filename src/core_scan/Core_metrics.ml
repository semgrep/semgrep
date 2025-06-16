(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
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
(* This module contains Opentelemetry metrics for the core engine. Please review
   Ometrics.ml before adding more metrics.

   I (@austin) like to abstract away the various metric interfaces to a module with
   a specific function or small set of functions that take semgrep specific
   types, instead of what the value we're recording e.g. say we want to record
   some info about our rules, make some instruments for each aspect of rules you
   want to measure, like number of rules, and then number of languages. The
   function to call these instruments should just be a function that takes in
   rules. This way it's trivial to add more metrics here without having to
   change interfaces or insert new code in core semgrep code *)
(*****************************************************************************)
(* Meters *)
(*****************************************************************************)
module Scan_input_meter = (val Ometrics.make_meter Ometrics.default_meter_meta)
(** [Scan_input_meter] measures various aspects of our scan inputs*)

(*****************************************************************************)
(* Instruments *)
(*****************************************************************************)
(* [Scan_inputs_*] are the instruments for measuring scan inputs. These
   instruments are used to record the number of rules, targets, target errors,
   and skipped targets during a scan. *)

module Scan_inputs_num_rules =
  (val Scan_input_meter.make_int_counter
         (Ometrics.make_instrument_meta ~name:"semgrep.scan.inputs.num_rules"
            ~description:"How many rules Semgrep is scanning with"
            ~unit_:"rules" ()))

module Scan_inputs_num_targets =
  (val Scan_input_meter.make_int_counter
         (Ometrics.make_instrument_meta ~name:"semgrep.scan.inputs.num_targets"
            ~description:"How many targets Semgrep is scanning" ~unit_:"targets"
            ()))

module Scan_inputs_num_target_errors =
  (val Scan_input_meter.make_int_counter
         (Ometrics.make_instrument_meta
            ~name:"semgrep.scan.inputs.num_target_errors"
            ~description:
              "How many target errors Semgrep had while discovering targets"
            ~unit_:"errors" ()))

(*****************************************************************************)
(* Entrypoints *)
(*****************************************************************************)

let meter_scan_inputs ~invalid_rules ~valid_rules ~targets ~errors ~skipped =
  let num_invalid_rules = List.length invalid_rules in
  let num_rules = List.length valid_rules in
  let num_targets = List.length targets in
  let num_errors = List.length errors in
  let num_skipped = List.length skipped in
  (* TODO? Maybe we want to move these constants elsewhere so we can reuse? *)
  let skipped_attrs skipped = [ ("skipped", `Bool skipped) ] in
  let valid_attrs valid = [ ("valid", `Bool valid) ] in
  Scan_inputs_num_rules.record ~attrs:(valid_attrs true) num_rules;
  Scan_inputs_num_rules.record ~attrs:(valid_attrs false) num_invalid_rules;
  Scan_inputs_num_targets.record ~attrs:(skipped_attrs false) num_targets;
  Scan_inputs_num_targets.record ~attrs:(skipped_attrs true) num_skipped;
  Scan_inputs_num_target_errors.record num_errors

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
(* Meters *)
(*****************************************************************************)
module Time_limit_meter = (val Ometrics.make_meter Ometrics.default_meter_meta)
(** [Time_limit_meter] measures various aspects of our time limits*)

(*****************************************************************************)
(* Instruments *)
(*****************************************************************************)
module Time_limit_counter =
  (val Time_limit_meter.make_int_counter
         (Ometrics.make_instrument_meta
            ~name:"semgrep_scan_timeouts"
              (* No unit here as we are just accumulating a  unit-less value *)
            ~description:
              "How many timeouts Semgrep has set (and possibly exceeded)"
            ()))

module Time_limit_duration =
  (val Time_limit_meter.make_int_counter
         (Ometrics.make_instrument_meta ~name:"semgrep_scan_timeouts_duration"
            ~description:"Specified durations for time limits" ~unit_:"ms" ()))

module Time_limit_overrun =
  (val Time_limit_meter.make_int_counter
         (Ometrics.make_instrument_meta ~name:"semgrep_scan_timeouts_overrun"
            ~description:
              "How much time Semgrep has spent beyond its time limits"
            ~unit_:"ms" ()))

(*****************************************************************************)
(* Recording *)
(*****************************************************************************)

let time_limit_trigger_table = SharedCounterTable.create_int_table 10
let time_limit_time_spent_table = SharedCounterTable.create_int_table 10
let time_limit_overrun_table = SharedCounterTable.create_int_table 10

let record_time_limit ~(info : Exception.timeout_info)
    ~(result_info : Exception.timeout_result_info) =
  let ms_of_s x = x *. 1000.0 in

  (* Don't care about more than ms *)
  let duration_ms = info.max_duration |> ms_of_s |> int_of_float in
  let exceeded = result_info.exceeded in
  let table_key = (info.name, exceeded) in
  let triggers =
    SharedCounterTable.add_and_fetch time_limit_trigger_table table_key 1
  in
  let total_durations =
    SharedCounterTable.add_and_fetch time_limit_time_spent_table table_key
      duration_ms
  in
  (* We record overrun instead of just actual_duration + total_duraton and
     calculating it later, as if we were to do that, then we would have to have
     a way to identify individual timeout instances across metrics. E.g. if
     there are 3 timeouts, and 2 of them underrun by 2 seconds and one overruns
     by 4, this would look like there was no overrun when we took the difference
     of actual_duration and max_duration. We can't individually identify the
     timeouts either as that might introduce cardinality blowup in metrics which
     is bad. It would also make querying really messy and difficult. *)
  let total_overrun =
    let overrun_ms =
      if exceeded then
        result_info.actual_duration -. info.max_duration
        |> ms_of_s |> int_of_float
      else 0
    in
    SharedCounterTable.add_and_fetch time_limit_overrun_table table_key
      overrun_ms
  in
  let attrs = [ ("time_limit_name", `String info.name) ] in
  let trigger_attrs = attrs @ [ ("exceeded", `Bool exceeded) ] in
  Time_limit_counter.record ~attrs:trigger_attrs triggers;
  Time_limit_duration.record ~attrs:trigger_attrs total_durations;
  Time_limit_overrun.record ~attrs total_overrun

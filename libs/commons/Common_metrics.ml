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
(* Useful generic meters, also examples *)
(*****************************************************************************)

(* Extensible type for adding errors that may not be exceptions *)
type metered_error = ..

let error_table = SharedCounterTable.create_int_table 10

module Error_meter = (val Ometrics.(make_meter default_meter_meta))

module Error_count =
  (val Error_meter.make_int_counter
         (Ometrics.make_instrument_meta ~name:"ocaml_exceptions"
            ~description:"How many errors a service has encountered" ()))

let meter_exception (e : exn) =
  let exn_type = Printexc.exn_slot_name e in
  let attrs = [ ("type", `String exn_type); ("kind", `String "exception") ] in
  let count = SharedCounterTable.add_and_fetch error_table attrs 1 in
  Error_count.record ~attrs count

let meter_error (e : metered_error) =
  let error_type = Obj.Extension_constructor.(e |> of_val |> name) in
  let attrs = [ ("type", `String error_type); ("kind", `String "error") ] in
  let count = SharedCounterTable.add_and_fetch error_table attrs 1 in
  Error_count.record ~attrs count

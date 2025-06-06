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

module Otel = Opentelemetry
open Telemetry

type instrument_meta = {
  name : string;
  description : string option;
  unit_ : string option;
}

let make_instrument_meta ~name ?description ?unit_ () : instrument_meta =
  { name; description; unit_ }

type meter_meta = {
  name : string option; (* Overrides global service name *)
  attrs : (string * user_data) list;
}

let default_meter_meta = { name = None; attrs = [] }

module type Meter_provider = sig end

module Simple_meter_provider : Meter_provider = struct end

module type Instrument = sig
  type value

  val record : ?attrs:(string * user_data) list -> value -> unit
  (** [record ~attrs x] will record AND EMIT some data point from the instrument*)

  val record_exemplar :
    ?attrs:(string * user_data) list ->
    ?filtered_attrs:(string * user_data) list ->
    value ->
    unit
  (** [record_exemplar ~attrs ~filtered_attrs x] will record an exemplar data
      point from the instrument. Exemplar data points are used to record
      additional information ([filtered_attrs]) about a data point, such as a
      stack trace or other context. Usually exemplar data points are outliers of
      some sort, not every data point *)
end

module Fake_instrument_int : Instrument with type value = int = struct
  type value = int

  let record ?(attrs = []) _ =
    ignore attrs;
    ()

  let record_exemplar ?(attrs = []) ?(filtered_attrs = []) _ =
    ignore attrs;
    ignore filtered_attrs;
    ()
end

module Fake_instrument_int64 : Instrument with type value = int64 = struct
  type value = int64

  let record ?(attrs = []) _ =
    ignore attrs;
    ()

  let record_exemplar ?(attrs = []) ?(filtered_attrs = []) _ =
    ignore attrs;
    ignore filtered_attrs;
    ()
end

module Fake_instrument_float : Instrument with type value = float = struct
  type value = float

  let record ?(attrs = []) _ =
    ignore attrs;
    ()

  let record_exemplar ?(attrs = []) ?(filtered_attrs = []) _ =
    ignore attrs;
    ignore filtered_attrs;
    ()
end

module type Meter = sig
  val make_int_counter :
    instrument_meta -> (module Instrument with type value = int)
  (** [make_int_counter meta] creates an instrument that measures strictly
      increasing integer values. This is useful for counters that increment over
      time, such as the number of requests received. *)

  val make_int64_counter :
    instrument_meta -> (module Instrument with type value = int64)
  (** [make_int64_counter meta] creates an instrument that measures strictly
      increasing large integer values. This is useful for large counters that
      increment over time, such as the number of bytes we've read. *)

  val make_float_counter :
    instrument_meta -> (module Instrument with type value = float)
  (** [make_float_counter meta] creates an instrument that measures increasing
      floating point values. This is useful for floating point counters that
      increment over time, such as fractional seconds we've spent on a type of
      operation *)

  val make_int_updown_counter :
    instrument_meta -> (module Instrument with type value = int)
  (** [make_int_updown_counter meta] creates an instrument that measures
      increasing or decreasing integer values. This is useful for counters that
      can go up and down, such as the number of active network connections *)

  val make_int64_updown_counter :
    instrument_meta -> (module Instrument with type value = int64)
  (** [make_int64_updown_counter meta] creates an instrument that measures
      increasing or decreasing large integer values. This is useful for large
      counters that can go up and down, such as how many bytes of memory we're
      using *)

  val make_float_updown_counter :
    instrument_meta -> (module Instrument with type value = float)
  (** [make_float_updown_counter meta] creates an instrument that measures
      increasing or decreasing floating point values. This is useful for
      floating point counters that can go up and down, such as how many
      fractional shares we're trying to sell *)

  val make_int_gauge :
    instrument_meta -> (module Instrument with type value = int)
  (** [make_int_gauge meta] creates an instrument that can record integer values
      that are not necessarily additive. This is useful for recording things
      like CPU fan speed *)

  val make_int64_gauge :
    instrument_meta -> (module Instrument with type value = int64)
  (** [make_int64_gauge meta] creates an instrument that can record large
      integer values that are not necessarily additive. This is useful for
      recording things like cpu fan speed measured at rotations per millenia *)

  val make_float_gauge :
    instrument_meta -> (module Instrument with type value = float)
  (** [make_float_gauge meta] creates an instrument that can record floating
      point values that are not necessarily additive. This is useful for
      recording things like CPU temperature in degrees Celsius *)

  (* TODO: histograms... *)

  (* TODO: async variants *)
end

module Fake_meter : Meter = struct
  let make_int_counter _ =
    (module Fake_instrument_int : Instrument with type value = int)

  let make_int64_counter _ =
    (module Fake_instrument_int64 : Instrument with type value = int64)

  let make_float_counter _ =
    (module Fake_instrument_float : Instrument with type value = float)

  let make_int_updown_counter _ =
    (module Fake_instrument_int : Instrument with type value = int)

  let make_int64_updown_counter _ =
    (module Fake_instrument_int64 : Instrument with type value = int64)

  let make_float_updown_counter _ =
    (module Fake_instrument_float : Instrument with type value = float)

  let make_int_gauge _ =
    (module Fake_instrument_int : Instrument with type value = int)

  let make_int64_gauge _ =
    (module Fake_instrument_int64 : Instrument with type value = int64)

  let make_float_gauge _ =
    (module Fake_instrument_float : Instrument with type value = float)
end

let make_meter ?(provider = (module Simple_meter_provider : Meter_provider))
    meta : (module Meter) =
  ignore provider;
  ignore meta;
  (module Fake_meter : Meter)

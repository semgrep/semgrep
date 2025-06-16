open Telemetry
(**
   ATTENTION!!!!!!!! READ THE FOLLOWING BEFORE ADDING METRICS SERIOUSLY!!! It is
   VERY easy to create metrics that will blow up our metrics backend, and you
   WILL make infra sad

   !!! METRICS DO NOT CURRENTLY WORK AFTER WE HAVE FORKED BUT HAVE NOT RETURNED

   This module adds suport for opentelemetry metrics! Use this if you want to
   record simple data en masse, e.g. ocaml gc statistics

   [^0] = footnote

   Otel metrics can be a bit confusing, so here is a short primer:
   A Meter Provider is just an entrypoint, don't worry about it unless you are
   doing something crazy

   A meter is responsible for creating instruments, and instruments record
   individual data points

   An instrument is a type of metric, e.g. a counter, gauge, etc. and can be
   used to record say how many allocations have happened in the course of a
   program. See the doc comments below for when to use what kind of
   instrument

   Instrument names are important, see
   https://prometheus.io/docs/practices/naming/ for a guide, but TL;DR; They
   will be named semgrep_<thing_measuring>_<units>. Note that our infra will
   automatically add the units to the metric name, so no need to do that.

   A meter is just an organizational tool for instruments. You use them to apply
   labels/attributes to any data point recorded by an instrument in that meter.
   In general you should have a meter for each component of your application,
   say one for GC metrics, another for file targeting, a third for parsing etc.

   An attribute/label, which we will refer to as a label, is a way to collate
   data. What's important here is that labels should be thought of as a way to
   create different time series, not a way to record arbitrary data about a data
   point.

   For example if you are recording how many files you've scanned, a useful
   label may be the language, so you can have a timeseries of how many files
   we've scanned for each language. You would NOT want a label with the file
   name, directory, or project that you scanned, since that's not a helpful way
   to present a time series.

   Getting this important is critical for having correct metrics, as each label
   combination is a different time series, and most metrics you add will be
   cumulative. What this means is that if you have a metric recording GC
   allocations, and no labels, and one process reports 5 allocations, and the
   next 10 allocations, the metric will be 10 NOT 15. If you add a label "run1"
   to the first run and "run2" then these will be 2 separate time series, and
   the metric will report 15 [^1]. By default we will always add the trace id to
   the metric value, so this latter behavior will be the default.

   Getting this right is also important because of how metrics are stored. Each
   time series uses a lot of storage (relative to a data point). So high
   cardinality labels, such as project or file names, will create a lot of time
   series take up a dumb amount of storage. The exception here are high
   cardinality names that are ephemeral and we will only ever see once for a
   short period of time, e.g. a scan id. The backend will garbage collect labels
   that aren't used for a long time, and so we will not spend a lot of storage
   on them long term.

   See https://prometheus.io/docs/practices/naming/#labels for recommendations
   on label names.

   If you have any questions on if a label is ok, please ask!

   [^1] unless you use cumulative metrics, but that's a whole different thing,
   and is not possible right now.

*)

type instrument_meta = {
  name : string;
      (** Name of the instrument. Please see module doc comment for naming
          conventions *)
  description : string option;
  unit_ : string option;
}
(** [instrument_meta] just provides basic info about an instrument  *)

val make_instrument_meta :
  name:string -> ?description:string -> ?unit_:string -> unit -> instrument_meta
(** [make_instrument_meta ~name:"number_of_things" ~description:"records a
    number" ~unit_:"things" ()] creates an [instrument_meta] with the given
    name, description, and unit. See top level comment about naming
    instruments! *)

type meter_meta = {
  name : string option;  (** Overrides global service name *)
  attrs : (string * user_data) list;
      (** Attributes to apply to all instruments in this meter *)
}
(** [meter_meta] will override the service name if set, and attrs will apply
    labels to all data points recorded by any instruments made by this meter.
    For example you may want to record the number of some operation, and then
    always apply a label about which component it happened in *)

val default_meter_meta : meter_meta
(** [default_meter_meta] is the default metadata for a meter, which will use the
    global service name and no attributes. *)

(* https://opentelemetry.io/docs/specs/otel/metrics/api/#meterprovider *)
module type Meter_provider

module Simple_meter_provider : Meter_provider
(** A normal meter provider*)

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
      point from the instrument. Exemplar data points record additional
      information ([filtered_attrs]) about a data point, such as a stack trace
      or other context. Usually exemplar data points are outliers of some sort,
      not every data point *)
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

val make_meter :
  ?provider:(module Meter_provider) -> meter_meta -> (module Meter)
(** [make_meter ~provider:Simple_meter_provider meta] creates a meter with the
    given metadata. See [meter_meta] for what that may be. If no provider is
    given, it will use the default [Simple_meter_provider]. *)

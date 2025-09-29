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
module Timestamp_ns = Otel.Timestamp_ns
open Telemetry

open Otel_util
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

(* TODO: Upstream pretty much this entire module to the opentelemetry
   library *)
(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* https://opentelemetry.io/docs/specs/otel/metrics/api/#instrument *)
type instrument_meta = {
  name : string;
  description : string option;
  unit_ : string option;
}
(* should be self explanatory :). Just basic info *)

(* https://opentelemetry.io/docs/specs/otel/metrics/api/#get-a-meter *)
type meter_meta = {
  name : string option; (* Overrides global service name *)
  attrs : (string * user_data) list;
      (* labels applied to ALL data points recorded by any instruments made by
         this meter. an example may be: a Parsing meter that labels all gc
         metrics as parsing *)
}

(* handy type alias*)
type exemplar = Otel.Proto.Metrics.exemplar

(* Possible types for a metric value. This just maps to Otel.Proto.Metrics.As_* and is a bit nicer *)
type metric_value = Otel.Proto.Metrics.number_data_point_value
type exemplar_value = Otel.Proto.Metrics.exemplar_value

(* Needed so we can keep track of our histogram data *)
type histogram_data = {
  (* a list of buckets as described in
     https://opentelemetry.io/docs/specs/otel/metrics/sdk/#explicit-bucket-histogram-aggregation,
     with the second item in the tuple being a count for that bucket *)
  bounds_and_buckets : (float * int64) list option;
  (* We could derive the below from the above, but let's keep track of it
     separately for perf reasons I guess *)
  count : int64;
  (* Similar here for sum and max. Let's just be explicit for perf reasons *)
  sum : float option;
  min_max : (float * float) option;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* We copy these conv value functions from the otel library (opentelemetry.ml)
   so we can wrap the metrics emit functions in a nicer way. When we upstream
   this we could just make the types nicer there *)
let _conv_value =
  let open Otel.Proto.Common in
  function
  | `Int i -> Some (Int_value (Int64.of_int i))
  | `String s -> Some (String_value s)
  | `Bool b -> Some (Bool_value b)
  | `Float f -> Some (Double_value f)
  | `None -> None

let _conv_key_value (k, v) =
  let open Otel.Proto.Common in
  let value = _conv_value v in
  default_key_value ~key:k ~value ()

let _value_conv =
  let open Otel.Proto.Common in
  function
  | Some (Int_value i) -> `Int (Int64.to_int i)
  | Some (String_value s) -> `String s
  | Some (Bool_value b) -> `Bool b
  | Some (Double_value f) -> `Float f
  | Some (Array_value _)
  | Some (Bytes_value _)
  | Some (Kvlist_value _)
  | None ->
      `None

let _key_value_conv kv =
  let open Otel.Proto.Common in
  (kv.key, _value_conv kv.value)

let now = Timestamp_ns.now_unix_ns
let _program_start = now ()

(* nice conversion functions *)
let make_instrument_meta ~name ?description ?unit_ () : instrument_meta =
  { name; description; unit_ }

let default_meter_meta =
  {
    name = None;
    (* use global service name *)
    attrs = [];
  }

let create_histogram_data (explicit_bounds : float list option) =
  let bounds_and_buckets =
    explicit_bounds
    |> Option.map (fun bounds ->
           let bounds_and_buckets =
             List_.map (fun bound -> (bound, 0L)) bounds
           in
           (* We add infinity so there's an upper bound, which makes insertion a
              bit easier *)
           let bounds_and_buckets_with_inf =
             bounds_and_buckets @ [ (Float.infinity, 0L) ]
           in
           (* Sort bounds/buckets so it's easy to insert *)
           List.sort
             (fun (bound_a, _count_a) (bound_b, _count_b) ->
               Float.compare bound_a bound_b)
             bounds_and_buckets_with_inf)
  in
  { bounds_and_buckets; count = 0L; sum = None; min_max = None }

let metric_value_of_float (f : float) : metric_value = As_double f
let metric_value_of_int (i : int) : metric_value = As_int (Int64.of_int i)
let metric_value_of_int64 (i : int64) : metric_value = As_int i

(* Same as Otel.Metrics.float but with exemplars *)
let number_datapoint_of_metric_value ?(start_time_unix_nano = _program_start)
    ?(now = now ()) ?exemplars ?(attributes = []) ?flags (value : metric_value)
    : Otel.Proto.Metrics.number_data_point =
  Otel.Proto.Metrics.default_number_data_point ~start_time_unix_nano
    ~time_unix_nano:now ?exemplars ?flags ~attributes ~value ()

let explicit_bounds_of_histogram (histogram : histogram_data) =
  match histogram.bounds_and_buckets with
  | None -> None
  | Some bounds_and_buckets ->
      let explicit_bounds =
        bounds_and_buckets |> List_.map fst
        (* we don't want to include infinite in our bounds, but it is there for convenience *)
        (* See otel link above bounds_and_buckets in histogram_data for more info *)
        |> List.filter (fun b -> not (Float.is_infinite b))
      in
      Some explicit_bounds

let buckets_of_histogram (histogram : histogram_data) =
  match histogram.bounds_and_buckets with
  | None -> []
  | Some bounds_and_buckets -> List_.map snd bounds_and_buckets

(* See otel link above bounds_and_buckets in histogram_data for more info *)
let increment_bucket_count (value : float)
    (bounds_and_buckets : (float * int64) list) : (float * int64) list =
  let rec aux acc = function
    | [] -> List.rev acc
    | (bound, count) :: rest when value <= bound ->
        List.rev ((bound, Int64.succ count) :: acc) @ rest
    | (bound, count) :: rest -> aux ((bound, count) :: acc) rest
  in
  aux [] bounds_and_buckets

(* Add a value to a histogram data, updating the min, max, and sum as needed *)
let add_value_to_histogram_data (histogram : histogram_data) value =
  let bounds_and_buckets =
    Option.map (increment_bucket_count value) histogram.bounds_and_buckets
  in
  let min_max =
    match histogram.min_max with
    | None -> Some (value, value) (* first value is both min and max *)
    | Some (min, max) when value < min -> Some (value, max) (* new min *)
    | Some (min, max) when value > max -> Some (min, value) (* new max *)
    | Some min_max -> Some min_max
  in
  let count = Int64.succ histogram.count in
  let sum =
    match histogram.sum with
    | None -> Some value (* first value is the sum *)
    | Some s -> Some (s +. value)
    (* add to the sum *)
  in
  { bounds_and_buckets; count; sum; min_max }

let histogram_datapoint_of_histogram_data
    ?(start_time_unix_nano = _program_start) ?(now = now ()) ?exemplars
    ?(attributes = []) ?flags histogram_data :
    Otel.Proto.Metrics.histogram_data_point =
  let count = histogram_data.count in
  let sum = histogram_data.sum in
  let bucket_counts = buckets_of_histogram histogram_data in
  let explicit_bounds = explicit_bounds_of_histogram histogram_data in
  let min, max =
    match histogram_data.min_max with
    | None -> (None, None)
    | Some (min, max) -> (Some min, Some max)
  in
  Otel.Proto.Metrics.default_histogram_data_point ~start_time_unix_nano
    ~time_unix_nano:now ~count ~sum ~bucket_counts ?explicit_bounds ?exemplars
    ?flags ~attributes ~min ~max ()

let exemplar_value_of_metric_value (value : metric_value) : exemplar_value =
  match value with
  | As_double f -> Otel.Proto.Metrics.As_double f
  | As_int i -> Otel.Proto.Metrics.As_int i

let exemplar_of_metric_value ?(now = now ()) ?filtered_attrs value =
  let value = exemplar_value_of_metric_value value in
  let current_scope = Otel.Scope.get_ambient_scope () in
  let trace_id =
    Option.map
      (fun (scope : Otel.Scope.t) -> scope.trace_id |> Otel.Trace_id.to_bytes)
      current_scope
  in
  let span_id =
    Option.map
      (fun (scope : Otel.Scope.t) -> scope.span_id |> Otel.Span_id.to_bytes)
      current_scope
  in
  let filtered_attributes =
    Option.map (fun xs -> List_.map _conv_key_value xs) filtered_attrs
  in
  Otel.Proto.Metrics.default_exemplar ?filtered_attributes ?span_id ?trace_id
    ~time_unix_nano:now ~value ()

(*****************************************************************************)
(* Defaults *)
(*****************************************************************************)

(* NOTE: Some resource/global attributes have special handling and are
   automatically added to all metrics:
   https://opentelemetry.io/docs/specs/otel/compatibility/prometheus_and_openmetrics/#resource-attributes-1 *)
(* labels added to ALL metrics, see the prelude comment for why *)
(* WARNING: DO NOT ADD ANYTHING HERE WITHOUT SERIOUS!!!!!! CONSIDERATION AND
   CONVERSATION WITH OTHERS. You should not need to add anything here, you
   probably want to add it to the meter meta or pass the label when recording
   the data point *)
let default_metric_attributes () =
  let trace_id =
    match Telemetry.get_current_scope () with
    | Some scope -> Telemetry.show_scope scope
    | None -> "local_run"
  in
  let default_attr_keys =
    [
      (* Semgrep version *)
      Telemetry.Attributes.version;
      (* e.g. prod, dev2, staging *)
      Telemetry.Attributes.deployment_environment_name;
      (* pro or oss engine *)
      Telemetry.Attributes.scan_engine;
      (* if it was from SMS *)
      Telemetry.Attributes.scan_source;
      (* If it's an sms experiment *)
      Telemetry.Attributes.experiment_name;
      (* If we're running with eio or not *)
      Telemetry.Attributes.eio;
      (* e.g. console, intuit, etc. *)
      Otel.Conventions.Attributes.Service.namespace;
    ]
  in
  let default_attrs = [ ("trace_id", `String trace_id) ] in
  find_global_attrs default_attr_keys @ default_attrs

(*****************************************************************************)
(* OTel SDK *)
(*****************************************************************************)
(* https://opentelemetry.io/docs/specs/otel/metrics/api/#meterprovider *)
module type Meter_provider = sig
  val emit :
    ?service_name:string ->
    ?attrs:(string * user_data) list ->
    Otel.Metrics.t list ->
    unit
end

(* Normal meter provider, shouldn't need anything else unless we get weird *)
module Simple_meter_provider : Meter_provider = struct
  (* Basically just copypasta of Otel.Metrics.emit but with service_name pass
     through *)
  let emit ?service_name ?attrs l =
    let rm = Otel.Metrics.make_resource_metrics ?service_name ?attrs l in
    (* TODO: Maybe add a debug log statement to ret *)
    Otel.Collector.send_metrics [ rm ] ~ret:(fun () -> ())
end

(* an instrument kind is just a type for an instrument. An instrument can be
   float or int, monotonic or not, a sum/guage/histogram, and delta aggregate or
   not. This is nice to have so you can just pass straight ints or floats to the
   final record function, instead of an (As_double x) or something. It also
   wraps the data point types, number data point (for sum or guage) vs histogram
   vs exponential. *)
module type Instrument_kind = sig
  type data_point
  type value

  val make_data_point :
    ?start_time_unix_nano:int64 ->
    ?now:int64 ->
    ?exemplars:exemplar list ->
    ?attributes:Otel.Proto.Common.key_value list ->
    ?flags:int32 ->
    value ->
    data_point

  val make_exemplar :
    ?now:int64 -> ?filtered_attrs:(string * user_data) list -> value -> exemplar

  val report_data_points :
    name:string ->
    ?description:string ->
    ?unit_:string ->
    data_point list ->
    Otel.Metrics.t
end

(* a guage can record non additive values, think CPU fan speed *)
let make_gauge_kind (type a) (metric_value_of_value : a -> metric_value) :
    (module Instrument_kind with type value = a) =
  (module struct
    type data_point = Otel.Proto.Metrics.number_data_point
    type value = a

    let make_data_point ?start_time_unix_nano ?now ?exemplars ?attributes ?flags
        v =
      number_datapoint_of_metric_value ?start_time_unix_nano ?now ?exemplars
        ?attributes ?flags (metric_value_of_value v)

    let make_exemplar ?now ?filtered_attrs v =
      exemplar_of_metric_value ?now ?filtered_attrs (metric_value_of_value v)

    let report_data_points = Otel.Metrics.gauge
  end)

(* a sum records additive values, e.g. number of gc allocations, and can be
   delta aggregation, e.g. each data point reports the change in the value, or
   it's not, and so reports the underlying value. is_monotonic means the value strictcly increases*)
let make_sum_kind (type a) ?is_monotonic
    (metric_value_of_value : a -> metric_value) :
    (module Instrument_kind with type value = a) =
  (* TODO: We cannot use delta aggregation as we load balance our metric endpoints,
     leading to the metrics to be split in half, as the endpoint is what keeps the running total *)
  let delta_aggregation = false in
  let aggregation_temporality =
    if delta_aggregation then Otel.Metrics.Aggregation_temporality_delta
    else Otel.Metrics.Aggregation_temporality_cumulative
  in
  (module struct
    type data_point = Otel.Proto.Metrics.number_data_point
    type value = a

    let make_data_point ?start_time_unix_nano ?now ?exemplars ?attributes ?flags
        v =
      number_datapoint_of_metric_value ?start_time_unix_nano ?now ?exemplars
        ?attributes ?flags (metric_value_of_value v)

    let make_exemplar ?now ?filtered_attrs v =
      exemplar_of_metric_value ?now ?filtered_attrs (metric_value_of_value v)

    let report_data_points =
      Otel.Metrics.sum ~aggregation_temporality ?is_monotonic
  end)

(* default boundaries as described by the otel spec:
   https://opentelemetry.io/docs/specs/otel/metrics/sdk/#histogram-aggregations
 *)
let default_boundaries =
  [
    0.0;
    5.0;
    10.0;
    25.0;
    50.0;
    75.0;
    100.0;
    250.0;
    500.0;
    750.0;
    1000.0;
    2500.0;
    5000.0;
    7500.0;
    10000.0;
  ]

(* TODO: base2 exponential bucket histogram:
   https://opentelemetry.io/docs/specs/otel/metrics/sdk/#base2-exponential-bucket-histogram-aggregation
 *)
(* A histogram records data about a population of recorded measurements. It's
   useful for when you have a high cardinality + size population, and so want to
   avoid creating a label for each possible type. A bucket will provide the
   counts per bucket, and then a total sum and count of the population.
   Optionally it will also record the min and max.


   Boundaries define the buckets into which data points are recorded, and
   buckets are lower bound exclusive, and upper bound inclusive. A histogram
   without buckets will only record sum, count, and optionally min/max of the
   entire population.

   An example may be wanting to record the distribution of file sizes. We
   wouldn't want to create a new metric for each possible size, but we do want
   to see the shape of the population, so histograms are a good fit.
 *)
let make_histogram_kind ?(explicit_boundaries = Some default_boundaries) () :
    (module Instrument_kind with type value = float list) =
  (* TODO: We cannot use delta aggregation as we load balance our metric endpoints,
     leading to the metrics to be split in half, as the endpoint is what keeps the running total *)
  let delta_aggregation = false in
  let aggregation_temporality =
    if delta_aggregation then Otel.Metrics.Aggregation_temporality_delta
    else Otel.Metrics.Aggregation_temporality_cumulative
  in
  (module struct
    type data_point = Otel.Proto.Metrics.histogram_data_point
    type value = float list

    let make_data_point ?start_time_unix_nano ?now ?exemplars ?attributes ?flags
        value =
      (* Create a default empty histogram *)
      (* THINK: Do we want to allow people to change bounds after creating the
         metric? Otel spec wasn't clear to me *)
      let empty_histogram = create_histogram_data explicit_boundaries in
      let histogram_data =
        List.fold_left add_value_to_histogram_data empty_histogram value
      in
      histogram_datapoint_of_histogram_data ?start_time_unix_nano ?now
        ?exemplars ?attributes ?flags histogram_data

    let make_exemplar ?now ?filtered_attrs v =
      (* exemplars for histograms will be a little broken because of this :/ But
         they're already experimental so *)
      let v =
        match v with
        | [] -> 0.0 (* no values, use 0.0 as the exemplar value *)
        | v :: _ -> v
      in
      exemplar_of_metric_value ?now ?filtered_attrs (metric_value_of_float v)

    let report_data_points = Otel.Metrics.histogram ~aggregation_temporality
  end)

(* https://opentelemetry.io/docs/specs/otel/metrics/api/#instrument *)
(* TODO: async option *)
module type Instrument = sig
  type value

  val record : ?attrs:(string * user_data) list -> value -> unit

  val record_exemplar :
    ?attrs:(string * user_data) list ->
    ?filtered_attrs:(string * user_data) list ->
    value ->
    unit
end

(* https://opentelemetry.io/docs/specs/otel/metrics/api/#meter *)
(* Meter has some metadata, and then provides a way to make all the diff
   instruments *)
module type Meter = sig
  (* https://opentelemetry.io/docs/specs/otel/metrics/api/#counter *)
  val make_int_counter :
    instrument_meta -> (module Instrument with type value = int)

  (* https://opentelemetry.io/docs/specs/otel/metrics/api/#counter *)
  val make_int64_counter :
    instrument_meta -> (module Instrument with type value = int64)

  (* https://opentelemetry.io/docs/specs/otel/metrics/api/#counter *)
  val make_float_counter :
    instrument_meta -> (module Instrument with type value = float)

  (* https://opentelemetry.io/docs/specs/otel/metrics/api/#updowncounter *)
  val make_int_updown_counter :
    instrument_meta -> (module Instrument with type value = int)

  (* https://opentelemetry.io/docs/specs/otel/metrics/api/#updowncounter *)
  val make_int64_updown_counter :
    instrument_meta -> (module Instrument with type value = int64)

  (* https://opentelemetry.io/docs/specs/otel/metrics/api/#updowncounter *)
  val make_float_updown_counter :
    instrument_meta -> (module Instrument with type value = float)

  (* https://opentelemetry.io/docs/specs/otel/metrics/api/#gauge *)
  val make_int_gauge :
    instrument_meta -> (module Instrument with type value = int)

  (* https://opentelemetry.io/docs/specs/otel/metrics/api/#gauge *)
  val make_int64_gauge :
    instrument_meta -> (module Instrument with type value = int64)

  (* https://opentelemetry.io/docs/specs/otel/metrics/api/#gauge *)
  val make_float_gauge :
    instrument_meta -> (module Instrument with type value = float)
  (* TODO: histograms... *)

  val make_histogram :
    ?explicit_bounds:float list option ->
    instrument_meta ->
    (module Instrument with type value = float list)
  (* TODO: async variants *)
end

module Make_meter
    (P : Meter_provider)
    (M : sig
      val meta : meter_meta
    end) : Meter = struct
  let meter_meta = M.meta

  module Make_instrument
      (K : Instrument_kind)
      (M : sig
        val instrument_meta : instrument_meta
      end) : Instrument with type value = K.value = struct
    type value = K.value

    let instrument_meta = M.instrument_meta

    let report_data_points =
      K.report_data_points ~name:instrument_meta.name
        ?description:instrument_meta.description ?unit_:instrument_meta.unit_

    let emit ?exemplar ?(attrs = []) ?now (x : value) : unit =
      (* Always include some attrs on all metrics such as semgrep's version. See module level comment for
         why *)
      let attrs = attrs @ default_metric_attributes () in
      let data_point =
        let exemplars = Option.map (fun x -> [ x ]) exemplar in
        let attributes = List_.map _conv_key_value attrs in
        K.make_data_point ?now ?exemplars ~attributes x
      in
      let metric = report_data_points [ data_point ] in
      P.emit ?service_name:meter_meta.name ~attrs:meter_meta.attrs [ metric ]

    let record ?(attrs = []) (x : value) : unit = x |> emit ~attrs

    let record_exemplar ?attrs ?filtered_attrs (value : value) : unit =
      let now = now () in
      let exemplar = K.make_exemplar ~now ?filtered_attrs value in
      emit ?attrs ~exemplar ~now value
  end

  let make_instrument (type a)
      (kind : (module Instrument_kind with type value = a)) instrument_meta :
      (module Instrument with type value = a) =
    (module Make_instrument
              ((val kind))
              (struct
                let instrument_meta = instrument_meta
              end))

  let make_base_sum ~is_monotonic metric_value_of =
    make_instrument (make_sum_kind ~is_monotonic metric_value_of)

  let make_int_counter = make_base_sum ~is_monotonic:true metric_value_of_int

  let make_int64_counter =
    make_base_sum ~is_monotonic:true metric_value_of_int64

  let make_float_counter =
    make_base_sum ~is_monotonic:true metric_value_of_float

  let make_int_updown_counter =
    make_base_sum ~is_monotonic:false metric_value_of_int

  let make_int64_updown_counter =
    make_base_sum ~is_monotonic:false metric_value_of_int64

  let make_float_updown_counter =
    make_base_sum ~is_monotonic:false metric_value_of_float

  let make_base_gauge metric_value_of =
    make_instrument (make_gauge_kind metric_value_of)

  let make_int_gauge = make_base_gauge metric_value_of_int
  let make_int64_gauge = make_base_gauge metric_value_of_int64
  let make_float_gauge = make_base_gauge metric_value_of_float

  let make_histogram ?explicit_bounds =
    make_instrument
      (make_histogram_kind ?explicit_boundaries:explicit_bounds ())
end

let make_meter ?(provider = (module Simple_meter_provider : Meter_provider))
    meta : (module Meter) =
  (module Make_meter
            ((val provider))
            (struct
              let meta = meta
            end))

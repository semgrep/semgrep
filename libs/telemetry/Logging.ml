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

(* Commentary *)

module Otel = Opentelemetry
module Log = Log_commons.Log
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module is for sending logs via telemetry. It is NOT a general logging
   library (See Logs for that). Here we provide basically some things so we can
   convert + send logs from our Logs library via OTel. *)
(*****************************************************************************)
(* Levels *)
(*****************************************************************************)
(* Convert log level to Otel severity *)
let log_level_to_severity (level : Logs.level) : Otel.Logs.severity =
  match level with
  (* Is there a better option than unspecified? Maybe info, and make info info2? *)
  | Logs.App -> Otel.Logs.Severity_number_unspecified
  | Logs.Info -> Otel.Logs.Severity_number_info
  | Logs.Error -> Otel.Logs.Severity_number_error
  | Logs.Warning -> Otel.Logs.Severity_number_warn
  | Logs.Debug -> Otel.Logs.Severity_number_debug

(*****************************************************************************)
(* Logging *)
(*****************************************************************************)
(* TODO: upstream almost all of this into the otel library*)

(* Log a message to otel with some attrs *)
let log ?(attrs = []) ~level msg =
  (* Not sure why this is picked up by this rule...*)
  (* nosemgrep: no-logs-in-library *)
  let log_level = Logs.level_to_string (Some level) in
  (* Let's just grab the current span_id and trace_id here for now, instead of
     as params since they're the otel kind, and it'd be a bit annoying to
     convert between otrace and otel ids *)
  let current_scope = Otel.Scope.get_ambient_scope () in
  let span_id =
    current_scope |> Option.map (fun (scope : Otel.Scope.t) -> scope.span_id)
  in
  let trace_id =
    current_scope |> Option.map (fun (scope : Otel.Scope.t) -> scope.trace_id)
  in
  let severity = log_level_to_severity level in
  let log = Otel.Logs.make_str ~severity ~log_level ?trace_id ?span_id msg in
  (* Noop if no backend is set *)
  Otel.Logs.emit ~attrs [ log ]

let no_telemetry_tag = Logs_.create_tag "no_telemetry"
let no_telemetry_tag_set = Logs_.create_tag_set [ no_telemetry_tag ]

let otel_reporter : Logs.reporter =
  let report src level ~over k msgf =
    msgf (fun ?header ?(tags : Logs.Tag.set option) fmt ->
        let k _ =
          over ();
          k ()
        in
        Format.kasprintf
          (fun msg ->
            let tags = tags ||| Logs_.(create_tag_set []) in
            let attrs =
              let tags =
                (* This looks weird but is the easiest way to print log tags *)
                Logs.Tag.fold
                  (fun (tag : Logs.Tag.t) acc ->
                    let s = Format.asprintf "%a" Logs.Tag.pp tag in
                    s :: acc)
                  tags []
                |> [%to_yojson: string list] |> Yojson.Safe.to_string
              in
              let src_str = Logs.Src.name src in
              [
                (* Worth sending header?  *)
                ("header", `String (Option.value ~default:"" header));
                ("tags", `String tags);
                ("src", `String src_str);
                ("message", `String msg);
              ]
            in
            let do_not_emit = Logs.Tag.mem no_telemetry_tag tags in
            (match level with
            (* Let's not send debug logs for now, as they can be expensive and
               and we're not sure of the usefulness *)
            (* COUPLING: we do something similar in tracing.py. If we want to
               enable sending debug logs here we probably want to send them from
               pysemgrep too! *)
            | Logs.Debug -> ()
            (* Let's allow users to tag their logs when they don't want them
               emitted. This could be because they're in the GC alarm, or
               because they log info we don't want to leave the machine *)
            | _ when do_not_emit -> ()
            | _ -> log ~attrs ~level msg);
            k ())
          fmt)
  in
  { Logs.report }

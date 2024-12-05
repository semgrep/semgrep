(* Austin Theriault
 *
 * Copyright (C) 2019-2024 Semgrep, Inc.
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

(* See libs/tracing/unix/Tracing.ml. This is the virtual module to allow
   JS to build without requiring curl to be installed *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type span = int64 [@@deriving show]
type user_data = Trace_core.user_data

type config = {
  endpoint : Uri.t;
  (* set the deployment environment name for organization reasons in APMs *)
  env : string option;
  (* To add data to our opentelemetry top span, so easier to filter *)
  top_level_span : span option;
}
[@@deriving show]

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)
module Attributes = struct
  let version = "version"
  let instance_id = "instance_id"
  let deployment_environment_name = "deployment.environment.name"
end
(*****************************************************************************)
(* Levels *)
(*****************************************************************************)

type level =
  | Info  (** Enable standard tracing (default level) *)
  | Debug  (** Enable commonly used debug tracing *)
  | Trace  (** Enable everything *)

let show_level = function
  | Info -> "Info"
  | Debug -> "Debug"
  | Trace -> "Trace"

let level_to_trace_level level =
  match level with
  | Info -> Trace_core.Level.Info
  | Debug -> Trace_core.Level.Debug1
  | Trace -> Trace_core.Level.Trace

(*****************************************************************************)
(* Code *)
(*****************************************************************************)
let enter_span ?(level = Info) =
  let level = level_to_trace_level level in
  Trace_core.enter_span ~level

let exit_span = Trace_core.exit_span

let with_span ?(level = Info) =
  let level = level_to_trace_level level in
  Trace_core.with_span ~level

let add_data_to_span (_i : span) (_data : (string * Trace_core.user_data) list)
    =
  ()

let add_data (_data : (string * Trace_core.user_data) list) (_i : config option)
    =
  ()

let add_global_attribute _key _value = ()

let trace_data_only ?(level = Info) ~__FUNCTION__ ~__FILE__ ~__LINE__ _name
    (_f : unit -> (string * Yojson.Safe.t) list) =
  ignore level;
  ()

let no_telemetry_tag = Logs_.create_tag "no_telemetry"
let no_telemetry_tag_set = Logs_.create_tag_set [ no_telemetry_tag ]
let otel_reporter : Logs.reporter = Logs.nop_reporter
(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

let stop_tracing ~exit_active_spans:_ () = ()
let restart_tracing () = ()

let configure_tracing ?(attrs = []) (_service_name : string) (_endpoint : Uri.t)
    =
  ignore attrs;
  ()

let with_tracing (_fname : string)
    (_data : (string * Trace_core.user_data) list) f =
  f 0L

let with_tracing_paused f = f ()

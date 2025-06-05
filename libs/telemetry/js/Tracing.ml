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
module Otel = Opentelemetry

type scope = Otel.Scope.t
type user_data = Otel.value

let empty_span =
  Otel.Scope.make
    ~trace_id:Otel.Trace_id.(create ())
    ~span_id:Otel.Span_id.(create ())
    ()

let show_scope (sp : scope) =
  ignore sp;
  "span"

let pp_scope fmt (sp : scope) = Format.fprintf fmt "%s" (show_scope sp)

type config = {
  endpoint : Uri.t;
  env : string option;
  top_level_scope : scope option;
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

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let with_span ?(level = Info) ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data
    (_name : string) (f : scope -> 'a) =
  ignore level;
  ignore data;
  f empty_span

let get_current_scope () = None
let record_exn _sp _exn _bt = ()
let record_exn_curr_span _exn _bt = ()
let add_data_to_span (_i : scope) (_data : (string * user_data) list) = ()
let add_data (_data : (string * user_data) list) (_i : config option) = ()
let add_global_attribute _key _value = ()
let no_telemetry_tag = Logs_.create_tag "no_telemetry"
let no_telemetry_tag_set = Logs_.create_tag_set [ no_telemetry_tag ]
let otel_reporter : Logs.reporter = Logs.nop_reporter
(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

let stop_tracing () = ()
let restart_tracing () = ()

let configure_tracing ?(attrs = []) (_service_name : string) (_endpoint : Uri.t)
    =
  ignore attrs;
  ()

let with_tracing (_fname : string) (_data : (string * user_data) list) f =
  f empty_span

let with_tracing_paused f = f ()

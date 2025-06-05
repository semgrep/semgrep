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
open Telemetry

let empty_span =
  Otel.Scope.make
    ~trace_id:Otel.Trace_id.(create ())
    ~span_id:Otel.Span_id.(create ())
    ()

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

let record_exn _sp _exn _bt = ()
let record_exn_curr_span _exn _bt = ()
let add_data_to_span (_i : scope) (_data : (string * user_data) list) = ()
let add_data (_data : (string * user_data) list) (_i : config option) = ()
let add_global_attribute _key _value = ()

let with_tracing (_fname : string) (_data : (string * user_data) list) f =
  f empty_span

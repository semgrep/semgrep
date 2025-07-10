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
module Log = Log_commons.Log
open Common
open Otel_util
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module includes any functions/types that are used for telemetry, so OTel
   scopes, attributes, and more. This module is basically just a lot of setup
   stuff for OTel and some shared types*)
(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type scope = Otel.Scope.t

(* coupling: added to every metric in Ometrics.ml, so cannot contain spaces or
   special chars etc *)
let show_scope (sp : scope) = sp.trace_id |> Otel.Trace_id.to_hex
let pp_scope fmt (sp : scope) = Format.fprintf fmt "%s" (show_scope sp)

type user_data = Otel.value

let show_user_data (ud : user_data) =
  match ud with
  | `String s -> Format.sprintf "`String %s" s
  | `Int i -> Format.sprintf "`Int %d" i
  | `Float f -> Format.sprintf "`Float %f" f
  | `Bool b -> Format.sprintf "`Bool %b" b
  | `None -> "`None"

let pp_user_data fmt (ud : user_data) =
  Format.fprintf fmt "%s" (show_user_data ud)

type config = {
  endpoint : Uri.t;
  env : string option;
  (* To add data to our opentelemetry top level span, so easier to filter *)
  top_level_scope : scope option;
}
[@@deriving show]

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)
(* The endpoint that otel traces will be sent to. This should only ever be set
   in configure_otel, which is called once, at the beginning. The ref isn't
   nice, but we need it to start and stop tracing without having to pass around
   an env. See [with_otel_paused]

   TODO(SAF-1938): This is a Domain-local value in order to more closely match
   with ParMap (which re-creates its own endpoint after forking in order to
   pull random seeds - see [restart_otel]).  Once we are using multicore by
   default, we should revisit this.
   *)
let active_endpoint = Domain.DLS.new_key (const None)

(* Service related attributes *)
module Attributes = struct
  open Opentelemetry.Conventions

  let version = Attributes.Service.version
  let instance_id = Attributes.Service.instance_id
  let deployment_environment_name = "deployment.environment.name"
  let vcs_ref_head_revision = "vcs.ref.head.revision"
  let vcs_ref_head_name = "vcs.ref.head.name"

  (* These are semgrep specific and technically shouldn't be in this library but
     these will be applied to all metrics *)
  let scan_engine = "scan.engine"
  let scan_source = "scan.source"
  let experiment_name = "experiment.name"
end

(*****************************************************************************)
(* Helper Functions *)
(*****************************************************************************)

let ( let@ ) = ( @@ )

(* Needed so we can reset scope id's randomness on telemetry restart *)
(* See restart_otel for more detail *)
let mk_rand_bytes_8 rand_ () : bytes =
  let@ () = Otel.Lock.with_lock in
  let b = Bytes.create 8 in
  for i = 0 to 1 do
    let r = Random.State.bits rand_ in
    (* 30 bits, of which we use 24 *)
    Bytes.set b (i * 3) (Char.chr (r land 0xff));
    Bytes.set b ((i * 3) + 1) (Char.chr ((r lsr 8) land 0xff));
    Bytes.set b ((i * 3) + 2) (Char.chr ((r lsr 16) land 0xff))
  done;
  let r = Random.State.bits rand_ in
  Bytes.set b 6 (Char.chr (r land 0xff));
  Bytes.set b 7 (Char.chr ((r lsr 8) land 0xff));
  b

let mk_rand_bytes_16 rand_ () : bytes =
  let@ () = Otel.Lock.with_lock in
  let b = Bytes.create 16 in
  for i = 0 to 4 do
    let r = Random.State.bits rand_ in
    (* 30 bits, of which we use 24 *)
    Bytes.set b (i * 3) (Char.chr (r land 0xff));
    Bytes.set b ((i * 3) + 1) (Char.chr ((r lsr 8) land 0xff));
    Bytes.set b ((i * 3) + 2) (Char.chr ((r lsr 16) land 0xff))
  done;
  let r = Random.State.bits rand_ in
  Bytes.set b 15 (Char.chr (r land 0xff));
  (* last byte *)
  b

let get_current_scope () = Otel.Scope.get_ambient_scope ()

let get_global_attr_opt key =
  List.find_map
    (fun (kv : Otel.Proto.Common.key_value) ->
      if String.equal kv.key key then Some (_key_value_conv kv) else None)
    !Otel.Globals.global_attributes

let find_global_attrs attr_keys = List_.filter_map get_global_attr_opt attr_keys

(*****************************************************************************)
(* Entry points for setting up telemetry *)
(*****************************************************************************)
(* Safe to call whenever *)
let stop_otel () =
  (* hack: get the backend so we can easily stop tracing at any time. See
     [with_paused_tracing] for why we want the option to do this
  *)
  Otel.Collector.get_backend ()
  |> Option.iter (fun backend ->
         Log.info (fun m -> m "Stopping tracing");
         let module Backend = (val backend : Otel.Collector.BACKEND) in
         Otel.Collector.remove_backend ();
         Backend.cleanup ())

(* setup_otel sets the Otel tracing backend and Trace_core tracing backend *)
let setup_otel trace_endpoint =
  let url = Uri.to_string trace_endpoint in
  Log.info (fun m -> m "Tracing endpoint set to %s" url);
  let config = Opentelemetry_client_ocurl.Config.make ~url () in
  let otel_backend = Opentelemetry_client_ocurl.create_backend ~config () in
  (* hack: let's just keep track of the endpoint for if we restart tracing
     instead of having to pass it down everywhere. We will assume that we will
     only ever report to one endpoint for the lifetime of the program *)
  Domain.DLS.set active_endpoint (Some trace_endpoint);
  (* Set the Otel Collector *)
  Otel.Collector.set_backend otel_backend

(* Set according to README of https://github.com/imandra-ai/ocaml-opentelemetry/ *)
let configure_otel ?(attrs : (string * user_data) list = []) service_name
    trace_endpoint =
  Otel.Globals.service_name := service_name;
  Otel.Globals.default_span_kind := Otel.Span.Span_kind_internal;
  let attrs = attrs @ Otel.GC_metrics.get_runtime_attributes () in
  List.iter
    (fun (key, value) -> Otel.Globals.add_global_attribute key value)
    attrs;
  Log.info (fun m -> m "Setting up tracing with service name %s" service_name);
  Otel.GC_metrics.basic_setup ();
  Ambient_context.set_storage_provider (Ambient_context_lwt.storage ());
  setup_otel trace_endpoint

let restart_otel () =
  (* We must re-initialize the randomness on restart since this usually happens
     after a parmap fork. If we don't do this then all parmap forks will have
     the same randomness and use duplicate span ids! This behavior is fine in
     jaeger but duplicates don't show up in datadog *)
  let new_random_state = Random.State.make_self_init () in
  Otel.Rand_bytes.rand_bytes_8 := mk_rand_bytes_8 new_random_state;
  Otel.Rand_bytes.rand_bytes_16 := mk_rand_bytes_16 new_random_state;
  Domain.DLS.get active_endpoint
  |> Option.iter (fun endpoint ->
         Log.info (fun m -> m "Restarting tracing");
         setup_otel endpoint)

(* Otel SOMETIMES segfaults if the traced process forks while the collector is running. So we
   need to stop the backends before forking, then continue after forking is
   done.

   See https://github.com/imandra-ai/ocaml-opentelemetry/issues/68
*)
let with_otel_paused f =
  (* Don't exit current spans here since we only want to pause *)
  stop_otel ();
  Common.protect ~finally:restart_otel f

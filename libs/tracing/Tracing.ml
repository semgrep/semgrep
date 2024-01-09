(* Emma Jin
 *
 * Copyright (C) 2023 Emma Jin
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

module Otel = Opentelemetry

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Wrapper for ocaml-trace (https://github.com/c-cube/ocaml-trace) and a
 * corresponding backend, which are used to instrument code.
 *)

(* Functions used to instrument the code *)

let with_span = Trace_core.with_span

(* Setting up the backend *)

(* TODO: uncomment - Emma Jin 12/17/2023
   For testing purposes, here is a function that makes it easy to instrument
   using a simple backend, trace-tef. For now, I'm leaving it commented so
   that we don't have to install a package that doesn't do anything. *)

(* Here is a simple backend for profiling that makes it easy to collect traces
   and start to see this library work. TODO For actual tracing, we'll probably
   use something else like https://github.com/imandra-ai/ocaml-opentelemetry/ *)
let initial_configuration () =
  Otel.Globals.service_name := "semgrep";
  Otel.GC_metrics.basic_setup ();
  Ambient_context.set_storage_provider (Ambient_context_lwt.storage ())

let with_setup f =
  let otel_backend =
    Opentelemetry_client_ocurl.create_backend
      (* TODO we would actually have a config *) ()
  in
  Opentelemetry_trace.setup_with_otel_backend otel_backend;
  Opentelemetry_client_ocurl.with_setup () @@ f

(* Here's an example of how the two functions might be used in `Core_scan.ml`:

   let get_rules config =
        Common.with_time (fun () -> rules_from_rule_source config)
   [@@trace]

   let scan_with_exn_handler (config : Core_scan_config.t) :
       Core_result.result_or_exn =
     try
       let timed_rules = Tracing.with_setup (fun () -> get_rules config) in
       let res =
         Pre_post_core_scan.call_with_pre_and_post_processor Fun.id scan config
           timed_rules
       in
       sanity_check_invalid_patterns res
     with
     | exn when not !Flag_semgrep.fail_fast ->
         let e = Exception.catch exn in
         logger#info "Uncaught exception: %s" (Exception.to_string e);
         Error (e, None)
*)

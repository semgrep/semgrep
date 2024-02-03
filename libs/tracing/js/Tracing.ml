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

(* Commentary *)
(*  *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let with_span = Trace_core.with_span

let run_with_span span_name ?data f =
  let data = Option.map (fun d () -> d) data in
  Trace_core.with_span ?data ~__FILE__ ~__LINE__ span_name @@ fun _sp -> f ()

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

(* Set according to README of https://github.com/imandra-ai/ocaml-opentelemetry/ *)
let initial_configuration () = ()
let with_setup f = f ()

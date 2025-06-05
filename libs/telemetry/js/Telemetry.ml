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

type scope = Otel.Scope.t
type user_data = Otel.value

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

let get_current_scope () = None
(*****************************************************************************)
(* Entry points for setting up telemetry *)
(*****************************************************************************)

let stop_otel () = ()
let restart_otel () = ()

let configure_otel ?(attrs = []) (_service_name : string) (_endpoint : Uri.t) =
  ignore attrs;
  ()

let with_otel_paused f = f ()

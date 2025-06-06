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
  let vcs_ref_head_revision = "vcs.ref.head.revision"
  let vcs_ref_head_name = "vcs.ref.head.name"

  (* These are semgrep specific and technically shouldn't be in this library but
     these will be applied to all metrics *)
  let scan_engine = "scan.engine"
  let scan_source = "scan.source"
  let experiment_name = "experiment.name"
end

let get_current_scope () = None
let get_global_attr_opt _ = None
let find_global_attrs attr_keys = List_.filter_map get_global_attr_opt attr_keys
(*****************************************************************************)
(* Entry points for setting up telemetry *)
(*****************************************************************************)

let stop_otel () = ()
let restart_otel () = ()

let configure_otel ?(attrs = []) (_service_name : string) (_endpoint : Uri.t) =
  ignore attrs;
  ()

let with_otel_paused f = f ()

(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
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
(* Metrics we send to the mothership *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)
type client_metrics = {
  machineId : string option; [@default None]
  isNewAppInstall : bool; [@default false]
  sessionId : string option; [@default None]
  extensionVersion : string option; [@default None]
  extensionType : string; [@default "cli"]
  enabled : bool; [@default true]
}
[@@deriving yojson]

type t = {
  client_metrics : client_metrics;
  autofix_count : int;
  ignore_count : int;
}
[@@deriving yojson]

let client_metrics_default =
  {
    machineId = None;
    isNewAppInstall = false;
    sessionId = None;
    extensionVersion = None;
    extensionType = "cli";
    enabled = true;
  }

let default =
  {
    client_metrics = client_metrics_default;
    autofix_count = 0;
    ignore_count = 0;
  }

let t_of_yojson = of_yojson
let yojson_of_t = to_yojson
let pp fmt t = Yojson.Safe.pretty_print fmt (yojson_of_t t)

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
(* TODO actually send them *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)
type t = {
  machineId : string option; [@default None]
  isNewAppInstall : bool; [@default false]
  sessionId : string option; [@default None]
  extensionVersion : string option; [@default None]
  extensionType : string; [@default "cli"]
  enabled : bool; [@default true]
}
[@@deriving yojson]

let default =
  {
    machineId = None;
    isNewAppInstall = false;
    sessionId = None;
    extensionVersion = None;
    extensionType = "cli";
    enabled = true;
  }

let t_of_yojson json = of_yojson json
let yojson_of_t t = to_yojson t
let pp fmt t = Yojson.Safe.pretty_print fmt (yojson_of_t t)

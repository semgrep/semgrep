(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type client_metrics = {
  machineId : string option;
  isNewAppInstall : bool;
  sessionId : string option;
  extensionVersion : string option;
  extensionType : string;
  enabled : bool;
}
[@@deriving yojson]

type t = {
  client_metrics : client_metrics;
  (* # of autofix code actions *)
  autofix_count : int;
  (* # of ignore code actions *)
  ignore_count : int;
}

val t_of_yojson : Yojson.Safe.t -> (t, string) result
val yojson_of_t : t -> Yojson.Safe.t
val default : t
val client_metrics_default : client_metrics
val pp : Format.formatter -> t -> unit

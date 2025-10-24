(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val with_setup :
  ?log_to_file:Fpath.t ->
  ?log_to_otel:bool ->
  ?require_one_of_these_tags:string list ->
  ?quiet_log_setup:bool ->
  ?color:Console.highlight_setting ->
  level:Logs.level option ->
  (unit -> 'a) ->
  'a
(** Small wrapper around [Logs_.setup()], itself a wrapper around
    [Logs.set_xxx], that adds semgrep-specific things:
    - prints instructions for debug logging,
    - sets up opentelemetry,
    - specifies environment variables from which to read logging options.
*)

module Log : Logs.LOG
(** Log functions for this source ("semgrep") *)

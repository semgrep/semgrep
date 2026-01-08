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
val map_targets :
  conf:Parallelism_config.eio_state ->
  num_jobs:int ->
  (Target.t -> 'a) ->
  Target.t list ->
  ('a, Target.t * exn) result list
(** [map_targets] basically wraps {!Concurrent.map} but with some extra Semgrep
    specific tweaks, such as hacks to get telemetry to work across domain
    spawns *)

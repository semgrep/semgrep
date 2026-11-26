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
  ?element_cost:[ `Coarse | `Cheap_uniform | `Cheap_variable ] ->
  conf:Parallelism_config.eio_state ->
  num_jobs:int ->
  (Target.t -> 'a) ->
  Target.t list ->
  ('a, Target.t * exn) result list
(** [map_targets] basically wraps {!Concurrent.map} but with some extra Semgrep
    specific tweaks, such as hacks to get telemetry to work across domain
    spawns. [element_cost] is passed on to {!Concurrent.map}: use
    [`Cheap_variable] when scanning a target is usually cheap, as it is for
    most source files, and [`Coarse] (the default) when it is expensive. *)

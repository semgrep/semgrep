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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let map_targets ~(conf : Parallelism_config.t) ~(num_jobs : int)
    (f : Target.t -> 'a) (targets : Target.t list) =
  (* the scope does not persist across domain spawns due to this issue:
          https://github.com/imandra-ai/ocaml-opentelemetry/issues/104
          so we capture the current scope and reapply it within the new domain
       *)
  let current_scope_opt = Telemetry.get_current_scope () in
  let f x = Telemetry.with_opt_scope current_scope_opt (fun () -> f x) in
  Concurrent.map ~conf ~domain_count:num_jobs f targets

(* Cooper Pierce and Yosef Alsuhaibani
 *
 * Copyright (C) Semgrep, Inc. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see
 * <https://www.gnu.org/licenses/>.
 *)

(** [capped_length n l] returns [min (List.length l) n] without traversing
    more than [n] elements of [l]. *)
let capped_length n l =
  let rec aux i = function
    | _ when i >= n -> n
    | [] -> i
    | _ :: tl -> aux (i + 1) tl
  in
  aux 0 l

let map ~(conf : Parallelism_config.eio_state) ~domain_count f l =
  Eio.Switch.run @@ fun sw ->
  let domain_mgr = Eio.Stdenv.domain_mgr conf.env in
  let domain_count = max 1 (capped_length domain_count l) in
  let pool = Executor_pool.create ~sw ~domain_count domain_mgr in

  (* nosemgrep: no-logs-in-library *)
  Logs.debug (fun m ->
      m "Mapping %d elements across %d domains" (List.length l) domain_count);

  Eio.Fiber.List.map ~max_fibers:domain_count
    (fun elem ->
      (* NOTE: [submit] blocks the fiber until the task returns a result.*)
      (* Please see the comment block in [Hook.ml] concerning safe values of
       * [weight], if you are intending on changing it! *)
      match Executor_pool.submit pool ~weight:1.0 (fun () -> f elem) with
      | Ok res -> Ok res
      | Error err -> Error (elem, err))
    l
[@@tracing]

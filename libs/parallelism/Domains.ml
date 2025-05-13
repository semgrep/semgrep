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

let map ~pool f l =
  (* The main thread concurrently maps over the list of tasks via spawning
   * fibers (i.e weak threads) that submit and wait for the Domain pool to
   * return the result of submitting the task.
   *)
  Eio.Fiber.List.map
    (fun elem ->
      (* NOTE: [submit] blocks the fiber until the task returns a result.*)
      Eio.Executor_pool.submit pool ~weight:0.5 (fun () -> f elem))
    l

let wrap_timeout ~clock t f =
 fun x -> Eio.Time.with_timeout clock t (fun () -> Ok (f x))

let wrap_timeout_exn ~clock t f =
 fun x -> Eio.Time.with_timeout_exn clock t (fun () -> f x)

(* TODO: make the frequency configurable on the CLI, perhaps. Keep this a power of two! *)
let yield_frequency = 8192

(* Our goal for `maybe_yield` is to not be much more unresponsive than what the
 * OS scheduler would do, which means we shouldn't feel obligated to actually yield
 * more than once every centisecond or so.  If `maybe_yield` gets called in quick
 * succession (such as in [Matching_generic] combinators), then we'll be burning a lot
 * of CPU time having the Eio scheduler rerun needlessly, so we should only actually
 * occasionally yield when `maybe_yield` is called.
 *
 * The default choice of `[yield_frequency]` was chosen somewhat arbitrarily.  My
 * intuition is that too high (e.g. too infrequent yields) is better than too low
 * (e.g. too much spinning inside Eio); so long as we are still adhering to responding to
 * timeouts (which are on the granularity of seconds) then we are doing our job.
 *)
let yield_attempts = Atomic.make 0

let maybe_yield () =
  if not !Common.jsoo then
    if Atomic.fetch_and_add yield_attempts 1 land (yield_frequency - 1) = 0 then
      (* If we fail to get the context during yielding, we're not running in Eio. *)
      try Eio.Fiber.yield () with
      (* TODO: This is similar to Hook.attempt_in_eio. *)
      | Stdlib.Effect.Unhandled _ -> ()

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

val map :
  pool:Eio.Executor_pool.t -> ('a -> 'b) -> 'a list -> ('b, exn) result list
(** [map ~pool f xs] applies [f] to every element of [xs] in parallel via the
    Domains in [~pool], with the application evaluating to [Ok res] if
    [f elem] evaluates to [res] or [Err exn] if [f elem] raised the
    exception [exn]
*)

val wrap_timeout_exn : clock:_ Eio.Time.clock -> float -> ('a -> 'b) -> 'a -> 'b
(** Wraps the supplied function to be invoked on some fiber within some
  * duration (in seconds).
  *
  * Raises if we exceed the timeout, so we must only use this in an
  * exception-safe context (such as passing it directly to [Domains.map]).
  *)

val wrap_timeout :
  clock:_ Eio.Time.clock ->
  float ->
  ('a -> 'b) ->
  'a ->
  ('b, [> `Timeout ]) result
(** Wraps the supplied function to be invoked on some fiber within some
  * duration (in seconds).
  *)

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

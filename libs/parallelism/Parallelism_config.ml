(* Nathan Taylor
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

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
    State that relates to Eio and other concurrency/parallelism-related matters.

    TODO: this is not really a "config" in the sense of coming from CLI
    arguments.  What's a better name?
*)

(*************************************************************************)
(* Types *)
(*************************************************************************)

(* In this [env], we only expose the Eio capabilities we explicitly need.
 * https://github.com/ocaml-multicore/eio?tab=readme-ov-file#passing-env *)
type env = < clock : float Eio.Time.clock_ty Eio.Std.r >

type t = { env : env; [@opaque] exec_pool : Eio.Executor_pool.t [@opaque] }
[@@deriving show]

let create (env : Eio_unix.Stdenv.base) exec_pool =
  let env =
    object
      method clock = Eio.Stdenv.clock env
    end
  in
  { env; exec_pool }

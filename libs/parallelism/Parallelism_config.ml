(* Copyright (C) Semgrep, Inc. All rights reserved.
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
    State that relates to configuring concurrency/parallelism-related mechanisms.

    TODO: this is not really a "config" in the sense of coming from CLI
    arguments.  What's a better name?

    TODO: Why wasn't this written with a separate interface file?
*)

(*************************************************************************)
(* Types *)
(*************************************************************************)

type error = NotInEio of string

(* In this [env], we only expose the Eio capabilities we explicitly need.
 * https://github.com/ocaml-multicore/eio?tab=readme-ov-file#passing-env *)
type env =
  < clock : float Eio.Time.clock_ty Eio.Std.r
  ; domain_mgr : Eio.Domain_manager.ty Eio.Std.r >

type _base = Eio_unix.Stdenv.base

type eio_state = {
  (* [env] is the Eio environment with our required capabilities. *)
  env : env; [@opaque]
  base : _base; [@opaque]
}
[@@deriving show]

(* TODO: Makes sense to store the number of jobs in the parallelism config
 * too but we'd have to refactor Num_jobs out of `src/configuring`. *)
(* TODO: once parmap is fully-deprecated, we will only be running with an Eio
 * executor, obviating the need for this type entirely. *)
type t = Process | Eio_executor of eio_state [@@deriving show]

let create (env : Eio_unix.Stdenv.base) =
  Eio_executor { env :> env; base = env }

let default = Process

let unsafe_get_base (t : t) : (_base, error) result =
  (* This is unsafe because it exposes the full Eio environment, which
     may not be what we want.  Use with care! *)
  match t with
  | Process -> Result.error (NotInEio "unsafe_get_base")
  | Eio_executor eio_state -> Result.Ok eio_state.base

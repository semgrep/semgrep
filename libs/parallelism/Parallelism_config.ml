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
  ; domain_mgr : Eio.Domain_manager.ty Eio.Std.r
  ; process_mgr : Eio_unix.Process.mgr_ty Eio.Std.r >

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

(* [Eio_main.run], but on Unix use Eio's posix backend unless $EIO_BACKEND
 * says otherwise. With the io_uring backend every domain needs its own ring,
 * and rings are charged to the user's RLIMIT_MEMLOCK (8 MiB by default in
 * Docker/Kubernetes). Eio only falls back to posix if the *first* ring cannot
 * be created; a worker domain that cannot get one kills the scan with
 * ENOMEM from io_uring_queue_init (#11802). Several semgrep-core processes
 * running at once (e.g. pre-commit) hit this easily. We use Eio for
 * parallelism rather than IO, so the posix backend costs us nothing. *)
let run_eio (f : Eio_unix.Stdenv.base -> 'a) : 'a =
  (match Sys.getenv_opt "EIO_BACKEND" with
  | None
  | Some ""
    when Sys.unix ->
      Unix.putenv "EIO_BACKEND" "posix"
  | _ -> ());
  Eio_main.run f

let create (env : Eio_unix.Stdenv.base) =
  Eio_executor { env :> env; base = env }

let default = Process

let unsafe_get_env (t : t) : (env, error) result =
  (* This is unsafe because it exposes the full Eio environment, which
     may not be what we want.  Use with care! *)
  match t with
  | Process -> Result.error (NotInEio "get_env")
  | Eio_executor eio_state -> Result.Ok eio_state.env

let unsafe_get_base (t : t) : (_base, error) result =
  (* This is unsafe because it exposes the full Eio environment, which
     may not be what we want.  Use with care! *)
  match t with
  | Process -> Result.error (NotInEio "unsafe_get_base")
  | Eio_executor eio_state -> Result.Ok eio_state.base

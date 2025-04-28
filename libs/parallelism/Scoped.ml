(* Nathan Taylor
 *
 * Copyright (C) 2025 Semgrep Inc.
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

(* Abstractions for scoping mutable state to a particular concurrency mechanism. *)

(* XXX: In a later patch, this will get renamed to Hook - all Hooks will now be
 * concurrency aware down the road!!! *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* A scoping that is unaware of any intra-process concurrency primitives;
 * this must not be used unless you are sure the key will not escape its
 * concurrency context (domain, or fibre, or...)
 *
 * This is used in the implementation of the fiber-aware hook (this module)
 * for handling scoped accesses outside the Eio event loop.
 *)
module Proc = struct
  type 'a key = 'a ref

  let create init = ref init
  let get = ( ! )
  let with_hook_set = Common.save_excursion
end

let has_eio_context () =
  (* XXX: This is an abuse of Fiber.check, which is really intended to verify
   * whether the running fiber has been canceled.  Here, we use it to ascertain
   * whether the "running fiber" actually exists (e.g. whether we are in the
   * synchronous world or the effects world).  It would be vastly preferable
   * if Eio exposed such functionality to us.
   *
   * Filed https://github.com/ocaml-multicore/eio/issues/800 to ask the Eio
   * maintainers to expose this directly for us.
   *)
  try
    Eio.Fiber.check ();
    true
  with
  | Stdlib.Effect.Unhandled Eio__core__Cancel.Get_context -> false
(* Propagate all other exns; do not swallow a cancelation notification, for example *)

type 'a t = {
  (* The Fiber-local key that will encapsulate the 'a in scoped settings. *)
  key : 'a Eio.Fiber.key;
  (* The synchronous scoped values for when we are outside an Eio context.
   * If we are not executing inside Eio, a Fib should behave just like
   * a process-local scope.
   *
   * XXX: if the 'a is a mutable type, very bad things will happen.  This is
   * unfortunate because something that is fiber-local should conceptually be
   * able to be mutable!  If we in fact need mutable defaults (Gd willing we
   * will not, but one could imagine wanting per-fiber [Hashtbl.t]s or some
   * such thing) this could be a hashtable keyed on a Fiber Id (however, such
   * an Id does not appear to be exposed, so unclear how to make that happen.)
   *)
  sync_scope : 'a Proc.key;
}

let create init =
  let key = Eio.Fiber.create_key () in
  let sync_scope = Proc.create init in
  { key; sync_scope }

let get { key; sync_scope } =
  if has_eio_context () then
    match Eio.Fiber.get key with
    | None -> Proc.get sync_scope
    | Some v -> v
  else Proc.get sync_scope

let with_hook_set { key; sync_scope } v f =
  if has_eio_context () then Eio.Fiber.with_binding key v f
  else Proc.with_hook_set sync_scope v f

let with_ h v f () = with_hook_set h v f

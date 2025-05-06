(* Nat Mote, Nathan Taylor
 *
 * Copyright (C) 2020-2025 Semgrep Inc.
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

(* Abstractions for scoping mutable state to a particular fiber.
 *
 * A hook is usually a place in a function of a module that can be used to
 * add special behavior that can't be anticipated when the module was designed.
 * This is common practice in Emacs modes for example.
 *
 * By all rights, this should live in [parallelism], but it is placed here to
 * avoid a circular dependency between [parallelism] and [common]. *)

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

  (* Only ever to be used by _unscoped_set *)
  let unscoped_set h v = h := v
end

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

let attempt_in_eio feio fsync =
  (* XXX: It would be vastly preferable if Eio exposed a "are we executing in
   * an EIO loop" check to us, instead of boldly marching ahead and seeing if
   * we step on a rake in the process.
   *
   * Filed https://github.com/ocaml-multicore/eio/issues/800 to ask the Eio
   * maintainers to expose this directly for us.
   *)
  if !Common.jsoo then fsync ()
  else
    try feio () with
    (* Propagate all other exns; do not swallow a cancelation notification, for example *)
    | Stdlib.Effect.Unhandled Eio__core__Cancel.Get_context -> fsync ()

(* Note: The following functions catch an unhandled Get_context effect
 * in the case where we are calling these synchronously (e.g. outside
 * an Eio context).
 *
 * Filed https://github.com/ocaml-multicore/eio/issues/800 to ask the Eio
 * maintainers to expose this directly for us.
 *)

let get { key; sync_scope } =
  attempt_in_eio
    (fun () ->
      match Eio.Fiber.get key with
      | None -> Proc.get sync_scope
      | Some v -> v)
    (fun () -> Proc.get sync_scope)

let with_hook_set { key; sync_scope } v f =
  attempt_in_eio
    (fun () -> Eio.Fiber.with_binding key v f)
    (fun () -> Proc.with_hook_set sync_scope v f)

let with_ h v f () = with_hook_set h v f

let has_eio_context () =
  attempt_in_eio
    (fun () ->
      (* XXX: This is an abuse of Fiber, which is really intended to verify
       * whether the running fiber has been canceled.  Here, we use it to ascertain
       * whether the "running fiber" actually exists (e.g. whether we are in the
       * synchronous world or the effects world). *)
      Eio.Fiber.check ();
      true)
    (fun () -> false)

module Arg = struct
  (* Unconditionally stomps over a Hook's value, so long as we are
   * in the synchronous and not the Eio world.  This is both a sanity
   * check that we're not doing something _wildly_ off-base (CLI parsing
   * after all will definitely be before we enter the Eio event loop)
   * but also because there's just no way to represent such behaviour with
   * a Fiber-local value anyway!
   *)
  let unscoped_set { sync_scope; _ } v =
    if has_eio_context () then
      failwith
        "Can't set a hook's value via [unscoped_set] within an Eio context!"
    else Proc.unscoped_set sync_scope v

  let bool h = Arg.Bool (unscoped_set h)
  let set h = Arg.Unit (fun () -> unscoped_set h true)
  let clear h = Arg.Unit (fun () -> unscoped_set h false)
  let int h = Arg.Int (fun i -> unscoped_set h i)
  let str h = Arg.String (fun s -> unscoped_set h s)
end

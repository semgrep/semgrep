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

(* A scoping that is unaware of any concurrency primitives; this must only be
 * used if you are sure the key will not escape a fiber-local context.
 *
 * This is used in the implementation of the fiber-aware hook (this module)
 * for handling:
 *      1. Scoped access outside the Eio event loop (e.g. for jsoo or before
 *      Eio_main is called;
 *      2. _Unscoped_, unconditional assignment for things like CLI argument
 *      parsing, that occur before any scoped access.
 *)
module Proc = struct
  (* Process-"local" state that will encapsulate the 'a in scoped settings. *)
  type 'a key = 'a ref

  (* Tracks whether this fiber is allowed to be unconditionally set outside the
   * context of a scoped operation.  This is possible up until the first invocation
   * of [with_hook_set].
   *)
  let create = ref
  let get = ( ! )
  let with_hook_set = Common.save_excursion

  (* This should only be used by [Proc.with_hook_set], or [Arg.unscoped_set]. *)
  let unsafe_set = ( := )
end

type 'a t = {
  (* The Fiber-local state that will encapsulate the 'a in scoped settings.
   * We additionally track, per-fiber, how many nested [with_hook_set] calls
   * we are executing within. *)
  key : 'a Eio.Fiber.key;
  (* Tracks whether this fiber is allowed to be unconditionally set outside the
   * context of a scoped operation.  This is possible up until the first invocation
   * of [with_hook_set] that _any_ fiber does.
   *)
  can_unscoped_set : bool Atomic.t;
  (* The value to hand back from a [get] call when we have not yet set a hook value
   * within an Eio scope.
   *)
  proc_scope : 'a Proc.key;
}

let create default =
  let key = Eio.Fiber.create_key () in
  let proc_scope = Proc.create default in
  { key; proc_scope; can_unscoped_set = Atomic.make true }

let attempt_in_eio ~in_eio ~no_eio =
  (* XXX: It would be vastly preferable if Eio exposed a "are we executing in
   * an EIO loop" check to us, instead of boldly marching ahead and seeing if
   * we step on a rake in the process.
   *
   * Filed https://github.com/ocaml-multicore/eio/issues/800 to ask the Eio
   * maintainers to expose this directly for us. *)
  if !Common.jsoo then no_eio ()
  else
    try in_eio () with
    | Stdlib.Effect.Unhandled Eio__core__Cancel.Get_context -> no_eio ()

let get { key; proc_scope; _ } =
  attempt_in_eio
    ~in_eio:(fun () ->
      match Eio.Fiber.get key with
      | None -> Proc.get proc_scope
      | Some v -> v)
    ~no_eio:(fun () -> Proc.get proc_scope)

let with_hook_set { key; can_unscoped_set; proc_scope } v f =
  (* For those keeping score at home: setting [can_unscoped_set] here
   * will act as a barrier for subseqent access to [proc_scope.get]. *)
  Atomic.set can_unscoped_set false;
  attempt_in_eio
    ~in_eio:(fun () -> Eio.Fiber.with_binding key v f)
    ~no_eio:(fun () -> Proc.with_hook_set proc_scope v f)

let with_ h v f () = with_hook_set h v f

module Arg = struct
  (* Unconditionally stomps over a Hook's value, so long as no fiber has performed
   * a scoped operation with this hook.
   *)
  let unscoped_set { proc_scope; can_unscoped_set; _ } v =
    if not (Atomic.get can_unscoped_set) then
      failwith "Must not call [unscoped_set] after [with_hook_set]"
    else Proc.unsafe_set proc_scope v

  let bool h = Arg.Bool (unscoped_set h)
  let set h = Arg.Unit (fun () -> unscoped_set h true)
  let clear h = Arg.Unit (fun () -> unscoped_set h false)
  let int h = Arg.Int (fun i -> unscoped_set h i)
  let str h = Arg.String (fun s -> unscoped_set h s)
end

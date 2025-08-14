(* Yosef Alsuhaibani, Nat Mote, Nathan Taylor
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

(* At the moment, Hooks are represented by per-Domain local state ("DLS").
 * If you are reading this file and are aware that Eio manages our concurrency,
 * this might be surprising and concerning.  Here's what you need to know:
 *
 * First, a tiny bit of background: you can think of a Domain as a pthread,
 * preemptive and managed by the OS.  Fibers are cooperative green threads
 * managed by the Eio runtime, intended typically for short-lived I/O tasks.
 * Eio's Domain manager and user-mode scheduler will multiplex fibers onto
 * domains (but is not an m:n scheduler so fibers will not migrate between
 * domains; this is significant for later on.)
 *
 * Eio _does_ have fibre-local state ("FLS"), which, by rights, should exactly
 * be the abstraction we want: it's a place to stash values private to a fiber
 * and perform "local" mutation.  However, there are a few issues with it:
 *
 * 1) The value of a FLS variable is _normally_ inherited when a fiber forks,
 * _UNLESS_ that fork happens in a different domain.  In that case, the
 * value is unset.  This was, at best, documented obliquely (demonstrated in
 * a test but not explicitly written down in the docs), and has bitten us both
 * in the LSP work and in the Parmap -> multicore scan transition.
 *
 * 2) Fiber-local state is designed exclusively for scoped access: the docs
 * discuss how they take the lambda calculus as inspiration, which is noble,
 * but we live in the real world: there are places where we want to set a hook
 * in an unscoped setting (in particular, while setting CLI arguments) which does
 * not conform to their elegant abstraction.  The V1 Hook API (which you can
 * see in the parent of this commit) had some additional primitives built in
 * to accommodate "unscoped sets", but the invariants that had to be maintained
 * (in essence, you could set a value in an un-lexically scoped setting so long
 * as you hadn't set it in a scoped setting) were awkward.  (A vestige of this
 * is the [can_unscoped_set] value in [Hook.t].)
 *
 * 3) This is a minor point, but, every access to a FLS value hits a hash table
 * rather than an indirect pointer access.  We had an performance regresssion
 * where a hook was accessed on a hot code path.  This means we have to be
 * somewhat thoughtful about how we use Hooks, which is unfortunate.
 *
 * An orthogonal issue to the design of Eio's fiber-local storage is that we
 * at Semgrep are in the process of gradually migrating to it; since we need
 * backwards compatibility with the old Parmap-based parallelism model, we
 * can't assume that we are always running under an Eio event handler[1].
 * So, we need to really have _two_ hook implementations: one for the non-Eio
 * universe and another for it, and doing some juggling to see which
 * of the two we should be using at a given moment.
 *
 * Okay, so Eio's abstraction has issues: but, how is using domain-local state
 * safe?  If we had two Eio fibers scheduled on the same domain, they'll stomp
 * over each other's values; from the perspective of one of those fibers, a
 * hook value could spuriously change underneath its feet.  We'll call this a
 * "phantom" read or write.
 *
 * The invariant we have to maintain right now is thus: ***NEVER SHALL A DOMAIN
 * EVER HAVE MORE THAN ONE HOOK-MUTATING FIBER SCHEDULED ON IT.***
 *
 * This is actually already a desirable property anyway: Operations like
 * parsing and matching are compute-bound, so it wouldn't make sense to
 * have more than one on an OS thread since they'd be competing with each other
 * for the same underlying resource.  In order to maintain this invariant,
 * it's critical that Domains.map, when farming out tasks, assigns at minimum
 * the majority of a domain to that operation.  (If we gave it less than 50%,
 * then >1 such operations could land on the same domain, causing phantom writes
 * from another fiber to be observed.)
 *
 * In the presence of other tasks (like cohttp-eio or metrics collection) on
 * a shared domain, this is fine so long as those tasks don't modify hook state.
 * Those are free to be scheduled on the same domain.
 *
 * Since Eio's scheduler is per-domain, we don't have to worry about the runtime
 * trying to be smart and have us migrated around different OS threads, so it
 * will not violate this invariant for us.
 *
 * In a better world, we would migrate back to Eio's fiber-local state mechanism.
 * It's not good to have to always remember how to keep hook state safe.  At
 * minimum, issue 1) would be a blocker for our migration.  While issue 2) is
 * annoying, we've already done the work to ensure that "unscoped mutation" is
 * something we can do, so that's not a blocker.  I don't love issue 3), but it
 * is workable if the first two are solved.
 *
 * So, what do you, as as a programmer in the future, need to know about hooks?
 *
 * If you are just adding Hooks to the codebase, nothing.  Godspeed!
 *
 * If you are factoring out previously-single threaded compute to be in
 * parallel, so long as you are using Domains.map for its fork-join
 * parallelism, also nothing.  Thank you for speeding up our codebase and
 * making good use of our hardware.
 *
 * If you are adding new parallelism that does not go through Domains.map, and
 * if it potentially accesses Hook state, heed The Big Invariant above.
 *
 *
 * [1] fun facts with Nathan: why, you might ask, couldn't we just run legacy
 * Parmap runs with a dummy Eio event loop that isn't actually used?  Here, we
 * would fork the process and automatically inherit private copies of the Eio
 * state; and, for multicore runs, Eio would just do the right thing. Great
 * question: this doesn't play well with legacy forms of asynchrony, namely
 * signal handlers for e.g. timeouts: if a signal is delivered when we are
 * executing in the Eio effect handler function, we'll go off the rails when
 * the timeout exn fires on the wrong call stack.
 *)
type 'a t = {
  (* The thread-local state that will encapsulate the 'a in scoped settings. *)
  key : 'a Domain.DLS.key;
  (* Tracks whether this hook is allowed to be unconditionally set outside the
   * context of a scoped operation.  This is possible up until the first invocation
   * of [with_hook_set] that _any_ fiber does.
   *
   * Note: this is not strictly required with the DLS interface we are using,
   * however, it is left here for the future case where we wish to migrate back to
   * a pure-Eio hook solution, where this invariant _has_ to be maintained.
   *)
  can_unscoped_set : bool Atomic.t;
}

let create ?split_from_parent default =
  (* Different semantics from DLS' split from parent: we always want to
   * inherit the parent's value, not "reset" to the default one. *)
  let split_from_parent =
    match split_from_parent with
    | None -> Fun.id
    | Some f -> f
  in
  let key = Domain.DLS.new_key ~split_from_parent (Fun.const default) in
  { key; can_unscoped_set = Atomic.make true }

let get { key; _ } = Domain.DLS.get key

let with_hook_set { key; can_unscoped_set } v f =
  Atomic.set can_unscoped_set false;
  let old = Domain.DLS.get key in
  Domain.DLS.set key v;
  Common.finalize f (fun _ -> Domain.DLS.set key old)

let with_ h v f () = with_hook_set h v f

module Arg = struct
  (* Unconditionally stomps over a Hook's value, so long as no fiber has performed
   * a scoped operation with this hook.
   *)
  let unscoped_set { can_unscoped_set; key } v =
    if not (Atomic.get can_unscoped_set) then
      failwith "Must not call [unscoped_set] after [with_hook_set]"
    else Domain.DLS.set key v

  let unit h f = Arg.Unit (fun () -> unscoped_set h (f ()))
  let bool h = Arg.Bool (unscoped_set h)
  let set h = Arg.Unit (fun () -> unscoped_set h true)
  let clear h = Arg.Unit (fun () -> unscoped_set h false)
  let int h = Arg.Int (fun i -> unscoped_set h i)
  let str h = Arg.String (fun s -> unscoped_set h s)
end

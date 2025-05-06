(** A mutable "hook", allowing the behavior of one module to be modified by
 * mutating state. This is not generally very good practice, but we use this
 * pattern in quite a few places.
 *
 * This module is preferred over using refs directly because it prohibits us
 * from directly setting the hook. Instead, we must use `with_hook_set` which
 * scopes the mutation of the hook to the execution of a particular function,
 * then returns the hook to its previous value.
 *
 * This is critical in a multithreaded context, where overwriting "naked refs"
 * would cause race conditions.  Since `with_hook_set` "localises" mutation
 * to a sequence of function calls, state for a hook resides in Eio per-fiber
 * storage.  As a result, a concurrent program will not race on setting a hook's
 * value.
 *
 * This makes it easier to reason about the hooks and makes it less likely that
 * hook state will escape outside where it is intended. We've had issues where,
 * for example, hooks were not reset between tests, leading to bizarre and
 * difficult-to-debug behavior. This should prevent that.
 *)

type 'a t

val create : 'a -> 'a t
(** Lifts a value into a fiber-local context. *)

val get : 'a t -> 'a
(** Note that we do not provide a 'val set: 'a -> 'a t -> unit'! This is
 * on purpose, we want users of the hook to be forced to use
 * with_hook_set below.
 *)

val with_hook_set : 'a t -> 'a -> (unit -> 'b) -> 'b
(** Temporarily modify the hook to a certain value and then restore
 * the value (similar to Common.save_excursion)
 *)

val with_ : 'a t -> 'a -> (unit -> 'b) -> unit -> 'b
(** This is similar to [with_hook_set] but instead of executing
 * the callback to get the ['b] final computation value, we return
 * a closure that will compute the value. That way we can combine
 * multiple calls to [with_] using [@@] as in:
 *
 *   let with_foo_hooks f =
 *     let f =
 *          Hook.with_ hook1 v1
 *       @@ Hook.with_ hook2 v2
 *       @@ ...
 *       @@ f
 *     in
 *     f ()
 *
 * instead of having to define the intermediate [f] as in the more
 * boilerplate heavy:
 *
 *    let with_fook_hooks f =
 *     let f () = Hook.with_hook_set hook1 v1 f in
 *     let f () = Hook.with_hook_set hook2 v2 f in
 *     ...
 *     in
 *     f ()
 *)

(** Unfortunately, it's a slightly lie to say that every mutation on
 * a Hook is scoped - CLI argument parsing, for instance, is one such
 * "non-local" mutation that we can't avoid.  For this _particular_
 * situation, we expose unconditional mutation functionality that wraps
 * frequently-used [Arg] combinatorss.
 *
 * For example, a simple CLI's arguments might ordinarily be configured
 * and parsed thus:
 * [ let ref_verbose = ref false in
 *   let ref_nprocs  = ref 1 in
 *
 *   let speclist = [
 *      ("-verbose", Hook.Set ref_verbose, "Be noisy on stdout");
 *      ("-nprocs", Hook.Set_int ref_nprocs, "Parallelism")
 *   ] in
 *   Arg_.parse_options speclist Sys.argv ...
 * ]
 *
 * To banish `ref` from this code listing, we will of course use a [Hook],
 * [  let hook_verbose = Hook.create false in
 *    let hook_nprocs = Hook.create 1 in
 * ]
 *
 * And the combinators used in [speclist] should be the originals'
 * corresponding combinators:
 * [
 *   let speclist = [
 *      ("-verbose", Hook.Arg.set hook_verbose, "Be noisy on stdout");
 *      ("-nprocs", Hook.Arg.int hook_nprocs, "Parallelism")
 *   ] in ...
 * ]
 *
 * In this way, we know that the only way a [Hook] can be mutated outside a
 * scoped context (that is, with [with_hook_set]) is during CLI argument parsing.
 *)
module Arg : sig
  val bool : bool t -> Arg.spec
  val int : int t -> Arg.spec
  val str : string t -> Arg.spec
  val set : bool t -> Arg.spec
  val clear : bool t -> Arg.spec
end

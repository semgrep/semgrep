(* A mutable "hook", allowing the behavior of one module to be modified by
 * mutating state. This is not generally very good practice, but we use this
 * pattern in quite a few places.
 *
 * This module is preferred over using refs directly because it prohibits us
 * from directly setting the hook. Instead, we must use `with_hook_set` which
 * scopes the mutation of the hook to the execution of a particular function,
 * then returns the hook to its previous value.
 *
 * Since `with_hook_set` "localises" mutation to a sequence of function calls,
 * state for a hook resides in Domain-local storage.  As a result, a parallel
 * program will not race on setting a hook's value.
 *
 * This makes it easier to reason about the hooks and makes it less likely that
 * hook state will escape outside where it is intended. We've had issues where,
 * for example, hooks were not reset between tests, leading to bizarre and
 * difficult-to-debug behavior. This should prevent that. *)
type 'a t

val create : 'a -> 'a t

(* Note that we do not provide a 'val set: 'a -> 'a t -> unit'! This is
 * on purpose, we want users of the hook to be forced to use
 * with_hook_set below
 *)
val get : 'a t -> 'a

(* Temporarily modify the hook to a certain value and then restore
 * the value (similar to Common.save_excursion)
 *)
val with_hook_set : 'a t -> 'a -> (unit -> 'b) -> 'b

(* This is similar to [with_hook_set] but instead of executing
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
val with_ : 'a t -> 'a -> (unit -> 'b) -> unit -> 'b

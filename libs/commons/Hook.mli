(* A mutable hook, allowing the behavior of one module to be updated by mutating
 * state. This is not generally very good practice, but we use this pattern in
 * quite a few places.
 *
 * This module is preferred over using refs directly because it prohibits us
 * from directly setting the hook. Instead, we must use `with_hook_set` which
 * scopes the mutation of the hook to the execution of a particular function,
 * then returns the hook to its previous value.
 *
 * This makes it easier to reason about the hooks and makes it less likely that
 * hook state will escape outside where it is intended. We've had issues where,
 * for example, hooks were not reset between tests, leading to bizarre and
 * difficult-to-debug behavior. This should prevent that. *)
type 'a t

val create : 'a -> 'a t
val with_hook_set : 'a t -> 'a -> (unit -> 'b) -> 'b
val get : 'a t -> 'a

(* Ensures safe memoization of a function that can be called between threads.
 * If you are writing new code, think hard about whether your problem warrants
 * this solution!  This should typically only be used for legacy pre-multicore
 * shared state.
 *)

val make : ('a -> 'b) -> 'a -> 'b
(** Memoizes calls to the supplied function, such that reentrant calls across
 * domains is safe.*)

val make_with_state : Mutex.t -> ('a, 'b) Hashtbl.t -> ('a -> 'b) -> 'a -> 'b
(** Memoizes the given function for concurrent access, given a mutex and
    hashtable.  (This is useful when * the hashtable needs to be explicitly
    exposed outside [make]'s scope, e.g.
    [UTmp.register_temp_file_cleanup_hook].
 *)

val make_with_key_fn : ('a -> 'k) -> ('a -> 'b) -> 'a -> 'b
(** Memoizes calls to the supplied function, such that reentrant calls across
 * domains is safe.  The [key_fn] argument transforms the input argument for
 * the memoizer.
 *
 * (For the curious: a more sensible API would make the key function an optional
 * argument with the default being [Fun.id].  However, there's no way for the
 * type system to unify ['a. 'a -> 'a] and ['a -> 'k].  If you had dependent
 * types you could make the hashtable key type dependent on whether or not
 * key_fn was provided, but sadly we don't have that.)
 *)

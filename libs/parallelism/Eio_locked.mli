type 'a t [@@deriving show]
(** [Eio_locked.t] is a type of locked values compatible with Eio concurrency.
      It is used to ensure that we can store data that can be safely accessed from
      between different fibers and domains maintained by Eio.
    *)

val create : 'a -> 'a t
(** [create data] creates a new [Eio_locked.t] initialized with the given data. *)

val with_lock : 'a t -> (set:('a -> unit) -> 'a -> 'b) -> 'b
(** [with_lock x f] runs the function [f] with the lock held on [x].
      The function [f] is passed a function [set] that can be used to update the
      value of [x], as well as the current value of [x].

      SAFETY: This function _does not compose_.
      Attempting to take a lock on a [Eio_locked.t] from within a function passed to [with_lock]
      will result in a deadlock.

      Uses of `with_lock` should be carefully constructed to be _brief_. You should not
      call any function which may later be refactored to take a lock.
      Ideally, you should call `with_lock` as late as possible, and do no other work within the
      function passed to `with_lock` other
      than either setting the value of the lock or computing another value with the locked value.
    *)

val read : 'a t -> 'a
(** [read x] is a *lock-taking* operation that returns the current value of [x].*)

val set : 'a t -> 'a -> unit
(** [set x v] is a *lock-taking* operation that updates the value of [x] to [v]. *)

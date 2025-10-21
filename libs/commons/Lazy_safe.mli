(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Exists until https://github.com/ocaml-multicore/ocaml-multicore/issues/750
   is resolved *)
(* coupling: ppx_commons.ml, specifically the ppx lazy_safe *)

type 'a t
(** [Lazy_safe.t] is similar to [Lazy.t] and [Eio.Lazy.t]. In fact it is always
    one or the other, depending on if the eio runtime is active or not. This is
    done as [Lazy.t] values are not thread safe, but [Eio.Lazy.t] values do not
    work outside the eio runtime. By using [Lazy_safe.t] you can have a lazy
    value that is safe for eio based parallelism, but works outside eio
    contexts.

    There is also a corresponding ppx: [lazy_safe expr] which works the same as
    [lazy expr]. This means that it does not evaluate [expr], but instead
    creates a lazy thunk that returns that expression.

    Note: [Eio.Lazy.t] values support different cancellation types. The default
    for [Lazy_safe.t] is [`Protect], as that is most similar in behavior to
    [Lazy.t]. See the [Eio.Lazy] documentation for more details.
 *)

val from_val : 'a -> 'a t
(** [from_value x] works the same as [Lazy.from_val], and creates a lazy value
    that will immediately return the value [x] upon calling [Lazy_safe.force] *)

val from_fun : ?cancel:[ `Protect | `Record | `Restart ] -> (unit -> 'a) -> 'a t
(** [from_fun (fun () -> expr)] works the same as [Lazy.from_fun], and will
    evaluate and return [expr] upon the first call to [Lazy_safe.force], and
    will return that value upon every subsequent call.

    There is a corresponding ppx: [lazy_safe expr] which works the same as
    [from_fun (fun () -> expr)] *)

val force : 'a t -> 'a
(** [force lazy] will the lazy value the first time [force] is called, and
    return that value on subsequent calls. The optional [cancel] argument
    controls how cancellation is handled when running inside an Eio context. See
    the [Eio.Lazy.from_fun] documentation for more details. *)

val map :
  ?cancel:[ `Protect | `Record | `Restart ] -> ('a -> 'b) -> 'a t -> 'b t
(** [map f lazy] returns a new lazy value that will apply [f] to the result of
    forcing [lazy]. The optional [cancel] argument controls how cancellation
    is handled when running inside an Eio context. See the [Eio.Lazy] docs for
    more details. *)

val is_val : 'a t -> bool
(** [is_val lazy] returns [true] if [lazy] has already been forced, and [false]
    otherwise. Note that this will always return false when under an eio
    runtime *)

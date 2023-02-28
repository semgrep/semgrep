(*
   Abstract type for a file path within a git project.

   This avoids issues of Unix-style vs. Windows-style paths. All
   git paths use '/' as a separator.
*)

type t = private { string : string; components : string list }

(* A slash-separated path. *)
val of_string : string -> t
val to_string : t -> string

(* Create a path from the list of components. Components may not contain
   slashes. *)
val create : string list -> t
val components : t -> string list

(* Append a component to a path. *)
val append : t -> string -> t

module Ops : sig
  (* Same as append *)
  val ( / ) : t -> string -> t
end

val is_absolute : t -> bool
val is_relative : t -> bool

(* Turn foo/bar into /foo/bar *)
val make_absolute : t -> t

(*
   Syntactic path normalization.
   e.g. foo/../bar -> bar  (even if 'foo/' doesn't exist)

   Fail if the resulting path is absolute and refers to a file above the
   root e.g. '/..' or '/../..'.

   TODO: use filesystem path to the project root to perform a correct
   normalization.
*)
val normalize : t -> (t, string) result
val of_fpath : Fpath.t -> t

(* / *)
val root : t

(*
   Manipulation of Semgrep version info.

   The actual Semgrep version is in a generated file of its own.
   Use the Semver library to parse, print, and compare versions.
*)

type t = Semver.t

(* For ppx dumper. *)
val pp : Format.formatter -> t -> unit
val compare : t -> t -> int
val of_string : string -> t option
val to_string : t -> string

(* The current Semgrep version *)
val version : Semver.t
val major : int
val minor : int
val patch : int
val string : string

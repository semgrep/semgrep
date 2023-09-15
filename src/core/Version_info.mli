(*
   Manipulation of Semgrep version info.

   The actual Semgrep version is in a generated file of its own.
   Use the Semver library to parse, print, and compare versions.
*)

type t [@@deriving show, ord]

(* useful to parse the version stored in the semgrep rules in the
 * min_version and max_version fields
 *)
val of_string : string -> t option

(* useful for reporting errors related to versioning *)
val to_string : t -> string

(* The current Semgrep version (the parsed form of Version.version) *)
val version : t
val major : int
val minor : int
val patch : int

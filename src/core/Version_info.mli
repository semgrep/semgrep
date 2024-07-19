(*
   Manipulation of Semgrep version info.

   The actual Semgrep version is in a generated file of its own.
   Use the Semver library to parse, print, and compare versions.
*)

(* The current Semgrep version (the parsed form of Version.version) *)
val version : Semver.t
val major : int
val minor : int
val patch : int

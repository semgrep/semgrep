(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Temporary module while migrating code to osemgrep to fallback to
 * pysemgrep.
 *
 *)

(*************************************************************************)
(* Entry points *)
(*************************************************************************)

exception FallbackToPysemgrep

(* dispatch back to pysemgrep! *)
let pysemgrep argv =
  (* pysemgrep should be in the PATH, thx to the code in ../../../cli/bin/semgrep *)
  Unix.execvp "pysemgrep" argv

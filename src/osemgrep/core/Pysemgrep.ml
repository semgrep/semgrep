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

exception Fallback

(* dispatch back to pysemgrep! *)
let pysemgrep (caps : < Cap.exec >) argv =
  (* pysemgrep should be in the PATH, thx to the code in ../../../cli/bin/semgrep *)
  CapUnix.execvp caps#exec "pysemgrep" argv

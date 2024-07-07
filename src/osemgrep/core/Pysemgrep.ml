open Common

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Temporary module while migrating code to osemgrep to fallback to
 * pysemgrep when osemgrep does not handle yet certain options.
 *)

(*************************************************************************)
(* Types *)
(*************************************************************************)
exception Fallback

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(* dispatch back to pysemgrep! *)
let pysemgrep (caps : < Cap.exec >) argv =
  Logs.debug (fun m ->
      m "execute pysemgrep: %s"
        (argv |> Array.to_list
        |> List_.map (fun arg -> spf "%S" arg)
        |> String.concat " "));
  (* pysemgrep should be in the PATH, thx to the code in
     ../../../cli/bin/semgrep *)
  CapUnix.execvp caps#exec "pysemgrep" argv

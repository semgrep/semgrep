(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Temporary module while migrating code to osemgrep to fallback to
 * pysemgrep.
 *
 *)

open Common

let tags = Logs_.create_tags [ __MODULE__ ]

(*************************************************************************)
(* Entry points *)
(*************************************************************************)

exception Fallback

(* dispatch back to pysemgrep! *)
let pysemgrep (caps : < Cap.exec >) argv =
  Logs.debug (fun m ->
      m ~tags "execute pysemgrep: %s"
        (argv |> Array.to_list
        |> List_.map (fun arg -> spf "%S" arg)
        |> String.concat " "));
  (* pysemgrep should be in the PATH, thx to the code in
     ../../../cli/bin/semgrep *)
  CapUnix.execvp caps#exec "pysemgrep" argv

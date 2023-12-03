(*
   Various utilities used for testing and are not considered an extension
   of Alcotest (e.g. because it depends on Semgrep-specific libraries).
*)

open Printf

let run what f =
  printf "running %s...\n%!" what;
  Common.protect ~finally:(fun () -> printf "done with %s.\n%!" what) f

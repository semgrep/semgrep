(*
   Various utilities used for testing and are not considered an extension
   of Alcotest (e.g. because it depends on Semgrep-specific libraries).
*)

let run what f =
  UPrintf.printf "running %s...\n%!" what;
  Common.protect ~finally:(fun () -> UPrintf.printf "done with %s.\n%!" what) f

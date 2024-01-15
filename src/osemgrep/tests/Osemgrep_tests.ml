(*
   Osemgrep end-to-end tests.
*)

let tests caps =
  Testo.pack_suites "OSemgrep end-to-end"
    [ Test_osemgrep.tests caps; Test_target_selection.tests caps ]

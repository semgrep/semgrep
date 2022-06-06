(*
   The test suites are defined from files found in the file system relative
   to the current location. Having them created on demand allows running
   'dune utop' from any location.
*)
val tests : unit -> Testutil.test list

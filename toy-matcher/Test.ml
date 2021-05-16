(*
   Test suite for the toy matcher.

   This is meant to check correctness and demonstrate what's possible.
*)

let test_suites : unit Alcotest.test list = [ Matcher.test ]

let main () = Alcotest.run "toy-matcher" test_suites

let () = main ()

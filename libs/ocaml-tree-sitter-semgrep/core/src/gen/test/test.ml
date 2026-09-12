(*
   All the unit tests for this library.
*)

let test_suites : unit Alcotest.test list = [
  Factorize.test;
  Fresh.test;
  Protect_ident.test;
  Rectypes.test;
]

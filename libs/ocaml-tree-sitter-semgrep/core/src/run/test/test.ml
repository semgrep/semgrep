(*
   All the unit tests for this library.
*)

let test_suites : unit Alcotest.test list = [
  Matcher.test;
  Snippet.test;
  Src_file.test;
  Util_string.test;
]

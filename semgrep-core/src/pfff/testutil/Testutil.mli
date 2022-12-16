(*
   Utilities for writing test suites that are compatible with Alcotest.

   Alcotest API:
   https://mirage.github.io/alcotest/alcotest/Alcotest/index.html
*)

(*
   A test is a name and a function that raises exceptions to signal
   test failure.

   There are two main recommended ways of writing a test:

   1. With 'assert false:

   Each test may use 'assert false' to indicate that the test doesn't pass.
   This is the simplest way of failing while also showing the location
   of the failure. When using 'assert false', you should print the expected
   value and actual value if applicable.

   2. With 'Alcotest.(check ...)':

   This is a little nicer because the error messages prints something like
   "Expecting 'foo', got 'bar'". However, this makes tests more complicated
   to write. If the test already prints the expected value and the actual
   value as its output, it's just easier to fail with 'assert false'.

   In any case, Alcotest will capture the output (stdout, stderr) of each
   test and put it in its own file so we can consult it later. Don't
   hesitate to log a lot during the execution of the test.
*)
type test = string * (unit -> unit)

(*
   This extends the name of each test by adding a prefix, using '>'
   as a separator.

   This is because Alcotest only supports two levels of
   nesting. Here we just support one level, but in the end we use '>'
   to split the path to each test, and do the right thing.

   Usage:

     pack_tests "Suite Name" [test1; test2]
     pack_suites "Suite Name" [suite1; suite2; suite3]

   Design note: our use of '>' as a separator is much like '/' in file paths.
   It's a little hackish. A clean alternative is to represent a test path by
   a string list, but it makes writing individual test cases slightly
   more awkward:

     "do something", test_do_something;

   would become

     ["do something"], test_do_something;
*)
val pack_tests : string -> test list -> test list

val pack_suites : string -> test list list -> test list

(*
   Sort tests by path, alphabetically:

     "a>b" comes before "a>c",
     "a>b" come before "a b".

   Non-ascii path components are sorted by byte order, possibly giving
   unexpected results.
*)
val sort : test list -> test list

(*
   Keep only tests whose path matches the path.

   Each path exists in two variants: "a>b>c" (internal)
   and "a > b > c" (as known to alcotest). At least one of them must match
   for the test to be selected.

   The two selection methods right now are:
   - using a substring
   - using a regexp in PCRE (Perl) syntax

   Note that the regular command line exposed by Alcotest.run already
   enables some test filtering.
*)
val filter : ?substring:string -> ?pcre:string -> test list -> test list

(*
   Alcotest.test is a list of Alcotest.test_case, so we need to assign
   a test suite name to each test. This is done as follows:

     "Foo>Bar>hello" -> suite name = "Foo>Bar", test name = "hello"

   The speed level is set to either `Quick or `Slow for all the tests.
   The default is `Quick.

   Basic usage:

     let alcotest_tests = to_alcotest my_quick_tests

   Advanced usage with slow (background) tests:

     let alcotest_tests =
       to_alcotest ~speed_level:`Quick my_quick_tests
       @ to_alcotest ~speed_level:`Slow my_slow_tests
*)
val to_alcotest :
  ?speed_level:Alcotest.speed_level -> test list -> unit Alcotest.test list

(*
   Log a function call. e.g.

     Testutil.run file (fun () -> Parse_java.parse file)

   will log the file name instead of letting us guess which file was being
   parsed.
*)
val run : string -> (unit -> 'a) -> 'a

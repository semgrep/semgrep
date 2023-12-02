(*
   Manage the storage of test statuses and test results.

   We store the following:

   - result for the last run of each test: in a hidden folder
   - captured output for the last run of each test: in a hidden folder
   - expected output for each test: in a persistent folder

   We distinguish two levels of "statuses":

   - test result: the test result before comparing it to expectations:
     * Did it return or raise an exception?
     * What output did we capture?
   - test status: the test result confronted to our expectations:
     * Did the test run at all?
     * Does the test result match our expectations?
*)

(*

Types:

  result = (outcome, output)
  expectation = (outcome, output)
  status = (result, expectation)

Primitives
----------

init_status_workspace : ?path:string -> unit -> unit

get_outcome : test -> outcome
set_outcome : test -> outcome -> unit
clear_outcome : test -> unit

init_expectation_workspace : ?path:string -> unit -> unit

get_output : test -> output option
set_output : test -> output -> unit
clear_output : test -> unit

get_expected_output : test -> output option
set_expected_output : test -> output -> unit
clear_expected_output : test -> unit

Higher-level functions
----------------------

store_result : test -> result -> unit
get_status : test -> status
approve : test -> (unit, string) result
cleanup_unknown_tests : test list -> unit
*)

type captured_output =
  | Ignored
  | Captured_stdout of string
  | Captured_stderr of string
  | Captured_stdout_stderr of string * string
  | Captured_merged of string

type expected_outcome =
  | Should_succeed
  | Should_fail of string (* explain why we expect this test to fail *)

(*
   There are four statuses for a completed test, like in pytest:

   Should_succeed, Succeded -> PASS (good)
   Should_succeed, Failed -> FAIL (bad)
   Should_fail, Failed -> XFAIL (good)
   Should_fail, Succeeded -> XPASS (bad)

   When running a test suite, it's considered a success if all the tests
   complete with status PASS or XFAIL.
*)
type outcome = Succeeded | Failed

type expectation = {
  expected_outcome : expected_outcome;
  expected_output : captured_output;
}

type result = { outcome : outcome; captured_output : captured_output }

(*
   Usually, a successful status is one where both result and expectation
   are not None and are identical. It can be useful to distinguish various
   other statuses such as: missing expectation, missing result, xpass
   (success when expected outcome was Failed), ...
*)
type status = { expectation : expectation option; result : result option }

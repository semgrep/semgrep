(*
   Manage the storage of test statuses and test results.
   Much of this is private to this library.
*)

type captured_output =
  | Ignored
  | Captured_stdout of string
  | Captured_stderr of string
  | Captured_stdout_stderr of string * string
  | Captured_merged of string

type expected_outcome = Should_succeed | Should_fail of string
type outcome = Succeeded | Failed

type expectation = {
  expected_outcome : expected_outcome;
  expected_output : captured_output;
}

type result = { outcome : outcome; captured_output : captured_output }
type status = { expectation : expectation option; result : result option }

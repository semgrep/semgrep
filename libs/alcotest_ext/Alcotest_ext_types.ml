(*
   Mixed bag of types used by most modules of the library, including private
   modules.

   Some of these types are exported by the main module Alcotest_ext.
   Feel free to isolate types in their own modules if it makes things clearer.
*)

(*
   There are four statuses for a completed test, like in pytest:

   Should_succeed, Succeded -> PASS (good)
   Should_succeed, Failed -> FAIL (bad)
   Should_fail, Failed -> XFAIL (good)
   Should_fail, Succeeded -> XPASS (bad)

   When running a test suite, it's considered a success if all the tests
   complete with status PASS or XFAIL.
*)
(* private *)
type outcome = Succeeded | Failed

(* private *)
type captured_output =
  | Ignored
  | Captured_stdout of string
  | Captured_stderr of string
  | Captured_stdout_stderr of string * string
  | Captured_merged of string

(* private *)
type result = { outcome : outcome; captured_output : captured_output }

(* public *)
type expected_outcome =
  | Should_succeed
  | Should_fail of string (* explains why we expect this test to fail *)

(* private? (part of test status) *)
type expectation = {
  expected_outcome : expected_outcome;
  expected_output : captured_output;
}

(*
   Usually, a successful status is one where both result and expectation
   are not None and are identical. It can be useful to distinguish various
   other statuses such as: missing expectation, missing result, xpass
   (success when expected outcome was Failed), ...
*)
(* private? *)
type status = { expectation : expectation option; result : result option }

(* public *)
type output_kind =
  | Ignore_output
  | Stdout
  | Stderr
  | Merged_stdout_stderr
  | Separate_stdout_stderr

(* public *)
type 'a test = {
  (* The ID will be used as a compact key
     for referencing tests in filters and in file names. *)
  id : string;
  category : string list;
  name : string;
  func : unit -> 'a;
  (* Options *)
  expected_outcome : expected_outcome;
  tags : Alcotest_ext_tag.t list;
  speed_level : Alcotest.speed_level;
  output_kind : output_kind;
  skipped : bool;
}

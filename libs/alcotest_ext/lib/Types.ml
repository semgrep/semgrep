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
type outcome = Succeeded | Failed

type captured_output =
  | Ignored of string (* unchecked combined output *)
  | Captured_stdout of string * string (* stdout, unchecked output *)
  | Captured_stderr of string * string (* stderr, unchecked output *)
  | Captured_stdout_stderr of string * string (* stdout, stderr *)
  | Captured_merged of string (* combined output *)

type expected_output =
  | Ignored
  | Expected_stdout of string
  | Expected_stderr of string
  | Expected_stdout_stderr of string * string (* stdout, stderr *)
  | Expected_merged of string (* combined output *)

type result = { outcome : outcome; captured_output : captured_output }

type expected_outcome =
  | Should_succeed
  | Should_fail of string (* explains why we expect this test to fail *)

(*
   The expected output is optional so as to allow new tests for which
   there's no expected output yet but there's an expected outcome defined
   in the test suite.
*)
type expectation = {
  expected_outcome : expected_outcome;
  expected_output : (expected_output, string list (* missing files *)) Result.t;
}

(*
   Usually, a successful status is one where both result and expectation
   are not None and are identical. It can be useful to distinguish various
   other statuses such as: missing expectation, missing result, xpass
   (success when expected outcome was Failed), ...
*)
type status = {
  expectation : expectation;
  result : (result, string list (* missing files *)) Result.t;
}

(* A summary of the 'status' object using the same language as pytest.

   PASS: expected success, actual success
   FAIL: expected success, actual failure
   XFAIL: expected failure, actual failure
   XPASS: expected failure, actual success
   MISS: missing data

   Maximum string length for display: 5 characters
*)
type status_class = PASS | FAIL | XFAIL | XPASS | MISS

type status_summary = {
  status_class : status_class;
  has_expected_output : bool;
}

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
     for referencing tests in filters and in file names.
     It's a hash of the internal full name. Both must be unique. *)
  id : string;
  (* 'internal_full_name' is derived from 'category' and 'name' and is not
     expected to change. It may be used for display purposes but we could
     choose to display the test name differently in the future. *)
  internal_full_name : string;
  category : string list;
  name : string;
  func : unit -> 'a;
  (* Options *)
  expected_outcome : expected_outcome;
  tags : Tag.t list;
  speed_level : Alcotest.speed_level;
  mask_output : (string -> string) list;
  output_kind : output_kind;
  skipped : bool;
  tolerate_chdir : bool;
}

type 'a test_with_status = 'a test * status * status_summary

(* TODO: move to a module that has an mli? *)
(* "path > to > name" *)
let recompute_internal_full_name (test : _ test) =
  String.concat " > " (test.category @ [ test.name ])

(*
   Compare the captured output that is checked and ignore the unchecked output.
*)
let equal_checked_output (a : expected_output) (b : captured_output) =
  match (a, b) with
  | Ignored, Ignored _ -> true
  | Expected_stdout out, Captured_stdout (out2, _) -> String.equal out out2
  | Expected_stderr err, Captured_stderr (err2, _) -> String.equal err err2
  | Expected_merged data, Captured_merged data2 -> String.equal data data2
  | Expected_stdout_stderr (out, err), Captured_stdout_stderr (out2, err2) ->
      String.equal out out2 && String.equal err err2
  | _ -> false

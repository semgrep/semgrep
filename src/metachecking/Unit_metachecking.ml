open Common
open File.Operators
open Testutil
module E = Core_error

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Unit (and integration) tests exercising the metachecker.
 *)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* alt: we could split the purely-syntactical parsing error checks
 * from the metachecker checks, but simpler to consider all of that
 * as just errors.
 *)
let metachecker_checks_tests () =
  pack_tests "metachecker checks testing"
    (let dir = tests_path / "errors" in
     let files = Common2.glob (spf "%s/*.yaml" !!dir) in
     files
     |> Common.map (fun file ->
            let file = Fpath.v file in
            ( Fpath.basename file,
              fun () ->
                E.g_errors := [];
                E.try_with_exn_to_error !!file (fun () ->
                    let rules = Parse_rule.parse file in
                    rules
                    |> List.iter (fun rule ->
                           let errs = Check_rule.check rule in
                           E.g_errors := errs @ !E.g_errors));
                let actual = !E.g_errors in
                let expected = E.expected_error_lines_of_files [ !!file ] in
                E.compare_actual_to_expected_for_alcotest actual expected )))

(* Test the entire `-test_check` path *)
let metachecker_regression_tests () =
  [
    ( "metachecker regresion testing",
      fun () ->
        let path = tests_path / "metachecks" in
        Test_metachecking.test_rules ~unit_testing:true [ path ] );
  ]

(*****************************************************************************)
(* All tests *)
(*****************************************************************************)

let tests () =
  List.flatten [ metachecker_checks_tests (); metachecker_regression_tests () ]

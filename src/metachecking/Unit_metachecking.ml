open Common
open Fpath_.Operators
module E = Core_error
module TCM = Test_compare_matches

let t = Testo.create

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
  Testo.categorize "metachecker checks testing"
    (let dir = tests_path / "errors" in
     let files = Common2.glob (spf "%s/*.yaml" !!dir) in
     files
     |> List_.map (fun file ->
            let file = Fpath.v file in
            t (Fpath.basename file) (fun () ->
                (* note that try_with_exn_to_error also modifies g_errors *)
                E.try_with_exn_to_error file (fun () ->
                    let rules = Parse_rule.parse file in
                    rules
                    |> List.iter (fun rule ->
                           let errs = Check_rule.check rule in
                           E.g_errors := errs @ !E.g_errors));
                let actual = !E.g_errors in
                E.g_errors := [];
                let expected = TCM.expected_error_lines_of_files [ file ] in
                TCM.compare_actual_to_expected_for_alcotest actual expected)))

(* Test the entire `-test_check` path *)
let metachecker_regression_tests caps =
  [
    t "metachecker regression testing" (fun () ->
        let path = tests_path / "metachecks" in
        Test_metachecking.test_rules ~unit_testing:true caps [ path ]);
  ]

(*****************************************************************************)
(* All tests *)
(*****************************************************************************)

let tests caps =
  List.flatten
    [ metachecker_checks_tests (); metachecker_regression_tests caps ]

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
  Testo.categorize "metachecker"
    (let dir = tests_path / "errors" in
     let files = Common2.glob (spf "%s/*.yaml" !!dir) in
     files
     |> List_.map (fun file ->
            let file = Fpath.v file in
            t (Fpath.basename file) (fun () ->
                let actual =
                  match Parse_rule.parse file with
                  | Error e -> [ Core_error.error_of_rule_error e ]
                  | Ok rules ->
                      rules
                      |> List.concat_map (fun rule -> Check_rule.check rule)
                in
                let expected = TCM.expected_error_lines_of_files [ file ] in
                TCM.compare_actual_to_expected_for_alcotest
                  ~to_location:TCM.location_of_core_error actual expected)))

(* Test the entire `-test_check` path *)
let metachecker_regression_tests caps =
  [
    t "metachecker" (fun () ->
        let path = tests_path / "metachecks" in
        Test_metachecking.test_rules ~unit_testing:true caps [ path ]);
  ]

(*****************************************************************************)
(* All tests *)
(*****************************************************************************)

let tests caps =
  List_.flatten
    [ metachecker_checks_tests (); metachecker_regression_tests caps ]

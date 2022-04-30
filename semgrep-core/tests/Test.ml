
(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Unit tests entry point.
 *
 * From semgrep-core you can do
 *
 *   $./test test foo
 *
 * to run all the tests containing foo in their description.
 *)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let any_gen_of_string str =
  let any = Parse_python.any_of_string str in
  Python_to_generic.any any

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)
(* This file used to contain lots of tests, but it's better to now
 * distribute them in their relevant directory (e.g., engine/Unit_engine.ml)
 *)

(*****************************************************************************)
(* All tests *)
(*****************************************************************************)

let tests = List.flatten [
  Unit_parsing.tests;

  Unit_entropy.tests;
  Unit_ReDoS.tests;

  Unit_guess_lang.tests;
  Unit_memory_limit.tests;
  Unit_pcre_settings.tests;

  Unit_synthesizer.tests;
  Unit_synthesizer_targets.tests;

  Unit_dataflow.tests Parse_target.parse_program;
  Unit_typing_generic.tests
    Parse_target.parse_program
    (fun lang file -> Parse_pattern.parse_pattern lang file);
  Unit_naming_generic.tests Parse_target.parse_program;

  (* just expression vs expression testing for one language (Python) *)
  Unit_matcher.tests ~any_gen_of_string;
  (* TODO Unit_matcher.spatch_unittest ~xxx *)
  (* TODO Unit_matcher_php.unittest; (* sgrep, spatch, refactoring, unparsing *) *)
  Unit_engine.tests;
]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main () =
  let alcotest_tests = Testutil.to_alcotest tests in
  Alcotest.run "semgrep-core" alcotest_tests

let () = main ()

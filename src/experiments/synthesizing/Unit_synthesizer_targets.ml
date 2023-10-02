open Common
module R = Rule
module E = Core_error
module Out = Semgrep_output_v1_t

(* ran from the root of the semgrep repository *)
let test_path = "tests/synthesizing/targets/"

(* Format: file, list of target ranges, expected pattern line. *)
let stmt_tests =
  [
    (* Motivating example. *)
    ("hello.py", [ "2:0-2:5"; "5:0-5:8" ], "$X(3, ...)");
    (* Single statement. *)
    ("string_ellipsis.py", [ "2:0-2:13"; "5:0-5:13" ], "foo(\"...\")");
    (* Three targets. *)
    ( "string_ellipsis.py",
      [ "2:0-2:13"; "5:0-5:13"; "8:0-8:13" ],
      "foo(\"...\")" );
  ]

let statement_list_tests =
  [
    (* Motivating example for statement lists, Python version. *)
    ("equal_length_assign_call.py", [ "1:0-2:5"; "4:0-5:5" ], "$X = a\nfoo($X)");
    (* Motivating example for statement lists, JS version. *)
    ( "equal_length_assign_call.js",
      [ "1:0-2:7"; "4:0-5:7" ],
      "$X = req.query.foo;\nexec($X);" );
  ]

(* Tests that fail due to limitations of Pattern_for_Targets, where it would
 * be great if they passed.
 *)
let todo_tests =
  [
    (* x + 1, y + 1, z + 1. Fails because addition looks like function call. *)
    ("addition_constant.py", [ "1:6-1:10"; "2:6-2:10"; "3:6-3:10" ], "x + $X");
    (* Would fail with ... because Pattern_from_Targets doesn't have replacement
       strategy for DefStmts.
    *)
    ("vardef.js", [ "1:0-1:10" ], "var x = 42;");
  ]

(* Range.t does not derive eq *)
let compare_range (r1 : Range.t) (r2 : Range.t) : bool =
  r1.start =|= r2.start && r1.end_ =|= r2.end_

let parse_file lang file : AST_generic.program =
  Parse_target.parse_and_resolve_name_fail_if_partial lang file

let extract_range (m : Pattern_match.t) : Range.t =
  let start_token_loc, end_token_loc = m.range_loc in
  Range.range_of_token_locations start_token_loc end_token_loc

(* Evaluates to the ranges that a pattern matches in a file. *)
let ranges_matched lang file pattern : Range.t list =
  let ast = parse_file lang file in
  let rule =
    {
      Mini_rule.id = Rule_ID.of_string "unit-testing";
      pattern;
      inside = false;
      message = "";
      severity = Pattern_match.Error;
      langs = [ lang ];
      pattern_string = "test: no need for pattern string";
      fix = None;
    }
  in
  let equiv = [] in
  (* Are equivalences necessary for this? *)
  let matches =
    Match_patterns.check
      ~hook:(fun { Pattern_match.tokens = (lazy xs); _ } ->
        let toks = xs |> List.filter Tok.is_origintok in
        let minii, _maxii = Tok_range.min_max_toks_by_pos toks in
        let minii_loc = Tok.unsafe_loc_of_tok minii in
        E.error
          (Rule_ID.of_string "Synthesizer-tests")
          minii_loc "" Out.SemgrepMatchFound)
      (Rule_options.default_config, equiv)
      [ rule ] (file, lang, ast)
  in
  Common.map extract_range matches

let run_single_test file linecols expected_pattern =
  let lang, _, inferred_pattern =
    Synthesizer.generate_pattern_from_targets Rule_options.default_config
      (linecols @ [ file ])
  in
  let actual_pattern =
    Pretty_print_pattern.pattern_to_string lang inferred_pattern
  in
  let pattern_correct = actual_pattern = expected_pattern in
  let ranges_expected =
    Common.map (fun lcs -> Range.range_of_linecol_spec lcs file) linecols
  in
  let ranges_actual = ranges_matched lang file inferred_pattern in
  let ranges_correct =
    List.for_all
      (fun r -> List.exists (compare_range r) ranges_actual)
      ranges_expected
  in
  Alcotest.(check bool)
    (spf "actual pattern:\n%s\n\nexpected pattern:\n%s\n" actual_pattern
       expected_pattern)
    true pattern_correct;
  Alcotest.(check bool) "ranges should match" true ranges_correct

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests =
  [
    ( "pattern from targets",
      fun () ->
        stmt_tests @ statement_list_tests
        |> List.iter (fun (file, linecols, expected_pattern) ->
               run_single_test (test_path ^ file) linecols expected_pattern) );
  ]

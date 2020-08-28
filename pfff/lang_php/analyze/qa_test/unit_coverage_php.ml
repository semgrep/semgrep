open Common

open Coverage_tests_php
module Json_out = Json_io

(*****************************************************************************)
(* Unit test *)
(*****************************************************************************)
open OUnit

(* 
 * The test coverage analysis of pfff we do for facebook depends on
 * multiple components:
 * 
 *  - xdebug, and our ocaml binding to the runner and trace format
 *  - phpunit, and our ocaml binding to the runner and result format
 *  - some facebook extensions to phpunit and the way we run tests
 *  - some facebook specificities because of flib
 *  - the way we run php (zend or hphp)
 *  - MPI
 * 
 * If we want to unit tests, we need to remove from the equations a few
 * things. For instance we can get a trace without having to conform to
 * phpunit or to the test runner scripts we use by having
 * the coverage analysis function taking the specifics as 
 * parameters. So here we go into different steps:
 * 
 *  - first bypass almost everything (the conformance to phpunit, 
 *    to our test infrastructure), and instead just execute php code
 *    under xdebug with the basic php interpreter.
 *  - TODO introduce phpunit, and the fact that we bypass failing tests
 *  - TODO introduce facebook specificities
 * 
 * UPDATE: this is now obsolete. See facebook/qa_test/unit_qa_test.ml
 * which use HPHP instead of Xdebug.
 *)

(* shortcut *)
let p f = Common.fullpath (Config_pfff.path ^ "/tests/php/coverage/" ^ f)

(* mocking *)
let fake_phpunit_parse_trace file _output = {
  Phpunit.t_file = file;
  t_status = Phpunit.Pass (1, 0);
  t_time = 0;
  t_memory = 0.0;
  t_shimmed = 0;
  t_trace_nb_lines = 0;
}

(* normally we need to include the command of a test runner,
 * such as bin/phpunit, but here we don't even use phpunit.
 * Just raw call to php *)
let fake_php_cmd_run_test ~php_interpreter file = 
  spf "%s %s" php_interpreter file


let unittest = "coverage_php" >::: [

  (* Here we test the correctness of the the basic coverage algorithm 
   * and xdebug trace analysis. The test data is in tests/coverage.
   * It is mainly 2 sources file, a.php and b.php doing each 
   * a loop to print a character 1000 times. Those 2 files are
   * exercised by tests in the same directory, and whose name,
   * e.g. t1_a.php explains which files they use.
   *)
  "simple coverage" >:: (fun () ->

    OUnit.skip_if true "xdebug obsolete";

    let all_test_files () = 
      [ p "t1_a.php";
        p "t2_b.php";
        p "t3_a_b.php";
      ]
    in

    let cover, pbs = 
      coverage_tests 
        ~all_test_files
        ~php_cmd_run_test:fake_php_cmd_run_test
        ~phpunit_parse_trace:fake_phpunit_parse_trace
        ()
    in
    let json = Coverage_code.json_of_tests_coverage cover in
    let s = Json_out.string_of_json json in
    pr s;

    assert_equal [] pbs;

    let cover_a = List.assoc (p "a.php") cover in
    let cover_b = List.assoc (p "b.php") cover in

    assert_equal [p "t1_a.php"; p "t3_a_b.php"] (Common2.keys cover_a);
    assert_equal [p "t2_b.php"; p "t3_a_b.php"] (Common2.keys cover_b);

    (* Right now if a function calls another function in a loop, then
     * those functions calls will occcur a lot in the trace, which
     * currently will bias our coverage ranking to favor such case.
     * 
     * todo? maybe should remember which lines were covered so that
     * we favor not code with loop but code whose lines are
     * all exercised by a test.
     *)
        assert_bool 
          "t1_a.php exercises a lot a.php; score should be high"
          (List.assoc (p "t1_a.php") cover_a > 90.0); 

        assert_bool 
          "t3_a_b.php calls a() 2x times more than b(), score of a should be higher"
          (List.assoc (p "t3_a_b.php") cover_a > 
          List.assoc (p "t3_a_b.php") cover_b ); 
  );


  (* We don't want to include in the coverage tests that exercise
   * a file only through include or certain calls such as 
   * require_module()
   *)
  "coverage skipping calls" >:: (fun () ->
    OUnit.skip_if true "xdebug obsolete";

    let all_test_files () = [ 
      p "t4_only_require.php";
      p "t5_not_just_require.php";
    ]
    in
    let skip_require_module_calls call = 
      match call.Xdebug.f_call with
      | Xdebug.FunCall "require_module" -> true
      | _ -> false
    in

    let cover, pbs = 
      coverage_tests 
        ~all_test_files
        ~php_cmd_run_test:fake_php_cmd_run_test
        ~phpunit_parse_trace:fake_phpunit_parse_trace
        ~skip_call:skip_require_module_calls
        ()
    in
    let json = Coverage_code.json_of_tests_coverage cover in
    let s = Json_out.string_of_json json in
    pr s;

    assert_equal [] pbs;
    let cover_require_ex = List.assoc (p "require_ex.php") cover in
    

    assert_bool 
      "t4_only_require.php should not cover require_ex.php"
      (not (List.mem_assoc (p "t4_only_require.php") cover_require_ex)); 
    
    assert_bool 
      "t5_not_just_require.php should cover require_ex.php"
      (List.mem_assoc (p "t5_not_just_require.php") cover_require_ex); 

  );

  "coverage and json input output" >:: (fun () ->
    assert_bool
      "should parse good_trace.json"
      (let _ = Coverage_code.load_tests_coverage (p "good_trace.json") in true);
    assert_bool
      "should generate exn on bad_trace.json"
      (try let _ = 
        Coverage_code.load_tests_coverage (p "bad_trace.json") in false
       with _exn -> true
      );
  );
]

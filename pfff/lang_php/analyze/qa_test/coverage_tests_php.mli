
(* see also h_program-lang/coverage_code.mli *)

val threshold_working_tests_percentage : float ref

exception NotEnoughWorkingTests

(* Compute coverage information of a set of phpunit test files. 
 * 
 * Note that because this function internally use Xdebug, 
 * the filenames in the coverage data are in a realpath format.
 * 
 * Note also that this function can leverage MPI (which is why
 * it takes the set of test files inside a closure, see 
 * Distribution.map_reduce_lazy).
 * 
 * This may raise NotEnoughWorkingTests.
 *)

val coverage_tests :
  ?phpunit_parse_trace:(Common.filename -> string list -> Phpunit.test_result)->
  ?skip_call:(Xdebug.call_trace -> bool) ->
  php_cmd_run_test:(php_interpreter:string ->
                     Common.filename -> string) ->
  all_test_files:(unit -> Common.filename list) ->
  unit ->

  Coverage_code.tests_coverage * 
  (Common.filename * string (* error *)) list (* tests with pbs *)


val lines_coverage_from_tests:  
  ?skip_call:(Xdebug.call_trace -> bool) ->
  ?is_directive_to_filter:(string -> bool) ->
  php_cmd_run_test:(php_interpreter:string -> Common.filename -> string) ->
  all_test_files:Common.filename list ->
  all_files:Common.filename list ->
  unit ->
  Coverage_code.lines_coverage


(* for percentage statistics per file *)
val get_all_calls: 
  ?is_directive_to_filter:(string -> bool) -> Cst_php.any -> 
  (string option * Cst_php.tok) list

val get_all_call_lines_with_sanity_check:
  ?is_directive_to_filter:(string -> bool) ->
  Common.filename -> int list (* covered line according to xdebug *) ->
  int list

val actions: unit -> Common.cmdline_actions

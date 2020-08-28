(*s: phpunit.mli *)

val is_phpunit_derived_class_heuristics: 
  Cst_php.class_def -> bool

(* todo: not yet implemented *)
val is_phpunit_derived_class: 
  Cst_php.class_def -> (*Database_php.database -> *) bool

val find_testcase_class_if_any: 
  is_phpunit_base_class_name:(string -> bool) ->
  Cst_php.toplevel list -> Cst_php.class_def option


(* a poor's man test result database ... *)

type test_status = 
  | Pass of int (* nb passing tests *) * int (* skipped *)
  | Fail of int (* fail *) * int (* pass *)
  | Fatal of string

type test_result = {
  t_file: Common.filename;

  t_status: test_status;

  t_time: int; (* seconds *)
  t_memory: float; (* Mb *)
  t_trace_nb_lines: int;

  t_shimmed: int; (* # of "Creating new database shim" messages *)
}

val s_of_test_status: test_status -> string

(* 
 * When we run PHPUnit tests in a set of files, a single tests trace
 * in a file will contain many test result entries. The first arg below
 * is the trace file and the strings are the partial content of this
 * file regarding one PHPUnit test file. We pass the filename mainly
 * for error reporting and because of the needed t_file field. 
 *)
val parse_one_trace: 
  Common.filename -> string list -> test_result

(* a discrete view over t_time *)
type test_speed = 
  | Fast
  | NormalSpeed
  | Slow

(* a discrete view over t_memory *)
type test_space = 
  | SmallMem
  | NormalMem
  | BigMem

val threshold_slow_seconds : int ref
val threshold_normal_seconds : int ref
val threshold_big_mem : float ref
val threshold_normal_mem : float ref

val test_speed_of_int: int -> test_speed
val test_space_of_float: float -> test_space

(* 
 * For regression testing we need to save the state of a past run in a file,
 * a regression file in PFFF_HOME/tmp/ for instance. We want the filename
 * to be dependent of the phpunit test "query", so that the next test run 
 * with the same arguments will be compared to the appropriate regression
 * file.
 * 
 * ex: gen_regression_filename ["foo/"; "bar/test.php"] will generate
 * a filename like foo__bar__test_php_245ghhm.marshalled
 *)
val gen_regression_filename: 
  Common.filename list -> string

val regression: 
  regression_file:Common.filename -> test_result list -> unit
val regression_perf: 
  regression_file:Common.filename -> test_result list -> unit

val final_report: 
  ?report_also_pass:bool ->
  test_result list -> unit

(* to load/save test results, again poor's man database persistence *)
val json_of_test_results: test_result list -> Json_type.t
val test_results_of_json: Json_type.t -> test_result list

(*e: phpunit.mli *)

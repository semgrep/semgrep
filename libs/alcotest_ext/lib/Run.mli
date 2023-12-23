(*
   Filter and run tests
*)

type status_output_style = Short | Full

(* Type alias for Alcotest test cases *)
type 'unit_promise alcotest_test_case =
  string * Alcotest.speed_level * (unit -> 'unit_promise)

(* Type alias for an Alcotest 'test'. *)
type 'unit_promise alcotest_test =
  string * 'unit_promise alcotest_test_case list

(* See comments in the public interface Alcotest_ext.mli *)
val to_alcotest :
  'unit_promise Types.test list -> 'unit_promise alcotest_test list

val run_tests :
  mona:'unit_promise Mona.t ->
  filter_by_substring:string option ->
  lazy_:bool ->
  show_output:bool ->
  'unit_promise Types.test list ->
  (int -> 'unit_promise Types.test_with_status list -> _) ->
  _

(* Print the status of each test.
   Return a non-zero exit status if any of the tests is not a success
   (PASS or XFAIL). *)
val list_status :
  filter_by_substring:string option ->
  output_style:status_output_style ->
  show_output:bool ->
  'unit_promise Types.test list ->
  int * 'unit_promise Types.test_with_status list

val approve_output : ?filter_by_substring:string -> _ Types.test list -> int

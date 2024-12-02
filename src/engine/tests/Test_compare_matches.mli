(* Helpers for unit testing *)

val location_of_pm : Core_match.t -> Fpath.t * int
val location_of_core_error : Core_error.t -> Fpath.t * int

(* extract all the lines with ERROR: comment in test files *)
val expected_error_lines_of_files :
  ?regexp:string (* default to default_error_regexp below *) ->
  ?ok_regexp:string ->
  Fpath.t list ->
  (Fpath.t * int) (* line with ERROR *) list

(* ".*\\(ERROR\\|MATCH\\):" *)
val default_error_regexp : string

(* Return the number of errors and an error message, if there's any error. *)
val compare_actual_to_expected :
  to_location:('a -> Fpath.t * int) ->
  'a list ->
  (Fpath.t * int) list ->
  (unit, int * string) result

(* Call Alcotest.fail in case of errors *)
val compare_actual_to_expected_for_alcotest :
  to_location:('a -> Fpath.t * int) -> 'a list -> (Fpath.t * int) list -> unit

(* Helpers for unit testing *)

(* extract all the lines with ERROR: comment in test files *)
val expected_error_lines_of_files :
  ?regexp:string ->
  ?ok_regexp:string option ->
  Fpath.t list ->
  (Fpath.t * int) (* line with ERROR *) list

(* Return the number of errors and an error message, if there's any error. *)
val compare_actual_to_expected :
  Core_error.t list -> (Fpath.t * int) list -> (unit, int * string) result

(* Call Alcotest.fail in case of errors *)
val compare_actual_to_expected_for_alcotest :
  Core_error.t list -> (Fpath.t * int) list -> unit

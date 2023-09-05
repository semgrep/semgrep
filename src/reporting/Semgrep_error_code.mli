(*
   Exception and error management for semgrep-core
*)

(*****************************************************************************)
(* Main error type *)
(*****************************************************************************)

type error = {
  rule_id : Rule_ID.t option;
  typ : Semgrep_output_v1_t.core_error_kind;
  loc : Tok.location;
  msg : string;
  (* ?? diff with msg? *)
  details : string option;
}
[@@deriving show]

val g_errors : error list ref

(*****************************************************************************)
(* Converter functions *)
(*****************************************************************************)

val mk_error :
  Rule_ID.t option ->
  Tok.location ->
  string ->
  Semgrep_output_v1_t.core_error_kind ->
  error

val error :
  Rule_ID.t ->
  Tok.location ->
  string ->
  Semgrep_output_v1_t.core_error_kind ->
  unit

(* Convert an invalid rule into an error.
   TODO: return None for rules that are being skipped due to version
   mismatches.
*)
val error_of_invalid_rule_error : Rule.invalid_rule_error -> error

(* Convert a caught exception and its stack trace to a Semgrep error. *)
val exn_to_error : Rule_ID.t option -> Common.filename -> Exception.t -> error

(*****************************************************************************)
(* Try with error *)
(*****************************************************************************)

val try_with_exn_to_error : Common.filename -> (unit -> unit) -> unit
val try_with_print_exn_and_reraise : Common.filename -> (unit -> unit) -> unit

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

val string_of_error : error -> string

val severity_of_error :
  Semgrep_output_v1_t.core_error_kind -> Semgrep_output_v1_t.core_severity

(*****************************************************************************)
(* Helpers for unit testing *)
(*****************************************************************************)

(* extract all the lines with ERROR: comment in test files *)
val expected_error_lines_of_files :
  ?regexp:string ->
  ?ok_regexp:string option ->
  Common.filename list ->
  (Common.filename * int) (* line with ERROR *) list

(* Return the number of errors and an error message, if there's any error. *)
val compare_actual_to_expected :
  error list -> (Common.filename * int) list -> (unit, int * string) result

(* Call Alcotest.fail in case of errors *)
val compare_actual_to_expected_for_alcotest :
  error list -> (Common.filename * int) list -> unit

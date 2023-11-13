(*
   Exception and error management for semgrep-core
*)

(*****************************************************************************)
(* Main error type *)
(*****************************************************************************)

type t = {
  rule_id : Rule_ID.t option;
  typ : Semgrep_output_v1_t.error_type;
  loc : Tok.location;
  msg : string;
  (* ?? diff with msg? *)
  details : string option;
}
[@@deriving show]

val g_errors : t list ref

module ErrorSet : Set.S with type elt = t

(*****************************************************************************)
(* Converter functions *)
(*****************************************************************************)

val mk_error :
  Rule_ID.t option ->
  Tok.location ->
  string ->
  Semgrep_output_v1_t.error_type ->
  t

val error :
  Rule_ID.t -> Tok.location -> string -> Semgrep_output_v1_t.error_type -> unit

(* Convert an invalid rule into an error.
   TODO: return None for rules that are being skipped due to version
   mismatches.
*)
val error_of_invalid_rule_error : Rule.invalid_rule_error -> t

(* Convert a caught exception and its stack trace to a Semgrep error.
 * See also JSON_report.json_of_exn for non-target related exn handling.
 *)
val exn_to_error : Rule_ID.t option -> string (* filename *) -> Exception.t -> t

(*****************************************************************************)
(* Try with error *)
(*****************************************************************************)

val try_with_exn_to_error : string (* filename *) -> (unit -> unit) -> unit

val try_with_print_exn_and_reraise :
  string (* filename *) -> (unit -> unit) -> unit

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

val string_of_error : t -> string

val severity_of_error :
  Semgrep_output_v1_t.error_type -> Semgrep_output_v1_t.error_severity

(*****************************************************************************)
(* Helpers for unit testing *)
(*****************************************************************************)

(* extract all the lines with ERROR: comment in test files *)
val expected_error_lines_of_files :
  ?regexp:string ->
  ?ok_regexp:string option ->
  string (* filename *) list ->
  (string (* filename *) * int) (* line with ERROR *) list

(* Return the number of errors and an error message, if there's any error. *)
val compare_actual_to_expected :
  t list -> (string (* filename *) * int) list -> (unit, int * string) result

(* Call Alcotest.fail in case of errors *)
val compare_actual_to_expected_for_alcotest :
  t list -> (string (* filename *) * int) list -> unit

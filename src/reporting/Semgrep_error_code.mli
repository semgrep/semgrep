(*
   Exception and error management for semgrep-core
*)

(*****************************************************************************)
(* Main error type *)
(*****************************************************************************)

type error = {
  rule_id : Rule_ID.t option;
  typ : Output_from_core_t.core_error_kind;
  loc : Tok.location;
  msg : string;
  (* ?? diff with msg? *)
  details : string option;
}
[@@deriving show]

type error_class =
  | Error of error
  (* Specific kind of error condition that needs more info than simple errors
     to be actionable. *)
  | Incompatible_rule of Semgrep_output_v1_t.incompatible_rule

val g_errors : error list ref
val g_incompatible_rules : Semgrep_output_v1_t.incompatible_rule list ref

(*****************************************************************************)
(* Converter functions *)
(*****************************************************************************)

val mk_error :
  ?rule_id:Rule_ID.t option ->
  Tok.location ->
  string ->
  Output_from_core_t.core_error_kind ->
  error

val error :
  Rule_ID.t ->
  Tok.location ->
  string ->
  Output_from_core_t.core_error_kind ->
  unit

(* Convert a caught exception and its stack trace to a Semgrep error. *)
val exn_to_error_class :
  ?rule_id:Rule_ID.t option -> Common.filename -> Exception.t -> error_class

(* Turn a single exceptions into lists of the different classes of errors,
   ready to be injected in a response record. *)
val exn_to_error_lists :
  ?rule_id:Rule_ID.t option ->
  Common.filename ->
  Exception.t ->
  error list * Semgrep_output_v1_t.incompatible_rule list

(*****************************************************************************)
(* Try with error *)
(*****************************************************************************)

val try_with_exn_to_error_class : Common.filename -> (unit -> unit) -> unit
val try_with_print_exn_and_reraise : Common.filename -> (unit -> unit) -> unit

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

val string_of_error : error -> string

val severity_of_error :
  Output_from_core_t.core_error_kind -> Output_from_core_t.core_severity

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

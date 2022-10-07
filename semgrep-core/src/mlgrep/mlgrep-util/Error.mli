(*
   Raise structured exceptions when an error occurs, to be caught
   and reported before exiting the process.

   The exception printer is already registered and will print the errors
   in a user-friendly manner when 'Printexc.to_string' is invoked.
*)

(* Error object that holds all kinds of errors. *)
type t
type exit_code = Exit_code of int

type level =
  | Warn (* always an error *)
  | Error (* only an error if "strict" is set *)

exception Semgrep_error of t

(*
   Fields needed to construct some kinds of error objects.
*)
type details = {
  short_msg : string;
  long_msg : string option;
  spans : Rule_lang.span list;
  help : string option;
}

(*
   Standard exit codes.
   All calls to exit must use one of these.
*)
val ok_exit_code : exit_code
val findings_exit_code : exit_code
val fatal_exit_code : exit_code
val invalid_pattern_exit_code : exit_code
val unparseable_yaml_exit_code : exit_code
val missing_config_exit_code : exit_code
val invalid_language_exit_code : exit_code
val invalid_api_key_exit_code : exit_code
val scan_fail_exit_code : exit_code
val not_implemented_in_mlgrep : exit_code

(*
   Create error exceptions describing the errors encountered.
*)
val semgrep_core_error :
  code:exit_code -> level:level -> Output_from_core_t.core_error -> t

val invalid_rule_schema_error : details -> t
val unknown_language_error : details -> t

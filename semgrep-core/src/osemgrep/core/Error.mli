(*
   Raise structured exceptions when an error occurs, to be caught
   and reported before exiting the process.

   The exception printer is already registered and will print the errors
   in a user-friendly manner when 'Printexc.to_string' is invoked.
*)

(* Error object that holds all kinds of errors. *)
type t

type level =
  | Warn (* always an error *)
  | Error (* only an error if "strict" is set *)

(* for the CLI JSON output *)
val string_of_level : level -> string

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
   Create error exceptions describing the errors encountered.
*)
val semgrep_core_error :
  code:Exit_code.t -> level:level -> Output_from_core_t.core_error -> t

val invalid_rule_schema_error : details -> t
val unknown_language_error : details -> t

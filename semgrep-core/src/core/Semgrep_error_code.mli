type error = {
  rule_id : Rule.rule_id option;
  typ : Output_from_core_t.core_error_kind;
  loc : Parse_info.token_location;
  msg : string;
  details : string option;
  yaml_path : string list option;
}

type severity = Error | Warning

val g_errors : error list ref
val options : unit -> Common.cmdline_options

(*****************************************************************************)
(* Convertor functions *)
(*****************************************************************************)

val mk_error :
  ?rule_id:Rule.rule_id option ->
  Parse_info.token_location ->
  string ->
  Output_from_core_t.core_error_kind ->
  error

val error :
  Rule.rule_id ->
  Parse_info.token_location ->
  string ->
  Output_from_core_t.core_error_kind ->
  unit

val exn_to_error :
  ?rule_id:Rule.rule_id option -> Common.filename -> exn -> error

(*****************************************************************************)
(* Try with error *)
(*****************************************************************************)

val try_with_exn_to_error : Common.filename -> (unit -> unit) -> unit
val try_with_print_exn_and_reraise : Common.filename -> (unit -> unit) -> unit

(*
   Print exception and exit with code 2. No stack trace is printed because
   it takes 2 seconds in some instances.
*)
val try_with_print_exn_and_exit_fast : Common.filename -> (unit -> unit) -> unit

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

val string_of_error : error -> string
val severity_of_error : Output_from_core_t.core_error_kind -> severity

(*****************************************************************************)
(* Helpers for unit testing *)
(*****************************************************************************)

(* extract all the lines with ERROR: comment in test files *)
val expected_error_lines_of_files :
  ?regexp:string ->
  Common.filename list ->
  (Common.filename * int) (* line with ERROR *) list

(*
   Return the number of errors and an error message, if there's any error.
*)
val compare_actual_to_expected :
  error list -> (Common.filename * int) list -> (unit, int * string) result

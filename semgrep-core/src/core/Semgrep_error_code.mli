type error = {
  typ : error_kind;
  loc : Parse_info.token_location;
  sev : severity;
}

and severity = Error | Warning | Info

and error_kind =
  (* parsing related errors.
   * See also try_with_exn_to_errors(), try_with_error_loc_and_reraise(), and
   * filter_maybe_parse_and_fatal_errors
   *)
  | LexicalError of string
  | ParseError (* aka SyntaxError *)
  | AstBuilderError of string
  (* matching (semgrep) related *)
  | MatchingError of string (* internal error, e.g., NoTokenLocation *)
  | SemgrepMatchFound of (string (* check_id *) * string) (* msg *)
  | TooManyMatches of string (* can contain offending pattern *)
  (* other *)
  | FatalError of string (* missing file, OCaml errors, etc. *)
  | Timeout of string option
  | OutOfMemory of string option

val g_errors : error list ref

val options : unit -> Common.cmdline_options

(*****************************************************************************)
(* Convertor functions *)
(*****************************************************************************)

val mk_error_loc : Parse_info.token_location -> error_kind -> error

val error : Parse_info.t -> error_kind -> unit

val error_loc : Parse_info.token_location -> error_kind -> unit

val exn_to_error : Common.filename -> exn -> error

val check_id_of_error_kind : error_kind -> string

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

val string_of_error_kind : error_kind -> string

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

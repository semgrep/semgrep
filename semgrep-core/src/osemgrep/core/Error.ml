module Out = Output_from_core_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Centralizing error management in osemgrep.

   Translated from error.py

   LATER: we should merge with Semgrep_core_error.ml, as well
   as the errors defined in semgrep_output_v1.atd
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* python: class SemgrepError(Exception)
   All Semgrep exceptions are caught and their error messages
   are displayed to the user.
   They all include at least this information.
*)
type t = { code : Exit_code.t; level : level; kind : error_kind }

(* TOPORT?
 * Warn = 3; Only an error if "strict" is set
 * Error = 4; Always an error
 *)
and level = Severity.basic_severity

(* The body of the main Error exception. *)
and error_kind =
  | Basic of string
  | Semgrep_core_error of Out.core_error
  | Invalid_rule_schema_error of details
  | Unknown_language_error of details

(*
   originally: class ErrorWithSpan(SemgrepError)

   Error which will print context from the Span. You should provide
   the most specific span possible, eg. if the error is an invalid
   key, provide exactly the span for that key. You can then expand
   what's printed with span.with_context(...). Conversely, if you
   don't want to display the entire span, you can use `span.truncate`

   Here is what the generated error will look like:

       <level>: <short_msg>
         --> <span.filename>:<span.start.line>
       1 | rules:
       2 |   - id: eqeq-is-bad
       3 |     pattern-inside: foo(...)
         |     ^^^^^^^^^^^^^^
       4 |     patterns:
       5 |       - pattern-not: 1 == 1
       = help: <help>
       <long_msg>

   :param short_msg: 1 or 2 word description of the problem (eg. missing key)
   :param level: How bad is the problem? error,warn, etc.
   :param spans: A list of spans to display for context.
   :help help: An optional hint about how to fix the problem
   :cause cause: The underlying exception
*)
and details = {
  short_msg : string;
  long_msg : string option;
  spans : Rule_lang.span list;
  help : string option;
}

exception Semgrep_error of t

(*****************************************************************************)
(* string of *)
(*****************************************************************************)

(* massive TODO *)
let string_of_error (x : t) =
  match x.kind with
  | Basic s -> s
  | _else_ -> "uh oh, something's wrong"

let register_exception_printer () =
  Printexc.register_printer (function
    | Semgrep_error err -> Some (string_of_error err)
    | _else_ -> None)

(*
   Modify the behavior of 'Printexc.to_string' to print Semgrep exceptions
   nicely.
*)
let () = register_exception_printer ()

(*****************************************************************************)
(* Builders *)
(*****************************************************************************)

let basic ?(code = Exit_code.fatal) ?(level = `Error) str =
  { code; level; kind = Basic str }

let semgrep_core_error ~code ~level x =
  { code; level; kind = Semgrep_core_error x }

let invalid_rule_schema_error details =
  {
    code = Exit_code.invalid_pattern;
    level = `Error;
    kind = Invalid_rule_schema_error details;
  }

let unknown_language_error details =
  {
    code = Exit_code.invalid_language;
    level = `Error;
    kind = Unknown_language_error details;
  }

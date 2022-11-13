(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Centralizing error management in osemgrep.

   Translated from error.py

   LATER: we should merge with Semgrep_core_error.ml, as well
   as the errors defined in semgrep_output_v1.atd (especially core_error).

   coupling: See the CLI.safe_run function which should catch all the exns
   defined in this module and return an appropriate exit code.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* If no exit code is given, will default to Exit_code.fatal.
   See CLI.safe_run()
*)
exception Semgrep_error of string * Exit_code.t option

(* TOPORT?
   exception Semgrep_core_error of Output_from_core_t.core_error

   (*
      python: class ErrorWithSpan(SemgrepError)

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

   exception Invalid_rule_schema of details (* (Exit_code.invalid_pattern) *)
   exception Inknown_language of details (* (Exit_code.invalid_language) *)
*)

(*****************************************************************************)
(* string of/registering exns *)
(*****************************************************************************)

(* TODO
   let register_exception_printer () =
     Printexc.register_printer (function
       | Semgrep_error err -> Some (string_of_error err)
       | _else_ -> None)

   (*
      Modify the behavior of 'Printexc.to_string' to print Semgrep exceptions
      nicely.
   *)
   let () = register_exception_printer ()
*)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let abort msg =
  (* TOPORT: click.seecho(message, fg="red", err=True) *)
  raise (Semgrep_error (msg, None))

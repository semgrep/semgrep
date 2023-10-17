module Out = Semgrep_output_v1_j

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
exception Exit of Exit_code.t

(* TOPORT?
   exception Semgrep_core_error of Semgrep_output_v1_t.core_error

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
(* Shortcuts *)
(*****************************************************************************)

let abort msg = raise (Semgrep_error (msg, None))
let exit code = raise (Exit code)

(*****************************************************************************)
(* string of/registering exns *)
(*****************************************************************************)

let () =
  Printexc.register_printer (function
    | Semgrep_error (msg, opt_exit_code) ->
        let base_msg = Printf.sprintf "Fatal error: %s" msg in
        Some
          (match opt_exit_code with
          | None -> base_msg
          | Some exit_code ->
              Printf.sprintf "%s\nExit code %i: %s" base_msg
                (Exit_code.to_int exit_code)
                (Exit_code.to_message exit_code))
    | Exit exit_code ->
        Some
          (Printf.sprintf "Exit code %i: %s"
             (Exit_code.to_int exit_code)
             (Exit_code.to_message exit_code))
    | _ -> None)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* This is used for the CLI text output and also for the metrics
 * payload.errors.error.
 * This used to be stored also in the cli_error.type_ field, but
 * we now store directly the error_type (which should have the
 * same string representation for most cases as before except
 * for the constructors with arguments.
 *)
let rec string_of_error_type (error_type : Out.error_type) : string =
  match error_type with
  (* # convert to the same string of core.ParseError for now *)
  | PartialParsing _ -> string_of_error_type ParseError
  (* other constructors with arguments *)
  | PatternParseError _ -> string_of_error_type PatternParseError0
  | IncompatibleRule _ -> string_of_error_type IncompatibleRule0
  (* All the other cases don't have arguments in Semgrep_output_v1.atd
   * and have some <json name="..."> annotations to generate the right string
   * so we can mostly just call Out.string_of_error_type (and remove the
   * quotes)
   *)
  | PatternParseError0
  | IncompatibleRule0
  | LexicalError
  | RuleParseError
  | SemgrepError
  | InvalidRuleSchemaError
  | UnknownLanguageError
  | MissingPlugin
  | ParseError
  | SpecifiedParseError
  | AstBuilderError
  | InvalidYaml
  | MatchingError
  | SemgrepMatchFound
  | TooManyMatches
  | FatalError
  | Timeout
  | OutOfMemory
  | TimeoutDuringInterfile
  | OutOfMemoryDuringInterfile ->
      Out.string_of_error_type error_type
      |> JSON.remove_enclosing_quotes_of_jstring

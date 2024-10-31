module OutJ = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Centralizing error management in osemgrep.

   Translated from error.py

   LATER: we should merge with Core_error.ml

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
exception Exit_code of Exit_code.t

(* TOPORT?
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
let exit_code_exn code = raise (Exit_code code)

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
              Printf.sprintf "%s\nExit code %i: %s" base_msg exit_code.code
                exit_code.description)
    | Exit_code exit_code ->
        Some
          (Printf.sprintf "Exit code %i: %s" exit_code.code
             exit_code.description)
    | _ -> None)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* This is used for the CLI text output and also for the metrics
 * payload.errors.errors.
 * The resulting string used to be stored also in the cli_error.type_ field,
 * but we now store directly the error_type (which should have the
 * same string representation for most cases as before except
 * for the constructors with arguments).
 * python: error_type_string() in error.py
 *)
let rec string_of_error_type (error_type : OutJ.error_type) : string =
  match error_type with
  (* python: convert to the same string of core.ParseError for now *)
  | PartialParsing _ -> string_of_error_type ParseError
  (* other constructors with arguments *)
  | PatternParseError _ -> string_of_error_type PatternParseError0
  | IncompatibleRule _ -> string_of_error_type IncompatibleRule0
  | DependencyResolutionError _ -> "Dependency resolution error"
  (* All the other cases don't have arguments in Semgrep_output_v1.atd
   * and have some <json name="..."> annotations to generate the right string
   * so we can mostly just call Out.string_of_error_type (and remove the
   * quotes)
   *)
  | PatternParseError0
  | IncompatibleRule0
  | LexicalError
  | RuleParseError
  | SemgrepWarning
  | SemgrepError
  | InvalidRuleSchemaError
  | UnknownLanguageError
  | MissingPlugin
  | ParseError
  | OtherParseError
  | AstBuilderError
  | InvalidYaml
  | MatchingError
  | SemgrepMatchFound
  | TooManyMatches
  | FatalError
  | Timeout
  | OutOfMemory
  | StackOverflow
  | TimeoutDuringInterfile
  | OutOfMemoryDuringInterfile ->
      OutJ.string_of_error_type error_type
      |> JSON.remove_enclosing_quotes_of_jstring

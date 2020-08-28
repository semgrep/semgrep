(*s: pfff/h_program-lang/Error_code.mli *)

(* types *)

(*s: type [[Error_code.error]] *)
type error = {
  typ: error_kind;
  loc: Parse_info.token_location;
  sev: severity;
}
(*e: type [[Error_code.error]] *)
(*s: type [[Error_code.severity]] *)
 and severity = Error | Warning | Info
(*e: type [[Error_code.severity]] *)

(*s: type [[Error_code.error_kind]] *)
 and error_kind =
  (* parsing related *)
  | LexicalError of string
  | ParseError (* a.k.a SyntaxError *)
  | AstBuilderError of string
  | AstGenericError of string
  | OtherParsingError of string

  (* global analysis *)
  | Deadcode of entity

  (* use/def entities *)
  | UndefinedDefOfDecl of entity
  | UnusedExport of entity * Common.filename
  | UnusedVariable of string * Scope_code.t

  (* CFG/DFG *)
  | UnusedStatement
  | UnusedAssign of string (* var name *)
  | UseOfUninitialized of string
  | CFGError of string

  (* sgrep lint rules *)
  | SgrepLint of (string (* check_id *) * string (* msg *))

  (* other *)
  | FatalError of string
  | Timeout of string option
  | OutOfMemory of string option
(*e: type [[Error_code.error_kind]] *)

(*s: type [[Error_code.entity]] *)
 and entity = (string * Entity_code.entity_kind)
(*e: type [[Error_code.entity]] *)

(* internal: you should prefer to use the error function below *)
val mk_error_loc: Parse_info.token_location -> error_kind -> error

(*s: type [[Error_code.annotation]] *)
(* @xxx to acknowledge or explain false positives *)
type annotation =
  | AtScheck of string
(*e: type [[Error_code.annotation]] *)

(* main API *)

(*s: signature [[Error_code.g_errors]] *)
val g_errors: error list ref
(*e: signature [[Error_code.g_errors]] *)

(*s: signature [[Error_code.error]] *)
(* !modify g_errors! *)
val error  : Parse_info.t -> error_kind -> unit
(*e: signature [[Error_code.error]] *)
(*s: signature [[Error_code.warning]] *)
val warning: Parse_info.t -> error_kind -> unit
(*e: signature [[Error_code.warning]] *)
(*s: signature [[Error_code.info]] *)
val info: Parse_info.t -> error_kind -> unit
(*e: signature [[Error_code.info]] *)

(*s: signature [[Error_code.error_loc]] *)
val error_loc  : Parse_info.token_location -> error_kind -> unit
(*e: signature [[Error_code.error_loc]] *)
(*s: signature [[Error_code.warning_loc]] *)
val warning_loc: Parse_info.token_location -> error_kind -> unit
(*e: signature [[Error_code.warning_loc]] *)
(*s: signature [[Error_code.info_loc]] *)
val info_loc: Parse_info.token_location -> error_kind -> unit
(*e: signature [[Error_code.info_loc]] *)

(* string-of *)

(*s: signature [[Error_code.string_of_error]] *)
val string_of_error: error -> string
(*e: signature [[Error_code.string_of_error]] *)
(*s: signature [[Error_code.string_of_error_kind]] *)
val string_of_error_kind: error_kind -> string
(*e: signature [[Error_code.string_of_error_kind]] *)

(*s: signature [[Error_code.check_id_of_error_kind]] *)
val check_id_of_error_kind: error_kind -> string
(*e: signature [[Error_code.check_id_of_error_kind]] *)

(* ranking *)

(*s: type [[Error_code.rank]] *)
type rank =
 | Never
 | OnlyStrict
 | Less
 | Ok
 | Important
 | ReallyImportant
(*e: type [[Error_code.rank]] *)

(*s: signature [[Error_code.score_of_rank]] *)
val score_of_rank:  rank -> int
(*e: signature [[Error_code.score_of_rank]] *)
(*s: signature [[Error_code.rank_of_error]] *)
val rank_of_error:  error -> rank
(*e: signature [[Error_code.rank_of_error]] *)
(*s: signature [[Error_code.score_of_error]] *)
val score_of_error: error -> int
(*e: signature [[Error_code.score_of_error]] *)

(*s: signature [[Error_code.annotation_at]] *)
val annotation_at:
  Parse_info.token_location -> annotation option
(*e: signature [[Error_code.annotation_at]] *)

(* error adjustments *)

(*s: signature [[Error_code.options]] *)
val options: unit -> Common.cmdline_options
(*e: signature [[Error_code.options]] *)

(*s: signature [[Error_code.report_parse_errors]] *)
val report_parse_errors: bool ref
(*e: signature [[Error_code.report_parse_errors]] *)
(*s: signature [[Error_code.report_fatal_errors]] *)
val report_fatal_errors: bool ref
(*e: signature [[Error_code.report_fatal_errors]] *)

(*s: signature [[Error_code.filter_maybe_parse_and_fatal_errors]] *)
(* use the flags above to filter certain errors *)
val filter_maybe_parse_and_fatal_errors: error list -> error list
(* convert parsing and other fatal exceptions in regular 'error'
 * added to g_errors
 *)
(*e: signature [[Error_code.filter_maybe_parse_and_fatal_errors]] *)
(*s: signature [[Error_code.adjust_paths_relative_to_root]] *)
(* convert parsing and other fatal exceptions in regular 'error'
 * added to g_errors
 *)

val adjust_paths_relative_to_root: 
  Common.path -> error list -> error list
(*e: signature [[Error_code.adjust_paths_relative_to_root]] *)

(*s: signature [[Error_code.exn_to_error]] *)
val exn_to_error: Common.filename -> exn -> error
(*e: signature [[Error_code.exn_to_error]] *)

(*s: signature [[Error_code.try_with_exn_to_error]] *)
val try_with_exn_to_error:
  Common.filename -> (unit -> unit) -> unit
(*e: signature [[Error_code.try_with_exn_to_error]] *)

(*s: signature [[Error_code.try_with_print_exn_and_reraise]] *)
val try_with_print_exn_and_reraise:
  Common.filename -> (unit -> unit) -> unit
(*e: signature [[Error_code.try_with_print_exn_and_reraise]] *)

(*s: type [[Error_code.identifier_index]] *)
(* to detect false positives (we use the Hashtbl.find_all property) *)
type identifier_index = (string, Parse_info.token_location) Hashtbl.t
(*e: type [[Error_code.identifier_index]] *)

(*s: signature [[Error_code.adjust_errors]] *)
(* have some approximations and Fps in graph_code_checker so filter them *)
val adjust_errors:
  error list -> error list
(*e: signature [[Error_code.adjust_errors]] *)

(* helpers for unit testing code *)

(*s: signature [[Error_code.expected_error_lines_of_files]] *)
(* extract all the lines with ERROR: comment in test files *)
val expected_error_lines_of_files:
  Common.filename list -> (Common.filename * int (* line with ERROR *)) list
(*e: signature [[Error_code.expected_error_lines_of_files]] *)

(*s: signature [[Error_code.compare_actual_to_expected]] *)
(* use Ounit *)
val compare_actual_to_expected:
  error list -> (Common.filename * int) list -> unit
(*e: signature [[Error_code.compare_actual_to_expected]] *)
(*e: pfff/h_program-lang/Error_code.mli *)

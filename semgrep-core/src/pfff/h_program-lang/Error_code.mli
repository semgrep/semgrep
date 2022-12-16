
(* types *)

type t = {
  typ: error_kind;
  loc: Parse_info.token_location;
  sev: severity;
}
and severity = Error | Warning | Info

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

  (* other *)
  | FatalError of string
  | Timeout of string option
  | OutOfMemory of string option

and entity = (string * Entity_code.entity_kind)

(* deprecated: alias, but you should use Error_code.t *)
type error = t

val mk_error_loc: Parse_info.token_location -> error_kind -> error

(* @xxx to acknowledge or explain false positives *)
type annotation =
  | AtScheck of string

(* main API *)

val g_errors: error list ref

(* !modify g_errors! *)
val error  : Parse_info.t -> error_kind -> unit
val warning: Parse_info.t -> error_kind -> unit
val info: Parse_info.t -> error_kind -> unit

val error_loc  : Parse_info.token_location -> error_kind -> unit
val warning_loc: Parse_info.token_location -> error_kind -> unit
val info_loc: Parse_info.token_location -> error_kind -> unit

(* string-of *)

val string_of_error: error -> string
val string_of_error_kind: error_kind -> string

(* ranking *)

type rank =
  | Never
  | OnlyStrict
  | Less
  | Ok
  | Important
  | ReallyImportant

val score_of_rank:  rank -> int
val rank_of_error:  error -> rank
val score_of_error: error -> int

val annotation_at:
  Parse_info.token_location -> annotation option

(* error adjustments *)

val options: unit -> Common.cmdline_options

val report_parse_errors: bool ref
val report_fatal_errors: bool ref

(* use the flags above to filter certain errors *)
val filter_maybe_parse_and_fatal_errors: error list -> error list
(* convert parsing and other fatal exceptions in regular 'error'
 * added to g_errors
*)
(* convert parsing and other fatal exceptions in regular 'error'
 * added to g_errors
*)

val adjust_paths_relative_to_root:
  Common.path -> error list -> error list

val exception_to_error: Common.filename -> Exception.t -> error

val try_with_exn_to_error : Common.filename -> (unit -> unit) -> unit

val try_with_print_exn_and_reraise:
  Common.filename -> (unit -> unit) -> unit

(* to detect false positives (we use the Hashtbl.find_all property) *)
type identifier_index = (string, Parse_info.token_location) Hashtbl.t

(* have some approximations and Fps in graph_code_checker so filter them *)
val adjust_errors:
  error list -> error list

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

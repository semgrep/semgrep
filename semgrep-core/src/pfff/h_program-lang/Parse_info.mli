(* TODO: this module should be split in 3 or 4, and especially we should have
 * a separate Tok.ml with all token related types.
*)
(*****************************************************************************)
(* Tokens *)
(*****************************************************************************)

(* ('token_location' < 'token_origin' < 'token_mutable') * token_kind *)

(* to report errors, regular position information *)
type token_location = {
  str: string; (* the content of the "token" *)
  charpos: int; (* byte position, 0-based *)
  line: int; (* 1-based *)
  column: int; (* 0-based *)
  file: Common.filename;
}
(* see also type filepos = { l: int; c: int; } in Common.mli *)

(* to deal with expanded tokens, e.g. preprocessor like cpp for C *)
type token_origin =
  | OriginTok  of token_location
  | FakeTokStr of string  * (token_location * int) option (* next to *)
  | ExpandedTok of token_location * token_location * int
  | Ab (* abstract token, see Parse_info.ml comment *)

(* to allow source to source transformation via token "annotations",
 * see the documentation for spatch.
*)
type token_mutable = {
  token: token_origin;
  (* for spatch *)
  mutable transfo: transformation;
}

and transformation =
  | NoTransfo
  | Remove
  | AddBefore of add
  | AddAfter of add
  | Replace of add
  | AddArgsBefore of string list

and add =
  | AddStr of string
  | AddNewlineAndIdent

(* Shortcut.
 * Technically speaking this is not a token, because we do not have
 * the kind of the token (e.g., PLUS | IDENT | IF | ...).
 * It's just a lexeme, but the word lexeme is not as known as token.
*)
type t = token_mutable
[@@deriving eq]

(* deprecated *)
type info_ = t

(* for ppx_deriving *)
val pp_full_token_info: bool ref
val pp : Format.formatter -> t -> unit
val pp_token_location: Format.formatter -> token_location -> unit
val equal_token_location: token_location -> token_location -> bool
val show_token_location: token_location -> string

(* mostly for the fuzzy AST builder *)
type token_kind =
  | LPar | RPar
  | LBrace | RBrace
  | LBracket | RBracket
  | LAngle | RAngle
  | Esthet of esthet
  | Eof
  | Other

and esthet =
  | Comment
  | Newline
  | Space

(*****************************************************************************)
(* Errors during parsing *)
(*****************************************************************************)

(* TODO? move to Error_code.mli instead *)

(* note that those exceptions can be converted in Error_code.error with
 * Error_code.try_with_exn_to_error()
*)
(* see also Parsing.Parse_error and Failure "empty token" raised by Lexing *)
exception Lexical_error of string * t
(* better than Parsing.Parse_error, which does not have location information *)
exception Parsing_error of t
(* when convert from CST to AST *)
exception Ast_builder_error of string * t
(* other stuff *)
exception Other_error of string * t

exception NoTokenLocation of string

val lexical_error: string -> Lexing.lexbuf -> unit

(*
   Register printers for the exceptions defined in this module.

   This makes 'Printexc.to_string' print the exceptions in a more complete
   fashion than the default printer, which only prints ints and strings
   and doesn't descend any deeper.
*)
val register_exception_printer : unit -> unit

(*****************************************************************************)
(* Info accessors and builders *)
(*****************************************************************************)

(* Fake tokens: safe vs unsafe
 * ---------------------------
 * "Safe" fake tokens require an existing location to attach to, and so
 * token_location_of_info will work on these fake tokens. "Unsafe" fake tokens
 * do not carry any location info, so calling token_location_of_info on these
 * will raise a NoTokenLocation exception.
 *
 * Always prefer "safe" functions (no "unsafe_" prefix), which only introduce
 * "safe" fake tokens. The unsafe_* functions introduce "unsafe" fake tokens,
 * please use them only as a last resort. *)

val fake_token_location : token_location

(* NOTE: These functions introduce unsafe fake tokens, prefer safe functions
 * below, use these only as a last resort! *)
val unsafe_fake_info : string -> t
val unsafe_fake_bracket: 'a -> t * 'a * t
val unsafe_sc: t

val fake_info_loc : token_location -> string -> t
val fake_info : t -> string -> t
val abstract_info: t

val fake_bracket_loc : token_location -> 'a -> t * 'a * t
val fake_bracket : t -> 'a -> t * 'a * t
val unbracket: t * 'a * t -> 'a
val sc_loc : token_location -> t
val sc : t -> t

val mk_info_of_loc: token_location -> t

val is_fake: t -> bool
val first_loc_of_file: Common.filename -> token_location

(* Extract the lexeme (token) as a string *)
val str_of_info   : t -> string

(* Extract position information *)
val line_of_info  : t -> int
val col_of_info   : t -> int
val pos_of_info   : t -> int
val file_of_info  : t -> Common.filename

(* Format the location file/line/column into a string *)
val string_of_info: t -> string

val is_origintok: t -> bool

val token_location_of_info: t -> (token_location, string) result
(** @raise NoTokenLocation if given an unsafe fake token (without location info) *)
val unsafe_token_location_of_info: t -> token_location
val get_original_token_location: token_origin -> token_location

val compare_pos: t -> t -> int
val min_max_ii_by_pos: t list -> t * t

(*****************************************************************************)
(* Parsing results *)
(*****************************************************************************)

type parsing_stat = {
  filename: Common.filename;
  total_line_count: int;
  mutable error_line_count: int;
  mutable have_timeout: bool;
  (* used only for cpp for now, to help diagnose problematic macros,
   * see print_recurring_problematic_tokens below.
  *)
  mutable commentized: int;
  mutable problematic_lines: (string list * int ) list;
}

(*
   Print file name and number of lines and error lines in compact format
   suitable for logging.
*)
val summary_of_stat : parsing_stat -> string

val default_stat: Common.filename -> parsing_stat
val bad_stat: Common.filename -> parsing_stat
val correct_stat: Common.filename -> parsing_stat
val print_parsing_stat_list: ?verbose:bool -> parsing_stat list -> unit
val print_recurring_problematic_tokens: parsing_stat list -> unit

val aggregate_stats: parsing_stat list -> int * int (* total * bad *)

val print_regression_information:
  ext:string -> Common2.path list -> Common2.score -> unit

(* a parser can also "return" an exception like Lexical_error,
 * or Parsing_error (unless Flag_parsing.error_recovery is true).
*)
type ('ast, 'toks) parsing_result = {
  ast: 'ast;
  (* Note that the token list contains usually also the comment-tokens *)
  tokens: 'toks list;
  stat: parsing_stat
}

(*****************************************************************************)
(* Lexer helpers *)
(*****************************************************************************)

(* lexer helpers *)
type 'tok tokens_state = {
  mutable rest:         'tok list;
  mutable current:      'tok;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed:       'tok list;
}
val mk_tokens_state: 'tok list -> 'tok tokens_state

val tokinfo:
  Lexing.lexbuf -> t
val yyback: int -> Lexing.lexbuf -> unit

(* can deprecate? *)
val tokinfo_str_pos:  string -> int -> t

val rewrap_str: string -> t -> t
val tok_add_s: string -> t -> t
(* used mainly by tree-sitter based parsers in semgrep *)
val combine_infos: t -> t list -> t
(* this function assumes the full content of the token is on the same
 * line, otherwise the line/col of the result might be wrong *)
val split_info_at_pos: int -> t -> t * t

(* to be used by the lexer *)
val tokenize_all_and_adjust_pos:
  Common.filename ->
  (Lexing.lexbuf -> 'tok) (* tokenizer *) ->
  ((t -> t) -> 'tok -> 'tok) (* token visitor *) ->
  ('tok -> bool) (* is_eof *) ->
  'tok list
val mk_lexer_for_yacc: 'tok list -> ('tok -> bool) (* is_comment *) ->
  'tok tokens_state * (* token stream for error recovery *)
  (Lexing.lexbuf -> 'tok) * (* the lexer to pass to the ocamlyacc parser *)
  Lexing.lexbuf (* fake lexbuf needed by ocamlyacc API *)

(* can deprecate? just use tokenize_all_and_adjust_pos *)
(* f(i) will contain the (line x col) of the i char position *)
val full_charpos_to_pos_large:
  Common.filename -> (int -> (int * int))
(* fill in the line and column field of token_location that were not set
 * during lexing because of limitations of ocamllex. *)
(* fill in the line and column field of token_location that were not set
 * during lexing because of limitations of ocamllex. *)
val complete_token_location_large :
  Common.filename -> (int -> (int * int))  -> token_location -> token_location

(** Fix the location info in a token. *)
val fix_token_location : (token_location -> token_location) -> token_mutable -> token_mutable

(** See [adjust_info_wrt_base]. *)
val adjust_pinfo_wrt_base : token_location -> token_location -> token_location

(* Token locations are supposed to denote the beginning of a token.
   Suppose we are interested in instead having line, column, and charpos of
   the end of a token instead.
   This is something we can do at relatively low cost by going through and inspecting
   the contents of the token, plus the start information.
*)
val get_token_end_info : token_location -> int * int * int

(** [adjust_info_wrt_base base_loc tok], where [tok] represents a location
  * relative to [base_loc], returns the same [tok] but with an absolute
  * {! token_location}. This is useful for fixing parse info after
  * {! Common2.with_tmp_file}. E.g. if [base_loc] points to line 3, and
  * [tok] points to line 2 (interpreted line 2 starting in line 3), then
  * the adjusted token will point to line 4. *)
val adjust_info_wrt_base : token_location -> token_mutable -> token_mutable

val error_message : Common.filename -> (string * int) -> string
val error_message_info :  t -> string
val print_bad: int -> int * int -> string array -> unit

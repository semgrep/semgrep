(*s: pfff/h_program-lang/Parse_info.mli *)
(* ('token_location' < 'token_origin' < 'token_mutable') * token_kind *)

(*s: type [[Parse_info.token_location]] *)
(* to report errors, regular position information *)
type token_location = {
    str: string; (* the content of the "token" *)
    charpos: int; (* byte position *)
    line: int; column: int;
    file: Common.filename;
} 
(*e: type [[Parse_info.token_location]] *)
(* see also type filepos = { l: int; c: int; } in Common.mli *)

(*s: type [[Parse_info.token_origin]] *)
(* to deal with expanded tokens, e.g. preprocessor like cpp for C *)
type token_origin =
  | OriginTok  of token_location
  | FakeTokStr of string  * (token_location * int) option (* next to *)
  | ExpandedTok of token_location * token_location * int 
  | Ab (* abstract token, see Parse_info.ml comment *)
(*e: type [[Parse_info.token_origin]] *)

(* to allow source to source transformation via token "annotations", 
 * see the documentation for spatch.
 *)
(*s: type [[Parse_info.token_mutable]] *)
type token_mutable = {
  token: token_origin; 
  (* for spatch *)
  mutable transfo: transformation;
}
(*e: type [[Parse_info.token_mutable]] *)

(*s: type [[Parse_info.transformation]] *)
 and transformation = 
  | NoTransfo
  | Remove 
  | AddBefore of add
  | AddAfter of add
  | Replace of add
  | AddArgsBefore of string list
(*e: type [[Parse_info.transformation]] *)

(*s: type [[Parse_info.add]] *)
  and add = 
    | AddStr of string
    | AddNewlineAndIdent
(*e: type [[Parse_info.add]] *)

(*s: type [[Parse_info.t]] *)
(* Shortcut.
 * Technically speaking this is not a token, because we do not have
 * the kind of the token (e.g., PLUS | IDENT | IF | ...).
 * It's just a lexeme, but the word lexeme is not as known as token.
 *)
type t = token_mutable
(*e: type [[Parse_info.t]] *)
(*s: type [[Parse_info.info_]] *)
(* deprecated *)
type info_ = t
(*e: type [[Parse_info.info_]] *)

(* for ppx_deriving *)
val pp_full_token_info: bool ref
val pp : Format.formatter -> t -> unit

(* mostly for the fuzzy AST builder *)
(*s: type [[Parse_info.token_kind]] *)
type token_kind =
  | LPar | RPar
  | LBrace | RBrace
  | LBracket | RBracket
  | LAngle | RAngle
  | Esthet of esthet
  | Eof
  | Other
(*e: type [[Parse_info.token_kind]] *)
(*s: type [[Parse_info.esthet]] *)
  and esthet =
   | Comment
   | Newline
   | Space
(*e: type [[Parse_info.esthet]] *)

(* note that those exceptions can be converted in Error_code.error with
 * Error_code.try_with_exn_to_error()
 *)
(*s: exception [[Parse_info.Lexical_error]] *)
(* see also Parsing.Parse_error and Failure "empty token" raised by Lexing *)
exception Lexical_error of string * t
(*e: exception [[Parse_info.Lexical_error]] *)
(*s: exception [[Parse_info.Parsing_error]] *)
(* better than Parsing.Parse_error, which does not have location information *)
exception Parsing_error of t
(*e: exception [[Parse_info.Parsing_error]] *)
(*s: exception [[Parse_info.Ast_builder_error]] *)
(* when convert from CST to AST *)
exception Ast_builder_error of string * t
(*e: exception [[Parse_info.Ast_builder_error]] *)
(*s: exception [[Parse_info.Other_error]] *)
(* other stuff *)
exception Other_error of string * t
(*e: exception [[Parse_info.Other_error]] *)

(*s: exception [[Parse_info.NoTokenLocation]] *)
exception NoTokenLocation of string
(*e: exception [[Parse_info.NoTokenLocation]] *)

(*s: signature [[Parse_info.lexical_error]] *)
val lexical_error: string -> Lexing.lexbuf -> unit
(*e: signature [[Parse_info.lexical_error]] *)

(*s: signature [[Parse_info.fake_token_location]] *)
val fake_token_location : token_location
(*e: signature [[Parse_info.fake_token_location]] *)
(*s: signature [[Parse_info.fake_info]] *)
val fake_info : string -> t
(*e: signature [[Parse_info.fake_info]] *)
(*s: signature [[Parse_info.first_loc_of_file]] *)
val first_loc_of_file: Common.filename -> token_location
(*e: signature [[Parse_info.first_loc_of_file]] *)

(*s: signature [[Parse_info.str_of_info]] *)
val str_of_info   : t -> string
(*e: signature [[Parse_info.str_of_info]] *)
(*s: signature [[Parse_info.line_of_info]] *)
val line_of_info  : t -> int
(*e: signature [[Parse_info.line_of_info]] *)
(*s: signature [[Parse_info.col_of_info]] *)
val col_of_info   : t -> int
(*e: signature [[Parse_info.col_of_info]] *)
(*s: signature [[Parse_info.pos_of_info]] *)
val pos_of_info   : t -> int
(*e: signature [[Parse_info.pos_of_info]] *)
(*s: signature [[Parse_info.file_of_info]] *)
val file_of_info  : t -> Common.filename
(*e: signature [[Parse_info.file_of_info]] *)

(*s: signature [[Parse_info.string_of_info]] *)
(* small error reporting, for longer reports use error_message above *)
val string_of_info: t -> string
(*e: signature [[Parse_info.string_of_info]] *)

(*s: signature [[Parse_info.is_origintok]] *)
val is_origintok: t -> bool
(*e: signature [[Parse_info.is_origintok]] *)

(*s: signature [[Parse_info.token_location_of_info]] *)
val token_location_of_info: t -> token_location
(*e: signature [[Parse_info.token_location_of_info]] *)
(*s: signature [[Parse_info.get_original_token_location]] *)
val get_original_token_location: token_origin -> token_location
(*e: signature [[Parse_info.get_original_token_location]] *)

(*s: signature [[Parse_info.compare_pos]] *)
val compare_pos: t -> t -> int
(*e: signature [[Parse_info.compare_pos]] *)
(*s: signature [[Parse_info.min_max_ii_by_pos]] *)
val min_max_ii_by_pos: t list -> t * t
(*e: signature [[Parse_info.min_max_ii_by_pos]] *)


(*s: type [[Parse_info.parsing_stat]] *)
type parsing_stat = {
  filename: Common.filename;
  mutable correct: int;
  mutable bad: int;
  (* used only for cpp for now *)
  mutable have_timeout: bool;
  mutable commentized: int;
  mutable problematic_lines: (string list * int ) list;
}
(*e: type [[Parse_info.parsing_stat]] *)
(*s: signature [[Parse_info.default_stat]] *)
val default_stat: Common.filename -> parsing_stat
(*e: signature [[Parse_info.default_stat]] *)
val bad_stat: Common.filename -> parsing_stat
val correct_stat: Common.filename -> parsing_stat
(*s: signature [[Parse_info.print_parsing_stat_list]] *)
val print_parsing_stat_list: ?verbose:bool -> parsing_stat list -> unit
(*e: signature [[Parse_info.print_parsing_stat_list]] *)
(*s: signature [[Parse_info.print_recurring_problematic_tokens]] *)
val print_recurring_problematic_tokens: parsing_stat list -> unit
(*e: signature [[Parse_info.print_recurring_problematic_tokens]] *)


(*s: type [[Parse_info.tokens_state]] *)
(* lexer helpers *)
type 'tok tokens_state = {
  mutable rest:         'tok list;
  mutable current:      'tok;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed:       'tok list;
}
(*e: type [[Parse_info.tokens_state]] *)
(*s: signature [[Parse_info.mk_tokens_state]] *)
val mk_tokens_state: 'tok list -> 'tok tokens_state
(*e: signature [[Parse_info.mk_tokens_state]] *)

(*s: signature [[Parse_info.tokinfo]] *)
val tokinfo:
  Lexing.lexbuf -> t
(*e: signature [[Parse_info.tokinfo]] *)
(*s: signature [[Parse_info.yyback]] *)
val yyback: int -> Lexing.lexbuf -> unit
(*e: signature [[Parse_info.yyback]] *)

(*s: signature [[Parse_info.tokinfo_str_pos]] *)
(* can deprecate? *)
val tokinfo_str_pos:  string -> int -> t
(*e: signature [[Parse_info.tokinfo_str_pos]] *)

(*s: signature [[Parse_info.rewrap_str]] *)
val rewrap_str: string -> t -> t
(*e: signature [[Parse_info.rewrap_str]] *)
(*s: signature [[Parse_info.tok_add_s]] *)
val tok_add_s: string -> t -> t
(*e: signature [[Parse_info.tok_add_s]] *)

(*s: signature [[Parse_info.tokenize_all_and_adjust_pos]] *)
(* to be used by the lexer *)
val tokenize_all_and_adjust_pos: 
  ?unicode_hack:bool ->
  Common.filename -> 
  (Lexing.lexbuf -> 'tok) (* tokenizer *) -> 
  ((t -> t) -> 'tok -> 'tok) (* token visitor *) -> 
  ('tok -> bool) (* is_eof *) -> 
  'tok list
(*e: signature [[Parse_info.tokenize_all_and_adjust_pos]] *)
(*s: signature [[Parse_info.mk_lexer_for_yacc]] *)
val mk_lexer_for_yacc: 'tok list -> ('tok -> bool) (* is_comment *) ->
  'tok tokens_state * (* token stream for error recovery *)
   (Lexing.lexbuf -> 'tok) * (* the lexer to pass to the ocamlyacc parser *)
   Lexing.lexbuf (* fake lexbuf needed by ocamlyacc API *)
(*e: signature [[Parse_info.mk_lexer_for_yacc]] *)

(* can deprecate? just use tokenize_all_and_adjust_pos *)
(*s: signature [[Parse_info.full_charpos_to_pos_large]] *)
(* f(i) will contain the (line x col) of the i char position *)
val full_charpos_to_pos_large: 
  Common.filename -> (int -> (int * int))
(* fill in the line and column field of token_location that were not set
 * during lexing because of limitations of ocamllex. *)
(*e: signature [[Parse_info.full_charpos_to_pos_large]] *)
(*s: signature [[Parse_info.complete_token_location_large]] *)
(* fill in the line and column field of token_location that were not set
 * during lexing because of limitations of ocamllex. *)
val complete_token_location_large : 
  Common.filename -> (int -> (int * int))  -> token_location -> token_location
(*e: signature [[Parse_info.complete_token_location_large]] *)


(*s: signature [[Parse_info.error_message]] *)
val error_message : Common.filename -> (string * int) -> string
(*e: signature [[Parse_info.error_message]] *)
(*s: signature [[Parse_info.error_message_info]] *)
val error_message_info :  t -> string
(*e: signature [[Parse_info.error_message_info]] *)
(*s: signature [[Parse_info.print_bad]] *)
val print_bad: int -> int * int -> string array -> unit
(*e: signature [[Parse_info.print_bad]] *)
(*e: pfff/h_program-lang/Parse_info.mli *)

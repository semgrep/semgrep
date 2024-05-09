(* Parsing helpers used by the ocamllex/ocamlyacc/menhir parsers.
 * For parsing helpers for tree-sitter, see Parse_tree_sitter_helpers.ml
 *)

(* history: we used to use /tmp files to parse Semgrep string patterns by
 * just dumping the pattern string first with Common.with_tmp_file and
 * then using the tmp file. Even though /tmp access is fast and often
 * cached in memory (e.g., by the Linux buffer cache), it is still
 * a bit ugly to use /tmp to parse strings and also a bit slower.
 * Enter input_source.
 *)
type input_source = Str of string | File of Fpath.t

(* just a shortcut for Parsing_helpers.File (Fpath.v s) *)
val file : string (* filename *) -> input_source

(* To be used by the lexer.
 * TODO: use labels instead of comments below?
 *)
val tokenize_all_and_adjust_pos :
  input_source ->
  (* tokenizer, usually derived from ocamllex *)
  (Lexing.lexbuf -> 'tok) ->
  (* token visitor (used to adjust the location of tokens) *)
  ((Tok.t -> Tok.t) -> 'tok -> 'tok) ->
  (* is_eof *)
  ('tok -> bool) ->
  'tok list

(* Many parsers need to interact with the lexer, or use tricks around
 * the stream of tokens, or do some error recovery, or just need to
 * skip certain tokens (like the comments token). All of this requires
 * to have access to this stream of remaining tokens.
 * Enter tokens_state.
 *)
type 'tok tokens_state = {
  mutable rest : 'tok list;
  mutable current : 'tok;
  (* it's passed/skipped since last "checkpoint", not passed from the
   * beginning *)
  mutable passed : 'tok list;
}

val mk_tokens_state : 'tok list -> 'tok tokens_state

val mk_lexer_for_yacc :
  'tok list ->
  (* is_comment *)
  ('tok -> bool) ->
  (* token stream for error recovery *)
  'tok tokens_state
  * ((* the lexer to pass to the ocamlyacc parser *)
     Lexing.lexbuf -> 'tok)
  * (* fake lexbuf needed by ocamlyacc API *)
    Lexing.lexbuf

(* go back in the Lexing stream *)
val yyback : int -> Lexing.lexbuf -> unit

(* TODO? remove? *)
val error_message : string (* filename *) -> string * int -> string
val error_message_info : Tok.t -> string
val show_parse_error_line : int -> int * int -> string array -> string

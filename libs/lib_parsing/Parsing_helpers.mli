(* history: we used to use /tmp files to parse Semgrep string patterns by
 * just dumping the pattern string first with Common.with_tmp_file and
 * then using the file. Even though /tmp access is fast and often
 * cached in memory (e.g., by the Linux buffer cache), it is still
 * a bit ugly to use /tmp to parse strings and also a bit slower.
 * Enter input_source.
 *)
type input_source = Str of string | File of Fpath.t

(* just a shortcut for Parsing_helpers.File (Fpath.v s) *)
val file : Common.filename -> input_source

(* to be used by the lexer *)
val tokenize_all_and_adjust_pos :
  input_source ->
  (Lexing.lexbuf -> 'tok) ->
  (* tokenizer *) ((Tok.t -> Tok.t) -> 'tok -> 'tok) ->
  (* token visitor *) ('tok -> bool) ->
  (* is_eof *) 'tok list

(* Many parsers need to interact with the lexer, or use tricks around
 * the stream of tokens, or do some error recovery, or just need to
 * pass certain tokens (like the comments token) which requires
 * to have access to this stream of remaining tokens.
 * The tokens_state type helps.
 *)
type 'tok tokens_state = {
  mutable rest : 'tok list;
  mutable current : 'tok;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed : 'tok list;
}

val mk_tokens_state : 'tok list -> 'tok tokens_state

val mk_lexer_for_yacc :
  'tok list ->
  ('tok -> bool) ->
  (* is_comment *)
  'tok tokens_state
  * ((* token stream for error recovery *)
     Lexing.lexbuf -> 'tok)
  * (* the lexer to pass to the ocamlyacc parser *)
    Lexing.lexbuf
(* fake lexbuf needed by ocamlyacc API *)

(* go back in the Lexing stream *)
val yyback : int -> Lexing.lexbuf -> unit
val error_message : Common.filename -> string * int -> string
val error_message_info : Tok.t -> string
val print_bad : int -> int * int -> string array -> unit

(* lexer helpers *)
type 'tok tokens_state = {
  mutable rest : 'tok list;
  mutable current : 'tok;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed : 'tok list;
}

val mk_tokens_state : 'tok list -> 'tok tokens_state
val yyback : int -> Lexing.lexbuf -> unit

(* to be used by the lexer *)
val tokenize_all_and_adjust_pos :
  Common.filename ->
  (Lexing.lexbuf -> 'tok) ->
  (* tokenizer *) ((Parse_info.t -> Parse_info.t) -> 'tok -> 'tok) ->
  (* token visitor *) ('tok -> bool) ->
  (* is_eof *) 'tok list

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

(* can deprecate? just use tokenize_all_and_adjust_pos *)
(* f(i) will contain the (line x col) of the i char position *)
val full_charpos_to_pos_large : Common.filename -> int -> int * int

(* fill in the line and column field of token_location that were not set
 * during lexing because of limitations of ocamllex. *)
(* fill in the line and column field of token_location that were not set
 * during lexing because of limitations of ocamllex. *)
val complete_token_location_large :
  Common.filename ->
  (int -> int * int) ->
  Parse_info.token_location ->
  Parse_info.token_location

val fix_token_location :
  (Parse_info.token_location -> Parse_info.token_location) ->
  Parse_info.token_mutable ->
  Parse_info.token_mutable
(** Fix the location info in a token. *)

val adjust_pinfo_wrt_base :
  Parse_info.token_location ->
  Parse_info.token_location ->
  Parse_info.token_location
(** See [adjust_info_wrt_base]. *)

(* Token locations are supposed to denote the beginning of a token.
   Suppose we are interested in instead having line, column, and charpos of
   the end of a token instead.
   This is something we can do at relatively low cost by going through and inspecting
   the contents of the token, plus the start information.
*)
val get_token_end_info : Parse_info.token_location -> int * int * int

val adjust_info_wrt_base :
  Parse_info.token_location ->
  Parse_info.token_mutable ->
  Parse_info.token_mutable
(** [adjust_info_wrt_base base_loc tok], where [tok] represents a location
  * relative to [base_loc], returns the same [tok] but with an absolute
  * {! token_location}. This is useful for fixing parse info after
  * {! Common2.with_tmp_file}. E.g. if [base_loc] points to line 3, and
  * [tok] points to line 2 (interpreted line 2 starting in line 3), then
  * the adjusted token will point to line 4. *)

val error_message : Common.filename -> string * int -> string
val error_message_info : Parse_info.t -> string
val print_bad : int -> int * int -> string array -> unit

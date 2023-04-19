(* TODO: split with a Parsing_error.ml ? *)
open Tok

(*****************************************************************************)
(* Tokens *)
(*****************************************************************************)

(* TODO: remove at some point *)
type t = Tok.t [@@deriving eq, show]

(* mostly for the fuzzy AST builder *)
type token_kind =
  | LPar
  | RPar
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | LAngle
  | RAngle
  | Esthet of esthet
  | Eof
  | Other

and esthet = Comment | Newline | Space

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

val lexical_error : string -> Lexing.lexbuf -> unit

(*
   Register printers for the exceptions defined in this module.

   This makes 'Printexc.to_string' print the exceptions in a more complete
   fashion than the default printer, which only prints ints and strings
   and doesn't descend any deeper.
*)
val register_exception_printer : unit -> unit

(*****************************************************************************)
(* Info builders *)
(*****************************************************************************)

val tokinfo : Lexing.lexbuf -> t
val mk_info_of_loc : token_location -> t
val first_loc_of_file : Common.filename -> token_location

(* TODO? could also be in Lexer helpers section *)
(* can deprecate? *)
val tokinfo_str_pos : string -> int -> t
val rewrap_str : string -> t -> t
val tok_add_s : string -> t -> t

(* used mainly by tree-sitter based parsers in semgrep *)
val combine_infos : t -> t list -> t

(* this function assumes the full content of the token is on the same
 * line, otherwise the line/col of the result might be wrong *)
val split_info_at_pos : int -> t -> t * t

(*****************************************************************************)
(* Fake tokens: safe vs unsafe *)
(*****************************************************************************)
(* "Safe" fake tokens require an existing location to attach to, and so
 * token_location_of_info will work on these fake tokens. "Unsafe" fake tokens
 * do not carry any location info, so calling token_location_of_info on these
 * will raise a NoTokenLocation exception.
 *
 * Always prefer "safe" functions (no "unsafe_" prefix), which only introduce
 * "safe" fake tokens. The unsafe_* functions introduce "unsafe" fake tokens,
 * please use them only as a last resort. *)

exception NoTokenLocation of string

val fake_token_location : token_location
val is_fake : t -> bool
val is_origintok : t -> bool

(* NOTE: These functions introduce unsafe fake tokens, prefer safe functions
 * below, use these only as a last resort! *)
val unsafe_fake_info : string -> t
val unsafe_fake_bracket : 'a -> t * 'a * t

(* sc for semicolon, which are often fake tokens because of
 * ASI (Automatic Semicolon Insertion) in languages like Javascript.
 *)
val unsafe_sc : t

(* "safe" fake versions *)

val fake_info_loc : token_location -> string -> t
val fake_info : t -> string -> t
val fake_bracket_loc : token_location -> 'a -> t * 'a * t
val fake_bracket : t -> 'a -> t * 'a * t
val sc_loc : token_location -> t
val sc : t -> t

(* accessor *)
val unbracket : t * 'a * t -> 'a

(*****************************************************************************)
(* Info accessors *)
(*****************************************************************************)

(* Extract the lexeme (token) as a string *)
val str_of_info : t -> string

(* Extract position information *)
val line_of_info : t -> int
val col_of_info : t -> int
val pos_of_info : t -> int
val file_of_info : t -> Common.filename

(* Format the location file/line/column into a string *)
val string_of_info : t -> string
val token_location_of_info : t -> (token_location, string) result

(* @raise NoTokenLocation if given an unsafe fake token (without location info) *)
val unsafe_token_location_of_info : t -> token_location
val get_original_token_location : token_origin -> token_location
val compare_pos : t -> t -> int
val min_max_ii_by_pos : t list -> t * t

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)
val abstract_info : t

(*****************************************************************************)
(* Parsing stats *)
(*****************************************************************************)
(* now in Parsing_stat.ml *)

(*****************************************************************************)
(* Lexer helpers *)
(*****************************************************************************)
(* now in Parsing_helpers.ml *)

(*****************************************************************************)
(* Tokens *)
(*****************************************************************************)

(* TODO: remove at some point *)
type t = Tok.t [@@deriving eq, show]

(*****************************************************************************)
(* Info builders *)
(*****************************************************************************)

val tokinfo : Lexing.lexbuf -> t
val mk_info_of_loc : Tok.location -> t

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

val fake_info_loc : Tok.location -> string -> t
val fake_info : t -> string -> t
val fake_bracket_loc : Tok.location -> 'a -> t * 'a * t
val fake_bracket : t -> 'a -> t * 'a * t
val sc_loc : Tok.location -> t
val sc : t -> t

(* accessor *)
val unbracket : t * 'a * t -> 'a

(*****************************************************************************)
(* Info accessors *)
(*****************************************************************************)

(* Extract the lexeme (token) as a string *)
val str_of_info : t -> string
val file_of_info : t -> Common.filename

(* Format the location file/line/column into a string *)
val string_of_info : t -> string
val token_location_of_info : t -> (Tok.location, string) result

(* @raise NoTokenLocation if given an unsafe fake token (without location info) *)
val unsafe_token_location_of_info : t -> Tok.location
val get_original_token_location : Tok.origin -> Tok.location
val compare_pos : t -> t -> int
val min_max_ii_by_pos : t list -> t * t

(*****************************************************************************)
(* Parsing errors *)
(*****************************************************************************)
(* now in Parsing_error.ml *)

(*****************************************************************************)
(* Parsing stats *)
(*****************************************************************************)
(* now in Parsing_stat.ml *)

(*****************************************************************************)
(* Lexer helpers *)
(*****************************************************************************)
(* now in Parsing_helpers.ml *)

(* Type to represent a token (e.g., an identifier, a keyword).
 * Tok.t is used in many ASTs (including in AST_generic.ml) and CSTs
 * across Semgrep.
 *
 * Tok.t is a bit complicated because we want to represent "fake" and
 * "expanded" tokens. We use fake tokens because in many of
 * the ASTs/CSTs in Semgrep (including in AST_generic.ml) we store the
 * tokens in the ASTs/CSTs at the leaves, and sometimes the actual
 * token is optional (e.g., a virtual semicolon in Javascript).
 * We could use an option type, but that would involve a big refactoring
 * hence the current use of FakeTok.
 * We also use expanded tokens to track tokens through macro expansions
 * or file includes (e.g., #include in C, #import in Jsonnet).
 *
 * Ideally, we should not store tokens in the ASTs, just location information,
 * but again this would involve a big refactoring and redesigning deeply how
 * we track location in Semgrep.
 *
 * It is reliable to extract the string and position from a token when
 * the token is an OriginTok (see also Tok.is_origintok()). For the other
 * cases, you might get an NoTokenLocation exn.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type location = {
  str : string; (* the content of the token starting at pos (e.g., "if") *)
  pos : Pos.t;
}
[@@deriving show, eq, ord, sexp]

type t =
  (* Token found in the original file *)
  | OriginTok of location
  (* Present only in the AST and generated after parsing. Can be used
   * when building some extra AST elements (e.g., fake semicolons).
   * The string (e.g., ";")  is to help the generic pretty printer.
   *)
  | FakeTok of string * virtual_location option
  (* "Expanded" tokens are marked with a special tag so that if someone does
   * a transformation on those expanded tokens, they will get a warning
   * (because we may have trouble back-propagating the transformation back
   *  to the original file).
   * The location refers to the preprocessed file (e.g. /tmp/pp-xxxx.pphp).
   *)
  | ExpandedTok of location * virtual_location
  (* The Ab constructor is (ab)used to call '=' to compare big AST portions.
   * Ab means AbstractLineTok (short name to not polluate in debug mode).
   * An alternative is to use the t_always_equal special type below.
   *)
  | Ab

(* Sometimes we generate fake tokens close to existing
 * origin tokens. This can be useful when we need to give an error
 * message that involves a fakeToken. The int below is a kind of
 * virtual position, an offset.
 * Those are called "safe" fake tokens (in contrast to the
 * regular/unsafe one which have no position information at all).
 *
 * For ExpandedTok the location refers to the last token
 * before a series of expanded tokens and the int is an offset.
 * The goal is to be able to compare the position of tokens,
 * even for expanded tokens. See compare_pos().
 *)
and virtual_location = location * int [@@deriving show, eq, ord, sexp]

(* To customize show() dynamically. If you set this to true, AST
 * dumper will display the full token information instead of just a '()'
 *)
val pp_full_token_info : bool ref

(* As opposed to 't', the equal and hash functions for 't_always_equal'
 * are actually not automatically derived; Tok.ml provides customized
 * behavior where we assume all tokens are equal.
 * This is used by Semgrep in AST_generic and Raw_tree to be able to
 * check for equality of big AST constructs (e.g., complex expressions) by not
 * caring about differences in token positions.
 *)
type t_always_equal = t [@@deriving show, eq, ord, hash, sexp]

(*****************************************************************************)
(* Token builders *)
(*****************************************************************************)

val tok_of_lexbuf : Lexing.lexbuf -> t
val tok_of_loc : location -> t

(* deprecated: TODO used only in Lexer_php.mll *)
val make : str:string -> file:Fpath.t -> bytepos:int -> t

(* the token will be empty, but its pos will be the beginning of the file *)
val first_tok_of_file : Fpath.t -> t

(* similar, the location will be empty *)
val first_loc_of_file : Fpath.t -> location

(* used mainly by tree-sitter based parsers in semgrep.
 * [combine_toks t1 ts] will return a token where t1::ts
 * have been combined in a single token, with a starting pos
 * of t1.pos.
   TODO: please explain how to represent an empty string literal in the
   generic AST, which has type 'string wrap bracket'.
 *)
val combine_toks : t -> t list -> t

(* Try to concatenate the tokens such that the original (line, column) offsets
   of each token are preserved in the concatenated string, as well as the byte
   offset if possible.

   Source with a line continuation and a comment:
     "a \\\n# comment\nb"

   Tokens obtained with a parser that removes comments and line continuations:
     ["a "; "b"]

   A naive concatenation gives us:
     "a b"

   A syntax error on "b" when parsing "a b" will report an error on
   line 1, column 2 instead of line 3, column 0 in the source.

   A suitable concatenation is:
     "a         \\\n\\\nb"
        ^^^^^^^^
                ^^^^^^^^
        inserted
        spaces
                inserted
                line continuations

   This is used to assemble Bash code fragments produced by the Dockerfile
   parser before invoking the Bash parser. In this case, missing strings
   are line continuations and comments.

   Default values:
   - ignorable_newline: "\n" (a single LF character)
   - ignorable_blank: ' ' (space)
*)
val combine_sparse_toks :
  ?ignorable_newline:string ->
  ?ignorable_blank:char ->
  t ->
  t list ->
  t Option.t

(* Create the empty token corresponding to the position right after a
   given token. This is intended for representing empty strings and such. *)
val empty_tok_after : t -> t

(* Safe combination of a list of tokens contained within quotes or similar. *)
val combine_bracket_contents : t * ('a * t) list * t -> t

(* this function assumes the full content of the token is on the same
 * line, otherwise the line/col of the result might be wrong *)
val split_tok_at_bytepos : int -> t -> t * t

(*****************************************************************************)
(* Fake tokens (safe and unsafe) *)
(*****************************************************************************)
(* "Safe" fake tokens require an existing location to attach to, and so
 * loc_of_tok will work on these fake tokens. "Unsafe" fake tokens
 * do not carry any location info, so calling loc_of_tok on those
 * will raise a NoTokenLocation exception.
 *
 * Always prefer "safe" functions (no "unsafe_" prefix); use the unsafe_*
 * functions only as a last resort.
 *)

val is_fake : t -> bool
val is_origintok : t -> bool

exception NoTokenLocation of string

val fake_tok : t -> string -> t
val unsafe_fake_tok : string -> t

(* sc stands for semicolon. Semicolons are often fake tokens because of
 * ASI (Automatic Semicolon Insertion) in languages like Javascript.
 *)
val sc : t -> t
val unsafe_sc : t
val fake_bracket : t -> 'a -> t * 'a * t
val unsafe_fake_bracket : 'a -> t * 'a * t

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

val loc_of_tok : t -> (location, string) result

(* Format the location file/line/column into a string *)
val stringpos_of_tok : t -> string

(* @raise NoTokenLocation if given an unsafe fake token (without location) *)
val unsafe_loc_of_tok : t -> location

(* Unsafe: Extract the token (really lexeme) content *)
val content_of_tok : t -> string

(* Extract the token (really lexeme) content *)
val content_of_tok_opt : t -> string option

(* Extract position information *)
val line_of_tok : t -> int
val col_of_tok : t -> int
val bytepos_of_tok : t -> int
val file_of_tok : t -> Fpath.t

(* Token positions in loc.pos denote the beginning of a token.
   Suppose we are interested in having instead the line, column, and charpos
   of the end of a token.
   This is something we can do at relatively low cost by going through and
   inspecting the content of the location, plus the start information.
   alt: return a Pos.t instead
*)
val end_pos_of_loc : location -> int * int * int (* line x col x charpos *)

(*****************************************************************************)
(* Adjust string *)
(*****************************************************************************)

(* Deprecated: but still used in many ocamllex lexers in Semgrep *)
val rewrap_str : string -> t -> t
val tok_add_s : string -> t -> t

(*****************************************************************************)
(* Adjust location *)
(*****************************************************************************)
val fix_location : (location -> location) -> t -> t
(** adjust the location in a token *)

val fix_pos : (Pos.t -> Pos.t) -> location -> location
(** adjust the position in a location *)

val adjust_tok_wrt_base : location -> t -> t
(** [adjust_tok_wrt_base base_loc tok], where [tok] represents a location
  * relative to [base_loc], returns the same [tok] but with an absolute
  * {! location}. This is useful for fixing tokens after
  * {! Common2.with_tmp_file}. E.g. if [base_loc] points to line 3, and
  * [tok] points to line 2 (interpreted line 2 starting in line 3), then
  * the adjusted token will point to line 4. *)

val adjust_loc_wrt_base : location -> location -> location
(** See [adjust_tok_wrt_base]. *)

(*****************************************************************************)
(* Adjust line x col in a location *)
(*****************************************************************************)

(* fill in the line and column field of location that were not set
 * during lexing because of limitations of ocamllex and Lexing.position.
 *)
val complete_location :
  Fpath.t -> Pos.bytepos_linecol_converters -> location -> location

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

val unbracket : t * 'a * t -> 'a

(* deprecated: you should use t_always_equal instead of using
 * abstract_tok (and Ab) to compare ASTs
 *)
val abstract_tok : t

(* comparison (TODO? should use deriving ord?) *)
val compare_pos : t -> t -> int

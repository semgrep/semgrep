(* Token type used in many ASTs (including in AST_generic.ml) and CSTs
 * across Semgrep.
 *
 * The types below are a bit complicated because we want
 * to represent "fake" and "expanded" tokens. We also want to
 * annotate tokens with transformation (for Spatch).
 *
 * The main type below is 't', which represents a token (real or fake).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type location = {
  str : string; (* the content of the token starting at pos *)
  pos : Pos.t;
}
[@@deriving show, eq]

type kind =
  (* Token found in the original file *)
  | OriginTok of location
  (* Present only in the AST and generated after parsing. Can be used
   * when building some extra AST elements (e.g., fake semicolons) *)
  | FakeTokStr of
      string (* to help the generic pretty printer, e.g. "," *)
      * (* Sometimes we generate fake tokens close to existing
         * origin tokens. This can be useful when we need to give an error
         * message that involves a fakeToken. The int is a kind of
         * virtual position, an offset.
         * Those are called "safe" fake tokens (in contrast to the
         * regular/unsafe one which have no position information at all).
         *)
      (location * int) option
  (* "Expanded" tokens are marked with a special tag so that if someone does
   * a transformation on those expanded tokens, they will get a warning
   * (because we may have trouble back-propagating the transformation back
   *  to the original file).
   *)
  | ExpandedTok of
      (* refers to the preprocessed file, e.g. /tmp/pp-xxxx.pphp *)
      location
      * (* kind of a virtual position. The location refers to the last token
         * before a series of expanded tokens and the int is an offset.
         * The goal is to be able to compare the position of tokens
         * between then, even for expanded tokens. See compare_pos().
         *)
        location
      * int
  (* The Ab constructor is (ab)used to call '=' to compare big AST portions.
   * Ab means AbstractLineTok (short name to not polluate in debug mode).
   * An alternative is to use the t_always_equal special type below.
   *)
  | Ab
[@@deriving show, eq]

type transformation =
  | NoTransfo
  | Remove
  | AddBefore of add
  | AddAfter of add
  | Replace of add
  | AddArgsBefore of string list

and add = AddStr of string | AddNewlineAndIdent [@@deriving show, eq]

type t = {
  (* contains among other things the position of the token through
   * the 'location' embedded inside the kind type.
   *)
  token : kind;
  (* The transfo field as its name suggests is to allow source to source
   * transformations via token "annotations". See the documentation for Spatch.
   * TODO: remove now that we use AST-based autofix in Semgrep.
   *)
  mutable transfo : transformation;
}
[@@deriving show, eq]

(* to customize show() dynamically *)
val pp_full_token_info : bool ref

(* As opposed to 't', the equal and hash functions for 't_always_equal'
 * are actually not automatically derived; Tok.ml provides customized
 * behavior where we assume all tokens are equal.
 * This is used by Semgrep in AST_generic and Raw_tree to be able to
 * check for equality of big AST constructs (e.g., complex expressions) by not
 * caring about differences in token positions.
 *)
type t_always_equal = t [@@deriving show, eq, hash]

(*****************************************************************************)
(* Fake tokens (safe and unsafe) *)
(*****************************************************************************)
(* "Safe" fake tokens require an existing location to attach to, and so
 * loc_of_tok will work on these fake tokens. "Unsafe" fake tokens
 * do not carry any location info, so calling loc_of_tok on these
 * will raise a NoTokenLocation exception.
 *
 * Always prefer "safe" functions (no "unsafe_" prefix), which only introduce
 * "safe" fake tokens. The unsafe_* functions introduce "unsafe" fake tokens,
 * please use them only as a last resort.
 *)

exception NoTokenLocation of string

val fake_location : location

(*****************************************************************************)
(* Token builders *)
(*****************************************************************************)

val tok_of_lexbuf : Lexing.lexbuf -> t
val tok_of_loc : location -> t

(* deprecated: TODO used only in Lexer_php.mll *)
val tok_of_str_and_bytepos : string -> int -> t

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

val loc_of_tok : t -> (location, string) result

(* @raise NoTokenLocation if given an unsafe fake token (without location) *)
val unsafe_loc_of_tok : t -> location

(* Extract the token (really lexeme) content *)
val content_of_tok : t -> string

(* Extract position information *)
val line_of_tok : t -> int
val col_of_tok : t -> int
val bytepos_of_tok : t -> int
val file_of_tok : t -> Common.filename

(* Token positions in loc.pos denote the beginning of a token.
   Suppose we are interested in having instead the line, column, and charpos
   of the end of a token.
   This is something we can do at relatively low cost by going through and
   inspecting the content of the location, plus the start information.
   alt: return a Pos.t instead
*)
val end_pos_of_loc : location -> int * int * int (* line x col x charpos *)

(*****************************************************************************)
(* Builders *)
(*****************************************************************************)

(* deprecated: you should use instead Pos.first_pos_of_file *)
val first_loc_of_file : Common.filename -> location

(*****************************************************************************)
(* Adjust location *)
(*****************************************************************************)
val fix_location : (location -> location) -> t -> t
(** adjust the location in a token *)

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
  Common.filename -> (int -> int * int) -> location -> location

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* deprecated: you should use t_always_equal instead of using
 * abstract_tok (and Ab) to compare ASTs
 *)
val abstract_tok : t

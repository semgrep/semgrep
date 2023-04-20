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

type origin =
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
   * the 'location' embedded inside the 'origin' type.
   *)
  token : origin;
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

val fake_location : location

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* deprecated: you should use instead Pos.first_pos_of_file *)
val first_loc_of_file : Common.filename -> location

(* deprecated: you should use t_always_equal instead of using
 * abstract_tok (and Ab) to compare ASTs
 *)
val abstract_tok : t

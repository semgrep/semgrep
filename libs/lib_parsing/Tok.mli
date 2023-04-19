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
         * origin tokens. This can be useful when have to give an error
         * message that involves a fakeToken. The int is a kind of
         * virtual position, an offset.
         * Those are called "safe" fake tokens (in contrast to the
         * regular/unsafe one which have no position information at all).
         *)
      (location * int) option
  (* "Expanded" tokens maked with a special tag so that if someone do
   * some transformation on those expanded tokens, they will get a warning
   * (because we may have trouble back-propagating the transformation back
   *  to the original file).
   *)
  | ExpandedTok of
      (* refers to the preprocessed file, e.g. /tmp/pp-xxxx.pphp *)
      location
      * (* kind of virtual position. This info refers to the last token
         * before a serie of expanded tokens and the int is an offset.
         * The goal is to be able to compare the position of tokens
         * between then, even for expanded tokens. See compare_pos
         * below.
         *)
        location
      * int
  (* The Ab constructor is (ab)used to call '=' to compare big AST portions.
   * Ab means AbstractLineTok (short name to not polluate in debug mode).
   *)
  | Ab
[@@deriving show, eq]

(* The transfo field as its name suggest is to allow source to source
 * transformation via token "annotations". See the documentation for spatch.
 *
 * Technically speaking this is not a token, because we do not have
 * the kind of the token (e.g., PLUS | IDENT | IF | ...).
 * It's just a lexeme, but the word lexeme is not as known as token.
 *)
type t = {
  (* contains among other things the position of the token through
   * the 'location' embedded inside the 'origin' type.
   *)
  token : origin;
  (* for spatch *)
  mutable transfo : transformation;
}

and transformation =
  | NoTransfo
  | Remove
  | AddBefore of add
  | AddAfter of add
  | Replace of add
  | AddArgsBefore of string list

and add = AddStr of string | AddNewlineAndIdent [@@deriving show, eq]

(* to customize deriving show dynamically *)
val pp_full_token_info : bool ref

(*****************************************************************************)
(* Fake tokens: save vs unsafe *)
(*****************************************************************************)

val fake_location : location

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

val first_loc_of_file : Common.filename -> location
val abstract_tok : t

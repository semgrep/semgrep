(*
   Regexp AST
*)

type loc = Tok_range.t

(*
   Matching options set with (?X) or (?-X), where X is a letter that specify
   the option to turn on or off.
*)
type opt =
  | Caseless (* i *)
  | Allow_duplicate_names (* J *)
  | Multiline (* m *)
  | Dotall (* s (pcre_dotall) *)
  | Default_lazy (* U *)
  | Ignore_whitespace (* x (pcre_extended) *)

(*
   Any atomic sequence that doesn't consume a character of input,
   such as '^', '\A', '\w', etc.
*)
type special =
  | Beginning_of_line (* ^ *)
  | End_of_line (* $ *)
  | Beginning_of_input (* \A *)
  | End_of_last_line (* \Z *)
  | End_of_input (* \z *)
  | Beginning_of_match (* \G *)
  | Numeric_back_reference of int
  | Named_back_reference of string
  | Word_boundary (* \b *)
  | Not_word_boundary (* \B *)
  | Match_point_reset (* \K *)
  | Set_option of opt (* (?i) (?J) (?m) (?s) (?U) (?x) *)
  | Clear_option of opt (* (?-i) (?-J) (?-m) (?-s) (?-U) (?-x) *)
  | Callout of int (* (?C) or (?Cn) for n in [0, 255] *)
  | Recurse_pattern of int (* (?R), (?R1), ..., (?R42), ... *)
  | Call_subpattern_by_abs_number of int
  | Call_subpattern_by_rel_number of int
  | Call_subpattern_by_name of string

type abstract_char_class =
  | Dot (* any character except newline, unless we're in dotall mode *)
  | Unicode_character_property of string
    (* Unicode character property introduced with \p{...}
       e.g. \p{Meroitic_Hieroglyphs} *)
  | Extended_grapheme_cluster (* \X *)

(*
   Character ranges or single characters as they occur within
   character classes e.g. '[a-z_]'.
   Characters are represented by their Unicode identifier.

   Pure ascii classes should not use 'Name' but instead should be expanded
   into the exact set of code points. Mixed ascii/unicode characters classes
   may be represented using 'Name' or using the other constructs.
*)
type char_class =
  | Empty
  | Singleton of int
  | Range of int * int
  | Union of char_class * char_class
  | Inter of char_class * char_class (* exotic *)
  | Diff of char_class * char_class (* exotic *)
  | Complement of char_class
  | Abstract of abstract_char_class
  | Other of string (* anything we can't make sense of *)

type repeat_range = int * int option

type matching_pref =
  | Default (* longest first or greedy e.g. 'a*', unless in (?U) mode *)
  | Lazy (* shortest first or lazy e.g. 'a*?' *)
  | Possessive (* disable backtracking e.g. 'a*+' *)

type group_kind =
  | Non_capturing
  | Non_capturing_reset (* (?| ... ) *)
  | Capturing
  | Named_capture of string
  | Lookahead
  | Neg_lookahead
  | Lookbehind
  | Neg_lookbehind
  | Atomic (* (?> ... ) *)
  | Other of int (* some unrecognized character following '(?' *)

type t =
  | Empty of loc
  | Char of loc * char_class (* match a single character from a set *)
  | Special of loc * special
    (* all the backslash sequences like '\A' that don't consume
       a character. Some of them are assertions, others may modify the
       behavior of the matching engine. *)
  | Seq of loc * t * t
  | Alt of loc * t * t
  | Repeat of loc * t * repeat_range * matching_pref
  | Group of loc * group_kind * t
  | Conditional of loc * condition * t * t option

and condition =
  | Abs_ref_cond of int
  | Rel_ref_cond of int
  | Named_ref_cond of string
  | Num_group_recursion_cond of int
  | Named_group_recursion_cond of string
  | Define
  | Assertion of t
(* must be one of the 4
   positive/negative lookahead/lookbehind assertions *)

(***************************************************************************)
(* Constructors, meant for the parser or for testing. *)
(***************************************************************************)

(* Eliminate one of the terms if it's'Empty' since we get that a lot due
   to how parsing is done *)
val union : char_class -> char_class -> char_class

(* Eliminate one of the terms if it's'Empty' since we get that a lot due
   to how parsing is done *)
val seq : t -> t -> t
val seq_of_list : t list -> t
val seq_of_code_points : (loc * int) list -> t
val seq_of_ascii_string : loc -> string -> t

(*
   Break down a string into a list of characters.
   TODO: compute the correct location for character based on its offset.
*)
val chars_of_ascii_string : string -> char list
val code_points_of_ascii_string : string -> int list
val code_points_of_ascii_string_loc : loc -> string -> (loc * int) list
val location : t -> loc
val location2 : t -> t -> loc
val range : loc -> loc -> loc
val dummy_loc : loc

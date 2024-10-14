(*
   Regexp AST
*)

type loc = Tok_range.t

type opt =
  | Caseless
  | Allow_duplicate_names
  | Multiline
  | Dotall
  | Default_lazy
  | Ignore_whitespace

type special =
  | Beginning_of_line
  | End_of_line
  | Beginning_of_input
  | End_of_last_line
  | End_of_input
  | Beginning_of_match
  | Numeric_back_reference of int
  | Named_back_reference of string
  | Word_boundary
  | Not_word_boundary
  | Match_point_reset
  | Set_option of opt
  | Clear_option of opt
  | Callout of int
  | Recurse_pattern of int
  | Call_subpattern_by_abs_number of int
  | Call_subpattern_by_rel_number of int
  | Call_subpattern_by_name of string

type abstract_char_class =
  | Dot
  | Unicode_character_property of string
  | Extended_grapheme_cluster (* \X *)

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
type matching_pref = Default | Lazy | Possessive

type group_kind =
  | Non_capturing
  | Non_capturing_reset
  | Capturing
  | Named_capture of string
  | Lookahead
  | Neg_lookahead
  | Lookbehind
  | Neg_lookbehind
  | Atomic
  | Other of int (* some unrecognized character following '(?' *)

type t =
  | Empty of loc
  | Char of loc * char_class
  | Special of loc * special
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

let location = function
  | Empty loc
  | Char (loc, _)
  | Special (loc, _)
  | Seq (loc, _, _)
  | Alt (loc, _, _)
  | Repeat (loc, _, _, _)
  | Group (loc, _, _) ->
      loc
  | Conditional (loc, _, _, _) -> loc

let location2 a b =
  let start, _ = location a in
  let _, end_ = location b in
  (start, end_)

let range (a, _) (_, b) = (a, b)

let dummy_loc =
  let tok = Tok.unsafe_fake_tok "" in
  (tok, tok)

let union (a : char_class) (b : char_class) =
  match (a, b) with
  | a, Empty -> a
  | Empty, b -> b
  | a, b -> Union (a, b)

let seq (a : t) (b : t) =
  match (a, b) with
  | a, Empty _ -> a
  | Empty _, b -> b
  | a, b ->
      let loc = range (location a) (location b) in
      Seq (loc, a, b)

let chars_of_ascii_string s : char list =
  let codes = ref [] in
  for i = String.length s - 1 downto 0 do
    codes := s.[i] :: !codes
  done;
  !codes

let code_points_of_ascii_string s : int list =
  let codes = ref [] in
  for i = String.length s - 1 downto 0 do
    codes := Char.code s.[i] :: !codes
  done;
  !codes

let code_points_of_ascii_string_loc loc s : (loc * int) list =
  let codes = ref [] in
  for i = String.length s - 1 downto 0 do
    (* TODO: set correct location for the character *)
    codes := (loc, Char.code s.[i]) :: !codes
  done;
  !codes

let seq_of_list (l : t list) : t =
  List_.fold_right (fun x acc -> seq x acc) l (Empty dummy_loc)

let seq_of_code_points (l : (loc * int) list) : t =
  List_.map (fun (loc, c) -> Char (loc, Singleton c)) l |> seq_of_list

let seq_of_ascii_string loc s =
  code_points_of_ascii_string_loc loc s |> seq_of_code_points

open AST
open Printf

type pp = Line of string | Block of pp list | Inline of pp list

let to_buf buf l =
  let open Printf in
  let rec pp indent = function
    | Line s -> bprintf buf "%s%s\n" indent s
    | Block l -> List.iter (pp (indent ^ "  ")) l
    | Inline l -> List.iter (pp indent) l
  in
  pp "" (Inline l)

let show_opt (x : opt) =
  match x with
  | Caseless -> "Caseless"
  | Allow_duplicate_names -> "Allow_duplicate_names"
  | Multiline -> "Multiline"
  | Dotall -> "Dotall"
  | Default_lazy -> "Default_lazy"
  | Ignore_whitespace -> "Ignore_whitespace"

let pp_special (x : special) =
  match x with
  | Beginning_of_line -> "Beginning_of_line"
  | End_of_line -> "End_of_line"
  | Beginning_of_input -> "Beginning_of_input"
  | End_of_last_line -> "End_of_last_line"
  | End_of_input -> "End_of_input"
  | Numeric_back_reference n -> sprintf "Numeric_back_reference %i" n
  | Named_back_reference name -> sprintf "Named_back_reference %s" name
  | Word_boundary -> "Word_boundary"
  | Not_word_boundary -> "Not_word_boundary"
  | Beginning_of_match -> "Beginning_of_match"
  | Match_point_reset -> "Match_point_reset"
  | Set_option opt -> sprintf "Set_option %s" (show_opt opt)
  | Clear_option opt -> sprintf "Clear_option %s" (show_opt opt)
  | Callout n -> sprintf "Callout %i" n
  | Recurse_pattern n -> sprintf "Recurse_pattern %i" n
  | Call_subpattern_by_abs_number n ->
      sprintf "Call_subpattern_by_abs_number %i" n
  | Call_subpattern_by_rel_number n ->
      sprintf "Call_subpattern_by_rel_number %i" n
  | Call_subpattern_by_name name -> sprintf "Call_subpattern_by_name %s" name

let show_char code =
  if code < 128 then sprintf "%C" (Char.chr code) else sprintf "0x%X" code

(*
   We print a character class on a single line, using a format designed
   to be easy to understand. For example, '\w' is printed as 'word_char'.
*)
let pp_char_class (x : char_class) =
  let rec pp buf (x : char_class) =
    match x with
    | Empty -> bprintf buf "{}"
    | Singleton code -> bprintf buf "%s" (show_char code)
    | Range (a, b) -> bprintf buf "[%s-%s]" (show_char a) (show_char b)
    | Union (a, b) -> bprintf buf "(%a|%a)" pp a pp b
    | Inter (a, b) -> bprintf buf "(%a&%a)" pp a pp b
    | Diff (a, b) -> bprintf buf "(%a-%a)" pp a pp b
    | Complement a -> bprintf buf "^%a" pp a
    | Abstract Dot -> bprintf buf "."
    | Abstract (Unicode_character_property name) ->
        bprintf buf "(Unicode_property %s)" name
    | Abstract Extended_grapheme_cluster ->
        bprintf buf "(Extended_grapheme_cluster)"
    | Other data -> bprintf buf "(Other %S)" data
  in
  let buf = Buffer.create 64 in
  pp buf x;
  Buffer.contents buf

let show_repeat_range (low, high) =
  let s n = if n > 1 then "s" else "" in
  match (low, high) with
  | 0, Some high -> sprintf "up to %i time%s" high (s high)
  | low, None -> sprintf "%i or more times" low
  | low, Some high when low = high -> sprintf "%i time%s" high (s high)
  | low, Some high -> sprintf "%i-%i time%s" low high (s high)

let show_matching_pref = function
  | Default -> "[longest match first]"
  | Lazy -> "shortest match first"
  | Possessive -> "longest match, no backtracking"

let show_group_kind = function
  | Non_capturing -> "Non_capturing"
  | Non_capturing_reset -> "Non_capturing_reset"
  | Capturing -> "Capturing"
  | Named_capture name -> "Named_capture " ^ name
  | Lookahead -> "Lookahead"
  | Neg_lookahead -> "Neg_lookahead"
  | Lookbehind -> "Lookbehind"
  | Neg_lookbehind -> "Neg_lookbehind"
  | Atomic -> "Atomic"
  | Other c -> sprintf "Other %s" (show_char c)

let rec pp (node : t) =
  match node with
  | Empty _ -> [ Line "Empty" ]
  | Char (_, x) -> [ Line ("Char: " ^ pp_char_class x) ]
  | Special (_, x) -> [ Line ("Special: " ^ pp_special x) ]
  | Seq (_, a, b) -> [ Block (pp a); Line "."; Block (pp b) ]
  | Alt (_, a, b) -> [ Block (pp a); Line "|"; Block (pp b) ]
  | Repeat (_, a, range, pref) ->
      [
        Line
          (sprintf "Repeat %s, %s:" (show_repeat_range range)
             (show_matching_pref pref));
        Block (pp a);
      ]
  | Group (_, kind, a) ->
      [ Line (sprintf "Group: %s" (show_group_kind kind)); Block (pp a) ]
  | Conditional (_, cond, then_, else_) ->
      let cond =
        match cond with
        | Abs_ref_cond n -> `Simple (sprintf "Abs_ref_cond %i" n)
        | Rel_ref_cond n -> `Simple (sprintf "Rel_ref_cond %i" n)
        | Named_ref_cond name -> `Simple (sprintf "Named_ref_cond %s" name)
        | Num_group_recursion_cond n ->
            `Simple (sprintf "Num_group_recursion_cond %i" n)
        | Named_group_recursion_cond name ->
            `Simple (sprintf "Named_group_recursion_cond %s" name)
        | Define -> `Simple "Define"
        | Assertion x -> `Complex (pp x)
      in
      let if_ =
        match cond with
        | `Simple s -> [ Line (sprintf "If %s" s) ]
        | `Complex x -> [ Line "If:"; Block x ]
      in
      let then_ = [ Line "Then:"; Block (pp then_) ] in
      let else_ =
        match else_ with
        | None -> []
        | Some else_ -> [ Line "Else:"; Block (pp else_) ]
      in
      [ Inline if_; Inline then_; Inline else_ ]

let print node =
  let buf = Buffer.create 1000 in
  to_buf buf (pp node);
  print_string (Buffer.contents buf)

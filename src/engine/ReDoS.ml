(*
   Detect regexps vulnerable to denial-of-service attacks (ReDoS) by static
   analysis.
*)

open Parser_regexp

let parse_regexp conf re_str =
  try Some (Parse.string ~conf re_str) with
  | Parsing_error.Syntax_error _ -> None

(* Iterate over all the nodes of a regexp *)
let rec iter f (x : AST.t) =
  f x;
  match x with
  | Empty _ -> ()
  | Char _ -> ()
  | Special _ -> ()
  | Seq (_loc, a, b) ->
      iter f a;
      iter f b
  | Alt (_loc, a, b) ->
      iter f a;
      iter f b
  | Repeat (_loc, x, _repeat_range, _matching_pref) -> iter f x
  | Group (_loc, _group_kind, x) ->
      (* TODO: remove groups in a processing step? *)
      iter f x
  | Conditional (_loc, _condition, then_, else_) -> (
      iter f then_;
      match else_ with
      | Some else_ -> iter f else_
      | None -> ())

(* Find nodes that match a predicate *)
let find_all test x =
  let acc = ref [] in
  iter (fun x -> if test x then acc := x :: !acc) x;
  List.rev !acc

(* Same as 'find' but ignore which subnodes were matched *)
let matches_deep test x = find_all test x <> []

let matches_sequence test_left test_right (x : AST.t) =
  match x with
  | Seq (_, a, b) -> test_left a && test_right b
  | __else__ -> false

(* A choice that always allows two or more branches to match nonempty input.
   Examples include:
   - a|aa
   - aa?        # same as a{1,2}
   - a+
   - ab|abab    # allows for periodic input
   - [ab]|[ac]
   - a|a?
   - a*         # greedy repeat
   - a*?        # lazy repeat

   Examples do not include:
   - a?         # one of the branches always matches empty input
   - a|b        # at most one branch can match at a given position
   - a|ab       # cannot match more than one branch multiple times in a row

   Determining this correctly turns out to be hard!

   Things to note:
   - greedy or lazy repeats don't change anything since the explosion occurs
     when matching fails. In these cases, all combinations are tried
     so their order doesn't matter.
   - possessive quantifiers prevent backtracking, so these are always safe.
     They're not used very often, though.

   Crude approach:
   - look for <anything> repeated
   - number of repeats must be greater than 1
   - at least two values must be allowed for the number of repeats
   This will cover:
   - a*
   - a+
   - a{1,2}

   The following won't be caught (TODO: preprocess the regexp tree to
   transform these into a single repeated pattern):
   - aa*        # same as a+
   - aa?        # same as a{1,2}
   - a|aa       # same as a{1,2}
   - a|aaa
*)
let rec matches_in_two_nonempty_branches (x : AST.t) =
  match x with
  | Empty _ -> false
  | Char _ -> false
  | Special _ -> false
  | Seq _ -> false
  | Alt _ -> false
  | Repeat (_, _, _, Possessive) -> false
  | Repeat (_, _, (min_reps, max_reps), (Default | Lazy)) -> (
      (* repeats two different number of times that are greater than 0 *)
      match max_reps with
      | None -> true
      | Some max_reps ->
          (min_reps >= 1 && max_reps > min_reps)
          || (min_reps = 0 && max_reps >= 2))
  | Group (_, _, x) -> matches_in_two_nonempty_branches x
  | Conditional (_, _cond, then_, else_) -> (
      matches_in_two_nonempty_branches then_
      ||
      match else_ with
      | None -> false
      | Some x -> matches_in_two_nonempty_branches x)

let matches_nonpossessive_repeat ~min_repeat test (x : AST.t) =
  match x with
  | Repeat (_, _, _, Possessive) -> false
  | Repeat (_, x, (_min_reps, opt_max_reps), _) ->
      let satisfies_min_repeats =
        match opt_max_reps with
        | None -> true
        | Some max_reps -> max_reps >= min_repeat
      in
      satisfies_min_repeats && test x
  | __else__ -> false

let matches_not_everywhere (x : AST.t) =
  let match_constraints =
    x
    |> find_all (function
         | Empty _ -> false
         | Char _ -> true
         | Special (_loc, x) -> (
             match x with
             | Beginning_of_line
             (* ^ *)
             | End_of_line
             (* $ *)
             | Beginning_of_input
             (* \A *)
             | End_of_last_line
             (* \Z *)
             | End_of_input
             (* \z *)
             | Beginning_of_match
             (* \G *)
             | Numeric_back_reference _
             | Named_back_reference _
             | Word_boundary
             (* \b *)
             | Not_word_boundary (* \B *) ->
                 (* possibly *) true
             | Match_point_reset (* \K *) -> false
             | Set_option _ -> false
             | Clear_option _ -> false
             | Callout _ -> (* presumably *) true
             | Recurse_pattern _ -> (* hopefully *) true
             | Call_subpattern_by_abs_number _
             | Call_subpattern_by_rel_number _
             | Call_subpattern_by_name _ ->
                 (* presumably *) true)
         | Seq _ -> false
         | Alt _ -> false
         | Repeat _ -> false
         | Group _ -> false
         | Conditional _ -> false)
  in
  match_constraints <> []

(*
   Return all the nodes that look vulnerable.
*)
let find_vulnerable_nodes =
  find_all
    (matches_sequence
       (matches_deep
          (matches_nonpossessive_repeat ~min_repeat:4
             (matches_deep matches_in_two_nonempty_branches)))
       matches_not_everywhere)

(* Take the requested substring if possible, otherwise return the original
   string. *)
let safe_sub s pos sublen =
  let len = String.length s in
  if pos < 0 || len < 0 || pos >= len || pos + sublen > len then s
  else String.sub s pos sublen

(*
   Replace AST nodes by their source code.
*)
let recover_source re_str node =
  let start, end_ = AST.location node in
  let first_tok_pos = Tok.bytepos_of_tok start in
  let last_tok_pos = Tok.bytepos_of_tok end_ in
  let last_tok_str = Tok.content_of_tok end_ in
  let len = last_tok_pos + String.length last_tok_str - first_tok_pos in
  safe_sub re_str first_tok_pos len

let find_vulnerable_subpatterns ?(dialect = Dialect.PCRE) re_str =
  let conf = Dialect.conf dialect in
  match parse_regexp conf re_str with
  | None -> Error ()
  | Some re_ast ->
      Ok (find_vulnerable_nodes re_ast |> List_.map (recover_source re_str))

let regexp_may_be_vulnerable ?dialect re_str =
  match find_vulnerable_subpatterns ?dialect re_str with
  | Ok [] -> false
  | Ok _ -> true
  | Error () -> false

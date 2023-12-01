(*
   Representation of the location of a piece of code by a pair of tokens.

   This is used so far in intermediate, language-specific ASTs.
*)
open Common

type t = Tok.t * Tok.t [@@deriving show]

let fix ((left, right) as loc) =
  if Tok.is_fake left then (right, right)
  else if Tok.is_fake right then (left, left)
  else loc

let create left right =
  if Tok.is_fake left then (right, right)
  else if Tok.is_fake right then (left, left)
  else (left, right)

let is_fake (left, right) = Tok.is_fake left || Tok.is_fake right
let left_loc_tok (left, right) = if Tok.is_fake left then right else left
let right_loc_tok (left, right) = if Tok.is_fake right then left else right

(*
   Form a new location from a leftmost location and a rightmost location.
   We try to mitigate problems due to fake tokens.
*)
let range a b =
  let start_tok = left_loc_tok a in
  let end_tok = right_loc_tok b in
  if Tok.is_fake end_tok then (start_tok, start_tok)
  else if Tok.is_fake start_tok then (end_tok, end_tok)
  else (start_tok, end_tok)

let unsafe_fake_tok = Tok.unsafe_fake_tok "fake"
let unsafe_fake_loc = (unsafe_fake_tok, unsafe_fake_tok)

(*
   'tok_pos_left' and 'tok_pos_right' both return the start position of the
   token. Only their fallback differs when no position is known.
*)
let tok_pos_left tok =
  match Tok.loc_of_tok tok with
  | Ok x -> x.pos.bytepos
  | Error _ -> max_int

let tok_pos_right tok =
  match Tok.loc_of_tok tok with
  | Ok x -> x.pos.bytepos
  | Error _ -> min_int

(* Prefer non-error. In case of a tie, prefer left-handside argument. *)
let min_tok (a : Tok.t) (b : Tok.t) =
  if tok_pos_left a <= tok_pos_left b then a else b

(* Prefer non-error. In case of a tie, prefer right-handside argument. *)
let max_tok (a : Tok.t) (b : Tok.t) =
  if tok_pos_right b >= tok_pos_right a then b else a

let update_start new_start_tok (_, end_) : t = (new_start_tok, end_)
let update_end (start, _) new_end_tok : t = (start, new_end_tok)

(*
   This is safe but a bit heavy.
   Hopefully it's not too expensive in practice.
*)
let extend (start, end_) tok =
  (* Eliminate any fake token that might exist in the original location,
     if possible. *)
  let start = if Tok.is_fake start then end_ else start in
  let end_ = if Tok.is_fake end_ then start else end_ in
  (* Extend the location to the left, to the right, or both in pathological
     cases. *)
  (min_tok start tok, max_tok tok end_)

let of_toks get_tok xs =
  List.fold_left (fun loc x -> extend loc (get_tok x)) unsafe_fake_loc xs

let union loc_a (start, end_) =
  let loc = extend loc_a start in
  extend loc end_

let of_locs locs = List.fold_left union unsafe_fake_loc locs
let of_list get_loc xs = List_.map get_loc xs |> of_locs

(* TODO: we should filter with is_origintok() first, to avoid having
 * the caller to do it.
 * TODO: we should use min_tok, max_tok instead (this function used to be
 * in Parse_info.ml)
 *)
let min_max_toks_by_pos xs =
  match xs with
  | [] ->
      raise
        (Tok.NoTokenLocation
           "Match returned an empty list with no token location information; \
            this may be fixed by adding enclosing token information (e.g. \
            bracket or parend tokens) to the list's enclosing node type.")
  | [ x ] -> (x, x)
  | x :: xs ->
      let pos_leq p1 p2 = Tok.compare_pos p1 p2 =|= -1 in
      xs
      |> List.fold_left
           (fun (minii, maxii) e ->
             let maxii' = if pos_leq maxii e then e else maxii in
             let minii' = if pos_leq e minii then e else minii in
             (minii', maxii'))
           (x, x)

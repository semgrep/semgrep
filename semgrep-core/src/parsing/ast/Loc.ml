(*
   Representation of the location of a piece of code by a pair of tokens.

   This is used so far in intermediate, language-specific ASTs.
*)

type tok = Parse_info.t

type t = tok * tok

let fix ((left, right) as loc) =
  if Parse_info.is_fake left then (right, right)
  else if Parse_info.is_fake right then (left, left)
  else loc

let create left right =
  if Parse_info.is_fake left then (right, right)
  else if Parse_info.is_fake right then (left, left)
  else (left, right)

let is_fake (left, right) = Parse_info.is_fake left || Parse_info.is_fake right

let left_loc_tok (left, right) = if Parse_info.is_fake left then right else left

let right_loc_tok (left, right) =
  if Parse_info.is_fake right then left else right

(*
   Form a new location from a leftmost location and a rightmost location.
   We try to mitigate problems due to fake tokens.
*)
let range a b =
  let start_tok = left_loc_tok a in
  let end_tok = right_loc_tok b in
  if Parse_info.is_fake end_tok then (start_tok, start_tok)
  else if Parse_info.is_fake start_tok then (end_tok, end_tok)
  else (start_tok, end_tok)

let unsafe_fake_tok = Parse_info.unsafe_fake_info "fake"

let unsafe_fake_loc = (unsafe_fake_tok, unsafe_fake_tok)

(*
   'tok_pos_left' and 'tok_pos_right' both return the start position of the
   token. Only their fallback differs when no position is known.
*)
let tok_pos_left tok =
  match Parse_info.token_location_of_info tok with
  | Ok x -> x.charpos
  | Error _ -> max_int

let tok_pos_right tok =
  match Parse_info.token_location_of_info tok with
  | Ok x -> x.charpos
  | Error _ -> min_int

(* Prefer non-error. In case of a tie, prefer left-handside argument. *)
let min_tok (a : Parse_info.t) (b : Parse_info.t) =
  if tok_pos_left a <= tok_pos_left b then a else b

(* Prefer non-error. In case of a tie, prefer right-handside argument. *)
let max_tok (a : Parse_info.t) (b : Parse_info.t) =
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
  let start = if Parse_info.is_fake start then end_ else start in
  let end_ = if Parse_info.is_fake end_ then start else end_ in
  (* Extend the location to the left, to the right, or both in pathological
     cases. *)
  (min_tok start tok, max_tok tok end_)

let of_toks get_tok xs =
  List.fold_left (fun loc x -> extend loc (get_tok x)) unsafe_fake_loc xs

let union loc_a (start, end_) =
  let loc = extend loc_a start in
  extend loc end_

let of_locs locs = List.fold_left union unsafe_fake_loc locs

let of_list get_loc xs = Common.map get_loc xs |> of_locs

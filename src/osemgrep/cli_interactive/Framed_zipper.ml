(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A framed zipper.

   See the .mli for more information
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type 'a t = {
  before_rev : 'a list;
  pointer : int;
  max_len : int;
  after : 'a list;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let shift_frame_left m =
  match (m.before_rev, m.after) with
  | [], _ -> m
  | x :: xs, after -> { m with before_rev = xs; after = x :: after }

let shift_frame_right m =
  match (m.before_rev, m.after) with
  | _, [] -> m
  | before_rev, x :: xs -> { m with before_rev = x :: before_rev; after = xs }

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* A move necessitates a pointer move, which may or may
   not cause a frame move, depending on if the pointer is
   at the boundaries of the frame.
*)
let move_left m =
  if m.pointer <= 0 then { (shift_frame_left m) with pointer = 0 }
  else { m with pointer = m.pointer - 1 }

let move_right m =
  if m.pointer >= List.length m.after - 1 then
    (* This is the case where we move the pointer
       down, but we don't have enough entries left.
       In this case, don't move the pointer.
    *)
    m
  else if m.pointer >= m.max_len - 1 then
    { (shift_frame_right m) with pointer = m.max_len - 1 }
  else { m with pointer = m.pointer + 1 }

let take n m = Common2.take_safe n m.after
let of_list max_len l = { before_rev = []; after = l; pointer = 0; max_len }
let relative_position m = m.pointer
let get_current m = List.nth m.after (relative_position m)

let map_current f m =
  {
    m with
    after =
      Common.mapi
        (fun idx x -> if idx = relative_position m then f x else x)
        m.after;
  }

let set_frame_size n m = { m with max_len = n }
let absolute_position m = List.length m.before_rev + m.pointer
let length m = List.length m.before_rev + List.length m.after
let frame_size m = m.max_len
let is_empty m = List.length m.after + List.length m.before_rev = 0

let empty_with_max_len max_len =
  { before_rev = []; after = []; pointer = 0; max_len }

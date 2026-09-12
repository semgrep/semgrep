(*
   Various string manipulation functions that need unit tests.
*)

let safe_sub s orig_start orig_len =
  let s_len = String.length s in
  let orig_end = orig_start + orig_len in
  let start = min s_len (max 0 orig_start) in
  let end_ = min s_len (max 0 orig_end) in
  let len = max 0 (end_ - start) in
  String.sub s start len

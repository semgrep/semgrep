(*
   Keep a copy of the source input, suitable for returning source code
   from ranges of locations.
*)

open Printf

(* The contents of the source document. *)
type t = string

let of_string contents = contents

let of_channel ic =
  let buf = Buffer.create 10000 in
  (try
     while true do
       bprintf buf "%s\n" (input_line ic)
     done;
     assert false
   with End_of_file ->
     Buffer.contents buf
  )

let of_stdin () = of_channel stdin

let of_file file =
  let ic = open_in_bin file in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let to_lexbuf s =
  Lexing.from_string s

(* Find the index (position from the beginning of the string)
   right after the end of the current line. *)
let rec find_end_of_line s i =
  if i >= String.length s then i
  else
    match s.[i] with
    | '\n' -> i + 1
    | _ -> find_end_of_line s (i + 1)

let ensure_newline s =
  match s with
  | "" -> ""
  | s ->
      if s.[String.length s - 1] <> '\n' then
        s ^ "\n"
      else
        s

(*
   Same as String.sub but shrink the requested range to a valid range
   if needed.
*)
let safe_string_sub s orig_start orig_len =
  let s_len = String.length s in
  let orig_end = orig_start + orig_len in
  let start = min s_len (max 0 orig_start) in
  let end_ = min s_len (max 0 orig_end) in
  let len = max 0 (end_ - start) in
  String.sub s start len

let lines_of_pos_range s start_pos end_pos =
  let open Lexing in
  let start = start_pos.pos_bol in
  let end_ = find_end_of_line s end_pos.pos_bol in
  safe_string_sub s start (end_ - start)
  |> ensure_newline

let lines_of_loc_range s (start_pos, _) (_, end_pos) =
  lines_of_pos_range s start_pos end_pos

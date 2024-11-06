(*
   Keep a copy of the source input, suitable for returning source code
   from ranges of locations.
*)

open Printf

type source = File of string | Stdin | String | Channel

(* The contents of the source document. *)
type t = { source : source; contents : string }

let source x = x.source

let show_source = function
  | File s -> s
  | Stdin -> "<stdin>"
  | String -> "<string>"
  | Channel -> "<channel>"

let source_string x = x |> source |> show_source
let contents x = x.contents
let length x = String.length x.contents
let replace_contents x f = { x with contents = f x.contents }
let of_string ?(source = String) contents = { source; contents }

let partial_input max_len ic =
  let buf = Bytes.create max_len in
  let rec read pos remaining =
    if remaining > 0 then
      let n_read = input ic buf pos remaining in
      if n_read > 0 then read (pos + n_read) (remaining - n_read) else pos
    else pos
  in
  let len = read 0 max_len in
  Bytes.sub_string buf 0 len

let get_channel_length ic =
  try Some (in_channel_length ic) with
  | _ -> None

let input_all_from_nonseekable_channel ic =
  let buf = Buffer.create 10000 in
  try
    while true do
      bprintf buf "%s\n" (input_line ic)
    done;
    assert false
  with
  | End_of_file -> Buffer.contents buf

let of_channel ?(source = Channel) ?max_len ic =
  let contents =
    match max_len with
    | None -> (
        match get_channel_length ic with
        | None -> input_all_from_nonseekable_channel ic
        | Some len -> really_input_string ic len)
    | Some max_len -> partial_input max_len ic
  in
  { source; contents }

let of_stdin ?(source = Stdin) () = of_channel ~source stdin

let of_file ?source ?max_len file =
  let source =
    match source with
    | None -> File file
    | Some x -> x
  in
  (* This needs to work on named pipes such as those created by bash with
     so-called "process substitution" (for those, 'in_channel_length' returns
     but then we can't read the file again).
     It's convenient for testing using the spacegrep command line:
     $ spacegrep hello <(echo 'hello')
  *)
  let contents = UFile.Legacy.read_file ?max_len file in
  { source; contents }

let to_lexbuf x =
  let lexbuf = Lexing.from_string x.contents in
  Lexing.set_filename lexbuf (show_source x.source);
  lexbuf

(* Find the index (position from the beginning of the string)
   right after the end of the current line. *)
let rec find_end_of_line s i =
  if i >= String.length s then i
  else
    match s.[i] with
    | '\n' -> i + 1
    | _ -> find_end_of_line s (i + 1)

(* Remove the trailing newline character if there is one. *)
let remove_trailing_newline s =
  match s with
  | "" -> ""
  | s ->
      let len = String.length s in
      if s.[len - 1] = '\n' then String.sub s 0 (len - 1) (* nosem *) else s

(* Add a trailing newline character if the last character isn't a newline
   (or there is no last character). *)
let ensure_newline s =
  match s with
  | "" -> ""
  | s -> if s.[String.length s - 1] <> '\n' then s ^ "\n" else s

let insert_line_prefix prefix s =
  if prefix = "" then s
  else if s = "" then s
  else
    let buf = Buffer.create (2 * String.length s) in
    Buffer.add_string buf prefix;
    let len = String.length s in
    for i = 0 to len - 1 do
      let c = s.[i] in
      Buffer.add_char buf c;
      if c = '\n' && i < len - 1 then Buffer.add_string buf prefix
    done;
    Buffer.contents buf

let insert_highlight highlight s start end_ =
  let len = String.length s in
  if start < 0 || end_ > len || start > end_ then s
  else
    let buf = Buffer.create (2 * len) in
    for i = 0 to start - 1 do
      Buffer.add_char buf s.[i]
    done;
    let pos = ref start in
    for i = start to end_ - 1 do
      match s.[i] with
      | '\n' ->
          Buffer.add_string buf (highlight (String.sub s !pos (i - !pos)));
          Buffer.add_char buf '\n';
          pos := i + 1
      | _ -> ()
    done;
    Buffer.add_string buf (highlight (String.sub s !pos (end_ - !pos)));
    for i = end_ to len - 1 do
      Buffer.add_char buf s.[i]
    done;
    Buffer.contents buf

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

let region_of_pos_range x start_pos end_pos =
  let open Lexing in
  safe_string_sub x.contents start_pos.pos_cnum
    (end_pos.pos_cnum - start_pos.pos_cnum)

let region_of_loc_range x (start_pos, _) (_, end_pos) =
  region_of_pos_range x start_pos end_pos

let lines_of_pos_range ?(force_trailing_newline = true) ?highlight
    ?(line_prefix = "") x start_pos end_pos =
  let s = x.contents in
  let open Lexing in
  let start = start_pos.pos_bol in
  let match_start = start_pos.pos_cnum in
  assert (match_start >= start);
  let end_ = find_end_of_line s end_pos.pos_bol in
  let match_end = end_pos.pos_cnum in
  assert (match_end <= end_);
  let lines =
    let s = safe_string_sub s start (end_ - start) in
    if force_trailing_newline then ensure_newline s else s
  in
  let with_highlight =
    match highlight with
    | None -> lines
    | Some highlight ->
        insert_highlight highlight lines (match_start - start)
          (match_end - start)
  in
  insert_line_prefix line_prefix with_highlight

let lines_of_loc_range ?force_trailing_newline ?highlight ?line_prefix x
    (start_pos, _) (_, end_pos) =
  lines_of_pos_range ?force_trailing_newline ?highlight ?line_prefix x start_pos
    end_pos

let list_lines_of_pos_range ?highlight ?line_prefix x start_pos end_pos =
  let s =
    lines_of_pos_range ~force_trailing_newline:false ?highlight ?line_prefix x
      start_pos end_pos
  in
  remove_trailing_newline s |> String.split_on_char '\n'

let list_lines_of_loc_range ?highlight ?line_prefix x (start_pos, _) (_, end_pos)
    =
  list_lines_of_pos_range ?highlight ?line_prefix x start_pos end_pos

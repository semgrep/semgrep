(*
   Keep a copy of the source input, suitable for returning source code
   from ranges of locations.
*)

open Printf

type source =
  | File of string
  | Stdin
  | String
  | Channel

(* The contents of the source document. *)
type t = {
  source: source;
  contents: string;
}

let source x = x.source

let show_source = function
  | File s -> s
  | Stdin -> "<stdin>"
  | String -> "<string>"
  | Channel -> "<channel>"

let source_string x = x |> source |> show_source

let contents x = x.contents

let of_string ?(source = String) contents = {
  source;
  contents
}

let of_channel ?(source = Channel) ic =
  let buf = Buffer.create 10000 in
  (try
     while true do
       bprintf buf "%s\n" (input_line ic)
     done;
     assert false
   with End_of_file ->
     let contents = Buffer.contents buf in
     { source; contents }
  )

let of_stdin ?(source = Stdin) () = of_channel ~source stdin

let of_file ?source file =
  let ic = open_in_bin file in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
       let contents = really_input_string ic (in_channel_length ic) in
       let source =
         match source with
         | None -> File file
         | Some x -> x
       in
       {
         source;
         contents;
       }
    )

let to_lexbuf x =
  Lexing.from_string x.contents

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

let insert_line_prefix prefix s =
  if prefix = "" then s
  else
    if s = "" then s
    else
      let buf = Buffer.create (2 * String.length s) in
      Buffer.add_string buf prefix;
      let len = String.length s in
      for i = 0 to len - 1 do
        let c = s.[i] in
        Buffer.add_char buf c;
        if c = '\n' && i < len - 1 then
          Buffer.add_string buf prefix
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

let lines_of_pos_range ?(line_prefix = "") x start_pos end_pos =
  let s = x.contents in
  let open Lexing in
  let start = start_pos.pos_bol in
  let end_ = find_end_of_line s end_pos.pos_bol in
  let lines =
    safe_string_sub s start (end_ - start)
    |> ensure_newline
  in
  insert_line_prefix line_prefix lines

let lines_of_loc_range ?line_prefix x (start_pos, _) (_, end_pos) =
  lines_of_pos_range ?line_prefix x start_pos end_pos

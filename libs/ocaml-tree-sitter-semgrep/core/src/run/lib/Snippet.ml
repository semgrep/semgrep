(*
   Functions to extract a good snippet of code, with the error highlighted.
*)

type position = Tree_sitter_bindings.Tree_sitter_output_t.position = {
  row: int; (* line number, starting from 0. *)
  column: int; (* position within the line, starting from 0. *)
}

type snippet_fragment =
  | Normal of string
  | Highlight of string
  | Ellipsis

type snippet_line = snippet_fragment list

type t = snippet_line list

(* Split string at the specified position. *)
let split s pos2 =
  let a = Util_string.safe_sub s 0 pos2 in
  let b = Util_string.safe_sub s pos2 (String.length s - pos2) in
  a, b

let normal_max_length = 120
let highlight_max_length = 200

let ellide max_len s =
  assert (max_len >= 2);
  let len = String.length s in
  if len > max_len then
    let len2 = max_len/2 in
    let a = Util_string.safe_sub s 0 len2 in
    let b = Util_string.safe_sub s (len-len2) len2 in
    Some (a, b)
  else
    None

let shorten_snippet_line line =
  List.map (fun frag ->
    match frag with
    | Normal s ->
        (match ellide normal_max_length s with
         | None -> [frag]
         | Some (a, b) -> [Normal a; Ellipsis; Normal b]
        )
    | Highlight s ->
        (match ellide highlight_max_length s with
         | None -> [frag]
         | Some (a, b) -> [Highlight a; Ellipsis; Highlight b]
        )
    | Ellipsis ->
        [Ellipsis]
  ) line
  |> List.flatten

let shorten_snippet_lines lines =
  List.map shorten_snippet_line lines

(* Ensure all lines are LF-terminated. *)
let add_missing_newline line =
  if line = "" || line.[String.length line - 1] <> '\n' then
    line ^ "\n"
  else
    line

let extract
    ?(lines_before = 2)
    ?(lines_after = 2)
    ~start_pos
    ~end_pos
    (src : Src_file.t) =
  (*
     The lines stored in src.lines are complete, including the trailing
     LF character.
  *)
  let src_line_count = Array.length src.lines in
  if src_line_count = 0 then []
  else
    (*
       In what follows, 'start' is inclusive and 'end' is exclusive,
       such that a length is always 'end' - 'start'.
    *)
    let start_line = start_pos.row in
    let end_line = end_pos.row + 1 in
    let snip_start_line = max (start_line - lines_before) 0 in
    let snip_end_line = min (end_line + lines_after) src_line_count in
    let line_acc = ref [] in
    let add line = line_acc := line :: !line_acc in
    for line_num = snip_start_line to snip_end_line - 1 do
      let line =
        Src_file.safe_get_row src line_num
        |> add_missing_newline
      in
      if line_num < start_line || line_num >= end_line then
        (* Highlight nothing. *)
        add [Normal line]
      else
        (* Highlight something *)
      if start_line = end_line - 1 then
        let start_column = start_pos.column in
        let end_column =
          (* Highlight the start position even if the range is empty.
             If we're at the end of the line, we'll produce (Highlight "\n")
             which is rendered as a special case by the highlighter. *)
          max (start_column + 1) end_pos.column
        in
        (* Highlight substring in the middle of a line. *)
        let ab, c = split line end_column in
        let a, b = split ab start_column in
        add [
          Normal a;
          Highlight b;
          Normal c;
        ]
      else if line_num = start_line then
        (* Highlight the end of the line. *)
        let a, b = split line start_pos.column in
        add [
          Normal a;
          Highlight b;
        ]
      else if line_num = end_line - 1 then
        (* Highlight the beginning of the line. *)
        let a, b = split line end_pos.column in
        add [
          Highlight a;
          Normal b;
        ]
      else
        (* Highlight everything. *)
        add [Highlight line]
    done;
    List.rev !line_acc
    |> shorten_snippet_lines

let ansi_term_highlight s =
  match s with
  | "" -> s
  | s -> ANSITerminal.(sprintf [Bold; Underlined; red] "%s" s)

let format_line buf ~color line =
  let highlight =
    if color then ansi_term_highlight
    else (fun s -> s)
  in
  List.iter (fun frag ->
    match frag with
    | Normal s -> Buffer.add_string buf s
    | Highlight "\n" ->
        Buffer.add_string buf (highlight " \n")
    | Highlight s -> Buffer.add_string buf (highlight s)
    | Ellipsis -> Buffer.add_string buf "…"
  ) line

(* Replace all characters except newlines *)
let replace s char =
  String.map (function
    | '\n' -> '\n'
    | _ -> char
  ) s

let format_underline buf ~color line =
  if not color
  && List.exists (function Highlight _ -> true | _ -> false) line then (
    List.iter (fun frag ->
      match frag with
      | Normal s -> Buffer.add_string buf (replace s ' ')
      | Highlight "\n" ->
          Buffer.add_string buf "^\n"
      | Highlight s -> Buffer.add_string buf (replace s '^')
      | Ellipsis -> Buffer.add_string buf " "
    ) line
  )

type style = Auto | Color | Text

let format ~style lines =
  let color =
    match style with
    | Auto -> Unix.(isatty stdout) && Unix.(isatty stderr)
    | Color -> true
    | Text -> false
  in
  let buf = Buffer.create 1000 in
  lines
  |> List.iter (fun line ->
    format_line buf ~color line;
    format_underline buf ~color line;
  );
  Buffer.contents buf

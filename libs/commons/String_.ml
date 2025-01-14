let unit_str ?(pad = false) count str =
  let str = if count <> 1 then str ^ "s" else if pad then str ^ " " else str in
  Printf.sprintf "%d %s" count str

let search ~term str =
  try Some (Str.search_forward (Str.regexp_string term) str 0) with
  | Not_found -> None

let contains ~term str = search ~term str <> None
let empty s = s = ""
let split ~sep s = Str.split (Str.regexp sep) s

let safe_sub str virtual_start virtual_sublen =
  let len = String.length str in
  (* Treat negative length as zero length *)
  let virtual_sublen = max 0 virtual_sublen in
  (* Convert possibly out-of-range start and end to legal start and end. *)
  let virtual_end = virtual_start + virtual_sublen in
  let virtual_end =
    if virtual_end < virtual_start then
      (* int overflow from the addition above *)
      max_int
    else virtual_end
  in
  let real_start = max 0 (min virtual_start len) in
  let real_end = max 0 (min len virtual_end) in
  let real_sublen = real_end - real_start in
  (* It should be safe now *)
  String.sub str real_start real_sublen

let show ?(max_len = 200) str =
  let len = String.length str in
  if len > max_len then
    Printf.sprintf "%S (%i bytes)"
      (safe_sub str 0 max_len ^ "...")
      (String.length str)
  else Printf.sprintf "%S" str

let trim_cr s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\r' then String.sub s 0 (len - 1) else s

let lines_of_range (start_offset, end_offset) str =
  let len = String.length str in
  if
    start_offset > end_offset || end_offset > len || start_offset < 0 || len = 0
  then []
  else
    (* Search left for a '\n' char so we can determine the start of the first
     * line in the range. *)
    let start_offset =
      (* Consider start_offset here:
       * "foo\nbar\n"
       *     ^
       * We want to include "foo" in the result, so we move the cursor left one
       * character in this case.
       *
       * NB: This can result in a start_offset of -1. That is acceptable to
       * rindex_from_opt and the unit tests exercise that case.
       *)
      if str.[start_offset] = '\n' then start_offset - 1 else start_offset
    in
    let start_line_offset =
      match String.rindex_from_opt str start_offset '\n' with
      | None -> 0
      | Some x ->
          (* We want to start the resulting substring at the beginning of the
           * line and not include the newline character itself, so add one. Note
           * that this can set `start_line_offset` to `len`, but that is an
           * acceptable second argument to `String.sub`. *)
          x + 1
    in
    (* Search right for a '\n' char so we can determine the end of the last line
     * in the range. *)
    let end_line_offset =
      match String.index_from_opt str end_offset '\n' with
      | None -> len
      | Some x -> x
    in
    let substr =
      String.sub str start_line_offset (end_line_offset - start_line_offset)
    in
    let lines = String.split_on_char '\n' substr in
    List_.map trim_cr lines

let is_capitalized s =
  String.length s <> 0
  &&
  match s.[0] with
  (* https://ocaml.org/manual/5.2/patterns.html#sss:pat-range *)
  | 'A' .. 'Z' -> true
  | _ -> false

let truncate_with_message max_len fmt s =
  if String.length s > max_len then
    let truncated_length = String.length s - max_len in
    let trunc_s = Str.first_chars s max_len in
    fmt trunc_s truncated_length
  else s

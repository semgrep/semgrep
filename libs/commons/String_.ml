open Common

let unit_str ?(pad = false) count str =
  let str = if count <> 1 then str ^ "s" else if pad then str ^ " " else str in
  spf "%d %s" count str

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

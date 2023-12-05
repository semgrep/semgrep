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

open Common

let unit_str ?(pad = false) count str =
  let str = if count <> 1 then str ^ "s" else if pad then str ^ " " else str in
  spf "%d %s" count str

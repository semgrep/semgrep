(** Recognition of Semgrep's legacy regexp-literal convention. Keep its syntax
    in one place so parsers preserve regexp escapes before matching. *)

open Common

let regexp = "^=~/\\(.*\\)/\\([mi]?\\)$"
let is_regexp text = text =~ regexp

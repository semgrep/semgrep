
type org_line =
  | Header of int * string (* full string, including starting stars *)
  | Comment of string
  | Other of string

type org = org_line list

val parse:
  Common.filename -> org

val highlight:
  org -> (string * Highlight_code.category option * Common2.filepos) list

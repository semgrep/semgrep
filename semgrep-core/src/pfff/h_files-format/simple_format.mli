open Common2.BasicType

val regexp_comment_line : string

val cat_and_filter_comments : filename -> string list

val title_colon_elems_space_separated :
  filename -> (string * string list) list

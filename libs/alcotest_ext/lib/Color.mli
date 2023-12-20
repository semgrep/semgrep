(*
   Colorize the output
*)

type style = Default | Red | Green | Yellow | Cyan | Bold

val format : style -> string -> string

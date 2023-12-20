(*
   Colorize the output
*)

type style = Default | Red | Green | Yellow | Bold

val format : style -> string -> string

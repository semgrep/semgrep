(*
   Colorize the output

   (TODO: use an external library for this?)
*)

type conf = Color | No_color
type style = Default | Red | Green | Yellow | Bold

val format : conf -> style -> string -> string

(*
   Colorize the output
*)

type style = Default | Red | Green | Yellow | Cyan | Bold

let ansi_code_of_style = function
  | Default -> "0"
  | Red -> "31"
  | Green -> "32"
  | Yellow -> "33"
  | Cyan -> "36"
  | Bold -> "1"

let format style str =
  Printf.sprintf "\027[%sm%s\027[%sm" (ansi_code_of_style style) str
    (ansi_code_of_style Default)

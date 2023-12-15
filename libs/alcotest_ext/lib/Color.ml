(*
   Colorize the output
*)

type conf = Color | No_color

(* TODO: allow combined styles like "bold red"? *)
type style = Default | Red | Green | Yellow | Bold

let ansi_code_of_style = function
  | Default -> "0"
  | Red -> "31"
  | Green -> "32"
  | Yellow -> "33"
  | Bold -> "1"

let format conf style str =
  match conf with
  | Color ->
      Printf.sprintf "\027[%sm%s\027[%sm" (ansi_code_of_style style) str
        (ansi_code_of_style Default)
  | No_color -> str

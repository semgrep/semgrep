(* python: original code:
   class Colors(Enum):
       # these colors come from user's terminal theme
       foreground = 0
       white = 7
       black = 256
       cyan = "cyan"  # for filenames
       green = "green"  # for autofix
       yellow = "yellow"  # TODO: benchmark timing output?
       red = "red"  # for errors
       bright_blue = "bright_blue"  # TODO: line numbers?

       # these colors ignore user's terminal theme
       forced_black = 16  # #000
       forced_white = 231  # #FFF
*)

(* python: was in constant.py, but not really constants, more like types *)

type _color =
  | Foreground
  | White
  | Black
  | Cyan
  | Green
  | Yellow
  | Red
  | Bright_blue
  | Forced_black
  | Forced_white

type color_code = Int of int | String of string

(* What's the encoding? *)
let _encode_color = function
  | Foreground -> Int 0
  | White -> Int 7 (* really? *)
  | Black -> Int 256 (* really? *)
  | Cyan -> String "cyan"
  | Green -> String "green"
  | Yellow -> String "yellow"
  | Red -> String "red"
  | Bright_blue -> String "bright_blue"
  | Forced_black -> Int 16
  | Forced_white -> Int 231

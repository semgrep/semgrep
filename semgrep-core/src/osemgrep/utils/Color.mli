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


(* Canonical color form. Most precise. Between 0 and 1 *)
type rgbf = (float * float * float)

(* between 0 and 255 *)
type rgb = (int * int * int)
(* as in graphics.ml *)
type color = int
(* emacs color names *)
type emacs_color = string


(* xxx_of_rgbf *)
val rgb_of_rgbf: rgbf -> rgb
val color_of_rgbf: rgbf -> color

(* rgbf_of_xxx *)
val rgbf_of_string: emacs_color -> rgbf
val rgbf_of_rgb: rgb -> rgbf
val rgbf_of_color: color -> rgbf

val rainbow_array: rgbf array
val rainbow_color: int -> rgbf

val emacs_colors: (emacs_color * rgbf) list

val emacs_basic_colors: (emacs_color * rgbf) list
val emacs_degrade_colors: (emacs_color * rgbf) list
val emacs_gray_colors: (emacs_color * rgbf) list

(* choose one of the pool of colors *)
val random_emacs_color: (emacs_color * rgbf) list -> emacs_color

(* extra converters *)
val rgb_of_string: string -> rgb
val color_of_string: string -> color
val color_of_rgb: rgb -> color
val rgb_of_color: color -> rgb

(* find closest emacs color name *)
val string_of_color: color -> emacs_color

(* alias for Graphics.rgb *)
val rgb: int -> int -> int -> color

val unrgb: color -> rgb

(* shortcut *)
val c: string -> color

val black : color
val white : color
val red : color
val green : color
val blue : color
val yellow : color
val cyan : color
val magenta : color

val blackf : rgbf
val whitef : rgbf
val redf : rgbf
val greenf : rgbf
val bluef : rgbf
val yellowf : rgbf
val cyanf : rgbf
val magentaf : rgbf


type degrade =
  | Degrade1
  | Degrade2
  | Degrade3
  | Degrade4
val mk_degrade: int -> degrade

val degrade: string -> degrade -> color
val degrade_random: string -> color

(* when 100 it will be black, when 0 it will be white *)
val degrade_grey:
  float (* 0 .. 100 *) -> color

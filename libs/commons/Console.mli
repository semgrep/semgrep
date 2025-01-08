(*
   Utilities to help printing user-facing messages with optional color on
   stdout and stderr.

   Programs such as Semgrep use both stdout and stderr to display
   human-readable messages.

   See UConsole.ml or better CapConsole.ml to actually print messages.
   This is the shared "safe" part of console management.
*)

(*
   Whether we want terminal highlighting to be on or off,
   or detected from the properties of stdout and stderr.
*)
type highlight_setting = Auto | On | Off [@@deriving show]

(* The result of applying 'highlight_setting' *)
type highlight = On | Off [@@deriving show]

(* Note that this module use the globals at the end of this file to
 * store the current settings but they should be manipulated only
 * by UConsole.setup() (or CapConsole.setup())
 *)

type color = ANSITerminal.color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default  (** Default color of the terminal *)

type style = ANSITerminal.style =
  | Reset
  | Bold
  | Underlined
  | Blink
  | Inverse
  | Hidden
  | Foreground of color
  | Background of color

(* Shortcuts for [Foreground xxx] *)
val black : style
val red : style
val green : style
val yellow : style
val blue : style
val magenta : style
val cyan : style
val white : style
val default : style

(* Will display special color/style ANSI sequence if highlighting is on.
 * Note that this will not work on the default Windows terminal
 * (install a fancier ANSI compatible one or use VSCode which integrates one)
 *)
val sprintf : style list -> ('a, unit, string) format -> 'a

(* sprintf shortcuts *)
val color : style -> string -> string
val bold : string -> string
val underline : string -> string

(*
   These functions turn a string into color (red, yellow, or green)
   if highlighting is on.
*)
val error : string -> string
val warning : string -> string
val success : string -> string

(* Constant messages " ERROR ", " WARNING ", etc. whose formatting depends
   on the 'highlight' setting.
   TODO: explain why they're padded with one space character.
*)
val error_tag : unit -> string
val warning_tag : unit -> string
val success_tag : unit -> string

(* Pretty-prints the heading in a box: [heading "Hello"] is layouted as:
    ---------
    | Hello |
    ---------
*)
val heading : string -> string

(* Pretty-prints the table with the heading. The first row are strings,
   the remaining are integers. The first row is left-aligned, all others
   right-aligned.
   [table ("H1", [ "H2"; "H3"]) [ ("A", [ 1; 2 ]); ("B", [ 100; 20 ]) ]]

     H1  H2 H3
     ---------
     A    1  2
     B  100 20
*)
val table : string * string list -> (string * int list) list -> string

(* Pretty-prints two tables with headings side by side, with some spacing in
   between. Look at [table] for the individual arguments. *)
val tables :
  string * string list * (string * int list) list ->
  string * string list * (string * int list) list ->
  string

(*
   Query the global state.
   This setting is shared by stdout and stderr.
*)
val get_highlight_setting : unit -> highlight_setting
val get_highlight : unit -> highlight

(* internals, you should not change that, only UConsole.setup can *)
val highlight_setting : highlight_setting ref
val highlight : highlight ref

(*
   Utilities to help printing user-facing messages with optional color on
   stdout and stderr.

   Programs such as Semgrep use both stdout and stderr to display
   human-readable messages.

   The 'Logs_' module depends on this module because logging
   is done on stderr by default.

   TODO: use this module for printing all user-facing messages.
   Maybe it's only possible once we drop pysemgrep because of bugs and
   quirks we have to reproduce for now.

   See UConsole.ml (or better CapConsole.ml) to actually print messages.
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

(*
   Query the global state.
   This setting is shared by stdout and stderr.
*)
val get_highlight_setting : unit -> highlight_setting
val get_highlight : unit -> highlight

(*
   These functions turn a string into color (red, yellow, or green)
   if highlighting is on.
*)
val error : string -> string
val warning : string -> string
val success : string -> string

(*
   These functions turn a string into bold white face with a background
   color (red, yellow, or green) if highlighting is on.
*)
val strong_error : string -> string
val strong_warning : string -> string
val strong_success : string -> string

(* Constant messages " ERROR ", " WARNING ", etc. whose formatting depends
   on the 'highlight' setting.
   TODO: explain why they're padded with one space character.
*)
val error_tag : unit -> string
val warning_tag : unit -> string
val success_tag : unit -> string

(* internals, you should not change that, only UConsole.setup can *)
val highlight_setting : highlight_setting ref
val highlight : highlight ref

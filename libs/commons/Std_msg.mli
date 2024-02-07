(*
   Utilities for printing user-facing messages with optional color on stderr
   and stdout.

   Semgrep uses both stdout and stderr to display human-readable messages.

   The 'Logs_' module which depends on this one because logging
   is done on stderr by default.

   TODO: use this module for printing all user-facing messages.
   Maybe it's only possible once we drop pysemgrep because of bugs and
   quirks we have to reproduce for now.
*)

(*
   Whether we want terminal highlighting to be on or off,
   or detected from the properties of stdout and stderr.
*)
type highlight_setting = Auto | On | Off

(* The result of applying 'highlight_setting' *)
type highlight = On | Off

(*
   Set the global state indicating whether we want text to use color and
   font highlighting. The default is 'Auto'.

   Auto: if stdout or stderr is not a terminal, highlighting is turned off.
*)
val setup : ?highlight_setting:highlight_setting -> unit -> unit

(*
   Query the global state.
   This setting is shared by stdout and stderr.
*)
val get_highlight_setting : unit -> highlight_setting
val get_highlight : unit -> highlight

(*
   Set the 'highlight' setting temporary for the execution of the function.
   This is intended for tests.
*)
val with_highlight : highlight_setting -> (unit -> 'a) -> 'a

(*
   These functions add opening and closing markers for highlighting
   the string in an ANSI terminal if applicable.
   Depends on the 'highlight' setting.
*)
val highlight_error : string -> string
val highlight_warning : string -> string
val highlight_success : string -> string

(* Constant messages " ERROR ", " WARNING ", etc. whose formatting depends
   on the 'highlight' setting.
   TODO: explain why they're padded with one space character.
*)
val error_tag : unit -> string
val warning_tag : unit -> string
val success_tag : unit -> string

(* Print a string, print a newline, and flush the stdout channel. *)
val print : string -> unit

(* Print a string, print a newline, and flush the stderr channel. *)
val eprint : string -> unit

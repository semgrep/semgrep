(* See Console.mli for more information
 *
 * DO NOT USE THIS FILE! You should use CapConsole.mli instead.
 *)

(*
   Set the global state indicating whether we want text to use color and
   font highlighting. The default is 'Auto'.

   Auto: if stdout or stderr is not a terminal, highlighting is turned off.
*)
val setup : ?highlight_setting:Console.highlight_setting -> unit -> unit

(*
   Set the 'highlight' setting temporary for the execution of the function.
   This is intended for tests.
*)
val with_highlight : Console.highlight_setting -> (unit -> 'a) -> 'a

(* Print a string, print a newline, and flush the stdout channel. *)
val print : string -> unit

(* Print a string and flush the stdout channel. *)
val print_no_nl : string -> unit

(* Print a string, print a newline, and flush the stderr channel.
 * You should avoid using this function; Prefer Logs.err or Logs.warn
 * in general.
 *)
val eprint : string -> unit

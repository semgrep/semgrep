(* File: ANSITerminal.mli

   Copyright 2004 Troestler Christophe
   Christophe.Troestler(at)umh.ac.be

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)
(** This module offers basic control of ANSI compliant terminals.

  @author Christophe Troestler
  @version 0.3
*)

(** {2 Color} *)

type color =
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  | Default (** Default color of the terminal *)

(** Various styles for the text.  [Blink] and [Hidden] may not work on
    every terminal. *)
type style =
  | Reset
  | Bold | Underlined | Blink | Inverse | Hidden
  | Foreground of color
  | Background of color

val black : style	(* Shortcut for [Foreground Black] *)
val red : style		(* Shortcut for [Foreground Red] *)
val green : style	(* Shortcut for [Foreground Green] *)
val yellow : style	(* Shortcut for [Foreground Yellow] *)
val blue : style	(* Shortcut for [Foreground Blue] *)
val magenta : style	(* Shortcut for [Foreground Magenta] *)
val cyan : style	(* Shortcut for [Foreground Cyan] *)
val white : style	(* Shortcut for [Foreground White] *)
val default : style	(* Shortcut for [Foreground Default] *)

val on_black : style	(* Shortcut for [Background Black] *)
val on_red : style	(* Shortcut for [Background Red] *)
val on_green : style	(* Shortcut for [Background Green] *)
val on_yellow : style	(* Shortcut for [Background Yellow] *)
val on_blue : style	(* Shortcut for [Background Blue] *)
val on_magenta : style	(* Shortcut for [Background Magenta] *)
val on_cyan : style	(* Shortcut for [Background Cyan] *)
val on_white : style	(* Shortcut for [Background White] *)
val on_default : style	(* Shortcut for [Background Default] *)

val set_autoreset : bool -> unit
  (** Turns the autoreset feature on and off.  It defaults to on. *)

val print_string : style list -> string -> unit
  (** [print_string attr txt] prints the string [txt] with the
    attibutes [attr].  After printing, the attributes are
    automatically reseted to the defaults, unless autoreset is turned
    off. *)

val printf : style list -> ('a, unit, string, unit) format4 -> 'a
  (** [printf attr format arg1 ... argN] prints the arguments
    [arg1],...,[argN] according to [format] with the attibutes [attr].
    After printing, the attributes are automatically reseted to the
    defaults, unless autoreset is turned off. *)


(** {2 Erasing} *)

type loc = Above | Below | Screen

val erase : loc -> unit
  (** [erase Above] erases everything before the position of the cursor.
    [erase Below] erases everything after the position of the cursor.
    [erase Screen] erases the whole screen.
  *)


(** {2 Cursor} *)

val set_cursor : int -> int -> unit
  (** [set_cursor x y] puts the cursor at position [(x,y)], [x]
    indicating the column (the leftmost one being 1) and [y] being the
    line (the topmost one being 1).  If [x <= 0], the [x] coordinate
    is unchanged; if [y <= 0], the [y] coordinate is unchanged.  *)

val move_cursor : int -> int -> unit
  (** [move_cursor x y] moves the cursor by [x] columns (to the right
    if [x > 0], to the left if [x < 0]) and by [y] lines (downwards if
    [y > 0] and upwards if [y < 0]). *)

val save_cursor : unit -> unit
  (** [save_cursor()] saves the current position of the cursor. *)

val restore_cursor : unit -> unit
  (** [restore_cursor()] replaces the cursor to the position saved
    with [save_cursor()]. *)


(** {2 Scrolling} *)

val scroll : int -> unit
  (** [scroll n] scrolls the terminal by [n] lines, up (creating new
    lines at the bottom) if [n > 0] and down if [n < 0]. *)

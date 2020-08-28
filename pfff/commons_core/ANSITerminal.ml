(* File: ANSITerminal.ml
   Allow colors, cursor movements, erasing,... under Unix and DOS shells.
   *********************************************************************

   Copyright 2004 by Troestler Christophe
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
(** See the file ctlseqs.html (unix)
    and (for DOS) http://www.ka.net/jmenees/Dos/Ansi.htm
 *)


open Printf

(* Erasing *)

type loc = Above | Below | Screen

let erase = function
  | Above -> print_string "\027[1J"
  | Below -> print_string "\027[0J"
  | Screen -> print_string "\027[2J"


(* Cursor *)

let set_cursor x y =
  if x <= 0 then (if y > 0 then printf "\027[%id" y)
  else (* x > 0 *) if y <= 0 then printf "\027[%iG" x
  else printf "\027[%i;%iH" y x

let move_cursor x y =
  if x > 0 then printf "\027[%iC" x
  else if x < 0 then printf "\027[%iD" (-x);
  if y > 0 then printf "\027[%iB" y
  else if y < 0 then printf "\027[%iA" (-y)

let save_cursor () = print_string "\027[s"
let restore_cursor () = print_string "\027[u"

(* Scrolling *)

let scroll lines =
  if lines > 0 then printf "\027[%iS" lines
  else if lines < 0 then printf "\027[%iT" (- lines)

(* Colors *)

let autoreset = ref true

let set_autoreset b = autoreset := b


type color =
    Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default

type style =
  | Reset | Bold | Underlined | Blink | Inverse | Hidden
  | Foreground of color
  | Background of color

let black = Foreground Black
let red = Foreground Red
let green = Foreground Green
let yellow = Foreground Yellow
let blue = Foreground Blue
let magenta = Foreground Magenta
let cyan = Foreground Cyan
let white = Foreground White
let default = Foreground Default

let on_black = Background Black
let on_red = Background Red
let on_green = Background Green
let on_yellow = Background Yellow
let on_blue = Background Blue
let on_magenta = Background Magenta
let on_cyan = Background Cyan
let on_white = Background White
let on_default = Background Default

let style_to_string = function
  | Reset -> "0"
  | Bold -> "1"
  | Underlined -> "4"
  | Blink -> "5"
  | Inverse -> "7"
  | Hidden -> "8"
  | Foreground Black -> "30"
  | Foreground Red -> "31"
  | Foreground Green -> "32"
  | Foreground Yellow -> "33"
  | Foreground Blue -> "34"
  | Foreground Magenta -> "35"
  | Foreground Cyan -> "36"
  | Foreground White -> "37"
  | Foreground Default -> "39"
  | Background Black -> "40"
  | Background Red -> "41"
  | Background Green -> "42"
  | Background Yellow -> "43"
  | Background Blue -> "44"
  | Background Magenta -> "45"
  | Background Cyan -> "46"
  | Background White -> "47"
  | Background Default -> "49"


let print_string style txt =
  print_string "\027[";
  let s = String.concat ";" (List.map style_to_string style) in
  print_string s;
  print_string "m";
  print_string txt;
  if !autoreset then print_string "\027[0m"


let printf style = kprintf (print_string style)



(* On DOS & windows, to enable the ANSI sequences, ANSI.SYS should be
   loaded in C:\CONFIG.SYS with a line of the type

   DEVICE = C:\DOS\ANSI.SYS
   DEVICEHIGH=C:\WINDOWS\COMMAND\ANSI.SYS

   This routine checks whether the line is present and, if not, it
   inserts it and tells the user to reboot.

   On WINNT, one will create a ANSI.NT in the user dir and a
   command.com link on the desktop (with Configfilename = our ANSI.NT)
   and tell the user to use it.

   REM: that does NOT work under winxp because OCaml programs are not
   considered to run in DOS mode only...

   http://support.microsoft.com/default.aspx?scid=kb;en-us;816179
   http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/console_functions.asp
*)


(* let is_readable file = *)
(*   try close_in(open_in file); true *)
(*   with Sys_error _ -> false *)

(* let config_sys = "C:\\CONFIG.SYS" *)
(* exception OK *)

(* let win9x () = *)
(*   (\* Locate ANSI.SYS *\) *)
(*   let ansi_sys = List.find is_readable [ *)
(*     "C:\\DOS\\ANSI.SYS"; *)
(*     "C:\\WINDOWS\\COMMAND\\ANSI.SYS"; ] in *)
(*   (\* Parse CONFIG.SYS to see wether it has the right line *\) *)
(*   try *)
(*     let re = Str.regexp_case_fold *)
(*                ("^DEVICE\\(HIGH\\)?[ \t]*=[ \t]*" ^ ansi_sys ^ "[ \t]*$") in *)
(*     let fh = open_in config_sys in *)
(*     begin try *)
(*       while true do *)
(*         if Str.string_match re (input_line fh) 0 then raise OK *)
(*       done *)
(*     with *)
(*     | End_of_file -> *)
(*         (\* Correct line not found: add it *\) *)
(*         close_in fh; *)
(*         raise(Sys_error "win9x") *)
(*     | OK -> close_in fh (\* Correct line found, keep going *\) *)
(*     end *)
(*   with Sys_error _ -> *)
(*     (\* config_sys not does not exists or does not contain the right line. *\) *)
(*     let fh = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] *)
(*                0x777 config_sys in *)
(*     output_string fh ("DEVICEHIGH=" ^ ansi_sys ^ "\n"); *)
(*     close_out fh; *)
(*     prerr_endline "Please restart your computer and rerun the program."; *)
(*     exit 1 *)



(* let winnt home = *)
(*   (\* Locate ANSI.SYS *\) *)
(*   let system = *)
(*     try Sys.getenv "SystemRoot" *)
(*     with Not_found -> "C:\\WINDOWS" in *)
(*   let ansi_sys = *)
(*     List.find is_readable (List.map (fun s -> Filename.concat system s) *)
(*                              [ "SYSTEM32\\ANSI.SYS"; ]) in *)
(*   (\* Create an ANSI.SYS file in the user dir *\) *)
(*   let ansi_nt = Filename.concat home "ANSI.NT" in *)
(*   let fh = open_out ansi_nt in *)
(*   output_string fh "dosonly\ndevice="; *)
(*   output_string fh ansi_sys; *)
(*   output_string fh "\ndevice=%SystemRoot%\\system32\\himem.sys *)
(* files=40 *)
(* dos=high, umb *)
(* " ; *)
(*   close_out fh; *)
(*   (\* Make a command.com link on the desktop *\) *)
(*   let fh = open_out (Filename.concat home "command.lnk") in *)
(*   close_out fh *)


(* let () = *)
(*   if Sys.os_type = "Win32" then begin *)
(*     try  winnt(Sys.getenv "USERPROFILE")  (\* WinNT, Win2000, WinXP *\) *)
(*     with Not_found -> win9x() (\* Win9x *\) *)
(*   end *)

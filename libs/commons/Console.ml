(* Martin Jambon, Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Centralize and unify the use of the standard output (and error) of a
 * program.
 *
 * There are many different ways a program can output a string on
 * the standard output:
 *  - Stdlib.print_string using Stdlib.stdout
 *  - Unix.write using Unix.stdout
 *  - Format.fmt using Formatter.std_formatter
 *  - Fmt.pr, or Fmt.pf using Fmt.stdout
 *  - UCommon.pr
 *  - Ocolor_format.printf
 *  - ...
 *  - and now UConsole.print() and CapConsole.print too :)
 *
 * The goal of this module is to provide another way that can be
 * mocked and grepped and redirected more easily. This is useful
 * for command-line programs that want to clearly separate
 * the normal output of the program from other output.
 *
 * alt:
 *  - Logs.app(), but Logs.app actually print on stderr by default
 *    (without any leading tag), and using a Logs.xxx function for
 *    a regular output feels a bit weird
 *  - use Common.pr(), but we can't easily redirect or mock the output
 *    and we use Common.pr in too many contexts. The use of a separate
 *    UConsole.print() makes it clear this is intended to be the real output
 *    of the program.
 *
 * TODO:
 *  - allow to mock the output, with_mock_output () ?
 *    update: or use Testo recent capture_stdout which uses
 *    some Unix.dupe internally so no need to mock
 *  - allow to redirect the output via a --output flag
 *    with_redirect_output () ?
 *
 * This module contains the shared part between UConsole.ml and
 * CapConsole.ml
 *)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

type highlight_setting = Auto | On | Off [@@deriving show]
type highlight = On | Off [@@deriving show]

let highlight_setting : highlight_setting ref = ref Auto
let highlight : highlight ref = ref (Off : highlight)
let get_highlight_setting () = !highlight_setting
let get_highlight () = !highlight

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type style = Error | Warning | Success

let color_of_style = function
  | Error -> ANSITerminal.red
  | Warning -> ANSITerminal.yellow
  | Success -> ANSITerminal.green

let style_string style str =
  match get_highlight () with
  | On -> ANSITerminal.sprintf [ color_of_style style ] "%s" str
  | Off -> str

let strong_style_string style str =
  match get_highlight () with
  | On ->
      ANSITerminal.sprintf
        [ ANSITerminal.white; ANSITerminal.Bold; color_of_style style ]
        "%s" str
  | Off -> str

(*****************************************************************************)
(* API (see also UConsole.ml and CapConsole.ml) *)
(*****************************************************************************)

let error str = style_string Error str
let warning str = style_string Warning str
let success str = style_string Success str
let strong_error str = strong_style_string Error str
let strong_warning str = strong_style_string Warning str
let strong_success str = strong_style_string Success str
let error_tag () = strong_error " ERROR "
let warning_tag () = strong_warning " WARNING "
let success_tag () = strong_success " SUCCESS "

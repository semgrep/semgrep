(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
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
(* Centralize and unify the use of the standard output of a program.
 *
 * There are many different ways a program can output a string on
 * the standard output:
 *  - Stdlib.print_string using Stdlib.stdout
 *  - Unix.write using Unix.stdout
 *  - Format.fmt using Formatter.std_formatter
 *  - Fmt.pr, or Fmt.pf using Fmt.stdout
 *  - Common.pr
 *  - Ocolor_format.printf
 *  - ...
 *  - and now Out.put() too :)
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
 *    Out.put() makes it clear this is intended to be the real output
 *    of the program.
 *
 * TODO:
 *  - allow to mock the output
 *  - allow to redirect the output via a --output flag
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Out.put :) *)
let put s = Common.pr s
let formatter () = UFormat.std_formatter

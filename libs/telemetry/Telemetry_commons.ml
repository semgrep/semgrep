(* Copyright (C) Semgrep, Inc.
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

(* This module includes redefinitions of functionality provided in [Common], which
 * is duplicated here to avoid circular dependencies. *)

let protect ~finally work =
  (* nosemgrep: no-fun-protect *)
  try Fun.protect ~finally work with
  | Fun.Finally_raised exn ->
      (* old: `catch_and_reraise exn` - this requires pulling in [Exception] *)
      let trace = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace exn trace

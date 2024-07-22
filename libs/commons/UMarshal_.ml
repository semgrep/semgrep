(* Yoann Padioleau
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
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around the Marshal module offering a few additional helpers.
 *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* was in common2.ml before *)
let get_value (filename : Fpath.t) : 'a =
  let chan = UStdlib.open_in_bin !!filename in
  let x = UStdlib.input_value chan in
  (* <=> Marshal.from_channel  *)
  close_in chan;
  x

let write_value (valu : 'a) (filename : Fpath.t) : unit =
  let chan = UStdlib.open_out_bin !!filename in
  UStdlib.output_value chan valu;
  (* <=> Marshal.to_channel *)
  (* Marshal.to_channel chan valu [Marshal.Closures]; *)
  close_out chan

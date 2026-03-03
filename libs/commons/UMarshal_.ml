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

let get_value (filename : Fpath.t) : 'a =
  let chan = Stdlib.open_in_bin !!filename in
  Common.protect
    ~finally:(fun () -> close_in chan)
    (fun () -> Marshal.from_channel chan)

let write_impl (v : 'a) (fn : Fpath.t) ~(closures : bool) : unit =
  let args = if closures then [ Marshal.Closures ] else [] in
  let chan = Stdlib.open_out_bin !!fn in
  try
    Common.protect
      ~finally:(fun () -> close_out chan)
      (fun () -> Marshal.to_channel chan v args)
  with
  | exn ->
      (* Should the marshalling fail for whatever reason, ensure
       * that we tidy up the e.g. half-written file. *)
      let exn = Exception.catch exn in
      (try Sys.remove !!fn with
      | _ -> ());
      Exception.reraise exn

let write_value = write_impl ~closures:false
let write_with_closures = write_impl ~closures:true

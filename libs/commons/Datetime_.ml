(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
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

(* Commentary *)
(* Extra functions for time things *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let of_unix_int_time (timestamp : int64) (sign : [ `Plus | `Minus ])
    (hours : int) (minutes : int) =
  let float_stamp = Int64.to_float timestamp in
  let tm = Timedesc.Timestamp.of_float_s float_stamp in
  let offset_s = (hours * 60 * 60) + (minutes * 60) in
  let offset_s =
    match sign with
    | `Plus -> offset_s
    | `Minus -> -offset_s
  in
  let offset_tm = offset_s |> float_of_int |> Timedesc.Timestamp.of_float_s in
  Timedesc.Timestamp.add tm offset_tm

(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module is inspired by bill's flib-map based autofixer in checkModule.
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let suggest s xs =
  try 
    Some (xs |> Common.find_some (fun s2 ->
      let dist = Common2.edit_distance s s2 in
      if dist <= 2
      then Some (s2, dist)
      else None
    ))
  with Not_found -> None




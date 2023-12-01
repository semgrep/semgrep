(* Yoann Padioleau
 *
 * Copyright (C) 1998-2023 Yoann Padioleau
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

type 'a t = 'a list ref

let push v l = l := v :: !l

let pop l =
  let v = List_.hd_exn "unexpected empty list" !l in
  l := List_.tl_exn "unexpected empty list" !l;
  v

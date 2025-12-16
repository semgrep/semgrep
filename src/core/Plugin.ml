(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(**
   A receiving slot for a plugin.
*)

(* The type of a plugin slot *)
type 'a t = { name : string; default : 'a; mutable plugin : 'a option }

let create_slot ~name default = { name; default; plugin = None }

let get x =
  match x.plugin with
  | None -> x.default
  | Some res -> res

let load x plugin =
  match x.plugin with
  | None -> x.plugin <- Some plugin
  | Some already_there ->
      (* Tolerate the same plugin being loaded twice *)
      if not (Common.phys_equal already_there plugin) then
        failwith
          ("Fatal error: attempt to load two plugins into the same slot "
         ^ x.name)

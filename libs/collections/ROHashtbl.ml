(* Nat Mote
 *
 * Copyright (C) 2025 Semgrep, Inc.
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

include Hashtbl

let create () = Hashtbl.create 0
let of_hashtbl = Fun.id

(* [Base] shadows the library module [Base] for the rest of this file. There
 * is nothing after this module, and inside its own body below, unqualified
 * [Base] still refers to the library (a module can't refer to itself before
 * its own definition is complete). *)
module Base = struct
  type ('a, 'b) t = ('a, 'b) Base.Hashtbl.t

  let of_hashtbl = Fun.id
  let create () = Base.Hashtbl.Poly.create ()

  let of_seq (s : ('a * 'b) Seq.t) : ('a, 'b) t =
    Hashtbl_.Base.hash_of_list (List.of_seq s)

  let find (tbl : ('a, 'b) t) (key : 'a) : 'b = Hashtbl_.Base.find tbl key

  let find_opt (tbl : ('a, 'b) t) (key : 'a) : 'b option =
    Base.Hashtbl.find tbl key

  let mem (tbl : ('a, 'b) t) (key : 'a) : bool = Base.Hashtbl.mem tbl key

  let iter (f : 'a -> 'b -> unit) (tbl : ('a, 'b) t) : unit =
    Base.Hashtbl.iteri tbl ~f:(fun ~key ~data -> f key data)

  let fold (f : 'a -> 'b -> 'acc -> 'acc) (tbl : ('a, 'b) t) (init : 'acc) :
      'acc =
    Base.Hashtbl.fold tbl ~init ~f:(fun ~key ~data acc -> f key data acc)

  let length (tbl : ('a, 'b) t) : int = Base.Hashtbl.length tbl
  let to_alist (tbl : ('a, 'b) t) : ('a * 'b) list = Base.Hashtbl.to_alist tbl
  let keys (tbl : ('a, _) t) : 'a list = Base.Hashtbl.keys tbl
  let data (tbl : (_, 'b) t) : 'b list = Base.Hashtbl.data tbl
end

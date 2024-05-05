(* Nat Mote
 *
 * Copyright (C) 2019-2022 Semgrep Inc.
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

type t = Node of { children : t list; separator : string } | Leaf of string

let of_string str = Leaf str
let combine ?(sep = "") xs = Node { children = xs; separator = sep }

let to_string tree =
  (* Could traverse the tree first to get the total length to avoid buffer
   * resizes *)
  let buf = Buffer.create 16 in
  let rec traverse_tree =
    let rec traverse_list lst separator =
      match lst with
      | [] -> ()
      | [ x ] -> traverse_tree x
      | x :: xs ->
          traverse_tree x;
          Buffer.add_string buf separator;
          traverse_list xs separator
    in
    function
    | Leaf str -> Buffer.add_string buf str
    | Node { children; separator } -> traverse_list children separator
  in
  traverse_tree tree;
  Buffer.contents buf

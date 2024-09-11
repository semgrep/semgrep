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

(*****************************************************************************)
(* Hash and lists *)
(*****************************************************************************)

let hash_to_list h =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h [] |> List.sort compare

let hash_of_list xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
  h

(*****************************************************************************)
(* Hash sets *)
(*****************************************************************************)

type 'a hashset = ('a, bool) Hashtbl.t

let hashset_to_list h = hash_to_list h |> List_.map fst

(* old: slightly slower?
 * let hashset_of_list xs =
 *   xs +> map (fun x -> x, true) +> hash_of_list
 *)
let hashset_of_list (xs : 'a list) : ('a, bool) Hashtbl.t =
  let h = Hashtbl.create (List.length xs) in
  xs |> List.iter (fun k -> Hashtbl.replace h k true);
  h

let hkeys h =
  let hkey = Hashtbl.create 101 in
  h |> Hashtbl.iter (fun k _v -> Hashtbl.replace hkey k true);
  hashset_to_list hkey

(*****************************************************************************)
(* Grouping values by key without find_all *)
(*****************************************************************************)

let push (tbl : ('k, 'v list ref) Hashtbl.t) (key : 'k) (value : 'v) =
  let stack =
    try Hashtbl.find tbl key with
    | Not_found ->
        let stack = ref [] in
        Hashtbl.add tbl key stack;
        stack
  in
  stack := value :: !stack

let peek_opt (tbl : ('k, 'v list ref) Hashtbl.t) (key : 'k) : 'v option =
  match Hashtbl.find_opt tbl key with
  | Some { contents = hd :: _ } -> Some hd
  | Some { contents = [] } -> None
  | None -> None

let get_stack tbl key =
  try !(Hashtbl.find tbl key) with
  | Not_found -> []

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)
let map (f : 'k -> 'v -> 'w) (h : ('k, 'v) Hashtbl.t) : ('k, 'w) Hashtbl.t =
  let res : ('k, 'w) Hashtbl.t = Hashtbl.create 101 in
  h
  |> Hashtbl.iter (fun k v ->
         let w = f k v in
         Hashtbl.add res k w);
  res

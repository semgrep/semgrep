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
(* Hash functions *)
(*****************************************************************************)

type hash = int

let rotate_left1 x =
  let width = Sys.int_size in
  (x lsl 1) lor (x lsr (width - 1))

let combine_hash hash1 hash2 = rotate_left1 hash1 lxor hash2

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

let hashset_to_list h = hash_to_list h |> List.map fst

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
        Hashtbl.replace tbl key stack;
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
      Hashtbl.replace res k w);
  res

let sorted_iter ~cmp f h =
  h |> Hashtbl.to_seq |> List.of_seq
  |> List.sort (fun (key1, _) (key2, _) -> cmp key1 key2)
  |> List.iter (fun (key, data) -> f key data)

let find_default key value_if_not_found h =
  try Hashtbl.find h key with
  | Not_found ->
      Hashtbl.replace h key (value_if_not_found ());
      Hashtbl.find h key

let update_default key ~update:op ~default:value_if_not_found h =
  let old = find_default key value_if_not_found h in
  Hashtbl.replace h key (op old)

(*****************************************************************************)
(* Base-backed twins *)
(*****************************************************************************)

(* [Base] shadows the library module [Base] for the rest of this file. There
 * is nothing after this module, and inside its own body below, unqualified
 * [Base] still refers to the library (a module can't refer to itself before
 * its own definition is complete). *)
module Base = struct
  let find (tbl : ('k, 'v) Base.Hashtbl.t) (key : 'k) : 'v =
    match Base.Hashtbl.find tbl key with
    | Some v -> v
    | None -> raise Not_found

  let hash_of_list (xs : ('k * 'v) list) : ('k, 'v) Base.Hashtbl.t =
    let h = Base.Hashtbl.Poly.create ~size:(List.length xs) () in
    xs |> List.iter (fun (k, v) -> Base.Hashtbl.set h ~key:k ~data:v);
    h

  let hash_to_list (h : ('k, 'v) Base.Hashtbl.t) : ('k * 'v) list =
    Base.Hashtbl.to_alist h |> List.sort compare

  let hkeys (h : ('k, _) Base.Hashtbl.t) : 'k list =
    Base.Hashtbl.keys h |> List.sort compare

  let map (f : 'k -> 'v -> 'w) (h : ('k, 'v) Base.Hashtbl.t) :
      ('k, 'w) Base.Hashtbl.t =
    Base.Hashtbl.mapi h ~f:(fun ~key ~data -> f key data)

  let sorted_iter ~cmp (f : 'k -> 'v -> unit) (h : ('k, 'v) Base.Hashtbl.t) :
      unit =
    h |> Base.Hashtbl.to_alist
    |> List.sort (fun (key1, _) (key2, _) -> cmp key1 key2)
    |> List.iter (fun (key, data) -> f key data)

  type 'a hashset = ('a, bool) Base.Hashtbl.t

  let hashset_of_list (xs : 'a list) : 'a hashset =
    let h = Base.Hashtbl.Poly.create ~size:(List.length xs) () in
    xs |> List.iter (fun k -> Base.Hashtbl.set h ~key:k ~data:true);
    h

  let hashset_to_list (h : 'a hashset) : 'a list =
    hash_to_list h |> List.map fst

  let push (tbl : ('k, 'v list ref) Base.Hashtbl.t) (key : 'k) (value : 'v) :
      unit =
    let stack = Base.Hashtbl.find_or_add tbl key ~default:(fun () -> ref []) in
    stack := value :: !stack

  let peek_opt (tbl : ('k, 'v list ref) Base.Hashtbl.t) (key : 'k) : 'v option =
    match Base.Hashtbl.find tbl key with
    | Some { contents = hd :: _ } -> Some hd
    | Some { contents = [] } -> None
    | None -> None

  let get_stack (tbl : ('k, 'v list ref) Base.Hashtbl.t) (key : 'k) : 'v list =
    match Base.Hashtbl.find tbl key with
    | Some stack -> !stack
    | None -> []

  let find_default (key : 'k) (value_if_not_found : unit -> 'v)
      (h : ('k, 'v) Base.Hashtbl.t) : 'v =
    Base.Hashtbl.find_or_add h key ~default:value_if_not_found

  let update_default (key : 'k) ~(update : 'v -> 'v) ~(default : unit -> 'v)
      (h : ('k, 'v) Base.Hashtbl.t) : unit =
    let old = find_default key default h in
    Base.Hashtbl.set h ~key ~data:(update old)
end

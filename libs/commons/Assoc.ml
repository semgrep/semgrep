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
open Common

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

type ('a, 'b) t = ('a * 'b) list

(*****************************************************************************)
(* Sort *)
(*****************************************************************************)

let sort_prof a b = List.sort a b

let sort_by_val_highfirst xs =
  sort_prof (fun (_k1, v1) (_k2, v2) -> compare v2 v1) xs

let sort_by_val_lowfirst xs =
  sort_prof (fun (_k1, v1) (_k2, v2) -> compare v1 v2) xs

let sort_by_key_highfirst xs =
  sort_prof (fun (k1, _v1) (k2, _v2) -> compare k2 k1) xs

let sort_by_key_lowfirst xs =
  sort_prof (fun (k1, _v1) (k2, _v2) -> compare k1 k2) xs

(*****************************************************************************)
(* Group *)
(*****************************************************************************)

(* Partition elements by key. Preserve the original order. *)
let group_by f xs =
  (* use Hashtbl.find_all property *)
  let h = Hashtbl.create 101 in

  (* could use Set *)
  let hkeys = Hashtbl.create 101 in

  xs
  |> List.iter (fun x ->
         let k = f x in
         Hashtbl.replace hkeys k true;
         Hashtbl.add h k x);
  Hashtbl.fold
    (fun k _ acc -> (k, Hashtbl.find_all h k |> List.rev) :: acc)
    hkeys []

let group_by_multi fkeys xs =
  (* use Hashtbl.find_all property *)
  let h = Hashtbl.create 101 in

  (* could use Set *)
  let hkeys = Hashtbl.create 101 in

  xs
  |> List.iter (fun x ->
         let ks = fkeys x in
         ks
         |> List.iter (fun k ->
                Hashtbl.replace hkeys k true;
                Hashtbl.add h k x));
  Hashtbl.fold (fun k _ acc -> (k, Hashtbl.find_all h k) :: acc) hkeys []

(* you should really use group_assoc_bykey_eff *)
let rec group_by_mapped_key fkey l =
  match l with
  | [] -> []
  | x :: xs ->
      let k = fkey x in
      let xs1, xs2 =
        List.partition
          (fun x' ->
            let k2 = fkey x' in
            k =*= k2)
          xs
      in
      (k, x :: xs1) :: group_by_mapped_key fkey xs2

let group_assoc_bykey_eff xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl.add h k v);
  let keys = Hashtbl_.hkeys h in
  keys |> List_.map (fun k -> (k, Hashtbl.find_all h k))

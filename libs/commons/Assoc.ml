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
(* Set-like operations *)
(*****************************************************************************)
let keys assoc = List_.map fst assoc
let join_keys assoc1 assoc2 = keys assoc1 @ keys assoc2 |> List_.deduplicate

(*****************************************************************************)
(* find *)
(*****************************************************************************)

let find_opt key assoc = List.assoc_opt key assoc

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

(* Partition elements by key. Preserve the original order of the values. *)
let group_by get_key xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun x -> Hashtbl_.push h (get_key x) x);
  Hashtbl.fold (fun k stack acc -> (k, List.rev !stack) :: acc) h []

(* TODO: unused => remove? *)
let group_by_multi get_keys xs =
  let h = Hashtbl.create 101 in
  xs
  |> List.iter (fun x ->
         get_keys x |> List.iter (fun key -> Hashtbl_.push h key x));
  Hashtbl.fold (fun k stack acc -> (k, List.rev !stack) :: acc) h []

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
  xs |> List.iter (fun (k, v) -> Hashtbl_.push h k v);
  let keys = Hashtbl_.hkeys h in
  keys |> List_.map (fun k -> (k, Hashtbl_.get_stack h k))

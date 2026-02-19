(* Yoann Padioleau, Martin Jambon
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

(* nosemgrep: no-list-map *)
let map = List.map

(*****************************************************************************)
(* Additional iterators *)
(*****************************************************************************)

let iter_with_view_into_neighbor_elements
    (f : prev:'a option -> cur:'a -> next:'a option -> unit) xs =
  let rec loop ~prev xs =
    match xs with
    | x :: tail ->
        let next =
          match tail with
          | [] -> None
          | next :: _ -> Some next
        in
        f ~prev ~cur:x ~next;
        loop ~prev:(Some x) tail
    | [] -> ()
  in
  loop ~prev:None xs

exception Map2_different_lengths

let map2_exn f l1 l2 =
  (* nosemgrep: no-list-map2 *)
  try List.map2 f l1 l2 with
  | Invalid_argument "List.map2" -> raise Map2_different_lengths

let map2_opt f l1 l2 =
  try Some (map2_exn f l1 l2) with
  | Map2_different_lengths -> None

(* List.flatten is non-tailrec as of OCaml 5.3. *)
let flatten xss =
  xss |> List.fold_left (fun acc xs -> List.rev_append xs acc) [] |> List.rev

let append a b = List.rev_append (List.rev a) b

(* List.fold_right is non-tailrec as of OCaml 5.3. *)
let fold_right func xs acc =
  List.fold_left (fun acc x -> func x acc) acc (List.rev xs)

(*****************************************************************************)
(* Other list functions *)
(*****************************************************************************)

let hd_opt = function
  | [] -> None
  | x :: _ -> Some x

let hd_exn errmsg xs =
  match xs with
  | [] -> failwith errmsg
  | head :: _ -> head

let tl_exn errmsg xs =
  match xs with
  | [] -> failwith errmsg
  | _ :: tail -> tail

let rec last_opt xs =
  match xs with
  | [] -> None
  | [ x ] -> Some x
  | _ :: tl -> last_opt tl

(* nosemgrep: no-list-mapi *)
let mapi = List.mapi

let rec drop n xs =
  match (n, xs) with
  | 0, _ -> xs
  | _, [] -> failwith "drop: not enough"
  | n, _x :: xs -> drop (n - 1) xs

let take n xs =
  let rec next n xs acc =
    match (n, xs) with
    | 0, _ -> List.rev acc
    | _, [] -> failwith "List_.take: not enough"
    | n, x :: xs -> next (n - 1) xs (x :: acc)
  in
  next n xs []

let enum x n =
  if not (x <= n) then
    failwith (Printf.sprintf "bad values in enum, expect %d <= %d" x n);
  let rec enum_aux acc x n =
    if x = n then n :: acc else enum_aux (x :: acc) (x + 1) n
  in
  List.rev (enum_aux [] x n)

let exclude p xs = List.filter (fun x -> not (p x)) xs

let span (p : 'a -> bool) xs =
  let rec span acc_left xs =
    match xs with
    | [] -> (acc_left, [])
    | x :: tail -> if p x then span (x :: acc_left) tail else (acc_left, xs)
  in
  let acc_left, right = span [] xs in
  (List.rev acc_left, right)

let rec take_safe n xs =
  match (n, xs) with
  | 0, _ -> []
  | _, [] -> []
  | n, x :: xs -> x :: take_safe (n - 1) xs

(* List.split is non-tailrec as of OCaml 5.3. *)
let split xs = fold_right (fun (x, y) (xs, ys) -> (x :: xs, y :: ys)) xs ([], [])

(* List.combine is non-tailrec as of OCaml 5.3. *)
let combine_exn xs ys = map2_exn (fun a b -> (a, b)) xs ys

let null xs =
  match xs with
  | [] -> true
  | _ -> false

let index_list xs =
  if null xs then [] (* enum 0 (-1) generate an exception *)
  else combine_exn xs (enum 0 (List.length xs - 1))

let index_list_0 xs = index_list xs
let index_list_1 xs = xs |> index_list |> map (fun (x, i) -> (x, i + 1))

(*****************************************************************************)
(* Options and lists *)
(*****************************************************************************)

let filter_map_endo f xs =
  let changed = ref false in
  let xs' =
    List.fold_left
      (fun acc x ->
        match f x with
        | None ->
            changed := true;
            acc
        | Some y ->
            (* nosemgrep: physical-inequality *)
            if x != y then changed := true;
            y :: acc)
      [] xs
    |> List.rev
  in
  if !changed then xs' else xs

(* nosemgrep: no-list-filter-map *)
let filter_map = List.filter_map
let filter_some xs = filter_map (fun x -> x) xs

let rec find_some_opt p = function
  | [] -> None
  | x :: l -> (
      match p x with
      | Some v -> Some v
      | None -> find_some_opt p l)

let find_some p xs =
  match find_some_opt p xs with
  | None -> raise Not_found
  | Some x -> x

(* often used in grammar actions in menhir *)
let optlist_to_list = function
  | None -> []
  | Some xs -> xs

(*****************************************************************************)
(* Results and lists *)
(*****************************************************************************)

let map_result = Result_.list_map

(*****************************************************************************)
(* Generic op *)
(*****************************************************************************)

let sort xs = List.sort compare xs

let sort_by_key (key : 'a -> 'b) (cmp : 'b -> 'b -> int) (xs : 'a list) =
  map (fun x -> (key x, x)) xs
  |> List.sort (fun (x, _) (y, _) -> cmp x y)
  |> map snd

(* maybe too slow? use an hash instead to first group, and then in
 * that group remove duplicates? *)
let uniq_by eq xs =
  let rec uniq_by acc xs =
    match xs with
    | [] -> acc
    | x :: xs ->
        if List.exists (fun y -> eq x y) acc then uniq_by acc xs
        else uniq_by (x :: acc) xs
  in
  uniq_by [] xs |> List.rev

let deduplicate_gen ~get_key xs =
  let tbl = Hashtbl.create (List.length xs) in
  (* We could use List.filter but it's not guaranteed to proceed from
     left to right which would result in not necessarily selecting the first
     occurrence of each element *)
  List.fold_left
    (fun acc x ->
      let key = get_key x in
      if Hashtbl.mem tbl key then acc
      else (
        Hashtbl.replace tbl key ();
        x :: acc))
    [] xs
  |> List.rev

let deduplicate xs = deduplicate_gen (fun x -> x) xs

(*****************************************************************************)
(* Misc (was in common2.ml) *)
(*****************************************************************************)

(* Tail-recursive to prevent stack overflows. *)
let join_gen a xs =
  let rec aux acc = function
    | [] -> List.rev acc
    | [ x ] -> List.rev (x :: acc)
    | x :: xs -> aux (a :: x :: acc) xs
  in
  aux [] xs

let enum x n =
  if not (x <= n) then
    failwith (Printf.sprintf "bad values in enum, expect %d <= %d" x n);
  let rec enum_aux acc x n =
    if x = n then n :: acc else enum_aux (x :: acc) (x + 1) n
  in
  List.rev (enum_aux [] x n)

(* for 'open List_.Operators' *)
module Operators = struct
  let ( @ ) = ( @ )
end

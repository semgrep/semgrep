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

(*****************************************************************************)
(* Faster and stack-safe List.map *)
(*****************************************************************************)

open Eq.Operators

(*
   Custom list type used to store intermediate lists, while minimizing
   the number of allocated blocks.
*)
type 'a list5 =
  | Elt of 'a * 'a list5
  | Tuple of 'a * 'a * 'a * 'a * 'a * 'a list5
  | Empty

let rev5 l =
  let rec aux acc l =
    match l with
    | Tuple (e, d, c, b, a, l) ->
        (* common case *)
        aux (a :: b :: c :: d :: e :: acc) l
    | Elt (a, l) -> aux (a :: acc) l
    | Empty -> acc
  in
  aux [] l

let rec slow_map acc f l =
  match l with
  | [] -> rev5 acc
  | [ a ] -> rev5 (Elt (f a, acc))
  | [ a; b ] ->
      let a = f a in
      let b = f b in
      rev5 (Elt (b, Elt (a, acc)))
  | [ a; b; c ] ->
      let a = f a in
      let b = f b in
      let c = f c in
      rev5 (Elt (c, Elt (b, Elt (a, acc))))
  | [ a; b; c; d ] ->
      let a = f a in
      let b = f b in
      let c = f c in
      let d = f d in
      rev5 (Elt (d, Elt (c, Elt (b, Elt (a, acc)))))
  | [ a; b; c; d; e ] ->
      let a = f a in
      let b = f b in
      let c = f c in
      let d = f d in
      let e = f e in
      rev5 (Elt (e, Elt (d, Elt (c, Elt (b, Elt (a, acc))))))
  | a :: b :: c :: d :: e :: l ->
      let a = f a in
      let b = f b in
      let c = f c in
      let d = f d in
      let e = f e in
      slow_map (Tuple (e, d, c, b, a, acc)) f l

let rec fast_map rec_calls_remaining f l =
  if rec_calls_remaining <= 0 then slow_map Empty f l
  else
    match l with
    | [] -> []
    | [ a ] -> [ f a ]
    | [ a; b ] ->
        let a = f a in
        let b = f b in
        [ a; b ]
    | [ a; b; c ] ->
        let a = f a in
        let b = f b in
        let c = f c in
        [ a; b; c ]
    | [ a; b; c; d ] ->
        let a = f a in
        let b = f b in
        let c = f c in
        let d = f d in
        [ a; b; c; d ]
    | [ a; b; c; d; e ] ->
        let a = f a in
        let b = f b in
        let c = f c in
        let d = f d in
        let e = f e in
        [ a; b; c; d; e ]
    | a :: b :: c :: d :: e :: l ->
        let a = f a in
        let b = f b in
        let c = f c in
        let d = f d in
        let e = f e in
        a :: b :: c :: d :: e :: fast_map (rec_calls_remaining - 1) f l

(*
   This implementation of List.map makes at most 1000 non-tailrec calls
   before switching to a slower tailrec implementation.

   Additionally, this implementation guarantees left-to-right evaluation.
*)
let map f l = fast_map 1000 f l

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

(*****************************************************************************)
(* Faster List.map2 *)
(*****************************************************************************)

let rec slow_map2 acc f l1 l2 =
  match (l1, l2) with
  | [], [] -> rev5 acc
  | [ a1 ], [ a2 ] -> rev5 (Elt (f a1 a2, acc))
  | [ a1; b1 ], [ a2; b2 ] ->
      let a = f a1 a2 in
      let b = f b1 b2 in
      rev5 (Elt (b, Elt (a, acc)))
  | [ a1; b1; c1 ], [ a2; b2; c2 ] ->
      let a = f a1 a2 in
      let b = f b1 b2 in
      let c = f c1 c2 in
      rev5 (Elt (c, Elt (b, Elt (a, acc))))
  | [ a1; b1; c1; d1 ], [ a2; b2; c2; d2 ] ->
      let a = f a1 a2 in
      let b = f b1 b2 in
      let c = f c1 c2 in
      let d = f d1 d2 in
      rev5 (Elt (d, Elt (c, Elt (b, Elt (a, acc)))))
  | [ a1; b1; c1; d1; e1 ], [ a2; b2; c2; d2; e2 ] ->
      let a = f a1 a2 in
      let b = f b1 b2 in
      let c = f c1 c2 in
      let d = f d1 d2 in
      let e = f e1 e2 in
      rev5 (Elt (e, Elt (d, Elt (c, Elt (b, Elt (a, acc))))))
  | a1 :: b1 :: c1 :: d1 :: e1 :: l1, a2 :: b2 :: c2 :: d2 :: e2 :: l2 ->
      let a = f a1 a2 in
      let b = f b1 b2 in
      let c = f c1 c2 in
      let d = f d1 d2 in
      let e = f e1 e2 in
      slow_map2 (Tuple (e, d, c, b, a, acc)) f l1 l2
  | _other -> raise (Failure "List_.map2: lists not equal length")

let rec fast_map2 rec_calls_remaining f l1 l2 =
  if rec_calls_remaining <= 0 then slow_map2 Empty f l1 l2
  else
    match (l1, l2) with
    | [], [] -> []
    | [ a1 ], [ a2 ] -> [ f a1 a2 ]
    | [ a1; b1 ], [ a2; b2 ] ->
        let a = f a1 a2 in
        let b = f b1 b2 in
        [ a; b ]
    | [ a1; b1; c1 ], [ a2; b2; c2 ] ->
        let a = f a1 a2 in
        let b = f b1 b2 in
        let c = f c1 c2 in
        [ a; b; c ]
    | [ a1; b1; c1; d1 ], [ a2; b2; c2; d2 ] ->
        let a = f a1 a2 in
        let b = f b1 b2 in
        let c = f c1 c2 in
        let d = f d1 d2 in
        [ a; b; c; d ]
    | [ a1; b1; c1; d1; e1 ], [ a2; b2; c2; d2; e2 ] ->
        let a = f a1 a2 in
        let b = f b1 b2 in
        let c = f c1 c2 in
        let d = f d1 d2 in
        let e = f e1 e2 in
        [ a; b; c; d; e ]
    | a1 :: b1 :: c1 :: d1 :: e1 :: l1, a2 :: b2 :: c2 :: d2 :: e2 :: l2 ->
        let a = f a1 a2 in
        let b = f b1 b2 in
        let c = f c1 c2 in
        let d = f d1 d2 in
        let e = f e1 e2 in
        a :: b :: c :: d :: e :: fast_map2 (rec_calls_remaining - 1) f l1 l2
    | _other -> raise (Failure "List_.map2: lists not equal length")

(*
   This implementation of List.map makes at most 1000 non-tailrec calls
   before switching to a slower tailrec implementation.

   Additionally, this implementation guarantees left-to-right evaluation.
*)
let map2 f l1 l2 = fast_map2 1000 f l1 l2

(*****************************************************************************)
(* Other safer alternatives to List.xxx functions *)
(*****************************************************************************)

(* Tail-recursive to prevent stack overflows. *)
let flatten xss =
  xss |> List.fold_left (fun acc xs -> List.rev_append xs acc) [] |> List.rev

let append a b = List.rev_append (List.rev a) b

let fold_right func xs acc =
  List.fold_left (fun acc x -> func x acc) acc (List.rev xs)

(*****************************************************************************)
(* Other list functions *)
(*****************************************************************************)

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

let mapi f l = map2 f (List.init (List.length l) Fun.id) l

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
    if x =|= n then n :: acc else enum_aux (x :: acc) (x + 1) n
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

(* Safe reimplementation of List.split *)
let split xs = fold_right (fun (x, y) (xs, ys) -> (x :: xs, y :: ys)) xs ([], [])

(* Safe reimplementation of List.combine *)
let combine xs ys = map2 (fun a b -> (a, b)) xs ys

let null xs =
  match xs with
  | [] -> true
  | _ -> false

let index_list xs =
  if null xs then [] (* enum 0 (-1) generate an exception *)
  else combine xs (enum 0 (List.length xs - 1))

let index_list_0 xs = index_list xs
let index_list_1 xs = xs |> index_list |> map (fun (x, i) -> (x, i + 1))

(*****************************************************************************)
(* Options and lists *)
(*****************************************************************************)

(* Tail-recursive to prevent stack overflows. *)
let filter_map f xs =
  List.fold_left
    (fun acc x ->
      match f x with
      | None -> acc
      | Some y -> y :: acc)
    [] xs
  |> List.rev

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
        Hashtbl.add tbl key ();
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
    if x =|= n then n :: acc else enum_aux (x :: acc) (x + 1) n
  in
  List.rev (enum_aux [] x n)

(* for 'open List_.Operators' *)
module Operators = struct
  let ( @ ) = ( @ )
end

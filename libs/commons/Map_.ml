(*pad: same than for Setb,  module Make(Ord: OrderedType) = struct *)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* map.ml 1.15 2004/04/23 10:01:33 xleroy Exp  *)

(*
    type key = Ord.t

    type 'a t =
        Empty
      | Node of 'a t * key * 'a * 'a t * int
*)
type ('key, 'v) t =
  | Empty
  | Node of ('key, 'v) t * 'key * 'v * ('key, 'v) t * int
[@@deriving show]

let empty = Empty

let is_empty = function
  | Empty -> true
  | _ -> false

let height = function
  | Empty -> 0
  | Node (_, _, _, _, h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node (l, x, d, r, if hl >= hr then hl + 1 else hr + 1)

let bal l x d r =
  let hl =
    match l with
    | Empty -> 0
    | Node (_, _, _, _, h) -> h
  in
  let hr =
    match r with
    | Empty -> 0
    | Node (_, _, _, _, h) -> h
  in
  if hl > hr + 2 then
    match l with
    | Empty -> invalid_arg "Map.bal"
    | Node (ll, lv, ld, lr, _) -> (
        if height ll >= height lr then create ll lv ld (create lr x d r)
        else
          match lr with
          | Empty -> invalid_arg "Map.bal"
          | Node (lrl, lrv, lrd, lrr, _) ->
              create (create ll lv ld lrl) lrv lrd (create lrr x d r))
  else if hr > hl + 2 then
    match r with
    | Empty -> invalid_arg "Map.bal"
    | Node (rl, rv, rd, rr, _) -> (
        if height rr >= height rl then create (create l x d rl) rv rd rr
        else
          match rl with
          | Empty -> invalid_arg "Map.bal"
          | Node (rll, rlv, rld, rlr, _) ->
              create (create l x d rll) rlv rld (create rlr rv rd rr))
  else Node (l, x, d, r, if hl >= hr then hl + 1 else hr + 1)

let rec add x data = function
  | Empty -> Node (Empty, x, data, Empty, 1)
  | Node (l, v, d, r, h) ->
      let c = compare x v in
      if c = 0 then Node (l, x, data, r, h)
      else if c < 0 then bal (add x data l) v d r
      else bal l v d (add x data r)

let rec find x = function
  | Empty -> raise Not_found
  | Node (l, v, d, r, _) ->
      let c = compare x v in
      if c = 0 then d else find x (if c < 0 then l else r)

let rec mem x = function
  | Empty -> false
  | Node (l, v, _d, r, _) ->
      let c = compare x v in
      c = 0 || mem x (if c < 0 then l else r)

let rec min_binding = function
  | Empty -> raise Not_found
  | Node (Empty, x, d, _r, _) -> (x, d)
  | Node (l, _x, _d, _r, _) -> min_binding l

let rec remove_min_binding = function
  | Empty -> invalid_arg "Map.remove_min_elt"
  | Node (Empty, _x, _d, r, _) -> r
  | Node (l, x, d, r, _) -> bal (remove_min_binding l) x d r

let merge t1 t2 =
  match (t1, t2) with
  | Empty, t -> t
  | t, Empty -> t
  | _, _ ->
      let x, d = min_binding t2 in
      bal t1 x d (remove_min_binding t2)

let rec remove x = function
  | Empty -> Empty
  | Node (l, v, d, r, _h) ->
      let c = compare x v in
      if c = 0 then merge l r
      else if c < 0 then bal (remove x l) v d r
      else bal l v d (remove x r)

let rec iter f = function
  | Empty -> ()
  | Node (l, v, d, r, _) ->
      iter f l;
      f v d;
      iter f r

let rec map f = function
  | Empty -> Empty
  | Node (l, v, d, r, h) -> Node (map f l, v, f d, map f r, h)

let rec mapi f = function
  | Empty -> Empty
  | Node (l, v, d, r, h) -> Node (mapi f l, v, f v d, mapi f r, h)

let rec fold f m accu =
  match m with
  | Empty -> accu
  | Node (l, v, d, r, _) -> fold f l (f v d (fold f r accu))

(* addons pad *)
let of_list xs = List.fold_left (fun acc (k, v) -> add k v acc) empty xs
let to_list t = fold (fun k v acc -> (k, v) :: acc) t []

let rec update x f = function
  | Empty -> (
      match f None with
      | None -> Empty
      | Some data -> Node (Empty, x, data, Empty, 1))
  | Node (l, v, d, r, h) as m ->
      let c = compare x v in
      if c = 0 then
        match f (Some d) with
        | None -> merge l r
        | Some data ->
            if d == data (* nosem *) then m else Node (l, x, data, r, h)
      else if c < 0 then
        let ll = update x f l in
        if l == ll (* nosem *) then m else bal ll v d r
      else
        let rr = update x f r in
        if r == rr (* nosem *) then m else bal l v d rr
(* NOTE(dinosaure): this implementation was taken from the standard library
   (since 4.06) and it uses physical equality to be able to manipulate huge
   tree (and avoid a /traversal comparison/ into subtree). *)

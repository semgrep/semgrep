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
(* Types *)
(*****************************************************************************)

type ('a, 'b) t = ('a, 'b) Either.t = Left of 'a | Right of 'b
[@@deriving eq, show, sexp]

type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c
[@@deriving eq, show]

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* If you don't want to use [@@deriving eq, show] above, you
 * can copy-paste manually the generated code by getting the
 * result of ocamlfind ocamlc -dsource ... on this code
 *  type ('a, 'b) either =
 *  | Left of 'a
 *  | Right of 'b
 *  [@@deriving show]
 *
 * which should look like this:
 * let pp_either = fun poly_a -> fun poly_b -> fun fmt -> function
 *   | Left a0 ->
 *       (Format.fprintf fmt "(@[<2>Left@ ";
 *        (poly_a fmt) a0;
 *        Format.fprintf fmt "@])")
 *   | Right a0 ->
 *       (Format.fprintf fmt "(@[<2>Right@ ";
 *        (poly_b fmt) a0;
 *        Format.fprintf fmt "@])")
 *
 * let pp_either3 = fun poly_a -> fun poly_b -> fun poly_c -> fun fmt -> function
 *   | Left3 a0 ->
 *       (Format.fprintf fmt "(@[<2>Left3@ ";
 *        (poly_a fmt) a0;
 *        Format.fprintf fmt "@])")
 *   | Middle3 a0 ->
 *       (Format.fprintf fmt "(@[<2>Middle3@ ";
 *        (poly_b fmt) a0;
 *        Format.fprintf fmt "@])")
 *   | Right3 a0 ->
 *       (Format.fprintf fmt "(@[<2>Right3@ ";
 *        (poly_c fmt) a0;
 *        Format.fprintf fmt "@])")
 *)

let partition f l =
  let rec part_either left right = function
    | [] -> (List.rev left, List.rev right)
    | x :: l -> (
        match f x with
        | Left e -> part_either (e :: left) right l
        | Right e -> part_either left (e :: right) l)
  in
  part_either [] [] l

let partition_either3 f l =
  let rec part_either left middle right = function
    | [] -> (List.rev left, List.rev middle, List.rev right)
    | x :: l -> (
        match f x with
        | Left3 e -> part_either (e :: left) middle right l
        | Middle3 e -> part_either left (e :: middle) right l
        | Right3 e -> part_either left middle (e :: right) l)
  in
  part_either [] [] [] l

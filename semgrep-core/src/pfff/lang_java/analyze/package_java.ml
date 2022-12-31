(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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
module G = Graph_code
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let (lookup_fully_qualified2 :
      Graph_code.t -> string list -> Graph_code.node option) =
 fun g xs ->
  let rec aux current xs =
    match xs with
    | [] -> Some current
    | x :: xs -> (
        let children = G.children current g in
        (* because have File intermediate (noisy) nodes *)
        let children =
          children
          |> List.map (fun child ->
                 match child with
                 | _, E.File -> G.children child g
                 (* we prefer Package to Dir when we lookup, we don't want
                  * The "multiple entities" warning when have both
                  * a "net" package and "net" directory.
                  *)
                 | _, E.Dir -> []
                 | _ -> [ child ])
          |> List.flatten
        in
        (* sanity check, quite expansive according to -profile *)
        Common.group_assoc_bykey_eff children
        |> List.iter (fun (k, xs) ->
               if
                 List.length xs > 1
                 (* issue warnings lazily, only when the ambiguity concerns
                  * something we are actually looking for
                  *)
                 && k =$= x
               then (
                 (* todo: this will be a problem when go from class-level
                  * to method/field level dependencies
                  *)
                 pr2 "WARNING: multiple entities with same name";
                 pr2_gen (k, xs)));

        let str =
          match current with
          | ".", E.Dir -> x
          | s, _ -> s ^ "." ^ x
        in
        let new_current =
          children
          |> Common.find_some_opt (fun (s2, kind) ->
                 if str =$= s2 then Some (s2, kind) else None)
        in
        match new_current with
        (* less: could return at least what we were able to resolve *)
        | None -> None
        | Some current -> aux current xs)
  in
  aux G.root xs

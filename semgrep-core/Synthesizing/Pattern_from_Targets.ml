(* Emma Jin
 *
 * Copyright (C) 2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
*)
open AST_generic
open Common
module Set = Set_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The main target intersection algorithm.
 * Helper functions are very similar to Pattern_from_Code --- refactor?
 *
 * related work:
 *  - coccinelle spinfer?
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type stage = DONE | ANY of any

(*****************************************************************************)
(* Print *)
(*****************************************************************************)

let show_stage = function
  | DONE -> "done"
  | ANY any -> AST_generic.show_any any

let rec show_patterns patterns =
  match patterns with
  | [] -> pr2 ""
  | (pattern, target, _)::pats ->
      pr2 ("( " ^ (show_any pattern) ^ ", " ^ (show_stage target) ^ " )");
      show_patterns pats

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fk = Parse_info.fake_info "fake"
let fk_stmt = ExprStmt (Ellipsis fk, fk) |> s
let _body_ellipsis t1 t2 = Block(t1, [fk_stmt], t2) |> s
let _bk f (lp,x,rp) = (lp, f x, rp)

let default_id str =
  N (Id((str, fk),
        {id_resolved = ref None; id_type = ref None; id_constness = ref None}))

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

let pattern_from_any s =
  let metavar_pattern _e = E (default_id "X") in
  match s with
  | ANY (E e) -> [metavar_pattern e, DONE, fun x -> x]
  | _ -> []

let get_one_step_replacements (_, target, f) =
  (* Case on any and use it to get the list of possible replacements (pattern, removed_target, g) list *)
  let target_replacements = pattern_from_any target in
  (* Return the (f pattern, removed_target, f circ g) list *)
  List.map (fun (pattern, removed_target, g) -> (f pattern, removed_target, fun x -> f (g x))) target_replacements

let get_intersecting_patterns pattern_lists =
  let intersect_all sets =
    match sets with
    | [] -> Set.empty
    | [x] -> x
    | x::xs -> List.fold_left (fun acc s -> Set.inter acc s) x xs
  in
  let sets = List.map (fun patterns ->
    List.fold_left (fun s (pattern, _, _) -> Set.add pattern s) Set.empty patterns)
    pattern_lists
  in
  let intersection = intersect_all sets in
  List.map (fun patterns -> List.filter (fun (pattern, _, _) -> Set.mem pattern intersection) patterns) pattern_lists,
  Set.is_empty intersection

let rec generate_patterns_help target_patterns =
  (* For each pattern in each set of target_patterns, generate the list of one step replacements *)
  (*    ex: ($X, bar(foo(2), x), f) ------> [bar(...), [foo(2), x], fun xs -> bar(xs)] *)
  (*        (pattern, any, any -> any) list *)
  (* Flatten the list. Each node n will have a corresponding set of patterns Sn *)
  let new_target_patterns =
    List.map (fun patterns -> List.flatten (List.map get_one_step_replacements patterns)) target_patterns
  in
  (* Keep only the patterns in each Sn that appear in every other *)
  let intersecting_patterns, cont = get_intersecting_patterns new_target_patterns in
  (* Call recursively on these patterns *)
  if cont then generate_patterns_help intersecting_patterns else intersecting_patterns


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let generate_patterns s =
  (* Start each target node any as [$X, any, fun x -> x ] *)
  let patterns = List.map (fun any -> [E (Ellipsis fk), ANY any, fun x -> x]) s in
  let patterns = List.flatten (generate_patterns_help patterns) in
  List.map (fun (pattern, _, _) -> pattern) patterns

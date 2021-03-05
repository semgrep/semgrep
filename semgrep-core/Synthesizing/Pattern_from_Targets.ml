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

exception InvalidSubstitution

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
type pattern_instrs = (any * any * ((stage * (any -> any -> any)) list)) list

let global_lang = ref Lang.OCaml

(*****************************************************************************)
(* Print *)
(*****************************************************************************)

let p_any any =
  match any with
  | E _ -> Pretty_print_generic.pattern_to_string !global_lang any
  | _ -> "can't print"

let stage_string = function
  | DONE -> "done"
  | ANY any -> p_any any

let rec show_replacements reps =
  let list_string =
    match reps with
    | [] -> "]"
    | [target, _] -> stage_string target ^ "]"
    | (target, _)::reps' -> stage_string target ^ " , " ^ show_replacements reps'
  in
  "[" ^ list_string

let rec show_patterns (patterns : pattern_instrs) =
  match patterns with
  | [] -> pr2 "---"
  | (any, pattern, replacements)::pats ->
      pr2 ("( " ^ (p_any any) ^ ", " ^ (p_any pattern) ^ ", " ^ show_replacements replacements ^ " )");
      show_patterns pats

let show_pattern_sets patsets =
  pr2 "[";
  List.iter show_patterns patsets;
  pr2 "]\n"

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

let replace_sk { s = _s; s_id; s_use_cache; s_backrefs; s_bf } s_kind =
  { s = s_kind; s_id; s_use_cache; s_backrefs; s_bf }

let add_pattern s pattern =
  Set.add (p_any pattern) s

let add_patterns s patterns =
  List.fold_left (fun s' (_, pattern, _) -> add_pattern s' pattern) s patterns

let lookup_pattern pattern s =
  Set.mem (p_any pattern) s


(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

let metavar_pattern _e = (default_id "$X")

let _sub_expr f sub =
  match sub with
  | E e -> f e
  | _ -> raise InvalidSubstitution

let pattern_from_call e (e', (lp, args, rp)) : pattern_instrs =
  let replace_name e x =
    match e, x with
    | (E (Call (_, (lp, args, rp)))), E x -> E (Call (x, (lp, args, rp)))
    | _ -> raise InvalidSubstitution
  in
  let replace_args e x =
    match e, x with
    | (E (Call (e, (lp, _, rp)))), Args x -> E (Call (e, (lp, x, rp)))
    | _ -> raise InvalidSubstitution
  in
  [ e, E (Call (metavar_pattern e', (lp, [Arg (Ellipsis fk)], rp))),
    [ ANY (E e'), replace_name;
      ANY (Args args), replace_args
    ]
  ]

let pattern_from_expr e : pattern_instrs =
  match e with
  | Call (e', (lp, args, rp)) -> pattern_from_call (E e) (e', (lp, args, rp))
  | _ -> [E e, E (metavar_pattern e), [DONE, fun e _x -> e]]

let rec pattern_from_stmt ({s; _} as stmt) : pattern_instrs =
  match s with
  | ExprStmt (e, sc) ->
      let fill_exprstmt s x =
        match s, x with
        | S stmt, E e -> S (replace_sk stmt (ExprStmt (e, sc)))
        | _ -> raise InvalidSubstitution
      in
      let _, pattern =
        get_one_step_replacements (S stmt, E (Ellipsis fk),
                                   [ANY (E e), fill_exprstmt])
      in pattern
  | _ -> []

and pattern_from_any s : pattern_instrs =
  match s with
  | ANY (E e) -> pattern_from_expr e
  | ANY (S stmt) -> pattern_from_stmt stmt
  | _ -> []

(* pattern construction *)

and get_one_step_replacements (any, pattern, holes) =
  (* Try the first hole (target, f) *)
  match holes with
  | [] -> (any, pattern, []), []
  | (target, f)::holes' ->
      (* Get all the possible replacements for the target (pattern, holes) list *)
      let target_replacements = pattern_from_any target in
      (* Use each replacement to fill the chosen hole *)
      let incorporate_holes holes =
        List.map (fun (removed_target, g) -> (removed_target, fun any' x -> f any (g any' x))) holes
      in
      (any, pattern, holes'),
      List.map (fun (any', pattern, target_holes) -> (any', f any pattern, (incorporate_holes target_holes) @ holes'))
        target_replacements

let get_included_patterns pattern_children =
  let intersect_all sets =
    match sets with
    | [] -> Set.empty
    | [x] -> x
    | x::xs -> List.fold_left (fun acc s -> Set.inter acc s) x xs
  in
  let sets = List.map (fun patterns ->
    List.fold_left (fun s (_, child_patterns) -> add_patterns s child_patterns) Set.empty patterns)
    pattern_children
  in
  let intersection = intersect_all sets in
  (* pr2 "sets";
     List.iter (Set.iter (fun pattern -> pr2 pattern)) sets;
     pr2 "intersection";
     Set.iter (fun pattern -> pr2 pattern) intersection; *)
  let include_pattern ((any, pattern, holes), children) =
    let included_children = List.filter (fun (_, pattern, _) -> lookup_pattern pattern intersection) children in
    match included_children with
    | [] ->  (
        match holes with
        | [] -> []
        | _ -> [any, pattern, holes]
      )
    | _ -> children
  in
  List.map (fun patterns -> List.flatten (List.map include_pattern patterns)) pattern_children,
  not (Set.is_empty intersection)

let rec generate_patterns_help (target_patterns : pattern_instrs list) =
  (* For each pattern in each set of target_patterns, generate the list of one step replacements *)
  (*    ex: ($X, bar(foo(2), x), f) ------> [$X(...), [bar, fun x -> x(...); [foo(2), x], fun xs -> bar(xs)]] *)
  (*        (pattern, any, any -> any) list *)
  (* Flatten the list. Each node n will have a corresponding set of patterns Sn *)
  pr2 "target patterns";
  show_pattern_sets target_patterns;
  let pattern_children =
    List.map (fun patterns -> List.map (fun pattern -> get_one_step_replacements pattern) patterns)
      target_patterns
  in
  let new_target_patterns =
    List.map (fun patterns -> List.flatten (List.map (fun (_, new_pattern) -> new_pattern) patterns))
      pattern_children
  in
  pr2 "new target patterns";
  show_pattern_sets new_target_patterns;
  (* Keep only the patterns in each Sn that appear in every other OR *)
  (* the patterns that were included last time, don't have children, and have another replacement to try *)
  let included_patterns, cont = get_included_patterns pattern_children in
  (* Call recursively on these patterns *)
  if cont then generate_patterns_help included_patterns else target_patterns


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let generate_patterns s lang =
  global_lang := lang;
  (* Start each target node any as [$X, any, fun x -> x ] *)
  let patterns = List.map (fun any -> [any, E (Ellipsis fk), [ANY any, fun _a x -> x]]) s in
  let patterns =
    match generate_patterns_help patterns with
    | [] -> []
    | x::_ -> x
  in
  List.map (fun (_, pattern, _) -> pattern) patterns

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
exception UnsupportedTargetType

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
type env = { prev : any; count : int; mapping : (expr * expr) list; }
type pattern_instrs = (env * any * ((stage * ((any -> any) -> any -> any)) list)) list

let global_lang = ref Lang.OCaml

(*****************************************************************************)
(* Print *)
(*****************************************************************************)

let p_any = Pretty_print_generic.pattern_to_string !global_lang

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
  | (_any, pattern, replacements)::pats ->
      pr2 ("( " (* ^ (p_any any) ^ ", " *) ^ (p_any pattern) ^ ", " ^ show_replacements replacements ^ " )");
      show_patterns pats

let show_pattern_sets patsets =
  pr2 "[";
  List.iter show_patterns patsets;
  pr2 "]\n"

(*****************************************************************************)
(* Pattern_from_Code Helpers *)
(*****************************************************************************)

(* TODO make mapping a map and use map lookup *)
let lookup env e =
  let mapping = env.mapping in
  let rec look = function
    | [] -> None
    | (e1, e2)::xs ->
        if Matching_generic.equal_ast_binded_code (E e) (E e1) then Some e2 else look xs
  in
  look mapping

let fk = Parse_info.fake_info "fake"
let fk_stmt = ExprStmt (Ellipsis fk, fk) |> s
let _body_ellipsis t1 t2 = Block(t1, [fk_stmt], t2) |> s
let _bk f (lp,x,rp) = (lp, f x, rp)

let default_id str =
  N (Id((str, fk),
        {id_resolved = ref None; id_type = ref None; id_constness = ref None}))

let count_to_id count =
  let make_id ch = Format.sprintf "$%c" ch in
  match count with
  | 1 -> make_id 'X'
  | 2 -> make_id 'Y'
  | 3 -> make_id 'Z'
  | _ when count <= 26 -> make_id (Char.chr (count - 4 + Char.code 'A'))
  | _ -> Format.sprintf "$X%d" (count - 26)

(* If the id is already in env, return that *)
(* Otherwise, this depends on the with_type flag *)
(* If with_type is true, if there is a type, try to generate a TypedMetavar *)
(* In all other cases, generate an id *)
(* Add to env's mapping and return it *)
let get_id env e =
  let id = lookup env e in
  match id with
    Some x -> (env, x)
  | None ->
      let notype_id = default_id (count_to_id env.count) in
      let new_id = notype_id
      in
      ({ count = env.count + 1; mapping = (e, new_id)::(env.mapping); prev = env.prev }, new_id)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let replace_sk { s = _s; s_id; s_use_cache; s_backrefs; s_bf } s_kind =
  { s = s_kind; s_id; s_use_cache; s_backrefs; s_bf }

let add_pattern s pattern =
  Set.add (p_any pattern) s

let add_patterns s patterns =
  List.fold_left (fun s' (_, pattern, _) -> add_pattern s' pattern) s patterns

let lookup_pattern pattern s =
  Set.mem (p_any pattern) s

let set_prev { prev = _; count; mapping } prev' = { prev = prev'; count; mapping }

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

let metavar_pattern env e = get_id env e

let pattern_from_args env args : pattern_instrs =
  let replace_first_arg f args =
    match args with
    | Args ((Arg arg)::xs) -> (
        match f (E arg) with
        | E x -> Args ((Arg x)::xs)
        | x -> pr2 (show_any x); raise InvalidSubstitution
      )
    | Args (_::_) -> args
    | _ -> pr2 "h6"; raise InvalidSubstitution
  in
  let replace_rest f args =
    match args with
    | Args ((Arg e)::rest) -> (
        match f (Args rest) with
        | Args args' -> Args ((Arg e)::args')
        | _ -> pr2 "h7"; raise InvalidSubstitution
      )
    | _ -> pr2 "h8"; raise InvalidSubstitution
  in
  match args with
  | [] -> []
  | Arg arg::rest ->
      [ let env', id = metavar_pattern env arg in
        env', Args [Arg id; Arg (Ellipsis fk)],
        [ANY (E arg), replace_first_arg;
         ANY (Args rest), replace_rest]
      ]
  | _ -> []

let pattern_from_call env (e', (lp, args, rp)) : pattern_instrs =
  let replace_name f e =
    match e with
    | E (Call (e, (lp, args, rp))) -> (
        match f (E e) with
        | E x -> E (Call (x, (lp, args, rp)))
        | _ -> raise InvalidSubstitution
      )
    | x -> pr2 ("h3 " ^ show_any x); raise InvalidSubstitution
  in
  let replace_args f e =
    match e with
    | E (Call (e, (lp, args, rp))) -> (
        match f (Args args) with
        | Args x -> E (Call (e, (lp, x, rp)))
        | _ -> raise InvalidSubstitution
      )
    | _ -> pr2 "h2"; raise InvalidSubstitution
  in
  [ let env', id = metavar_pattern env e' in
    env', E (Call (id, (lp, [Arg (Ellipsis fk)], rp))),
    [ ANY (E e'), replace_name;
      ANY (Args args), replace_args
    ]
  ]

let pattern_from_expr env e : pattern_instrs =
  match e with
  | Call (e', (lp, args, rp)) -> pattern_from_call env (e', (lp, args, rp))
  | N _ | DotAccess _
  | L _ -> [env, E e, [DONE, fun f e -> f e]]
  | expr -> [ let env', id = metavar_pattern env expr in env', E id, [DONE, fun f e -> f e]]

let rec pattern_from_stmt env ({s; _} as stmt) : pattern_instrs =
  match s with
  | ExprStmt (e, sc) ->
      let fill_exprstmt f exprstmt =
        match exprstmt with
        | S ({s = ExprStmt (e', _); _} as stmt) -> (
            match (f (E e')) with
            | E x -> S (replace_sk stmt (ExprStmt (x, sc)))
            | _ -> raise InvalidSubstitution
          )
        (* | S stmt, S { s = ExprStmt (e, _); _ } -> S (replace_sk stmt (ExprStmt (e, sc))) *)
        | _ -> pr2 ("h1"); raise InvalidSubstitution
      in
      let _, pattern =
        get_one_step_replacements (env, (fill_exprstmt (fun _ -> E (Ellipsis fk)) (S stmt)),
                                   [ANY (E e), fill_exprstmt])
      in pattern
  | _ -> []

and pattern_from_any env s : pattern_instrs =
  match s with
  | ANY (E e) -> pattern_from_expr env e
  | ANY (S stmt) -> pattern_from_stmt env stmt
  | ANY (Args args) -> pattern_from_args env args
  | _ -> []

(* pattern construction *)

and get_one_step_replacements (env, pattern, holes) =
  (* Try the first hole (target, f) *)
  match holes with
  | [] -> (env, pattern, []), []
  | (target, f)::holes' ->
      (* Get all the possible replacements for the target (pattern, holes) list *)
      let target_replacements = pattern_from_any env target in
      (* Use each replacement to fill the chosen hole *
       * For example, if f turns foo(...), a -> foo(a), and g turns (...), a -> (a, ...), we want a function *
       * that turns foo(...), a -> foo(a, ...) *)
      let incorporate_holes pattern holes =
        pr2 ("when incorporating " ^ p_any pattern);
        List.map (fun (removed_target, g) -> (removed_target, fun (h : any -> any) (any : any) : any -> f (g h) any)) holes
      in
      (env, pattern, holes'),
      List.map
        (fun (env, pattern', target_holes) ->
           (set_prev env pattern', f (fun _ -> pattern') pattern, (incorporate_holes pattern' target_holes) @ holes'))
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
  let include_pattern ((env, pattern, holes), children) =
    let included_children = List.filter (fun (_, pattern, _) -> lookup_pattern pattern intersection) children in
    match included_children with
    | [] ->  (
        match holes with
        | [] -> []
        | _ -> [set_prev env pattern, pattern, holes]
      )
    | _ -> children
  in
  List.map (fun patterns -> List.flatten (List.map include_pattern patterns)) pattern_children

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
  let included_patterns = get_included_patterns pattern_children in
  let cont = List.fold_left (fun prev patterns -> prev || (not (List.length patterns = 0))) false included_patterns in
  (* Call recursively on these patterns *)
  if cont then generate_patterns_help included_patterns else target_patterns


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let generate_patterns s lang =
  global_lang := lang;
  (* Start each target node any as [$X, [ any, fun x -> x ]] *)
  let starting_pattern any =
    match any with
    | E _ -> [{ prev = E (Ellipsis fk); count = 1; mapping = [] }, E (Ellipsis fk), [ANY any, fun f a -> f a]]
    | S _ -> [{ prev = S (exprstmt (Ellipsis fk)); count = 1; mapping = [] }, S (exprstmt (Ellipsis fk)), [ANY any, fun f a -> f a]]
    | _ -> raise UnsupportedTargetType
  in
  let patterns = List.map starting_pattern s in
  let patterns =
    match generate_patterns_help patterns with
    | [] -> []
    | x::_ -> x
  in
  List.map (fun (_, pattern, _) -> pattern) patterns

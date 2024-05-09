(* Emma Jin
 *
 * Copyright (C) 2020 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
module Set = Set_
open AST_generic
module G = AST_generic

exception InvalidSubstitution
exception UnsupportedTargetType

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module is used to generate a pattern from multiple pieces of code.
 * Note that this is different from Pattern_from_Code, which synthesizes
 * multiple pattern suggestions given one code snippet.
 *
 * See the mli for a detailed description of the algorithm
 *
 * Helper functions are very similar to Pattern_from_Code --- refactor?
 *
 * related work:
 *  - coccinelle spinfer?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* An intermediate type that allows us to give our own progression of pattern *
 * specificity. For example, "abc" goes from $X to "..." to "abc". This can   *
 * also be achieved by adding checks, but having a type makes it easier to    *
 * separate the cases
 *)
type stage = DONE | ANY of any | LN (* literal name *) of any

type env = {
  config : Rule_options.t;
  prev : any;
  count : int;
  mapping : (expr * expr) list;
}

(* Each target comes with a list of patterns : pattern_instrs *)
(* A pattern needs to keep track of its environment (env), the pattern (any), *
 * and a list of instructions for how to make the pattern more specific.      *
 * The replacement information gives the piece that was removed from the hole *
 * as well as instructions for where to put it back                           *
 *)
type replacement_info = stage * ((any -> any) -> any -> any)
type pattern_instr = env * any * replacement_info list
type pattern_instrs = pattern_instr list

let global_lang = ref Lang.Ocaml

(*****************************************************************************)
(* Print *)
(*****************************************************************************)

let p_any any = Pretty_print_pattern.pattern_to_string !global_lang any

let stage_string = function
  | DONE -> "done"
  | ANY any -> p_any any
  | LN any -> p_any any

let rec show_replacements reps =
  let list_string =
    match reps with
    | [] -> "]"
    | [ (target, _) ] -> stage_string target ^ "]"
    | (target, _) :: reps' ->
        stage_string target ^ " , " ^ show_replacements reps'
  in
  "[" ^ list_string

let rec show_patterns print (patterns : pattern_instrs) =
  match patterns with
  | [] -> print "---"
  | (_any, pattern, replacements) :: pats ->
      print
        ("( " (* ^ (p_any any) ^ ", " *) ^ p_any pattern
        ^ ", "
        ^ show_replacements replacements
        ^ " )");
      show_patterns print pats

let show_pattern_sets print patsets =
  print "[";
  List.iter (show_patterns print) patsets;
  print "]\n"

(*****************************************************************************)
(* Pattern_from_Code Helpers *)
(*****************************************************************************)

(* TODO make mapping a map and use map lookup *)
let lookup env e =
  let mapping = env.mapping in
  let rec look = function
    | [] -> None
    | (e1, e2) :: xs ->
        if Matching_generic.equal_ast_bound_code env.config (E e) (E e1) then
          Some e2
        else look xs
  in
  look mapping

let fk = Tok.unsafe_fake_tok "fake"
let fk_stmt = ExprStmt (Ellipsis fk |> G.e, fk) |> G.s
let _body_ellipsis t1 t2 = Block (t1, [ fk_stmt ], t2) |> G.s
let _bk f (lp, x, rp) = (lp, f x, rp)

let default_id str =
  N (Id ((str, fk), empty_id_info ~id:IdInfoId.unsafe_default ())) |> G.e

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
  | Some x -> (env, x)
  | None ->
      let notype_id = default_id (count_to_id env.count) in
      let new_id = notype_id in
      let env' =
        { env with count = env.count + 1; mapping = (e, new_id) :: env.mapping }
      in
      (env', new_id)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let replace_sk stmt s_kind = { stmt with s = s_kind }
let add_pattern s pattern = Set.add (p_any pattern) s

let add_patterns s patterns =
  List.fold_left (fun s' (_, pattern, _) -> add_pattern s' pattern) s patterns

let lookup_pattern pattern s = Set.mem (p_any pattern) s
let set_prev env prev' = { env with prev = prev' }

(* Tranposes a list of lists, must be rectangular. *)
let rec transpose (list : 'a list list) : 'a list list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x
      :: List_.map (List_.hd_exn "cannot transpose non-rectangular matrix") xss
      )
      :: transpose
           (xs
           :: List_.map
                (List_.tl_exn "cannot transpose non-rectangular matrix")
                xss)

(* We can't handle lists of statements of unequal size yet.
 * Check that each target has the same number of statements.
 *)
let check_equal_length (targets : 'a list list) : bool =
  match targets with
  | [] -> true
  | _ ->
      let lengths = List_.map List.length targets in
      let hdlen = List_.hd_exn "unexpected empty list" lengths in
      List.for_all (phys_equal hdlen) lengths

(*****************************************************************************)
(* Pattern generation *)
(*****************************************************************************)

let metavar_pattern env e = get_id env e

let pattern_from_args env args : pattern_instrs =
  let replace_first_arg f args =
    match args with
    | Args (Arg { e = Ellipsis el; _ } :: Arg arg :: xs) -> (
        match f (E arg) with
        | E x -> Args (Arg (Ellipsis el |> G.e) :: Arg x :: xs)
        | _x -> raise InvalidSubstitution)
    | Args (_ :: _) -> args
    | _ -> raise InvalidSubstitution
  in
  let remove_ellipsis _f args =
    match args with
    | Args (Arg { e = Ellipsis _; _ } :: x :: xs) -> Args (x :: xs)
    | Args (_ :: _) -> args
    | _ -> raise InvalidSubstitution
  in
  let replace_rest f args =
    match args with
    | Args (Arg { e = Ellipsis el; _ } :: Arg e :: rest) -> (
        match f (Args rest) with
        | Args args' -> Args (Arg (Ellipsis el |> G.e) :: Arg e :: args')
        | _ -> raise InvalidSubstitution)
    | Args (Arg e :: rest) -> (
        match f (Args rest) with
        | Args args' -> Args (Arg e :: args')
        | _ -> raise InvalidSubstitution)
    | _ -> raise InvalidSubstitution
  in
  let remove_end_ellipsis _f args =
    let rec remove_end = function
      | [] -> []
      | [ Arg { e = Ellipsis _el; _ } ] -> []
      | x :: xs -> x :: remove_end xs
    in
    match args with
    | Args args' -> Args (remove_end args')
    | _ -> raise InvalidSubstitution
  in
  let max = List.length args in
  let rec make_arg_subs args count =
    let substitute_next rest =
      (* If it's the last one, try deleting the ellipses at the end *)
      let funs =
        if count =|= max then
          [
            (ANY (Args rest), replace_rest);
            (ANY (E (Ellipsis fk |> G.e)), remove_end_ellipsis);
          ]
        else []
      in
      (* Always try replacing the arguments *)
      let funs = (ANY (Args rest), replace_rest) :: funs in
      (* If it's the first one, try deleting the ellipses at the start *)
      if count =|= 1 then
        (ANY (E (Ellipsis fk |> G.e)), remove_ellipsis) :: funs
      else funs
    in
    match args with
    | [] -> []
    | Arg arg :: rest ->
        (let env', id = metavar_pattern env arg in
         ( env',
           Args [ Arg (Ellipsis fk |> G.e); Arg id; Arg (Ellipsis fk |> G.e) ],
           (ANY (E arg), replace_first_arg) :: substitute_next rest ))
        :: make_arg_subs rest (count + 1)
    | _ -> []
  in
  make_arg_subs args 1

let pattern_from_call env (e', (lp, args, rp)) : pattern_instrs =
  let replace_name f e =
    match e with
    | E { e = Call (e, (lp, args, rp)); _ } -> (
        match f (E e) with
        | E x -> E (Call (x, (lp, args, rp)) |> G.e)
        | _ -> raise InvalidSubstitution)
    | _ -> raise InvalidSubstitution
  in
  let replace_args f e =
    match e with
    | E { e = Call (e, (lp, args, rp)); _ } -> (
        match f (Args args) with
        | Args x -> E (Call (e, (lp, x, rp)) |> G.e)
        | _ -> raise InvalidSubstitution)
    | _ -> raise InvalidSubstitution
  in
  [
    (let env', id = metavar_pattern env e' in
     ( env',
       E (Call (id, (lp, [ Arg (Ellipsis fk |> G.e) ], rp)) |> G.e),
       [ (ANY (E e'), replace_name); (ANY (Args args), replace_args) ] ));
  ]

let pattern_from_literal env lit : pattern_instrs =
  match lit with
  | String (l, (_, tok), r) ->
      [
        ( env,
          E (L (String (l, ("...", tok), r)) |> G.e),
          [ (LN (E (L lit |> G.e)), fun f any -> f any) ] );
      ]
  | _ -> [ (env, E (L lit |> G.e), [ (DONE, fun f e -> f e) ]) ]

type side = Left | Right

let pattern_from_assign env (e1, tok, e2) : pattern_instrs =
  let replace_assign_ops side f e =
    match (e, side) with
    | E { e = Assign (e1, tok, e2); _ }, Left -> (
        match f (E e1) with
        | E x -> E (Assign (x, tok, e2) |> G.e)
        | _ -> raise InvalidSubstitution)
    | E { e = Assign (e1, tok, e2); _ }, Right -> (
        match f (E e2) with
        | E x -> E (Assign (e1, tok, x) |> G.e)
        | _ -> raise InvalidSubstitution)
    | _ -> raise InvalidSubstitution
  in
  [
    (let env, id1 = metavar_pattern env e1 in
     let env, id2 = metavar_pattern env e2 in
     ( env,
       E (Assign (id1, tok, id2) |> G.e),
       [
         (ANY (E e1), replace_assign_ops Left);
         (ANY (E e2), replace_assign_ops Right);
       ] ));
  ]

let pattern_from_expr env e : pattern_instrs =
  match e.e with
  | Call (e', (lp, args, rp)) -> pattern_from_call env (e', (lp, args, rp))
  | L l -> pattern_from_literal env l
  | Assign (e1, tok, e2) -> pattern_from_assign env (e1, tok, e2)
  | N _
  | DotAccess _ ->
      [ (env, E e, [ (DONE, fun f any -> f any) ]) ]
  | _expr ->
      [
        (let env', id = metavar_pattern env e in
         (env', E id, [ (DONE, fun f any -> f any) ]));
      ]

let rec pattern_from_stmt env ({ s; _ } as stmt) : pattern_instrs =
  match s with
  | ExprStmt (e, sc) ->
      let fill_exprstmt f exprstmt =
        match exprstmt with
        | S ({ s = ExprStmt (e', _); _ } as stmt) -> (
            match f (E e') with
            | E x -> S (replace_sk stmt (ExprStmt (x, sc)))
            | _ -> raise InvalidSubstitution)
        | _ ->
            (* old: print "h1"; ??? *)
            raise InvalidSubstitution
      in
      let _, pattern =
        get_one_step_replacements
          ( env,
            fill_exprstmt (fun _ -> E (Ellipsis fk |> G.e)) (S stmt),
            [ (ANY (E e), fill_exprstmt) ] )
      in
      pattern
  | _ -> []

and pattern_from_any env stage : pattern_instrs =
  match stage with
  | ANY (E e) -> pattern_from_expr env e
  | ANY (S stmt) -> pattern_from_stmt env stmt
  | ANY (Args args) -> pattern_from_args env args
  | LN any -> [ (env, any, [ (DONE, fun f any -> f any) ]) ]
  | _ -> []

(*****************************************************************************)
(* Infrastructure *)
(*****************************************************************************)
and get_one_step_replacements (env, pattern, holes) =
  (* Try the first hole (target, f) *)
  match holes with
  | [] -> ((env, pattern, []), [])
  | (target, f) :: holes' ->
      (* Get all the possible replacements for the target (pattern, holes) list *)
      let target_replacements = pattern_from_any env target in
      (* Use each replacement to fill the chosen hole *
       * For example, if f turns foo(...), a -> foo(a), and g turns (...), a -> (a, ...), we want a function *
       * that turns foo(...), a -> foo(a, ...) *)
      let incorporate_holes holes =
        List_.map
          (fun (removed_target, g) ->
            (removed_target, fun h any -> f (g h) any))
          holes
      in
      ( (env, pattern, holes'),
        List_.map
          (fun (env, pattern', target_holes) ->
            ( set_prev env pattern',
              f (fun _ -> pattern') pattern,
              incorporate_holes target_holes @ holes' ))
          target_replacements )

let get_included_patterns pattern_children =
  let intersect_all sets =
    match sets with
    | [] -> Set.empty
    | [ x ] -> x
    | x :: xs -> List.fold_left (fun acc s -> Set.inter acc s) x xs
  in
  let sets =
    List_.map
      (fun patterns ->
        List.fold_left
          (fun s (_, child_patterns) -> add_patterns s child_patterns)
          Set.empty patterns)
      pattern_children
  in
  let intersection = intersect_all sets in
  let include_pattern ((env, pattern, holes), children) =
    let included_children =
      List.filter
        (fun (_, pattern, _) -> lookup_pattern pattern intersection)
        children
    in
    match included_children with
    | [] -> (
        match holes with
        | [] -> []
        | _ -> [ (set_prev env pattern, pattern, holes) ])
    | _ -> children
  in
  List_.map (List.concat_map include_pattern) pattern_children

let rec generate_patterns_help (target_patterns : pattern_instrs list) =
  (* For each pattern in each set of target_patterns, generate the list of one
   * step replacements
   *    ex: ($X, bar(foo(2), x), f) ------> [$X(...), [bar, fun x -> x(...); [foo(2), x], fun xs -> bar(xs)]]
   *        (pattern, any, any -> any) list
   * Flatten the list. Each node n will have a corresponding set of patterns Sn
   *)
  (* nosemgrep: no-logs-in-library *)
  Logs.debug (fun m ->
      let s =
        Buffer_.with_buffer_to_string (fun buf ->
            let print s = Printf.bprintf buf "%s" s in
            show_pattern_sets print target_patterns)
      in
      m "target patterns: %s" s);
  let pattern_children =
    List_.map (List_.map get_one_step_replacements) target_patterns
  in
  (* Keep only the patterns in each Sn that appear in every other OR *)
  (* the patterns that were included last time, don't have children, and have another replacement to try *)
  let included_patterns = get_included_patterns pattern_children in
  let cont =
    List.fold_left
      (fun prev patterns -> prev && not (patterns =*= []))
      true included_patterns
  in
  (* Call recursively on these patterns *)
  if cont then generate_patterns_help included_patterns
  else List_.map (List_.hd_exn "unexpected empty list") target_patterns

let extract_pattern (pats : pattern_instr) : Pattern.t =
  (fun (_, pattern, _) -> pattern) pats

(* Start each target node any as [$X, [ any, fun x -> x ]] *)
let generate_starting_patterns config (targets : AST_generic.any list list) :
    pattern_instrs list list =
  let starting_pattern any =
    match any with
    | E _ ->
        let env =
          { config; prev = E (Ellipsis fk |> G.e); count = 1; mapping = [] }
        in
        [ (env, E (Ellipsis fk |> G.e), [ (ANY any, fun f a -> f a) ]) ]
    | S _ ->
        let env =
          {
            config;
            prev = S (exprstmt (Ellipsis fk |> G.e));
            count = 1;
            mapping = [];
          }
        in
        [
          (env, S (exprstmt (Ellipsis fk |> G.e)), [ (ANY any, fun f a -> f a) ]);
        ]
    | _ -> raise UnsupportedTargetType
  in
  List_.map (List_.map starting_pattern) targets

(* Copies the metavar count and mapping from src pattern_instr to
 *  each env in dsts.
 *)
let cp_meta_env (src : pattern_instr) (dsts : pattern_instrs) : pattern_instrs =
  let senv, _, _ = src in
  let cp dst =
    let denv, dpattern, dholes = dst in
    let denv' = { denv with count = senv.count; mapping = senv.mapping } in
    (denv', dpattern, dholes)
  in
  List_.map cp dsts

(* Calls generate_patterns_help on each list of pattern_instrs, retaining
 * the metavariable environment between calls.
 * The environment is retained within a single target for subsequent statements,
 * not across targets.
 *)
let rec generate_with_env (target_patterns : pattern_instrs list list) :
    pattern_instrs =
  match target_patterns with
  | [] -> []
  | [ cur ] ->
      [ List_.hd_exn "unexpected empty list" (generate_patterns_help cur) ]
  | cur :: next :: rest ->
      let curpats = generate_patterns_help cur in
      let next' = List_.map2 cp_meta_env curpats next in
      List_.hd_exn "unexpected empty list" curpats
      :: generate_with_env (next' :: rest)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let generate_patterns config targets lang =
  global_lang := lang;
  let split_targets = List_.map Range_to_AST.split_any targets in
  if check_equal_length split_targets then
    split_targets
    |> generate_starting_patterns config
    (* Transpose to intersect across targets, not within. *)
    |> transpose
    |> generate_with_env |> List_.map extract_pattern |> Range_to_AST.join_anys
  else failwith "Only targets of equal length are supported."

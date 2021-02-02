(*s: semgrep/engine/Semgrep.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
(*e: pad/r2c copyright *)
open Common

module R = Rule
module MR = Mini_rule
module PM = Pattern_match
module G = AST_generic
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The core engine of Semgrep.
 *
 * This module implements the boolean composition of patterns.
 * See Semgrep_generic.ml for the code to handle a single pattern and
 * the visitor/matching engine.
 *
 * Thus, we can decompose the engine in 3 main components:
 *  - composing matching results using boolean/set logic (this file)
 *  - visiting code (=~ Semgrep_generic.ml)
 *  - matching code (=~ Generic_vs_generic.ml)
 *
 * There are also "preprocessing" work before that:
 *  - parsing (lexing, parsing) rules, code, patterns
 *  - normalizing (convert to a generic AST)
 *  - naming (but bugs probably)
 *  - SEMI typing (propagating type decls at least and small inference)
 *  - SEMI analyzing (dataflow constant propagation)
 *    but could do much more: deep static analysis using Datalog?
 *
 * TODO
 *  - spacegrep, link with spacegrep lib :)
 *  - regexp, use PCRE compatible regexp OCaml lib
 *  - metavar comparison, use Eval_generic :)
 *  - pattern-where-python? use pycaml? works for dlint rule?
 *    right now only 4 rules are using pattern-where-python
 *
 * LATER (if really decide to rewrite the python wrapper in OCaml):
 *  - paths
 *  - autofix
 *  - ...
 *
 * FUTURE WORK:
 * Right now we just analyze one file at a time. Later we could
 * maybe take a list of files and do some global analysis for:
 *     * caller/callee in different files
 *     * inheritance awareness, because right now we can't match
 *       code that inherit indirectly form a class mentioned in a pattern
 * There are different options for such global analysis:
 *  - generate a giant file a la CIL, but scale?
 *  - do it via a 2 passes process. 1st pass iterate over all files, report
 *    already matches, record semantic information (e.g., inheritance tree,
 *    call graph, etc.) as it goes, and let the matching engine report
 *    todo_second_pass if for example is_children returned a Maybe.
 *    Then in 2nd pass just process the files that were marked as todo.
 *  - use LSP, so don't even need 2 pass and can even work when passing
 *    a single file or subdir to semgrep
 *
 * Note that we opted here for simple patterns with simple extensions
 * to the grammar (metavar, ellipsis) with simple (but powerful) logic
 * compositions of patterns.
 * Coccinelle instead opted for very complex patterns and using CTL to
 * hold of that together.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* Id of a single pattern in a formula. This will be used to generate
 * mini rules with this id, and later when we evaluate the formula, find
 * the matching results corresponding to this id.
*)
type pattern_id = R.pattern_id

(* range with metavars *)
type range_with_mvars = {
  r: Range.t;
  mvars: Metavariable.bindings;

  origin: Pattern_match.t;
}

(* !This hash table uses the Hashtbl.find_all property! *)
type id_to_match_results = (pattern_id, Pattern_match.t) Hashtbl.t

type env = {
  pattern_matches: id_to_match_results;
  (* unused for now, but could be passed down for Range.content_at_range *)
  file: Common.filename
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (xpatterns_in_formula: R.formula -> R.xpattern list) = fun e ->
  let res = ref [] in
  e |> R.visit_new_formula (fun xpat -> Common.push xpat res);
  !res

let partition_xpatterns xs =
  xs |> Common.partition_either3 (fun xpat ->
    let id = xpat.R.pid in
    match xpat.R.pat with
    | R.Sem x -> Left3 (id, x)
    | R.Spacegrep x -> Middle3 (id, x)
    | R.Regexp x -> Right3 (id, x)
  )

let (mini_rule_of_pattern: R.t -> (R.pattern_id * Pattern.t) -> MR.t) =
  fun r (id, pattern) ->
  { MR.
    id = string_of_int id; pattern;
    (* parts that are not really needed I think in this context, since
     * we just care about the matching result.
    *)
    message = ""; severity = MR.Error;
    languages =
      (match r.R.languages with
       | R.L (x, xs) -> x::xs
       | R.LNone | R.LGeneric -> raise Impossible
      );
    pattern_string = "";
  }

(* todo: change Pattern_match type so we don't need this gymnastic.
 * A pattern_match should just need the id from the mini_rule
*)
let (mini_rule_of_regexp: (R.pattern_id * string) -> MR.t) =
  fun (id, s) ->
  { MR.
    id = string_of_int id;
    pattern = G.E (G.L (G.Null (PI.fake_info "")));
    message = ""; severity = MR.Error; languages = [];
    pattern_string = s;
  }

(* todo: same, we should not need that *)
let hmemo = Hashtbl.create 101
let info_of_token_location file loc =
  (* first complete the loc with line/col which are needed when printing
   * matches *)
  let conv =
    Common.memoized hmemo file (fun () -> PI.full_charpos_to_pos_large file)
  in
  let (line, column) = conv loc.PI.charpos in
  let loc = { loc with PI. line; column } in
  { PI.token = PI.OriginTok loc; transfo = PI.NoTransfo }

let (group_matches_per_pattern_id: Pattern_match.t list ->id_to_match_results)=
  fun xs ->
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun m ->
    let id = int_of_string (m.Pattern_match.rule.MR.id) in
    Hashtbl.add h id m
  );
  h

let (range_to_match_result: range_with_mvars -> Pattern_match.t) =
  fun range -> range.origin

let (match_result_to_range: Pattern_match.t -> range_with_mvars) =
  fun m ->
  let { Pattern_match.location = (start_loc, end_loc); env = mvars; _} = m in
  let r = Range.range_of_token_locations start_loc end_loc in
  { r; mvars; origin = m; }

(* return list of "positive" x list of Not x list of Conds *)
let (split_and:
       R.formula list -> R.formula list * R.formula list * R.metavar_cond list) =
  fun xs ->
  xs |> Common.partition_either3 (fun e ->
    match e with
    | R.Not f -> Middle3 f
    | R.MetavarCond c -> Right3 c
    | _ -> Left3 e
  )

(*****************************************************************************)
(* Logic on ranges *)
(*****************************************************************************)

open Range

(* TODO: also check metavariables! *)
let intersect_ranges xs ys =
  let surviving_xs =
    xs |> List.filter (fun x ->
      ys |> List.exists (fun y ->
        x.r $<=$ y.r
      )) in
  let surviving_ys =
    ys |> List.filter (fun y ->
      xs |> List.exists (fun x ->
        y.r $<=$ x.r
      ))
  in
  surviving_xs @ surviving_ys

let difference_ranges pos neg =
  let surviving_pos =
    pos |> List.filter (fun x ->
      not (neg |> List.exists (fun y ->
        x.r $<=$ y.r
      ))
    )
  in
  surviving_pos

let filter_ranges xs cond =
  xs |> List.filter (fun r ->
    let bindings = r.origin.PM.env in
    match cond with
    | R.CondGeneric e ->
        let env = Eval_generic.bindings_to_env bindings in
        Eval_generic.eval_bool env e
    (* todo: would be nice to have CondRegexp also work on
     * eval'ed bindings.
     * We could also use re.match(), to be close to python, but really
     * Eval_generic must do something special here with the metavariable
     * which may not always be a string. The regexp is really done on
     * the text representation of the metavar content.
    *)
    | R.CondRegexp (mvar, (re_str, _re)) ->
        let fk = Parse_info.fake_info "" in
        let fki = AST_generic.empty_id_info () in
        let e =
          (* old: spf "semgrep_re_match(%s, \"%s\")" mvar re_str
           * but too many possible escaping problems, so easier to build
           * an expression manually.
          *)
          G.Call (G.DotAccess(G.Id (("re", fk), fki), fk, EId ((("match"),fk),fki)),
                  (fk, [G.Arg (G.Id ((mvar, fk), fki));
                        G.Arg (G.L (G.String (re_str, fk)))], fk))

        in
        let env = Eval_generic.bindings_to_env_with_just_strings bindings in
        Eval_generic.eval_bool env e
  )

(*****************************************************************************)
(* Evaluating xpatterns *)
(*****************************************************************************)

let matches_of_xpatterns with_caching orig_rule (file, lang, ast) xpatterns =
  let (patterns, _spacegrepsTODO, regexps) =
    partition_xpatterns xpatterns in

  (* semgrep *)
  let mini_rules =
    patterns |> List.map (mini_rule_of_pattern orig_rule) in
  let equivalences =
    (* TODO *)
    [] in
  let semgrep_matches =
    Semgrep_generic.check
      ~with_caching
      ~hook:(fun _ _ -> ())
      mini_rules equivalences file lang ast
  in

  (* spacegrep *)

  (* regexps *)
  let regexp_matches =
    if regexps = []
    then []
    else begin
      let big_str = Common.read_file file in
      regexps |> List.map (fun (id, (s, re)) ->
        let subs = Pcre.exec_all ~rex:re big_str in
        subs |> Array.to_list |> List.map (fun sub ->
          let (charpos, _) = Pcre.get_substring_ofs sub 0 in
          let str = Pcre.get_substring sub 0 in
          let loc = {PI. str; charpos; file; line = -1; column = -1 } in
          let mini_rule = mini_rule_of_regexp (id, s) in
          {PM. rule = mini_rule; file; location = loc, loc;
           tokens = lazy [info_of_token_location file loc]; env = [] }
        )
      ) |> List.flatten
    end
  in

  (* final result *)
  semgrep_matches @ regexp_matches

(*****************************************************************************)
(* Formula evaluation *)
(*****************************************************************************)
(* TODO: use Set instead of list? *)
let rec (evaluate_formula: env -> R.formula -> range_with_mvars list) =
  fun env e ->
  match e with
  | R.P xpat ->
      let id = xpat.R.pid in
      let match_results =
        try Hashtbl.find_all env.pattern_matches id with Not_found -> []
      in
      match_results |> List.map match_result_to_range
  | R.Or xs ->
      xs |> List.map (evaluate_formula env) |> List.flatten
  | R.And xs ->
      let pos, neg, conds = split_and xs in
      (match pos with
       | [] -> failwith "empty And; no positive terms in And"
       | start::pos ->
           let res = evaluate_formula env start in
           let res = pos |> List.fold_left (fun acc x ->
             intersect_ranges acc (evaluate_formula env x)
           ) res in
           let res = neg |> List.fold_left (fun acc x ->
             difference_ranges acc (evaluate_formula env x)
           ) res in
           let res = conds |> List.fold_left (fun acc cond ->
             filter_ranges acc cond
           ) res in
           res
      )
  | R.Not _ ->
      failwith "Invalid Not; you can only negate inside an And"
  | R.MetavarCond _ ->
      failwith "Invalid MetavarCond; you can MetavarCond only inside an And"

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* 'with_caching' is unlabeled because ppx_profiling doesn't support labeled
   arguments *)
let check with_caching hook rules (file, lang, ast) =
  rules |> List.map (fun r ->

    let formula =
      match r.R.formula with
      | R.New f -> f
      | R.Old oldf -> Convert_rule.convert_formula_old oldf
    in

    let xpatterns =
      xpatterns_in_formula formula in
    let matches =
      matches_of_xpatterns with_caching r (file, lang, ast) xpatterns in
    (* match results per minirule id which is the same than pattern_id in
     * the formula *)
    let pattern_matches_per_id =
      group_matches_per_pattern_id matches in
    let env = {
      pattern_matches = pattern_matches_per_id;
      file;
    } in
    let final_ranges =
      evaluate_formula env formula in

    let back_to_match_results =
      final_ranges |> List.map (range_to_match_result) in
    back_to_match_results |> List.iter (fun (m : Pattern_match.t) ->
      hook m.env m.tokens
    );

    back_to_match_results

  ) |> List.flatten
[@@profiling]
(*e: semgrep/engine/Semgrep.ml *)

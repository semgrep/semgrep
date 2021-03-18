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
module MV = Metavariable

let logger = Logging.get_logger [__MODULE__]

let debug_timeout = ref false
let debug_matches = ref true

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
 *       which can be useful to understand keyword arguments
 *     * inheritance awareness, because right now we can't match
 *       code that inherits indirectly form a class mentioned in a pattern
 * There are different options for such global analysis:
 *  - generate a giant file a la CIL, but scale?
 *    (there is a recent LLVM project that does the same)
 *  - do it via a 2 passes process. 1st pass iterates over all files, report
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
  (* subtle but the pattern:/pattern-inside: and
   * pattern-not:/pattern-not-inside: are actually different, so we need
   * to keep the information around during the evaluation.
   * Note that this useful only for few tests in semgrep-rules/ so we
   * probably want to simplify things later and remove the difference between
   * xxx and xxx-inside.
  *)
  inside: R.inside option;

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
(* Range_with_mvars *)
(*****************************************************************************)

let ($<=$) rv1 rv2 =
  Range.($<=$) rv1.r rv2.r &&
  rv1.mvars |> List.for_all (fun (mvar, mval1) ->
    match List.assoc_opt mvar rv2.mvars with
    | None -> true
    | Some mval2 -> Matching_generic.equal_ast_binded_code mval1 mval2
  )

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
    let str = xpat.R.pstr in
    match xpat.R.pat with
    | R.Sem (x, _lang) -> Left3 (x, id, str)
    | R.Spacegrep x -> Middle3 (x, id, str)
    | R.Regexp x -> Right3 (x, id, str)
  )


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
  { r; mvars; origin = m; inside = None }

(* return list of "positive" x list of Not x list of Conds *)
let (split_and:
       R.formula list -> R.formula list * R.formula list * R.metavar_cond list) =
  fun xs ->
  xs |> Common.partition_either3 (fun e ->
    match e with
    | R.Not (f) -> Middle3 (f)
    | R.Leaf (R.MetavarCond c) -> Right3 c
    | _ -> Left3 e
  )

(*****************************************************************************)
(* Adapters *)
(*****************************************************************************)

let (mini_rule_of_pattern: R.t -> (Pattern.t * Rule.pattern_id * string) -> MR.t) =
  fun r (pattern, id, pstr) ->
  { MR.
    id = string_of_int id;
    pattern;
    (* parts that are not really needed I think in this context, since
     * we just care about the matching result.
    *)
    message = ""; severity = MR.Error;
    languages =
      (match r.R.languages with
       | R.L (x, xs) -> x::xs
       | R.LNone | R.LGeneric -> raise Impossible
      );
    (* useful for debugging timeout *)
    pattern_string = pstr;
  }

(* todo: change Pattern_match type so we don't need this gymnastic.
 * A pattern_match should just need the id from the mini_rule
*)
let (mini_rule_of_string: (R.pattern_id * string) -> MR.t) =
  fun (id, s) ->
  { MR.
    id = string_of_int id;
    pattern = G.E (G.L (G.Null (PI.fake_info "")));
    message = ""; severity = MR.Error; languages = [];
    pattern_string = s;
  }

(* todo: same, we should not need that *)
let hmemo = Hashtbl.create 101
let line_col_of_charpos file charpos =
  let conv =
    Common.memoized hmemo file (fun () -> PI.full_charpos_to_pos_large file)
  in
  conv charpos

(* todo: same, we should not need that *)
let info_of_token_location loc =
  { PI.token = PI.OriginTok loc; transfo = PI.NoTransfo }

let lexing_pos_to_loc  file x str =
  (* like Spacegrep.Semgrep.semgrep_pos() *)
  let line = x.Lexing.pos_lnum in
  let charpos = x.Lexing.pos_cnum in
  let column = x.Lexing.pos_cnum - x.Lexing.pos_bol + 1 in
  {PI. str; charpos; file; line; column }

let mval_of_spacegrep_string str t =
  let literal =
    match int_of_string_opt str with
    | Some i -> G.Int (Some i, t)
    (* TODO? could try float_of_string_opt? *)
    | None ->  G.String (str, t)
  in
  MV.E (G.L literal)

(*****************************************************************************)
(* Logic on ranges *)
(*****************************************************************************)

let intersect_ranges xs ys =
  let surviving_xs =
    xs |> List.filter (fun x ->
      ys |> List.exists (fun y ->
        x $<=$ y
      )) in
  let surviving_ys =
    ys |> List.filter (fun y ->
      xs |> List.exists (fun x ->
        y $<=$ x
      ))
  in
  surviving_xs @ surviving_ys

let difference_ranges pos neg =
  let surviving_pos =
    pos |> List.filter (fun x ->
      not (neg |> List.exists (fun y ->
        (* pattern-not vs pattern-not-inside, the difference matters.
         * This fixed 10 mismatches in semgrep-rules.
        *)
        match y.inside with
        (* pattern-not-inside: *)
        | Some R.Inside -> x $<=$ y
        (* pattern-not: we require the ranges to be equal *)
        | None -> x $<=$ y && y $<=$ x
      ))
    )
  in
  surviving_pos

let filter_ranges xs cond =
  xs |> List.filter (fun r ->
    let bindings = r.mvars in
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
          G.Call (G.DotAccess(G.N (G.Id (("re", fk), fki)), fk, EN (Id ((("match"),fk),fki))),
                  (fk, [G.Arg (G.N (G.Id ((mvar, fk), fki)));
                        G.Arg (G.L (G.String (re_str, fk)))], fk))

        in
        let env = Eval_generic.bindings_to_env_with_just_strings bindings in
        Eval_generic.eval_bool env e
  )

(*****************************************************************************)
(* Debugging semgrep *)
(*****************************************************************************)

let debug_semgrep config mini_rules equivalences file lang ast =
  (* process one mini rule at a time *)
  mini_rules |> List.map (fun mr ->
    logger#debug "Checking mini rule with pattern %s" (mr.MR.pattern_string);
    let res =
      Semgrep_generic.check ~hook:(fun _ _ -> ()) config
        [mr] equivalences (file, lang, ast)
    in
    if !debug_matches
    then begin
      let json = res |> List.map JSON_report.match_to_json in
      let json_uniq =
        Common.uniq_by (=) json in
      let res_uniq =
        Common.uniq_by (AST_utils.with_structural_equal PM.equal) res in
      logger#debug "Found %d mini rule matches (uniq = %d) (json_uniq = %d)"
        (List.length res) (List.length res_uniq) (List.length json_uniq);
      res |> List.iter (fun m -> logger#debug "match = %s" (PM.show m));
    end;
    res
  ) |> List.flatten

(*****************************************************************************)
(* Evaluating xpatterns *)
(*****************************************************************************)

let matches_of_xpatterns config orig_rule
    (file, xlang, lazy_ast_and_errors, lazy_content)
    xpatterns
  =
  (* Right now you can only mix semgrep/regexps and spacegrep/regexps, but
   * in theory we could mix all of them together.This is why below
   * I don't match over xlang and instead assume we could have the 3 different
   * kind of patterns at the same time.
  *)
  let (patterns, spacegreps, regexps) = partition_xpatterns xpatterns in

  (* semgrep *)
  let (semgrep_matches, errors), semgrep_match_time =
    match xlang with
    | R.L (lang, _) ->
        let (ast, errors) = Lazy.force lazy_ast_and_errors in
        Common.with_time (fun () ->
          let mini_rules =
            patterns |> List.map (mini_rule_of_pattern orig_rule) in
          let equivalences =
            (* TODO *)
            []
          in
          (* debugging path *)
          if !debug_timeout || !debug_matches
          then
            (debug_semgrep config mini_rules equivalences file lang ast,
             errors)
            (* regular path *)
          else Semgrep_generic.check ~hook:(fun _ _ -> ()) config
              mini_rules equivalences (file, lang, ast),
               errors
        )
    | _ -> ([], []), 0.0
  in

  (* spacegrep *)
  let spacegrep_matches, spacegrep_match_time =
    if spacegreps = []
    then [], 0.0
    else begin
      let src = Spacegrep.Src_file.of_file file in
      let doc = Spacegrep.Parse_doc.of_src src in
      (* pr (Spacegrep.Doc_AST.show doc); *)
      Common.with_time (fun () ->
        spacegreps |> List.map (fun (pat, id, pstr) ->
          let matches =
            Spacegrep.Match.search ~case_sensitive:true src pat doc
          in
          matches |> List.map (fun m ->
            let ((pos1,_),(_pos2,_)) = m.Spacegrep.Match.region in
            let {Spacegrep.Match.value = str; _} = m.Spacegrep.Match.capture in
            let env =
              m.Spacegrep.Match.named_captures |> List.map (fun (s, capture) ->
                let mvar = "$" ^ s in
                let {Spacegrep.Match.value = str; loc = (pos, _)} = capture in
                let loc = lexing_pos_to_loc file pos str in
                let t = info_of_token_location loc in
                let mval = mval_of_spacegrep_string str t in
                mvar, mval
              )
            in

            let loc = lexing_pos_to_loc file pos1 str in
            let mini_rule = mini_rule_of_string (id, pstr) in
            {PM. rule = mini_rule; file; location = loc, loc; env;
             tokens = lazy [info_of_token_location loc];
            }
          )
        ) |> List.flatten
      )
    end
  in

  (* regexps *)
  let regexp_matches, regexp_match_time =
    if regexps = []
    then [], 0.0
    else begin
      let big_str = Lazy.force lazy_content in
      Common.with_time (fun () ->
        regexps |> List.map (fun ((s, re), id, _pstr) ->
          let subs =
            try
              Pcre.exec_all ~rex:re big_str
            with Not_found -> [||]
          in
          subs |> Array.to_list |> List.map (fun sub ->
            let (charpos, _) = Pcre.get_substring_ofs sub 0 in
            let str = Pcre.get_substring sub 0 in

            let (line, column) = line_col_of_charpos file charpos in
            let loc = {PI. str; charpos; file; line; column } in
            let mini_rule = mini_rule_of_string (id, s) in
            {PM. rule = mini_rule; file; location = loc, loc;
             tokens = lazy [info_of_token_location loc]; env = [] }
          )
        ) |> List.flatten
      )
    end
  in

  (* final result *)
  semgrep_matches @ regexp_matches @ spacegrep_matches,
  errors,
  semgrep_match_time +. regexp_match_time +. spacegrep_match_time

(*****************************************************************************)
(* Formula evaluation *)
(*****************************************************************************)
(* TODO: use Set instead of list? *)
let rec (evaluate_formula: env -> R.formula -> range_with_mvars list) =
  fun env e ->
  match e with
  | R.Leaf (R.P (xpat, inside)) ->
      let id = xpat.R.pid in
      let match_results =
        try Hashtbl.find_all env.pattern_matches id with Not_found -> []
      in
      match_results
      |> List.map match_result_to_range
      |> List.map (fun r -> { r with inside })
  | R.Or xs ->
      xs |> List.map (evaluate_formula env) |> List.flatten
  | R.And xs ->
      let pos, neg, conds = split_and xs in
      (* we now treat pattern: and pattern-inside: differently. We first
       * process the pattern: and then the pattern-inside.
       * This fixed only one mismatch in semgrep-rules.
       * old:
       *  (match pos with
       *  | [] -> failwith "empty And; no positive terms in And"
       *  | start::pos ->
       *     let res = evaluate_formula env start in
       *    let res = pos |> List.fold_left (fun acc x ->
       *      intersect_ranges acc (evaluate_formula env x)
       * ...
      *)

      (* let's start with the positive ranges *)
      let posrs = List.map (evaluate_formula env) pos in
      (* subtle: we need to process and intersect the pattern-inside after
       * (see tests/OTHER/rules/inside.yaml) *)
      let posrs, posrs_inside = posrs |> Common.partition_either (fun xs ->
        match xs with
        (* todo? should we double check they are all inside? *)
        | {inside = Some R.Inside; _}::_ -> Right xs
        | _ -> Left xs
      )
      in
      (match posrs @ posrs_inside with
       | [] -> failwith "empty And; no positive terms in And";
       | posr::posrs ->
           let res = posr in
           let res = posrs |> List.fold_left (fun acc r ->
             intersect_ranges acc r
           ) res in

           (* let's remove the negative ranges *)
           let res = neg |> List.fold_left (fun acc x ->
             difference_ranges acc (evaluate_formula env x)
           ) res in
           (* let's apply additional filters *)
           let res = conds |> List.fold_left (fun acc cond ->
             filter_ranges acc cond
           ) res in
           res
      )
  | R.Not _ ->
      failwith "Invalid Not; you can only negate inside an And"
  | R.Leaf (R.MetavarCond _) ->
      failwith "Invalid MetavarCond; you can MetavarCond only inside an And"

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check hook config rules (file, xlang, lazy_ast_and_errors) =
  logger#info "checking %s with %d rules" file (List.length rules);
  let lazy_content = lazy (Common.read_file file) in
  rules |> List.map (fun r ->

    let formula = Rule.formula_of_rule r in
    let relevant_rule =
      if !Flag_semgrep.filter_irrelevant_rules
      then
        match Analyze_rule.regexp_prefilter_of_rule r with
        | None -> true
        | Some (re, f) ->
            let content = Lazy.force lazy_content in
            logger#info "looking for %s in %s" re file;
            (f content)
      else true
    in
    if not relevant_rule
    then begin
      logger#info "skipping rule %s for %s" (r.R.id) file;
      [], [], 0.0
    end else begin
      let xpatterns =
        xpatterns_in_formula formula in
      let matches, errors, match_time =
        matches_of_xpatterns config
          r (file, xlang, lazy_ast_and_errors, lazy_content)
          xpatterns
      in
      logger#info "found %d matches" (List.length matches);
      (* match results per minirule id which is the same than pattern_id in
       * the formula *)
      let pattern_matches_per_id =
        group_matches_per_pattern_id matches in
      let env = {
        pattern_matches = pattern_matches_per_id;
        file;
      } in
      logger#info "evaluating the formula";
      let final_ranges =
        evaluate_formula env formula in
      logger#info "found %d final ranges" (List.length final_ranges);

      (final_ranges |> List.map (range_to_match_result)
       |> before_return (fun v -> v |> List.iter (fun (m : Pattern_match.t) ->
         let str = spf "with rule %s" r.R.id in
         hook str m.env m.tokens
       )),
       errors,
       match_time)
    end
  )
  |> Common2.unzip3
  |> (fun (xxs, yys, match_times) ->
    (List.flatten xxs, List.flatten yys, Common2.sum_float  match_times)
  )
[@@profiling]
(*e: semgrep/engine/Semgrep.ml *)

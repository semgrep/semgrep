(*s: semgrep/engine/Match_rules.ml *)
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
module RP = Report

let logger = Logging.get_logger [ __MODULE__ ]

let debug_timeout = ref false

let debug_matches = ref false

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The core engine of Semgrep.
 *
 * This module implements the boolean composition of patterns.
 * See Match_patterns.ml for the code to handle a single pattern and
 * the visitor/matching engine.
 *
 * Thus, we can decompose the engine in 3 main components:
 *  - composing matching results using boolean/set logic (this file)
 *  - visiting code (=~ Match_patterns.ml)
 *  - matching code (=~ Generic_vs_generic.ml)
 *
 * There are also "preprocessing" work before:
 *  - parsing (lexing, parsing) rules, code, patterns
 *  - normalizing (convert to a generic AST)
 *  - naming (but bugs probably)
 *  - SEMI typing (propagating type decls at least and small inference)
 *  - SEMI analyzing (dataflow constant propagation)
 *    but we could do much more: deep static analysis using Datalog?
 *
 * TODO
 *  - associate the metavariable-regexp to the appropriate pattern
 *    to get the right scope (right now we accept the wrong scope but
 *    this forces to extend some ranges with metavars from other ranges)
 *  - pattern-where-python? use pycaml? works for dlint rule?
 *    right now only 4 rules are using pattern-where-python, so maybe
 *    we could deprecate it?
 *
 * LATER (if really decide to rewrite the whole python wrapper code in OCaml):
 *  - paths
 *  - autofix
 *  - adjust messages with metavariable content
 *  - ...
 *
 * FUTURE WORK:
 * update: TODO move in DeepSemgrep
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

type range_kind = Plain | Inside | Regexp [@@deriving show]

(* range with metavars *)
type range_with_mvars = {
  r : Range.t;
  mvars : Metavariable.bindings;
  (* subtle but the pattern:/pattern-inside:/pattern-regex: and
   * pattern-not:/pattern-not-inside:/pattern-not-regex: are actually different,
   * so we need to keep the information around during the evaluation.
   * Note that this useful only for few tests in semgrep-rules/ so we
   * probably want to simplify things later and remove the difference between
   * xxx, xxx-inside, and xxx-regex.
   * TODO: in fact, if we do a proper intersection of ranges, where we
   * properly intersect (not just filter one or the other), and also merge
   * metavariables, this will clean lots of things, and remove the need
   * to keep around the Inside. 'And' would be commutative again!
   *)
  kind : range_kind;
  origin : Pattern_match.t;
}
[@@deriving show]

type ranges = range_with_mvars list [@@deriving show]

(* !This hash table uses the Hashtbl.find_all property! *)
type id_to_match_results = (pattern_id, Pattern_match.t) Hashtbl.t

type env = {
  config : Config_semgrep.t;
  pattern_matches : id_to_match_results;
  (* used by metavariable-pattern to recursively call evaluate_formula *)
  file : Common.filename;
  rule : R.rule;
  xlang : R.xlang;
  equivalences : Equivalence.equivalences;
}

(*****************************************************************************)
(* Range_with_mvars *)
(*****************************************************************************)

let included_in config rv1 rv2 =
  Range.( $<=$ ) rv1.r rv2.r
  && rv1.mvars
     |> List.for_all (fun (mvar, mval1) ->
            match List.assoc_opt mvar rv2.mvars with
            | None -> true
            | Some mval2 ->
                Matching_generic.equal_ast_binded_code config mval1 mval2)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (xpatterns_in_formula : R.formula -> R.xpattern list) =
 fun e ->
  let res = ref [] in
  e |> R.visit_new_formula (fun xpat -> Common.push xpat res);
  !res

let partition_xpatterns xs =
  let semgrep = ref [] in
  let spacegrep = ref [] in
  let regexp = ref [] in
  let comby = ref [] in
  xs
  |> List.iter (fun xpat ->
         let id = xpat.R.pid in
         let str = xpat.R.pstr in
         match xpat.R.pat with
         | R.Sem (x, _lang) -> Common.push (x, id, str) semgrep
         | R.Spacegrep x -> Common.push (x, id, str) spacegrep
         | R.Regexp x -> Common.push (x, id, str) regexp
         | R.Comby x -> Common.push (x, id, str) comby);
  (List.rev !semgrep, List.rev !spacegrep, List.rev !regexp, List.rev !comby)

let (group_matches_per_pattern_id : Pattern_match.t list -> id_to_match_results)
    =
 fun xs ->
  let h = Hashtbl.create 101 in
  xs
  |> List.iter (fun m ->
         let id = int_of_string m.PM.rule_id.id in
         Hashtbl.add h id m);
  h

let (range_to_pattern_match_adjusted :
      Rule.t -> range_with_mvars -> Pattern_match.t) =
 fun r range ->
  let m = range.origin in
  let rule_id = m.rule_id in
  (* adjust the rule id *)
  let rule_id =
    {
      rule_id with
      Pattern_match.id = r.R.id;
      message = r.R.message (* keep pattern_str which can be useful to debug *);
    }
  in
  (* Need env to be the result of evaluate_formula, which propagates metavariables *)
  (* rather than the original metavariables for the match                          *)
  { m with rule_id; env = range.mvars }

let (match_result_to_range : Pattern_match.t -> range_with_mvars) =
 fun m ->
  let { Pattern_match.range_loc = start_loc, end_loc; env = mvars; _ } = m in
  let r = Range.range_of_token_locations start_loc end_loc in
  { r; mvars; origin = m; kind = Plain }

(* return list of "positive" x list of Not x list of Conds *)
let (split_and :
      R.formula list -> R.formula list * R.formula list * R.metavar_cond list) =
 fun xs ->
  xs
  |> Common.partition_either3 (fun e ->
         match e with
         | R.Not f -> Middle3 f
         | R.Leaf (R.MetavarCond c) -> Right3 c
         | _ -> Left3 e)

let lazy_force x = Lazy.force x [@@profiling]

(*****************************************************************************)
(* Adapters *)
(*****************************************************************************)

let (mini_rule_of_pattern : R.t -> Pattern.t * Rule.pattern_id * string -> MR.t)
    =
 fun r (pattern, id, pstr) ->
  {
    MR.id = string_of_int id;
    pattern;
    (* parts that are not really needed I think in this context, since
     * we just care about the matching result.
     *)
    message = "";
    severity = MR.Error;
    languages =
      ( match r.R.languages with
      | R.L (x, xs) -> x :: xs
      | R.LNone | R.LGeneric -> raise Impossible );
    (* useful for debugging timeout *)
    pattern_string = pstr;
  }

(*****************************************************************************)
(* Logic on ranges *)
(*****************************************************************************)

(* when we know x <= y, are the ranges also in the good Inside direction *)
let inside_compatible x y =
  let x_inside = x.kind = Inside in
  let y_inside = y.kind = Inside in
  (* IF x is pattern-inside THEN y must be too:
   * if we do x=pattern-inside:[1-2] /\ y=pattern:[1-3]
   * we don't want this x to survive.
   * See tests/OTHER/rules/and_inside.yaml
   *)
  (not x_inside) || y_inside

(* We now not only check whether a range is included in another,
 * we also merge their metavars. The reason is that with some rules like:
 * - pattern-inside:  { dialect: $DIALECT,  ... }
 * - pattern: { minVersion: 'TLSv1' }
 * - metavariable-regex:
 *     metavariable: $DIALECT
 *     regex: "['\"](mariadb|mysql|postgres)['\"]"
 * when intersecting the two patterns, we must propagate the binding
 * for $DIALECT, so we can evaluate the metavariable-regex.
 *
 * alt: we could force the user to first And the metavariable-regex
 * with the pattern-inside, to have the right scope.
 * See https://github.com/returntocorp/semgrep/issues/2664
 * alt: we could do the rewriting ourselves, detecting that the
 * metavariable-regex has the wrong scope.
 *)
let intersect_ranges config xs ys =
  let left_merge r1 r2 =
    (* [r1] extended with [r2.mvars], assumes [included_in config r1 r2] *)
    let r2_only_mvars =
      r2.mvars
      |> List.filter (fun (mvar, _) -> not (List.mem_assoc mvar r1.mvars))
    in
    { r1 with mvars = r2_only_mvars @ r1.mvars }
  in
  let left_included_merge us vs =
    us
    |> Common2.map_flatten (fun u ->
           vs
           |> List.filter_map (fun v ->
                  if included_in config u v && inside_compatible u v then
                    Some (left_merge u v)
                  else None))
  in
  if !debug_matches then
    logger#info "intersect_range:\n\t%s\nvs\n\t%s" (show_ranges xs)
      (show_ranges ys);
  left_included_merge xs ys @ left_included_merge ys xs
  [@@profiling]

let difference_ranges config pos neg =
  let surviving_pos =
    pos
    |> List.filter (fun x ->
           not
             ( neg
             |> List.exists (fun y ->
                    (* pattern-not vs pattern-not-inside vs pattern-not-regex,
                     * the difference matters!
                     * This fixed 10 mismatches in semgrep-rules and some e2e tests.
                     *)
                    match y.kind with
                    (* pattern-not-inside: x cannot occur inside y *)
                    | Inside -> included_in config x y
                    (* pattern-not-regex: x and y exclude each other *)
                    | Regexp -> included_in config x y || included_in config y x
                    (* pattern-not: we require the ranges to be equal *)
                    | Plain -> included_in config x y && included_in config y x)
             ))
  in
  surviving_pos
  [@@profiling]

(*****************************************************************************)
(* Debugging semgrep *)
(*****************************************************************************)

let debug_semgrep config mini_rules equivalences file lang ast =
  (* process one mini rule at a time *)
  logger#info "DEBUG SEMGREP MODE!";
  mini_rules
  |> List.map (fun mr ->
         logger#debug "Checking mini rule with pattern %s" mr.MR.pattern_string;
         let res =
           Match_patterns.check
             ~hook:(fun _ _ -> ())
             config [ mr ] equivalences (file, lang, ast)
         in
         if !debug_matches then
           (* TODO
              let json = res |> List.map (fun x ->
                   x |> JSON_report.match_to_match_ |> SJ.string_of_match_) in
              let json_uniq = Common.uniq_by ( = ) json in
              let res_uniq =
                Common.uniq_by (AST_utils.with_structural_equal PM.equal) res
              in
              logger#debug
                "Found %d mini rule matches (uniq = %d) (json_uniq = %d)"
                (List.length res) (List.length res_uniq) (List.length json_uniq);
           *)
           res |> List.iter (fun m -> logger#debug "match = %s" (PM.show m));
         res)
  |> List.flatten

(*****************************************************************************)
(* Evaluating Semgrep patterns *)
(*****************************************************************************)

let matches_of_patterns config orig_rule equivalences
    (file, xlang, lazy_ast_and_errors) patterns =
  match xlang with
  | R.L (lang, _) ->
      let (ast, errors), parse_time =
        Common.with_time (fun () -> lazy_force lazy_ast_and_errors)
      in
      let (matches, errors), match_time =
        Common.with_time (fun () ->
            let mini_rules =
              patterns |> List.map (mini_rule_of_pattern orig_rule)
            in

            (* debugging path *)
            if !debug_timeout || !debug_matches then
              ( debug_semgrep config mini_rules equivalences file lang ast,
                errors ) (* regular path *)
            else
              ( Match_patterns.check
                  ~hook:(fun _ _ -> ())
                  config mini_rules equivalences (file, lang, ast),
                errors ))
      in
      { RP.matches; errors; profiling = { RP.parse_time; match_time } }
  | _ -> RP.empty_semgrep_result

(*****************************************************************************)
(* Extended patterns matchers *)
(*****************************************************************************)

(* This type and matches_of_matcher() below factorize code between
 * the regexp, spacegrep, and now comby matchers.
 *)
type ('target_content, 'xpattern) xpattern_matcher = {
  (* init returns an option to let the matcher the option to skip
   * certain files (e.g., big binary or minified files for spacegrep)
   *)
  init : filename -> 'target_content option;
  matcher :
    'target_content -> filename -> 'xpattern -> (match_range * MV.bindings) list;
}

(* bugfix: I used to just report one token_location, and if the match
 * was on multiple lines anyway the token_location.str was contain
 * the whole string. However, external programs using a startp/endp
 * expect a different location if the end part is on a different line
 * (e.g., the semgrep Python wrapper), so I now return a pair.
 *)
and match_range = Parse_info.token_location * Parse_info.token_location

(* this will be adjusted later in range_to_pattern_match_adjusted *)
let fake_rule_id (id, str) =
  { PM.id = string_of_int id; pattern_string = str; message = "" }

(* todo: same, we should not need that *)
let info_of_token_location loc =
  { PI.token = PI.OriginTok loc; transfo = PI.NoTransfo }

let (matches_of_matcher :
      ('xpattern * pattern_id * string) list ->
      ('target_content, 'xpattern) xpattern_matcher ->
      filename ->
      RP.times RP.match_result) =
 fun xpatterns matcher file ->
  if xpatterns = [] then RP.empty_semgrep_result
  else
    let target_content_opt, parse_time =
      Common.with_time (fun () -> matcher.init file)
    in
    match target_content_opt with
    | None -> RP.empty_semgrep_result (* less: could include parse_time *)
    | Some target_content ->
        let res, match_time =
          Common.with_time (fun () ->
              xpatterns
              |> List.map (fun (xpat, id, pstr) ->
                     let xs = matcher.matcher target_content file xpat in
                     xs
                     |> List.map (fun ((loc1, loc2), env) ->
                            (* this will be adjusted later *)
                            let rule_id = fake_rule_id (id, pstr) in
                            {
                              PM.rule_id;
                              file;
                              range_loc = (loc1, loc2);
                              env;
                              tokens = lazy [ info_of_token_location loc1 ];
                            }))
              |> List.flatten)
        in
        {
          RP.matches = res;
          errors = [];
          profiling = { RP.parse_time; match_time };
        }

(* todo: same, we should not need that *)
let hmemo = Hashtbl.create 101

let line_col_of_charpos file charpos =
  let conv =
    Common.memoized hmemo file (fun () -> PI.full_charpos_to_pos_large file)
  in
  conv charpos

let mval_of_string str t =
  let literal =
    match int_of_string_opt str with
    | Some i -> G.Int (Some i, t)
    (* TODO? could try float_of_string_opt? *)
    | None -> G.String (str, t)
  in
  MV.E (G.L literal)

(*-------------------------------------------------------------------*)
(* Spacegrep *)
(*-------------------------------------------------------------------*)

let lexing_pos_to_loc file x str =
  (* almost like Spacegrep.Semgrep.semgrep_pos() *)
  let line = x.Lexing.pos_lnum in
  let charpos = x.Lexing.pos_cnum in
  (* bugfix: not +1 here, Parse_info.column is 0-based.
   * JSON_report.json_range does the adjust_column + 1.
   *)
  let column = x.Lexing.pos_cnum - x.Lexing.pos_bol in
  { PI.str; charpos; file; line; column }

let spacegrep_matcher (doc, src) file pat =
  let matches = Spacegrep.Match.search ~case_sensitive:true src pat doc in
  matches
  |> List.map (fun m ->
         let (pos1, _), (_, pos2) = m.Spacegrep.Match.region in
         let { Spacegrep.Match.value = str; _ } = m.Spacegrep.Match.capture in
         let env =
           m.Spacegrep.Match.named_captures
           |> List.map (fun (s, capture) ->
                  let mvar = "$" ^ s in
                  let { Spacegrep.Match.value = str; loc = pos, _ } = capture in
                  let loc = lexing_pos_to_loc file pos str in
                  let t = info_of_token_location loc in
                  let mval = mval_of_string str t in
                  (mvar, mval))
         in
         let loc1 = lexing_pos_to_loc file pos1 str in
         let loc2 = lexing_pos_to_loc file pos2 "" in
         ((loc1, loc2), env))

let matches_of_spacegrep spacegreps file =
  matches_of_matcher spacegreps
    {
      init =
        (fun _ ->
          (* coupling: mostly copypaste of Spacegrep_main.run_all *)
          (*
           We inspect the first 4096 bytes to guess whether the file type.
           This saves time on large files, by reading typically just one
           block from the file system.
          *)
          let peek_length = 4096 in
          let partial_doc_src =
            Spacegrep.Src_file.of_file ~max_len:peek_length file
          in
          let doc_type = Spacegrep.File_type.classify partial_doc_src in
          match doc_type with
          | Minified | Binary ->
              logger#info "ignoring gibberish file: %s\n%!" file;
              None
          | _ ->
              let src =
                if
                  Spacegrep.Src_file.length partial_doc_src < peek_length
                  (* it's actually complete, no need to re-input the file *)
                then partial_doc_src
                else Spacegrep.Src_file.of_file file
              in
              (* pr (Spacegrep.Doc_AST.show doc); *)
              Some (Spacegrep.Parse_doc.of_src src, src));
      matcher = spacegrep_matcher;
    }
    file
  [@@profiling]

(*-------------------------------------------------------------------*)
(* Regexps *)
(*-------------------------------------------------------------------*)
let regexp_matcher big_str file (re_str, re) =
  let subs = try Pcre.exec_all ~rex:re big_str with Not_found -> [||] in
  subs |> Array.to_list
  |> List.map (fun sub ->
         let matched_str = Pcre.get_substring sub 0 in
         let charpos, _ = Pcre.get_substring_ofs sub 0 in
         let str = matched_str in
         let line, column = line_col_of_charpos file charpos in
         let loc1 = { PI.str; charpos; file; line; column } in

         let charpos = charpos + String.length str in
         let str = "" in
         let line, column = line_col_of_charpos file charpos in
         let loc2 = { PI.str; charpos; file; line; column } in

         (* return regexp binded group $1 $2 etc *)
         let n = Pcre.num_of_subs sub in
         let env =
           match n with
           | 1 -> []
           | _ when n <= 0 -> raise Impossible
           | n ->
               Common2.enum 1 (n - 1)
               |> Common.map_filter (fun n ->
                      try
                        let charpos, _ = Pcre.get_substring_ofs sub n in
                        let str = Pcre.get_substring sub n in
                        let line, column = line_col_of_charpos file charpos in
                        let loc = { PI.str; charpos; file; line; column } in
                        let t = PI.mk_info_of_loc loc in
                        Some (spf "$%d" n, MV.Text (str, t))
                      with Not_found ->
                        logger#debug "not found %d substring of %s in %s" n
                          re_str matched_str;
                        None)
         in
         ((loc1, loc2), env))

let matches_of_regexs regexps lazy_content file =
  matches_of_matcher regexps
    {
      init = (fun _ -> Some (Lazy.force lazy_content));
      matcher = regexp_matcher;
    }
    file
  [@@profiling]

(*-------------------------------------------------------------------*)
(* Comby *)
(*-------------------------------------------------------------------*)

module CK = Comby_kernel
module MS = CK.Matchers.Metasyntax

(* less: "if you need line/column conversion [in Comby], then there's another
 * function to run over matches to hydrate line/column info in the result"
 * src: https://github.com/comby-tools/comby/issues/244
 *)
let line_col_charpos_of_comby_range file range =
  let { CK.Match.Location.offset = charpos; line = _; column = _ } =
    range.CK.Match.Range.match_start
  in
  (* reusing line_col_of_charpos is fine for now *)
  let line, col = line_col_of_charpos file charpos in
  (line, col, charpos)

let comby_matcher (m_all, source) file pat =
  let matches = m_all ~template:pat ~source () in
  (*Format.printf "%a@." CK.Match.pp_json_lines (None, matches);*)
  matches
  |> List.map (fun { CK.Match.range; environment; matched } ->
         let env =
           CK.Match.Environment.vars environment
           |> List.map (fun s ->
                  let mvar = "$" ^ s in
                  let str_opt = CK.Match.Environment.lookup environment s in
                  let range_opt =
                    CK.Match.Environment.lookup_range environment s
                  in
                  match (str_opt, range_opt) with
                  | Some str, Some range ->
                      let line, column, charpos =
                        line_col_charpos_of_comby_range file range
                      in
                      let loc = { PI.str; charpos; file; line; column } in
                      let t = info_of_token_location loc in
                      let mval = mval_of_string str t in
                      (mvar, mval)
                  | _ -> raise Impossible)
         in
         let line, column, charpos =
           line_col_charpos_of_comby_range file range
         in
         let loc1 = { PI.str = matched; charpos; file; line; column } in

         let charpos2 = charpos + String.length matched in
         let line, column = line_col_of_charpos file charpos2 in
         let loc2 = { PI.str = ""; charpos = charpos2; file; line; column } in

         ((loc1, loc2), env))

let matches_of_combys combys lazy_content file =
  matches_of_matcher combys
    {
      init =
        (fun _ ->
          let _d, _b, e = Common2.dbe_of_filename file in
          let metasyntax =
            {
              MS.syntax =
                [ MS.Hole (Everything, MS.Delimited (Some "$", None)) ];
              identifier = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            }
          in
          match
            CK.Matchers.Alpha.select_with_extension ~metasyntax ("." ^ e)
          with
          | None ->
              logger#info "no Alpha Comby module for extension %s" e;
              None
          | Some x ->
              let (module M : CK.Matchers.Matcher.S) = x in
              Some
                ( (fun ~template ~source () -> M.all ~template ~source ()),
                  Lazy.force lazy_content ));
      matcher = comby_matcher;
    }
    file
  [@@profiling]

(*****************************************************************************)
(* Evaluating xpatterns *)
(*****************************************************************************)

let matches_of_xpatterns config orig_rule equivalences
    (file, xlang, lazy_ast_and_errors, lazy_content) xpatterns =
  (* Right now you can only mix semgrep/regexps and spacegrep/regexps, but
   * in theory we could mix all of them together. This is why below
   * I don't match over xlang and instead assume we could have multiple
   * kinds of patterns at the same time.
   *)
  let patterns, spacegreps, regexps, combys = partition_xpatterns xpatterns in

  (* final result *)
  RP.collate_semgrep_results
    [
      matches_of_patterns config orig_rule equivalences
        (file, xlang, lazy_ast_and_errors)
        patterns;
      matches_of_spacegrep spacegreps file;
      matches_of_regexs regexps lazy_content file;
      matches_of_combys combys lazy_content file;
    ]
  [@@profiling]

(*****************************************************************************)
(* Formula evaluation *)
(*****************************************************************************)

(* less: use Set instead of list? *)
let rec (evaluate_formula :
          env -> range_with_mvars option -> R.formula -> range_with_mvars list)
    =
 fun env ctx_opt e ->
  match e with
  | R.Leaf (R.P (xpat, inside)) ->
      let id = xpat.R.pid in
      let match_results =
        try Hashtbl.find_all env.pattern_matches id with Not_found -> []
      in
      let kind =
        match inside with
        | Some R.Inside -> Inside
        | None when R.is_regexp xpat -> Regexp
        | None -> Plain
      in
      match_results
      |> List.map match_result_to_range
      |> List.map (fun r -> { r with kind })
  | R.Or xs -> xs |> List.map (evaluate_formula env ctx_opt) |> List.flatten
  | R.And xs -> (
      let pos, neg, conds = split_and xs in

      (* we now treat pattern: and pattern-inside: differently. We first
       * process the pattern: and then the pattern-inside.
       * This fixed only one mismatch in semgrep-rules.
       *
       * old: the old code was simpler ... but incorrect.
       *  (match pos with
       *  | [] -> failwith "empty And; no positive terms in And"
       *  | start::pos ->
       *     let res = evaluate_formula env start in
       *    let res = pos |> List.fold_left (fun acc x ->
       *      intersect_ranges acc (evaluate_formula env x)
       * ...
       *)

      (* let's start with the positive ranges *)
      let posrs = List.map (evaluate_formula env ctx_opt) pos in
      (* subtle: we need to process and intersect the pattern-inside after
       * (see tests/OTHER/rules/inside.yaml).
       * TODO: this is ugly; AND should be commutative, so we should just
       * merge ranges, not just filter one or the other.
       * update: however we have some tests that rely on pattern-inside:
       * being special, see tests/OTHER/rules/and_inside.yaml.
       *)
      let posrs, posrs_inside =
        posrs
        |> Common.partition_either (fun xs ->
               match xs with
               (* todo? should we double check they are all inside? *)
               | { kind = Inside; _ } :: _ -> Right xs
               | _ -> Left xs)
      in
      let all_posr =
        match posrs @ posrs_inside with
        | [] -> (
            match ctx_opt with
            | None -> failwith "empty And; no positive terms in And"
            | Some r -> [ [ r ] ] )
        | ps -> ps
      in
      match all_posr with
      | [] -> failwith "FIXME"
      | posr :: posrs ->
          let res = posr in
          let res =
            posrs
            |> List.fold_left
                 (fun acc r -> intersect_ranges env.config acc r)
                 res
          in

          (* let's remove the negative ranges *)
          let res =
            neg
            |> List.fold_left
                 (fun acc x ->
                   difference_ranges env.config acc
                     (evaluate_formula env ctx_opt x))
                 res
          in
          (* let's apply additional filters.
           * TODO: Note that some metavariable-regexp may be part of an
           * AND where not all patterns define the metavar, e.g.,
           *   pattern-inside: def $FUNC() ...
           *   pattern: return $X
           *   metavariable-regexp: $FUNC regex: (foo|bar)
           * in which case the order in which we do the operation matters
           * (at this point intersect_range will have filtered the
           *  range of the pattern_inside).
           * alternative solutions?
           *  - bind closer metavariable-regexp with the relevant pattern
           *  - propagate metavariables when intersecting ranges
           *  - distribute filter_range in intersect_range?
           * See https://github.com/returntocorp/semgrep/issues/2664
           *)
          let res =
            conds
            |> List.fold_left (fun acc cond -> filter_ranges env acc cond) res
          in
          res )
  | R.Not _ -> failwith "Invalid Not; you can only negate inside an And"
  | R.Leaf (R.MetavarCond _) ->
      failwith "Invalid MetavarCond; you can MetavarCond only inside an And"

and filter_ranges env xs cond =
  xs
  |> List.filter (fun r ->
         let bindings = r.mvars in
         match cond with
         | R.CondGeneric e ->
             let env = Eval_generic.bindings_to_env bindings in
             Eval_generic.eval_bool env e
         | R.CondPattern (mvar, opt_lang, formula) ->
             match_range_with_pattern env r mvar opt_lang formula
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
               G.Call
                 ( G.DotAccess
                     ( G.N (G.Id (("re", fk), fki)),
                       fk,
                       EN (Id (("match", fk), fki)) ),
                   ( fk,
                     [
                       G.Arg (G.N (G.Id ((mvar, fk), fki)));
                       G.Arg (G.L (G.String (re_str, fk)));
                     ],
                     fk ) )
             in

             let env =
               Eval_generic.bindings_to_env_with_just_strings bindings
             in
             Eval_generic.eval_bool env e)

and match_range_with_pattern env r mvar opt_lang formula =
  let bindings = r.mvars in
  (* If anything goes wrong the default is to filter out! *)
  match List.assoc_opt mvar bindings with
  | None ->
      (* THINK: fatal error instead? *)
      logger#error "rule %s: metavariable-pattern: %s not found" env.rule.id
        mvar;
      false
  | Some mval -> (
      let mval_range = MV.range_of_mvalue mval in
      match (opt_lang, mval) with
      | Some lang, MV.Text (content, _)
      | Some lang, MV.E (G.L (G.String (content, _))) ->
          (* The matched text will be interpreted according to `lang'. *)
          let lazy_ast_and_errors =
            lazy_ast_of_string lang content (fun () ->
                spf
                  "rule %s: metavariable-pattern: failed to fully parse the \
                   content of %s"
                  env.rule.id mvar)
          in
          eval_nested_formula env env.xlang formula lazy_ast_and_errors
            (lazy content)
            None
      | Some _lang, _mval ->
          (* THINK: fatal error instead? *)
          logger#error
            "rule %s: metavariable-pattern: the content of %s is not text"
            env.rule.id mvar;
          false
      | None, _mval -> (
          match MV.program_of_mvalue mval with
          | None ->
              (* THINK: fatal error instead? *)
              logger#error
                "rule %s: metavariable-pattern: %s does not bound a sub-program"
                env.rule.id mvar;
              false
          | Some mast ->
              let lazy_ast_and_errors = lazy (mast, []) in
              let lazy_content =
                lazy (Range.content_at_range env.file mval_range)
              in
              eval_nested_formula env env.xlang formula lazy_ast_and_errors
                lazy_content
                (Some { r with r = mval_range }) ) )

and lazy_ast_of_string lang str warn_msg =
  lazy
    (let ext =
       match Lang.ext_of_lang lang with x :: _ -> x | [] -> assert false
     in
     Common2.with_tmp_file ~str ~ext (fun file ->
         let { Parse_target.ast; errors; _ } =
           Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file
         in
         if errors <> [] then pr2 (warn_msg ());
         (ast, errors)))

and eval_nested_formula env xlang formula lazy_ast_and_errors lazy_content
    opt_context =
  (* the following code is very similar to `check' below;
     * we could maybe factorize things *)
  let xpatterns = xpatterns_in_formula formula in
  let res =
    matches_of_xpatterns env.config env.rule env.equivalences
      (env.file, xlang, lazy_ast_and_errors, lazy_content)
      xpatterns
  in
  let pattern_matches_per_id = group_matches_per_pattern_id res.matches in
  let env = { env with pattern_matches = pattern_matches_per_id } in
  match evaluate_formula env opt_context formula with
  | [] -> false
  | _ :: _ -> true
  [@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check hook default_config rules equivs file_and_more =
  let file, xlang, lazy_ast_and_errors = file_and_more in
  logger#info "checking %s with %d rules" file (List.length rules);
  if rules = [] then logger#error "empty rules";
  if !Common.profile = Common.ProfAll then (
    logger#info "forcing eval of ast outside of rules, for better profile";
    lazy_force lazy_ast_and_errors |> ignore );

  let lazy_content = lazy (Common.read_file file) in
  rules
  |> List.map (fun r ->
         Common.profile_code (spf "real_rule:%s" r.R.id) (fun () ->
             let formula = Rule.formula_of_rule r in
             let relevant_rule =
               if !Flag_semgrep.filter_irrelevant_rules then (
                 match Analyze_rule.regexp_prefilter_of_rule r with
                 | None -> true
                 | Some (re, f) ->
                     let content = Lazy.force lazy_content in
                     logger#info "looking for %s in %s" re file;
                     f content )
               else true
             in
             if not relevant_rule then (
               logger#info "skipping rule %s for %s" r.R.id file;
               RP.empty_semgrep_result )
             else
               let xpatterns = xpatterns_in_formula formula in
               let config = r.settings ||| default_config in
               let res =
                 matches_of_xpatterns config r equivs
                   (file, xlang, lazy_ast_and_errors, lazy_content)
                   xpatterns
               in
               logger#info "found %d matches" (List.length res.matches);
               (* match results per minirule id which is the same than pattern_id in
                * the formula *)
               let pattern_matches_per_id =
                 group_matches_per_pattern_id res.matches
               in
               let env =
                 {
                   config;
                   pattern_matches = pattern_matches_per_id;
                   file;
                   rule = r;
                   xlang;
                   equivalences = equivs;
                 }
               in
               logger#info "evaluating the formula";
               let final_ranges = evaluate_formula env None formula in
               logger#info "found %d final ranges" (List.length final_ranges);

               {
                 matches =
                   final_ranges
                   |> List.map (range_to_pattern_match_adjusted r)
                   (* dedup similar findings (we do that also in Match_patterns.ml,
                    * but different mini-rules matches can now become the same match)
                    *)
                   |> Common.uniq_by (AST_utils.with_structural_equal PM.equal)
                   |> before_return (fun v ->
                          v
                          |> List.iter (fun (m : Pattern_match.t) ->
                                 let str = spf "with rule %s" r.R.id in
                                 hook str m.env m.tokens));
                 errors = res.errors;
                 profiling = res.profiling;
               }))
  |> RP.collate_semgrep_results
  [@@profiling]

(*e: semgrep/engine/Match_rules.ml *)

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
open Common
module R = Rule
module MR = Mini_rule
module PM = Pattern_match
module G = AST_generic
module PI = Parse_info
module MV = Metavariable
module RP = Report
module S = Specialize_formula
module RM = Range_with_metavars
module E = Semgrep_error_code
module Resp = Output_from_core_t

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

(* !This hash table uses the Hashtbl.find_all property! *)
type id_to_match_results = (pattern_id, Pattern_match.t) Hashtbl.t

type env = {
  (* less: we might want to get rid of equivalences at some point as
   * they are not exposed to the user anymore. *)
  config : Config_semgrep.t * Equivalence.equivalences;
  pattern_matches : id_to_match_results;
  (* used by metavariable-pattern to recursively call evaluate_formula *)
  file_and_more : Xtarget.t;
  rule : R.rule;
  (* problems found during evaluation, one day these may be caught earlier by
   * the meta-checker *)
  errors : E.error list ref;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Report errors during evaluation to the user rather than just logging them
 * as we did before. *)
let error env msg =
  (* We are not supposed to report errors in the config file for several reasons
   * (one being that it's often a temporary file anyways), so we report them on
   * the target file. *)
  let loc = PI.first_loc_of_file env.file_and_more.Xtarget.file in
  (* TODO: warning or error? MatchingError or ... ? *)
  let err =
    E.mk_error ~rule_id:(Some (fst env.rule.Rule.id)) loc msg E.MatchingError
  in
  Common.push err env.errors

let (xpatterns_in_formula : S.sformula -> (R.xpattern * R.inside option) list) =
 fun e ->
  let res = ref [] in
  e |> S.visit_sformula (fun xpat inside -> Common.push (xpat, inside) res);
  !res

let partition_xpatterns xs =
  let semgrep = ref [] in
  let spacegrep = ref [] in
  let regexp = ref [] in
  let comby = ref [] in
  xs
  |> List.iter (fun (xpat, inside) ->
         let id = xpat.R.pid in
         let str = fst xpat.R.pstr in
         match xpat.R.pat with
         | R.Sem (x, _lang) -> Common.push (x, inside, id, str) semgrep
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

let (range_to_pattern_match_adjusted : Rule.t -> RM.t -> Pattern_match.t) =
 fun r range ->
  let m = range.origin in
  let rule_id = m.rule_id in
  (* adjust the rule id *)
  let rule_id =
    {
      rule_id with
      Pattern_match.id = fst r.R.id;
      message = r.R.message (* keep pattern_str which can be useful to debug *);
    }
  in
  (* Need env to be the result of evaluate_formula, which propagates metavariables *)
  (* rather than the original metavariables for the match                          *)
  { m with rule_id; env = range.mvars }

let error_with_rule_id rule_id (error : E.error) =
  { error with rule_id = Some rule_id }

let lazy_force x = Lazy.force x [@@profiling]

(*****************************************************************************)
(* Adapters *)
(*****************************************************************************)

(* old: Before we used to pass the entire rule and we got the list of languages
 * from there. But after metavariable-pattern we can now recursively call
 * evaluate_formula using a different language than the one used by the rule!
 * If the rule is a generic one, but the nested pattern formula is not, then
 * this will raise Impossible... Thus, now we have to pass the language(s) that
 * we are specifically targeting. *)
let (mini_rule_of_pattern :
      Xlang.t -> Pattern.t * R.inside option * Rule.pattern_id * string -> MR.t)
    =
 fun xlang (pattern, inside, id, pstr) ->
  {
    MR.id = string_of_int id;
    pattern;
    inside = Option.is_some inside;
    (* parts that are not really needed I think in this context, since
     * we just care about the matching result.
     *)
    message = "";
    severity = R.Error;
    languages =
      (match xlang with
      | L (x, xs) -> x :: xs
      | LRegex
      | LGeneric ->
          raise Impossible);
    (* useful for debugging timeout *)
    pattern_string = pstr;
  }

(*****************************************************************************)
(* Debugging semgrep *)
(*****************************************************************************)

let debug_semgrep config mini_rules file lang ast =
  (* process one mini rule at a time *)
  logger#info "DEBUG SEMGREP MODE!";
  mini_rules
  |> List.map (fun mr ->
         logger#debug "Checking mini rule with pattern %s" mr.MR.pattern_string;
         let res =
           Match_patterns.check
             ~hook:(fun _ _ -> ())
             config [ mr ] (file, lang, ast)
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

let matches_of_patterns ?range_filter config file_and_more patterns =
  let { Xtarget.file; xlang; lazy_ast_and_errors; lazy_content = _ } =
    file_and_more
  in
  match xlang with
  | Xlang.L (lang, _) ->
      let (ast, errors), parse_time =
        Common.with_time (fun () -> lazy_force lazy_ast_and_errors)
      in
      let (matches, errors), match_time =
        Common.with_time (fun () ->
            let mini_rules =
              patterns |> List.map (mini_rule_of_pattern xlang)
            in

            if !debug_timeout || !debug_matches then
              (* debugging path *)
              (debug_semgrep config mini_rules file lang ast, errors)
            else
              (* regular path *)
              ( Match_patterns.check
                  ~hook:(fun _ _ -> ())
                  ?range_filter config mini_rules (file, lang, ast),
                errors ))
      in
      {
        RP.matches;
        errors;
        skipped = [];
        profiling = { RP.parse_time; match_time };
      }
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
and match_range = PI.token_location * PI.token_location

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
          skipped = [];
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
  MV.E (G.L literal |> G.e)

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
  let matches =
    Spacegrep.Match.search ~no_skip_search:false ~case_sensitive:true src pat
      doc
  in
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
          | Minified
          | Binary ->
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
  let subs = SPcre.exec_all_noerr ~rex:re big_str in
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

let matches_of_xpatterns config file_and_more xpatterns =
  let { Xtarget.file; lazy_content; _ } = file_and_more in
  (* Right now you can only mix semgrep/regexps and spacegrep/regexps, but
   * in theory we could mix all of them together. This is why below
   * I don't match over xlang and instead assume we could have multiple
   * kinds of patterns at the same time.
   *)
  let patterns, spacegreps, regexps, combys = partition_xpatterns xpatterns in

  (* final result *)
  RP.collate_pattern_results
    [
      matches_of_patterns config file_and_more patterns;
      matches_of_spacegrep spacegreps file;
      matches_of_regexs regexps lazy_content file;
      matches_of_combys combys lazy_content file;
    ]
  [@@profiling]

(*****************************************************************************)
(* Formula evaluation *)
(*****************************************************************************)

(*
   Find the metavariable value, convert it back to a string, and
   run it through an analyzer that returns true if there's a "match".
*)
let analyze_metavar env bindings mvar analyzer =
  match List.assoc_opt mvar bindings with
  | None ->
      error env
        (Common.spf
           "metavariable-analysis failed because %s is not in scope, please \
            check your rule"
           mvar);
      false
  | Some mval -> (
      match Eval_generic.text_of_binding mvar mval with
      | None -> false
      | Some data -> analyzer data)

let rec filter_ranges env xs cond =
  xs
  |> List.filter (fun r ->
         let bindings = r.RM.mvars in
         match cond with
         | R.CondEval e ->
             let env = Eval_generic.bindings_to_env (fst env.config) bindings in
             Eval_generic.eval_bool env e
         | R.CondNestedFormula (mvar, opt_lang, formula) ->
             satisfies_metavar_pattern_condition env r mvar opt_lang formula
         (* todo: would be nice to have CondRegexp also work on
          * eval'ed bindings.
          * We could also use re.match(), to be close to python, but really
          * Eval_generic must do something special here with the metavariable
          * which may not always be a string. The regexp is really done on
          * the text representation of the metavar content.
          *)
         | R.CondRegexp (mvar, (re_str, _re)) ->
             let fk = PI.unsafe_fake_info "" in
             let fki = AST_generic.empty_id_info () in
             let e =
               (* old: spf "semgrep_re_match(%s, \"%s\")" mvar re_str
                * but too many possible escaping problems, so easier to build
                * an expression manually.
                *)
               G.Call
                 ( G.DotAccess
                     ( G.N (G.Id (("re", fk), fki)) |> G.e,
                       fk,
                       FN (Id (("match", fk), fki)) )
                   |> G.e,
                   ( fk,
                     [
                       G.Arg (G.L (G.String (re_str, fk)) |> G.e);
                       G.Arg (G.N (G.Id ((mvar, fk), fki)) |> G.e);
                     ],
                     fk ) )
               |> G.e
             in

             let env =
               Eval_generic.bindings_to_env_with_just_strings (fst env.config)
                 bindings
             in
             Eval_generic.eval_bool env e
         | R.CondAnalysis (mvar, CondEntropy) ->
             let bindings = r.mvars in
             analyze_metavar env bindings mvar Entropy.has_high_score
         | R.CondAnalysis (mvar, CondReDoS) ->
             let bindings = r.mvars in
             let analyze re_str =
               logger#debug
                 "Analyze regexp captured by %s for ReDoS vulnerability: %s"
                 mvar re_str;
               match ReDoS.find_vulnerable_subpatterns re_str with
               | Ok [] -> false
               | Ok subpatterns ->
                   subpatterns
                   |> List.iter (fun pat ->
                          logger#info
                            "The following subpattern was predicted to be \
                             vulnerable to ReDoS attacks: %s"
                            pat);
                   true
               | Error () ->
                   logger#debug
                     "Failed to parse metavariable %s's value as a regexp: %s"
                     mvar re_str;
                   false
             in
             analyze_metavar env bindings mvar analyze)

and satisfies_metavar_pattern_condition env r mvar opt_xlang formula =
  let bindings = r.mvars in
  (* If anything goes wrong the default is to filter out! *)
  match List.assoc_opt mvar bindings with
  | None ->
      error env
        (Common.spf
           "metavariable-pattern failed because %s is not in scope, please \
            check your rule"
           mvar);
      false
  | Some mval -> (
      (* We will create a temporary file with the content of the metavariable,
       * then call evaluate_formula recursively. *)
      match MV.range_of_mvalue mval with
      | None ->
          error env
            (Common.spf
               "metavariable-pattern failed because we lack range info for %s, \
                please file a bug report"
               mvar);
          false
      | Some (mval_file, mval_range) -> (
          let r' =
            (* Fix the range to match the content of the temporary file. *)
            {
              r with
              r = { start = 0; end_ = mval_range.end_ - mval_range.start };
            }
          in
          match (opt_xlang, mval) with
          | None, __any_mval__ -> (
              (* We match wrt the same language as the rule.
               * NOTE: A generic pattern nested inside a generic won't work because
               *   generic mode binds metavariables to `MV.Text`, and
               *   `MV.program_of_mvalue` does not handle `MV.Text`. So one must
               *   specify `language: generic` (case `Some xlang` below). *)
              match MV.program_of_mvalue mval with
              | None ->
                  error env
                    (Common.spf
                       "metavariable-pattern failed because %s does not bind \
                        to a sub-program, please check your rule"
                       mvar);
                  false
              | Some mast ->
                  (* Note that due to symbolic propagation, `mast` may be
                   * outside of the current file/AST, so we must get
                   * `mval_range` from `mval_file` and not from `env.file`! *)
                  let content = Range.content_at_range mval_file mval_range in
                  Common2.with_tmp_file ~str:content ~ext:"mvar-pattern"
                    (fun file ->
                      (* We don't want having to re-parse `content', but then we
                       * need to fix the token locations in `mast`. *)
                      let mast_start_loc =
                        mval |> MV.ii_of_mval |> Visitor_AST.range_of_tokens
                        |> fst |> PI.unsafe_token_location_of_info
                      in
                      let fix_loc loc =
                        {
                          loc with
                          PI.charpos = loc.PI.charpos - mast_start_loc.charpos;
                          line = loc.line - mast_start_loc.line + 1;
                          column = loc.column - mast_start_loc.column;
                          file;
                        }
                      in
                      let fixing_visitor =
                        Map_AST.mk_fix_token_locations fix_loc
                      in
                      let mast' = fixing_visitor.Map_AST.vprogram mast in
                      let file_and_more =
                        {
                          env.file_and_more with
                          file;
                          lazy_ast_and_errors = lazy (mast', []);
                          lazy_content = lazy content;
                        }
                      in
                      nested_formula_has_matches { env with file_and_more }
                        formula (Some r')))
          | Some xlang, MV.Text (content, _tok)
          | Some xlang, MV.E { e = G.L (G.String (content, _tok)); _ } ->
              (* We re-parse the matched text as `xlang`. *)
              Common2.with_tmp_file ~str:content ~ext:"mvar-pattern"
                (fun file ->
                  let lazy_ast_and_errors =
                    lazy
                      (match xlang with
                      | L (lang, _) ->
                          let { Parse_target.ast; errors; _ } =
                            Parse_target
                            .parse_and_resolve_name_use_pfff_or_treesitter lang
                              file
                          in
                          (* TODO: If we wanted to report the parse errors then we should
                           * fix the parse info with Parse_info.adjust_info_wrt_base! *)
                          if errors <> [] then
                            pr2
                              (spf
                                 "rule %s: metavariable-pattern: failed to \
                                  fully parse the content of %s"
                                 (fst env.rule.Rule.id) mvar);
                          (ast, errors)
                      | LRegex
                      | LGeneric ->
                          failwith "requesting generic AST for LRegex|LGeneric")
                  in
                  let file_and_more =
                    {
                      Xtarget.file;
                      xlang;
                      lazy_ast_and_errors;
                      lazy_content = lazy content;
                    }
                  in
                  nested_formula_has_matches { env with file_and_more } formula
                    (Some r'))
          | Some _lang, mval ->
              (* This is not necessarily an error in the rule, e.g. you may be
               * matching `$STRING + ...` and then add a metavariable-pattern on
               * `$STRING`. This will only work when `$STRING` binds to some text
               * but it can naturally bind to other string expressions. *)
              logger#debug
                "metavariable-pattern failed because the content of %s is not \
                 text: %s"
                mvar (MV.show_mvalue mval);
              false))

and nested_formula_has_matches env formula opt_context =
  let res, final_ranges =
    matches_of_formula env.config env.rule env.file_and_more formula opt_context
  in
  env.errors := res.RP.errors @ !(env.errors);
  match final_ranges with
  | [] -> false
  | _ :: _ -> true

(* less: use Set instead of list? *)
and (evaluate_formula : env -> RM.t option -> S.sformula -> RM.t list) =
 fun env opt_context e ->
  match e with
  | S.Leaf (xpat, inside) ->
      let id = xpat.R.pid in
      let match_results =
        try Hashtbl.find_all env.pattern_matches id with Not_found -> []
      in
      let kind =
        match inside with
        | Some R.Inside -> RM.Inside
        | None when R.is_regexp xpat -> RM.Regexp
        | None -> RM.Plain
      in
      match_results
      |> List.map RM.match_result_to_range
      |> List.map (fun r -> { r with RM.kind })
  | S.Or xs -> xs |> List.map (evaluate_formula env opt_context) |> List.flatten
  | S.And
      {
        selector_opt;
        positives = pos;
        negatives = neg;
        conditionals = conds;
        focus;
      } -> (
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
      let posrs = List.map (evaluate_formula env opt_context) pos in
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
               | { RM.kind = Inside; _ } :: _ -> Right xs
               | _ -> Left xs)
      in
      let all_posr =
        match posrs @ posrs_inside with
        | [] -> (
            match opt_context with
            | None -> failwith "empty And; no positive terms in And"
            | Some r -> [ [ r ] ])
        | ps -> ps
      in
      match all_posr with
      | [] -> failwith "FIXME"
      | posr :: posrs ->
          let res = posr in
          let res =
            posrs
            |> List.fold_left
                 (fun acc r ->
                   RM.intersect_ranges (fst env.config) !debug_matches acc r)
                 res
          in

          (* optimization of `pattern: $X` *)
          let res = run_selector_on_ranges env selector_opt res in

          (* let's remove the negative ranges *)
          let res =
            neg
            |> List.fold_left
                 (fun acc x ->
                   RM.difference_ranges (fst env.config) acc
                     (evaluate_formula env opt_context x))
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

          let res = apply_focus_on_ranges env focus res in

          res)
  | S.Not _ -> failwith "Invalid Not; you can only negate inside an And"

and run_selector_on_ranges env selector_opt ranges =
  match (selector_opt, ranges) with
  | _, [] ->
      (* Empty context, nothing to select from. *)
      []
  | None, ranges ->
      (* Nothing to select. *)
      ranges
  | Some { S.pattern; pid; pstr; _ }, ranges ->
      (* Find matches of `pattern` *but* only inside `ranges`, this prevents
       * matching and allocations that are wasteful because they are going to
       * be thrown away later when interesecting the results. *)
      let range_filter (tok1, tok2) =
        let r = Range.range_of_token_locations tok1 tok2 in
        List.exists (fun rwm -> Range.( $<=$ ) r rwm.RM.r) ranges
      in
      let patterns = [ (pattern, None, pid, fst pstr) ] in
      let res =
        matches_of_patterns ~range_filter env.config env.file_and_more patterns
      in
      logger#info "run_selector_on_ranges: found %d matches"
        (List.length res.matches);
      res.matches
      |> List.map RM.match_result_to_range
      |> RM.intersect_ranges (fst env.config) !debug_matches ranges

and apply_focus_on_ranges env focus ranges : RM.ranges =
  let ( let* ) = Option.bind in
  let apply_focus_mvar range focus_mvar =
    let* _mvar, mval =
      List.find_opt
        (fun (mvar, _mval) -> MV.equal_mvar focus_mvar mvar)
        range.RM.mvars
    in
    let* range_loc = Visitor_AST.range_of_any_opt (MV.mvalue_to_any mval) in
    let focus_match =
      {
        PM.rule_id = fake_rule_id (-1, focus_mvar);
        PM.file = env.file_and_more.file;
        PM.range_loc;
        PM.tokens = lazy (MV.ii_of_mval mval);
        PM.env = range.mvars;
      }
    in
    let focus_range = RM.match_result_to_range focus_match in
    (* Essentially, we are intersecting the ranges. *)
    if Range.( $<=$ ) focus_range.r range.r then Some focus_range
    else if Range.( $<=$ ) range.r focus_range.r then Some range
    else None
  in
  let apply_focus init_range =
    focus
    |> List.fold_left
         (fun opt_range focus_mvar ->
           let* range = opt_range in
           apply_focus_mvar range focus_mvar)
         (Some init_range)
  in
  ranges |> List.filter_map apply_focus

and matches_of_formula config rule file_and_more formula opt_context :
    RP.rule_profiling RP.match_result * RM.ranges =
  let formula = S.formula_to_sformula formula in
  let xpatterns = xpatterns_in_formula formula in
  let res =
    matches_of_xpatterns config file_and_more xpatterns |> RP.add_rule rule
  in
  logger#trace "found %d matches" (List.length res.matches);
  (* match results per minirule id which is the same than pattern_id in
   * the formula *)
  let pattern_matches_per_id = group_matches_per_pattern_id res.matches in
  let env =
    {
      config;
      pattern_matches = pattern_matches_per_id;
      file_and_more;
      rule;
      errors = ref [];
    }
  in
  logger#trace "evaluating the formula";
  let final_ranges = evaluate_formula env opt_context formula in
  logger#trace "found %d final ranges" (List.length final_ranges);
  let res' = { res with RP.errors = res.RP.errors @ !(env.errors) } in
  (res', final_ranges)
  [@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check_rule r hook (default_config, equivs) pformula xtarget =
  let config = r.R.options ||| default_config in
  let formula = R.formula_of_pformula pformula in
  let rule_id = fst r.id in
  let res, final_ranges =
    matches_of_formula (config, equivs) r xtarget formula None
  in
  {
    RP.matches =
      final_ranges
      |> List.map (range_to_pattern_match_adjusted r)
      (* dedup similar findings (we do that also in Match_patterns.ml,
       * but different mini-rules matches can now become the same match)
       *)
      |> PM.uniq
      |> before_return (fun v ->
             v
             |> List.iter (fun (m : Pattern_match.t) ->
                    let str = spf "with rule %s" rule_id in
                    hook str m.env m.tokens));
    errors = res.errors |> List.map (error_with_rule_id rule_id);
    skipped = res.skipped;
    profiling = res.profiling;
  }

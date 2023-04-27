(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2019, 2020 r2c
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
open AST_generic
module MR = Mini_rule
module Eq = Equivalence
module PM = Pattern_match
module GG = Generic_vs_generic
module MV = Metavariable
module Flag = Flag_semgrep
module Config = Config_semgrep_t
module MG = Matching_generic

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Main matching engine behind semgrep. This module implements mainly
 * the expr/stmt/... visitor, while generic_vs_generic does the matching.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

(* This is used to let the user know which rule the engine was using when
 * a Timeout or OutOfMemory exn occured.
 *)
let (last_matched_rule : Mini_rule.t option ref) = ref None

let set_last_matched_rule rule f =
  last_matched_rule := Some rule;
  (* note that if this raise an exn, last_matched_rule will not be
   * reset to None and that's what we want!
   *)
  let res = f () in
  last_matched_rule := None;
  res

(*****************************************************************************)
(* Matchers *)
(*****************************************************************************)

let match_e_e rule a b env =
  Profiling.profile_code ("rule:" ^ rule.MR.id) (fun () ->
      set_last_matched_rule rule (fun () -> GG.m_expr_root a b env))
  [@@profiling]

let match_st_st rule a b env =
  Profiling.profile_code ("rule:" ^ rule.MR.id) (fun () ->
      set_last_matched_rule rule (fun () -> GG.m_stmt a b env))
  [@@profiling]

let match_sts_sts rule a b env =
  Profiling.profile_code ("rule:" ^ rule.MR.id) (fun () ->
      set_last_matched_rule rule (fun () ->
          (* When matching statements, we need not only to report whether
           * there is match, but also the actual statements that were matched.
           * Indeed, even if we want the implicit '...' at the end of
           * a sequence of statements pattern (AST_generic.Ss) to match all
           * the rest, we don't want to report the whole Ss as a match but just
           * the actually matched subset.
           *
           * TODO? do we need to generate unique key? we don't want
           * nested calls to m_stmts_deep to pollute our metavar? We need
           * to pass the key to m_stmts_deep?
           *)
          let env =
            match b with
            | [] -> env
            | stmt :: _ -> MG.extend_stmts_match_span stmt env
          in
          GG.m_stmts_deep ~inside:rule.MR.inside ~less_is_ok:true a b env))
  [@@profiling]

(* for unit testing *)
let match_any_any pattern e env = GG.m_any pattern e env

let match_t_t rule a b env =
  Profiling.profile_code ("rule:" ^ rule.MR.id) (fun () ->
      set_last_matched_rule rule (fun () -> GG.m_type_ a b env))
  [@@profiling]

let match_p_p rule a b env =
  Profiling.profile_code ("rule:" ^ rule.MR.id) (fun () ->
      set_last_matched_rule rule (fun () -> GG.m_pattern a b env))
  [@@profiling]

let match_partial_partial rule a b env =
  Profiling.profile_code ("rule:" ^ rule.MR.id) (fun () ->
      set_last_matched_rule rule (fun () -> GG.m_partial a b env))
  [@@profiling]

let match_at_at rule a b env =
  Profiling.profile_code ("rule:" ^ rule.MR.id) (fun () ->
      set_last_matched_rule rule (fun () -> GG.m_attribute a b env))
  [@@profiling]

let match_fld_fld rule a b env =
  Profiling.profile_code ("rule:" ^ rule.MR.id) (fun () ->
      set_last_matched_rule rule (fun () -> GG.m_field a b env))
  [@@profiling]

let match_flds_flds rule a b env =
  Profiling.profile_code ("rule:" ^ rule.MR.id) (fun () ->
      set_last_matched_rule rule (fun () -> GG.m_fields a b env))
  [@@profiling]

let match_name_name rule a b env =
  Profiling.profile_code ("rule:" ^ rule.MR.id) (fun () ->
      set_last_matched_rule rule (fun () -> GG.m_name a b env))
  [@@profiling]

let match_xml_attribute_xml_attribute rule a b env =
  Profiling.profile_code ("rule:" ^ rule.MR.id) (fun () ->
      set_last_matched_rule rule (fun () -> GG.m_xml_attr a b env))
  [@@profiling]

let match_raw_raw rule a b env =
  Profiling.profile_code ("rule:" ^ rule.MR.id) (fun () ->
      set_last_matched_rule rule (fun () -> GG.m_raw_tree a b env))
  [@@profiling]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (rule_id_of_mini_rule : Mini_rule.t -> Pattern_match.rule_id) =
 fun mr ->
  {
    PM.id = mr.Mini_rule.id;
    message = mr.Mini_rule.message;
    pattern_string = mr.Mini_rule.pattern_string;
    fix = mr.Mini_rule.fix;
    languages = mr.Mini_rule.languages;
  }

let match_rules_and_recurse lang config (file, hook, matches) rules matcher k
    any x =
  rules
  |> List.iter (fun (pattern, rule, cache) ->
         let env = MG.empty_environment cache lang config in
         let matches_with_env = matcher rule pattern x env in
         if matches_with_env <> [] then
           (* Found a match *)
           matches_with_env
           |> List.iter (fun (env : MG.tin) ->
                  let env = env.mv.full_env in
                  match AST_generic_helpers.range_of_any_opt (any x) with
                  | None ->
                      (* TODO: Report a warning to the user? *)
                      logger#error
                        "Cannot report match because we lack range info: %s"
                        (show_any (any x));
                      ()
                  | Some range_loc ->
                      let tokens =
                        lazy (AST_generic_helpers.ii_of_any (any x))
                      in
                      let rule_id = rule_id_of_mini_rule rule in
                      let pm =
                        {
                          PM.rule_id;
                          file;
                          env;
                          range_loc;
                          tokens;
                          taint_trace = None;
                          (* This will be overrided later on by the Pro engine, if this is
                             from a Pro run.
                          *)
                          engine_kind = OSS;
                        }
                      in
                      Common.push pm matches;
                      hook pm));
  (* try the rules on substatements and subexpressions *)
  k x

let must_analyze_statement_bloom_opti_failed pattern_strs
    (st : AST_generic.stmt) =
  (* if it's empty, meaning we were not able to extract any useful specific
   * identifiers or strings from the pattern, then the pattern is too general
   * and we must analyze the stmt
   *)
  match st.s_strings with
  (* No bloom filter, expected if -bloom_filter is not used *)
  | None -> true
  (* only when the Bloom_filter says No we can skip the stmt *)
  | Some strs ->
      Set_.subset pattern_strs strs
      |> Common.before_return (fun b ->
             if not b then
               logger#debug "skipping pattern on stmt %d"
                 (AST_utils.Node_ID.to_int st.s_id))

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* [range_filter] is a predicate that defines "regions of interest" when matching
 *   expressions, this is e.g. used for optimizing `pattern: $X`. Note that
 *   traversing the Generic AST is generally fairly cheap, what could be more
 *   expensive is to do the matching (due to combinatorics) and all the allocations
 *   associatiated, especially when the pattern causaes a lot of matches. This
 *   filter allows us to avoid all that expensive stuff when matching expressions
 *   unless they fall in specific regions of the code.
 *   See also docs for {!check} in Match_pattern.mli. *)
let check2 ~hook mvar_context range_filter (config, equivs) rules
    (file, lang, ast) =
  logger#trace "checking %s with %d mini rules" file (List.length rules);

  let rules =
    (* simple opti using regexps; the bloom filter opti might supersede this *)
    if !Flag.filter_irrelevant_patterns then
      Mini_rules_filter.filter_mini_rules_relevant_to_file_using_regexp rules
        lang file
    else rules
  in
  if rules = [] then []
  else
    let matches = ref [] in

    (* old: let prog = Normalize_AST.normalize (Pr ast) lang in
       * we were rewriting code, e.g., A != B was rewritten as !(A == B),
       * which enable some nice semantic matching demo where searching for
       * $X == $X would also find code written as a != a. The problem
       * is that if we don't do the same rewriting on the pattern, then
       * looking for $X != $X would not find anything anymore.
       * In any case, rewriting the source code is less necessary
       * now that we have user-defined code equivalences (see Equivalence.ml)
       * and this will also be less surprising (you can see the set of
       * equivalences in the equivalence file).
    *)
    let prog = Pr ast in

    let expr_rules = ref [] in
    let stmt_rules = ref [] in
    let stmts_rules = ref [] in
    let type_rules = ref [] in
    let pattern_rules = ref [] in
    let attribute_rules = ref [] in
    let xml_attribute_rules = ref [] in
    let fld_rules = ref [] in
    let flds_rules = ref [] in
    let partial_rules = ref [] in
    let name_rules = ref [] in
    let raw_rules = ref [] in
    rules
    |> List.iter (fun rule ->
           (* less: normalize the pattern? *)
           let any = rule.MR.pattern in
           let any = Apply_equivalences.apply equivs lang any in
           let cache =
             if !Flag.with_opt_cache then Some (Caching.Cache.create ())
             else None
           in
           (* Annotate exp, stmt, stmts patterns with the rule strings *)
           let push_with_annotation any pattern rules =
             let strs =
               if !Flag.use_bloom_filter then
                 Bloom_annotation.set_of_pattern_strings any
               else Set_.empty
             in
             Common.push (pattern, strs, rule, cache) rules
           in
           match any with
           | E pattern -> push_with_annotation any pattern expr_rules
           | S pattern -> push_with_annotation any pattern stmt_rules
           | Ss pattern -> push_with_annotation any pattern stmts_rules
           | T pattern -> Common.push (pattern, rule, cache) type_rules
           | P pattern -> Common.push (pattern, rule, cache) pattern_rules
           | At pattern -> Common.push (pattern, rule, cache) attribute_rules
           | Fld pattern -> Common.push (pattern, rule, cache) fld_rules
           | Flds pattern -> Common.push (pattern, rule, cache) flds_rules
           | Partial pattern -> Common.push (pattern, rule, cache) partial_rules
           | Name pattern -> Common.push (pattern, rule, cache) name_rules
           | Raw pattern -> Common.push (pattern, rule, cache) raw_rules
           | XmlAt pattern ->
               Common.push (pattern, rule, cache) xml_attribute_rules
           | Args _
           | Params _
           | Xmls _
           | I _
           | Str _
           | Def _
           | Dir _
           | Tk _
           | TodoK _
           | Ar _
           | Pa _
           | Tp _
           | Ta _
           | Modn _
           | Ce _
           | Cs _
           | ForOrIfComp _
           | ModDk _
           | En _
           | Dk _
           | Di _
           | Lbli _
           | Anys _
           | Pr _ ->
               failwith
                 "only expr/stmt(s)/type/pattern/annotation/field(s)/partial \
                  patterns are supported");

    let visitor =
      object (_self : 'self)
        inherit [_] Matching_visitor.matching_visitor as super

        method! visit_expr env x =
          (* this could be quite slow ... we match many sgrep patterns
           * against an expression recursively
           *)
          !expr_rules
          |> List.iter (fun (pattern, _bf, rule, cache) ->
                 match AST_generic_helpers.range_of_any_opt (E x) with
                 | None ->
                     logger#error "Skipping because we lack range info: %s"
                       (show_expr_kind x.e);
                     ()
                 | Some range_loc when range_filter range_loc ->
                     let env =
                       MG.empty_environment ~mvar_context cache lang config
                     in
                     let matches_with_env = match_e_e rule pattern x env in
                     if matches_with_env <> [] then
                       (* Found a match *)
                       matches_with_env
                       |> List.iter (fun (env : MG.tin) ->
                              let env = env.mv.full_env in
                              let tokens =
                                lazy (AST_generic_helpers.ii_of_any (E x))
                              in
                              let rule_id = rule_id_of_mini_rule rule in
                              let pm =
                                {
                                  PM.rule_id;
                                  file;
                                  env;
                                  range_loc;
                                  tokens;
                                  taint_trace = None;
                                  engine_kind = OSS;
                                }
                              in
                              Common.push pm matches;
                              hook pm)
                 | Some (start_loc, end_loc) ->
                     logger#info
                       "While matching pattern %s in file %s, we skipped \
                        expression at %d:%d-%d:%d (outside any range of \
                        interest)"
                       rule.pattern_string start_loc.pos.file start_loc.pos.line
                       start_loc.pos.column end_loc.pos.line end_loc.pos.column;
                     ());
          (* try the rules on subexpressions *)
          (* this can recurse to find nested matching inside the
           * matched code itself *)
          super#visit_expr env x

        (* mostly copy paste of expr code but with the _st functions *)
        method! visit_stmt env x =
          (* old:
           *   match_rules_and_recurse (file, hook, matches)
           *   !stmt_rules match_st_st k (fun x -> S x) x
           * but inlined to handle specially Bloom filter in stmts for now.
           *)
          let visit_stmt () =
            !stmt_rules
            |> List.iter (fun (pattern, _pattern_strs, rule, cache) ->
                   let env = MG.empty_environment cache lang config in
                   let matches_with_env = match_st_st rule pattern x env in
                   if matches_with_env <> [] then
                     (* Found a match *)
                     matches_with_env
                     |> List.iter (fun (env : MG.tin) ->
                            let env = env.mv.full_env in
                            match
                              AST_generic_helpers.range_of_any_opt (S x)
                            with
                            | None ->
                                (* TODO: Report a warning to the user? *)
                                logger#error
                                  "Cannot report match because we lack range \
                                   info: %s"
                                  (show_stmt x);
                                ()
                            | Some range_loc ->
                                let tokens =
                                  lazy (AST_generic_helpers.ii_of_any (S x))
                                in
                                let rule_id = rule_id_of_mini_rule rule in
                                let pm =
                                  {
                                    PM.rule_id;
                                    file;
                                    env;
                                    range_loc;
                                    tokens;
                                    taint_trace = None;
                                    engine_kind = OSS;
                                  }
                                in
                                Common.push pm matches;
                                hook pm));
            super#visit_stmt env x
          in
          (* If bloom_filter is not enabled, always visit the statement *)
          (* Otherwise, filter rules first *)
          if not !Flag.use_bloom_filter then visit_stmt ()
          else
            let new_stmt_rules =
              !stmt_rules
              |> List.filter (fun (_, pattern_strs, _, _cache) ->
                     must_analyze_statement_bloom_opti_failed pattern_strs x)
            in
            let new_stmts_rules =
              !stmts_rules
              |> List.filter (fun (_, pattern_strs, _, _cache) ->
                     must_analyze_statement_bloom_opti_failed pattern_strs x)
            in
            let new_expr_rules =
              !expr_rules
              |> List.filter (fun (_, pattern_strs, _, _cache) ->
                     must_analyze_statement_bloom_opti_failed pattern_strs x)
            in
            Common.save_excursion stmt_rules new_stmt_rules (fun () ->
                Common.save_excursion stmts_rules new_stmts_rules (fun () ->
                    Common.save_excursion expr_rules new_expr_rules (fun () ->
                        visit_stmt ())))

        method! v_stmts env x =
          (* this is potentially slower than what we did in Coccinelle with
           * CTL. We try every sequences. Hopefully the first statement in
           * the pattern will filter lots of sequences so we need to do
           * the heavy stuff (e.g., handling '...' between statements) rarely.
           *
           * we can't factorize with match_rules_and_recurse because we
           * do things a little bit different with the matched_statements also
           * in matches_with_env here.
           *)
          !stmts_rules
          |> List.iter (fun (pattern, _pattern_strs, rule, cache) ->
                 Profiling.profile_code "Semgrep_generic.kstmts" (fun () ->
                     let env = MG.empty_environment cache lang config in
                     let matches_with_env = match_sts_sts rule pattern x env in
                     if matches_with_env <> [] then
                       (* Found a match *)
                       matches_with_env
                       |> List.iter (fun (env : MG.tin) ->
                              let span = env.stmts_match_span in
                              match Stmts_match_span.location span with
                              | None -> () (* empty sequence or bug *)
                              | Some range_loc ->
                                  let env = env.mv.full_env in
                                  let tokens =
                                    lazy
                                      (Stmts_match_span.list_original_tokens
                                         span)
                                  in
                                  let rule_id = rule_id_of_mini_rule rule in
                                  let pm =
                                    {
                                      PM.rule_id;
                                      file;
                                      env;
                                      range_loc;
                                      tokens;
                                      taint_trace = None;
                                      engine_kind = OSS;
                                    }
                                  in
                                  Common.push pm matches;
                                  hook pm)));
          super#v_stmts env x

        method! visit_type_ env x =
          match_rules_and_recurse lang config (file, hook, matches) !type_rules
            match_t_t (super#visit_type_ env)
            (fun x -> T x)
            x

        method! visit_pattern env x =
          match_rules_and_recurse lang config (file, hook, matches)
            !pattern_rules match_p_p (super#visit_pattern env)
            (fun x -> P x)
            x

        method! visit_attribute env x =
          match_rules_and_recurse lang config (file, hook, matches)
            !attribute_rules match_at_at
            (super#visit_attribute env)
            (fun x -> At x)
            x

        method! visit_xml_attribute env x =
          match_rules_and_recurse lang config (file, hook, matches)
            !xml_attribute_rules match_xml_attribute_xml_attribute
            (super#visit_xml_attribute env)
            (fun x -> XmlAt x)
            x

        method! visit_field env x =
          match_rules_and_recurse lang config (file, hook, matches) !fld_rules
            match_fld_fld (super#visit_field env)
            (fun x -> Fld x)
            x

        method! v_fields env x =
          match_rules_and_recurse lang config (file, hook, matches) !flds_rules
            match_flds_flds (super#v_fields env)
            (fun x -> Flds x)
            x

        method! v_partial ~recurse env x =
          match_rules_and_recurse lang config (file, hook, matches)
            !partial_rules match_partial_partial
            (super#v_partial ~recurse env)
            (fun x -> Partial x)
            x

        method! visit_name env x =
          match_rules_and_recurse lang config (file, hook, matches) !name_rules
            match_name_name (super#visit_name env)
            (fun x -> Name x)
            x

        method! visit_raw_tree env x =
          match_rules_and_recurse lang config (file, hook, matches) !raw_rules
            match_raw_raw (super#visit_raw_tree env)
            (fun x -> Raw x)
            x
      end
    in
    let visitor_env =
      let vardef_assign = config.Config.vardef_assign in
      let flddef_assign = config.Config.flddef_assign in
      let attr_expr = config.Config.attr_expr in
      Matching_visitor.mk_env ~vardef_assign ~flddef_assign ~attr_expr ()
    in
    (* later: opti: dont analyze certain ASTs if they do not contain
     * certain constants that interect with the pattern?
     * But this requires to analyze the pattern to extract those
     * constants (name of function, field, etc.).
     *)
    visitor#visit_any visitor_env prog;

    !matches |> List.rev
    (* TODO: optimize uniq_by? Too slow? Use a hash?
     * Note that this may not be enough for Semgrep.ml. Indeed, we can have
     * different mini-rules matching the same code with the same metavar,
     * but in Semgrep.ml they get agglomerated under the same rule id, in
     * which case we want to dedup them.
     * old: this uniq_by was introducing regressions in semgrep!
     * See tests/rules/regression_uniq_or_ellipsis.go but it's fixed now.
     *)
    |> PM.uniq

(* TODO: cant use [@@profile] because it does not handle yet label params *)
let check ~hook ?(mvar_context = None) ?(range_filter = fun _ -> true) config a
    b =
  Profiling.profile_code "Match_patterns.check" (fun () ->
      check2 ~hook mvar_context range_filter config a b)

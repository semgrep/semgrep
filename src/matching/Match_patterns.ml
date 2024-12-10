(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2019-2023 Semgrep Inc.
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
open Fpath_.Operators
open AST_generic
module MR = Mini_rule
module Eq = Equivalence
module PM = Core_match
module GG = Generic_vs_generic
module MV = Metavariable
module Flag = Flag_semgrep
module Options = Rule_options_t
module MG = Matching_generic
module Log = Log_matching.Log

let profile_mini_rules = ref false

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Main matching engine behind semgrep. This module implements mainly
 * the expr/stmt/... visitor, while generic_vs_generic does the matching.
 *)

(* environment of stuff common to each matcher *)
type env = {
  m_env : MG.tin;
  path : Target.path;
  hook : PM.t -> unit;
  matches : PM.t Stack_.t;
  has_as_metavariable : bool;
}

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

(* This is used to let the user know which rule the engine was using when
 * a Timeout or OutOfMemory exn occured.
 *)
let (last_matched_rule : Mini_rule.t option ref) = ref None

let set_last_matched_rule (rule : Mini_rule.t) f =
  last_matched_rule := Some rule;
  (* note that if this raise an exn, last_matched_rule will not be
   * reset to None and that's what we want!
   *)
  let res =
    if !profile_mini_rules then
      Profiling.profile_code ("rule:" ^ Rule_ID.to_string rule.id) f
    else f ()
  in
  last_matched_rule := None;
  res

(*****************************************************************************)
(* Matchers *)
(*****************************************************************************)

let match_e_e rule a b env =
  set_last_matched_rule rule (fun () -> GG.m_expr_root a b env)
[@@profiling]

let match_st_st rule a b env =
  set_last_matched_rule rule (fun () -> GG.m_stmt a b env)
[@@profiling]

let match_sts_sts rule a b env =
  set_last_matched_rule rule (fun () ->
      (* When matching statements, we need not only to report whether
       * there is match, but also the actual statements that were matched.
       * Indeed, even if we want the implicit '...' at the end of
       * a sequence of statements pattern (AST_generic.Ss) to match all
       * the rest, we don't want to report the whole Ss as a match but just
       * the actually matched subset.
       *)
      let env =
        match b with
        | [] -> env
        | stmt :: _ -> MG.extend_stmts_matched stmt env
      in
      GG.m_stmts_deep ~inside:rule.MR.inside ~less_is_ok:true a b env)
[@@profiling]

(* for unit testing *)
let match_any_any pattern e env = GG.m_any pattern e env

let match_t_t rule a b env =
  set_last_matched_rule rule (fun () -> GG.m_type_ a b env)
[@@profiling]

let match_p_p rule a b env =
  set_last_matched_rule rule (fun () -> GG.m_pattern a b env)
[@@profiling]

let match_partial_partial rule a b env =
  set_last_matched_rule rule (fun () -> GG.m_partial a b env)
[@@profiling]

let match_at_at rule a b env =
  set_last_matched_rule rule (fun () -> GG.m_attribute a b env)
[@@profiling]

let match_fld_fld rule a b env =
  set_last_matched_rule rule (fun () -> GG.m_field a b env)
[@@profiling]

let match_flds_flds rule a b env =
  set_last_matched_rule rule (fun () -> GG.m_fields a b env)
[@@profiling]

let match_name_name rule a b env =
  set_last_matched_rule rule (fun () -> GG.m_name a b env)
[@@profiling]

let match_xml_attribute_xml_attribute rule a b env =
  set_last_matched_rule rule (fun () -> GG.m_xml_attr a b env)
[@@profiling]

let match_raw_raw rule a b env =
  set_last_matched_rule rule (fun () -> GG.m_raw_tree a b env)
[@@profiling]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (rule_id_of_mini_rule : Mini_rule.t -> Core_match.rule_id) =
 fun (mr : Mini_rule.t) ->
  {
    PM.id = mr.id;
    message = mr.message;
    metadata = mr.metadata;
    pattern_string = mr.pattern_string;
    fix = mr.fix;
    fix_regexp = mr.fix_regexp;
    langs = mr.langs;
  }

let match_rules_and_recurse
    ({ m_env; path; hook; matches; has_as_metavariable } : env) rules matcher k
    any x =
  rules
  |> List.iter (fun (pattern, rule) ->
         let matches_with_env = matcher rule pattern x m_env in
         matches_with_env
         |> List.iter (fun (env : MG.tin) ->
                let mv = env.mv in
                match AST_generic_helpers.range_of_any_opt (any x) with
                | None ->
                    (* TODO: Report a warning to the user? *)
                    Log.warn (fun m ->
                        m "Cannot report match because we lack range info: %s"
                          (show_any (any x)));
                    ()
                | Some range_loc ->
                    let tokens = lazy (AST_generic_helpers.ii_of_any (any x)) in
                    let rule_id = rule_id_of_mini_rule rule in
                    let pm =
                      {
                        PM.rule_id;
                        path;
                        env = mv;
                        range_loc;
                        (* as-metavariable: *)
                        ast_node =
                          (if has_as_metavariable then Some (any x) else None);
                        tokens;
                        taint_trace = None;
                        (* This will be overrided later on by the Pro engine, if this is
                           from a Pro run.
                        *)
                        engine_of_match = `OSS;
                        validation_state = `No_validator;
                        severity_override = None;
                        metadata_override = None;
                        sca_match = None;
                        (* NOTE: the global rule fix and fix_regexp in rule_id
                         * will be applied _later_, in `Autofix.ml` *)
                        fix_text = None;
                        facts = [];
                      }
                    in
                    Stack_.push pm matches;
                    hook pm));
  (* try the rules on substatements and subexpressions *)
  k x

let location_stmts stmts =
  AST_generic_helpers.range_of_any_opt (AST_generic.Ss stmts)

let list_original_tokens_stmts stmts =
  AST_generic_helpers.ii_of_any (Ss stmts) |> List.filter Tok.is_origintok

module FactCompare = struct
  type t = fact

  let compare = compare_fact
end

module FactSet = Set.Make (FactCompare)

(* we store facts in expressions so that we can filter out matches
 * based on those facts. however, matches can sometimes be a statement
 * and not an expression. as a result, we need to define the facts of
 * a statement based on its subexpressions.
 *
 * to get a safe approximation of facts that must hold within a
 * statement, we use the intersection of facts from all its subexpressions.
 * this ensures we only consider facts that are true across all parts of
 * the statement.
 *
 * for example, say we have a code snippet like this:
 *
 * if (x == 0) {
 *   // ruleid: ...
 *   for (int i = 0; i < 5; i++) {
 *     <loop body>
 *   }
 * }
 *
 * // ok: ...
 * for (int i = 0; i < 5; i++) {
 *   if (x == 0) {
 *     <loop body>
 *   }
 * }
 *
 * and we want to match all for loops with the filtering condition being
 * x == 0, we should only match the first for loop since all its
 * subexpressions (e.g. i < 5) would be annotated with the fact x == 0
 * (i.e. the intersection of its subexpressions' facts contains x == 0).
 *)
class ['self] get_facts_visitor =
  object
    inherit ['self] AST_generic.iter_no_id_info as super

    method! visit_expr (facts, is_first) expr =
      (* facts is a ref containing the intersection of facts of the
       * subexpressions that we have visited.
       *
       * when is_first is true, it means it is the first expression
       * we encounter when traversing through the subexpressions of
       * a statement. so, we set the facts ref to contain that
       * expressions's facts.
       *)
      (if !is_first then (
         facts := expr.facts;
         is_first := false)
       else
         (* since we expect the number of facts attached to an expression to be
          * small, we don't expect the conversion from lists to sets to affect
          * performance
          *)
         let base_facts = FactSet.of_list !facts in
         let expr_facts = FactSet.of_list expr.facts in
         (* finds intersection of the facts of all the expressions
          * in a statement
          *)
         let intersection = FactSet.inter base_facts expr_facts in
         facts := FactSet.elements intersection);
      super#visit_expr (facts, is_first) expr
  end

let get_facts_of_stmt stmt =
  let fact_v = new get_facts_visitor in
  let facts = ref [] in
  let is_first = ref true in
  fact_v#visit_stmt (facts, is_first) stmt;
  !facts

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* [range_filter] is a predicate that defines "regions of interest" when
 * matching expressions, this is e.g. used for optimizing `pattern: $X`.
 * Note that traversing the Generic AST is generally fairly cheap, what could
 * be more expensive is to do the matching (due to combinatorics) and all the
 * allocations associatiated, especially when the pattern causaes a lot of
 * matches. This filter allows us to avoid all that expensive stuff when
 * matching expressions unless they fall in specific regions of the code.
 * See also docs for {!check} in Match_pattern.mli.
 *)
let check ~hook ?(has_as_metavariable = false) ?mvar_context
    ?(range_filter = fun _ -> true) (config, equivs) rules
    (internal_path_to_content, origin, lang, ast) =
  Log.info (fun m ->
      m "checking %s with %d mini rules" !!internal_path_to_content
        (List.length rules));
  let rules =
    (* simple opti using regexps *)
    if !Flag.filter_irrelevant_patterns then
      Mini_rules_filter.filter_mini_rules_relevant_to_file_using_regexp rules
        lang !!internal_path_to_content
    else rules
  in
  if rules = [] then []
  else
    let matches = ref [] in
    (* Our matching environment. We can augment this with new information based on the AST,
     * but we should only need to create it once.
     *)
    let m_env = MG.environment_of_program lang config ast in

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
           (* Annotate exp, stmt, stmts patterns with the rule strings *)
           let push_with_annotation _any pattern rules =
             Stack_.push (pattern, rule) rules
           in
           match any with
           | E pattern -> push_with_annotation any pattern expr_rules
           | S pattern -> push_with_annotation any pattern stmt_rules
           | Ss pattern -> push_with_annotation any pattern stmts_rules
           | T pattern -> Stack_.push (pattern, rule) type_rules
           | P pattern -> Stack_.push (pattern, rule) pattern_rules
           | At pattern -> Stack_.push (pattern, rule) attribute_rules
           | Fld pattern -> Stack_.push (pattern, rule) fld_rules
           | Flds pattern -> Stack_.push (pattern, rule) flds_rules
           | Partial pattern -> Stack_.push (pattern, rule) partial_rules
           | Name pattern -> Stack_.push (pattern, rule) name_rules
           | Raw pattern -> Stack_.push (pattern, rule) raw_rules
           | XmlAt pattern -> Stack_.push (pattern, rule) xml_attribute_rules
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
    let path : Target.path = { internal_path_to_content; origin } in

    (* or "Match_patterns" env, which is the environment of collecting all
       stuff which is common to each `match_rules_and_recurse`
       this makes it easier to get information to each call
    *)
    let mp_env = { m_env; path; hook; matches; has_as_metavariable } in

    let visitor =
      object (_self : 'self)
        inherit [_] Matching_visitor.matching_visitor as super

        method! visit_expr env x =
          (* this could be quite slow ... we match many sgrep patterns
           * against an expression recursively
           *)
          !expr_rules
          |> List.iter (fun (pattern, rule) ->
                 match AST_generic_helpers.range_of_any_opt (E x) with
                 | None ->
                     Log.warn (fun m ->
                         m "Skipping because we lack range info: %s"
                           (show_expr_kind x.e));
                     ()
                 | Some range_loc when range_filter range_loc ->
                     let env =
                       {
                         m_env with
                         mv =
                           (match mvar_context with
                           | None -> []
                           | Some mvs -> mvs);
                       }
                     in
                     let matches_with_env = match_e_e rule pattern x env in
                     matches_with_env
                     |> List.iter (fun (env : MG.tin) ->
                            let mv = env.mv in
                            let tokens =
                              lazy (AST_generic_helpers.ii_of_any (E x))
                            in
                            let rule_id = rule_id_of_mini_rule rule in
                            let pm =
                              {
                                PM.rule_id;
                                path;
                                env = mv;
                                range_loc;
                                (* as-metavariable: *)
                                ast_node =
                                  (if has_as_metavariable then Some (E x)
                                   else None);
                                tokens;
                                taint_trace = None;
                                engine_of_match = `OSS;
                                validation_state = `No_validator;
                                severity_override = None;
                                metadata_override = None;
                                sca_match = None;
                                fix_text = None;
                                facts = x.facts;
                              }
                            in
                            Stack_.push pm matches;
                            hook pm)
                 | Some (start_loc, end_loc) ->
                     Log.debug (fun m ->
                         m
                           "While matching pattern %s in file %s, we skipped \
                            expression at %d:%d-%d:%d (outside any range of \
                            interest)"
                           rule.pattern_string !!(start_loc.pos.file)
                           start_loc.pos.line start_loc.pos.column
                           end_loc.pos.line end_loc.pos.column);
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
           * TODO: bloom filter was removed, undo this inlining?
           *)
          let visit_stmt () =
            !stmt_rules
            |> List.iter (fun (pattern, rule) ->
                   let matches_with_env = match_st_st rule pattern x m_env in
                   matches_with_env
                   |> List.iter (fun (env : MG.tin) ->
                          let mv = env.mv in
                          match AST_generic_helpers.range_of_any_opt (S x) with
                          | None ->
                              (* TODO: Report a warning to the user? *)
                              Log.warn (fun m ->
                                  m
                                    "Cannot report match because we lack range \
                                     info: %s"
                                    (show_stmt x));
                              ()
                          | Some range_loc ->
                              let tokens =
                                lazy (AST_generic_helpers.ii_of_any (S x))
                              in
                              let facts = get_facts_of_stmt x in
                              let rule_id = rule_id_of_mini_rule rule in
                              let pm =
                                {
                                  PM.rule_id;
                                  path;
                                  env = mv;
                                  range_loc;
                                  (* as-metavariable: *)
                                  ast_node =
                                    (if has_as_metavariable then Some (S x)
                                     else None);
                                  tokens;
                                  taint_trace = None;
                                  engine_of_match = `OSS;
                                  validation_state = `No_validator;
                                  severity_override = None;
                                  metadata_override = None;
                                  sca_match = None;
                                  fix_text = None;
                                  facts;
                                }
                              in
                              Stack_.push pm matches;
                              hook pm));
            super#visit_stmt env x
          in
          visit_stmt ()

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
          |> List.iter (fun (pattern, rule) ->
                 Profiling.profile_code "Semgrep_generic.kstmts" (fun () ->
                     let matches_with_env =
                       match_sts_sts rule pattern x m_env
                     in
                     matches_with_env
                     |> List.iter (fun (env : MG.tin) ->
                            let matched = env.stmts_matched in
                            match location_stmts matched with
                            | None -> () (* empty sequence or bug *)
                            | Some range_loc ->
                                let mv = env.mv in
                                let tokens =
                                  lazy (list_original_tokens_stmts matched)
                                in
                                let rule_id = rule_id_of_mini_rule rule in
                                let pm =
                                  {
                                    PM.rule_id;
                                    path;
                                    env = mv;
                                    range_loc;
                                    (* as-metavariable: *)
                                    ast_node =
                                      (if has_as_metavariable then
                                         Some (Ss matched)
                                       else None);
                                    tokens;
                                    taint_trace = None;
                                    engine_of_match = `OSS;
                                    validation_state = `No_validator;
                                    severity_override = None;
                                    metadata_override = None;
                                    sca_match = None;
                                    fix_text = None;
                                    facts = [];
                                  }
                                in
                                Stack_.push pm matches;
                                hook pm)));
          super#v_stmts env x

        method! visit_type_ env x =
          match_rules_and_recurse mp_env !type_rules match_t_t
            (super#visit_type_ env)
            (fun x -> T x)
            x

        method! visit_pattern env x =
          match_rules_and_recurse mp_env !pattern_rules match_p_p
            (super#visit_pattern env)
            (fun x -> P x)
            x

        method! visit_attribute env x =
          match_rules_and_recurse mp_env !attribute_rules match_at_at
            (super#visit_attribute env)
            (fun x -> At x)
            x

        method! visit_xml_attribute env x =
          match_rules_and_recurse mp_env !xml_attribute_rules
            match_xml_attribute_xml_attribute
            (super#visit_xml_attribute env)
            (fun x -> XmlAt x)
            x

        method! visit_field env x =
          match_rules_and_recurse mp_env !fld_rules match_fld_fld
            (super#visit_field env)
            (fun x -> Fld x)
            x

        method! v_fields env x =
          (* Copied from v_stmts.
             Essentially, we would like users to be able to write patterns which
             look like sequences of fields, and can match to fields as well.

             Consider a Python example, of a class:
             ```
             class A:
               foo()
               bar()
             ```

             If we wanted to match any time where `bar()` came after `foo()`, we could
             not match with just the pattern
             ```
             foo()
             ...
             bar()
             ````
             because the latter is an Ss, and will only match to Ss, whereas the former
             is actually a class which contains Flds.

             So if someone writes a pattern which could be interpreted as a sequence of
             fields, we allow it to match to fields.
          *)
          !stmts_rules
          |> List.iter (fun (pattern, rule) ->
                 Profiling.profile_code "Semgrep_generic.kfields" (fun () ->
                     let x = List_.map (fun (F x) -> x) x in
                     let matches_with_env =
                       match_sts_sts rule pattern x m_env
                     in
                     matches_with_env
                     |> List.iter (fun (env : MG.tin) ->
                            let matched = env.stmts_matched in
                            match location_stmts matched with
                            | None -> () (* empty sequence or bug *)
                            | Some range_loc ->
                                let mv = env.mv in
                                let tokens =
                                  lazy (list_original_tokens_stmts matched)
                                in
                                let rule_id = rule_id_of_mini_rule rule in
                                let pm =
                                  {
                                    PM.rule_id;
                                    path;
                                    env = mv;
                                    range_loc;
                                    (* as-metavariable: *)
                                    ast_node =
                                      (if has_as_metavariable then
                                         Some (Ss matched)
                                       else None);
                                    tokens;
                                    taint_trace = None;
                                    engine_of_match = `OSS;
                                    validation_state = `No_validator;
                                    severity_override = None;
                                    metadata_override = None;
                                    sca_match = None;
                                    fix_text = None;
                                    facts = [];
                                  }
                                in
                                Stack_.push pm matches;
                                hook pm)));
          match_rules_and_recurse mp_env !flds_rules match_flds_flds
            (super#v_fields env)
            (fun x -> Flds x)
            x

        method! v_partial ~recurse env x =
          match_rules_and_recurse mp_env !partial_rules match_partial_partial
            (super#v_partial ~recurse env)
            (fun x -> Partial x)
            x

        method! visit_name env x =
          match_rules_and_recurse mp_env !name_rules match_name_name
            (super#visit_name env)
            (fun x -> Name x)
            x

        method! visit_raw_tree env x =
          match_rules_and_recurse mp_env !raw_rules match_raw_raw
            (super#visit_raw_tree env)
            (fun x -> Raw x)
            x
      end
    in
    let visitor_env =
      let vardef_assign = config.Options.vardef_assign in
      let flddef_assign = config.Options.flddef_assign in
      let attr_expr = config.Options.attr_expr in
      let implicit_return = config.Options.implicit_return in
      Matching_visitor.mk_env ~vardef_assign ~flddef_assign ~attr_expr
        ~implicit_return ()
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
[@@profiling]

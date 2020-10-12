(*s: semgrep/matching/Semgrep_generic.ml *)
(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2019, 2020 r2c
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
open AST_generic

module V = Visitor_AST
module AST = AST_generic
module Err = Error_code
module PI = Parse_info
module R = Rule
module Eq = Equivalence
module Res = Match_result
module GG = Generic_vs_generic
module MV = Metavars_generic
module Flag = Flag_semgrep

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Main matching engine behind sgrep. This module implements mainly
 * the expr/stmt visitor, while generic_vs_generic does the matching.
 *
 * history: this file was split in sgrep_generic.ml for -e/-f and
 * sgrep_lint_generic.ml for -rules_file. The -e/-f returns results as
 * it goes and takes a single pattern while -rules_file applies a list
 * of patterns and return a result just at the end. We have now factorized
 * the two files because of many bugs and discrepancies between the
 * two operating modes. It was easy to forget to add a new feature in
 * one of the file. Now -rules_file and -e/-f work mostly the same.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Semgrep_generic.matcher]] *)
type ('a, 'b) matcher = 'a -> 'b ->
  Metavars_generic.metavars_binding list
(*e: type [[Semgrep_generic.matcher]] *)

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

(* This is used to let the user know which rule the engine was using when
 * a Timeout or OutOfMemory exn occured.
 *)
let (last_matched_rule: Rule.t option ref) = ref None

let set_last_matched_rule rule f =
  last_matched_rule := Some rule;
  (* note that if this raise an exn, last_matched_rule will not be
   * reset to None and that's what we want!
   *)
  let res = f() in
  last_matched_rule := None;
  res


(*****************************************************************************)
(* Matchers *)
(*****************************************************************************)

(*s: function [[Semgrep_generic.match_e_e]] *)
let match_e_e2 pattern e =
  let env = Matching_generic.empty_environment () in
  GG.m_expr pattern e env
(*e: function [[Semgrep_generic.match_e_e]] *)
let match_e_e rule a b =
     Common.profile_code ("rule:" ^ rule.R.id) (fun () ->
     set_last_matched_rule rule (fun () ->
      match_e_e2 a b))
[@@profiling]

(*s: function [[Semgrep_generic.match_st_st]] *)
let match_st_st2 pattern e =
  let env = Matching_generic.empty_environment () in
  GG.m_stmt pattern e env
(*e: function [[Semgrep_generic.match_st_st]] *)
let match_st_st rule a b =
    Common.profile_code ("rule:" ^ rule.R.id) (fun () ->
     set_last_matched_rule rule (fun () ->
      match_st_st2 a b))
[@@profiling]

(*s: function [[Semgrep_generic.match_sts_sts]] *)
let match_sts_sts2 pattern e =
  let env = Matching_generic.empty_environment () in
  (* When matching statements, we need not only to report whether
   * there is match, but also the actual statements that were matched.
   * Indeed, even if we want the implicit '...' at the end of
   * a sequence of statements pattern (AST_generic.Ss) to match all
   * the rest, we don't want to report the whole Ss as a match but just
   * the actually matched subset.
   * To do so would require to change the interface of a matcher
   * to not only return the matched environment but also the matched
   * statements. This would require in turn to provide new versions
   * for >>=, >||>, etc.
   * Instead, we can abuse the environment to also record the
   * matched statements! This is a bit ugly, but the alternative might
   * be worse.
   *
   * TODO? do we need to generate unique key? we don't want
   * nested calls to m_stmts_deep to polluate our metavar? We need
   * to pass the key to m_stmts_deep?
   *)
  let key = MV.matched_statements_special_mvar in
  let env = (key, Ss [])::env in

  let res = GG.m_stmts_deep ~less_is_ok:true pattern e env in

  res |> List.map (fun tin ->
    match List.assoc_opt key tin with
    | Some (Ss xs) ->
          (* we use List.rev because Generic_vs_generic.env_add_matched_stmt
           * adds the matched statements gradually at the beginning
           * of the list
           *)
          List.remove_assoc key tin, (Ss (List.rev xs))
    | _ -> raise Impossible
  )
(*e: function [[Semgrep_generic.match_sts_sts]] *)
let match_sts_sts rule a b =
    Common.profile_code ("rule:" ^ rule.R.id) (fun () ->
     set_last_matched_rule rule (fun () ->
      match_sts_sts2 a b))
[@@profiling]

(*s: function [[Semgrep_generic.match_any_any]] *)
(* for unit testing *)
let match_any_any pattern e =
  let env = Matching_generic.empty_environment () in
  GG.m_any pattern e env
(*e: function [[Semgrep_generic.match_any_any]] *)

let match_t_t2 pattern e =
  let env = Matching_generic.empty_environment () in
  GG.m_type_ pattern e env
let match_t_t rule a b =
    Common.profile_code ("rule:" ^ rule.R.id) (fun () ->
     set_last_matched_rule rule (fun () ->
      match_t_t2 a b))
[@@profiling]

let match_p_p2 pattern e =
  let env = Matching_generic.empty_environment () in
  GG.m_pattern pattern e env
let match_p_p rule a b =
    Common.profile_code ("rule:" ^ rule.R.id) (fun () ->
     set_last_matched_rule rule (fun () ->
      match_p_p2 a b))
[@@profiling]

let match_partial_partial2 pattern e =
  let env = Matching_generic.empty_environment () in
  GG.m_partial pattern e env
let match_partial_partial rule a b =
    Common.profile_code ("rule:" ^ rule.R.id) (fun () ->
     set_last_matched_rule rule (fun () ->
      match_partial_partial2 a b))
[@@profiling]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let match_rules_and_recurse (file, hook, matches) rules matcher k any x =
  rules |> List.iter (fun (pattern, rule) ->
    let matches_with_env = matcher rule pattern x in
    if matches_with_env <> []
    then (* Found a match *)
        matches_with_env |> List.iter (fun env ->
           Common.push { Res. rule; file; env; code = any x } matches;
           let matched_tokens = lazy (Lib_AST.ii_of_any (any x)) in
           hook env matched_tokens
        )
  );
  (* try the rules on substatements and subexpressions *)
  k x

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Semgrep_generic.check2]] *)
let check2 ~hook rules equivs file lang ast =

  let rules =
    if !Flag.filter_irrelevant_rules
    then Rules_filter.filter_rules_relevant_to_file_using_regexp
           rules lang file
    else rules
  in
  if rules = []
  then []
  else begin

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
  let prog = (Pr ast) in

  let expr_rules = ref [] in
  let stmt_rules = ref [] in
  let stmts_rules = ref [] in
  let type_rules = ref [] in
  let pattern_rules = ref [] in
  let partial_rules = ref [] in
  (*s: [[Semgrep_generic.check2()]] populate [[expr_rules]] and other *)
  rules |> List.iter (fun rule ->
    (* less: normalize the pattern? *)
    let any = rule.R.pattern in
    (*s: [[Semgrep_generic.check2()]] apply equivalences to rule pattern [[any]] *)
    let any = Apply_equivalences.apply equivs any in
    (*e: [[Semgrep_generic.check2()]] apply equivalences to rule pattern [[any]] *)
    match any with
    | E pattern  -> Common.push (pattern, rule) expr_rules
    | S pattern -> Common.push (pattern, rule) stmt_rules
    | Ss pattern -> Common.push (pattern, rule) stmts_rules
    | T pattern -> Common.push (pattern, rule) type_rules
    | P pattern -> Common.push (pattern, rule) pattern_rules
    | Partial pattern -> Common.push (pattern, rule) partial_rules
    | _ -> failwith
            "only expr/stmt/stmts/type/pattern/partial patterns are supported"
  );
  (*e: [[Semgrep_generic.check2()]] populate [[expr_rules]] and other *)

  let visitor = V.mk_visitor { V.default_visitor with
    (*s: [[Semgrep_generic.check2()]] visitor fields *)
    V.kexpr = (fun (k, _) x ->
      (* this could be quite slow ... we match many sgrep patterns
       * against an expression recursively
       *)
      !expr_rules |> List.iter (fun (pattern, rule) ->
         let matches_with_env = match_e_e rule pattern x in
         if matches_with_env <> []
         then (* Found a match *)
           matches_with_env |> List.iter (fun env ->
             Common.push { Res. rule; file; env; code = E x } matches;
             let matched_tokens = lazy (Lib_AST.ii_of_any (E x)) in
             hook env matched_tokens
         )
      );
      (* try the rules on subexpressions *)
      (* this can recurse to find nested matching inside the
       * matched code itself *)
      k x
    );
    (*x: [[Semgrep_generic.check2()]] visitor fields *)
    (* mostly copy paste of expr code but with the _st functions *)
    V.kstmt = (fun (k, _) x ->
         match_rules_and_recurse (file, hook, matches)
              !stmt_rules match_st_st k (fun x -> S x) x
    );
    (*x: [[Semgrep_generic.check2()]] visitor fields *)
    V.kstmts = (fun (k, _) x ->
      (* this is potentially slower than what we did in Coccinelle with
       * CTL. We try every sequences. Hopefully the first statement in
       * the pattern will filter lots of sequences so we need to do
       * the heavy stuff (e.g., handling '...' between statements) rarely.
       *
       * we can't factorize with match_rules_and_recurse because we
       * do things a little bit different with the matched_statements also
       * in matches_with_env here.
       *)

      !stmts_rules |> List.iter (fun (pattern, rule) ->
         let matches_with_env = match_sts_sts rule pattern x in
         if matches_with_env <> []
         then (* Found a match *)
           matches_with_env |> List.iter (fun (env, matched_statements) ->
             Common.push { Res. rule; file; env; code = matched_statements }
               matches;
             let matched_tokens = lazy (Lib_AST.ii_of_any matched_statements)
             in
             hook env matched_tokens
           )
      );
      k x
    );
    (*e: [[Semgrep_generic.check2()]] visitor fields *)

    V.ktype_ = (fun (k, _) x ->
      match_rules_and_recurse (file, hook, matches)
         !type_rules match_t_t k (fun x -> T x) x
    );
    V.kpattern = (fun (k, _) x ->
      match_rules_and_recurse (file, hook, matches)
         !pattern_rules match_p_p k (fun x -> P x) x
    );
    V.kpartial = (fun (k, _) x ->
      match_rules_and_recurse (file, hook, matches)
         !partial_rules match_partial_partial k (fun x -> Partial x) x
    );
  }
  in
  (* later: opti: dont analyze certain ASTs if they do not contain
   * certain constants that interect with the pattern?
   * But this requires to analyze the pattern to extract those
   * constants (name of function, field, etc.).
   *)
  visitor prog;

  !matches |> List.rev
  end
(*e: function [[Semgrep_generic.check2]] *)

(* TODO: cant use [@@profile] because it does not handle yet label params *)
let check ~hook a b c d e =
  Common.profile_code "Semgrep.check" (fun () -> check2 ~hook a b c d e)

(*e: semgrep/matching/Semgrep_generic.ml *)

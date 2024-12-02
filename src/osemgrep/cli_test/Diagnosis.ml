(* Brandon Wu
 *
 * Copyright (C) 2019-2024 Semgrep Inc.
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
open Fpath_.Operators
module Out = Semgrep_output_v1_j
module ME = Matching_explanation

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* We are interested in the problem of diagnosing why a match was produced or
 * was not produced, when a rule is written in an erroneous way. In particular,
 * we are interested in when a test-annotated target and rule pair have a
 * failing test.
 * It is not always obvious why a rule is matching or not matching somewhere.
 * Matching explanations get us a bit closer to the way there, but require some
 * processing -- the standard procedure is to look up and down the tree to see
 * when the desired match was killed or failed to be produced.
 * This is a simplistic enough process that it can be automated. This is the
 * goal of this module.
 *
 * The precise approach that we take is perhaps best understood by reading the
 * file from bottom-up. We decompose the problem into two sub-problems:
 * 1) Diagnosing an unexpected match
 * 2) Diagnosing an unexpected lack of match (an "unexpected no-match")
 * The algorithms that we employ in both cases are detailed in their respective
 * sections.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type diagnosis = Out.matching_diagnosis

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let indent s = "  " ^ s
let line_of_pm (pm : Core_match.t) = (fst pm.range_loc).pos.line
let hash_of_tok (tok : Rule.tok) = Tok.bytepos_of_tok tok

let rec get_focus_nodes (explanation : ME.t) : ME.t list =
  let children_results = List.concat_map get_focus_nodes explanation.children in
  match explanation.op with
  | Filter "metavariable-focus" -> explanation :: children_results
  | _ -> children_results

(*****************************************************************************)
(* Code snippets *)
(*****************************************************************************)

let count_indentation (chars : char list) =
  let either =
    List.fold_left
      (fun acc char ->
        match (acc, char) with
        | Either.Right _, _ -> acc
        | Left count, ' ' -> Left (count + 1)
        (* not robust but who cares *)
        | Left count, '\t' -> Left (count + 4)
        | Left count, _ -> Right count)
      (Left 0) chars
  in
  match either with
  | Left count -> count
  | Right count -> count

(* hack hack hack but i don't care care care *)
(* this wouldn't need to happen if we had real positions for the patterns,
   but unfortunately we do not.
   we are only holding the `Pos` of the first token in the pattern operator,
   which might span several lines. this means we cannot know where it ends.
   additionally, the `Rule.t` doesn't have this information either, we
   throw it away when translating from the Generic AST.
   so, for simplicity, we just hack our way through now.
*)
let pattern_text_of_pos (line : int) (file : Fpath.t) : string =
  (* TODO: put line column *)
  let relevant_lines = UFile.cat file |> List_.drop (line - 1) in
  let indentation_at_line (line : string) =
    line |> String.to_seq |> List.of_seq |> count_indentation
  in
  let indentation_at_pos_line =
    indentation_at_line (List.nth relevant_lines 0)
  in
  Logs.debug (fun m ->
      m "indentation at line %d in file %s: %d" line (Fpath.to_string file)
        indentation_at_pos_line);
  let same_indented_lines, _rest =
    match relevant_lines with
    | [] -> failwith "how"
    | line :: rest_lines ->
        let lines, skipped =
          rest_lines
          |> List_.span (fun line ->
                 (* this is also technically wrong, we should consider
                      a: foo
                      b: bar
                    as one entity, but
                      - foo
                      - bar
                    as two entities
                    they have the same indentation though
                 *)
                 (* bad bad bad bad
                    we do this because if the pattern starts on a hyphen,
                    we don't want to take the rest of the stuff in the list,
                    as they are not part of the pattern we're concerned with
                 *)
                 (not
                    (String.contains (List.nth relevant_lines 0) '-'
                    && String.contains line '-'))
                 && indentation_at_line line >= indentation_at_pos_line)
        in
        (line :: lines, skipped)
  in
  same_indented_lines |> String.concat "\n"

(*****************************************************************************)
(* Explanation manipulation *)
(*****************************************************************************)

(* A brief recap on how matching explanations work is warranted.
   From the type of `Matching_explanation.t`, we see that matching explanations
   form an n-ary tree, which is labeled by the operation of the node. These
   closely reflect the structure of a `Rule.formula`, with the exception of that
   we have nodes for `Filter` (encompassing `metavariable-comparison`,
   `focus-metavariable`, etc), which are themselves first-class nodes of the tree
   (e.g. they can be children of another node).

   The way that we translate a rule like this:
   all:
     - pattern: A
     - pattern-not: B
   where:
     - metavariable: A
       comparison: C
     - focus: $X

   is that it produces the following explanation tree:
                   And
        /      /        \       \
   Xpat (A) Negation  Filter  Filter

   where the latter two filters correspond to the `comparison` and `focus`,
   respectively.

   - The `matches` stored at `A` will be just the matches of `A`.
   - The matches stored at the `Negation` will be the _matches of `A` that are
     not `B`_, ergo the post-intersection matches of the `all`.
   - The matches stored at the first `Filter` will be the previous bullet point,
     but only the ones which survive the filtering of the comparison of `C`.
   - The matches stored at the second `Filter` will be the result of focusing on
     $X in all of the matches from the previous bullet point,

   This is important to understand, because it will inform how we implement the
   `traverse` function below.
*)

(* "positive" node, meaning one of the traditional six pattern operators that
   may introduce matches
*)
let is_positive_node (node : ME.t) : bool =
  match node.op with
  | Out.Inside
  | XPat _
  | Anywhere
  | Or
  | And ->
      true
  | Negation
  | Filter _ ->
      false
  | Taint
  | TaintSource
  | TaintSink
  | TaintSanitizer
  | EllipsisAndStmts
  | ClassHeaderAndElems ->
      true

let out_matches_of_node (node : ME.t) = node.matches

(* `traverse` calls `f` on the in-match and out-match pairs at every single
   positive node in the matching explanation tree, plus every negative node that
   occurs directly underneath a positive node
   in practice, these are all of the nodes which have a role in the way that the
   "positive" matches are killed during match evaluation

   CAVEAT: When called upon a node with "negative children" (including Filter),
   the "before" and "after" reflect the way that formula evaluation happens:
   1. evaluate all positive children
   2. combine them with the root operation
   3. remove all negative children (Negation or Filter)
   the "after" of this node is not the output of step 3, but the output of step 2.
   Essentially, the node is traversed for just its own effect on the matches,
   not bundled with the Filter or Negations below it, which are considere differently.
   This is so we can more precisely ascribe blame for killing a match to a particular
   filter or negation, rather than the node above it.
*)
let rec traverse ~f (explanation : ME.t) =
  match explanation.op with
  | Or
  | Inside
  | Anywhere
  | And ->
      let positive, negative_and_filters =
        List.partition is_positive_node explanation.children
      in
      (* this case looks like:
                  P
           /   /    \     \
          P1 P_n    N1 ... N_m

         where P are "positive" and N are "negative", which means
         either Negation or Filter

         there may not actually be any negative nodes

         notably, the P1 through P_n are positive nodes which may have children which must
         be recursively traversed, but we do not need to do this for the negative nodes,
         because we are only interested in "positive" matches
      *)
      (* positive nodes *)
      List.iter (traverse ~f) positive;
      (* We are interested in the matches outputted from the root operation
         purely, because we would like to consider that seperately from each
         negation step which occurs afterwards
         We stored this information in `before_negation_matches` earlier for
         And nodes, and otherwise it is just the root matches.
      *)
      (match (explanation.op, explanation.extra) with
      | And, Some { before_negation_matches = Some matches; _ }
      | _, Some { before_filter_matches = Some matches; _ } ->
          let before_negation_matches = matches in
          (* main node operation *)
          f
            ~before:(List.concat_map out_matches_of_node positive)
            ~after:before_negation_matches explanation;
          (* negative node and filter nodes *)
          List.fold_left
            (fun (prior_matches : Core_match.t list) (child : ME.t) ->
              f ~before:prior_matches ~after:child.matches child;
              child.matches)
            before_negation_matches negative_and_filters
          |> ignore
      (* no filtering or negation case *)
      | _ ->
          f
            ~before:(List.concat_map out_matches_of_node positive)
            ~after:explanation.matches explanation);
      ()
  | XPat _s ->
      assert (List.length explanation.children =*= 0);
      f ~before:[] ~after:explanation.matches explanation
  (* by precondition, this should not occur, because we do not call `traverse` on the
     non-positive nodes
  *)
  | Negation
  | Filter _ ->
      raise Impossible
  | Taint
  | TaintSource
  | TaintSink
  | TaintSanitizer
  | EllipsisAndStmts
  | ClassHeaderAndElems ->
      List.iter (traverse ~f) explanation.children

(*****************************************************************************)
(* Unexpected matches *)
(*****************************************************************************)

(* NOTE(unexpected-matches): If a match is unexpected, then it is a fact that it either:
   1) must have originated from an xpattern
   2) must have originated from a focus on a metavariable, which
      could happen from another match.

   Reasons for why it survived may be that:
   1) it wasn't killed by a filter, negation, inside condition, or intersection

   So our algorithm will be:
   1) Look at all of the xpatterns matches. Determine which are on the unexpected line.
   2) Look at all the instances of a `focus`, and check if its children do NOT have a
      match on the relevant line, but the focus does.
   2) Look at the trace of the match up through the formula tree, and note each time
      that it encounters one of the kill conditions above. At each point, this is a
      potential point where the rule-writer failed to kill it off.
      NOTE: There may be many times that the match encounters a filter that simply
      wasn't meant for it, and it doesn't make sense to attribute blame there. But,
      determining if a filter is "relevant" is going to be extremely difficult, so
      we must take note of each site.
*)

(* This is a complex problem, because parent relationship is actually not necessarily
   strictly parents.
   This is primarily because `patterns` defines an evaluation order which is no
   strictly hierarchical.
   That... kind of sucks, actually.
*)
let rec nodes_after_another ~(target : ME.t) (node : ME.t) =
  if hash_of_tok target.pos =*= hash_of_tok node.pos then Some []
  else
    let path =
      List_.fold_right
        (fun child acc ->
          match acc with
          | Some path -> Some path
          | None -> nodes_after_another ~target child)
        node.children None
    in
    match (path, node.op) with
    | None, _ -> None
    | Some path, Out.And ->
        (* TODO: wrong order? *)
        let negations =
          List.filter
            (fun (child : ME.t) -> child.op =*= Negation)
            node.children
        in
        let filters =
          List.filter
            (fun (child : ME.t) ->
              match child.op with
              | Filter _ -> true
              | _ -> false)
            node.children
        in
        Logs.debug (fun m ->
            m "%d negations, %d filters" (List.length negations)
              (List.length filters));
        Some ((node :: filters) @ negations @ path)
    | Some path, _ -> Some (node :: path)

(* A killing node is one which has the power to remove matches that
   are incoming to it.
   Technically, Inside is not one which has the power to remove
   matches incoming to it, but it has the power to enforce a condition
   causing a later node to remove matches incoming to it (And).
*)
let is_killing_node (node : ME.t) =
  match node.op with
  | And -> Some `And
  | Inside -> Some `Inside
  | Negation -> Some `Negation
  | Filter s -> Some (`Filter s)
  | XPat _
  | Anywhere
  | Or ->
      None
  (* TODO: taint*)
  | Taint
  | TaintSource
  | TaintSink
  | TaintSanitizer
  | EllipsisAndStmts
  | ClassHeaderAndElems ->
      None

let get_unexpected_matching_diagnosis ~target_file ~rule_file ~matched_line
    ~kind (node : ME.t) (explanations : ME.t list) :
    Out.unexpected_match_diagnosis =
  let parents =
    (* Find the first of our explanations such that it has nodes above
       our target node.
       I say the "first" of our explanations, but by construction there
       should be one, and only one.
    *)
    List.find_map
      (fun expl ->
        let res = nodes_after_another ~target:node expl in
        Logs.debug (fun m -> m "has %d parents " (List.length (Option.get res)));
        res)
      explanations
    |> Option.get |> List.rev
    |> List_.filter_map (fun (node : ME.t) ->
           match is_killing_node node with
           | None -> None
           | Some kind -> Some (node, kind))
  in
  let killing_parents =
    List_.filter_map
      (fun ((parent : ME.t), kind) ->
        match Tok.loc_of_tok parent.pos with
        | Error _ -> None
        | Ok loc ->
            let parent_text = pattern_text_of_pos loc.pos.line rule_file in
            Some
              {
                Out.killing_parent_kind = kind;
                snippet = { Out.line = loc.pos.line; text = parent_text };
              })
      parents
  in
  let match_lines =
    (* TODO: more context? *)
    UFile.cat target_file |> List_.index_list_1
    |> List.filter (fun (_line, idx) -> idx =*= matched_line)
    |> List_.map fst |> List_.map indent |> String.concat "\n"
  in
  let originating_text =
    let originating_line = (Tok.unsafe_loc_of_tok node.pos).pos.line in
    {
      Out.line = originating_line;
      text = pattern_text_of_pos originating_line rule_file;
    }
  in
  {
    Out.matched_text = { line = matched_line; text = match_lines };
    originating_kind = kind;
    originating_text;
    killing_parents;
  }

let get_introducing_nodes ~(matched_line : int) (explanations : ME.t list) =
  let results = ref [] in
  List.iter
    (fun (node : ME.t) ->
      traverse
        ~f:(fun ~before ~after (node : ME.t) ->
          let introduced_match =
            (not
               (List.exists (fun pm -> line_of_pm pm =*= matched_line) before))
            && List.exists (fun pm -> line_of_pm pm =*= matched_line) after
          in
          let kind =
            if node.op =*= Filter "metavariable-focus" then `Focus
            else `Xpattern
          in
          if introduced_match then results := (node, kind) :: !results;
          ())
        node)
    explanations;
  !results

(* see NOTE(unexpected-matches) *)
let diagnose_unexpected_match ~target_file ~rule_file ~(matched_line : int)
    (explanations : ME.t list) : Out.unexpected_match_diagnosis list =
  Logs.debug (fun m ->
      m "Explanation is %s" (Common2.string_of_list ME.show explanations));
  get_introducing_nodes ~matched_line explanations
  |> List_.map (fun (node, kind) ->
         get_unexpected_matching_diagnosis ~target_file ~rule_file ~matched_line
           ~kind node explanations)

(*****************************************************************************)
(* Unexpected no matches *)
(*****************************************************************************)

let get_killing_nodes ~(line : int) (explanations : ME.t list) : (ME.t * _) list
    =
  let results = ref [] in
  List.iter
    (fun (node : ME.t) ->
      traverse
        ~f:(fun ~before ~after node ->
          let killed_match =
            List.exists (fun pm -> line_of_pm pm =*= line) before
            && not (List.exists (fun pm -> line_of_pm pm =*= line) after)
          in
          let kind = is_killing_node node in
          if killed_match then results := (node, kind) :: !results;
          ())
        node)
    explanations;
  !results

(* NOTE(unexpected-no-matches): If a no-match was unexpected, then it is a fact
   that it either:
   1) it originated from no xpatterns or focuses
   2) it did originate from one of those, but it was killed along the way
      by a filter, negation, inside condition, or intersection

   So our algorithm will be to:
   1) check all xpatterns and focuses to see if they introduced a match at the
      line in question. If they didn't, then the rule-writer simply failed to
      match on the line in the first place.
   2) examine the in-matches and out-matches of each killing operator.
      eventually, we should find one which killed the match. isolate that one
      and suggest that it may be responsible.
*)
let diagnose_unexpected_no_match ~rule_file ~(unmatched_line : int)
    (explanations : ME.t list) : Out.unexpected_no_match_diagnosis =
  let xpattern_and_focus_nodes_that_introduced_match =
    get_introducing_nodes ~matched_line:unmatched_line explanations
  in
  let kind =
    if List.length xpattern_and_focus_nodes_that_introduced_match =*= 0 then
      `Never_matched
    else
      (* there may be multiple killing nodes, because we could have originated
         the match from multiple different sources, then killed them all
      *)
      let killing_nodes = get_killing_nodes ~line:unmatched_line explanations in
      `Killed_by_nodes
        (List_.filter_map
           (fun ((parent : ME.t), kind) ->
             match (Tok.loc_of_tok parent.pos, kind) with
             | Error _, _ -> None
             | _, None -> None
             | Ok loc, Some kind ->
                 let parent_text = pattern_text_of_pos loc.pos.line rule_file in
                 Some
                   {
                     Out.killing_parent_kind = kind;
                     snippet = { Out.line = loc.pos.line; text = parent_text };
                   })
           killing_nodes)
  in
  { line = unmatched_line; kind }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let diagnose_single_file ~target ~rule_file (report : Out.expected_reported)
    (explanations : ME.t list) : Out.matching_diagnosis =
  (* NOTE: this is not the only cae for a `rule_result`. sometimes, we run the
     rule against many files (such as in `Match_search_mode`).
     but, when running tests, we only run against one file.
  *)
  let _common, only_in_expected, only_in_actual =
    Common2.diff_set_eff report.expected_lines report.reported_lines
  in
  Logs.debug (fun m ->
      m "only in actual: [%s]"
        (String.concat ", " (List_.map string_of_int only_in_actual)));
  Logs.debug (fun m ->
      m "only in expected: [%s]"
        (String.concat ", " (List_.map string_of_int only_in_expected)));
  let unexpected_match_diagnoses =
    List.concat_map
      (fun line ->
        diagnose_unexpected_match ~target_file:target ~rule_file
          ~matched_line:line explanations)
      only_in_actual
  in
  let unexpected_no_match_diagnoses =
    List_.map
      (fun line ->
        diagnose_unexpected_no_match ~rule_file ~unmatched_line:line
          explanations)
      only_in_expected
  in
  { target; unexpected_match_diagnoses; unexpected_no_match_diagnoses }

let report_unexpected_match ~target_file ~rule_file
    {
      Out.matched_text = { line = matched_line; text = matched_snippet };
      originating_kind;
      originating_text = { line = originating_line; text = originating_text };
      killing_parents;
    } =
  let report_snippet snippet =
    snippet |> String.split_on_char '\n' |> List_.map indent
  in
  let text =
    let header_text =
      Common.spf "%s: Unexpected match at line %d:"
        (Fpath.to_string target_file)
        matched_line
      :: report_snippet matched_snippet
    in
    let originating_lines =
      let reason =
        match originating_kind with
        | `Focus ->
            spf
              "This match originated from this focus-metavariable at line %d \
               in %s:"
              originating_line !!rule_file
        | `Xpattern ->
            spf "This match originated at this pattern at line %d in %s:"
              originating_line !!rule_file
      in
      reason :: report_snippet originating_text
    in
    let parent_lines =
      match killing_parents with
      | [] ->
          [
            "Maybe try inserting a filter like `metavariable-pattern`, or use \
             a `patterns` to constrain the match.";
          ]
      | _ ->
          let parent_report =
            List.concat_map
              (fun ({ snippet = { Out.line; text }; killing_parent_kind = _ } :
                     Out.killing_parent) ->
                Common.spf "line %d:" line :: report_snippet text)
              killing_parents
          in
          "Maybe the match was supposed to be filtered out by one of these \
           patterns?"
          :: List_.map indent parent_report
    in
    List_.flatten [ header_text; originating_lines; parent_lines ]
  in
  String.concat "\n" text

let report_unexpected_no_match ~target_file ~rule_file
    ({ line; kind } : Out.unexpected_no_match_diagnosis) =
  let report_snippet snippet =
    snippet |> String.split_on_char '\n' |> List_.map indent
  in
  let text =
    let header_text =
      [
        Common.spf "%s: Unexpected lack of match at line %d"
          (Fpath.to_string target_file)
          line;
      ]
    in
    let kind_text =
      match kind with
      | `Never_matched ->
          [
            "This line was never matched by any base pattern, nor introduced \
             by any `focus-metavariable`.";
            "Consider rewriting your rule to match this line.";
          ]
      | `Killed_by_nodes killing_parents ->
          List.concat_map
            (fun ({
                    snippet = { Out.line = pattern_line; text };
                    killing_parent_kind = _;
                  } :
                   Out.killing_parent) ->
              Common.spf
                "A potential match at line %d was removed by this pattern at \
                 line %d in %s:"
                line pattern_line !!rule_file
              :: report_snippet text)
            killing_parents
    in
    List_.flatten [ header_text; kind_text ]
  in
  String.concat "\n" text

let report ~rule_file
    ({ target; unexpected_match_diagnoses; unexpected_no_match_diagnoses } :
      diagnosis) : string =
  let unexpected_matches =
    List_.map
      (report_unexpected_match ~target_file:target ~rule_file)
      unexpected_match_diagnoses
  in
  let unexpected_no_matches =
    List_.map
      (report_unexpected_no_match ~target_file:target ~rule_file)
      unexpected_no_match_diagnoses
  in
  String.concat "\n"
    (List_.flatten [ unexpected_matches; unexpected_no_matches ])

let diagnose ~target ~rule_file (report : Out.expected_reported)
    (explanations : ME.t list) : diagnosis =
  diagnose_single_file ~target ~rule_file report explanations

(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2019 r2c
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
open Common
open Ast_generic
module V = Visitor_ast
module E = Error_code
module PI = Parse_info
module R = Rule

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * The goal of this module is to make it easy to add lint rules
 * by using sgrep patterns (see https://github.com/facebook/pfff/wiki/Sgrep).
 * One has just to store in a special file the patterns and the corresponding
 * warning we want the linter to raise. Here is an example of such a file:
 *
 *    //security.sgrep_lint:
 *
 *    - HTML(render_link(...))
 *
 *    ERROR: <ui:link> does that!
 *
 *    - curl_setopt(X, CURLOPT_POSTFIELDS, Y)

 *    WARNING: Please use either curl_set_post_fields() or
 *    curl_set_post_fields_with_magic_file_upload_UNSAFE().
 *
 *    - HTML($Y->toString())
 *
 *    WARNING: If the output of toString() is passed directly to HTML(),
 *    you can probably remove both the toString() and HTML() calls.
 *
 *
 * TODO:
 *  - implement the '...' operator so can have patterns like:
 *     - $X = Y->toString(); ... HTML($X)
 *  - could also as erling suggested use a dataflow analysis to magically
 *    handle such cases.
 *  - all those things about the-order-of-the-rule-matters is maybe too
 *    complicated. Maybe we should just extend sgrep to allow difference
 *    patterns ? At the same time having rules like '- HTML($X)' and then
 *    at the very end the default fallback '- HTML(...)' is convenient.
 * - right now when a pattern involve variable or arrays one has to
 *   write a rule for the lvalue case and one for the rvalue cause
 *   (and even more when one wants also to match $val += ..., $val -= ...).
 *
 * related:
 * - http://tv.jetbrains.net/videocontent/intellij-idea-static-analysis-custom-rules-with-structural-search-replace
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let string_of_id id =
  spf "%s" id

let error matched_tokens rule =
  let tok = List.hd matched_tokens in
  match rule.R.severity with
  | R.Error ->
      E.error tok (E.SgrepLint (string_of_id rule.R.id, rule.R.message))
  | R.Warning ->
      E.warning tok (E.SgrepLint (string_of_id rule.R.id, rule.R.message))
  | R.Ok -> ()


(*****************************************************************************)
(* Checking *)
(*****************************************************************************)

let check2 rules ast =

  (* This is similar to what we do in main_sgrep.ml except we apply
   * a list of sgrep patterns to the AST.
   *)

  let stmt_rules = ref [] in
  let expr_rules = ref [] in
  (* we use List.rev here because we push2 below and we want the
   * first rule in the files to be also the first rules in the x_rules
   * lists for the 'OK' to work (see the comment above about OK).
   *)
  List.rev rules |> List.iter (fun rule ->
    match rule.R.pattern with
    | E pattern  -> Common.push (pattern, rule) expr_rules
    | S pattern -> Common.push (pattern, rule) stmt_rules
    | _ -> failwith "only expr and stmt patterns supported for now"
  );

  let visitor = V.mk_visitor { V.default_visitor with
    V.kexpr = (fun (k, _) expr ->
      (* this could be quite slow ... we match many sgrep patterns
       * against an expression recursively
       *)
      let rec apply_rules rules =
        match rules with
        | [] ->
            (* no more rules, try the rules on subexpressions *)
            k expr
        | (pattern, rule)::xs ->
            let matches_with_env = Sgrep_generic.match_e_e pattern expr in
            if matches_with_env = []
            then
              (* Try another rule *)
              apply_rules xs
            else
              (* Found a match, we can stop everything. We could also
               * recurse to find nested matching inside the matched code
               * itself but we already found an error so we are
               * happy enough.
               *)
              let matched_tokens = Lib_ast.ii_of_any (E expr) in
              error matched_tokens rule
      in
      apply_rules !expr_rules
    );

    (* mostly copy paste of expr code but with the _st functions *)
    V.kstmt = (fun (k, _) stmt ->
      let rec apply_rules rules =
        match rules with
        | [] ->
            k stmt
        | (pattern, rule)::xs ->
            let matches_with_env = Sgrep_generic.match_st_st pattern stmt in
            if matches_with_env = []
            then apply_rules xs
            else
              let matched_tokens = Lib_ast.ii_of_any (S stmt) in
               error matched_tokens rule
      in
      apply_rules !stmt_rules
    );
  }
  in
  visitor (Pr ast);
  ()

let check rules a =
  Common.profile_code "Sgrep_lint.check" (fun () -> check2 rules a)

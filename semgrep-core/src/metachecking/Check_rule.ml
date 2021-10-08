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
module FT = File_type
open Rule
module R = Rule
module E = Semgrep_error_code
module PI = Parse_info

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Checking the checker (metachecking).
 *
 * The goal of this module is to detect bugs, performance issues, or
 * to suggest the use of certain features in semgrep rules.
 *
 * See also semgrep-rules/meta/ for semgrep meta rules expressed in
 * semgrep itself!
 *
 * When to use semgrep/yaml (or spacegrep) to express meta rules and
 * when to use OCaml (this file)?
 * When you need to express meta rules on the yaml structure,
 * then semgrep/yaml is fine, but sometimes you need to express
 * meta-rules by inspecting the pattern content, in which case
 * you have to use OCaml (a bit like in templating languages).
 *
 * TODO rules:
 *  - detect if scope of metavariable-regexp is wrong and should be put
 *    in a AND with the relevant pattern. If used with an AND of OR,
 *    make sure all ORs define the metavar.
 *    see https://github.com/returntocorp/semgrep/issues/2664
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = { r : Rule.t; errors : E.error list ref }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error env t s =
  let loc = Parse_info.unsafe_token_location_of_info t in
  let check_id = "semgrep-metacheck-builtin" in
  let rule_id, _ = env.r.id in
  let err =
    E.mk_error ~rule_id:(Some rule_id) loc s (E.SemgrepMatchFound check_id)
  in
  Common.push err env.errors

(*****************************************************************************)
(* Formula *)
(*****************************************************************************)

let equal_formula x y = AST_utils.with_structural_equal R.equal_formula x y

let check_formula env lang f =
  (* check duplicated patterns, essentially:
   *  $K: $PAT
   *  ...
   *  $K2: $PAT
   * but at the same level!
   *
   * See also now semgrep-rules/meta/identical_pattern.sgrep :)
   *)
  let rec find_dupe f =
    match f with
    | P _ -> ()
    | Not (_, f) -> find_dupe f
    | Or (t, xs)
    | And (t, xs, _) ->
        let rec aux xs =
          match xs with
          | [] -> ()
          | x :: xs ->
              (* todo: for Pat, we could also check if exist PatNot
               * in which case intersection will always be empty
               *)
              xs
              |> List.iter (fun y ->
                     if equal_formula x y then
                       let tx, ty = (R.tok_of_formula x, R.tok_of_formula y) in
                       let kind = R.kind_of_formula x in
                       error env ty
                         (spf "Duplicate %s of %s at line %d" kind kind
                            (PI.line_of_info tx)));
              xs
              |> List.iter (fun y ->
                     if equal_formula (Not (t, x)) y then
                       let tx, ty = (R.tok_of_formula x, R.tok_of_formula y) in
                       let kind = R.kind_of_formula x in
                       error env ty
                         (spf "Unsatisfiable formula with %s at line %d" kind
                            (PI.line_of_info tx)));
              aux xs
        in
        (* breadth *)
        aux xs;
        (* depth *)
        xs |> List.iter find_dupe
  in
  find_dupe f;

  (* call Check_pattern subchecker *)
  f
  |> visit_new_formula (fun { pat; pstr = _pat_str; pid = _ } ->
         match (pat, lang) with
         | Sem (semgrep_pat, _lang), L (lang, _rest) ->
             Check_pattern.check lang semgrep_pat
         | Spacegrep _spacegrep_pat, LGeneric -> ()
         | Regexp _, _ -> ()
         | _ -> raise Impossible);
  List.rev !(env.errors)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let check r =
  (* less: maybe we could also have formula_old specific checks *)
  match r.mode with
  | Rule.Search pf ->
      let f = Rule.formula_of_pformula pf in
      check_formula { r; errors = ref [] } r.languages f
  | Taint _ -> (* TODO *) []

(* We parse the parsing function fparser (Parser_rule.parse) to avoid
 * circular dependencies.
 * Similar to Test_parsing.test_parse_rules.
 *)
let check_files fparser xs =
  let fullxs, skipped_paths =
    xs
    |> File_type.files_of_dirs_or_files (function
         | FT.Config (FT.Yaml (*FT.Json |*) | FT.Jsonnet) -> true
         | _ -> false)
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in
  let fullxs, more_skipped_paths =
    List.partition (fun file -> not (file =~ ".*\\.test\\.yaml")) fullxs
  in
  let _skipped_paths = more_skipped_paths @ skipped_paths in
  if fullxs = [] then logger#error "no rules to check";
  fullxs
  |> List.iter (fun file ->
         logger#info "processing %s" file;
         try
           let rs = fparser file in
           rs
           |> List.iter (fun file ->
                  let errs = check file in
                  errs |> List.iter (fun err -> pr2 (E.string_of_error err)))
         with exn -> pr2 (E.string_of_error (E.exn_to_error file exn)))

let stat_files fparser xs =
  let fullxs, _skipped_paths =
    xs
    |> File_type.files_of_dirs_or_files (function
         | FT.Config (FT.Yaml (*FT.Json |*) | FT.Jsonnet) -> true
         | _ -> false)
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in
  let good = ref 0 in
  let bad = ref 0 in
  fullxs
  |> List.iter (fun file ->
         logger#info "processing %s" file;
         let rs = fparser file in
         rs
         |> List.iter (fun r ->
                let res = Analyze_rule.regexp_prefilter_of_rule r in
                match res with
                | None ->
                    incr bad;
                    pr2
                      (spf "PB: no regexp prefilter for rule %s:%s" file
                         (fst r.id))
                | Some (s, _f) ->
                    incr good;
                    pr2 (spf "regexp: %s" s)));
  pr2 (spf "good = %d, no regexp found = %d" !good !bad)

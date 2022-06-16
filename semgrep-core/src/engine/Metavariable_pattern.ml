(* Iago Abal
 *
 * Copyright (C) 2021-2022 r2c
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
open Match_env
module MV = Metavariable
module RM = Range_with_metavars
module G = AST_generic

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let adjust_content_for_language (xlang : Xlang.t) (content : string) : string =
  match xlang with
  | Xlang.L (Lang.Php, _) -> "<?php " ^ content
  | _ -> content

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let satisfies_metavar_pattern_condition nested_formula_has_matches env r mvar
    opt_xlang formula =
  let bindings = r.RM.mvars in
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
                  Xpattern_matcher.with_tmp_file ~str:content
                    ~ext:"mvar-pattern" (fun file ->
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
                      let xtarget =
                        {
                          env.xtarget with
                          file;
                          lazy_ast_and_errors = lazy (mast', []);
                          lazy_content = lazy content;
                        }
                      in
                      nested_formula_has_matches { env with xtarget } formula
                        (Some r')))
          | Some xlang, MV.Text (content, _tok)
          | Some xlang, MV.Xmls [ XmlText (content, _tok) ]
          | Some xlang, MV.E { e = G.L (G.String (content, _tok)); _ } ->
              let content = adjust_content_for_language xlang content in
              logger#debug "nested analysis of |||%s||| with lang '%s'" content
                (Xlang.to_string xlang);
              (* We re-parse the matched text as `xlang`. *)
              Xpattern_matcher.with_tmp_file ~str:content ~ext:"mvar-pattern"
                (fun file ->
                  let lazy_ast_and_errors =
                    lazy
                      (match xlang with
                      | L (lang, _) ->
                          let { Parse_target.ast; skipped_tokens; _ } =
                            Parse_target.parse_and_resolve_name lang file
                          in
                          (* TODO: If we wanted to report the parse errors
                           * then we should fix the parse info with
                           * Parse_info.adjust_info_wrt_base! *)
                          if skipped_tokens <> [] then
                            pr2
                              (spf
                                 "rule %s: metavariable-pattern: failed to \
                                  fully parse the content of %s"
                                 (fst env.rule.Rule.id) mvar);
                          (ast, skipped_tokens)
                      | LRegex
                      | LGeneric ->
                          failwith "requesting generic AST for LRegex|LGeneric")
                  in
                  let xtarget =
                    {
                      Xtarget.file;
                      xlang;
                      lazy_ast_and_errors;
                      lazy_content = lazy content;
                    }
                  in
                  nested_formula_has_matches { env with xtarget } formula
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

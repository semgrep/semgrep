(* Iago Abal
 *
 * Copyright (C) 2021-2022 Semgrep Inc.
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
open Match_env
module MV = Metavariable
module RM = Range_with_metavars
module G = AST_generic
module Log = Log_engine.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let adjust_content_for_language (analyzer : Analyzer.t) (content : string) :
    string =
  match analyzer with
  | Analyzer.L (Lang.Php, _)
    when not (content =~ {|[ \t\n]*<\?\(php\|=\)?[ \t\n]+|}) ->
      (* THINK:
         * - Shouldn't the parser just handle the absence of `<?php` ?
         * - Isn't the `?>` closing needed ?
      *)
      "<?php " ^ content
  | __else__ -> content

(* This function adds mvars to a range, but only the mvars which are not already
   inside of that range.
   Precondition: You should make sure that `range` has been vetted such that all
   of its mvars unify with those in `mvars`!

   In this file, it's OK to call this function because we know that, due to
   `opt_context` being passed down into `get_nested_formula_matches`, we have
   already ensured that all produced ranges from that function are such that
   they are consistent with the metavariables in `range`.

   This means that if we had:
   patterns:
     - pattern: |
         foo($A, $B)
     - metavariable-pattern:
         metavariable: $A
         pattern: |
           bar($B)

    we are ensured that any produced matches from the `metavariable-pattern`
    can only match `bar($B)` such that `$B` unifies with the outer `$B`.

    So we add the non-inherited metavariables into this range.

    This may also produce duplicate ranges, depending on how many metavariables
    are in the file.
*)
let filter_new_mvars_by_range range mvars =
  mvars
  |> List.filter (fun (mvar, _) ->
         not (Option.is_some (List.assoc_opt mvar range.RM.mvars)))

(* We take the bindings from the nested matches, and produce a new match where we add
   the enclosed metavariables to the original range.

   Care that these bindings were produced in a temp file, with possibly-different location
   information, so we need to construct a visitor so we can convert the mvalues to have
   tokens with the correct information.
*)
let get_persistent_bindings revert_loc r nested_matches =
  let reverting_visitor =
    AST_generic_helpers.fix_token_locations_any revert_loc
  in
  nested_matches
  |> List_.map (fun nested_match ->
         (* The bindings in this match were produced from a target whose location
             data was all adjusted, to avoid re-parsing.
         *)
         let readjusted_mvars =
           nested_match.RM.mvars
           |> List_.map (fun (mvar, mval) ->
                  let mval =
                    mval |> MV.mvalue_to_any |> reverting_visitor
                    |> MV.mvalue_of_any
                  in
                  (mvar, mval))
         in
         { nested_match with RM.mvars = readjusted_mvars })
  |> List_.map (fun r' -> filter_new_mvars_by_range r r'.RM.mvars)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let get_nested_metavar_pattern_bindings get_nested_formula_matches env r mvar
    (opt_analyzer : Analyzer.t option) formula =
  let bindings = r.RM.mvars in
  (* If anything goes wrong the default is to filter out! *)
  match List.assoc_opt mvar bindings with
  | None ->
      error env
        (Common.spf
           "metavariable-pattern failed because %s is not in scope, please \
            check your rule"
           mvar);
      []
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
          []
      | Some (mval_file, mval_range) -> (
          let r' =
            (* Fix the range to match the content of the temporary file. *)
            {
              r with
              r = { start = 0; end_ = mval_range.end_ - mval_range.start };
            }
          in
          (* We don't want having to re-parse `content', but then we
           * need to fix the token locations in `mast`. *)
          let mast_start_loc =
            mval |> MV.ii_of_mval |> AST_generic_helpers.range_of_tokens_unsafe
            |> fst |> Tok.unsafe_loc_of_tok
          in
          let mast_start_pos = mast_start_loc.pos in
          let fix_loc (file : Fpath.t) (loc : Tok.location) : Tok.location =
            (* The column is only perturbed if this loc is on the first line of
             * the original metavariable match *)
            let pos = loc.pos in
            let column =
              if pos.line =|= mast_start_pos.line then
                pos.column - mast_start_pos.column
              else pos.column
            in
            let pos =
              Pos.make file ~column
                ~line:(pos.line - mast_start_pos.line + 1)
                (pos.bytepos - mast_start_pos.bytepos)
            in
            { loc with pos }
          in
          (* We need to undo the changes made to the file location,
             for when we preserve this binding and re-localize it to
             the original file.
          *)
          let revert_loc (loc : Tok.location) =
            (* See fix_loc *)
            let pos = loc.pos in
            let column =
              if pos.line =|= 1 then pos.column + mast_start_pos.column
              else pos.column
            in
            let pos =
              Pos.make mval_file ~column
                ~line:(pos.line + mast_start_pos.line - 1)
                (pos.bytepos + mast_start_pos.bytepos)
            in
            { loc with pos }
          in
          match opt_analyzer with
          | None -> (
              (* We match wrt the same language as the rule. *)
              match MV.program_of_mvalue mval with
              | None ->
                  error env
                    (Common.spf
                       "metavariable-pattern failed because %s does not bind \
                        to a sub-program, please check your rule"
                       mvar);
                  []
              | Some mast ->
                  (* Note that due to symbolic propagation, `mast` may be
                   * outside of the current file/AST, so we must get
                   * `mval_range` from `mval_file` and not from `env.file`! *)
                  let contents = Range.content_at_range mval_file mval_range in
                  (* TODO: find a way to not use tmp files! parse strings *)
                  (* nosemgrep: forbid-tmp *)
                  UTmp.with_temp_file ~contents ~suffix:".mvar-pattern"
                    (fun (file : Fpath.t) ->
                      let mast' =
                        AST_generic_helpers.fix_token_locations_program
                          (fix_loc file) mast
                      in
                      let xtarget =
                        {
                          env.xtarget with
                          path =
                            {
                              env.xtarget.path with
                              internal_path_to_content = file;
                            };
                          lazy_ast_and_errors = lazy (mast', []);
                          lazy_content = lazy contents;
                        }
                      in
                      (* Persist the bindings from inside the `metavariable-pattern`
                         matches
                      *)
                      get_nested_formula_matches { env with xtarget } formula r'
                      |> get_persistent_bindings revert_loc r))
          | Some analyzer -> (
              let content =
                (* Previously we only allowed metavariable-pattern with a
                 * different language when the metavariable was bound to a
                 * string. For these cases, we keep the previous behavior, which
                 * I (nmote) believe makes only the contents of the string
                 * available for matching in the sub language, and not the
                 * enclosing quotes.
                 *
                 * However, now we also allow non-text metavariable bindings to
                 * match in generic mode. Because non-text metavariable bindings
                 * are highly unlikely to be syntactically valid in any
                 * language, we limit this to generic mode only. However, it can
                 * still be useful to do some fuzzy matching on the contents of
                 * a metavariable binding. For this case, we just use the
                 * original program text as it exists in the target program, at
                 * the range to which the metavariable is bound. *)
                match (analyzer, mval) with
                | _, MV.Text (content, _tok, _)
                | _, MV.Xmls [ XmlText (content, _tok) ]
                | _, MV.E { e = G.L (G.String (_, (content, _tok), _)); _ } ->
                    Some content
                | (LSpacegrep | LAliengrep), _ ->
                    Some (Range.content_at_range mval_file mval_range)
                | _else_ -> None
              in
              match content with
              | None ->
                  (* This is not necessarily an error in the rule, e.g. you may
                   * be matching `$STRING + ...` and then add a
                   * metavariable-pattern on `$STRING`. This will only work when
                   * `$STRING` binds to some text (except when using language:
                   * generic, see above) but it can naturally bind to other
                   * string expressions. *)
                  Log.debug (fun m ->
                      m
                        "metavariable-pattern failed because the content of %s \
                         is not text: %s"
                        mvar (MV.show_mvalue mval));
                  []
              | Some contents ->
                  let contents =
                    adjust_content_for_language analyzer contents
                  in
                  Log.info (fun m ->
                      m "nested analysis of |||%s||| with lang '%s'" contents
                        (Analyzer.to_string analyzer));
                  (* We re-parse the matched text as `analyzer`. *)
                  (* TODO: find a way to not use tmp files! parse strings *)
                  (* nosemgrep: forbid-tmp *)
                  UTmp.with_temp_file ~contents ~suffix:".mvar-pattern"
                    (fun file ->
                      let ast_and_errors_res =
                        match analyzer with
                        | L (lang, _) -> (
                            try
                              let { Parsing_result2.ast; skipped_tokens; _ } =
                                Parse_target.parse_and_resolve_name lang file
                              in
                              (* Reposition the errors to the original source
                               * text, so that we don't report them to the end
                               * user as being from the temp file that we
                               * created. *)
                              let skipped_tokens =
                                skipped_tokens
                                |> List_.map (fun tok -> revert_loc tok)
                              in
                              if skipped_tokens <> [] then
                                Log.warn (fun m ->
                                    m
                                      "rule %s: metavariable-pattern: failed \
                                       to fully parse the content of %s"
                                      (Rule_ID.to_string (fst env.rule.Rule.id))
                                      mvar);
                              Ok (lazy (ast, skipped_tokens))
                            with
                            | Parsing_error.Syntax_error tk ->
                                Error (Tok.content_of_tok tk))
                        | LRegex
                        | LSpacegrep
                        | LAliengrep ->
                            Ok
                              (lazy
                                (failwith
                                   "requesting generic AST for \
                                    LRegex|LSpacegrep|LAliengrep"))
                      in
                      match ast_and_errors_res with
                      | Error msg ->
                          error env
                            (Common.spf
                               "rule %s: metavariable-pattern failed when \
                                parsing %s's content as %s: %s"
                               (Rule_ID.to_string (fst env.rule.Rule.id))
                               mvar
                               (Analyzer.to_string analyzer)
                               msg);
                          []
                      | Ok lazy_ast_and_errors ->
                          let xtarget : Xtarget.t =
                            {
                              path =
                                {
                                  origin = File mval_file;
                                  internal_path_to_content = file;
                                };
                              analyzer;
                              lazy_ast_and_errors;
                              lazy_content = lazy contents;
                            }
                          in
                          (* Persist the bindings from inside the `metavariable-pattern`
                             matches
                          *)
                          get_nested_formula_matches { env with xtarget }
                            formula r'
                          |> get_persistent_bindings revert_loc r))))

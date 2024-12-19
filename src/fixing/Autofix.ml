(* Nat Mote, Brandon Wu
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
module OutJ = Semgrep_output_v1_t
module Log = Log_fixing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Autofix logic entry point for semgrep.
 *
 * This is the main module for AST-based autofix. This module will attempt to
 * synthesize a fix based a rule's fix pattern and the match's metavariable
 * bindings.
 *
 * See also Textedit.ml for the generic library supporting text edits.
 *)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* For matching things of the form \<num>.
   See `regex_fix` below for why this is needed.
*)
let capture_group_regex = "\\\\([0-9]+)"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* When we produce a fix, we take some content and paste it into the
   place where the fix needs to go.

   For instance, if we want to do a fix of
   foo();
   bar();

   and we want to do it on all calls to qux() in the following function:
   int main() {
     qux();
   }

   we would just insert "foo();\nbar();" in the place where "qux();" goes.
   However, this produces the following code:
   int main() {
     foo();
   bar();
   }
   which is not indented correctly!

   This is because not every newline is made the same. In the original YAML
   of the fix, the newline before `bar` had no indentation because indeed,
   neither did `foo`.

   But, when replacing text which may appear in an arbitrary file, we might
   want to ensure that all the fix lines are aligned with respect to each
   other. This is only all lines but the first, however, because we assume
   that the start column should be where the first line starts.

   This code just goes and does that.
*)
let align_nonfirst_lines_at_column ~start_column text =
  let replacement_lines =
    match String.split_on_char '\n' text with
    | [] -> []
    | first :: rest ->
        let padding = String.make start_column ' ' in
        first :: List_.map (fun s -> padding ^ s) rest
  in
  String.concat "\n" replacement_lines

let%test _ =
  align_nonfirst_lines_at_column ~start_column:0 "foo\nbar" =*= "foo\nbar"

let%test _ =
  align_nonfirst_lines_at_column ~start_column:1 "foo\nbar" =*= "foo\n bar"

let%test _ =
  align_nonfirst_lines_at_column ~start_column:1 " foo\nbar" =*= " foo\n bar"

(*****************************************************************************)
(* AST-based autofix helpers *)
(*****************************************************************************)

let parse_pattern lang pattern =
  try Ok (Parse_pattern.parse_pattern lang pattern) with
  | Time_limit.Timeout _ as e -> Exception.catch_and_reraise e
  | e ->
      let e = Exception.catch e in
      Error e

(* TODO: should not rely on tmp file, have Parse_target accept a
 * more flexible origin/source
 *)
let parse_target lang text =
  (* ext shouldn't matter, but could use Lang.ext_of_lang if needed *)
  (* nosemgrep: forbid-tmp *)
  UTmp.with_temp_file ~contents:text ~suffix:".check" (fun file ->
      try Ok (Parse_target.just_parse_with_lang lang file) with
      | Time_limit.Timeout _ as e -> Exception.catch_and_reraise e
      | e ->
          let e = Exception.catch e in
          Error e)

(* Fixes up the AST to be syntactically valid. Currently I (nmote) am only aware
 * of one case where this needs to be done (see the function body). Another
 * option in this case might be to omit or downrank matches that rely on
 * unordered keyword matching, but that assumes that a better match does in fact
 * exist. Such a change would also have a larger impact on Semgrep as a whole.
 * *)
let transform_fix lang ast =
  match lang with
  | Lang.Python ->
      (* Due to unordered keyword argument matching (see
       * Generic_vs_generic.m_list__m_argument), we can end up generating
       * autofixes where keyword arguments are moved before positional
       * arguments. In some languages (OCaml, for example) this doesn't change
       * the semantics, but in Python this is actually syntactically invalid.
       * So, to avoid generating invalid autofixes, we move the positional
       * arguments in front of the keyword arguments.
       *
       * See the fix_ellipsis_metavar.py test case for an example of when this
       * can happen. *)
      let mapper =
        object (_self : 'self)
          inherit [_] AST_generic.map as super

          method! visit_arguments env (l, args, r) =
            let args =
              List.stable_sort
                (fun a b ->
                  match (a, b) with
                  | ( (AST_generic.Arg _ | ArgType _ | OtherArg _),
                      (ArgKwd _ | ArgKwdOptional _) ) ->
                      -1
                  | ( (ArgKwd _ | ArgKwdOptional _),
                      (Arg _ | ArgType _ | OtherArg _) ) ->
                      1
                  | _else_ -> 0)
                args
            in
            super#visit_arguments env (l, args, r)
        end
      in
      mapper#visit_any () ast
  | _else_ -> ast

(* Check whether the proposed fix results in syntactically valid code *)
let validate_fix lang target_contents edit =
  Log.debug (fun m -> m "validate fix %s" (Textedit.show edit));
  let fail err =
    Error
      (spf "Rendered autofix does not parse. Aborting: `%s`:\n%s"
         edit.Textedit.replacement_text err)
  in
  let full_fixed_contents = Textedit.apply_edit_to_text target_contents edit in
  match parse_target lang full_fixed_contents with
  | Ok { skipped_tokens = []; _ } -> Ok edit
  | Ok { skipped_tokens = fix_skipped_tokens; _ } -> (
      (* We had a partial parse failure, but this does happen. As long as we
       * didn't make the parse issue worse, we are probably good to go. So,
       * let's parse the original text and compare the number of skipped tokens
       * before and after the fix. *)
      match parse_target lang target_contents with
      | Ok { skipped_tokens = orig_skipped_tokens; _ } ->
          if List.length fix_skipped_tokens <= List.length orig_skipped_tokens
          then Ok edit
          else fail "More partial parse failures after autofix application"
      | Error _ ->
          (* This really shouldn't happen. How did we get to this point if the
           * original file didn't parse? And why did it just parse without any
           * fatal errors after the autofix was applied? If you're here, good
           * luck. Probably it changed out from under us. *)
          fail "Failed to parse original file")
  | Error e -> fail (Exception.to_string e)

(*****************************************************************************)
(* Kinds of fixes *)
(*****************************************************************************)

(* Attempts to render a fix. If successful, returns the text that should replace
 * the matched range in the target file. If unsuccessful, returns None.
 *
 * Failure causes include, but are not limited to:
 * - The fix pattern does not parse.
 * - A metavariable is bound to an AST node that is not suitable for the context
 *   in which it is used in the fix.
 * - Printing of the resulting fix AST fails (probably because there is simply a
 *   node that is unhandled).
 * *)
let ast_based_fix ~fix (start, end_) (pm : Core_match.t) : Textedit.t option =
  let fix_pattern = fix in
  let* lang = List.nth_opt pm.rule_id.langs 0 in
  let metavars = pm.env in
  let target_contents =
    lazy (UFile.read_file pm.path.internal_path_to_content)
  in
  let result =
    try
      (* Fixes are not exactly patterns, but they can contain metavariables that
       * should be substituted with the nodes to which they are bound in the match.
       * Because they can contain metavariables, we need to parse them as patterns.
       * *)
      let/ fix_pattern_ast =
        parse_pattern lang fix_pattern
        |> Result.map_error (fun e ->
               spf "Failed to parse fix pattern:\n%s" (Exception.to_string e))
        |> Result.join
      in

      (* Look through the fix pattern's AST and replace metavariables with the nodes to
       * which they are bound in the match. This should generate a well-formed AST,
       * which when printed to text, should replace the range in the original match.
       *
       * We need to do this instead of just replacing metavars with their original
       * text during printing. It's important for correctness to construct a
       * well-formed AST as an intermediate step. For example, an ellipsis
       * metavariable ($...X) might be bound to zero arguments in a function call
       * (foo(1, $...X) would match foo(1), for example). If we were to skip this
       * step, we would end up printing the extraneous comma before `$...X`.
       *
       * As we improve autofix, we may also want to perform other operations over
       * the fixed AST.
       * *)
      let/ fixed_pattern_ast =
        Autofix_metavar_replacement.replace_metavars metavars fix_pattern_ast
      in

      (* It's possible to represent syntactically invalid code in the generic
       * AST, so in case we've done that in the previous step, we need to
       * transform it to be syntactically valid. See the transform_fix function
       * for more details. *)
      let fixed_pattern_ast = transform_fix lang fixed_pattern_ast in

      (* Try to print the fixed pattern AST. *)
      let/ text =
        Autofix_printer.print_ast ~lang ~metavars ~target_contents
          ~fix_pattern_ast ~fix_pattern fixed_pattern_ast
      in
      let text =
        align_nonfirst_lines_at_column
          ~start_column:(fst pm.range_loc).pos.column text
      in

      let edit =
        {
          Textedit.path = pm.path.internal_path_to_content;
          start;
          end_;
          replacement_text = text;
        }
      in

      (* Perform sanity checks for the resulting fix. *)
      validate_fix lang (Lazy.force target_contents) edit
    with
    | Time_limit.Timeout _ as e -> Exception.catch_and_reraise e
    | e ->
        let e = Exception.catch e in
        Error
          (spf "Unexpected error while rendering autofix:\n%s"
             (Exception.to_string e))
  in
  match result with
  | Ok x -> Some x
  | Error err ->
      let msg =
        spf "Failed to render AST-based fix `%s`:\n%s" fix_pattern err
      in
      (* Print line-by-line so that each line is preceded by the logging header.
         Looks nicer and makes it easier to mask in e2e test output.
         TODO: make the Logs_ library do this by default. *)
      String.split_on_char '\n' msg
      |> List.iter (fun s -> Log.warn (fun m -> m "%s" s));
      None

let basic_fix ~(fix : string) (start, end_) (pm : Core_match.t) : Textedit.t =
  let start_column = (fst pm.range_loc).pos.column in
  (* TODO: Use m.env instead *)
  let replacement_text =
    Metavar_replacement.interpolate_metavars fix
      (Metavar_replacement.of_bindings pm.env)
    |> align_nonfirst_lines_at_column ~start_column
  in
  let edit =
    Textedit.
      { path = pm.path.internal_path_to_content; start; end_; replacement_text }
  in
  edit

let regex_fix ~fix_regexp:Rule.{ regexp; count; replacement } (start, end_)
    (pm : Core_match.t) =
  let rex = Pcre2_.regexp regexp in
  (* You need a minus one, to make it compatible with the inclusive Range.t *)
  let content =
    Range.content_at_range pm.path.internal_path_to_content
      Range.{ start; end_ = end_ - 1 }
  in
  (* What is this for?
     Before, when autofix was in the Python CLI, `fix-regex` had the semantics
     of allowing backreferences to be substituted via the syntax '\1' for the
     first backreference.
     Unfortunately, the Pcre-ocaml library has no conception of this syntax. It
     instead uses $1, $2, etc, for backreferences.
     We don't want to break this existing behavior, however.
     The fix will be that if someone gives us a replacement regex of the form
     '\1', we will try to replace it instead with '$1', etc.
  *)
  let replaced_replacement =
    let capture_group_rex = Pcre2_.regexp capture_group_regex in
    (* Confusingly, this $1 in the template is separate from the literal
       capture group it is replacing. It is simply a dollar sign in front of
       the capture group's number, which is captured in the `capture_group_regex`
       above.
       This lets us essentially capture everything matched by \<num> with
       $<num>.
    *)
    Pcre2_.replace ~rex:capture_group_rex ~template:"$$$1" replacement
  in
  let replacement_text =
    match count with
    | None -> Pcre2_.replace ~rex ~template:replaced_replacement content
    | Some count ->
        Common2.foldn
          (fun content _i ->
            Pcre2_.replace_first ~rex ~template:replaced_replacement content)
          content count
    (* TODO: We could align text here, but the problem is that we need the
       start column of the area being replaced.
       The area being replaced in a regex fix is not necessarily that of the
       entire match. So we would need to compute that.
    *)
  in
  let edit =
    Textedit.
      { path = pm.path.internal_path_to_content; start; end_; replacement_text }
  in
  edit

(*****************************************************************************)
(* Autofix selection logic *)
(*****************************************************************************)

let render_fix (pm : Core_match.t) : Textedit.t option =
  let fix =
    (* We prefer the fix that is global to the entire rule if it exists.
       Otherwise, we'll take the fix_text that we populated the pattern
       match with during Match_search_mode.
    *)
    match (pm.rule_id.fix, pm.fix_text) with
    | None, None -> None
    | Some fix, _ -> Some fix
    | _, Some fix -> Some fix
  in
  let fix_regex = pm.rule_id.fix_regexp in
  let range =
    let start, end_ = pm.range_loc in
    let _, _, end_charpos = Tok.end_pos_of_loc end_ in
    (start.Tok.pos.bytepos, end_charpos)
  in
  match (fix, fix_regex) with
  | None, None -> None
  | Some fix, _ -> (
      match ast_based_fix ~fix range pm with
      | None -> Some (basic_fix ~fix range pm)
      | Some fix -> Some fix)
  | _, Some fix_regexp -> Some (regex_fix ~fix_regexp range pm)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let produce_autofix (m : Core_result.processed_match) =
  { m with autofix_edit = render_fix m.pm }

let produce_autofixes (matches : Core_result.processed_match list) =
  (* TODO Simple function, inline at callsites? *)
  List_.map produce_autofix matches

(* This is used for testing only. This is why it raises an exception. *)
let apply_fixes_to_file_exn path edits =
  let file_text = UFile.read_file path in
  match Textedit.apply_edits_to_text path file_text edits with
  | Success x -> x
  | Overlap { conflicting_edits; _ } ->
      failwith
        (spf "Could not apply fix because it overlapped with another: %s"
           (List_.hd_exn "unexpected empty list" conflicting_edits)
             .replacement_text)

let apply_fixes ?(dryrun = false) (edits : Textedit.t list) : unit =
  let modified_files, failed_fixes = Textedit.apply_edits ~dryrun edits in

  (match modified_files with
  | _ :: _ ->
      Log.info (fun m ->
          m "%smodified %s."
            (match failed_fixes with
            | [] -> "successfully "
            | _ -> "")
            (String_.unit_str (List.length modified_files) "file(s)"))
  | [] -> Log.info (fun m -> m "no files modified."));
  match failed_fixes with
  | [] -> ()
  | _ ->
      Log.warn (fun m ->
          m "failed to apply %i fix(es)." (List.length failed_fixes))

let apply_fixes_of_core_matches ?dryrun (matches : OutJ.core_match list) : unit
    =
  matches
  |> List_.filter_map (fun (m : OutJ.core_match) ->
         let* replacement_text = m.extra.fix in
         let start = m.start.offset in
         let end_ = m.end_.offset in
         Some Textedit.{ path = m.path; start; end_; replacement_text })
  |> apply_fixes ?dryrun

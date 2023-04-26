(* Nat Mote
 *
 * Copyright (C) 2019-2022 r2c
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

let logger = Logging.get_logger [ __MODULE__ ]
let ( let/ ) = Result.bind

(******************************************************************************)
(* Main module for AST-based autofix. This module will attempt to synthesize a
 * fix based a rule's fix pattern and the match's metavariable bindings. *)
(******************************************************************************)

let parse_pattern lang pattern =
  try Ok (Parse_pattern.parse_pattern lang pattern) with
  | Time_limit.Timeout _ as e -> Exception.catch_and_reraise e
  | e ->
      let e = Exception.catch e in
      Error e

let parse_target lang text =
  (* ext shouldn't matter, but could use Lang.ext_of_lang if needed *)
  Common2.with_tmp_file ~str:text ~ext:"check" (fun file ->
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

(******************************************************************************)
(* Entry Points *)
(******************************************************************************)

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
let render_fix pm =
  let* fix_pattern = pm.Pattern_match.rule_id.fix in
  let* lang = List.nth_opt pm.Pattern_match.rule_id.languages 0 in
  let metavars = pm.Pattern_match.env in
  let start, end_ =
    let start, end_ = pm.Pattern_match.range_loc in
    let _, _, end_charpos = Tok.end_pos_of_loc end_ in
    (start.Tok.pos.charpos, end_charpos)
  in
  let target_contents = lazy (Common.read_file pm.Pattern_match.file) in
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

      let edit =
        { Textedit.path = pm.file; start; end_; replacement_text = text }
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
      let msg = spf "Failed to render fix `%s`:\n%s" fix_pattern err in
      (* Print line-by-line so that each line is preceded by the logging header.
       * Looks nicer and makes it easier to mask in e2e test output. *)
      String.split_on_char '\n' msg
      |> List.iter (fun line -> logger#info "%s" line);
      None

(* Apply the fix for the list of matches to the given file, returning the
 * resulting file contents. Currently used only for tests, but with some changes
 * could be used in production as well. *)
let apply_fixes_to_file matches ~file =
  let file_text = Common.read_file file in
  let edits =
    Common.map
      (fun pm ->
        match render_fix pm with
        | Some edit -> edit
        (* TODO option rather than exception if used in production *)
        | None -> failwith (spf "could not render fix for %s" file))
      matches
  in
  match Textedit.apply_edits_to_text file_text edits with
  | Success x -> x
  | Overlap { conflicting_edits; _ } ->
      failwith
        (spf "Could not apply fix because it overlapped with another: %s"
           (Common.hd_exn "unexpected empty list" conflicting_edits)
             .replacement_text)

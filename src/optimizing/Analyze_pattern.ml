(* Yoann Padioleau
 *
 * Copyright (C) 2021 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Analyzing a semgrep pattern for optimization purpose.
 *
 * It is useless to run the semgrep engine with rules containing specific
 * identifiers (e.g., 'eval') or strings (e.g., 'react.js')
 * that are never mentioned in the target file.
 *
 * We did something similar in Coccinelle I think. This also has been
 * mentioned many times (by Clint, HN, etc.).
 *
 * Right now the main optimization is to extract a regexp from
 * a pattern that we can run first on the target file.
 *
 * This module is currently used by:
 *  - Mini_rules_filter and Semgrep_generic, to skip certain mini-rules
 *    (but not entire files)
 *  - the Semgrep.ml engine to skip entire files!
 *)

module MvarSet = Common2.StringSet

type mvars = MvarSet.t

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let extract_strings_and_mvars ?lang any =
  let strings = ref [] in
  let mvars = ref [] in
  let visitor =
    object (_self : 'self)
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_ident _env (str, _tok) =
        match () with
        | _ when Mvar.is_metavar_name str -> Stack_.push str mvars
        | _ when not (Pattern.is_special_identifier ?lang str) ->
            Stack_.push str strings
        | _ -> ()

      method! visit_name env x =
        match x with
        | Id (_id, { id_flags; _ }) when IdFlags.is_hidden !id_flags ->
            (* This identifier is not present in the pattern source.
                We assume a match is possible without the identifier
                being present in the target source, so we ignore it. *)
            ()
        | _ -> super#visit_name env x

      method! visit_directive env x =
        match x with
        | { d = ImportFrom (_, FileName (str, _), _); _ }
        | { d = ImportAs (_, FileName (str, _), _); _ }
        | { d = ImportAll (_, FileName (str, _), _); _ }
          when str <> "..."
               && (not (Mvar.is_metavar_name str))
               && (* deprecated *) not (Pattern.is_regexp_string str) ->
            (* Semgrep can match "foo" against "foo/bar", so we just
             * overapproximate taking the sub-strings, see
             * Generic_vs_generic.m_module_name_prefix. *)
            String_.split ~sep:{|/\|\\|} str
            |> List.iter (fun s -> Stack_.push s strings);
            super#visit_directive env x
        | _ -> super#visit_directive env x

      method! visit_expr env x =
        match x.e with
        (* less: we could extract strings for the other literals too?
         * atoms, chars, even int?
         * We do now semantic equivance on integers between values so
         * 1000 is now equivalent to 1_000 so we can't "regexpize" it.
         *)
        | L (String (_, (_str, _tok), _)) ->
            (* Previously we used all string literals for pre-filtering, but this does
             * not play well with constant propagation. If we are looking for pattern
             * `"foobar"` a file may not contain `"foobar"` as-is, but it may contain
             * e.g. `"foo" + "bar"`. *)
            super#visit_expr env x
        | Call
            ( { e = IdSpecial (Require, _); _ },
              (_, [ Arg { e = L (String (_, (str, _tok), _)); _ } ], _) ) ->
            if not (Pattern.is_special_string_literal str) then
              Stack_.push str strings
        | IdSpecial (Eval, t) ->
            if Tok.is_origintok t then
              Stack_.push (Tok.content_of_tok t) strings
        | TypedMetavar (_, _, type_) -> (
            match type_ with
            | { t = TyN (IdQualified _ as name); _ } ->
                (* We need to be careful because built-in types such as "int" are
                 * represented with 'TyN' and do not need to occur in the file.
                 * However, if the type is an 'IdQualified', e.g. `java.lang.String`,
                 * and when running an intra-file analysis, then we do exepct the
                 * sub-identifiers (e.g. 'java', 'lang', and 'String') to occur in
                 * the file.
                 * TODO: If we eventually use Naming SAST in --pro-intrafile, then
                 *   we will need to check that this name is not imported by default,
                 *   as it is 'java.lang.String'! *)
                super#visit_name env name
            | __else__ ->
                (* do not recurse there, the type does not have to be in the source *)
                ())
        (* for bloom_filters: do not recurse here (for ApplyEquivalence,
         * this would be an error)
         * THINK: bloom filter was removed, something to re-consider here? *)
        | DisjExpr _ -> ()
        | _ -> super#visit_expr env x
    end
  in
  visitor#visit_any () any;
  (Common2.uniq !strings, Common2.uniq !mvars)

let extract_specific_strings ?lang any =
  extract_strings_and_mvars ?lang any |> fst

(* In general, if we encounter a `metavariable-regex` operator, we cannot
 * simply use the `regex` for pre-filtering, because the metavariable may
 * be matching a string that is the result of constant folding.
 *
 * This visitor extracts the set of metavariables that are used in
 * an "identifier position" that we do not expect to be affected by
 * constant folding. This is often a COMPROMISE. For example, in some
 * languages it might be possible to write `import "foobar"` as
 * `import ("foo" + "bar"), but we do not expect that in regular code
 * and using module names for pre-filtering is very important for perf.
 *
 * alt: We could do the opposite and find the metavariables in a position
 *      where we expect constant-folding to be a problem, e.g. "$MVAR" or
 *      $FUNC(..., $MVAR, ...). *)
let extract_mvars_in_id_position ?lang:_ any =
  let mvars = ref MvarSet.empty in
  let visitor =
    object (_self : 'self)
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_directive env x =
        match x with
        | { d = ImportFrom (_, FileName (str, _), _); _ }
        | { d = ImportAs (_, FileName (str, _), _); _ }
        | { d = ImportAll (_, FileName (str, _), _); _ }
          when Mvar.is_metavar_name str ->
            mvars := MvarSet.add str !mvars;
            super#visit_directive env x
        | _ -> super#visit_directive env x

      method! visit_type_kind env x =
        match x with
        | TyN (Id ((str, _tok), _ii)) when Mvar.is_metavar_name str ->
            mvars := MvarSet.add str !mvars
        | _ -> super#visit_type_kind env x

      method! visit_expr env x =
        match x.e with
        | Call ({ e = N (Id ((str, _tok), _ii)); _ }, _)
        | New (_, { t = TyN (Id ((str, _tok), _ii)); _ }, _, _)
        | DotAccess (_, _, FN (Id ((str, _tok), _ii)))
          when Mvar.is_metavar_name str ->
            mvars := MvarSet.add str !mvars
        | _ -> super#visit_expr env x
    end
  in
  visitor#visit_any () any;
  !mvars

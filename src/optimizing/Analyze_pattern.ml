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
        | _ when Metavariable.is_metavar_name str -> Common.push str mvars
        | _ when not (Pattern.is_special_identifier ?lang str) ->
            Common.push str strings
        | _ -> ()

      method! visit_name env x =
        match x with
        | Id (_id, id_info) when is_hidden id_info ->
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
               && (not (Metavariable.is_metavar_name str))
               && (* deprecated *) not (Pattern.is_regexp_string str) ->
            (* Semgrep can match "foo" against "foo/bar", so we just
             * overapproximate taking the sub-strings, see
             * Generic_vs_generic.m_module_name_prefix. *)
            Common.split {|/\|\\|} str
            |> List.iter (fun s -> Common.push s strings);
            super#visit_directive env x
        | _ -> super#visit_directive env x

      method! visit_expr env x =
        match x.e with
        (* less: we could extract strings for the other literals too?
         * atoms, chars, even int?
         * We do now semantic equivance on integers between values so
         * 1000 is now equivalent to 1_000 so we can't "regexpize" it.
         *)
        | L (String (_, (str, _tok), _)) ->
            if not (Pattern.is_special_string_literal str) then
              Common.push str strings
        | IdSpecial (Eval, t) ->
            if Tok.is_origintok t then
              Common.push (Tok.content_of_tok t) strings
        (* do not recurse there, the type does not have to be in the source *)
        | TypedMetavar _ -> ()
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

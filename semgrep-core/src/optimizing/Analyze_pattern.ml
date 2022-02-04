(* Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
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
open AST_generic
module V = Visitor_AST

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
 *  - the bloom filter pattern extractor of Nathan and Emma
 *  - the Semgrep.ml engine to skip entire files!
 *
 * TODO:
 *  - extract identifiers, and basic strings
 *  - TODO extract filenames in import
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let _error s = failwith s

(*****************************************************************************)
(* Extractions *)
(*****************************************************************************)

(* TODO(iago): This is partly redundant with Bloom_annotation.statement_strings,
 * it might be more maintainable if we had a single visitor that worked for both
 * statements and patterns. *)

let extract_strings_and_mvars ?lang any =
  let strings = ref [] in
  let mvars = ref [] in
  let visitor =
    V.mk_visitor
      {
        V.default_visitor with
        V.kident =
          (fun (_k, _) (str, _tok) ->
            match () with
            | _ when Metavariable.is_metavar_name str -> Common.push str mvars
            | _ when not (Pattern.is_special_identifier ?lang str) ->
                Common.push str strings
            | _ -> ());
        V.kname =
          (fun (k, _) x ->
            match x with
            | Id (_id, { id_hidden = true; _ }) ->
                (* This identifier is not present in the pattern source.
                    We assume a match is possible without the identifier
                    being present in the target source, so we ignore it. *)
                ()
            | _ -> k x);
        V.kdir =
          (fun (k, _) x ->
            match x with
            | { d = ImportFrom (_, FileName (str, _), _, _); _ }
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
                k x
            | _ -> k x);
        V.kexpr =
          (fun (k, _) x ->
            match x.e with
            (* less: we could extract strings for the other literals too?
             * atoms, chars, even int?
             * We do now semantic equivance on integers between values so
             * 1000 is now equivalent to 1_000 so we can't "regexpize" it.
             *)
            | L (String (str, _tok)) ->
                if not (Pattern.is_special_string_literal str) then
                  Common.push str strings
            | IdSpecial (Eval, t) ->
                if Parse_info.is_origintok t then
                  Common.push (Parse_info.str_of_info t) strings
            (* do not recurse there, the type does not have to be in the source *)
            | TypedMetavar _ -> ()
            (* for bloom_filters: do not recurse here (for ApplyEquivalence,
             * this would be an error) *)
            | DisjExpr _ -> ()
            | _ -> k x);
      }
  in
  visitor any;
  (Common2.uniq !strings, Common2.uniq !mvars)

let extract_specific_strings ?lang any =
  extract_strings_and_mvars ?lang any |> fst

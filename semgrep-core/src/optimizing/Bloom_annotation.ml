(* Emma Jin, Yoann Padioleau
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
module B = Bloom_filter
module V = Visitor_AST
module Set = Set_
open AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helper functions for the Nathan's bloom filter optimization to skip
 * matching certain patterns against some code in semgrep.
 *
 * The intuition is that if the pattern contains an identifier like in
 * 'foo();', there is no point in running the Semgrep matching engine on all
 * the statements in the program if those statements do not contain this
 * identifier.
 * The Bloom filter allows to generalize this idea to a set of identifiers
 * that must be present in some statements while still being space-efficient.
 *
 * Note that we must make sure we don't skip statements we should analyze!
 * For example with the naming aliasing, a pattern like 'foo()'
 * should be matched on code like 'bar()' if bar was actualy an alias
 * for foo via a previous import.
 * The same is true for user-defined equivalences when a pattern contains
 * A DisjExpr, in which case we may need a set of Bloom filters for each
 * branch. Another example are integer literals, which in OCaml
 * can be written as 1000 or 1_000, so we want to do the bloom hasking
 * on the integer final value, not the string.
 *
 * See also semgrep-core/matching/Rules_filter.ml
 *)

(*****************************************************************************)
(* List helpers *)
(*****************************************************************************)

(* Note: If the bottom most node has n strings and k parents, it will take
 * O(kn) to add it to all the lists. Since it will also take O(kn) to add it
 * to all the bloom filters, this is acceptable, but it would not be
 * necessary if we used a linked list
 *)

let push_list s' s = s := Set.union s' !s

let push v s = s := Set.add v !s

(*****************************************************************************)
(* Traversal methods *)
(*****************************************************************************)

(* Use a visitor_AST to extract the strings from all identifiers,
 * and from all literals for now, except all semgrep stuff:
 *  - identifier which are metavariables
 *  - string like "..."
 *  - string like "=~/stuff/"
 *
 * See also Analyze_pattern.extract_specific_strings
 *)

let rec statement_strings stmt =
  let res = ref Set.empty in
  let top_level = ref true in
  let visitor =
    V.mk_visitor
      {
        V.default_visitor with
        V.kident = (fun (_k, _) (str, _tok) -> push str res);
        V.kexpr =
          (fun (k, _) x ->
            match x with
            (* less: we could extract strings for the other literals too?
             * atoms, chars, even int?
             *)
            | L (String (str, _tok)) -> push str res
            | IdSpecial (_, tok) -> push (Parse_info.str_of_info tok) res
            | _ -> k x);
        V.kconstness =
          (fun (k, _) x ->
            match x with
            | Lit (String (str, _tok)) ->
                if not (Pattern.is_special_string_literal str) then push str res
            | _ -> k x);
        (* The default behavior of kid_info is to not call the continutation *)
        (* We want to recurse so that we can index processed information like constants *)
        V.kid_info = (fun (k, _) x -> k x);
        V.kstmt =
          (fun (k, _) x ->
            (* First statement visited is the current statement *)
            if !top_level then (
              top_level := false;
              k x )
            else
              (* For any other statement, recurse to add the filter *)
              let strs = statement_strings x in
              let bf =
                B.make_bloom_from_set
                  !Flag_semgrep.set_instead_of_bloom_filter
                  strs
              in
              push_list strs res;
              x.s_bf <- Some bf);
      }
  in
  visitor (S stmt);
  !res

(*****************************************************************************)
(* Analyze the pattern *)
(*****************************************************************************)
let list_of_pattern_strings ?lang any =
  Analyze_pattern.extract_specific_strings ?lang any

(*****************************************************************************)
(* Analyze the code *)
(*****************************************************************************)

(* TODO: visit AST and set the s_bf field in statements *)
let annotate_program ast =
  let visitor =
    V.mk_visitor
      {
        V.default_visitor with
        V.kstmt =
          (fun (_k, _) x ->
            let ids = statement_strings x in
            let bf =
              B.make_bloom_from_set
                !Flag_semgrep.set_instead_of_bloom_filter
                ids
            in
            x.s_bf <- Some bf);
      }
  in
  visitor (Ss ast)
  [@@profiling]

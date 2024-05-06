(* Yoann Padioleau
 *
 * Copyright (C) 2020 Semgrep Inc.
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
module Flag = Flag_semgrep
module R = Mini_rule
module Log = Log_optimizing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mini rules filtering using regexps.
 *
 * This is deprecated; It is better to do the
 * regexp-extraction-from-pattern optimization at the rule level
 * in Semgrep.ml (instead of on mini-rule level in Semgrep_generic.ml).
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let filter_mini_rules_relevant_to_file_using_regexp rules lang file =
  let str = UFile.Legacy.read_file file in
  rules
  |> List.filter (fun rule ->
         let pat = rule.R.pattern in
         let xs = Analyze_pattern.extract_specific_strings ~lang pat in
         (* pr2_gen xs; *)
         let match_ =
           (* we could avoid running multiple regexps on the same file
            * by first orring them and do the and only of the or succeed,
            * but probably not worth the opti.
         let t = xs |> List.map (fun x -> regexp_matching_str x) |> Re.alt in
         let re = compile_regexp t in
         run_regexp re str
            *)
           (* Note that right now we do a for_all but it mighe be incorrect
            * at some point if the pattern contains DisjExpr for example, in
            * which case we will need extract_specific_strings to directly
            * extract a complex regexp instead handling itself disjunction.
            *)
           xs
           |> List.for_all (fun x ->
                  let re = Pcre2_.matching_exact_string x in
                  Pcre2_.unanchored_match re str)
         in

         if not match_ then
           Log.info (fun m ->
               m "filtering out rule %s" (Rule_ID.to_string rule.id));
         match_)
[@@profiling "Mini_rules_filter.filter"]

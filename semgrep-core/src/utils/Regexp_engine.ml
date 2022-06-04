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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper to the actual regexp engine(s).
 *
 * Regexps are used in many places in Semgrep:
 *  - in Generic_vs_generic to support the "=~/.../",
 *  - in Semgrep.ml for the metavariable-regexp and pattern-regexp
 *  - in Optimizing/ for skipping rules or target files
 *    (See Analyze_pattern.ml for more information).
 *  - TODO for include/exclude globbing
 *
 * notes: I tried to use the ocaml-re (Re) regexp libray instead of Str
 * because I thought it would be faster, and because it offers regexp
 * combinators (alt, rep, etc.) which might be useful at some point to
 * handle patterns containing explicit DisjExpr or for Analyze_rule.ml.
 * However when running on Zulip codebase with zulip semgrep rules,
 * Str is actually faster than Re.
 *
 * alternatives:
 *  - Str: simple, builtin
 *  - Re: provides alt() to build complex regexp, and also pure OCaml
 *    implem which is great in a JSOO context, but it seems slower.
 *    Can also support globbing with the Re.Glob module!
 *  - PCRE: powerful, but C dependency
 *
 * TODO:
 *  - move the regexp-related code in Generic_vs_generic here!
 *  - use Re.Glob just for globbing?
 *
 *)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

(* keep the string around for show *)
type t = string * Pcre.regexp

let pcre_pattern = fst
let pcre_regexp = snd
let show (s, _) = s
let pp fmt (s, _) = Format.fprintf fmt "\"%s\"" s
let equal (s1, _) (s2, _) = s1 = s2

let matching_exact_string s =
  let quoted = Pcre.quote s in
  (quoted, SPcre.regexp quoted)

let matching_exact_word s =
  let re = "\b" ^ Pcre.quote s ^ "\b" in
  (re, SPcre.regexp re)

(*
   MULTILINE = ^ and $ match at the beginning and end of lines rather than
               just at the beginning and end of input.
*)
let pcre_compile pat = (pat, SPcre.regexp ~flags:[ `MULTILINE ] pat)

let anchored_match =
  (* ~iflags are precompiled flags for better performance compared to ~flags *)
  let iflags = Pcre.rflags [ `ANCHORED ] in
  fun (_, re) str -> SPcre.pmatch_noerr ~iflags ~rex:re str

let unanchored_match (_, re) str = SPcre.pmatch_noerr ~rex:re str

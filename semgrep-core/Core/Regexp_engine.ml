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
 *  - switch everything to PCRE? use Re.Glob just for globbing?
 *  - make internal modules so easy to test and switch implem?
 *
*)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

(* Those functions using Re instead of Str are actually slower on Zulip.
   let regexp_matching_str s =
   Re.str s
   let compile_regexp t =
   Re.compile t
   let run_regexp re str =
   Re.execp re str
*)

module Str_engine = struct
  type t = Str.regexp

  let matching_string s =
    Str.regexp_string s

  (* this is not anchored! *)
  let run re str =
    (* bugfix:
     * this does not work!:  Str.string_match re str 0
     * because you need to add ".*" in front to make it work,
     * (but then you can not use regexp_string above)
     * => use Str.search_forward instead.
    *)
    try
      Str.search_forward re str 0 |> ignore; true
    with Not_found -> false
  [@@profiling]
end

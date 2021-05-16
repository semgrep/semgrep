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
module Re_engine = struct
  type t = string * Re.t (* not compiled *)

  let show (s, _) = s

  (* calling pp on Re.t is really slow, so better just print the string *)
  let pp fmt (s, _) = Format.fprintf fmt "\"%s\"" s

  let matching_exact_string s = ("exact:" ^ s, Re.str s)

  let regexp s = (s, Re.Pcre.re s)

  let compile t = Re.compile t [@@profiling]

  (* nice! *)
  let alt (s1, t1) (s2, t2) = (s1 ^ "|" ^ s2, Re.alt [ t1; t2 ])

  let run (_, t) str =
    let re = compile t in
    Re.execp re str
    [@@profiling]
end

module Str_engine = struct
  (* keep the string around for show *)
  type t = string * Str.regexp

  let show (s, _) = s

  let matching_exact_string s = ("!exact:s!", Str.regexp_string s)

  let regexp s = (s, Str.regexp s)

  (* this is not anchored! *)
  let run (_, re) str =
    (* bugfix:
     * this does not work!:  Str.string_match re str 0
     * because you need to add ".*" in front to make it work,
     * (but then you can not use regexp_string above)
     * => use Str.search_forward instead.
     *)
    try
      Str.search_forward re str 0 |> ignore;
      true
    with Not_found -> false
    [@@profiling]
end

module Pcre_engine = struct
  (* keep the string around for show *)
  type t = string * Pcre.regexp

  let show (s, _) = s

  let pp fmt (s, _) = Format.fprintf fmt "\"%s\"" s

  let equal (s1, _) (s2, _) = s1 = s2

  let matching_exact_string s =
    let quoted = Pcre.quote s in
    (quoted, Pcre.regexp quoted)

  let matching_exact_word s =
    let re = "\b" ^ Pcre.quote s ^ "\b" in
    (re, Pcre.regexp re)

  let run (_, re) str = Pcre.pmatch ~rex:re str
end

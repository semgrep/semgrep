(* Yoann Padioleau
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

(* We need deriving 'hash' below because formulas are now hashed
 * in Match_tainting_mode.Formula_tbl and formula contain patterns.
 *)
open Ppx_hash_lib.Std.Hash.Builtin

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type compiled_regexp = Pcre2_.t [@@deriving show, eq]
type regexp_string = string [@@deriving show, eq, hash]
(* see the NOTE "Regexp" below for the need to have this type *)

(* used in the engine for rule->mini_rule and match_result gymnastic *)
type pattern_id = int [@@deriving show, eq]

type xpattern_kind =
  (* bugfix: We previously made pattern parsing lazy which helps performance but
   * it breaks error handling, since pattern parsing can raise an exception. *)
  | Sem of Pattern.t * Lang.t (* language used for parsing the pattern *)
  | Spacegrep of Spacegrep.Pattern_AST.t
  | Aliengrep of Aliengrep.Pat_compile.t
  | Regexp of regexp_string
      (** NOTE "Regexp":
      * We used to keep the compiled regexp of type `Regexp_engine.t', but
      * that is not a pure OCaml data structure and it cannot be serialized.
      *
      * This had previously caused weird Semgrep crashes like
      *
      *     Invalid_argument "output_value: abstract value (Custom)"
      *
      * after PR #5725 (taint labels), see PA-1724.
      *
      * When we use -j 2 (or higher) and -json, then Parmap is used, and the result
      * of `Match_rules.check' (of type `Report.match_result') will contain some
      * unevaluated thunks. Then Parmap will try to marshal this value and it will
      * crash if there is any "Custom" block involved.
      *)
[@@deriving show, eq]

(* eXtended pattern *)
type t = {
  pat : xpattern_kind; [@hash.ignore]
  (* w.r.t. hashing, these are just Generic ASTs, which can be rather large.
     Whereas `Hashtbl.hash` will hash only to a certain depth, because it
     is polymorphic, it will be sensitive to things like tokens, which should
     be ignored by the hash function.
     The generated hash function for Generic ASTs will be rather hefty though,
     for the above reasoning. So we will choose to just decline to hash
     the generic AST.
  *)
  (* Regarding @equal below, even if two patterns have different indentation,
   * we still consider them equal in the metachecker context.
   * We rely only on the equality on pat, which will
   * abstract away line positions.
   * TODO: right now we have some false positives, e.g., in Python
   * assert(...) and assert ... are considered equal AST-wise
   * but it might be a bug!.
   *)
  pstr : string AST_generic.wrap; [@equal fun _ _ -> true]
  (* Unique id, incremented via a gensym()-like function in mk_pat().
   * This is used to run the patterns in a formula in a batch all-at-once
   * and remember what was the matching results for a certain pattern id.
   *)
  pid : pattern_id; [@equal fun _ _ -> true] [@hash.ignore]
}
[@@deriving show, eq, hash]
(* For hashing patterns, let's just hash the originating string. It's
   a good enough proxy.
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let count = ref 0

let mk_xpat pat pstr =
  incr count;
  { pat; pstr; pid = !count }

let is_regexp xpat =
  match xpat.pat with
  | Regexp _ -> true
  | __else__ -> false

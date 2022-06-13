(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
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

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type regexp = Regexp_engine.t [@@deriving show, eq]

(* used in the engine for rule->mini_rule and match_result gymnastic *)
type pattern_id = int [@@deriving show, eq]

type xpattern_kind =
  | Sem of Pattern.t * Lang.t (* language used for parsing the pattern *)
  | Spacegrep of Spacegrep.Pattern_AST.t
  | Regexp of regexp
  | Comby of string
[@@deriving show, eq]

(* eXtended pattern *)
type t = {
  pat : xpattern_kind;
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
  pid : pattern_id; [@equal fun _ _ -> true]
}
[@@deriving show, eq]

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
  | _ -> false

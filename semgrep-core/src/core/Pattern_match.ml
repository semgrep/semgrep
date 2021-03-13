(*s: semgrep/core/Pattern_match.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
(*e: pad/r2c copyright *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* We use 'eq' below to possibly remove redundant equivalent matches
 * (Generic_vs_generic sometimes return multiple times the same match,
 * sometimes because of some bugs we didn't fix, sometimes it's normal
 * because of the way '...' operate. TODO: an example
 * Note that you should not ignore the rule when comparing 2 matches!
 * One (mini) rule can be part of a pattern-not: in which case
 * even if it returns the same match than a similar rule in a pattern:,
 * we should not merge them!
*)

(*s: type [[Match_result.t]] *)
type t = {
  rule: Mini_rule.t [@equal fun a b -> a.Mini_rule.id = b.Mini_rule.id];
  file: Common.filename;
  location: Parse_info.token_location * Parse_info.token_location;
  (* do we need to be lazy? *)
  tokens: Parse_info.t list Lazy.t [@equal fun _a _b -> true];
  env: Metavariable.bindings;
}
[@@deriving show, eq]
(*e: type [[Match_result.t]] *)
(*e: semgrep/core/Pattern_match.ml *)

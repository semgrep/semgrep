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
(* Prelude *)
(*****************************************************************************)
(* Type to represent a pattern match.
 *
 * old: used to be called Match_result.t
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* We use 'eq' below to possibly remove redundant equivalent matches. Indeed,
 * Generic_vs_generic sometimes return multiple times the same match,
 * sometimes because of some bugs we didn't fix, sometimes it's normal
 * because of the way '...' operate. TODO: add an example of such situation.
 *
 * Note that you should not ignore the rule id when comparing 2 matches!
 * One match can come from a pattern-not: in which case
 * even if it returns the same match than a similar match coming
 * from a pattern:, we should not merge them!
 *)

(*s: type [[Match_result.t]] *)
type t = {
  (* rule (or mini rule) responsible for the pattern match found *)
  rule_id : rule_id; [@equal fun a b -> a.id = b.id]
  (* location information *)
  file : Common.filename;
  (* less: redundant with location? *)
  (* note that the two token_location can be equal *)
  range_loc : Parse_info.token_location * Parse_info.token_location;
  (* less: do we need to be lazy? *)
  tokens : Parse_info.t list Lazy.t; [@equal fun _a _b -> true]
  (* metavars for the pattern match *)
  env : Metavariable.bindings;
}

(* This is currently a record, but really only the rule id should matter.
 *
 * We could derive information in the other fields from the id, but that
 * would require to pass around the list of rules to get back the
 * information. Instead by embedding the information in the pattern match,
 * some functions are simpler (we use the same trick with Parse_info.t
 * where for example we embed the filename in it, not just a position).
 * alt: reuse Mini_rule.t
 *)
and rule_id = {
  (* This id is usually a string like 'check-double-equal'.
   * It can be the id of a rule or mini rule.
   *
   * Note that when we process a full rule, this id can temporarily
   * contain a Rule.pattern_id.
   *)
  id : string;
  (* other parts of a rule (or mini_rule) used in JSON_report.ml *)
  message : string;
  (* used for debugging (could be removed at some point) *)
  pattern_string : string;
}
[@@deriving show, eq]

(*e: type [[Match_result.t]] *)

(*e: semgrep/core/Pattern_match.ml *)

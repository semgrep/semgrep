(*s: semgrep/core/Tainting_rule.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2020 r2c
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
module R = Rule

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This is a spin-off of Rule.ml but specialized for tainting analysis.
 *
 * At some point we may want tainting to be integrated and queryable
 * directly from regular semgrep rules, but for now it's simpler
 * to have a specialized type and format.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Tainting_rule.pattern]] *)
(* right now only Expr is supported *)
type pattern = AST.any
(*e: type [[Tainting_rule.pattern]] *)

(* less: could extend Rule.t *)
(*s: type [[Tainting_rule.rule]] *)
type rule = {
  id: string;

  (* the list below are used to express disjunction *)
  source: pattern list;
  sanitizer: pattern list;
  sink: pattern list;

  message: string;
  severity: Rule.severity;
  languages: Lang.t list; (* at least one element *)
}
(*e: type [[Tainting_rule.rule]] *)

(*s: type [[Tainting_rule.rules]] *)
and rules = rule list
(*e: type [[Tainting_rule.rules]] *)

(*s: type [[Tainting_rule.t]] *)
(* alias *)
type t = rule
(*e: type [[Tainting_rule.t]] *)


(*s: function [[Tainting_rule.rule_of_tainting_rule]] *)
(* for Match_result.t.rule compatibility *)

let rule_of_tainting_rule tr =
  { R.
    id = tr.id;
    message = tr.message;
    severity = tr.severity;
    languages = tr.languages;
    (* arbitrary *)
    pattern = List.hd (tr.sink)
  }
(*e: function [[Tainting_rule.rule_of_tainting_rule]] *)
(*e: semgrep/core/Tainting_rule.ml *)

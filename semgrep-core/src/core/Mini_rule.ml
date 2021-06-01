(*s: semgrep/core/Mini_rule.ml *)
(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2019 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal of this module is to make it easy to add lint rules by using
 * semgrep patterns. You just have to store the patterns in a special
 * YAML file and add the corresponding warnings you want the linter to raise.
 *
 * update: if you need advanced patterns with boolean logic (which used
 * to be partially provided by the hacky OK error keyword), use
 * instead Rule.ml (or the semgrep python wrapper). Rule.ml also uses a YAML
 * file but it has more features, e.g. some pattern-either fields,
 * pattern-inside, metavariable-comparison, etc.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Rule.pattern]] *)
(*e: type [[Rule.pattern]] *)

(*s: type [[Rule.rule]] *)
type rule = {
  id : string;
  pattern : Pattern.t;
  message : string;
  severity : severity;
  languages : Lang.t list;
  (* at least one element *)
  (* Useful for debugging, to report bad rules. We could rule.id to
   * report those bad rules, but semgrep-python uses a weird encoding
   * when flattening the pattern-xxx (-and, -either, -not, etc.)
   * patterns in the original rule yaml file, which makes it hard
   * to know what what was the corresponding pattern.
   *)
  pattern_string : string;
}

(*e: type [[Rule.rule]] *)

(*s: type [[Rule.rules]] *)
and rules = rule list

(*e: type [[Rule.rules]] *)

(* TODO? just reuse Error_code.severity *)
(*s: type [[Rule.severity]] *)
and severity = Error | Warning | Info
(*e: type [[Rule.severity]] *)
[@@deriving eq, show]

(*s: type [[Rule.t]] *)
(* alias *)
type t = rule [@@deriving show]

(*e: type [[Rule.t]] *)
(*e: semgrep/core/Mini_rule.ml *)

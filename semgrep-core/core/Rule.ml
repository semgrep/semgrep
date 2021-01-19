(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
module MV = Metavariable

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Data structure representing a semgrep rule.
 *
 * See also Mini_rule.ml where formula and many other features disappears.
 *
 * TODO:
 *  - parse more spacegrep and equivalences
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* classic boolean-logic/set operators with text range set semantic *)
type 'a formula =
  | F of 'a
  | X of extra

  | Not of 'a (* could be of 'a formula? *)
  | And of 'a formula list
  | Or of 'a formula list

(* extra conditions, usually on metavariable content *)
and extra =
  | PatRegexp of regexp
  | Spacegrep of spacegrep (* TODO: parse it via spacegrep/lib/ *)

  | MetavarRegexp of MV.mvar * regexp
  | MetavarComparison of metavariable_comparison
  | PatWherePython of string

  (* less: could be done via Not PatRegexp later? *)
  | PatNotRegexp of regexp

and regexp = string

and spacegrep = string

(* See also matching/eval_generic.ml *)
and metavariable_comparison = {
  metavariable: MV.mvar;
  comparison: string;
  strip: bool option;
  base: int option;
}

[@@deriving show]

(* Unorthodox original pattern compositions.
 * See also the JSON schema in rule_schema.yaml
*)
type 'a formula_old =
  (* pattern: *)
  | Pat of 'a
  (* pattern-not: *)
  | PatNot of 'a

  | PatExtra of extra

  (* pattern-inside: *)
  | PatInside of 'a
  (* pattern-not-inside: *)
  | PatNotInside of 'a

  (* pattern-either: *)
  | PatEither of 'a formula_old list
  (* patterns: And? or Or? depends on formula inside, hmmm *)
  | Patterns of 'a formula_old list

[@@deriving show]

type lang =
  | L of Lang.t * Lang.t list
  (* for pattern-regex *)
  | LNone
  (* for spacegrep *)
  | LGeneric
[@@deriving show]

type paths = {
  include_: regexp list;
  exclude: regexp list;
}
[@@deriving show]

type pattern = (Pattern.t, spacegrep) Common.either
[@@deriving show]

type rule = {
  (* mandatory fields *)

  id: string;
  formula: pattern formula_old;
  message: string;
  severity: Mini_rule.severity;
  languages: lang;

  (* optional fields *)
  equivalences: string list; (* TODO: parse them *)

  fix: string option;
  fix_regexp: (regexp * int option * string) option;

  paths: paths option;

  (* ex: [("owasp", "A1: Injection")] *)
  metadata: (string * string) list;
}

and rules = rule list
[@@deriving show]

(* alias *)
type t = rule
[@@deriving show]

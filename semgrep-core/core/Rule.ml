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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Data structure for a semgrep rule.
 *
 * See also Mini_rule.ml where formula disappears.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* classic boolean-logic/set operators with range positions set semantic *)
type 'a formula =
  | F of 'a
  | Not of 'a (* could be of 'a formula? *)
  | And of 'a formula list
  | Or of 'a formula list
[@@deriving show]

(* unorthodox original pattern compositions *)
type 'a formula_old =
  (* pattern: *)
  | Pat of 'a
  (* pattern-not: *)
  | PatNot of 'a

  (* pattern-inside: *)
  | PatInside of 'a
  (* pattern-not-inside: *)
  | PatNotInside of 'a

  (* pattern-either: *)
  | PatEither of 'a formula_old list

  (* patterns: And? or Or? depends on formula inside, hmmm *)
  | PatList of 'a formula_old list
[@@deriving show]

type rule = {
  id: string;
  formula: Pattern.t formula_old;
  message: string;
  severity: Mini_rule.severity;
  languages: Lang.t list; (* at least one element *)

  (* ex: [("owasp", "A1: Injection")] *)
  metadata: (string * string) list;
}

and rules = rule list
[@@deriving show]


(* alias *)
type t = rule
[@@deriving show]

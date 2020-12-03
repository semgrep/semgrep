(*s: semgrep/core/Rule.ml *)
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
 * sgrep patterns. You just have to store in a special file the patterns
 * and the corresponding warning you want the linter to raise.
 *
 * update: if you need advanced patterns with boolean logic (which used
 * to be partially provided by the hacky OK error keyword), use
 * instead the sgrep python wrapper! It also uses a yaml file but it
 * has more features, e.g. some pattern-either fields, pattern-inside,
 * where-eval, etc.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Rule.pattern]] *)
(* right now only Expr, Stmt, and Stmts are supported *)
type pattern = Pattern.t
(*e: type [[Rule.pattern]] *)

(*s: type [[Rule.rule]] *)
type rule = {
  id: string;
  pattern: pattern;
  message: string;
  severity: severity;
  languages: Lang.t list; (* at least one element *)
}
(*e: type [[Rule.rule]] *)

(*s: type [[Rule.rules]] *)
and rules = rule list
(*e: type [[Rule.rules]] *)

(* TODO? just reuse Error_code.severity *)
(*s: type [[Rule.severity]] *)
and severity = Error | Warning | Info
(*e: type [[Rule.severity]] *)

(*s: type [[Rule.t]] *)
(* alias *)
type t = rule
(*e: type [[Rule.t]] *)
(*e: semgrep/core/Rule.ml *)

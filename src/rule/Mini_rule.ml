(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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

type rule = {
  id : Rule_ID.t;
  pattern : Pattern.t;
  inside : bool;
  (* originates from pattern-inside or pattern-not-inside *)
  message : string;
  metadata : JSON.t option;
  severity : Rule.severity;
  langs : Lang.t list;
  (* at least one element *)
  (* Useful for debugging, to report bad rules. We could rule.id to
   * report those bad rules, but semgrep-python uses a weird encoding
   * when flattening the pattern-xxx (-and, -either, -not, etc.)
   * patterns in the original rule yaml file, which makes it hard
   * to know what what was the corresponding pattern.
   *)
  pattern_string : string;
  fix : string option;
  fix_regexp : Rule.fix_regexp option;
}

and rules = rule list [@@deriving show]

(* alias *)
type t = rule [@@deriving show]

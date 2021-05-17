(*s: semgrep/core/Equivalence.ml *)
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
(* The goal of this module is to let the user defines "code equivalences"
 * (a.k.a. isomorphisms).
 *
 * There are lots of equivalences between code and more than one way
 * to perform an operation (TMTOWTDI). For example, when looking for
 * $X == $X, we may want this pattern to also match code like a != a,
 * because this is equivalent to !(a == a).
 *
 * One of the great idea in Coccinelle was to simply reuse the same
 * ideas and machinery to match code to also apply code equivalences!
 * One can write simply $X != $Y <==> !($X == $Y) in a config file
 * and have the engine handles this Equivalence.
 *
 * alternatives:
 *  - macros/templates over the yaml rule file to generate some pattern-either,
 *    but this may be a bit hacky and add yet another layer
 *    (sgrep-core -> sgrep-python -> sgrep-yaml-generator)
 *    As Matt said, adding a templating language, on top of a markup language
 *    on top of a domain specific language, on top of a programming language
 *    is hard to grasp.
 *    update: maybe with jsonnet it's not too bad
 *  - pfff/lang_GENERIC/analize/normalize_ast.ml but this requires
 *    to know OCaml (we go back to the argument of sgrep vs AST visitors)
 *
 * Note that some code equivalences are handled directly in the engine
 * in Generic_vs_generic.ml or normalize_ast.ml because they are too
 * difficult to encode otherwise (e.g., the less-is-ok).
 * todo: we should give them name too, so they can be disabled too.
 *
 * related work:
 *  - standard.iso in coccinelle
 *    https://github.com/coccinelle/coccinelle/blob/master/standard.iso
 *  - paper on semantic equivalences search recently at PLDI
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Equivalence.pattern]] *)
(* right now only Expr is supported *)
type pattern = Pattern.t

(*e: type [[Equivalence.pattern]] *)

(*s: type [[Equivalence.equivalence_kind]] *)
type equivalence_kind =
  | Equiv
  (* <==> *)
  | Imply

(* ==> *)

(*e: type [[Equivalence.equivalence_kind]] *)

(*s: type [[Equivalence.equivalence]] *)
type equivalence = {
  id : string;
  (* useful? to be able to disable some selectively by name? *)
  left : pattern;
  right : pattern;
  op : equivalence_kind;
  languages : Lang.t list; (* at least one element *)
}

(*e: type [[Equivalence.equivalence]] *)

(*s: type [[Equivalence.equivalences]] *)
and equivalences = equivalence list

(*e: type [[Equivalence.equivalences]] *)

(*s: type [[Equivalence.t]] *)
(* alias *)
type t = equivalence

(*e: type [[Equivalence.t]] *)
(*e: semgrep/core/Equivalence.ml *)

(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 Semgrep Inc.
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
 *  - a normalize_ast.ml but this requires
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

(* right now only Expr is supported *)
type pattern = Pattern.t

type equivalence_kind =
  | Equiv
  (* <==> *)
  | Imply

(* ==> *)

type equivalence = {
  id : string;
  (* useful? to be able to disable some selectively by name? *)
  left : pattern;
  right : pattern;
  op : equivalence_kind;
  languages : Lang.t list; (* at least one element *)
}

and equivalences = equivalence list

(* alias *)
type t = equivalence

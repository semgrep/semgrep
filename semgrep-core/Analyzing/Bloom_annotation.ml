(* Emma Jin, Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
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
module B = Bloom_filter

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helper functions for the Nathan's bloom filter optimization to skip
 * matching certain patterns against some code in semgrep.
 *
 * The intuition is that if the pattern contains an identifier like in
 * 'foo();', there is no point in running the Semgrep matching engine on all
 * the statements in the program if those statements do not contain this
 * identifier.
 * The Bloom filter allows to generalize this idea to a set of identifiers
 * that must be present in some statements while still being space-efficient.
 *
 * Note that we must make sure we don't skip statements we should analyze!
 * For example with the naming aliasing, a pattern like 'foo()'
 * should be matched on code like 'bar()' if bar was actualy an alias
 * for foo via a previous import.
 * The same is true for user-defined equivalences when a pattern contains
 * A DisjExpr, in which case we may need a set of Bloom filters for each
 * branch. Another example are integer literals, which in OCaml
 * can be written as 1000 or 1_000, so we want to do the bloom hasking
 * on the integer final value, not the string.
 *
 * See also semgrep-core/matching/Rules_filter.ml
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Use a visitor_AST to extract the strings from all identifiers,
 * and from all literals for now, except all semgrep stuff:
 *  - identifier which are metavariables
 *  - string like "..."
 *  - string like "=~/stuff/"
 *
 * See also Rules_filter.reserved_id and reserved_str and
 * Rules_filter.extract_specific_strings.
*)
let extract_strings _any =
  []

(*****************************************************************************)
(* Analyze the pattern *)
(*****************************************************************************)

let bloom_of_expr _e =
  B.create ()

let bloom_of_stmt _st =
  B.create ()

(*****************************************************************************)
(* Analyze the code *)
(*****************************************************************************)

(* TODO: visit AST and set the s_bf field in statements *)
let annotate_program _ast =
  ()

(* Yoann Padioleau
 *
 * Copyright (c) 2023 Semgrep Inc.
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
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST(s) for Elixir.
 *
 * Elixir is quite an unusual language with a very flexible syntax and
 * macro system. For example, there are no 'definition' or 'declaration'
 * grammar rules. Instead a definition looks really like a function call.
 * This is a bit similar to LISP where '(defun ...)' is not part of
 * the LISP syntax definition; 'defun' is actually a call to a
 * special construct that defines functions!
 * This is why we parse Elixir source in 2 phases:
 *  - phase 1, we parse the "Raw" constructs which roughly correspond to
 *    LISP sexps
 *  - TODO: phase 2, we analyze those raw constructs and try to infer higher-level
 *    constructs like module definitions or function definitions that
 *    are standard in Elixir
 * Note that because our aim is ultimately to transform Elixir code
 * in the generic ASTs, we didn't define an Elixir 'expr' type but
 * instead reuse the one from the generic AST, which makes it
 * easier later in Elixir_to_generic.ml to get back a full generic AST.
 *
 * references:
 * - https://hexdocs.pm/elixir/syntax-reference.html
 *)

(*****************************************************************************)
(* Raw constructs *)
(*****************************************************************************)
(* AST constructs or aliases corresponding to "raw" Elixir constructs.
 *
 * We try to follow the naming conventions in
 * https://hexdocs.pm/elixir/syntax-reference.html
 *)

type 'a wrap = 'a AST_generic.wrap
type 'a bracket = 'a AST_generic.bracket

(* lowercase ident *)
type ident = string wrap

(* uppercase ident *)
type alias = string wrap

(* there is no 'name' below. They use the term 'remote' for qualified calls
 * with lowercase ident.
 * Note that the alias can actually also contain some dots
 * and be also kinda of a name.
 *)

type expr = AST_generic.expr
type argument = AST_generic.argument

(* exprs separated by terminators (newlines or semicolons)
 * can be empty.
 *)
type body = expr list

(* less: restrict with special arg? *)
type call = expr

(* Ideally we would want just 'type keyword = ident * tok (*:*)',
 * but Elixir allows also "interpolated{ x }string": keywords.
 *)
type keyword = expr

(* note that Elixir supports also pairs using the (:atom => expr) syntax *)
type pair = keyword * expr

(* inside containers (list, bits, maps, tuples), separated by commas *)
type item = expr

(* Ideally it should be pattern list * tok * body option, but Elixir
 * is more general and use '->' also for type declarations in typespecs,
 * or for parameters (kind of patterns though).
 *)
type stab_clause =
  (argument list * (Tok.t (*'when'*) * expr) option) * Tok.t (* '->' *) * body

type clauses = stab_clause list

(* in after/rescue/catch/else and do blocks *)
type body_or_clauses = (body, clauses) either

(* the bracket here are do/end *)
type do_block =
  (body_or_clauses
  * (ident (* 'after/rescue/catch/else' *) * body_or_clauses) list)
  bracket

(* the bracket here are () *)
type block = body_or_clauses bracket

(*****************************************************************************)
(* Refined constructs *)
(*****************************************************************************)
(* TODO *)

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
module G = AST_generic

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
 *
 * references:
 * - https://hexdocs.pm/elixir/syntax-reference.html
 * - https://hexdocs.pm/elixir/Kernel.html
 *)

(*****************************************************************************)
(* Raw constructs *)
(*****************************************************************************)
(* AST constructs or aliases corresponding to "raw" Elixir constructs.
 *
 * We try to follow the naming conventions in
 * https://hexdocs.pm/elixir/syntax-reference.html
 *)

(* ------------------------------------------------------------------------- *)
(* Tokens *)
(* ------------------------------------------------------------------------- *)
type 'a wrap = 'a G.wrap
type 'a bracket = 'a G.bracket

(* ------------------------------------------------------------------------- *)
(* Names *)
(* ------------------------------------------------------------------------- *)

type ident =
  (* lowercase ident *)
  | Id of string wrap
  (* actually part of Elixir! *)
  | IdEllipsis of Tok.t (* '...' *)
  (* semgrep-ext: *)
  | IdMetavar of string wrap

(* uppercase ident; constructs that expand to atoms at compile-time *)
type alias = string wrap

(* ref: https://hexdocs.pm/elixir/operators.html *)
type operator =
  (* special forms operators that cannot be overriden *)
  | OPin (* ^ *)
  | ODot (* . *)
  | OMatch (* = *)
  | OCapture (* & *)
  | OType (* :: *)
  (* strict boolean variants *)
  | OStrictAnd
  | OStrictOr
  | OStrictNot
  (* other operators *)
  | OPipeline (* |> *)
  | OModuleAttr (* @ *)
  | OLeftArrow (* <-, used with 'for' and 'with'  *)
  | ODefault (* \\, default argument *)
  | ORightArrow (* -> *)
  | OCons (* |, =~ "::" in OCaml (comes from Erlang/Prolog) *)
  | OWhen (* when, in guards *)
  | O of G.operator
  | OOther of string

(* start of recursive type because atoms can contain interpolated exprs *)

(* TODO: need extract ':' for simple ident case *)
type atom = Tok.t (* ':' *) * string wrap or_quoted

(* TODO
         G.L (G.Atom (G.fake ":", x)) |> G.e
   ...
         let str = map_anon_choice_quoted_i_double_d7d5f65 env v2 in
         G.OtherExpr (("AtomExpr", t), [ E str ]) |> G.e
*)
and 'a or_quoted = X of 'a | Quoted of quoted
and quoted = expr bracket

(* TODO: They use the term 'remote' for qualified calls with lowercase ident *)
(* TODO: remote *)

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)

(* TODO
   when binaryop:
         OtherExpr
           ( ("When", twhen),
             [ E e1; E (Elixir_to_generic.expr_of_e_or_kwds e_or_kwds) ] )
         |> G.e
   consbinaryop:
         G.OtherExpr
           ( ("Join", tbar),
             [ G.E e1; G.E (Elixir_to_generic.expr_of_e_or_kwds e_or_kwds) ] )
         |> G.e

         let e = G.L (G.Int (int_of_string_opt s, t)) |> G.e in
         G.OtherExpr (("OpSlashInt", tslash), [ G.I id; G.E e ]) |> G.e
   /int binaryop:
         let e = G.L (G.Int (int_of_string_opt s, t)) |> G.e in
         G.OtherExpr (("OpSlashInt", tslash), [ G.I id; G.E e ]) |> G.e
*)
and expr =
  | I of ident
  | L of G.literal
  | Alias of alias
  | Block of block
  (* semgrep-ext: *)
  | DeepEllipsis of expr bracket

and argument = G.argument

(* Ideally we would want just 'type keyword = ident * tok (*:*)',
 * but Elixir allows also "interpolated{ x }string": keywords.
 *)
and keyword = expr

(* note that Elixir supports also pairs using the (:atom => expr) syntax *)
and pair = keyword * expr
and keywords = pair list
and expr_or_kwds = E of expr | Kwds of keywords

(* exprs separated by terminators (newlines or semicolons) *)
and body = expr list

(* less: restrict with special arg? *)
and call = expr
and remote_dot = expr * Tok.t (* '.' *) * ident or_quoted

(* TODO
   map_anonymous_call:
     let anon_fld = G.FDynamic (G.OtherExpr (("AnonDotField", tdot), []) |> G.e) in
     let e = G.DotAccess (e, tdot, anon_fld) |> G.e in
*)

(* inside containers (list, bits, maps, tuples), separated by commas *)
and item = expr

(* ------------------------------------------------------------------------- *)
(* Clauses *)
(* ------------------------------------------------------------------------- *)

(* Ideally it should be pattern list * tok * body option, but Elixir
 * is more general and use '->' also for type declarations in typespecs,
 * or for parameters (kind of patterns though).
 *)
and stab_clause =
  (argument list * (Tok.t (*'when'*) * expr) option) * Tok.t (* '->' *) * body

and clauses = stab_clause list

(* in after/rescue/catch/else and do blocks *)
and body_or_clauses =
  | Body of body
  (* can be empty *)
  | Clauses of clauses

(* ------------------------------------------------------------------------- *)
(* Blocks *)
(* ------------------------------------------------------------------------- *)

(* the bracket here are do/end *)
and do_block =
  (body_or_clauses * (exn_clause_kind wrap * body_or_clauses) list) bracket

and exn_clause_kind = After | Rescue | Catch | Else

(* the bracket here are () *)
and block = body_or_clauses bracket

type program = body

(*****************************************************************************)
(* Kernel constructs *)
(*****************************************************************************)
(* ref: https://hexdocs.pm/elixir/Kernel.html *)

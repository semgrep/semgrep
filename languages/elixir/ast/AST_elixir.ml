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
(* AST constructs corresponding to "raw" Elixir constructs.
 *
 * We try to follow the naming conventions in
 * https://hexdocs.pm/elixir/syntax-reference.html
 *)

(* ------------------------------------------------------------------------- *)
(* Tokens *)
(* ------------------------------------------------------------------------- *)
type 'a wrap = 'a * Tok.t [@@deriving show]
type 'a bracket = Tok.t * 'a * Tok.t [@@deriving show]

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
[@@deriving show { with_path = false }]

(* uppercase ident; constructs that expand to atoms at compile-time
 * TODO: seems like it contain contains string with dots! Foo.Bar is
 * parsed as a single alias, so maybe we need to inspect it and split it.
 *)
type alias = string wrap [@@deriving show]

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
  (* lots of operators here, +++, ---, etc. *)
  | OOther of string
[@@deriving show { with_path = false }]

type ident_or_operator = (ident, operator wrap) Common.either [@@deriving show]

(* start of BIG recursive type because atoms can contain interpolated exprs *)

(* TODO: need extract ':' for simple ident case *)
type atom = Tok.t (* ':' *) * string wrap or_quoted

(* TODO: need to extract the ':' for the ident case *)
and keyword = string wrap or_quoted * Tok.t (* : *)
and 'a or_quoted = X of 'a | Quoted of quoted
and quoted = (string wrap, expr bracket) Common.either list bracket

(* ------------------------------------------------------------------------- *)
(* Keywords and arguments *)
(* ------------------------------------------------------------------------- *)
(* inside calls *)
and arguments = expr list * keywords

(* inside containers (list, bits, maps, tuples), separated by commas *)
and items = expr list * keywords

(* Elixir semantic is to unsugar in regular (atom, expr) pair *)
and keywords = pair list

(* note that Elixir supports also pairs using the (:atom => expr) syntax *)
and pair = keyword * expr
and expr_or_kwds = E of expr | Kwds of keywords

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

   for Capture:
         AST_generic_helpers.set_e_range v1 v3 v2;

   map_unary_operator:
   plus
         let e1 = N (H2.name_of_id id) |> G.e in
         Elixir_to_generic.mk_call_no_parens e1 [ G.arg e2 ] None
   @
         OtherExpr (("AttrExpr", tat), [ E e ]) |> G.e
   &1
         let e = L (Int (int_of_string_opt s, t)) |> G.e in
         OtherExpr (("Shortcut", tand), [ E e ]) |> G.e
*)
and expr =
  (* lowercase idents *)
  | I of ident
  (* uppercase idents *)
  | Alias of alias
  | L of G.literal
  | A of atom
  | String of quoted
  | Charlist of quoted
  | Sigil of Tok.t (* '~' *) * sigil_kind * string wrap option
  | List of items bracket
  | Tuple of items bracket
  | Bits of items bracket
  | Map of Tok.t (* "%" *) * astruct option * items bracket
  | Block of block
  | DotAlias of expr * Tok.t * alias
  | DotTuple of expr * Tok.t * items bracket
  (* only inside Call *)
  | DotAnon of expr * Tok.t
  (* only inside Call *)
  | DotRemote of remote_dot
  | ModuleVarAccess of Tok.t (* @ *) * expr
  | ArrayAccess of expr * expr bracket
  | Call of call
  | UnaryOp of operator wrap * expr
  | BinaryOp of expr * operator wrap * expr
  (* coming from Erlang (comint itself from Prolog) *)
  | OpArity of operator wrap * Tok.t (* '/' *) * int option wrap
  | When of expr * Tok.t (* 'when' *) * expr_or_kwds
  | Join of expr * Tok.t (* '|' *) * expr_or_kwds
  (* let fdef =
       Elixir_to_generic.stab_clauses_to_function_definition tfn clauses
     in
     let fdef = { fdef with fkind = (LambdaKind, tfn) } in
     Lambda fdef |> G.e
  *)
  | Lambda of Tok.t (* 'fn' *) * clauses * Tok.t (* 'end' *)
  (*
      OtherExpr (("Shortcut", tand), [ E e ]) |> G.e
*)
  | Capture of Tok.t (* '&' *) * expr
  | ShortLambda of Tok.t (* '&' *) * expr bracket
  | PlaceHolder of Tok.t (* & *) * int option wrap
  (* semgrep-ext: *)
  | DeepEllipsis of expr bracket

(* restricted to Alias/A/I/DotAlias/DotTuple and all unary op *)
and astruct = expr

and sigil_kind =
  | Lower of char wrap * quoted
  | Upper of char wrap * string wrap bracket

(* the parenthesis can be fake *)
and call = expr * arguments bracket * do_block option
and remote_dot = expr * Tok.t (* '.' *) * ident_or_operator or_quoted

(* ------------------------------------------------------------------------- *)
(* Blocks *)
(* ------------------------------------------------------------------------- *)

(* the bracket here are () *)
and block = body_or_clauses bracket [@@deriving show { with_path = false }]

(* in after/rescue/catch/else and do blocks *)
and body_or_clauses =
  | Body of body
  (* can be empty *)
  | Clauses of clauses

(* exprs separated by terminators (newlines or semicolons) *)
and body = stmt list

(* The bracket here are do/end.
 * Elixir semantic is to unsugar in a list of pairs with "do:", "after:",
 * as the keys.
 *)
and do_block =
  (body_or_clauses * (exn_clause_kind wrap * body_or_clauses) list) bracket

and exn_clause_kind = After | Rescue | Catch | Else

(* ------------------------------------------------------------------------- *)
(* Clauses *)
(* ------------------------------------------------------------------------- *)
and clauses = stab_clause list

(* Ideally it should be pattern list * tok * body option, but Elixir
 * is more general and use '->' also for type declarations in typespecs,
 * or for parameters (kind of patterns though).
 *)
and stab_clause =
  (arguments * (Tok.t (*'when'*) * expr) option) * Tok.t (* '->' *) * body

(*****************************************************************************)
(* Kernel constructs *)
(*****************************************************************************)
(* ref: https://hexdocs.pm/elixir/Kernel.html *)
and stmt = expr

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

type program = body [@@deriving show]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any = Pr of program [@@deriving show { with_path = false }]

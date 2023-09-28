(* Yoann Padioleau
 * Iago Abal
 *
 * Copyright (C) 2019-2022 r2c
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
(* Intermediate Language (IL) for static analysis.
 *
 * Just like for the CST -> AST, the goal of an AST -> IL transformation
 * is to simplify things even more for program analysis purposes.
 *
 * Here are the simplifications done compared to the generic AST:
 *  - intermediate 'instr' type (instr for instruction), for expressions with
 *    side effects and statements without any control flow,
 *    moving Assign/Seq/Call/Conditional out of 'expr' and
 *    moving Assert out of 'stmt'
 *  - new expression type 'exp' for side-effect free expressions
 *  - intermediate 'lvalue' type; expressions are split into
 *    lvalue vs regular expressions, moved Dot/Index out of expr
 *
 *  - Assign/Calls are now instructions, not expressions, and no more Seq
 *  - no AssignOp, or Decr/Incr, just Assign
 *  - Lambdas are now instructions (not nested again)
 *
 *  - no For/Foreach/DoWhile/While, converted all to Loop,
 *  - no Foreach, converted to a Loop and 2 new special
 *  - no Switch, converted to Ifs
 *  - no Continue/Break, converted to goto
 *  - less use of expr option (in Return/Assert/...), use Unit in those cases
 *
 *  - no Sgrep constructs
 *  - Naming has been performed, no more ident vs name
 *
 * TODO:
 *   - TODO? have all arguments of Calls be variables?
 *
 *
 * Note that we still want to be close to the original code so that
 * error reported on the IL can be mapped back to error on the original code
 * (source "maps"), or more importantly semantic information computed
 * on the IL (e.g., types, svalue, match range, taint) can be mapped back
 * to the generic AST.
 * This is why you will see some 'eorig', 'iorig' fields below and the use of
 * refs such as svalue shared with the generic AST.
 * TODO? alt: store just the range and id_info_id, so easy to propagate back
 * info to generic AST or to return match ranges to semgrep.
 *
 * history:
 *  - cst_php.ml (was actually called ast_php.ml)
 *  - ast_php.ml (was called ast_php_simple.ml)
 *  - pil.ml, still for PHP
 *  - IL.ml for AST generic
 *
 * related work:
 *  - CIL, C Intermediate Language, Necula et al, CC'00
 *  - RIL, The Ruby Intermediate Language, Furr et al, DLS'09
 *  - SIL? in Infer? or more a generic AST than a generic IL?
 *  - Rust IL?
 *  - C-- in OCaml? too low-level?
 *  - LLVM IR (but too far away from original code? complicated
 *    source maps)
 *  - BRIL https://capra.cs.cornell.edu/bril/ used for teaching
 *  - gcc RTL (too low-level? similar to 3-address code?)
 *  - SiMPL language in BAP/BitBlaze dynamic analysis libraries
 *    but probably too close to assembly/bytecode
 *  - Jimpl in Soot/Wala
 *)

(*****************************************************************************)
(* Token (leaf) *)
(*****************************************************************************)

(* the classic *)
type tok = G.tok [@@deriving show]
type 'a wrap = 'a G.wrap [@@deriving show]

(* useful mainly for empty containers *)
type 'a bracket = tok * 'a * tok [@@deriving show]

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

type ident = G.ident [@@deriving show]

(* 'sid' below is the result of name resolution and variable disambiguation
 * using a gensym (see Naming_AST.ml). A name is guaranteed to be
 * global and unique (no need to handle variable shadowing, block scoping,
 * etc; this has been done already).
 * TODO: use it to also do SSA! so some control-flow insensitive analysis
 * can become control-flow sensitive? (e.g., DOOP)
 *
 *)
type name = { ident : ident; sid : G.sid; id_info : G.id_info }
[@@deriving show]

(*****************************************************************************)
(* Fixme constructs *)
(*****************************************************************************)

(* AST-to-IL translation is still a work in progress. When we encounter some
 * that we cannot handle, we insert a [FixmeExp], [FixmeInstr], or [FixmeStmt].
 *)
type fixme_kind =
  | ToDo (* some construct that we still don't support *)
  | Sgrep_construct (* some Sgrep construct shows up in the code, e.g. `...' *)
  | Impossible (* something we thought impossible happened *)
[@@deriving show]

(*****************************************************************************)
(* Mapping back to Generic *)
(*****************************************************************************)

(* Only use `SameAs` when the IL expression or instruction is indeed "the same as"
 * (i.e., semantically equivalent to) that Generic expression.
 *
 * When an IL expression is derived from some other Generic expression or
 * construct, but not semantically equivalent, you can use `Related`. Note that
 * this info will be used by taint-tracking to match sources/sanitizers/sinks;
 * so make sure that the annotation makes sense, or it could lead to weird taint
 * findings.
 *
 * When no orig info is needed, or it cannot be provided, then use NoOrig. For
 * example, auxiliary assignments where the RHS has the right orig-info can be
 * annotated with NoOrig. This also helps making -dump_il more readable.
 *)
type orig = SameAs of G.expr | Related of G.any | NoOrig
[@@deriving show { with_path = false }]

let related_tok tok = Related (G.Tk tok)
let related_exp exp_gen = Related (G.E exp_gen)

let any_of_orig = function
  | SameAs e -> G.E e
  | Related any -> any
  | NoOrig -> G.Anys []

(*****************************************************************************)
(* Lvalue *)
(*****************************************************************************)

type lval = { base : base; rev_offset : offset list }
(** An lvalue, represented similarly as in CIL as a pair: base and offsets.

  The offset list is *reversed*, so the first element in this list is the _last_
  offset!

  old: Previously we only kept one offset in lval and instead we used auxiliary
       variables. But then we added field sensitivity to taint-mode and all those
       extra variables became a problem, they would force us to add alias analysis
       to handle even trivial cases of field sensitivity. *)

and base =
  | Var of name
  | VarSpecial of var_special wrap
  (* aka DeRef, e.g. *E in C *)
  (* THINK: Mem of exp -> Deref of name *)
  | Mem of exp

and offset = {
  o : offset_kind;
  oorig : orig;
      (** `oorig' should be a DotAccess expression and gives us the corresponding
      * Generic expression for a sub-lvalue. Now that we represent `x.a.b.c` as
      * { base = "x"; rev_offsets = ["c"; "b"; "a"]}, it makes it very easy to
      * check whether a sub-lvalue is a source/santizer/sink by just checking
      * the range of the `oorig'.
      *
      * alt: We could compute the range of the sub-lvalue from the ranges of all
      *      the offsets in the sub-lvalue, but this is probably less efficent
      *      unless we cache the range here. So it seems better to have `oorig'.
      *)
}

and offset_kind =
  (* What about computed field names?
   * - handle them in Special?
   * - convert in Index with a string exp?
   * Note that Dot is used to access many different kinds of entities:
   *  objects/records (fields), classes (static fields), but also
   *  packages, modules, namespaces depending on the type of 'var' above.
   *
   * old: Previously the field was an `ident` but in some cases we want a
   *      proper (resolved) name here. For example, we want to resolve this.foo()
   *      to the class method "foo"; this is useful for our poor's man
   *      interprocedural analysis.
   *)
  | Dot of name
  | Index of exp

(* transpile at some point? *)
and var_special = This | Super | Self | Parent

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* We use 'exp' instead of 'expr' to accentuate the difference
 * with AST_generic.expr.
 * Here 'exp' does not contain any side effect!
 * todo: etype: typ;
 *)
and exp = { e : exp_kind; eorig : orig }

and exp_kind =
  | Fetch of lval (* lvalue used in a rvalue context *)
  | Literal of G.literal
  | Composite of composite_kind * exp list bracket
  (* Record could be a Composite where the arguments are CTuple with
   * the Literal (String) as a key, but they are pretty important I think
   * for some analysis so better to support them more directly.
   * TODO should we transform that in a New followed by a series of Assign
   * with Dot? simpler?
   * This could also be used for Dict.
   *)
  | Record of field list
  | Cast of G.type_ * exp
  (* This could be put in call_special, but dumped IL are then less readable
   * (they are too many intermediate _tmp variables then) *)
  | Operator of G.operator wrap * exp argument list
  | FixmeExp of
      fixme_kind
      * G.any (* fixme source *)
      * exp (* partial translation *) option

and field = Field of ident * exp | Spread of exp

and composite_kind =
  | CTuple
  | CArray
  | CList
  | CSet
  | CDict (* could be merged with Record *)
  | Constructor of name (* OCaml *)
  | Regexp

and 'a argument = Unnamed of 'a | Named of ident * 'a
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* THINK: Types contain expressions that we want to check (see e.g. 'TyExpr'), but
 * right now we don't need/want to duplicate the type 'type_' just for this
 * (perhaps we could parameterize it?). *)
type type_ = {
  type_ : G.type_;
  exps : exp list;
      (* IL translation of the expressions in `type_`, we want to check them because
       * these could be sinks! E.g. in PHP you can have `new $cons(args)` and the
       * constructor $cons could be a sink. See 'tests/rules/misc_php_new_taint.php'. *)
}
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Instruction *)
(*****************************************************************************)

(* Easier type to compute lvalue/rvalue set of a too general 'expr', which
 * is now split into  instr vs exp vs lval.
 *)
type instr = { i : instr_kind; iorig : orig }

and instr_kind =
  (* was called Set in CIL, but a bit ambiguous with Set module *)
  | Assign of lval * exp
  | AssignAnon of lval * anonymous_entity
  | Call of lval option * exp (* less: enforce lval? *) * exp argument list
  | CallSpecial of lval option * call_special wrap * exp argument list
  | New of lval * type_ * exp option (* constructor *) * exp argument list
  (* todo: PhiSSA! *)
  | FixmeInstr of fixme_kind * G.any

and call_special =
  | Eval
  (* TODO: lift up like in AST_generic *)
  | Typeof
  | Instanceof
  | Sizeof
  (* old: better in exp: | Operator of G.arithmetic_operator *)
  | Concat (* THINK: Normalize as a Operator G.Concat ? *)
  | Spread
  | Yield
  | Await
  (* was in stmt before, but with a new clean 'instr' type, better here *)
  | Assert
  (* was in expr before (only in C/PHP) *)
  | Ref (* TODO: lift up, have AssignRef? *)
  (* when transpiling certain features (e.g., patterns, foreach) *)
  | ForeachNext
  | ForeachHasNext
  (* JS: require('foo') *)
  | Require

(* primitives called under the hood *)

(* | IntAccess of composite_kind * int (* for tuples/array/list *)
   | StringAccess of string (* for records/hashes *)
*)
and anonymous_entity =
  | Lambda of function_definition
  | AnonClass of G.class_definition

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt = { s : stmt_kind (* sorig: G.stmt; ?*) }

and stmt_kind =
  | Instr of instr
  (* Switch are converted to a series of If *)
  | If of tok * exp * stmt list * stmt list
  (* While/DoWhile/For are converted in a unified Loop construct.
   * Break/Continue are handled via Label.
   * alt: we could go further and transform in If+Goto, but nice to
   * not be too far from the original code.
   *)
  | Loop of tok * exp * stmt list
  | Return of tok * exp (* use Unit instead of 'exp option' *)
  (* alt: do as in CIL and resolve that directly in 'Goto of stmt' *)
  | Goto of tok * label
  | Label of label
  | Try of
      stmt list
      * (name * stmt list) list (* catches *)
      * stmt list (* else *)
      * stmt list (* finally *)
  | Throw of tok * exp (* less: enforce lval here? *)
  | MiscStmt of other_stmt
  | FixmeStmt of fixme_kind * G.any

and other_stmt =
  (* everything except VarDef (which is transformed in an Assign instr) *)
  | DefStmt of G.definition
  | DirectiveStmt of G.directive
  | Noop of (* for debugging purposes *) string

and label = ident * G.sid

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)
(* See AST_generic.ml *)
and function_definition = {
  fparams : name list;
  frettype : G.type_ option;
  fbody : stmt list;
}
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Control-flow graph (CFG) *)
(*****************************************************************************)
(* Similar to controlflow.ml, but with a simpler node_kind.
 * See controlflow.ml for more information. *)
type node = {
  n : node_kind;
      (* old: there are tok in the nodes anyway
       * t: Parse_info.t option;
       *)
}

and node_kind =
  | Enter
  | Exit
  (* 'TrueNode' and 'FalseNode' follow 'NCond', and the 'exp' is the same
   * condition as in 'NCond'. *)
  | TrueNode of exp (* same as in Cond *)
  | FalseNode of exp (* same as in Cond *)
  | Join (* after Cond *)
  | NInstr of instr
  | NCond of tok * exp
  | NGoto of tok * label
  | NReturn of tok * exp
  | NThrow of tok * exp
  | NLambda of name list (* just the params, the body nodes follow this one *)
  | NOther of other_stmt
  | NTodo of stmt
[@@deriving show { with_path = false }]

(* For now there is just one kind of edge.
 * (we may use more? the "ShadowNode" idea of Julia Lawall?)
 *)
type edge = Direct
type cfg = (node, edge) CFG.t

(* an int representing the index of a node in the graph *)
type nodei = Ograph_extended.nodei

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type any = L of lval | E of exp | I of instr | S of stmt | Ss of stmt list
(*  | N of node *)
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* L/Rvalue helpers *)
(*****************************************************************************)
(* see IL_helpers.ml *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let str_of_name name = fst name.ident
let str_of_label ((n, _), _) = n

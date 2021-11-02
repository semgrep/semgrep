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
 * on the IL (e.g., types, constness) can be mapped back to the generic AST.
 * This is why you will see some 'eorig', 'iorig' fields below and the use of
 * refs such as constness shared with the generic AST.
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
(* Lvalue *)
(*****************************************************************************)

(* An lvalue, represented as in CIL as a pair. *)
type lval = { base : base; offset : offset }

and base =
  | Var of name
  | VarSpecial of var_special wrap
  (* aka DeRef, e.g. *E in C *)
  (* THINK: Mem of exp -> Deref of name *)
  | Mem of exp

and offset =
  | NoOffset
  (* What about nested field access? foo.x.y?
   * - use intermediate variable for that. TODO? same semantic?
   * - do as in CIL and have recursive offset and stop with NoOffset.
   * What about computed field names?
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
and exp = { e : exp_kind; eorig : G.expr }

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
  | Record of (ident * exp) list
  | Cast of G.type_ * exp
  (* This could be put in call_special, but dumped IL are then less readable
   * (they are too many intermediate _tmp variables then) *)
  | Operator of G.operator wrap * exp list
  | FixmeExp of fixme_kind * G.any

and composite_kind =
  | CTuple
  | CArray
  | CList
  | CSet
  | CDict (* could be merged with Record *)
  | Constructor of name (* OCaml *)
[@@deriving show { with_path = false }]

type argument = exp [@@deriving show]

(*****************************************************************************)
(* Instruction *)
(*****************************************************************************)

(* Easier type to compute lvalue/rvalue set of a too general 'expr', which
 * is now split into  instr vs exp vs lval.
 *)
type instr = { i : instr_kind; iorig : G.expr }

and instr_kind =
  (* was called Set in CIL, but a bit ambiguous with Set module *)
  | Assign of lval * exp
  | AssignAnon of lval * anonymous_entity
  | Call of lval option * exp (* less: enforce lval? *) * argument list
  | CallSpecial of lval option * call_special wrap * argument list
  (* todo: PhiSSA! *)
  | FixmeInstr of fixme_kind * G.any

and call_special =
  | Eval
  (* Note that in some languages (e.g., Python) some regular calls are
   * actually New under the hood.
   * The type_ argument is usually a name, but it can also be an name[] in
   * Java/C++.
   *)
  | New (* TODO: lift up and add 'of type_ * argument list'? *)
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

(* primitives called under the hood *)

(* | IntAccess of composite_kind * int (* for tuples/array/list *)
   | StringAccess of string (* for records/hashes *)
*)
and anonymous_entity =
  | Lambda of G.function_definition
  | AnonClass of G.class_definition
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
type stmt = { s : stmt_kind (* sorig: G.stmt; ?*) }

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
  | Try of stmt list * (name * stmt list) list (* catches *) * stmt list (* finally *)
  | Throw of tok * exp (* less: enforce lval here? *)
  | MiscStmt of other_stmt
  | FixmeStmt of fixme_kind * G.any

and other_stmt =
  (* everything except VarDef (which is transformed in a Set instr) *)
  | DefStmt of G.definition
  | DirectiveStmt of G.directive
  | Noop

and label = ident * G.sid [@@deriving show { with_path = false }]

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)
(* See AST_generic.ml *)

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
  | TrueNode
  | FalseNode (* for Cond *)
  | Join (* after Cond *)
  | NInstr of instr
  | NCond of tok * exp
  | NGoto of tok * label
  | NReturn of tok * exp
  | NThrow of tok * exp
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
let lval_of_instr_opt x =
  match x.i with
  | Assign (lval, _)
  | AssignAnon (lval, _)
  | Call (Some lval, _, _)
  | CallSpecial (Some lval, _, _) ->
      Some lval
  | Call _
  | CallSpecial _ ->
      None
  | FixmeInstr _ -> None

let lvar_of_instr_opt x =
  let open Common in
  lval_of_instr_opt x >>= fun lval ->
  match lval.base with
  | Var n -> Some n
  | VarSpecial _
  | Mem _ ->
      None

let exps_of_instr x =
  match x.i with
  | Assign (_, exp) -> [ exp ]
  | AssignAnon _ -> []
  | Call (_, e1, args) -> e1 :: args
  | CallSpecial (_, _, args) -> args
  | FixmeInstr _ -> []

let rexps_of_instr x =
  match x.i with
  | Assign (_, exp) -> [ exp ]
  | AssignAnon _ -> []
  | Call (_, e1, args) -> e1 :: args
  | CallSpecial (_, _, args) -> args
  | FixmeInstr _ -> []

(* opti: could use a set *)
let rec lvals_of_exp e =
  match e.e with
  | Fetch lval -> lval :: lvals_in_lval lval
  | Literal _ -> []
  | Cast (_, e) -> lvals_of_exp e
  | Composite (_, (_, xs, _))
  | Operator (_, xs) ->
      lvals_of_exps xs
  | Record ys -> lvals_of_exps (ys |> List.map snd)
  | FixmeExp _ -> []

and lvals_in_lval lval =
  let base_lvals =
    match lval.base with
    | Mem e -> lvals_of_exp e
    | _else_ -> []
  in
  let offset_lvals =
    match lval.offset with
    | Index e -> lvals_of_exp e
    | __else_ -> []
  in
  base_lvals @ offset_lvals

and lvals_of_exps xs = xs |> List.map lvals_of_exp |> List.flatten

(** The lvals in the RHS of the instruction. *)
let rlvals_of_instr x =
  let exps = rexps_of_instr x in
  lvals_of_exps exps

let rvars_of_instr x =
  x |> rlvals_of_instr
  |> Common.map_filter (function
       | { base = Var var; _ } -> Some var
       | ___else___ -> None)

let rlvals_of_node = function
  | Enter
  | Exit
  | TrueNode
  | FalseNode
  | NGoto _
  | Join ->
      []
  | NInstr x -> rlvals_of_instr x
  | NCond (_, e)
  | NReturn (_, e)
  | NThrow (_, e) ->
      lvals_of_exp e
  | NOther _
  | NTodo _ ->
      []

let lval_of_node_opt = function
  | NInstr x -> lval_of_instr_opt x
  | Enter
  | Exit
  | TrueNode
  | FalseNode
  | NGoto _
  | Join
  | NCond _
  | NReturn _
  | NThrow _
  | NOther _
  | NTodo _ ->
      None

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let str_of_name name = fst name.ident

let str_of_label ((n, _), _) = n

let find_node f cfg =
  cfg#nodes#tolist
  |> Common.find_some (fun (nodei, node) -> if f node then Some nodei else None)

let find_exit cfg = find_node (fun node -> node.n = Exit) cfg

let find_enter cfg = find_node (fun node -> node.n = Enter) cfg

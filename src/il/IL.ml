(* Yoann Padioleau, Iago Abal
 *
 * Copyright (C) 2019-2022 Semgrep Inc.
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
(** Intermediate Language (IL) for static analysis.

  = Key Design Principles =

  1. 3-address-code intermediate representation inspired by CIL.
      Complex expressions with side effects are broken down into
      sequences of simpler statements.
     Example: f(g()) becomes: tmp1 = g(); tmp2 = f(tmp1)

  2. Control Flow de-sugaring: control structures are translated into
     plain loops and ifs.

  3. Error Recovery: Unsupported or problematic constructs are wrapped in
     {!fixme} nodes ({!FixmeExp}, {!FixmeInstr}, {!FixmeStmt}.
     Please refer to {!fixme_kind} for kinds of Fixme nodes.

  4. Connection to original source code.
     To generate findings, IL and semantic information computed
     on the IL (e.g., types, svalue, match range, taint) can be mapped back
     to the generic AST (source "maps").
     To do this, IL records have 'eorig', 'iorig' fields and use of refs to
     generic AST objects such as svalue.

  = Additional simplifications =
   - instruction type: {!instr}, for expressions with
     side effects and statements without any control flow,
     moving Assign/Seq/Call/Conditional out of 'expr' and
     moving Assert out of 'stmt'
   - expression type: {!exp} for side-effect free expressions
   - intermediate {!lvalue} type; expressions are split into
     lvalue vs regular expressions, moved Dot/Index out of expr
   - Assign/Calls are instructions, not expressions, and no more Seq
   - no AssignOp, or Decr/Incr, just Assign
   - Lambdas are instructions (not nested again)
   - less use of expr option (in Return/Assert/...), use Unit in those cases

   - no Sgrep constructs
   - Naming has been performed, no more ident vs name

  = TODO =
    - Have all arguments of Calls be variables?
    - Store just the range and id_info_id, so easy to propagate back
      info to generic AST or to return match ranges to semgrep?

  history:
   - cst_php.ml (was actually called ast_php.ml)
   - ast_php.ml (was called ast_php_simple.ml)
   - pil.ml, still for PHP
   - IL.ml for AST generic

  related work:
   - CIL, C Intermediate Language, Necula et al, CC'00
   - RIL, The Ruby Intermediate Language, Furr et al, DLS'09
   - SIL? in Infer? or more a generic AST than a generic IL?
   - Rust IL?
   - C-- in OCaml? too low-level?
   - LLVM IR (but too far away from original code? complicated
     source maps)
   - BRIL https://capra.cs.cornell.edu/bril/ used for teaching
   - gcc RTL (too low-level? similar to 3-address code?)
   - SiMPL language in BAP/BitBlaze dynamic analysis libraries
     but probably too close to assembly/bytecode
   - Jimpl in Soot/Wala
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

(*
 * TODO: use it to also do SSA! so some control-flow insensitive analysis
 * can become control-flow sensitive? (e.g., DOOP)
 *
 *)
type name = {
  ident : ident;  (** In-code identifier *)
  sid : G.sid;
      (** Globally unique identifier for analysis.
          Result of name resolution and variable disambiguation using a gensym (see Naming_AST.ml).
          No need to handle variable shadowing, block scoping, etc; this has been done already).
          {!Gensym.MkId.unsafe_default} if unresolved. *)
  id_info : G.id_info;  (** Additional information about this identifier *)
}
[@@deriving show]

let str_of_name name = Common.spf "%s:%s" (fst name.ident) (G.SId.show name.sid)

let equal_name name1 name2 =
  let { ident = str1, _tok1; sid = sid1; id_info = _ } = name1 in
  let { ident = str2, _tok2; sid = sid2; id_info = _ } = name2 in
  G.SId.equal sid1 sid2 && String.equal str1 str2

let compare_name name1 name2 =
  let { ident = str1, _tok1; sid = sid1; id_info = _ } = name1 in
  let { ident = str2, _tok2; sid = sid2; id_info = _ } = name2 in
  match G.SId.compare sid1 sid2 with
  | 0 -> String.compare str1 str2
  | cmp -> cmp

module NameOrdered = struct
  type t = name

  let compare = compare_name
end

module NameSet : sig
  include Set.S with type elt = name

  val show : t -> string
end = struct
  include Set.Make (NameOrdered)

  let show nameset =
    "{"
    ^ (nameset |> elements |> List.map str_of_name |> String.concat ", ")
    ^ "}"
end

module NameMap = struct
  include Map.Make (NameOrdered)

  (* Guarantees physical equality if nothing changes. *)
  let filter_map_endo f m =
    let changed = ref false in
    let m' =
      m
      |> filter_map (fun _k x ->
          let res = f x in
          (match f x with
          | Some y when Eq.phys_equal x y -> ()
          | None
          | Some _ (* x != y *) ->
              changed := true);
          res)
    in
    if !changed then m' else m
end

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

type fixme_field = G.any [@@deriving show]

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
[@@deriving eq, ord, show { with_path = false }]

let related_tok tok = Related (G.Tk tok)
let related_exp exp_gen = Related (G.E exp_gen)

let any_of_orig = function
  | SameAs e -> G.E e
  | Related any -> any
  | NoOrig -> G.Anys []

(*****************************************************************************)
(* Parameters and arguments *)
(*****************************************************************************)

type 'a argument = Unnamed of 'a | Named of ident * 'a
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Parent iterator *)
(*****************************************************************************)

(* NOTE: We don't want visitors to automatically visit any AST_generic type. *)
class virtual ['self] iter_parent =
  object (self : 'self)
    method visit_tok _env _tok = ()
    method visit_sid _env _sid = ()
    method visit_ident _env _ident = ()
    method visit_name _env _name = ()

    method visit_bracket :
        'a. ('env -> 'a -> unit) -> 'env -> 'a bracket -> unit =
      fun f env (left, x, right) ->
        self#visit_tok env left;
        f env x;
        self#visit_tok env right

    method visit_wrap : 'a. ('env -> 'a -> unit) -> 'env -> 'a wrap -> unit =
      fun f env (x, tok) ->
        f env x;
        self#visit_tok env tok

    method visit_orig _env _orig = ()

    method visit_argument :
        'a. ('env -> 'a -> unit) -> 'env -> 'a argument -> unit =
      fun f env arg ->
        match arg with
        | Unnamed x -> f env x
        | Named (ident, x) ->
            self#visit_ident env ident;
            f env x

    method visit_pattern _env (_pattern : G.pattern) = ()
    method visit_literal _env _literal = ()
    method visit_operator _env _operator = ()
    method visit_type_ _env _typ = ()
    method visit_fixme_kind _env _fixme_kind = ()
    method visit_any _env _any = ()
    method visit_attribute _env _attr = ()
    method visit_type_parameters _env _typarams = ()
    method visit_function_kind _env _fkind = ()
    method visit_class_kind _env _ckind = ()
    method visit_class_parent _env _cparents = ()
    method visit_fixme_field _env _gfield = ()
    method visit_directive _env _directive = ()
  end

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
        Generic expression for a sub-lvalue. Now that we represent `x.a.b.c` as
        { base = "x"; rev_offsets = ["c"; "b"; "a"]}, it makes it very easy to
        check whether a sub-lvalue is a source/santizer/sink by just checking
        the range of the `oorig'.

        alt: We could compute the range of the sub-lvalue from the ranges of all
             the offsets in the sub-lvalue, but this is probably less efficent
             unless we cache the range here. So it seems better to have `oorig'.
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
  (* Records and dictionaries could also be 'Composite's, encoded as lists of
   * tuples, with the first element of the tuple being the field name or key.
   * (Python dictionary expressions are encoded this way in Generic.) But, for
   * analyses that are field- and index-sensitive (such as taint), it is more
   * convenient to have a separate representation as in here.
   *
   * THINK: should we transform Generic records/dict expressions into an empty
   *     constructor, followed by a series of Assign with Dot? simpler?
   *)
  | RecordOrDict of field_or_entry list
  | Cast of G.type_ * exp
  (* This could be put in call_special, but dumped IL are then less readable
   * (they are too many intermediate _tmp variables then) *)
  | Operator of G.operator wrap * exp argument list
  | FixmeExp of
      fixme_kind
      * G.any (* fixme source *)
      * exp (* partial translation *) option

(* THINK: Extend to better represent object literals ? *)
and field_or_entry =
  | Field of name * exp  (** struct field *)
  | Entry of exp * exp  (** dictionary entry, key and value *)
  | Spread of exp

and composite_kind =
  | CTuple
  | CArray
  | CList
  | CSet
  | Constructor of name (* OCaml *)
  | Regexp

(*****************************************************************************)
(* Instruction *)
(*****************************************************************************)

(* Easier type to compute lvalue/rvalue set of a too general 'expr', which
 * is now split into  instr vs exp vs lval.
 *)
and instr = { i : instr_kind; iorig : orig }

and instr_kind =
  (* was called Set in CIL, but a bit ambiguous with Set module *)
  | Assign of lval * exp
  | AssignCall of lval option * call
  | New of lval * G.type_ * exp option (* constructor *) * exp argument list
  (* todo: PhiSSA! *)
  | FixmeInstr of fixme_kind * G.any

and call = { c : call_kind; corig : orig }

and call_kind =
  | Call of exp * exp argument list
  | CallSpecial of call_special wrap * exp argument list

and call_special =
  | Eval
  (* TODO: lift up like in AST_generic *)
  | Typeof
  | Instanceof
  | Sizeof
  (* old: better in exp: | Operator of G.arithmetic_operator *)
  | Concat (* THINK: Normalize as a Operator G.Concat ? *)
  | SpreadFn
  | Yield
  | Await
  (* C++ *)
  | Delete
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
  | AnonClass of class_definition

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt = { s : stmt_kind (* sorig: G.stmt; ?*) }

and stmt_kind =
  | Instr of instr
  (* Switch are converted to a series of If *)
  | If of tok * exp * stmt list * stmt list
  | Match of pattern_match
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
      * stmt list
    (* finally / THINK: no finally vs empty finally ? use `option` ? *)
  | Throw of tok * exp (* less: enforce lval here? *)
  (* Lambdas and (with Pro-naming) nested function definitions, and annon classes.

    THINK:
      Eventually we may want to lift all nested definitions and have a `type top` for
      these rather than using 'stmt' like in Generic. However, it may complicate
      taint propagation from the outer function into the nested function/lambda.
    *)
  | NestedDef of definition
  | MiscStmt of other_stmt
  | FixmeStmt of fixme_kind * G.any

and other_stmt =
  (* Top-level definititions and (for now, for OSS-naming) also nested ones. *)
  | DefStmt of definition
  | DirectiveStmt of G.directive
  | Noop of (* for debugging purposes *) string

and label = ident * G.sid
and pattern_match = { scrutinee : name; branches : pattern_branch list }
and pattern_branch = { pattern : pattern_spec; body : stmt list }

and pattern_spec =
  | PatLiteral of G.literal
  | PatWildcard
  | PatVariable of name
  | PatConstructor of
      name * name list (* name option list? for internal wildcards? *)

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)
(* THINK: Attach a "fake name" to FixmeEntity *)
and entity_name = EN of name | FixmeEntity of G.any

and entity = {
  name : entity_name;
  attrs : G.attribute list;
  tparams : G.type_parameters option;
}

and variable_definition = { vtype : G.type_ option; vinit : exp option }

(* See AST_generic.ml *)
and function_definition = {
  fkind : G.function_kind wrap;
  fparams : param list;
  frettype : G.type_ option;
  fbody : stmt list;
}

(* THINK: Need an initialization section? Note that side-effects in class fields
  end up as statements outside of the class declaration. *)
and class_definition = {
  ckind : G.class_kind wrap;
  cextends : G.class_parent list;
  cimplements : G.type_ list;
  cmixins : G.type_ list;
  cfields : class_field list;
  (* TODO: We probably want to separate constructors from regular methods. *)
  (* cconstructors : class_constructor list; *)
  cmethods : class_method list;
  cfixmes : fixme_field list;
}

and class_field = entity * variable_definition
and class_constructor = entity * function_definition
and class_method = entity * function_definition
and definition = entity * definition_kind
(* THINK: Need an orig for definitions so that we can check if the def itself is a
    source/sink/etc ? *)

and definition_kind =
  (* THINK: When translating a top-level VarDef, we may want to keep it as is ? *)
  (* | VarDef of variable_definition *)
  | FuncDef of function_definition
  | ClassDef of class_definition
  | FixmeDef

(*****************************************************************************)
(* Program *)
(*****************************************************************************)
and program = stmt list

(*****************************************************************************)
(* Control-flow graph (CFG) *)
(*****************************************************************************)
(* Similar to controlflow.ml, but with a simpler node_kind.
 * See controlflow.ml for more information. *)
and node = {
  n : node_kind;
      (* old: there are tok in the nodes anyway
       * t: Parse_info.t option;
       *)
  mutable at_exit : bool;
}

and node_kind =
  | Enter
  | Exit
  (* 'TrueNode' and 'FalseNode' follow 'NCond', and the 'exp' is the same
   * condition as in 'NCond'. *)
  | TrueNode of exp (* same as in Cond *)
  | FalseNode of exp (* same as in Cond *)
  | Join
  | NInstr of instr
  | NCond of tok * exp
  | NGoto of tok * label
  | NReturn of tok * exp
  | NThrow of tok * exp
  | NMatch of name
  (* NCase follows NMatch, and the pattern spec is some condition to
     test against the scrutinee. The scrutinee is stored redundantly here
     to allow the transfer function to operate locally without looking at
     predecessor nodes. *)
  | NCase of name * pattern_spec
  | NNestedDef of entity
  | NOther of other_stmt
  | NTodo of stmt

and param_default = {
  dinit : stmt list;
      (** Stmts to evaluate at call time in the callee's scope, when the caller
          does not provide an argument for this parameter. *)
  dexp : exp;
}

and name_param = { pname : name; pdefault : param_default option }

and param =
  | Param of name_param
  | PatternParam of G.pattern
  | ParamRest of name  (** variadic, or "the rest of the remaining arguments" *)
  | FixmeParam
[@@deriving
  show { with_path = false },
  visitors { variety = "iter"; ancestors = [ "iter_parent" ] }]

(* For now there is just one kind of edge.
 * (we may use more? the "ShadowNode" idea of Julia Lawall?)
 *)
type edge = Direct
type cfg = (node, edge) CFG.t

(* an int representing the index of a node in the graph *)
type nodei = Ograph_extended.nodei

let mk_node n = { n; at_exit = false }

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

let ident_str_of_name name = fst name.ident
let str_of_label ((n, _), _) = n

let rec equal_base base1 base2 =
  match (base1, base2) with
  | Var name1, Var name2 -> compare_name name1 name2 = 0
  | VarSpecial (This, _), VarSpecial (This, _) -> true
  | VarSpecial (Super, _), VarSpecial (Super, _) -> true
  | VarSpecial (Self, _), VarSpecial (Self, _) -> true
  | VarSpecial (Parent, _), VarSpecial (Parent, _) -> true
  | ( Mem { e = Fetch { base = base1; rev_offset = [] }; _ },
      Mem { e = Fetch { base = base2; rev_offset = [] }; _ } ) ->
      (* Deref e.g. *base *)
      equal_base base1 base2
  | _ -> false

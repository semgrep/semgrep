(*s: pfff/h_program-lang/AST_generic.ml *)
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
(* A generic AST, to factorize similar analysis in different programming
 * languages (e.g., semgrep).
 *
 * Right now this generic AST is mostly the factorized union of:
 *  - Python, Ruby, Lua
 *  - Javascript, JSON, and Typescript
 *  - PHP
 *  - Java, CSharp
 *  - C (and some C++)
 *  - Go
 *  - OCaml
 *  - Scala
 *  - TODO next: Kotlin, Rust
 *
 * rational: In the end, programming languages have a lot in Common.
 * Even though most interesting analysis are probably better done on a
 * per-language basis, many useful analysis are trivial and require just an
 * AST and a visitor. One could duplicate those analysis for each language
 * or design an AST (this file) generic enough to factorize all those
 * analysis (e.g., unused entity). We want to remain
 * as precise as possible and not lose too much information while going
 * from the specific language AST to the generic AST. We also do not want
 * to be too generic as in ast_fuzzy.ml, where we have a very general
 * tree of nodes, but all the structure of the original AST is lost.
 *
 * The generic AST tries to be as close as possible to the original code but
 * not too close. When a programming language feature is really sugar or
 * an alternative way to do a thing, we usually unsugar. Here are the
 * simplifications done:
 *  - we do not keep the comma tokens in arguments. More generally we
 *    just keep the tokens to get the range right (see the discussions on
 *    invariants below) and get rid of the other (e.g., we remove
 *    parens around conditions in if). We keep the parens for 'Call'
 *    because we want to get the right range for those so we need the
 *    rightmost tokens in the AST.
 *  - multiple var declarations in one declaration (e.g., int a,b; in C)
 *    are expanded in multiple 'variable_definition'. Note that
 *    tuple assignments (e.g., a,b=1,2) are not expanded in multiple assigns
 *    because this is not always possible (e.g., a,b=foo()) and people may
 *    want to explicitely match tuples assignments (we do some magic in
 *    Generic_vs_generic though to let 'a=1' matches also 'a,b=1,2').
 *  - multiple ways to define a function are converted all to a
 *    'function_definition' (e.g., Javascript arrows are converted in that)
 *  - we are more general and impose less restrictions on where certain
 *    constructs can appear to simplify things.
 *     * there is no special lhs/lvalue type (see IL.ml for that) and so
 *      'Assign' takes a general 'expr' on its lhs.
 *     * there is no special toplevel/item vs stmt. Certain programming
 *       languages impose restrictions on where a function or directive can
 *       appear (e.g., just at the toplevel), but we allow those constructs
 *       at the stmt level.
 *     * the Splat and HashSplat operator can usually appear just in arguments
 *       or inside arrays or in struct definitions (a field) but we are more
 *       general and put it at the 'expr' level.
 *     * certain attributes are valid only for certain constructs but instead
 *       we use one attribute type (no class_attribute vs func_attribute etc.)
 *
 * todo:
 *  - add C++ (argh)
 *  - see ast_fuzzy.ml todos for ideas to use AST_generic for sgrep.
 *
 * related work:
 *  - ast_fuzzy.ml (in this directory)
 *  - github semantic
 *    https://github.com/github/semantic
 *  - UAST of babelfish
 *    https://doc.bblf.sh/uast/uast-specification-v2.html
 *  - Coverity common program representation?
 *  - Semmle internal common representation?
 *  - Sonarcube generic language
 *    https://github.com/SonarSource/slang
 *  - Infer SIL (for C++, Java, Objective-C)
 *  - Dawson Engler and Fraser Brown micro-checkers for multiple languages
 *  - Lightweight Multi-language syntax transformation paper, but does not
 *    really operate on an AST
 *  - https://tabnine.com/ which supports multiple languages, but probably
 *    again does not operate on an AST
 *  - srcML https://www.srcml.org/doc/srcMLGrammar.html
 *    but just for C/C++/C#/Java and seems pretty heavy
 *
 * design choices to have a generic data structure:
 *  - add some 'a, 'b, 'c around expr/stmt/...
 *  - data-type a la carte like in github-semantic but IMHO too high-level
 *    with astronaut-style architecture (too abstract, too advanced features).
 *  - CURRENT SOLUTION: the OtherXxx strategy used in this file (simple)
 *  - functorize and add some type hole (type tstmt; type texpr; ...),
 *    todo? not a bad idea if later we want to add type information on each
 *    expression nodes
 *
 * history:
 *  - started with crossproduct of Javascript, Python, PHP, Java, and C
 *    (and a bit of OCaml) after wanting to port checked_return from Js to
 *    Python and got the idea to factorize things
 *
 * INVARIANTS:
 *  - all the other_xxx types should contain only simple constructors (enums)
 *    without any parameter. I rely on that to simplify the code
 *    of the generic mapper and matcher.
 *    Same for keyword_attributes.
 *  - each expression or statement must have at least one token in it
 *    so that semgrep can track a location (e.g., 'Return of expr option'
 *    is not enough because with no expr, there is no location information
 *    for this return, so it must be 'Return of tok * expr option' instead)
 *  - each expression or statement should ideally have enough tokens in it
 *    to get its range, so at least the leftmost and rightmost token in
 *    all constructs, so the Return above should even be
 *    'Return of tok * expr option * tok' for the ending semicolon
 *    (alt: have the range info in each expr/stmt/pattern but big refactoring)
 *  - to correctly compute a CFG (Control Flow Graph), the stmt type
 *    should list all constructs that contains other statements and
 *    try to avoid to use the very generic 'OtherXxx of any'
 *  - to correctly compute a DFG (Data Flow Graph), and to correctly resolve
 *    names (see Naming_AST.ml), each constructs that introduce a new
 *    variable should have a relevant comment 'newvar:'
 *  - to correctly resolve names, each construct that introduces a new scope
 *    should have a relevant comment 'newscope:'
 *  - todo? each language should add the VarDefs that defines the locals
 *    used in a function (instead of having the first Assign play the role
 *    of a VarDef, as done in Python for example).
 *
 * See also pfff/lang_GENERIC/
 *)

(* Provide hash_* and hash_fold_* for the core ocaml types *)
open Ppx_hash_lib.Std.Hash.Builtin

(* ppx_hash refuses to hash mutable fields but we do it anyway. *)
let hash_fold_ref hash_fold_x acc x = hash_fold_x acc !x

(*****************************************************************************)
(* Token (leaf) *)
(*****************************************************************************)
(*s: type [[AST_generic.tok]] *)
(* Contains among other things the position of the token through
 * the Parse_info.token_location embedded inside it, as well as the
 * transformation field that makes possible spatch on the code.
 *)
type tok = Parse_info.t [@@deriving show]

(*e: type [[AST_generic.tok]] *)

(* with tarzan *)

(* sgrep: we do not care about position when comparing for equality 2 ASTs.
 * related: Lib_AST.abstract_position_info_any and then use OCaml generic '='.
 *)
let equal_tok _t1 _t2 = true

let hash_tok _t = 0

let hash_fold_tok acc _t = acc

(*s: type [[AST_generic.wrap]] *)
(* a shortcut to annotate some information with position information *)
type 'a wrap = 'a * tok
(*e: type [[AST_generic.wrap]] *)
[@@deriving show, eq, hash]

(* with tarzan *)

(*s: type [[AST_generic.bracket]] *)
(* Use for round(), square[], curly{}, and angle<> brackets.
 * note: in theory we should not care about those tokens in an AST,
 * but they are useful to report correct ranges in sgrep when we match
 * something that can just be those brackets (e.g., an empty container).
 *)
type 'a bracket = tok * 'a * tok
(*e: type [[AST_generic.bracket]] *)
[@@deriving show, eq, hash]

(* with tarzan *)

(* semicolon, a FakeTok in languages that do not require them (e.g., Python).
 * alt: tok option.
 * See the sc value also at the end of this file to build an sc.
 *)
type sc = tok [@@deriving show, eq, hash]

(* with tarzan *)

(* an AST element not yet handled; works with the Xx_Todo and Todo in any *)
type todo_kind = string wrap [@@deriving show, eq, hash]

(* with tarzan *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(*s: type [[AST_generic.ident]] *)
type ident = string wrap
(*e: type [[AST_generic.ident]] *)
[@@deriving show, eq, hash]

(*s: type [[AST_generic.dotted_ident]] *)
(* usually separated by a '.', but can be used also with '::' separators *)
type dotted_ident = ident list (* at least 1 element *)
(*e: type [[AST_generic.dotted_ident]] *)
[@@deriving show, eq, hash]

(* with tarzan *)

(*s: type [[AST_generic.module_name]] *)
(* module_name can also be used for a package name or a namespace *)
type module_name =
  | DottedName of dotted_ident (* ex: Python *)
  (* in FileName the '/' is similar to the '.' in DottedName *)
  | FileName of string wrap (* ex: Js import, C #include, Go import *)
(*e: type [[AST_generic.module_name]] *)
[@@deriving show { with_path = false }, eq, hash]

(* with tarzan *)

(* A single unique id: sid (uid would be a better name, but it usually
 * means "user id" for people).
 *
 * This single id simplifies further analysis which need less to care about
 * maintaining scoping information, for example to deal with variable
 * shadowing, or functions using the same parameter names
 * (even though you still need to handle specially recursive functions), etc.
 *
 * See Naming_AST.ml for more information.
 *
 * Most generic ASTs have a fake value (sid_TODO = -1) at first.
 * You need to call Naming_AST.resolve (or one of the lang-specific
 * Resolve_xxx.resolve) on the generic AST to set it correctly.
 *)
(*s: type [[AST_generic.sid]] *)
type sid = int

(* a single unique gensym'ed number. See gensym() below *)

(*e: type [[AST_generic.sid]] *)

(*s: type [[AST_generic.resolved_name]] *)
and resolved_name = resolved_name_kind * sid

(*e: type [[AST_generic.resolved_name]] *)
(*s: type [[AST_generic.resolved_name_kind]] *)
and resolved_name_kind =
  (* Global is useful in codemap/efuns to highlight differently and warn
   * about the use of globals inside functions.
   * old: Global was merged with ImportedEntity before but simpler to split, as
   * anyway I was putting often an empty list for dotted_ident with a
   * todo note in the code.
   *)
  | Global
  (* Those could be merged, but again this is useful in codemap/efuns *)
  | Local
  | Param
  (* For closures; can refer to a Local or Param.
   * With sid this is potentially less useful for scoping-related issues,
   * but this can be useful in codemap to again highlight specially
   * enclosed vars.
   * todo: this is currently used also for fields, but we should use another
   * constructor.
   * Note that it's tempting to add a depth parameter to EnclosedVar, but
   * that would prevent semgrep to work because whatever the depth you are,
   * if you reference the same entity, this entity must have the same
   * resolved_name (sid and resolved_name_kind).
   *)
  | EnclosedVar (* less: add depth? *)
  (* sgrep: those cases allow to match entities/modules even if they were
   * aliased when imported.
   * both dotted_ident must at least contain one element *)
  | ImportedEntity of dotted_ident (* can also use 0 for gensym *)
  | ImportedModule of module_name
  (* used in Go, where you can pass types as arguments and where we
   * need to resolve those cases
   *)
  | TypeName
  (* used for C *)
  | Macro
  | EnumConstant
(*e: type [[AST_generic.resolved_name_kind]] *)
[@@deriving show { with_path = false }, eq, hash]

(* with tarzan *)

(* Start of big mutually recursive types because of the use of 'any'
 * in OtherXxx *)

(*s: type [[AST_generic.name]] *)
(* old: Id below used to be called Name and was generalizing also IdQualified
 * but some analysis are easier when they just need to
 * handle a simple Id, hence the split. For example, there was some bugs
 * in sgrep because sometimes an identifier was an ident (in function header)
 * and sometimes a name (when called). For naming, we also need to do
 * things differently for Id vs IdQualified and would need many times to
 * inspect the name.name_qualifier to know if we have an Id or IdQualified.
 * We do the same split for Fid vs FName for fields.
 *
 * newvar: Id is sometimes abused to also introduce a newvar (as in Python)
 * but ultimately those cases should be rewritten to first introduce
 * a VarDef.
 *
 * todo: Sometimes some DotAccess should really be transformed in IdQualified
 * with a better qualifier because the obj is actually the name of a package
 * or module, but you may need advanced semantic information and global
 * analysis to disambiguate.
 *
 * less: factorize the id_info in both and inline maybe name_info
 *)
type name =
  | Id of ident * id_info
  | IdQualified of (ident * name_info) * id_info

(*e: type [[AST_generic.name]] *)
(*s: type [[AST_generic.name_info]] *)
and name_info = {
  name_qualifier : qualifier option;
  name_typeargs : type_arguments option; (* Java/Rust *)
}

(* todo: not enough in OCaml with functor and type args or C++ templates*)
(*e: type [[AST_generic.name_info]] *)
(*s: type [[AST_generic.qualifier]] *)
and qualifier =
  | QTop of tok (* ::, Ruby, C++, also '`' abuse for PolyVariant in OCaml *)
  | QDots of dotted_ident (* Java, OCaml *)
  | QExpr of expr * tok

(* Ruby *)

(*e: type [[AST_generic.qualifier]] *)

(* This is used to represent field names, where sometimes the name
 * can be a dynamic expression, or more recently also to
 * represent entities like in Ruby where a class name can be dynamic.
 *)
and name_or_dynamic =
  (* In the case of a field, it may be hard to resolve the id_info inside name.
   * For example, a method id can refer to many method definitions.
   * But for certain things, like a private field, we can resolve it
   * (right now we use an EnclosedVar for those fields).
   *
   * The IdQualified inside name is
   * Useful for OCaml field access, but also for Ruby class entity name.
   *)
  | EN of name
  (* for PHP/JS fields (even though JS use ArrayAccess for that), or Ruby *)
  | EDynamic of expr

(*****************************************************************************)
(* Naming/typing *)
(*****************************************************************************)

(*s: type [[AST_generic.id_info]] *)
and id_info = {
  id_resolved : resolved_name option ref;
  (* variable tagger (naming) *)
  (* sgrep: in OCaml we also use that to store the type of
   * a typed entity, which can be interpreted as a TypedMetavar in semgrep.
   * alt: have an explicity type_ field in entity.
   *)
  id_type : type_ option ref;
  (* type checker (typing) *)
  (* sgrep: this is for sgrep constant propagation hack.
   * todo? associate only with Id?
   * note that we do not use the constness for equality (hence the adhoc
   * @equal below) because the constness analysis is now controlflow-sensitive
   * meaning the same variable might have different id_constness value
   * depending where it is used.
   *)
  id_constness : constness option ref; [@equal fun _a _b -> true]
      (* THINK: Drop option? *)
}

(*e: type [[AST_generic.id_info]] *)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(*s: type [[AST_generic.expr]] *)
(* todo? we could do like for stmt and have 'expr' and 'expr_kind', which
 * would allow us to store more semantic information at each expr node,
 * e.g., type information, or constant evaluation, or range, but it
 * would be a bigger refactoring than for stmt.
 *)
and expr =
  (* basic (atomic) values *)
  | L of literal
  (* composite values *)
  | Container of container_operator * expr list bracket
  (*s: [[AST_generic.expr]] other composite cases *)
  (* special case of Container, at least 2 elements (except for Python where
   * you can actually have 1-uple, e.g., '(1,)' *)
  | Tuple of expr list bracket
  (*x: [[AST_generic.expr]] other composite cases *)
  (* And-type (field.vinit should be a Some) *)
  | Record of field list bracket
  (*x: [[AST_generic.expr]] other composite cases *)
  (* Or-type (could be used instead of Container, Cons, Nil, etc.).
   * (ab)used also for polymorphic variants where qualifier is QTop with
   * the '`' token.
   *)
  | Constructor of dotted_ident * expr list
  (* see also Call(IdSpecial (New,_), [ArgType _;...] for other values *)
  (*e: [[AST_generic.expr]] other composite cases *)
  | N of name
  (*s: [[AST_generic.expr]] other identifier cases *)
  (*x: [[AST_generic.expr]] other identifier cases *)
  | IdSpecial of special wrap (*e: [[AST_generic.expr]] other identifier cases *)
  (* operators and function application *)
  | Call of expr * arguments bracket (* can be fake '()' for OCaml/Ruby *)
  (*s: [[AST_generic.expr]] other call cases *)
  (* (XHP, JSX, TSX), could be transpiled also (done in IL.ml?) *)
  | Xml of xml
  (* IntepolatedString of expr list is simulated with a
   * Call(IdSpecial (Concat ...)) *)
  (*e: [[AST_generic.expr]] other call cases *)

  (* The left part should be an lvalue (Id, DotAccess, ArrayAccess, Deref)
   * but it can also be a pattern (Tuple, Container, even Record), but
   * you should really use LetPattern for that.
   * Assign can also be abused to declare new variables, but you should use
   * variable_definition for that.
   * less: should be in stmt, but most languages allow this at expr level :(
   * todo: see IL.ml where we normalize this AST with expr/instr/stmt
   * update: should even be in a separate simple_stmt, as in Go
   *)
  | Assign of
      expr * tok (* '=', '<-' in OCaml. ':=' Go is AssignOp (Eq) *) * expr
  (*s: [[AST_generic.expr]] other assign cases *)
  (* less: could desugar in Assign, should be only binary_operator *)
  | AssignOp of expr * operator wrap * expr
  (* newvar:! newscope:? in OCaml yes but we miss the 'in' part here  *)
  | LetPattern of pattern * expr (*e: [[AST_generic.expr]] other assign cases *)
  (* can be used for Record, Class, or Module access depending on expr.
   * In the last case it should be rewritten as a (N IdQualified) with a
   * qualifier though.
   *)
  | DotAccess of expr * tok (* ., ::, ->, # *) * name_or_dynamic
  (*s: [[AST_generic.expr]] array access cases *)
  (* in Js ArrayAccess is also abused to perform DotAccess (..., FDynamic) *)
  | ArrayAccess of expr * expr bracket
  (*x: [[AST_generic.expr]] array access cases *)
  (* could also use ArrayAccess with a Tuple rhs, or use a special *)
  | SliceAccess of
      expr
      * (expr option (* lower *) * expr option (* upper *) * expr option)
        (* step *)
        bracket
  (*e: [[AST_generic.expr]] array access cases *)
  (*s: [[AST_generic.expr]] anonymous entity cases *)
  (* very special value *)
  | Lambda of function_definition
  (* usually an argument of a New (used in Java, Javascript) *)
  | AnonClass of class_definition
  (*e: [[AST_generic.expr]] anonymous entity cases *)
  (*s: [[AST_generic.expr]] other cases *)
  (* a.k.a ternary expression. Note that even in languages like OCaml
   * where 'if's are expressions, we still prefer to use the stmt 'If'
   * because it allows an optional else part. We need to sometimes
   * wrap those stmts inside an OE_StmtExpr though.
   *)
  | Conditional of expr * expr * expr
  | MatchPattern of expr * action list
  | Yield of tok * expr option * bool (* 'from' for Python *)
  | Await of tok * expr
  (* Send/Recv of Go are currently in OtherExpr *)
  | Cast of type_ (* TODO: bracket or colon *) * expr
  (* less: should be in statement *)
  | Seq of expr list (* at least 2 elements *)
  (* less: could be in Special, but pretty important so I've lifted them here*)
  | Ref of tok (* &, address of *) * expr
  | DeRef of tok (* '*' in C, '!' or '<-' in OCaml, ^ in Reason *) * expr (*e: [[AST_generic.expr]] other cases *)
  (*s: [[AST_generic.expr]] semgrep extensions cases *)
  (* sgrep: ... in expressions, args, stmts, items, and fields
   * (and unfortunately also in types in Python) *)
  | Ellipsis of tok (* '...' *)
  (*x: [[AST_generic.expr]] semgrep extensions cases *)
  | DeepEllipsis of expr bracket (* <... ...> *)
  (*x: [[AST_generic.expr]] semgrep extensions cases *)
  | DisjExpr of expr * expr
  (*x: [[AST_generic.expr]] semgrep extensions cases *)
  | TypedMetavar of ident * tok (* : *) * type_
  (*e: [[AST_generic.expr]] semgrep extensions cases *)
  (* for ellipsis in method chaining *)
  | DotAccessEllipsis of expr * tok (* '...' *)
  (*s: [[AST_generic.expr]] OtherXxx case *)
  (* TODO: other_expr_operator wrap, so enforce at least one token instead
   * of relying that the any list contains at least one token *)
  | OtherExpr of other_expr_operator * any list

(*e: [[AST_generic.expr]] OtherXxx case *)
(*e: type [[AST_generic.expr]] *)

(*s: type [[AST_generic.literal]] *)
and literal =
  | Bool of bool wrap
  (* the numbers are an option because OCaml numbers (e.g., 63bits int)
   * may not be able to represent all numbers.
   *)
  | Int of int option wrap
  | Float of float option wrap
  | Char of string wrap
  | String of string wrap (* TODO? bracket, ', ", or even """ *)
  | Regexp of string wrap bracket (* // *) * string wrap option (* modifiers *)
  | Atom of tok (* ':' in Ruby, ''' in Scala *) * string wrap
  | Unit of tok
  (* a.k.a Void *)
  | Null of tok
  | Undefined of tok (* JS *)
  | Imag of string wrap
  (* Go, Python *)
  | Ratio of string wrap

(*e: type [[AST_generic.literal]] *)

(* The type of an unknown constant. *)
and const_type = Cbool | Cint | Cstr | Cany

(* set by the constant propagation algorithm and used in semgrep *)
and constness = Lit of literal | Cst of const_type | NotCst

(*s: type [[AST_generic.container_operator]] *)
and container_operator =
  (* Tuple was lifted up *)
  | Array (* todo? designator? use ArrayAccess for designator? *)
  | List
  | Set
  (* TODO? merge with Record *)
  | Dict

(* a.k.a Hash or Map (combine with Tuple to get Key/value pair) *)

(*e: type [[AST_generic.container_operator]] *)

(* It's useful to keep track in the AST of all those special identifiers.
 * They need to be handled in a special way by certain analysis and just
 * using Name for them would be error-prone.
 * Note though that by putting all of them together in a type, we lose
 * typing information, for example Eval takes only one argument and
 * InstanceOf takes a type and an expr. This is a tradeoff to also not
 * polluate too much expr with too many constructs.
 *)
(*s: type [[AST_generic.special]] *)
and special =
  (* special vars *)
  | This
  | Super (* called 'base' in C# *)
  | Self
  | Parent (* different from This/Super? *)
  | NextArrayIndex (* Lua, todo: just remove it, create Dict without key *)
  (* special calls *)
  | Eval
  | Typeof (* for C? and Go in switch x.(type) *)
  | Instanceof
  | Sizeof (* takes a ArgType *)
  | Defined (* defined? in Ruby, other? *)
  (* note that certain languages do not have a 'new' keyword
   * (e.g., Python, Scala 3), instead certain 'Call' are really 'New' *)
  | New (* usually associated with Call(New, [ArgType _;...]) *)
  (* new by itself is not a valid expression*)
  (* used for interpolated strings constructs *)
  | ConcatString of concat_string_kind
  | EncodedString of string (* only for Python for now (e.g., b"foo") *)
  (* TaggedString? for Javascript, for styled.div`bla{xx}`?
   * We could have this TaggedString where the first arg of Call
   * will be the tagging function, and the rest will be a Call ConcatString.
   * However, it is simpler to just transform those special calls as
   * regular calls even though they do not have parenthesis
   * (not all calls have parenthesis anyway, as in OCaml or Ruby).
   *)
  (* Use this to separate interpolated elements in interpolated strings
   * but this is a bit of a hack. We should probably add InterpolatedConcat
   * as an expression
   *)
  | InterpolatedElement
  (* "Inline" the content of a var containing a list (a.k.a a Splat in Ruby).
   * Used in a Container or Call argument context.
   * The corresponding constructor in a parameter context is ParamRest.
   *)
  | Spread (* ...x in JS, *x in Python/Ruby *)
  (* Similar to Spread, but for a var containing a hashtbl.
   * The corresponding constructor in a parameter context is ParamHashSplat.
   *)
  | HashSplat (* **x in Python/Ruby
               * (not to confused with Pow below which is a Binary op *)
  | ForOf (* Javascript, for generators, used in ForEach *)
  (* used for unary and binary operations *)
  | Op of operator
  (* less: should be lift up and transformed in Assign at stmt level *)
  | IncrDecr of (incr_decr * prefix_postfix)

(*e: type [[AST_generic.special]] *)

(* mostly binary operators.
 * less: could be divided in really Arith vs Logical (bool) operators,
 * but see is_boolean_operator() helper below.
 * Note that Mod can be used for %style string formatting in Python.
 * Note that Plus can also be used for string concatenations in Go/??.
 * todo? use a Special operator intead for that? but need type info?
 *)
(*s: type [[AST_generic.arithmetic_operator]] *)
and operator =
  | Plus
  (* unary too *)
  | Minus (* unary too *)
  | Mult
  | Div
  | Mod
  | Pow (* ** binary op; for unary see HashSplat above *)
  | FloorDiv
  | MatMult (* Python *)
  | LSL
  | LSR
  | ASR (* L = logic, A = Arithmetic, SL = shift left *)
  | BitOr
  | BitXor
  | BitAnd
  | BitNot
  (* unary *)
  | BitClear (* Go *)
  (* todo? rewrite in CondExpr? have special behavior *)
  | And
  | Or
  (* also shortcut operator *)
  | Xor
  (* PHP*)
  | Not (* unary *)
  | Eq (* '=' in OCaml, '==' in Go/... *)
  | NotEq (* less: could be desugared to Not Eq *)
  | PhysEq (* '==' in OCaml, '===' in JS/... *)
  | NotPhysEq (* less: could be desugared to Not PhysEq *)
  | Lt
  | LtE
  | Gt
  | GtE (* less: could be desugared to Or (Eq Lt) *)
  | Cmp (* <=>, PHP *)
  | Concat (* '.' PHP, '..' Lua *)
  | Append (* x[] = ... in PHP, just in AssignOp *)
  | RegexpMatch (* =~, Ruby (and Perl) *)
  | NotMatch (* !~ Ruby less: could be desugared to Not RegexpMatch *)
  | Range (* .. or ..., Ruby, one arg can be nil for endless range *)
  | RangeInclusive (* '..=' in Rust *)
  | NotNullPostfix (* ! in Typescript, postfix operator *)
  | Length (* '#' in Lua *)
  (* See https://en.wikipedia.org/wiki/Elvis_operator.
   * In PHP we currently generate a Conditional instead of a Binary Elvis.
   * It looks like the Nullish operator is quite similar to the Elvis
   * operator, so we may want to merge those operators at some point.
   *)
  | Elvis (* ?: in Kotlin, can compare possible null value *)
  | Nullish (* ?? in Javascript *)
  | In
  (* in: checks that value belongs to a collection *)
  | NotIn (* !in *)
  | Is
  (* is: checks value has type *)
  | NotIs

(* !is: *)

(*e: type [[AST_generic.arithmetic_operator]] *)
(*s: type [[AST_generic.incr_decr]] *)
and incr_decr = Incr | Decr

(* '++', '--' *)

(*e: type [[AST_generic.incr_decr]] *)
(*s: type [[AST_generic.prefix_postfix]] *)
and prefix_postfix = Prefix | Postfix

(*e: type [[AST_generic.prefix_postfix]] *)
and concat_string_kind =
  (* many languages do not require a special syntax to use interpolated
   * strings e.g. simply "this is {a}". Javascript uses backquotes.
   *)
  | InterpolatedConcat (* Javascript/PHP/Ruby/Perl *)
  (* many languages have a binary Concat operator to concatenate strings,
   * but some languages also allow the simple juxtaposition of multiple
   * strings to be concatenated, e.g. "hello" "world" in Python.
   *)
  | SequenceConcat (* Python/C *)
  (* Python requires the special f"" syntax to use interpolated strings,
   * and some semgrep users may want to explicitely match only f-strings,
   * which is why we record this information here.
   * update: also use for interpolated Scala strings
   * TODO: add of string ('f' or something else)
   *)
  | FString
  (* Javascript uses a special syntax called tagged template literals, e.g.,
   * foo`template string = ${id}`. We must use a different representation
   * for foo(`template string = ${id}`) because some semgrep users want
   * to find one and not the other.
   * In both case it will be inside Call (Apply (ConcatString  ... but then
   * the kind will differ.
   *)
  | TaggedTemplateLiteral

(* e.g., foo`template ${id}` *)

(* Python *)

(*s: type [[AST_generic.field_ident]] *)
(*s: [[AST_generic.field_ident]] other cases *)
(*e: [[AST_generic.field_ident]] other cases *)
(*e: type [[AST_generic.field_ident]] *)

(* newscope: newvar: *)
(*s: type [[AST_generic.action]] *)
and action = pattern * expr

(*e: type [[AST_generic.action]] *)

(* less: could make it more generic by adding a 'expr so it could be
 * reused in ast_js.ml, ast_php.ml
 *)
(*s: type [[AST_generic.xml]] *)
(* this is for JSX/TSX in javascript land (old: and XHP in PHP land) *)
and xml = {
  xml_kind : xml_kind;
  xml_attrs : xml_attribute list;
  xml_body : xml_body list;
}

(*e: type [[AST_generic.xml]] *)
and xml_kind =
  | XmlClassic of tok (*'<'*) * ident * tok (*'>'*) * tok (*'</foo>'*)
  | XmlSingleton of tok (*'<'*) * ident * tok (* '/>', with xml_body = [] *)
  (* React/JS specific *)
  | XmlFragment of tok (* '<>' *) * tok

(* '</>', with xml_attrs = [] *)

(*s: type [[AST_generic.xml_attribute]] *)
and xml_attribute =
  | XmlAttr of ident * tok (* = *) * xml_attr_value
  (* less: XmlAttrNoValue of ident. <foo a /> <=> <foo a=true /> *)
  (* jsx: usually a Spread operation, e.g., <foo {...bar} /> *)
  | XmlAttrExpr of expr bracket
  (* sgrep: *)
  | XmlEllipsis of tok

(*e: type [[AST_generic.xml_attribute]] *)
(* either a String or a bracketed expr, but right now we just use expr *)
and xml_attr_value = expr

(*s: type [[AST_generic.xml_body]] *)
and xml_body =
  (* sgrep-ext: can contain "..." *)
  | XmlText of string wrap
  (* this can be None when people abuse {} to put comments in it *)
  | XmlExpr of expr option bracket
  | XmlXml of xml

(*e: type [[AST_generic.xml_body]] *)

(*s: type [[AST_generic.arguments]] *)
and arguments = argument list

(*e: type [[AST_generic.arguments]] *)
(*s: type [[AST_generic.argument]] *)
and argument =
  (* regular argument *)
  | Arg of expr (* can be Call (IdSpecial Spread, Id foo) *)
  (*s: [[AST_generic.argument]] other cases *)
  (* keyword argument *)
  | ArgKwd of ident * expr
  (*x: [[AST_generic.argument]] other cases *)
  (* type argument for New, instanceof/sizeof/typeof, C macros *)
  | ArgType of type_
  (*e: [[AST_generic.argument]] other cases *)
  (*s: [[AST_generic.argument]] OtherXxx case *)
  | ArgOther of other_argument_operator * any list

(*e: [[AST_generic.argument]] OtherXxx case *)

(*e: type [[AST_generic.argument]] *)

(*s: type [[AST_generic.other_argument_operator]] *)
and other_argument_operator =
  (* Python *)
  | OA_ArgComp (* comprehension *)
  (* OCaml *)
  | OA_ArgQuestion

(*e: type [[AST_generic.other_argument_operator]] *)

(* todo: reduce, or move in other_special? *)
(*s: type [[AST_generic.other_expr_operator]] *)
and other_expr_operator =
  (* Javascript *)
  | OE_Exports
  | OE_Module
  | OE_Define
  | OE_Arguments
  | OE_NewTarget
  | OE_Delete
  | OE_YieldStar
  (* note: some of them are transformed in ImportFrom in js_to_generic.ml *)
  | OE_Require
  | OE_UseStrict (* todo: lift up to program attribute/directive? *)
  (* Python *)
  | OE_Invert
  | OE_Slices (* see also SliceAccess *)
  (* todo: newvar: *)
  | OE_CompForIf
  | OE_CompFor
  | OE_CompIf
  | OE_CmpOps
  | OE_Repr (* todo: move to special, special Dump *)
  (* Java *)
  | OE_NameOrClassType
  | OE_ClassLiteral
  | OE_NewQualifiedClass
  | OE_Annot
  (* C *)
  | OE_GetRefLabel
  | OE_ArrayInitDesignator (* [x] = ... todo? use ArrayAccess in container?*)
  (* PHP *)
  | OE_Unpack
  | OE_ArrayAppend (* $x[]. The AST for $x[] = 1 used to be
                    * handled as an AssignOp with special Append, but we now
                    * use OE_ArrayAppend for everything to simplify.
                    *)
  (* OCaml *)
  | OE_RecordWith
  | OE_RecordFieldName
  (* Go *)
  | OE_Send
  | OE_Recv
  (* Ruby *)
  (* Rust *)
  | OE_MacroInvocation
  (* C# *)
  | OE_Checked
  | OE_Unchecked
  (* Other *)
  | OE_StmtExpr (* OCaml/Ruby have just expressions, no statements *)
  | OE_Todo

(*e: type [[AST_generic.other_expr_operator]] *)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
(*s: type [[AST_generic.stmt]] *)
and stmt = {
  s : stmt_kind;
      [@equal AST_utils.equal_stmt_field_s equal_stmt_kind] [@hash.ignore]
  (* this can be used to compare and hash more efficiently stmts,
     or in semgrep to quickly know if a stmt is a children of another stmt.
  *)
  s_id : AST_utils.Node_ID.t; [@equal AST_utils.equal_stmt_field_s_id]
  (* todo? we could store a range: (tok * tok) to delimit the range of a stmt
   * which would allow us to remove some of the extra 'tok' in stmt_kind.
   * Indeed, the main use of those 'tok' is to accurately report a match range
   * in semgrep.
   *)
  mutable s_use_cache : bool; [@equal fun _a _b -> true] [@hash.ignore]
  (* whether this is a strategic point for match result caching.
     This field is relevant for patterns only.

     This applies to the caching optimization, in which the results of
     matching lists of statements can be cached. A list of statements
     is identified by its leading node. In the current implementation,
     the fields 's_id', 's_use_caching', and 's_backrefs' are treated as
     properties of a (non-empty) list of statements, rather than of individual
     statements. A cleaner implementation would consist of a custom
     list type in which each list has these properties, including the
     empty list.
  *)
  mutable s_backrefs : AST_utils.String_set.t option;
      [@equal fun _a _b -> true] [@hash.ignore]
  (* set of metavariables referenced in the "rest of the pattern", as
     determined by matching order.
     This field is relevant for patterns only.

     This is used to determine which of the bound
     metavariables should be added to the cache key for this node.
     This field is set on pattern ASTs only, in a pass right after parsing
     and before matching.
  *)
  (* used in semgrep to skip some AST matching *)
  mutable s_bf : Bloom_filter.t option; [@equal fun _a _b -> true] [@hash.ignore]
}

and stmt_kind =
  (* See also IL.ml where Call/Assign/Seq are not in expr and where there are
   * separate expr, instr, and stmt types *)
  | ExprStmt of expr * sc (* fake tok in Python, but also in JS/Go with ASI *)
  (* newscope: in C++/Java/Go *)
  | Block of stmt list bracket (* can be fake {} in Python where use layout *)
  (* EmptyStmt = Block [], or separate so can not be matched by $S? $ *)
  (* newscope: for vardef in expr in C++/Go/... *)
  | If of tok (* 'if' or 'elif' *) * expr * stmt * stmt option
  | While of tok * expr * stmt
  | Return of tok * expr option * sc
  (*s: [[AST_generic.stmt]] other cases *)
  | DoWhile of tok * stmt * expr
  (* newscope: *)
  | For of tok (* 'for', 'foreach'*) * for_header * stmt
  (* The expr can be None for Go and Ruby.
   * less: could be merged with ExprStmt (MatchPattern ...) *)
  | Switch of
      tok (* 'switch' or also 'select' in Go *)
      * expr option
      * case_and_body list
  | Continue of tok * label_ident * sc
  | Break of tok * label_ident * sc
  (* todo? remove stmt argument? more symetric to Goto *)
  | Label of label * stmt
  | Goto of tok * label
  | Throw of tok (* 'raise' in OCaml, 'throw' in Java/PHP *) * expr * sc
  | Try of tok * stmt * catch list * finally option
  | WithUsingResource of
      tok (* 'with' in Python, 'using' in C# *)
      * stmt (* resource acquisition *)
      * stmt (* newscope: block *)
  | Assert of tok * expr * expr option (* message *) * sc
  (*e: [[AST_generic.stmt]] other cases *)
  (*s: [[AST_generic.stmt]] toplevel and nested construct cases *)
  | DefStmt of definition
  (*x: [[AST_generic.stmt]] toplevel and nested construct cases *)
  | DirectiveStmt of directive
  (*e: [[AST_generic.stmt]] toplevel and nested construct cases *)
  (*s: [[AST_generic.stmt]] semgrep extensions cases *)
  (* sgrep: *)
  | DisjStmt of stmt * stmt
  (*e: [[AST_generic.stmt]] semgrep extensions cases *)
  (*s: [[AST_generic.stmt]] OtherXxx case *)
  (* this is important to correctly compute a CFG *)
  | OtherStmtWithStmt of other_stmt_with_stmt_operator * expr option * stmt
  (* any here should not contain any statement! otherwise the CFG will be
   * incorrect and some analysis (e.g., liveness) will be incorrect.
   * TODO: other_stmt_operator wrap, so enforce at least one token instead
   * of relying that the any list contains at least one token
   *)
  | OtherStmt of other_stmt_operator * any list

(*e: [[AST_generic.stmt]] OtherXxx case *)
(*e: type [[AST_generic.stmt]] *)

(* newscope: *)
(* less: could merge even more with pattern
 * list = PatDisj and Default = PatUnderscore,
 * so case_and_body of Switch <=> action of MatchPattern
 *)
(*s: type [[AST_generic.case_and_body]] *)
and case_and_body =
  | CasesAndBody of (case list * stmt)
  (* sgrep: *)
  | CaseEllipsis of tok

(* ... *)

(*e: type [[AST_generic.case_and_body]] *)
(*s: type [[AST_generic.case]] *)
and case =
  | Case of tok * pattern
  | Default of tok
  (* For Go, expr can contain some Assign bindings.
   * todo? could merge with regular Case? can 'case x := <-chan' be
   * transformed in a pattern?
   *)
  | CaseEqualExpr of tok * expr

(*e: type [[AST_generic.case]] *)

(* newvar: newscope: usually a PatVar *)
(*s: type [[AST_generic.catch]] *)
and catch = tok (* 'catch', 'except' in Python *) * pattern * stmt

(* newscope: *)
(*e: type [[AST_generic.catch]] *)
(*s: type [[AST_generic.finally]] *)
and finally = tok (* 'finally' *) * stmt

(*e: type [[AST_generic.finally]] *)

(*s: type [[AST_generic.label]] *)
and label = ident

(*e: type [[AST_generic.label]] *)
(*s: type [[AST_generic.label_ident]] *)
and label_ident =
  | LNone (* C/Python *)
  | LId of label (* Java/Go *)
  | LInt of int wrap (* PHP *)
  | LDynamic of expr

(* PHP, woohoo, dynamic break! bailout for CFG *)

(*e: type [[AST_generic.label_ident]] *)

(*s: type [[AST_generic.for_header]] *)
and for_header =
  (* todo? copy Go and have instead
   * ForClassic of simple option * expr * simple option?
   *)
  | ForClassic of
      for_var_or_expr list (* init *) * expr option (* cond *) * expr option (* next *)
  (* newvar: *)
  | ForEach of
      pattern * tok (* 'in' Python, 'range' Go, 'as' PHP, '' Java *) * expr (* pattern 'in' expr *)
  (* sgrep: *)
  | ForEllipsis of tok (* ... *)
  (* Lua. todo: merge with ForEach? *)
  | ForIn of for_var_or_expr list (* init *) * expr list

(* pattern 'in' expr *)

(*e: type [[AST_generic.for_header]] *)

(*s: type [[AST_generic.for_var_or_expr]] *)
and for_var_or_expr =
  (* newvar: *)
  | ForInitVar of entity * variable_definition
  | ForInitExpr of expr

(*e: type [[AST_generic.for_var_or_expr]] *)

(*s: type [[AST_generic.other_stmt_with_stmt_operator]] *)
and other_stmt_with_stmt_operator =
  (* Python/Javascript *)
  (* TODO: used in C# with 'Using', make new stmt TryWithResource? do Java?*)
  | OSWS_With (* newscope: newvar: in OtherStmtWithStmt with LetPattern *)
  (* Ruby *)
  | OSWS_BEGIN
  | OSWS_END (* also in Awk, Perl? *)
  | OSWS_Else_in_try
  (* Rust *)
  | OSWS_UnsafeBlock
  | OSWS_AsyncBlock
  | OSWS_ConstBlock
  | OSWS_ForeignBlock
  | OSWS_ImplBlock
  (* C# *)
  | OSWS_CheckedBlock
  | OSWS_UncheckedBlock

(*e: type [[AST_generic.other_stmt_with_stmt_operator]] *)

(*s: type [[AST_generic.other_stmt_operator]] *)
and other_stmt_operator =
  (* Python *)
  | OS_Delete
  (* todo: reduce? transpile? *)
  | OS_ForOrElse
  | OS_WhileOrElse
  | OS_TryOrElse
  | OS_ThrowFrom
  | OS_ThrowNothing
  | OS_ThrowArgsLocation (* Python2: `raise expr, expr` and `raise expr, expr, exr` *)
  | OS_Pass
  | OS_Async
  (* Java *)
  | OS_Sync
  (* C *)
  | OS_Asm
  (* Go *)
  | OS_Go
  | OS_Defer
  | OS_Fallthrough (* only in Switch *)
  (* PHP *)
  | OS_GlobalComplex (* e.g., global $$x, argh *)
  (* Ruby *)
  | OS_Redo
  | OS_Retry
  (* OCaml *)
  | OS_ExprStmt2
  (* Other *)
  | OS_Todo

(*e: type [[AST_generic.other_stmt_operator]] *)

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)
(* This is quite similar to expr. A few constructs in expr have
 * equivalent here prefixed with Pat (e.g., PaLiteral, PatId). We could
 * maybe factorize with expr, and this may help sgrep, but I think it's
 * cleaner to have a separate type because the scoping rules for a pattern and
 * an expr are quite different and not any expr is allowed here.
 *)
(*s: type [[AST_generic.pattern]] *)
and pattern =
  | PatLiteral of literal
  (* Or-Type, used also to match OCaml exceptions *)
  (* Used with Rust path expressions, with an empty pattern list *)
  | PatConstructor of dotted_ident * pattern list
  (* And-Type*)
  | PatRecord of (dotted_ident * pattern) list bracket
  (* newvar:! *)
  | PatId of ident * id_info (* Usually Local/Param, Global in toplevel let *)
  (* special cases of PatConstructor *)
  | PatTuple of pattern list bracket (* at least 2 elements *)
  (* less: generalize to other container_operator? *)
  | PatList of pattern list bracket
  | PatKeyVal of pattern * pattern (* a kind of PatTuple *)
  (* special case of PatId *)
  | PatUnderscore of tok
  (* OCaml and Scala *)
  | PatDisj of pattern * pattern (* also abused for catch in Java *)
  | PatTyped of pattern * type_
  | PatWhen of pattern * expr (* TODO: add tok, 'when' OCaml, 'if' Scala *)
  | PatAs of pattern * (ident * id_info)
  (* For Go also in switch x.(type) { case int: ... } *)
  | PatType of type_
  (* In catch for Java/PHP, and foreach in Java.
   * less: do instead PatAs (PatType(TyApply, var))?
   *       or even    PatAs (PatConstructor(id, []), var)?
   *)
  | PatVar of type_ * (ident * id_info) option
  (*s: [[AST_generic.pattern]] semgrep extensions cases *)
  (* sgrep: *)
  | PatEllipsis of tok
  | DisjPat of pattern * pattern
  (*e: [[AST_generic.pattern]] semgrep extensions cases *)
  | OtherPat of other_pattern_operator * any list

(*e: type [[AST_generic.pattern]] *)

(*s: type [[AST_generic.other_pattern_operator]] *)
and other_pattern_operator =
  (* Other *)
  | OP_Expr (* todo: Python should transform via expr_to_pattern() below *)
  | OP_Todo

(*e: type [[AST_generic.other_pattern_operator]] *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(*s: type [[AST_generic.type_]] *)
and type_ =
  (* TODO: TyLiteral, for Scala *)
  (* todo? a type_builtin = TInt | TBool | ...? see Literal.
   * or just delete and use (TyN Id) instead?
   *)
  | TyBuiltin of string wrap (* int, bool, etc. could be TApply with no args *)
  (* old: was 'type_ list * type*' , but languages such as C and
   * Go allow also to name those parameters, and Go even allow ParamRest
   * parameters so we need at least 'type_ * attributes', at which point
   * it's better to just use parameter.
   *)
  | TyFun of parameter list * type_ (* return type *)
  (* a special case of TApply, also a special case of TPointer *)
  | TyArray of (* const_expr *) expr option bracket * type_
  | TyTuple of type_ list bracket
  (*s: [[AST_generic.type_]] other cases *)
  (* old: was originally TyApply (name, []), but better to differentiate.
   * todo? may need also TySpecial because the name can actually be
   *  self/parent/static (e.g., in PHP)
   *)
  | TyN of name
  (* covers tuples, list, etc.
   * TODO: merge with TyN IdQualified? name_info has name_typeargs
   * or make more general? TyApply (type_ * type_arguments)?
   *)
  | TyNameApply of dotted_ident * type_arguments
  | TyVar of ident (* type variable in polymorphic types (not a typedef) *)
  | TyAny of tok (* anonymous type, '_' in OCaml, TODO: type bounds Scala? *)
  | TyPointer of tok * type_
  | TyRef of tok * type_ (* C++/Rust *)
  | TyQuestion of type_ * tok (* a.k.a option type *)
  | TyRest of tok * type_ (* '...foo' e.g. in a typescript tuple type *)
  (* intersection types, used for Java Cast, and in Typescript *)
  | TyAnd of type_ * tok (* &, or 'with' in Scala *) * type_
  (* union types in Typescript *)
  | TyOr of type_ * tok (* | *) * type_
  (* Anonymous record type, a.k.a shape in PHP/Hack. See also AndType.
   * Most record types are defined via a TypeDef and are then referenced
   * via a TyName. Here we have flexible record types (a.k.a. rows in OCaml).
   *)
  | TyRecordAnon of tok (* 'struct/shape', fake in other *) * field list bracket
  (* for Go *)
  | TyInterfaceAnon of tok (* 'interface' *) * field list bracket
  (* sgrep-ext: *)
  | TyEllipsis of tok
  (*e: [[AST_generic.type_]] other cases *)
  (*s: [[AST_generic.type_]] OtherXxx case *)
  | OtherType of other_type_operator * any list

(*e: [[AST_generic.type_]] OtherXxx case *)
(*e: type [[AST_generic.type_]] *)

(*s: type [[AST_generic.type_arguments]] *)
and type_arguments = type_argument list

(*e: type [[AST_generic.type_arguments]] *)

(*s: type [[AST_generic.type_argument]] *)
and type_argument =
  | TypeArg of type_
  (* Java only *)
  | TypeWildcard of
      tok (* '?' *) * (bool wrap (* extends|super, true=super *) * type_) option
  (* Rust *)
  | TypeLifetime of ident
  | OtherTypeArg of other_type_argument_operator * any list

(*e: type [[AST_generic.type_argument]] *)
(*s: type [[AST_generic.other_type_argument_operator]] *)
(*e: type [[AST_generic.other_type_argument_operator]] *)

(*s: type [[AST_generic.other_type_operator]] *)
and other_type_operator =
  (* C *)
  (* todo? convert in unique names with TyName? *)
  | OT_StructName
  | OT_UnionName
  | OT_EnumName
  (* PHP *)
  | OT_Variadic (* ???? *)
  (* Rust *)
  | OT_Lifetime
  (* Other *)
  | OT_Expr
  | OT_Arg (* Python: todo: should use expr_to_type() when can *)
  | OT_Todo

(*e: type [[AST_generic.other_type_operator]] *)
and other_type_argument_operator =
  (* Rust *)
  | OTA_Literal
  | OTA_ConstBlock
  (* Other *)
  | OTA_Todo

(*****************************************************************************)
(* Attribute *)
(*****************************************************************************)
(*s: type [[AST_generic.attribute]] *)
and attribute =
  (* a.k.a modifiers *)
  | KeywordAttr of keyword_attribute wrap
  (* a.k.a decorators, annotations *)
  | NamedAttr of tok (* @ *) * name * arguments bracket
  (*s: [[AST_generic.attribute]] OtherXxx case *)
  | OtherAttribute of other_attribute_operator * any list

(*e: [[AST_generic.attribute]] OtherXxx case *)
(*e: type [[AST_generic.attribute]] *)

(*s: type [[AST_generic.keyword_attribute]] *)
and keyword_attribute =
  (* the classic C modifiers *)
  | Static
  | Volatile
  | Extern
  (* for class fields/methods *)
  | Public
  | Private
  | Protected
  | Abstract
  | Final
  | Override (* override *)
  (* for vars (JS) *)
  | Var
  | Let
  (* for fields (kinda types) *)
  | Mutable (* a.k.a 'var' in Scala *)
  | Const (* a.k.a 'readonly' in Typescript, 'val' in Scala *)
  (* less: should be part of the type? *)
  | Optional
  (* Typescript '?' *)
  | NotNull (* Typescript '!' *)
  (* for functions/methods *)
  | Generator
  (* '*' in JS *)
  | Async
  | Recursive
  | MutuallyRecursive
  | Inline
  (* for methods *)
  | Ctor
  | Dtor
  | Getter
  | Setter
  (* Rust *)
  | Unsafe
  | DefaultImpl
  (* Scala *)
  | Lazy
  | CaseClass

(* By name application in Scala, via => T, in parameter *)

(* unstable, RFC 1210 *)

(*e: type [[AST_generic.keyword_attribute]] *)

(*s: type [[AST_generic.other_attribute_operator]] *)
and other_attribute_operator =
  (* Java *)
  | OA_StrictFP
  | OA_Transient
  | OA_Synchronized
  | OA_Native
  | OA_Default
  | OA_AnnotThrow
  (* Other *)
  | OA_Expr
  | OA_Todo

(* todo: Python, should transform in NamedAttr when can *)

(*e: type [[AST_generic.other_attribute_operator]] *)

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
(* definition (or just declaration sometimes) *)
(*s: type [[AST_generic.definition]] *)
and definition = entity * definition_kind

(*e: type [[AST_generic.definition]] *)

(* old: type_: type_ option; but redundant with the type information in
 * the different definition_kind, as well as in id_info, and does not
 * have any meanings for certain defs (e.g., ClassDef) so not worth
 * factoring.
 * update: this could be useful in OCaml when you explicitely type an
 * entity (as in 'let (f: int -> int) = fun i -> i + 1), but we
 * currently abuse id_info.id_type for that.
 *
 * see special_multivardef_pattern below for many vardefs in one entity in
 * ident.
 * less: could be renamed entity_def, and name is a kind of entity_use.
 *)
(*s: type [[AST_generic.entity]] *)
and entity = {
  (* In Ruby you can define a class with a qualified name as in
   * class A::B::C, and even dynamically.
   * In C++ you can define a method with a class qualifier outside a class,
   * hence the use of name_or_dynamic below and not just ident.
   *)
  name : name_or_dynamic;
  (*s: [[AST_generic.entity]] attribute field *)
  attrs : attribute list;
  (*e: [[AST_generic.entity]] attribute field *)
  (*s: [[AST_generic.entity]] id info field *)
  (*e: [[AST_generic.entity]] id info field *)
  (*s: [[AST_generic.entity]] other fields *)
  (*e: [[AST_generic.entity]] other fields *)
  tparams : type_parameter list;
}

(*e: type [[AST_generic.entity]] *)

(*s: type [[AST_generic.definition_kind]] *)
and definition_kind =
  (* newvar: can be used also for methods, nested functions, lambdas.
   * note: can have empty "body" when the def is actually a declaration
   * in a header file (called a prototype in C).
   *)
  | FuncDef of function_definition
  (* newvar: can be used also for constants.
   * can contain special_multivardef_pattern ident in which case vinit
   * is the pattern assignment.
   *)
  | VarDef of variable_definition
  (* FieldDefColon can be used only inside a record (in a FieldStmt).
   * This used to be merged with VarDef, but in semgrep we don't want
   * a VarDef to match a field definition for certain languages
   * (e.g., JS, OCaml), and we definitely don't want the
   * vardef_to_assign equivalence to be used on FieldDefColon.
   * TODO? maybe merge back with VarDef but add a field in
   *  variable_definition saying whether it's using a colon syntax?
   * TODO? merge instead JS objects with Containers?
   *
   * Note that we could have used a FieldVar in the field type instead
   * of this FieldDef here, which would be more precise, but
   * this complicates things in semgrep where it's convenient to have
   * a uniform FieldStmt(DefStmt) that covers field and methods
   * (see m_list__m_field in semgrep).
   * Note that FieldDefColon where vinit is a Lambda instead be converted
   * in a FuncDef!
   *)
  | FieldDefColon of (* todo: tok (*':'*) * *) variable_definition
  | ClassDef of class_definition
  (*s: [[AST_generic.definition_kind]] other cases *)
  | TypeDef of type_definition
  | ModuleDef of module_definition
  | MacroDef of macro_definition
  (*x: [[AST_generic.definition_kind]] other cases *)
  (* in a header file (e.g., .mli in OCaml or 'module sig') *)
  | Signature of type_
  (*x: [[AST_generic.definition_kind]] other cases *)
  (* Only used inside a function.
   * Needed for languages without local VarDef (e.g., Python/PHP)
   * where the first use is also its declaration. In that case when we
   * want to access a global we need to disambiguate with creating a new
   * local.
   *)
  | UseOuterDecl of tok (* 'global' or 'nonlocal' in Python, 'use' in PHP *)
  | OtherDef of other_def_operator * any list

(*e: [[AST_generic.definition_kind]] other cases *)
and other_def_operator = OD_Todo

(*e: type [[AST_generic.definition_kind]] *)

(* template/generics/polymorphic-type *)
(*s: type [[AST_generic.type_parameter]] *)
and type_parameter = ident * type_parameter_constraint list

(*e: type [[AST_generic.type_parameter]] *)
(*s: type [[AST_generic.type_parameter_constraints]] *)
(*e: type [[AST_generic.type_parameter_constraints]] *)
(*s: type [[AST_generic.type_parameter_constraint]] *)
and type_parameter_constraint =
  | Extends of type_
  | HasConstructor of tok
  | OtherTypeParam of other_type_parameter_operator * any list

(*e: type [[AST_generic.type_parameter_constraint]] *)
and other_type_parameter_operator =
  (* Rust *)
  | OTP_Lifetime
  | OTP_Ident
  | OTP_Constrained
  | OTP_Const
  (* Other *)
  | OTP_Todo

(* ------------------------------------------------------------------------- *)
(* Function (or method) definition *)
(* ------------------------------------------------------------------------- *)
(* less: could be merged with variable_definition *)
(*s: type [[AST_generic.function_definition]] *)
and function_definition = {
  fkind : function_kind wrap;
  fparams : parameters;
  frettype : type_ option;
  (* return type *)
  (* newscope:
   * note: can be empty statement for methods in interfaces.
   * update: can also be empty when used in a Partial.
   * can be simple expr too for JS lambdas, so maybe fbody type?
   * FExpr | FNothing | FBlock ?
   * use stmt list bracket instead?
   *)
  fbody : stmt;
}

(*e: type [[AST_generic.function_definition]] *)
(* We don't really care about the function_kind in semgrep, but who
 * knows maybe one day we will. We care about the token in the
 * function_kind wrap in fkind though for semgrep for accurate range.
 *)
and function_kind =
  | Function
  (* This is a bit redundant with having the func in a field *)
  | Method
  (* Also redundant; can just check if the fdef is in a Lambda *)
  | LambdaKind
  | Arrow
  | BlockCases

(* for Scala *)

(* a.k.a short lambdas *)

(*s: type [[AST_generic.parameters]] *)
and parameters = parameter list

(*e: type [[AST_generic.parameters]] *)
(*s: type [[AST_generic.parameter]] *)
(* newvar: *)
and parameter =
  | ParamClassic of parameter_classic
  (*s: [[AST_generic.parameter]] other cases *)
  | ParamPattern of pattern (* in OCaml, but also now JS, and Python2 *)
  (*e: [[AST_generic.parameter]] other cases *)
  (* Both those ParamXxx used to be handled as a ParamClassic with special
   * VariadicXxx attribute in p_attr, but they are used in so many
   * languages that it's better to move then in a separate type.
   * We could do a ParamXxx of tok * ident, but some of those params
   * may have attribute, they need a id_info, so simpler to reuse
   * parameter_classic, but pname is always a Some (except for Ruby).
   * ParamRest could be called ParamSpread.
   * alt: move that in ParamPattern instead.
   *)
  | ParamRest of tok (* '...' in JS, '*' in Python *) * parameter_classic
  | ParamHashSplat of tok (* '**' in Python *) * parameter_classic
  (*s: [[AST_generic.parameter]] semgrep extension cases *)
  (* sgrep: ... in parameters
   * note: foo(...x) of Js/Go is using the ParamRest, not this *)
  | ParamEllipsis of tok
  (*e: [[AST_generic.parameter]] semgrep extension cases *)
  (*s: [[AST_generic.parameter]] OtherXxx case *)
  | OtherParam of other_parameter_operator * any list

(*e: [[AST_generic.parameter]] OtherXxx case *)
(*e: type [[AST_generic.parameter]] *)

(* less: could be merged with variable_definition, or pattern
 * less: could factorize pname/pattrs/pinfo with entity
 *)
(*s: type [[AST_generic.parameter_classic]] *)
and parameter_classic = {
  (* alt: use a 'ParamNoIdent of type_' when pname is None instead? *)
  pname : ident option;
  ptype : type_ option;
  pdefault : expr option;
  (*s: [[AST_generic.parameter_classic]] attribute field *)
  pattrs : attribute list;
  (*e: [[AST_generic.parameter_classic]] attribute field *)
  (*s: [[AST_generic.parameter_classic]] id info field *)
  (* naming *)
  pinfo : id_info;
      (* Always Param *)
      (*e: [[AST_generic.parameter_classic]] id info field *)
}

(*e: type [[AST_generic.parameter_classic]] *)
(*s: type [[AST_generic.other_parameter_operator]] *)
and other_parameter_operator =
  (* Python *)
  (* single '*' or '/' to delimit regular parameters from special one *)
  | OPO_SingleStarParam
  | OPO_SlashParam
  (* Go *)
  | OPO_Receiver (* of parameter_classic, used to tag the "self" parameter*)
  (* PHP/Ruby *)
  | OPO_Ref (* of parameter_classic *)
  (* Other *)
  | OPO_Todo

(*e: type [[AST_generic.other_parameter_operator]] *)

(* ------------------------------------------------------------------------- *)
(* Variable definition *)
(* ------------------------------------------------------------------------- *)
(* Also used for constant_definition with attrs = [Const].
 * Also used for field definition in a class (and record).
 * less: could use for function_definition with vinit = Some (Lambda (...))
 *  but maybe useful to explicitely makes the difference for now?
 *)
(*s: type [[AST_generic.variable_definition]] *)
and variable_definition = {
  (* todo? should remove vinit and transform a VarDef with init with a VarDef
   * followed by an Assign (possibly to Null). See vardef_to_assign().
   *)
  vinit : expr option;
  (* less: (tok * expr) option? *)
  vtype : type_ option;
}

(*e: type [[AST_generic.variable_definition]] *)

(* ------------------------------------------------------------------------- *)
(* Type definition *)
(* ------------------------------------------------------------------------- *)
(*s: type [[AST_generic.type_definition]] *)
and type_definition = { tbody : type_definition_kind }

(*e: type [[AST_generic.type_definition]] *)

(*s: type [[AST_generic.type_definition_kind]] *)
and type_definition_kind =
  | OrType of or_type_element list (* enum/ADTs *)
  (* field.vtype should be defined here
   * record/struct (for class see class_definition)
   *)
  | AndType of field list bracket
  (* a.k.a typedef in C (and alias type in Go) *)
  | AliasType of type_
  (* Haskell/Hack/Go ('type x foo' vs 'type x = foo' in Go) *)
  | NewType of type_
  | Exception of ident (* same name than entity *) * type_ list
  | OtherTypeKind of other_type_kind_operator * any list

(*e: type [[AST_generic.type_definition_kind]] *)

(*s: type [[AST_generic.or_type_element]] *)
and or_type_element =
  (* OCaml *)
  | OrConstructor of ident * type_ list
  (* C *)
  | OrEnum of ident * expr option
  (* Java? *)
  | OrUnion of ident * type_
  | OtherOr of other_or_type_element_operator * any list

(*e: type [[AST_generic.or_type_element]] *)

(*s: type [[AST_generic.other_or_type_element_operator]] *)
and other_or_type_element_operator =
  (* Java, Kotlin *)
  | OOTEO_EnumWithMethods
  | OOTEO_EnumWithArguments

(*e: type [[AST_generic.other_or_type_element_operator]] *)

(* ------------------------------------------------------------------------- *)
(* Object/struct/record/class field definition *)
(* ------------------------------------------------------------------------- *)

(* Field definition and use, for classes, objects, and records.
 * note: I don't call it field_definition because it's used both to
 * define the shape of a field (a definition), and when creating
 * an actual field (a value).
 *
 * old: there used to be a FieldVar and FieldMethod similar to
 * VarDef and FuncDef but they are now converted into a FieldStmt(DefStmt).
 * This simplifies semgrep so that a function pattern can match
 * toplevel functions, nested functions, and methods.
 * Note that for FieldVar we sometimes converts it to a FieldDefColon
 * (which is very similar to a VarDef) because some people don't want a VarDef
 * to match a field definition in certain languages (e.g., Javascript) where
 * the variable declaration and field definition have a different syntax.
 * Note: the FieldStmt(DefStmt(FuncDef(...))) can have empty body
 * for interface methods.
 *
 * Note that not all stmt in FieldStmt are definitions. You can have also
 * a Block like in Kotlin for 'init' stmts.
 *)
(*s: type [[AST_generic.field]] *)
and field =
  | FieldStmt of stmt
  (*s: [[AST_generic.field]] other cases *)
  (* less: could abuse FieldStmt(ExprStmt(IdSpecial(Spread))) for that *)
  | FieldSpread of tok (* ... *) * expr

(*e: [[AST_generic.field]] other cases *)
(*e: type [[AST_generic.field]] *)

(*s: type [[AST_generic.other_type_kind_operator]] *)
and other_type_kind_operator = (* OCaml *)
  | OTKO_AbstractType

(*e: type [[AST_generic.other_type_kind_operator]] *)

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)
(* less: could be a special kind of type_definition *)
(*s: type [[AST_generic.class_definition]] *)
and class_definition = {
  ckind : class_kind wrap;
  (* usually just one parent, and type_ should be a TyApply *)
  (* TODO? the parent can have arguments, as in Scala, to call super
   * or when used inside a New.
   *)
  cextends : type_ list;
  (* class_kind in type_ must be Interface *)
  cimplements : type_ list;
  (* class_kind in type_ is usually a Trait *)
  cmixins : type_ list;
  (* PHP 'uses' *)
  (* for Java Record or Scala Classes; we could transpile them into fields *)
  cparams : parameters;
  (* newscope:
   * note: this can be an empty fake bracket when used in Partial.
   * TODO? use an option here?
   *)
  cbody : field list bracket;
}

(*e: type [[AST_generic.class_definition]] *)
(*s: type [[AST_generic.class_kind]] *)
(* invariant: this must remain a simple enum; Map_AST relies on it *)
and class_kind =
  | Class
  | Interface
  | Trait
  (* Kotlin, Scala *)
  | Object
  (* Java 'record', Scala 'case class' *)
  | RecordClass
  (* java: *)
  | AtInterface

(* @interface, a.k.a annotation type declaration *)

(*e: type [[AST_generic.class_kind]] *)

(* ------------------------------------------------------------------------- *)
(* Module definition  *)
(* ------------------------------------------------------------------------- *)
(*s: type [[AST_generic.module_definition]] *)
and module_definition = { mbody : module_definition_kind }

(*e: type [[AST_generic.module_definition]] *)

(*s: type [[AST_generic.module_definition_kind]] *)
and module_definition_kind =
  | ModuleAlias of dotted_ident
  (* newscope: *)
  | ModuleStruct of dotted_ident option * item list
  | OtherModule of other_module_operator * any list

(*e: type [[AST_generic.module_definition_kind]] *)

(*s: type [[AST_generic.other_module_operator]] *)
and other_module_operator =
  (* OCaml (functors and their applications) *)
  | OMO_Todo

(*e: type [[AST_generic.other_module_operator]] *)

(* ------------------------------------------------------------------------- *)
(* Macro definition *)
(* ------------------------------------------------------------------------- *)
(* Used by cpp in C/C++ *)
(*s: type [[AST_generic.macro_definition]] *)
and macro_definition = { macroparams : ident list; macrobody : any list }

(*e: type [[AST_generic.macro_definition]] *)

(*****************************************************************************)
(* Directives (Module import/export, package) *)
(*****************************************************************************)
(*s: type [[AST_generic.directive]] *)
(* It is tempting to simplify all those ImportXxx in a simpler
 * 'Import of dotted_ident * ...', but module_name is not always a DottedName
 * so it is better to clearly separate what is module_name/namespace from an
 * entity (in this module/namespace) even though some languages such as Python
 * blurs the difference.
 *)
and directive =
  (* newvar: *)
  | ImportFrom of
      tok (* 'import'/'from' for Python, 'include' for C *)
      * module_name
      * ident
      * alias option (* as name alias *)
  (*s: [[AST_generic.directive]] other imports *)
  | ImportAs of tok * module_name * alias option (* as name *)
  (* bad practice! hard to resolve name locally *)
  | ImportAll of tok * module_name * tok (* '.' in Go, '*' in Java/Python, '_' in Scala *)
  (*e: [[AST_generic.directive]] other imports *)
  (*s: [[AST_generic.directive]] package cases *)
  (* packages are different from modules in that multiple files can reuse
   * the same package name; they are agglomarated in the same package
   *)
  | Package of tok * dotted_ident (* a.k.a namespace *)
  (* for languages such as C++/PHP/Scala with scoped namespaces
   * alt: Package of tok * dotted_ident * item list bracket, but less
   * consistent with other directives, so better to use PackageEnd.
   *)
  | PackageEnd of tok
  (*e: [[AST_generic.directive]] package cases *)
  | Pragma of ident * any list
  (*s: [[AST_generic.directive]] OtherXxx cases *)
  | OtherDirective of other_directive_operator * any list

(*e: [[AST_generic.directive]] OtherXxx cases *)
(*e: type [[AST_generic.directive]] *)

(*s: type [[AST_generic.alias]] *)
(* ... as name *)
and alias = ident * id_info

(*e: type [[AST_generic.alias]] *)

(*s: type [[AST_generic.other_directive_operator]] *)
and other_directive_operator =
  (* Javascript *)
  | OI_Export
  | OI_ReExportNamespace
  (*e: type [[AST_generic.other_directive_operator]] *)
  (* PHP *)
  (* TODO: Declare, move OE_UseStrict here for JS? *)
  (* Ruby *)
  | OI_Alias
  | OI_Undef
  (* Rust *)
  | OI_Extern

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
(* item (a.k.a toplevel element, toplevel decl) is now equal to stmt.
 * Indeed, many languages allow nested functions, nested class definitions,
 * and even nested imports, so it is just simpler to merge item with stmt.
 * This simplifies sgrep too.
 * less: merge with field?
 *)
(*s: type [[AST_generic.item]] *)
and item = stmt

(*e: type [[AST_generic.item]] *)

(*s: type [[AST_generic.program]] *)
and program = item list

(*e: type [[AST_generic.program]] *)

(*****************************************************************************)
(* Partial *)
(*****************************************************************************)
(* sgrep: this is only used by semgrep *)
and partial =
  (* partial defs. The fbody or cbody in definition will be empty. *)
  | PartialDef of definition
  (* partial stmts *)
  | PartialIf of tok * expr (* todo? bracket *)
  | PartialTry of tok * stmt
  | PartialCatch of catch
  | PartialFinally of tok * stmt
  (* partial objects (just used in JSON and YAML patterns for now)
   * alt: todo? could be considered a full thing and use Fld?
   *)
  | PartialSingleField of string wrap (* id or str *) * tok (*:*) * expr
  (* not really a partial, but the partial machinery can help with that *)
  | PartialLambdaOrFuncDef of function_definition

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

(* mentioned in many OtherXxx so must be part of the mutually recursive type *)
(*s: type [[AST_generic.any]] *)
and any =
  (*s: [[AST_generic.any]] semgrep cases *)
  | E of expr
  | S of stmt
  | Ss of stmt list
  (*e: [[AST_generic.any]] semgrep cases *)
  (* also used for semgrep *)
  | T of type_
  | P of pattern
  | At of attribute
  | Fld of field
  | Args of argument list
  | Partial of partial
  (* misc *)
  | I of ident
  | Str of string wrap
  | Def of definition
  | Dir of directive
  | Pr of program
  (*s: [[AST_generic.any]] other cases *)
  (* todo: get rid of some? *)
  | Modn of module_name
  | ModDk of module_definition_kind
  | En of entity
  | Pa of parameter
  | Ar of argument
  | Dk of definition_kind
  | Di of dotted_ident
  | Lbli of label_ident
  | NoD of name_or_dynamic
  | Tk of tok
  | TodoK of todo_kind
(*e: [[AST_generic.any]] other cases *)
(*e: type [[AST_generic.any]] *)
[@@deriving show { with_path = false }, eq, hash]

(* with tarzan *)

(*s: constant [[AST_generic.special_multivardef_pattern]] *)
(* In JS one can do 'var {x,y} = foo();'. We used to transpile that
 * in multiple vars, but in sgrep one may want to match over those patterns.
 * However those multivars do not fit well with the (entity * definition_kind)
 * model we currently use, so for now we need this ugly hack of converting
 * the statement above in
 * ({name = "!MultiVarDef"}, VarDef {vinit = Assign (Record {...}, foo())}).
                                     * This is bit ugly, but at some point we may want to remove completely
                                     * VarDef by transforming them in Assign (see vardef_to_assign() below)
                                     * so this temporary hack is not too bad.
 *)
let special_multivardef_pattern = AST_generic_.special_multivardef_pattern

(*e: constant [[AST_generic.special_multivardef_pattern]] *)

(*****************************************************************************)
(* Error *)
(*****************************************************************************)

(*s: exception [[AST_generic.Error]] *)
(* this can be used in the xxx_to_generic.ml file to signal limitations,
 * and can be captured in Error_code.exn_to_error to pinpoint the error
 * location.
 *)
exception Error of string * Parse_info.t

(*e: exception [[AST_generic.Error]] *)

(*s: function [[AST_generic.error]] *)
let error tok msg = raise (Error (msg, tok))

(*e: function [[AST_generic.error]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* see also AST_generic_helpers.ml *)

(*s: constant [[AST_generic.sid_TODO]] *)
(* before Naming_AST.resolve can do its job *)
let sid_TODO = -1

(*e: constant [[AST_generic.sid_TODO]] *)

(*s: constant [[AST_generic.empty_name_info]] *)
let empty_name_info = { name_qualifier = None; name_typeargs = None }

(*e: constant [[AST_generic.empty_name_info]] *)

(*s: constant [[AST_generic.empty_var]] *)
let empty_var = { vinit = None; vtype = None }

(*e: constant [[AST_generic.empty_var]] *)

(*s: function [[AST_generic.empty_id_info]] *)
let empty_id_info () =
  { id_resolved = ref None; id_type = ref None; id_constness = ref None }

(*e: function [[AST_generic.empty_id_info]] *)

(*s: function [[AST_generic.basic_id_info]] *)
let basic_id_info resolved =
  {
    id_resolved = ref (Some resolved);
    id_type = ref None;
    id_constness = ref None;
  }

(*e: function [[AST_generic.basic_id_info]] *)

(*s: function [[AST_generic.param_of_id]] *)
let param_of_id id =
  {
    pname = Some id;
    pdefault = None;
    ptype = None;
    pattrs = [];
    pinfo = basic_id_info (Param, sid_TODO);
  }

(*e: function [[AST_generic.param_of_id]] *)
(*s: function [[AST_generic.param_of_type]] *)
let param_of_type typ =
  {
    ptype = Some typ;
    pname = None;
    pdefault = None;
    pattrs = [];
    pinfo = empty_id_info ();
  }

(*e: function [[AST_generic.param_of_type]] *)

(*s: function [[AST_generic.basic_entity]] *)
let basic_entity id attrs =
  let idinfo = empty_id_info () in
  { name = EN (Id (id, idinfo)); attrs; tparams = [] }

(*e: function [[AST_generic.basic_entity]] *)

let s skind =
  {
    s = skind;
    s_id = AST_utils.Node_ID.create ();
    s_use_cache = false;
    s_backrefs = None;
    s_bf = None;
  }

(*s: function [[AST_generic.basic_field]] *)
let basic_field id vopt typeopt =
  let entity = basic_entity id [] in
  FieldStmt (s (DefStmt (entity, VarDef { vinit = vopt; vtype = typeopt })))

(*e: function [[AST_generic.basic_field]] *)

(*s: function [[AST_generic.attr]] *)
let attr kwd tok = KeywordAttr (kwd, tok)

(*e: function [[AST_generic.attr]] *)
(*s: function [[AST_generic.arg]] *)
let arg e = Arg e

(*e: function [[AST_generic.arg]] *)

(*s: function [[AST_generic.fake]] *)
(* Try avoid using them! if you build new constructs, you should try
 * to derive the tokens in those new constructs from existing constructs.
 *)
let fake s = Parse_info.fake_info s

(*e: function [[AST_generic.fake]] *)
(*s: function [[AST_generic.fake_bracket]] *)
let fake_bracket x = (fake "(", x, fake ")")

(*e: function [[AST_generic.fake_bracket]] *)
(*s: function [[AST_generic.unbracket]] *)
let unbracket (_, x, _) = x

(*e: function [[AST_generic.unbracket]] *)
(* bugfix: I used to put ";" but now Parse_info.str_of_info prints
 * the string of a fake info
 *)
let sc = Parse_info.fake_info ""

let unhandled_keywordattr (s, t) =
  NamedAttr (t, Id ((s, t), empty_id_info ()), fake_bracket [])

let exprstmt e = s (ExprStmt (e, sc))

let fieldEllipsis t = FieldStmt (exprstmt (Ellipsis t))

let empty_fbody = s (Block (fake_bracket []))

let empty_body = fake_bracket []

(*s: function [[AST_generic.stmt1]] *)
let stmt1 xs =
  match xs with
  | [] -> s (Block (fake_bracket []))
  | [ st ] -> st
  | xs -> s (Block (fake_bracket xs))

(*e: function [[AST_generic.stmt1]] *)

(*e: pfff/h_program-lang/AST_generic.ml *)

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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A generic AST, to factorize similar "analysis" in different programming
 * languages (e.g., naming, semantic code highlighting, semgrep).
 *
 * Right now this generic AST is mostly the factorized union of:
 *  - Python, Ruby, Lua
 *  - Javascript, Typescript
 *  - PHP, Hack
 *  - Java, CSharp, Kotlin
 *  - C, C++
 *  - Go
 *  - JSON, YAML, HCL
 *  - OCaml, Scala, Rust
 *
 * rational: In the end, programming languages have a lot in Common.
 * Even though some interesting analysis are probably better done on a
 * per-language basis, many analysis are simple and require just an
 * AST and a visitor. One could duplicate those analysis for each language
 * or design an AST (this file) generic enough to factorize all those
 * analysis (e.g., unused entity). Note that we want to remain
 * as precise as possible and not lose too much information while going
 * from the specific language AST to the generic AST. We don't want
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
 *    update: but we now have a more precise function_body type
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
 * Note that this generic AST has become gradually more and more a
 * generic CST, to fix issues in autofix in Semgrep.
 * TODO? it may be time to rename this file CST_generic.ml
 *
 * todo:
 *  - add C++ (argh)
 *  - see ast_fuzzy.ml todos for ideas to use AST_generic for sgrep?
 *
 * related work:
 *  - ast_fuzzy.ml (in pfff)
 *  - github semantic (seems dead)
 *    https://github.com/github/semantic
 *  - UAST of babelfish
 *    https://doc.bblf.sh/uast/uast-specification-v2.html
 *  - Coverity common program representation?
 *  - Semmle internal common representation?
 *  - Sonarcube generic language
 *    https://github.com/SonarSource/slang
 *  - Facebook Infer SIL (for C++, Java, Objective-C)
 *  - Dawson Engler and Fraser Brown micro-checkers for multiple languages
 *  - Comby common representation by Rijnard,
 *    see "Lightweight Multi-language syntax transformation", but does not
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
 *)

(* Provide hash_* and hash_fold_* for the core ocaml types *)
open Ppx_hash_lib.Std.Hash.Builtin

(* ppx_hash refuses to hash mutable fields but we do it anyway. *)
let hash_fold_ref hash_fold_x acc x = hash_fold_x acc !x

(*****************************************************************************)
(* Token (leaf) *)
(*****************************************************************************)
(* Contains among other things the position of the token through
 * the Parse_info.token_location embedded inside it, as well as the
 * transformation field that makes possible spatch on the code.
 *)
type tok = Parse_info.t [@@deriving show]

(* sgrep: we do not care about position when comparing for equality 2 ASTs.
 * related: Lib_AST.abstract_position_info_any and then use OCaml generic '='.
 *)
let equal_tok _t1 _t2 = true

let hash_tok _t = 0

let hash_fold_tok acc _t = acc

(* a shortcut to annotate some information with position information *)
type 'a wrap = 'a * tok [@@deriving show, eq, hash]

(* Use for round(), square[], curly{}, and angle<> brackets.
 * note: in theory we should not care about those tokens in an AST,
 * but they are useful to report correct ranges in sgrep when we match
 * something that can just be those brackets (e.g., an empty container).
 *)
type 'a bracket = tok * 'a * tok [@@deriving show, eq, hash]

(* semicolon, a FakeTok in languages that do not require them (e.g., Python).
 * alt: tok option.
 * See the sc value also at the end of this file to build an sc.
 *)
type sc = tok [@@deriving show, eq, hash]

(* an AST element not yet handled; works with the Xx_Todo and Todo in any *)
type todo_kind = string wrap [@@deriving show, eq, hash]

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

type ident = string wrap [@@deriving show, eq, hash]

(* Usually separated by a '.', but can be used also with '::' separators.
 * less: we often need to get the last elt or adjust the qualifier part,
 * so maybe we should define it as = ident list * ident
 *)
type dotted_ident = ident list (* at least 1 element *)
[@@deriving show, eq, hash]

(* module_name can also be used for a package name or a namespace *)
type module_name =
  | DottedName of dotted_ident (* ex: Python *)
  (* in FileName the '/' is similar to the '.' in DottedName *)
  | FileName of string wrap (* ex: Js import, C #include, Go import *)
[@@deriving show { with_path = false }, eq, hash]

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
(* a single unique gensym'ed number. See gensym() below *)
type sid = int

and resolved_name = resolved_name_kind * sid

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
[@@deriving show { with_path = false }, eq, hash]

(* Start of big mutually recursive types because of the use of 'any'
 * in OtherXxx *)

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
 * DEBT? Sometimes some DotAccess should really be transformed in IdQualified
 * with a better qualifier because the obj is actually the name of a package
 * or module, but you may need advanced semantic information and global
 * analysis to disambiguate.
 *
 * less: factorize the id_info in both and inline maybe name_info
 *)
type name =
  | Id of ident * id_info
  | IdQualified of (ident * name_info) * id_info

and name_info = {
  name_qualifier : qualifier option;
  name_typeargs : type_arguments option; (* Java/Rust *)
}

(* TODO: not enough in OCaml with functor and type args or C++ templates.
 * We will need to merge name_typeargs and name_qualifier and have a
 * qualifier list instead (with QId and QTemplateId like in ast_cpp.ml)
 *)
and qualifier =
  (* ::, Ruby, C++, also '`' abuse for PolyVariant in OCaml *)
  | QTop of tok
  (* Java, OCaml *)
  | QDots of dotted_ident
  (* Ruby *)
  | QExpr of expr * tok

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

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* todo? we could store more semantic information at each expr node,
 * e.g., type information, constant evaluation.
 *)
and expr = {
  e : expr_kind;
  e_id : int;
  (* used to quickly get the range of an expression *)
  mutable e_range :
    (Parse_info.token_location * Parse_info.token_location) option;
      [@equal fun _a _b -> true] [@hash.ignore]
}

and expr_kind =
  (* basic (atomic) values *)
  | L of literal
  (* composite values *)
  | Container of container_operator * expr list bracket
  | Comprehension of container_operator * comprehension bracket
  (* And-type (field.vinit should be a Some) *)
  | Record of field list bracket
  (* Or-type (could be used instead of Container, Cons, Nil, etc.).
   * (ab)used also for polymorphic variants where qualifier is QTop with
   * the '`' token.
   *
   * Note that in OCaml constructors do not always need brackets.
   * For example, you can have 'let x = Foo 1'. However, you often
   * need the parenthesis, for example when you have multiple arguments
   * (e.g., 'let x = Foo (1,2)').
   * In that case, we could make '(1,2)' a single Tuple argument, and
   * that is mostly what is done in ast_ml.ml, but this is a bit ugly.
   * Morever, even with a single argument, you need extra parenthesis
   * if the argument is a complex expression (e.g., 'let x = Foo(1+2)').
   * And if those parenthesis are not in the AST, then matching range
   * or autofix on such construct may fail.
   * Finally, in some languages like Scala or Rust, constructors require
   * the parenthesis.
   * Thus, it is simpler to add the bracket here, because this is mostly
   * how user think.
   *)
  | Constructor of name * expr list bracket
  (* see also Call(IdSpecial (New,_), [ArgType _;...] for other values *)
  | N of name
  | IdSpecial of special wrap (*e: [[AST_generic.expr]] other identifier cases *)
  (* operators and function application *)
  | Call of expr * arguments bracket (* can be fake '()' for OCaml/Ruby *)
  (* TODO? Separate regular Calls from OpCalls where no need bracket and Arg *)
  (* (XHP, JSX, TSX), could be transpiled also (done in IL.ml?) *)
  | Xml of xml
  (* IntepolatedString of expr list is simulated with a
   * Call(IdSpecial (Concat ...)) *)

  (* The left part should be an lvalue (Id, DotAccess, ArrayAccess, Deref)
   * but it can also be a pattern (Container, even Record), but
   * you should really use LetPattern for that.
   * Assign can also be abused to declare new variables, but you should use
   * variable_definition for that.
   * less: should be in stmt, but most languages allow this at expr level :(
   * todo: see IL.ml where we normalize this AST with expr/instr/stmt
   * update: should even be in a separate simple_stmt, as in Go
   *)
  | Assign of
      expr * tok (* '=', '<-' in OCaml. ':=' Go is AssignOp (Eq) *) * expr
  (* less: could desugar in Assign, should be only binary_operator *)
  | AssignOp of expr * operator wrap * expr
  (* newvar:! newscope:? in OCaml yes but we miss the 'in' part here  *)
  | LetPattern of pattern * expr (*e: [[AST_generic.expr]] other assign cases *)
  (* can be used for Record, Class, or Module access depending on expr.
   * In the last case it should be rewritten as a (N IdQualified) with a
   * qualifier though.
   *)
  | DotAccess of expr * tok (* ., ::, ->, # *) * name_or_dynamic
  (* in Js ArrayAccess is also abused to perform DotAccess (..., FDynamic) *)
  | ArrayAccess of expr * expr bracket
  (* could also use ArrayAccess with a Tuple rhs, or use a special *)
  | SliceAccess of
      expr
      * (expr option (* lower *) * expr option (* upper *) * expr option)
        (* step *)
        bracket
  (* very special value. 'fbody' is usually an FExpr. *)
  | Lambda of function_definition
  (* also a special value. Usually an argument of a New
   * (used in Java, Javascript, etc.) *)
  | AnonClass of class_definition
  (* a.k.a ternary expression. Note that even in languages like OCaml
   * where 'if's are expressions, we still prefer to use the stmt 'If'
   * because it allows an optional else part. We need to sometimes
   * wrap those stmts inside an OE_StmtExpr though.
   * TODO: add toks? TODO? in C++ the second expr can be an option
   *)
  | Conditional of expr * expr * expr
  | Yield of tok * expr option * bool (* 'from' for Python *)
  | Await of tok * expr
  (* Send/Recv of Go are currently in OtherExpr *)
  | Cast of type_ * tok (* ':' or leftmost '(' or 'as' *) * expr
  (* less: should be in statement *)
  | Seq of expr list (* at least 2 elements *)
  (* less: could be in Special, but pretty important so I've lifted them here*)
  | Ref of tok (* &, address of *) * expr
  | DeRef of tok (* '*' in C, '!' or '<-' in OCaml, ^ in Reason *) * expr (*e: [[AST_generic.expr]] other cases *)
  (* sgrep: ... in expressions, args, stmts, items, and fields
   * (and unfortunately also in types in Python) *)
  | Ellipsis of tok (* '...' *)
  | DeepEllipsis of expr bracket (* <... ...> *)
  | DisjExpr of expr * expr
  | TypedMetavar of ident * tok (* : *) * type_
  (* for ellipsis in method chaining *)
  | DotAccessEllipsis of expr * tok (* '...' *)
  (* TODO: other_expr_operator wrap, so enforce at least one token instead
   * of relying that the any list contains at least one token *)
  | OtherExpr of other_expr_operator * any list

and literal =
  | Bool of bool wrap
  (* the numbers are an option because OCaml numbers
   * may not be able to represent all numbers. For example, OCaml integers
   * are limited to 63 bits, but C integers can use 64 bits.
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

(* The type of an unknown constant. *)
and const_type = Cbool | Cint | Cstr | Cany

(* set by the constant propagation algorithm and used in semgrep *)
and constness = Lit of literal | Cst of const_type | NotCst

and container_operator =
  | Array (* todo? designator? use ArrayAccess for designator? *)
  | List
  | Set
  (* a.k.a Hash or Map (combine with Tuple to get Key/value pair) *)
  (* TODO? merge with Record *)
  | Dict
  (* Tuples usually contain at least 2 elements, except for Python where
   * you can actually have 1-uple, e.g., '(1,)'.
   *)
  | Tuple

(* For Python/HCL (and Haskell later). The 'expr' is a 'Tuple' to
 * represent a Key/Value pair (like in Container). See keyval() below.
 * newscope:
 *)
and comprehension = expr * for_or_if_comp list

(* at least one element *)
and for_or_if_comp =
  (* newvar: *)
  | CompFor of tok (*'for'*) * pattern * tok (* 'in' *) * expr
  | CompIf of tok (*'if'*) * expr

(* It's useful to keep track in the AST of all those special identifiers.
 * They need to be handled in a special way by certain analysis and just
 * using Name for them would be error-prone.
 * Note though that by putting all of them together in a type, we lose
 * typing information. For example, Eval takes only one argument and
 * InstanceOf takes a type and an expr. This is a tradeoff to also not
 * polluate too much expr with too many constructs.
 * TODO: split in IdSpecial of special_id and CallSpecial of special_op
 * and then also just CallOp. And also a separate InterpolatedString.
 *)
and special =
  (* special vars *)
  | This
  | Super (* called 'base' in C# *)
  (* less: how different self/parent is from this/super? *)
  | Self
  | Parent
  (* for Lua, todo: just remove it, create Dict without key *)
  | NextArrayIndex
  (* special calls *)
  | Eval
  | Typeof (* for C? and Go in switch x.(type) *)
  | Instanceof
  | Sizeof (* takes a ArgType *)
  | Defined (* defined? in Ruby, other? *)
  (* Note that certain languages do not have a 'new' keyword
   * (e.g., Python, Scala 3), instead certain 'Call' are really 'New'.
   * Note that 'new' by itself is not a valid expression
   *)
  | New (* usually associated with Call(New, [ArgType _;...]) *)
  (* used for interpolated strings constructs
   * TODO: move out of 'special' and make special construct InterpolatedConcat
   * in 'expr' instead of abusing Call for that? that way can also
   * avoid those InterpolatedElement stuff.
   *)
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
   * but this is a bit of a hack.
   * TODO: We should probably add InterpolatedConcat as an expression
   *)
  | InterpolatedElement
  (* "Inline" the content of a var containing a list (a.k.a Splat in Ruby).
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
  (* used for unary and binary operations
   * TODO: move out of special too, in separate OpCall? (where can also
   * have 1 or 2 argument (or maybe even 0 for op reference?)
   *)
  | Op of operator
  (* less: should be lift up and transformed in Assign at stmt level *)
  | IncrDecr of (incr_decr * prefix_postfix)

(* mostly binary operators.
 * less: could be divided in really Arith vs Logical (bool) operators,
 * but see is_boolean_operator() helper below.
 * Note that Mod can be used for %style string formatting in Python.
 * Note that Plus can also be used for string concatenations in Go/??.
 * todo? use a Special operator intead for that? but need type info?
 *)
and operator =
  (* unary too *)
  | Plus
  (* unary too *)
  | Minus
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
  (* unary too *)
  | BitNot
  | BitClear (* Go *)
  (* And/Or are also shortcut operator.
   * todo? rewrite in CondExpr? They have a special behavior.
   *)
  | And
  | Or
  (* PHP has a xor shortcut operator ... hmmm *)
  | Xor
  (* unary *)
  | Not
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
  (* is: checks value has type *)
  | Is
  | NotIs
  (* Shell & and | *)
  | Background
  | Pipe

(* '++', '--' *)
and incr_decr = Incr | Decr

and prefix_postfix = Prefix | Postfix

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
   * example: foo`template ${id}`
   *)
  | TaggedTemplateLiteral

(* This is for JSX/TSX in javascript land (old: and XHP in PHP land).
 * less: we could make it more generic by adding a 'expr so it could be
 * reused in ast_js.ml, ast_php.ml
 *)
and xml = {
  xml_kind : xml_kind;
  xml_attrs : xml_attribute list;
  xml_body : xml_body list;
}

and xml_kind =
  | XmlClassic of tok (*'<'*) * ident * tok (*'>'*) * tok (*'</foo>'*)
  | XmlSingleton of tok (*'<'*) * ident * tok (* '/>', with xml_body = [] *)
  (* React/JS specific *)
  | XmlFragment of tok (* '<>' *) * (* '</>', with xml_attrs = [] *) tok

and xml_attribute =
  | XmlAttr of ident * tok (* = *) * a_xml_attr_value
  (* less: XmlAttrNoValue of ident. <foo a /> <=> <foo a=true /> *)
  (* jsx: usually a Spread operation, e.g., <foo {...bar} /> *)
  | XmlAttrExpr of expr bracket
  (* sgrep: *)
  | XmlEllipsis of tok

(* either a String or a bracketed expr, but right now we just use expr *)
and a_xml_attr_value = expr

and xml_body =
  (* sgrep-ext: can contain "..." *)
  | XmlText of string wrap
  (* this can be None when people abuse {} to put comments in it *)
  | XmlExpr of expr option bracket
  | XmlXml of xml

and arguments = argument list

and argument =
  (* regular argument *)
  | Arg of expr (* can be Call (IdSpecial Spread, Id foo) *)
  (* keyword argument *)
  | ArgKwd of ident * expr
  (* type argument for New, instanceof/sizeof/typeof, C macros *)
  | ArgType of type_
  | ArgOther of other_argument_operator * any list

and other_argument_operator =
  (* OCaml *)
  | OA_ArgQuestion
  (* Rust *)
  | OA_ArgMacro

(* todo: reduce, or move in other_special? *)
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
  | OE_Subshell
  (* Rust *)
  | OE_MacroInvocation
  (* C# *)
  | OE_Checked
  | OE_Unchecked
  (* Other *)
  | OE_StmtExpr (* OCaml/Ruby have just expressions, no statements *)
  | OE_Todo

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt = {
  s : stmt_kind;
      [@equal AST_utils.equal_stmt_field_s equal_stmt_kind] [@hash.ignore]
  (* this can be used to compare and hash more efficiently stmts,
   * or in semgrep to quickly know if a stmt is a children of another stmt.
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
  mutable s_bf : Bloom_filter.t option;
      [@equal fun _a _b -> true] [@hash.ignore]
  (* used to quickly get the range of a statement *)
  mutable s_range :
    (Parse_info.token_location * Parse_info.token_location) option;
      [@equal fun _a _b -> true] [@hash.ignore]
}

and stmt_kind =
  (* See also IL.ml where Call/Assign/Seq are not in expr and where there are
   * separate expr, instr, and stmt types *)
  | ExprStmt of expr * sc (* fake tok in Python, but also in JS/Go with ASI *)
  (* newscope: in C++/Java/Go *)
  | Block of stmt list bracket (* can be fake {} in Python where use layout *)
  (* EmptyStmt = Block [], or separate so can not be matched by $S? $
   * see also emptystmt() at the end of this file.
   *)
  (* newscope: for vardef in expr in C++/Go/... *)
  | If of tok (* 'if' or 'elif' *) * expr * stmt * stmt option
  | While of tok * expr * stmt
  | Return of tok * expr option * sc
  | DoWhile of tok * stmt * expr
  (* newscope: *)
  | For of tok (* 'for', 'foreach'*) * for_header * stmt
  (* The expr can be None for Go and Ruby.
   * less: could be merged with ExprStmt (MatchPattern ...) *)
  | Switch of
      tok (* 'switch' or also 'select' in Go *)
      * expr option
      * case_and_body list
  (* todo: merge with Switch.
   * In Scala and C# the match is infix (after the expr)
   *)
  | Match of tok * expr * action list
  | Continue of tok * label_ident * sc
  | Break of tok * label_ident * sc
  (* todo? remove stmt argument? more symetric to Goto *)
  | Label of label * stmt
  | Goto of tok * label
  (* TODO? move in expr! in C++ the expr can be an option *)
  | Throw of tok (* 'raise' in OCaml, 'throw' in Java/PHP *) * expr * sc
  | Try of tok * stmt * catch list * finally option
  | WithUsingResource of
      tok (* 'with' in Python, 'using' in C# *)
      * stmt (* resource acquisition *)
      * stmt (* newscope: block *)
  | Assert of tok * expr * expr option (* message *) * sc
  (* TODO? move this out of stmt and have a stmt_or_def_or_dir in Block?
   * or an item list where item is a stmt_or_def_or_dir (as well as field)
   *)
  | DefStmt of definition
  | DirectiveStmt of directive
  (* sgrep: *)
  | DisjStmt of stmt * stmt
  (* this is important to correctly compute a CFG *)
  | OtherStmtWithStmt of other_stmt_with_stmt_operator * expr option * stmt
  (* any here should not contain any statement! otherwise the CFG will be
   * incorrect and some analysis (e.g., liveness) will be incorrect.
   * TODO: other_stmt_operator wrap, so enforce at least one token instead
   * of relying that the any list contains at least one token
   *)
  | OtherStmt of other_stmt_operator * any list

(* newscope: *)
(* less: could merge even more with pattern
 * list = PatDisj and Default = PatUnderscore,
 * so case_and_body of Switch <=> action of MatchPattern
 *)
and case_and_body =
  | CasesAndBody of (case list * stmt)
  (* sgrep: *)
  | CaseEllipsis of (* ... *) tok

and case =
  | Case of tok * pattern
  | Default of tok
  (* For Go, expr can contain some Assign bindings.
   * todo? could merge with regular Case? can 'case x := <-chan' be
   * transformed in a pattern?
   *)
  | CaseEqualExpr of tok * expr

(* todo: merge with case at some point *)
(* newscope: newvar: *)
and action = pattern * expr

(* newvar: newscope: usually a PatVar *)
and catch = tok (* 'catch', 'except' in Python *) * pattern * stmt

(* newscope: *)
and finally = tok (* 'finally' *) * stmt

and label = ident

and label_ident =
  | LNone (* C/Python *)
  | LId of label (* Java/Go *)
  | LInt of int wrap (* PHP *)
  (* PHP, woohoo, dynamic break! bailout for CFG *)
  | LDynamic of expr

and for_header =
  (* todo? copy Go and have 'of simple option * expr * simple option'? *)
  | ForClassic of
      for_var_or_expr list (* init *) * expr option (* cond *) * expr option (* next *)
  (* newvar: *)
  | ForEach of
      pattern * tok (* 'in' Python, 'range' Go, 'as' PHP, '' Java *) * expr (* pattern 'in' expr *)
  (* Lua. todo: merge with ForEach? *)
  (* pattern 'in' expr *)
  | ForIn of for_var_or_expr list (* init *) * expr list
  (* sgrep: *)
  | ForEllipsis of (* ... *) tok

and for_var_or_expr =
  (* newvar: *)
  | ForInitVar of entity * variable_definition
  | ForInitExpr of expr

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

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)
(* This is quite similar to expr. A few constructs in expr have
 * equivalent here prefixed with Pat (e.g., PaLiteral, PatId). We could
 * maybe factorize with expr, and this may help sgrep, but I think it's
 * cleaner to have a separate type because the scoping rules for a pattern and
 * an expr are quite different and not any expr is allowed here.
 *)
and pattern =
  | PatLiteral of literal
  (* Or-Type, used also to match OCaml exceptions.
   * Used with Rust path expressions, with an empty pattern list.
   *)
  | PatConstructor of name * pattern list (* TODO: bracket also here *)
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
  (* sgrep: *)
  | PatEllipsis of tok
  | DisjPat of pattern * pattern
  | OtherPat of other_pattern_operator * any list

and other_pattern_operator =
  (* Other *)
  | OP_Expr (* todo: Python should transform via expr_to_pattern() below *)
  | OP_Todo

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
and type_ = {
  t : type_kind;
  (* used for C++ and Kotlin type qualifiers *)
  t_attrs : attribute list;
}

and type_kind =
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
  (* old: was originally TyApply (name, []), but better to differentiate.
   * todo? may need also TySpecial because the name can actually be
   *  self/parent/static (e.g., in PHP)
   *)
  | TyN of name
  (* covers list, hashtbl, etc.
   * note: the type_ should always be a TyN, so really it's a TyNameApply
   * but it's simpler to not repeat TyN to factorize code in semgrep regarding
   * aliasing.
   *)
  | TyApply of type_ * type_arguments
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
   * TODO: just generalize as TClass of name option * class_definition
   * for C++. Those are ugly because it would be cleaner to have
   * definitions only at the toplevel, but C/C++/Go allow those nested
   * class defs. We could lift them up at the top and introduce gensym'ed
   * classnames, but it's maybe better to stay close to the code.
   *)
  | TyRecordAnon of tok (* 'struct/shape', fake in other *) * field list bracket
  (* for Go *)
  | TyInterfaceAnon of tok (* 'interface' *) * field list bracket
  (* sgrep-ext: *)
  | TyEllipsis of tok
  | OtherType of other_type_operator * any list

(* <> in Java/C#/C++/Kotlin/Rust/..., [] in Scala and Go (for Map) *)
and type_arguments = type_argument list bracket

and type_argument =
  | TypeArg of type_
  (* Java only *)
  | TypeWildcard of
      tok (* '?' *) * (bool wrap (* extends|super, true=super *) * type_) option
  (* Rust *)
  | TypeLifetime of ident
  | OtherTypeArg of other_type_argument_operator * any list

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

and other_type_argument_operator =
  (* Rust *)
  | OTA_Literal
  | OTA_ConstBlock
  (* Other *)
  | OTA_Todo

(*****************************************************************************)
(* Attribute *)
(*****************************************************************************)
and attribute =
  (* a.k.a modifiers *)
  | KeywordAttr of keyword_attribute wrap
  (* a.k.a decorators, annotations *)
  | NamedAttr of tok (* @ *) * name * arguments bracket
  | OtherAttribute of other_attribute_operator * any list

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
  | Override
  (* for variables (JS) *)
  | Var
  | Let
  (* for fields (kinda types) *)
  | Mutable (* 'var' in Scala *)
  | Const (* 'readonly' in Typescript, 'val' in Scala *)
  (* less: should be part of the type? *)
  | Optional
  (* Typescript '?' *)
  | NotNull (* Typescript '!' *)
  (* for functions/methods *)
  | Recursive
  | MutuallyRecursive
  | Generator (* '*' in JS *)
  | Async
  | Inline
  (* for methods *)
  | Ctor
  | Dtor
  | Getter
  | Setter
  (* Rust *)
  | Unsafe
  | DefaultImpl (* Rust unstable, RFC 1210 *)
  (* Scala *)
  | Lazy (* By name application in Scala, via => T, in parameter *)
  | CaseClass

and other_attribute_operator =
  (* Java *)
  | OA_StrictFP
  | OA_Transient
  | OA_Synchronized
  | OA_Native
  | OA_Default
  | OA_AnnotThrow
  (* Other *)
  (* todo: used for Python, but should transform in NamedAttr when can *)
  | OA_Expr
  | OA_Todo

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
(* definition (or just declaration sometimes) *)
and definition = entity * definition_kind

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
and entity = {
  (* In Ruby you can define a class with a qualified name as in
   * class A::B::C, and even dynamically.
   * In C++ you can define a method with a class qualifier outside a class,
   * hence the use of name_or_dynamic below and not just ident.
   *)
  name : name_or_dynamic;
  attrs : attribute list;
  tparams : type_parameter list;
}

and definition_kind =
  (* newvar: can be used also for methods or nested functions.
   * note: can have an empty body when the def is actually a declaration
   * in a header file (called a prototype in C).
   *)
  | FuncDef of function_definition
  (* newvar: can be used also for constants.
   * note: can contain special_multivardef_pattern!! ident in which case vinit
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
  | TypeDef of type_definition
  | ModuleDef of module_definition
  | MacroDef of macro_definition
  (* in a header file (e.g., .mli in OCaml or 'module sig') *)
  | Signature of type_
  (* Only used inside a function.
   * Needed for languages without local VarDef (e.g., Python/PHP)
   * where the first use is also its declaration. In that case when we
   * want to access a global we need to disambiguate with creating a new
   * local.
   *)
  | UseOuterDecl of tok (* 'global' or 'nonlocal' in Python, 'use' in PHP *)
  | OtherDef of other_def_operator * any list

and other_def_operator = OD_Todo

(* template/generics/polymorphic-type *)
and type_parameter = ident * type_parameter_constraint list

and type_parameter_constraint =
  | Extends of type_
  | HasConstructor of tok
  | OtherTypeParam of other_type_parameter_operator * any list

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
(* We could merge this type with variable_definition, and use a
 * Lambda for vinit, but it feels better to use a separate type.
 * TODO? add ctor initializer here instead of storing them in fbody?
 *)
and function_definition = {
  fkind : function_kind wrap;
  fparams : parameters;
  (* return type *)
  frettype : type_ option;
  (* newscope: *)
  fbody : function_body;
}

(* We don't really care about the function_kind in semgrep, but who
 * knows, maybe one day we will. We care about the token in the
 * function_kind wrap in fkind though for semgrep for accurate range.
 *)
and function_kind =
  | Function
  (* This is a bit redundant with having the func in a field *)
  | Method
  (* Also redundant; can just check if the fdef is in a Lambda *)
  | LambdaKind
  (* a.k.a short lambdas *)
  | Arrow
  (* for Scala *)
  | BlockCases

and parameters = parameter list

(* newvar: *)
and parameter =
  | ParamClassic of parameter_classic
  | ParamPattern of pattern (* in OCaml, but also now JS, and Python2 *)
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
  (* sgrep: ... in parameters
   * note: foo(...x) of Js/Go is using the ParamRest, not this *)
  | ParamEllipsis of tok
  | OtherParam of other_parameter_operator * any list

(* less: could be merged with variable_definition, or pattern
 * less: could factorize pname/pattrs/pinfo with entity
 *)
and parameter_classic = {
  (* alt: use a 'ParamNoIdent of type_' when pname is None instead? *)
  pname : ident option;
  ptype : type_ option;
  pdefault : expr option;
  pattrs : attribute list;
  (* naming *)
  pinfo : id_info; (* Always Param *)
}

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

(* old: this used to be just an alias for 'stmt'; we were using
 * fake empty Block for FBDecl of fake ExprStmt for FBExpr.
 * However, some semgreo users may not like to treat a FBStmt
 * pattern to match an FBExpr, hence the more explicit cases.
 *)
and function_body =
  (* usually just a Block (where the brackets are fake in Ruby/Python/...) *)
  | FBStmt of stmt
  (* used for short lambdas in JS/Python, or regular func in OCaml/... *)
  | FBExpr of expr
  (* C/C++ prototypes or interface method declarations in Go/Java/... *)
  | FBDecl of sc
  (* Partial *)
  | FBNothing

(* ------------------------------------------------------------------------- *)
(* Variable definition *)
(* ------------------------------------------------------------------------- *)
(* Also used for constant_definition with attrs = [Const].
 * Also used for field definition in a class (and record).
 * We could use it for function_definition with vinit = Some (Lambda (...))
 * but maybe useful to explicitely makes the difference for now.
 *)
and variable_definition = {
  (* todo? should remove vinit and transform a VarDef with init with a VarDef
   * followed by an Assign (possibly to Null). See vardef_to_assign().
   *)
  vinit : expr option;
  (* less: (tok * expr) option? *)
  vtype : type_ option;
}

(* ------------------------------------------------------------------------- *)
(* Type definition *)
(* ------------------------------------------------------------------------- *)
and type_definition = { tbody : type_definition_kind }

and type_definition_kind =
  | OrType of or_type_element list (* enum/ADTs *)
  (* Record definitions (for struct/class, see class_definition).
   * The fields will be defined via a DefStmt (VarDef variable_definition)
   * where the field.vtype should be defined.
   *)
  | AndType of field list bracket
  (* a.k.a typedef in C (and alias type in Go) *)
  | AliasType of type_
  (* Haskell/Hack/Go ('type x foo' vs 'type x = foo' in Go) *)
  | NewType of type_
  | Exception of ident (* same name than entity *) * type_ list
  | OtherTypeKind of other_type_kind_operator * any list

and or_type_element =
  (* OCaml *)
  | OrConstructor of ident * type_ list
  (* C *)
  | OrEnum of ident * expr option
  (* Java? *)
  | OrUnion of ident * type_
  | OtherOr of other_or_type_element_operator * any list

and other_or_type_element_operator =
  (* Java, Kotlin *)
  | OOTEO_EnumWithMethods
  | OOTEO_EnumWithArguments

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
 * However ideally 'field' should really be just an alias for 'definition'.
 *)
and field =
  | FieldStmt of stmt
  (* DEBT? could abuse FieldStmt(ExprStmt(IdSpecial(Spread))) for that? *)
  | FieldSpread of tok (* ... *) * expr

and other_type_kind_operator = (* OCaml *)
  | OTKO_AbstractType | OTKO_Todo

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)
(* less: could be a special kind of type_definition *)
and class_definition = {
  ckind : class_kind wrap;
  (* cextends contains usually just one parent, and type_ should be TyApply *)
  (* TODO? the parent can have arguments, as in Scala, to call super
   * or when used inside a New.
   *)
  cextends : type_ list;
  (* the class_kind in type_ must be Interface *)
  cimplements : type_ list;
  (* the class_kind in type_ are usually Trait *)
  (* PHP 'uses' *)
  cmixins : type_ list;
  (* for Java Record or Scala Classes (we could transpile them into fields) *)
  cparams : parameters;
  (* newscope:
   * note: this can be an empty fake bracket when used in Partial.
   * TODO? use an option here?
   *)
  cbody : field list bracket;
}

(* invariant: this must remain a simple enum; Map_AST relies on it *)
and class_kind =
  | Class
  | Interface
  | Trait
  (* Kotlin, Scala *)
  | Object
  (* Java 'record', Scala 'case class' *)
  | RecordClass
  (* Java @interface, a.k.a annotation type declaration *)
  | AtInterface

(* ------------------------------------------------------------------------- *)
(* Module definition  *)
(* ------------------------------------------------------------------------- *)
and module_definition = { mbody : module_definition_kind }

and module_definition_kind =
  (* note that those could be converted also in ImportAs *)
  | ModuleAlias of dotted_ident
  (* newscope: *)
  | ModuleStruct of dotted_ident option * item list
  | OtherModule of other_module_operator * any list

and other_module_operator =
  (* OCaml (functors and their applications) *)
  | OMO_Todo

(* ------------------------------------------------------------------------- *)
(* Macro definition *)
(* ------------------------------------------------------------------------- *)
(* Used by cpp in C/C++ *)
and macro_definition = { macroparams : ident list; macrobody : any list }

(*****************************************************************************)
(* Directives (Module import/export, package) *)
(*****************************************************************************)
and directive = {
  d : directive_kind;
  (* Right now d_attrs is used just for Static import in Java, and for
   * OCaml attributes of directives (e.g., open).
   *)
  d_attrs : attribute list;
}

(* It is tempting to simplify all those ImportXxx in a simpler
 * 'Import of dotted_ident * ...', but module_name is not always a DottedName
 * so it is better to clearly separate what is module_name/namespace from an
 * entity (in this module/namespace) even though some languages such as Python
 * blur the difference.
 *)
and directive_kind =
  (* newvar: *)
  | ImportFrom of
      tok (* 'import'/'from' for Python, 'include' for C *)
      * module_name
      * ident
      * alias option (* as name alias *)
  | ImportAs of tok * module_name * alias option (* as name *)
  (* bad practice! hard to resolve name locally *)
  | ImportAll of tok * module_name * tok (* '.' in Go, '*' in Java/Python, '_' in Scala *)
  (* packages are different from modules in that multiple files can reuse
   * the same package name; they are agglomerated in the same package
   *)
  | Package of tok * dotted_ident (* a.k.a namespace *)
  (* This is used for languages such as C++/PHP/Scala with scoped namespaces.
   * alt: Package of tok * dotted_ident * item list bracket, but less
   * consistent with other directives, so better to use PackageEnd.
   *)
  | PackageEnd of tok
  | Pragma of ident * any list
  | OtherDirective of other_directive_operator * any list

(* xxx as name *)
and alias = ident * id_info

and other_directive_operator =
  (* Javascript *)
  | OI_Export
  | OI_ReExportNamespace
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
 * This simplifies semgrep too.
 * DEBT? merge with field too?
 *)
and item = stmt

and program = item list

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
  | PartialMatch of tok * expr
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
and any =
  | E of expr
  | S of stmt
  | Ss of stmt list
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
  | Tk of tok
  | TodoK of todo_kind
  | Ar of argument
  (* todo: get rid of some? *)
  | Modn of module_name
  | ModDk of module_definition_kind
  | En of entity
  | Pa of parameter
  | Dk of definition_kind
  | Di of dotted_ident
  | Lbli of label_ident
  | NoD of name_or_dynamic
  (* Used only for Rust macro arguments for now *)
  | Anys of any list
[@@deriving show { with_path = false }, eq, hash]

(*****************************************************************************)
(* Special constants *)
(*****************************************************************************)

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

(*****************************************************************************)
(* Error *)
(*****************************************************************************)

(* This can be used in the xxx_to_generic.ml file to signal limitations.
 * This is captured in Main.exn_to_error to pinpoint the error location.
 * alt: reuse Parse_info.Ast_builder_error exn.
 *)
exception Error of string * Parse_info.t

let error tok msg = raise (Error (msg, tok))

(*****************************************************************************)
(* Fake tokens *)
(*****************************************************************************)

(* Try avoid using them! if you build new constructs, you should try
 * to derive the tokens in those new constructs from existing constructs
 * and use the Parse_info.fake_info variant, not the unsafe_xxx one.
 *)
let fake s = Parse_info.unsafe_fake_info s

let fake_bracket x = (fake "(", x, fake ")")

(* bugfix: I used to put ";" but now Parse_info.str_of_info prints
 * the string of a fake info
 *)
let sc = Parse_info.unsafe_fake_info ""

(*****************************************************************************)
(* AST builder helpers *)
(*****************************************************************************)
(* see also AST_generic_helpers.ml *)

(* ------------------------------------------------------------------------- *)
(* Shortcuts *)
(* ------------------------------------------------------------------------- *)

(* statements *)
let s skind =
  {
    s = skind;
    s_id = AST_utils.Node_ID.create ();
    s_use_cache = false;
    s_backrefs = None;
    s_bf = None;
    s_range = None;
  }

(* expressions *)
let e ekind = { e = ekind; e_id = 0; e_range = None }

(* directives *)
let d dkind = { d = dkind; d_attrs = [] }

(* types *)
let t tkind = { t = tkind; t_attrs = [] }

(* patterns *)
(* less: nothing yet, but at some point we may want to use a record
 * also for patterns *)
let p x = x

(* ------------------------------------------------------------------------- *)
(* Ident and names *)
(* ------------------------------------------------------------------------- *)

(* before Naming_AST.resolve can do its job *)
let sid_TODO = -1

let empty_name_info = { name_qualifier = None; name_typeargs = None }

let empty_var = { vinit = None; vtype = None }

let empty_id_info () =
  { id_resolved = ref None; id_type = ref None; id_constness = ref None }

let basic_id_info resolved =
  {
    id_resolved = ref (Some resolved);
    id_type = ref None;
    id_constness = ref None;
  }

(* TODO: move AST_generic_helpers.name_of_id and ids here *)

(* ------------------------------------------------------------------------- *)
(* Entities *)
(* ------------------------------------------------------------------------- *)

let basic_entity id attrs =
  let idinfo = empty_id_info () in
  { name = EN (Id (id, idinfo)); attrs; tparams = [] }

(* ------------------------------------------------------------------------- *)
(* Arguments *)
(* ------------------------------------------------------------------------- *)

(* easier to use in List.map than each time (fun e -> Arg e) *)
let arg e = Arg e

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)
let special spec es =
  Call (IdSpecial spec |> e, fake_bracket (es |> List.map arg)) |> e

let opcall (op, t) es = special (Op op, t) es

(* TODO: have a separate InterpolatedConcat in expr with a cleaner type
 * instead of abusing special?
 *)
let interpolated (lquote, xs, rquote) =
  let special = IdSpecial (ConcatString InterpolatedConcat, lquote) |> e in
  Call
    ( special,
      ( lquote,
        xs
        |> List.map (function
             | Common.Left3 str -> Arg (L (String str) |> e)
             | Common.Right3 (lbrace, eopt, rbrace) ->
                 let special = IdSpecial (InterpolatedElement, lbrace) |> e in
                 let args = eopt |> Common.opt_to_list |> List.map arg in
                 Arg (Call (special, (lbrace, args, rbrace)) |> e)
             | Common.Middle3 e -> Arg e),
        rquote ) )
  |> e

(* todo? use a special construct KeyVal valid only inside Dict? *)
let keyval k _tarrow v = Container (Tuple, fake_bracket [ k; v ]) |> e

(* ------------------------------------------------------------------------- *)
(* Parameters *)
(* ------------------------------------------------------------------------- *)

let param_of_id id =
  {
    pname = Some id;
    pdefault = None;
    ptype = None;
    pattrs = [];
    pinfo = basic_id_info (Param, sid_TODO);
  }

let param_of_type typ =
  {
    ptype = Some typ;
    pname = None;
    pdefault = None;
    pattrs = [];
    pinfo = empty_id_info ();
  }

(* ------------------------------------------------------------------------- *)
(* Statements *)
(* ------------------------------------------------------------------------- *)

let exprstmt e = s (ExprStmt (e, sc))

(* alt: EmptyStmt of sc? of ExprStmt of expr option * sc *)
let emptystmt t = s (Block (t, [], t))

(* The dual of exprstmt.
 * This is mostly used for languages where the division
 * between stmt and expr is fuzzy or nonexistent (e.g., OCaml, Scala)
 * and where things like While, Match are expressions, but in the
 * generic AST they are statements.
 * See also AST_generic_helpers with expr_to_pattern, expr_to_type,
 * pattern_to_expr, etc.
 *)
let stmt_to_expr st = e (OtherExpr (OE_StmtExpr, [ S st ]))

let empty_body = fake_bracket []

let stmt1 xs =
  match xs with
  | [] -> s (Block (fake_bracket []))
  | [ st ] -> st
  | xs -> s (Block (fake_bracket xs))

(* ------------------------------------------------------------------------- *)
(* Fields *)
(* ------------------------------------------------------------------------- *)

(* this should be simpler at some point if we get rid of FieldStmt *)
let fld (ent, def) = FieldStmt (s (DefStmt (ent, def)))

let basic_field id vopt typeopt =
  let entity = basic_entity id [] in
  fld (entity, VarDef { vinit = vopt; vtype = typeopt })

let fieldEllipsis t = FieldStmt (exprstmt (e (Ellipsis t)))

(* ------------------------------------------------------------------------- *)
(* Attributes *)
(* ------------------------------------------------------------------------- *)

let attr kwd tok = KeywordAttr (kwd, tok)

let unhandled_keywordattr (s, t) =
  NamedAttr (t, Id ((s, t), empty_id_info ()), fake_bracket [])

(*****************************************************************************)
(* AST accessors *)
(*****************************************************************************)

let unbracket (_, x, _) = x

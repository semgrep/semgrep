(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2011-2015 Tomohiro Matsuyama
 * Copyright (C) 2019-2022 r2c
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
(* Abstract Syntax Tree for Python3 (with a few extensions to handle Python2).
 *
 * Most of the code in this file derives from code from
 * Tomohiro Matsuyama in ocaml-pythonlib, which itself derives from
 * the official grammar definition of Python.
 *
 * reference: http://docs.python.org/3/library/AST.html
 *
 * See also:
 *  - http://trevorjim.com/python-is-not-context-free/
 *  - https://github.com/gvanrossum/pegen a WIP to write the Python grammar
 *    using a PEG parser
 *
 * Note that this AST supports partly Python2 syntax with the special
 * print and exec statements. It supports also the special tuple
 * parameters syntax though. See the python2: tag below.
 *
 * related work:
 *  - https://github.com/m2ym/ocaml-pythonlib
 *    The original code. The repo was also forked by jeremy buisson
 *    who added a very basic simplifier but remains mostly the same.
 *  - Pyre-check
 *    typechecker and taint-tracker for Python, written in OCaml from facebook
 *  - https://github.com/mattgreen/hython
 *    Python3 interpreter written in Haskell
 *  - libCST (a concrete syntax tree, better for program transformation)
 *    by Instagram
 *
 * history:
 *  - 2019 port to the pfff infrastructure.
 *  - 2019 modified to support types, and many other Python 3 features
 *    (see the python3: tag in this file)
 *  - 2020 backport print and exec statements, to parse some python2 code.
 *  - 2022 incorporate constructs needed for tree-sitter-python
 *
 * todo:
 *  - for tree-sitter-python: async
 * less:
 *  - could use records for all the XxxDef, but what matters now is
 *    AST_generic.ml, which uses records at least.
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type tok = Tok.t [@@deriving show]

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * tok [@@deriving show] (* with tarzan *)

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok [@@deriving show] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

(* usually called 'ident' in our other ASTs, but the Python grammar
 * uses this term instead. *)
type name = string wrap [@@deriving show] (* with tarzan *)

(* note that name can be also the special "*" in an import context.
 * TODO: encode in a proper way. Let Python_to_generic.ml transpiles
 * that to a common representation.
 *)
type dotted_name = name list [@@deriving show] (* with tarzan *)

(* TODO? encode also __future__ which is used in tree-sitter-python? *)
type module_name =
  dotted_name
  * (* https://realpython.com/absolute-vs-relative-python-imports/ *)
  tok (* . or ... toks *) list option (* levels, for relative imports *)
[@@deriving show]

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

type literal =
  | Num of number (* n *)
  (* TODO: cleanup strings, have prefix in string wrap, then
   * content is string wrap bracket where bracket are enclosing
   * quote/double-quote/triple-quotes
   *)
  | Str of string wrap
  | Bool of bool wrap
  | None_ of tok

and expr =
  | Literal of literal
  (* s *)
  (* TODO bracket *)
  (* TODO: we should split the token in r'foo' in two, one string wrap
   * for the prefix and a string wrap for the string itself. *)
  | EncodedStr of string wrap * string (* prefix *)
  (* Introduce new vars when expr_context = Store.
   * Note that the ident can be "self".
   * alt: we could use an IdSpecial for it but self is actually not a
   * Python keyword; you can use a different name for it.
   *)
  | Name of name (* id *) * expr_context (* ctx *)
  (* TODO: in some context the tuple does not have the enclosing brackets
   * (in which case they are represented by fake tokens)
   *)
  | Tuple of expr list_or_comprehension * expr_context
  | List of expr list_or_comprehension * expr_context
  (* todo? split in two, with Set of expr list_or_comprehension *)
  | DictOrSet of dictorset_elt list_or_comprehension
  (* python3: TODO of tok *)
  | ExprStar of expr (* less: expr_context? always Store anyway no? *)
  (* python3: f-strings
   * reference: https://www.python.org/dev/peps/pep-0498/ *)
  | InterpolatedString of tok * interpolated list * tok
  | ConcatenatedString of interpolated list (* always Str *)
  | BoolOp of boolop wrap (* op *) * expr list (* values *)
  | BinOp of expr (* left *) * operator wrap (* op *) * expr (* right *)
  | UnaryOp of unaryop wrap (* op *) * expr (* operand *)
  | Compare of
      expr (* left *) * cmpop wrap list (* ops *) * expr list (* comparators *)
  (* note that Python does not have a 'new' keyword, a call with the name
   * of a class is a New *)
  | Call of expr (* func *) * argument list bracket (* args *)
  | Subscript of
      expr (* value *) * slice list bracket (* slice *) * expr_context
  (* the parameters do not have types here *)
  | Lambda of
      tok (* lambda *) * parameters (* args *) * tok (* : *) * expr (* body *)
  | IfExp of expr (* test *) * expr (* body *) * expr (* orelse *)
  (* TODO: in tree-sitter-python look more like
   * YieldFrom of tok * tok * expr, or Yield of tok * expr list
   *)
  | Yield of tok * expr option (* value *) * bool (* is_yield_from *)
  (* python3: *)
  | Await of tok * expr
  (* python 3.8+; see https://www.python.org/dev/peps/pep-0572/ *)
  | NamedExpr of expr * tok * expr
  | Repr of expr bracket (* `` *)
  (* =~ ObjAccess *)
  | Attribute of
      expr (* value *) * tok (* . *) * name (* attr *) * expr_context (* ctx *)
  (* type-only:
     this should not show up in ordinary exprs, but we share the same type
     for both exprs and types right now
  *)
  | ConstrainedType of type_ * tok * expr
  (* sgrep-ext: typing-ext: *)
  | Ellipsis of tok (* should be only in .pyi, types Dict[str,...], or sgrep *)
  (* sgrep-ext: *)
  | DeepEllipsis of expr bracket
  | TypedMetavar of name * tok * type_
  | DotAccessEllipsis of expr * tok (* ... *)
  | ParenExpr of expr bracket

and number =
  | Int of Parsed_int.t
  (* TODO: merge with Int? tree-sitter-python does not differentiate *)
  | LongInt of Parsed_int.t
  | Float of float option wrap
  | Imag of string wrap

(* less: could reuse AST_generic.arithmetic_operator *)
and boolop = And | Or

(* the % operator can also be used for strings! "foo %s" % name *)
and operator =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Pow
  | FloorDiv
  | LShift
  | RShift
  | BitOr
  | BitXor
  | BitAnd
  | MatMult (* Matrix Multiplication *)

and unaryop = Invert | Not | UAdd | USub
and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn

(* usually a Str or a simple expr.
 * TODO: should also handle format specifier, they are skipped for now
 * during parsing.
 * TODO: do like in AST_generic.ml, represent correctly interpolated
 * constructs.
 *)
and interpolated = expr

and 'a list_or_comprehension =
  | CompList of 'a list bracket
  | CompForIf of 'a comprehension bracket

(* tree-sitter-python: imposes the first for_if to be a CompFor *)
and 'a comprehension = 'a * for_if list

(* TODO: CompFor can have an Async *)
and for_if =
  | CompFor of expr (* introduce new vars *) * (* in *) expr
  | CompIf of expr

and dictorset_elt =
  | KeyVal of expr * (* TODO of tok ':' *) expr
  | Key of expr
  (* python3: TODO of tok '**', and merge with ArgPow? *)
  | PowInline of expr

(* AugLoad and AugStore are not used.
 * TODO: get rid of? Anyway it's not used in Python_to_generic.ml
 *)
and expr_context = Load | Store | Del | AugLoad | AugStore | Param

and slice =
  | Slice of
      expr option (* lower *) * expr option (* upper *) * expr option (* step *)
  | Index of expr (* value *)

(* ------------------------------------------------------------------------- *)
(* Arguments *)
(* ------------------------------------------------------------------------- *)
and argument =
  | Arg of expr (* this can be Ellipsis for sgrep *)
  (* tree-sitter-python: (and Python2) allows any expression for the key, but
   * the official Python 2 grammar says "ast.c makes sure it's a NAME" *)
  | ArgKwd of name (* arg *) * expr (* value *)
  (* TODO? just use ExprStar, and move PowInline in expr too? and just
   * say in which context those constructs can actually appear
   * (e.g., only in arg, or dict/set)
   *)
  | ArgStar of (* '*' *) tok * expr
  | ArgPow of (* '**' *) tok * expr
  (* TODO: merge with Tuple CompForIf, and actually there can be only 1
   * ArgComp in arguments *)
  | ArgComp of expr * for_if list

(* ------------------------------------------------------------------------- *)
(* Parameters (used for Lambda above and function_definition below) *)
(* ------------------------------------------------------------------------- *)
(* TODO: add bracket *)
and parameters = parameter list

and parameter =
  (* param_pattern is usually just a name.
   * TODO? merge with ParamDefault
   *)
  | ParamPattern of param_pattern * type_ option
  | ParamDefault of (param_pattern * type_ option) * expr (* default value *)
  (* TODO: tree-sitter-python allows also a Subscript or Attribute instead
   * of just name, what is that?? *)
  | ParamStar of tok (* '*' *) * (name * type_ option)
  | ParamPow of tok (* '**' *) * (name * type_ option)
  (* python3: single star delimiter to force keyword-only arguments after.
   * reference: https://www.python.org/dev/peps/pep-3102/ *)
  | ParamSingleStar of tok
  (* python3: single slash delimiter to force positional-only arg prior. *)
  | ParamSlash of tok
  (* sgrep-ext: *)
  | ParamEllipsis of tok

and param_pattern =
  | PatternName of name
  (* python2: this is only valid in python2 *)
  | PatternTuple of param_pattern list

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
(* python3: type annotations!
 * see https://docs.python.org/3/library/typing.html for the semantic
 * and https://www.python.org/dev/peps/pep-3107/ (function annotations)
 * for https://www.python.org/dev/peps/pep-0526/ (variable annotations)
 * for its syntax.
 *)
and type_ = expr

(* This is exposed for `semgrep-proprietary`, which uses this to parse
   type hover information from the Language Server Index Format (https://lsif.dev/).
   Python types do not natively have arrows in them, so we must expose this type to
   get that data out.
*)
and lsif_type = Type of type_ | Arrow of parameters * type_

(* used in inheritance, to allow default value for metaclass *)
and type_parent = argument

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)
(* Name, Tuple (CompList), List( CompList), Attribute, Subscript,
 * or ExprStar(Name|Attribute|Subscript), or more? *)
(* with tarzan *)
and pattern =
  | PatName of name
  | PatInterpolatedString of tok * interpolated list * tok
  | PatConcatenatedString of interpolated list (* always Str *)
  | PatAttribute of pattern * tok * name
  | PatConstructor of dotted_name * pattern list bracket
  | PatSplat of tok * pattern
  | PatDisj of pattern list
  | PatList of pattern list bracket
  | PatTuple of pattern list bracket
  | PatDict of pattern list bracket
  | PatLiteral of literal
  | PatAs of pattern * (* as *) tok * name
  | PatUnderscore of tok
  | PatComplex of tok option * number * tok * number
  | PatKeyVal of pattern * (* = *) tok * pattern
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
type stmt =
  | ExprStmt of expr (* value *)
  (* The left exprs should be lvalues: Name, List, Tuple, Subscript,
   * or Attribute, or ExprStar, which are anything with an expr_context
   * (see also Parser_python.set_expr_ctx).
   * They can also introduce new vars (which we should transform
   * in VarDef in Python_to_generic.ml).
   * Why take an expr list? because those exprs are all really lhs.
   * For example in 'a = b = c', we will have 'Assign ([a;b], c)'.
   * TODO: lhs should be expr * type_ option
   *)
  | Assign of
      (expr * (tok * type_) option) (*lhs*) list (* targets *)
      * tok
      * expr (* value *)
  | AugAssign of expr (* target *) * operator wrap (* op *) * expr (* value *)
  | Cast of expr * tok * type_
  | For of
      tok
      * expr (* (pattern) introduce new vars *)
      * tok
      * expr (* 'in' iter *)
      * stmt list (* body *)
      * stmt list (* orelse *)
  | While of
      tok * expr (* test *) * stmt list (* body *) * stmt list (* orelse *)
  | If of
      tok
      * expr (* test *)
      * stmt list (* body *)
      * stmt list option (* orelse *)
  (* https://docs.python.org/2.5/whatsnew/pep-343.html *)
  | With of tok * with_clause * stmt list (* body *)
  | Switch of tok * expr * case_and_body list
  | Return of tok * expr option (* value *)
  | Break of tok
  | Continue of tok
  | Pass of tok
  | Raise of tok * (expr * expr option (* from *)) option
  | RaisePython2 of
      tok * expr * expr option (* arguments *) * expr option (* location *)
  (* TODO: tree-sitter-python allow a finally also in TryExcept => merge? *)
  | TryExcept of
      tok
      * stmt list (* body *)
      * excepthandler list (* handlers *)
      * (tok * stmt list) option (* orelse *)
      * (tok * stmt list) option (* finally *)
  (* TODO: tree-sitter-python say expr list *)
  | Assert of tok * expr (* test *) * expr option (* msg *)
  (* 'Global' is needed because Python does not have a VarDef and abuse Assign
   * to declare new variables, which in turn requires Global and NonLocal
   * below to explicitely say you don't want Assign to declare a new var
   * but instead use an enclosing one (argh, I love Python).
   *)
  | Global of tok * name list (* names *)
  | Delete of tok * expr list (* targets *)
  (* python3: *)
  | NonLocal of tok * name list (* names *)
  (* python2: *)
  | Print of
      tok * expr option (* dest *) * expr list (* values *) * bool (* nl *)
  (* TODO: tree-sitter-python has Exec of tok * string wrap * expr list option
   * why? Python2 compatibility?
   *)
  | Exec of
      tok * expr (* body *) * expr option (* glob *) * expr option (* local *)
  (* python3: for With, For, and FunctionDef *)
  | Async of tok * stmt
  | ImportAs of tok * module_name (* name *) * name option (* asname *)
  | ImportAll of tok * module_name * tok (* * *)
  | ImportFrom of tok * module_name (* module *) * alias list (* names *)
  (* should be allowed just at the toplevel *)
  | FunctionDef of function_definition
  | ClassDef of class_definition
  | TypeAliasDef of tok * expr * expr

and case_and_body =
  | CasesAndBody of case list * stmt list
  (* sgrep-ext: *)
  | CaseEllipsis of (* ... *) tok

and case = Case of tok * pattern

and excepthandler =
  | ExceptHandler of
      tok
      * expr option (* type, possibly a list of types as in (Error,Fatal) *)
      * name option (* name, introduce new var, todo: only if pattern is Some *)
      * stmt list (* body *)

and with_clause = expr (* context_expr *) * expr option (* optional_vars *)

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
(* Note that there are no "Variable definition" section below.
 * Variables in Python are defined by assignment (ugly).
 *)

(* ------------------------------------------------------------------------- *)
(* Decorators (a.k.a annotations) *)
(* ------------------------------------------------------------------------- *)
and decorator = tok (* @ *) * expr

(* ------------------------------------------------------------------------- *)
(* Function (or method) definition *)
(* ------------------------------------------------------------------------- *)
(* less: use a record *)
and function_definition =
  tok (* 'def' *)
  * name (* name *)
  * parameters (* args *)
  * type_ option
  * (* return type *)
  stmt list (* body *)
  * decorator list (* decorator_list *)

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)
(* less: use a record *)
and class_definition =
  tok (* 'class' *)
  * name (* name *)
  * type_parent list (* bases *)
  * stmt list (* body *)
  * decorator list (* decorator_list *)

(*****************************************************************************)
(* Module *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Module import/export *)
(* ------------------------------------------------------------------------- *)
and alias = name (* name *) * name option (* asname *)
[@@deriving show { with_path = false }]
(* with tarzan *)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
type program = stmt list [@@deriving show] (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
(* This is mostly for semgrep to represent a pattern *)
type any =
  | Expr of expr
  | Stmt of stmt
  | Stmts of stmt list
  | Decorator of decorator
  | Program of program
  | DictElem of dictorset_elt
[@@deriving show { with_path = false }]
(* with tarzan *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let str_of_name = fst

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let context_of_expr = function
  | Attribute (_, _, _, ctx) -> Some ctx
  | Subscript (_, _, ctx) -> Some ctx
  | Name (_, ctx) -> Some ctx
  | List (_, ctx) -> Some ctx
  | Tuple (_, ctx) -> Some ctx
  | _ -> None

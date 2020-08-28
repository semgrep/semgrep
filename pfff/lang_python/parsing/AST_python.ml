(*s: pfff/lang_python/parsing/AST_python.ml *)
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2011-2015 Tomohiro Matsuyama
 * Copyright (C) 2019 r2c
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
(* Abstract Syntax Tree for Python3.
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
 * print and exec statements. It does not support the special tuple
 * parameters syntax though.
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
 *
 * todo:
 *  - could use records for all the XxxDef, but what matters now is 
 *    AST_generic.ml, which uses records at least.
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

(*s: type [[AST_python.tok]] *)
(* Contains among other things the position of the token through
 * the Parse_info.token_location embedded inside it, as well as the
 * transformation field that makes possible spatch on the code.
 *)
type tok = Parse_info.t
(*e: type [[AST_python.tok]] *)
 [@@deriving show]
(*s: type [[AST_python.wrap]] *)
(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * tok
(*e: type [[AST_python.wrap]] *)
 [@@deriving show] (* with tarzan *)
(*s: type [[AST_python.bracket]] *)
(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok
(*e: type [[AST_python.bracket]] *)
 [@@deriving show] (* with tarzan *)
(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)
(*s: type [[AST_python.name]] *)
type name = string wrap
(*e: type [[AST_python.name]] *)
 [@@deriving show] (* with tarzan *)

(*s: type [[AST_python.dotted_name]] *)
(* note that name can be also the special "*" in an import context. *)
type dotted_name = name list
(*e: type [[AST_python.dotted_name]] *)
 [@@deriving show] (* with tarzan *)

(*s: type [[AST_python.module_name]] *)
type module_name = 
 dotted_name * 
 (* https://realpython.com/absolute-vs-relative-python-imports/ *)
 (tok (* . or ... toks *) list) option (* levels, for relative imports *)
(*e: type [[AST_python.module_name]] *)
 [@@deriving show]

(*s: type [[AST_python.resolved_name]] *)
(* TODO: reuse AST_generic one? *)
type resolved_name =
  (* this can be computed by a visitor *)
  | LocalVar
  | Parameter
  | GlobalVar
  | ClassField
  (* both dotted_name should contain at least one element! *)
  | ImportedModule of dotted_name
  | ImportedEntity of dotted_name

  (* default case *)
  | NotResolved
(*e: type [[AST_python.resolved_name]] *)
 [@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
(*s: type [[AST_python.expr]] *)
type expr =
  | Num of number (* n *)
  | Str of string wrap (* s *)
  | EncodedStr of string wrap * string (* prefix *)
  (* python3: now officially reserved keywords *)
  | Bool of bool wrap
  | None_ of tok

  (* introduce new vars when expr_context = Store *)
  | Name of name (* id *) * expr_context (* ctx *) * resolved_name ref

  | Tuple of expr list_or_comprehension * expr_context
  | List  of expr list_or_comprehension * expr_context
  | DictOrSet of dictorset_elt list_or_comprehension

  (* python3: *)
  | ExprStar of expr (* less: expr_context? always Store anyway no? *)
  (* python3: f-strings
   * reference: https://www.python.org/dev/peps/pep-0498/ *)
  | InterpolatedString of interpolated list
  | ConcatenatedString of interpolated list (* always Str *)

  (* python3: *)
  (* inside an Assign (or ExprStmt) *)
  | TypedExpr of expr * type_
  (* sgrep-ext: *)
  | Ellipsis of tok (* should be only in .pyi, types Dict[str,...], or sgrep *)
  | DeepEllipsis of expr bracket
  | TypedMetavar of name * tok * type_

  | BoolOp of boolop wrap (* op *) * expr list (* values *)
  | BinOp of expr (* left *) * operator wrap (* op *) * expr (* right *)
  | UnaryOp of unaryop wrap (* op *) * expr (* operand *)
  | Compare of expr (* left *) * cmpop wrap list (* ops *) * expr list (* comparators *)

  (* note that Python does not have a 'new' keyword, a call with the name
   * of a class is a New *)
  | Call of expr (* func *) * argument list bracket (* args *)

  | Subscript of expr (* value *) * slice list (* slice *) * expr_context

  (* the parameters do not have types here *)
  | Lambda of parameters (* args *) * expr (* body *)

  | IfExp of expr (* test *) * expr (* body *) * expr (* orelse *)

  | Yield of tok * expr option (* value *) * bool (* is_yield_from *)
  (* python3: *)
  | Await of tok * expr

  (* python 3.8+; see https://www.python.org/dev/peps/pep-0572/ *)
  | NamedExpr of expr * tok * expr
  | Repr of expr bracket (* `` *)
  (* =~ ObjAccess *)
  | Attribute of expr (* value *) * tok (* . *) * name (* attr *) * 
       expr_context (* ctx *)
(*e: type [[AST_python.expr]] *)

(*s: type [[AST_python.number]] *)
  and number =
    | Int of string wrap
    | LongInt of string wrap
    | Float of string wrap
    | Imag of string wrap
(*e: type [[AST_python.number]] *)

  (* less: could reuse AST_generic.arithmetic_operator *)
(*s: type [[AST_python.boolop]] *)
  and boolop = And | Or
(*e: type [[AST_python.boolop]] *)

  (* the % operator can also be used for strings! "foo %s" % name *)  
(*s: type [[AST_python.operator]] *)
  and operator = 
    | Add | Sub | Mult | Div 
    | Mod | Pow | FloorDiv
    | LShift | RShift 
    | BitOr | BitXor | BitAnd 
    | MatMult (* Matrix Multiplication *)
(*e: type [[AST_python.operator]] *)
  
(*s: type [[AST_python.unaryop]] *)
  and unaryop = Invert | Not | UAdd | USub
(*e: type [[AST_python.unaryop]] *)
  
(*s: type [[AST_python.cmpop]] *)
  and cmpop = 
    | Eq | NotEq 
    | Lt | LtE | Gt | GtE 
    | Is | IsNot 
    | In | NotIn
(*e: type [[AST_python.cmpop]] *)
  
 (* usually a Str or a simple expr.
  * TODO: should also handle format specifier, they are skipped for now
  * during parsing
  *)
(*s: type [[AST_python.interpolated]] *)
  and interpolated = expr
(*e: type [[AST_python.interpolated]] *)

(*s: type [[AST_python.list_or_comprehension]] *)
  and 'a list_or_comprehension = 
    | CompList of 'a list bracket
    | CompForIf of 'a comprehension
(*e: type [[AST_python.list_or_comprehension]] *)

(*s: type [[AST_python.comprehension]] *)
    and 'a comprehension = 'a * for_if list
(*e: type [[AST_python.comprehension]] *)
(*s: type [[AST_python.for_if]] *)
      and for_if =
      | CompFor of expr (* introduce new vars *) * (* in *) expr
      | CompIf of expr
(*e: type [[AST_python.for_if]] *)
  
(*s: type [[AST_python.dictorset_elt]] *)
  and dictorset_elt = 
    | KeyVal of expr * expr
    | Key of expr
    (* python3: *)
    | PowInline of expr
(*e: type [[AST_python.dictorset_elt]] *)
  
  (* AugLoad and AugStore are not used *)
(*s: type [[AST_python.expr_context]] *)
  and expr_context = 
    | Load | Store 
    | Del 
    | AugLoad | AugStore
    | Param
(*e: type [[AST_python.expr_context]] *)
  
(*s: type [[AST_python.slice]] *)
  and slice =
    | Slice of expr option (* lower *) * expr option (* upper *) * expr option (* step *)
    | Index of expr (* value *)
(*e: type [[AST_python.slice]] *)
  
(*s: type [[AST_python.parameters]] *)
  and parameters = parameter list
(*e: type [[AST_python.parameters]] *)
(*s: type [[AST_python.parameter]] *)
   and parameter = 
      (* the first expr can be only a Name or a Tuple (pattern?),
       * and the Name can have a type associated with it
       *)
     | ParamDefault of (name * type_ option) * expr (* default value *)
     (* pattern can be either a name or a tuple pattern *)
     | ParamPattern of param_pattern * type_ option
     | ParamStar of (name * type_ option)
     (* python3: single star delimiter to force keyword-only arguments after.
      * reference: https://www.python.org/dev/peps/pep-3102/ *)
     | ParamSingleStar of tok
     (* python3: single slash delimiter to force positional-only arguments prior. *)
     | ParamSlash of tok
     | ParamPow  of (name * type_ option)
     (* sgrep-ext: *)
     | ParamEllipsis of tok
(*e: type [[AST_python.parameter]] *)
  
(*s: type [[AST_python.argument]] *)
  and argument = 
    | Arg of expr (* this can be Ellipsis for sgrep *)
    | ArgKwd of name (* arg *) * expr (* value *)
    | ArgStar of expr
    | ArgPow of expr
    | ArgComp of expr * for_if list
(*e: type [[AST_python.argument]] *)
 
  
(*****************************************************************************)
(* Type *)
(*****************************************************************************)
(* python3: type annotations!
 * see https://docs.python.org/3/library/typing.html for the semantic
 * and https://www.python.org/dev/peps/pep-3107/ (function annotations)
 * for https://www.python.org/dev/peps/pep-0526/ (variable annotations)
 * for its syntax.
 *)
(*s: type [[AST_python.type_]] *)
and type_ = expr
(*e: type [[AST_python.type_]] *)

(* used in inheritance, to allow default value for metaclass *)
(*s: type [[AST_python.type_parent]] *)
and type_parent = argument
(*e: type [[AST_python.type_parent]] *)

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)
(*s: type [[AST_python.pattern]] *)
(* Name, or Tuple? or more? *)
and pattern = expr
(*e: type [[AST_python.pattern]] *)

(* python2? *)
and param_pattern =
  | PatternName of name
  | PatternTuple of param_pattern list
 [@@deriving show { with_path = false }]  (* with tarzan *)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
(*s: type [[AST_python.stmt]] *)
type stmt =
  | ExprStmt of expr (* value *)

  (* the left expr should be an lvalue: Name, List, Tuple, Subscript,
   * or Attribute, or ExprStar, which are anything with an expr_context
   * (see also Parser_python.set_expr_ctx).
   * This can introduce new vars.
   * TODO: why take an expr list? can reuse Tuple for tuple assignment
   *)
  | Assign of expr list (* targets *) * tok * expr (* value *)
  | AugAssign of expr (* target *) * operator wrap (* op *) * expr (* value *)

  | For of tok * pattern (* (pattern) introduce new vars *) * 
           tok * expr (* 'in' iter *) * 
           stmt list (* body *) * stmt list (* orelse *)
  | While of tok * expr (* test *) * stmt list (* body *) * 
             stmt list (* orelse *)
  | If of tok * expr (* test *) * stmt list (* body *) * 
             stmt list option (* orelse *)
  (* https://docs.python.org/2.5/whatsnew/pep-343.html *)
  | With of tok * expr (* context_expr *) * expr option (* optional_vars *) * 
      stmt list (* body *)

  | Return of tok * expr option (* value *)
  | Break of tok | Continue of tok
  | Pass of tok

  | Raise of tok * (expr * expr option (* from *)) option
  | RaisePython2 of tok * expr * expr option (* arguments *) * expr option (* location *)
  | TryExcept of tok * stmt list (* body *) * excepthandler list (* handlers *)
           * stmt list (* orelse *)
  | TryFinally of tok * stmt list (* body *) * tok * stmt list (* finalbody *)
  | Assert of tok * expr (* test *) * expr option (* msg *)

  | Global of tok * name list (* names *)
  | Delete of tok * expr list (* targets *)
  (* python3: *)
  | NonLocal of tok * name list (* names *)

  (* python2: *)
  | Print of tok * expr option (* dest *) * expr list (* values *) * bool (* nl *)
  | Exec of tok * expr (* body *) * expr option (* glob *) * expr option (* local *)

  (* python3: for With, For, and FunctionDef *)
  | Async of tok * stmt

  | ImportAs   of tok * module_name (* name *) * name option (* asname *)
  | ImportAll  of tok * module_name * tok (* * *)
  | ImportFrom of tok * module_name (* module *) * alias list (* names *)

  (* should be allowed just at the toplevel *)
  | FunctionDef of 
       name (* name *) * 
       parameters (* args *) * 
       type_ option * (* return type *)
       stmt list (* body *) * 
       decorator list (* decorator_list *)

  | ClassDef of 
        tok *
        name (* name *) * 
        type_parent list (* bases *) * 
        stmt list (* body *) * 
        decorator list (* decorator_list *)
(*e: type [[AST_python.stmt]] *)


(*s: type [[AST_python.excepthandler]] *)
and excepthandler = 
  ExceptHandler of 
    tok *
    expr option (* type, possibly a list of types as in (Error,Fatal) *) * 
    name option (* name, introduce new var, todo: only if pattern is Some *) * 
    stmt list (* body *)
(*e: type [[AST_python.excepthandler]] *)

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Decorators (a.k.a annotations) *)
(* ------------------------------------------------------------------------- *)
(*s: type [[AST_python.decorator]] *)
and decorator = tok (* @ *) * dotted_name * argument list bracket option
(*e: type [[AST_python.decorator]] *)

(* ------------------------------------------------------------------------- *)
(* Function (or method) definition *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Variable definition *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* Module *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Module import/export *)
(* ------------------------------------------------------------------------- *)
(*s: type [[AST_python.alias]] *)
and alias = name (* name *) * name option (* asname *)
(*e: type [[AST_python.alias]] *)
  [@@deriving show { with_path = false }]  (* with tarzan *)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
(*s: type [[AST_python.program]] *)
type program = stmt list
(*e: type [[AST_python.program]] *)
  [@@deriving show]   (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
(*s: type [[AST_python.any]] *)
type any =
  | Expr of expr
  | Stmt of stmt
  | Stmts of stmt list
  | Program of program

  | DictElem of dictorset_elt
(*e: type [[AST_python.any]] *)
  [@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
(*s: constant [[AST_python.str_of_name]] *)
let str_of_name = fst
(*e: constant [[AST_python.str_of_name]] *)

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)
(*s: function [[AST_python.context_of_expr]] *)
let context_of_expr = function
  | Attribute (_, _, _, ctx) -> Some ctx
  | Subscript (_, _, ctx) -> Some ctx
  | Name (_, ctx, _)   -> Some ctx
  | List (_, ctx)         -> Some ctx
  | Tuple (_, ctx)        -> Some ctx
  | _                     -> None
(*e: function [[AST_python.context_of_expr]] *)
(*e: pfff/lang_python/parsing/AST_python.ml *)

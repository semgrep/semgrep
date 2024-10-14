(* Julien Verlaguet, Yoann Padioleau
 *
 * Copyright (C) 2011-2013 Facebook
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
(* A (real) Abstract Syntax Tree for PHP, not a Concrete Syntax Tree
 * as in cst_php.ml.
 *
 * This file contains a simplified PHP abstract syntax tree. The original
 * PHP syntax tree (cst_php.ml) is good for code refactoring or
 * code visualization; the types used matches exactly the source. However,
 * for other algorithms, the nature of the AST makes the code a bit
 * redundant. Hence the idea of a SimpleAST which is the
 * original AST where certain constructions have been factorized
 * or even removed.
 *
 * Here is a list of the simplications/factorizations:
 *  - no purely syntactical tokens in the AST like parenthesis, brackets,
 *    braces, angles, commas, semicolons, antislash, etc. No ParenExpr.
 *    No FinalDef. No NotParsedCorrectly. The only token information kept
 *    is for identifiers for error reporting. See wrap() below.
 *
 *  - support for old syntax is removed. No IfColon, ColonStmt,
 *    CaseColonList.
 *  - support for extra tools is removed. No XdebugXxx
 *    update: but support for semgrep is restored (Ellipsis)
 *  - support for features we don't really use in our code is removed
 *    e.g. unset cast. No Use, UseDirect, UseParen. No CastUnset.
 *    Also no StaticObjCallVar.
 *  - some known directives like 'declare(ticks=1);' or 'declare(strict=1);'
 *    are skipped because they don't have a useful semantic for
 *    the abstract interpreter or the type inference engine. No Declare.
 *
 *  - sugar is removed, no ArrayLong vs ArrayShort, no InlineHtml,
 *    no HereDoc, no EncapsXxx, no XhpSingleton (but kept Xhp), no
 *    implicit fields via constructor parameters.
 *  - some builtins, for instance 'echo', are transformed in "__builtin__echo".
 *    See builtin() below.
 *  - no include/require, they are transformed in call
 *    to __builtin__require (maybe not a good idea)
 *  - some special keywords, for instance 'self', are transformed in
 *    "__special__self". See special() below.
 *    The comment is still relevant but we should use a different example than self.
 *  - the different ways to define namespaces are merged, no
 *    NamespaceBracketDef.
 *
 *  - a simpler stmt type; no extra toplevel and stmt_and_def types,
 *    no FuncDefNested, no ClassDefNested. No StmtList.
 *  - a simpler expr type; no lvalue vs expr vs static_scalar vs attribute
 *    (update: now static_scalar = expr = lvalue also in cst_php.ml).
 *    Also no scalar. No Sc, no C. No Lv. Pattern matching constants
 *    is simpler:  | Sc (C (String ...)) -> ... becomes just | String -> ....
 *    Also no arg type. No Arg, ArgRef, ArgUnpack. Also no xhp_attr_value type.
 *    No XhpAttrString, XhpAttrExpr.
 *  - no EmptyStmt, it is transformed in an empty Block
 *  - a simpler If. 'elseif' are transformed in nested If, and empty 'else'
 *    in an empty Block.
 *  - a simpler Foreach, foreach_var_either and foreach_arrow are transformed
 *    into expressions with a new Arrow constructor (maybe not good idea)

 *  - some special constructs like AssignRef were transformed into
 *    composite calls to Assign and Ref. Same for AssignList, AssignNew.
 *    Same for arguments passed by reference, no Arg, ArgRef, ArgUnpack.
 *    Same for refs in arrays, no ArrayRef, ArrayArrowRef. Also no ListVar,
 *    ListList, ListEmpty. No ForeachVar, ForeachList.
 *    Array value are also decomposed in regular expr or Arrow, no
 *    ArrayArrowExpr, no ForeachArrow. More orthogonal.

 *  - a unified Call. No FunCallSimple, FunCallVar, MethodCallSimple,
 *    StaticMethodCallSimple, StaticMethodCallVar
 *    (update: same in cst_php.ml now)
 *  - a unified Array_get. No VArrayAccess, VArrayAccessXhp,
 *    VBraceAccess, OArrayAccess, OBraceAccess
 *    (update: same in cst_php.ml now)
 *  - unified Class_get and Obj_get instead of lots of duplication in
 *    many constructors, e.g. no ClassConstant in a separate scalar type,
 *    no retarded obj_prop_access/obj_dim types,
 *    no OName, CName, ObjProp, ObjPropVar, ObjAccessSimple vs ObjAccess,
 *    no ClassNameRefDynamic, no VQualifier, ClassVar, DynamicClassVar,
 *    etc.
 *    (update: same in cst_php.ml now)
 *  - unified eval_var, some constructs were transformed into calls to
 *    "eval_var" builtin, e.g. no GlobalDollar, no VBrace, no Indirect/Deref.
 *
 *  - a simpler 'name' for identifiers, xhp names and regular names are merged,
 *    the special keyword self/parent/static are merged,
 *    so the complex Id (XName [QI (Name "foo")]) becomes just Id ["foo"].
 *  - ...
 *
 * todo:
 *  - put back types! at least the basic one like f_return_type
 *    with no generics
 *  - less: factorize more? string vs Guil?
 *)

(*****************************************************************************)
(* Token (leaves) *)
(*****************************************************************************)

type tok = Tok.t [@@deriving show]
type 'a wrap = 'a * tok [@@deriving show]

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok [@@deriving show]
type ident = string wrap [@@deriving show]

(* the string contains the $ prefix *)
type var = string wrap [@@deriving show]

(* The keyword 'namespace' can be in a leading position. The special
 * ident 'ROOT' can also be leading.
 *)
type qualified_ident = ident list [@@deriving show]
type name = qualified_ident [@@deriving show]

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* lvalue and expr have been mixed in this AST, but an lvalue should be
 * an expr restricted to: Var $var, Array_get, Obj_get, Class_get, or List.
 *)
type expr =
  (* booleans are really just Int in PHP :( *)
  (* I don't think ^ is true. It reads like a boolean represents a truth
     value, where for purposes of conversion 0 is cast to false and
     non-0 is cast to true *)
  (* https://www.php.net/manual/en/language.types.boolean.php *)
  | Bool of bool wrap
  | Int of Parsed_int.t
  | Double of float option wrap
  (* PHP has no first-class functions so entities are sometimes passed
   * as strings so the string wrap below can actually correspond to a
   * 'Id name' sometimes. Some magic functions like param_post() also
   * introduce entities (variables) via strings.
   *)
  | String of string wrap (* TODO: bracket *)
  (* Id is valid for "entities" (functions, classes, constants). Id is also
   * used for class methods/fields/constants. It can also contain
   * "self/parent" or "static", "class". It can be "true", "false", "null"
   * and many other builtin constants. See builtin() and special() below.
   *
   * todo: For field name, if in the code they are referenced like $this->fld,
   * we should prepend a $ to fld to match their definition.
   *)
  | Id of name (* less: should be renamed Name *)
  | IdSpecial of special wrap
  (* Var used to be merged with Id. But then we were doing lots of
   * 'when Ast.is_variable name' so maybe better to have Id and Var
   * (at the same time OCaml does not differentiate Id from Var).
   * The string contains the '$'.
   *)
  | Var of var
  (* when None it means add to the end when used in lvalue position *)
  | Array_get of expr * expr option bracket
  (* Unified method/field access.
   * ex: $o->foo() ==> Call(Obj_get(Var "$o", Id "foo"), [])
   * ex: A::foo()  ==> Call(Class_get(Id "A", Id "foo"), [])
   * note that Id can be "self", "parent", "static".
   *)
  | Obj_get of expr * tok * expr
  | Class_get of expr * tok * expr
  | New of tok * expr * argument list
  | NewAnonClass of tok * argument list * class_def
  | InstanceOf of tok * expr * expr
  (* pad: could perhaps be at the statement level? The left expr
   * must be an lvalue (e.g. a variable).
   *)
  | Assign of expr * tok * expr
  | AssignOp of expr * binaryOp wrap * expr
  (* really a destructuring tuple let; always used as part of an Assign or
   * in foreach_pattern.
   *)
  | List of expr list bracket
  (* used only inside array_value or foreach_pattern, or for yield
   * (which is translated as a builtin and so a Call)
   *)
  | Arrow of expr * tok * expr
  (* $y =& $x is transformed into an Assign(Var "$y", Ref (Var "$x")). In
   * PHP refs are always used in an Assign context.
   *)
  | Ref of tok * expr
  (* e.g. f(...$x) *)
  | Unpack of expr
  | Call of expr * argument list bracket
  | Throw of tok * expr
  (* todo? transform into Call (builtin ...) ? *)
  | Infix of AST_generic.incr_decr wrap * expr
  | Postfix of AST_generic.incr_decr wrap * expr
  | Binop of expr * binaryOp wrap * expr
  | Unop of unaryOp wrap * expr
  | Guil of expr list bracket
  | ConsArray of array_value list bracket
  | CondExpr of expr * expr * expr
  | Cast of cast_type wrap * expr
  (* yeah! PHP 5.3 is becoming a real language *)
  | Lambda of func_def
  | Match of tok * expr * match_ list
  (* sgrep-ext: *)
  | Ellipsis of tok
  | DeepEllipsis of expr bracket

and match_ = MCase of expr list * expr | MDefault of tok * expr

and cast_type =
  | BoolTy
  | IntTy
  | DoubleTy (* float *)
  | StringTy
  | ArrayTy
  | ObjectTy

and special =
  (* often transformed in Var "$this" in the analysis *)
  | This
  (* represents the "self" keyword expression in a classes  *)
  | Self
  (* represents the "parent" keyword expression in a class *)
  | Parent
  | FuncLike of funclike

(* language constructs that look like functions *)
and funclike = Empty | Eval | Exit | Isset | Unset

and binaryOp =
  (* TODO: now available in AST_generic_ ? *)
  | BinaryConcat
  | CombinedComparison
  | ArithOp of AST_generic.operator

and unaryOp = AST_generic.operator

and argument =
  | Arg of expr
  | ArgRef of tok * expr
  | ArgUnpack of tok * expr
  | ArgLabel of ident * tok * expr

(* only Var, List, or Arrow, and apparently also Array_get is ok, so
 * basically any lvalue
 *)
and foreach_pattern = expr

(* often an Arrow *)
and array_value = expr

(* string_const_expr is for shape field names which are permitted to be either
 * literal strings or class constants. *)
and string_const_expr = expr

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
and hint_type =
  | Hint of name (* todo: add the generics *)
  | HintArray of tok
  | HintQuestion of tok * hint_type
  | HintTuple of hint_type list bracket
  | HintCallback of hint_type list * hint_type option
  | HintTypeConst of hint_type * tok * hint_type (* ?? *)
  | HintVariadic of tok * hint_type option

and class_name = hint_type

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt =
  | Expr of expr * tok
  | Block of stmt list bracket
  | If of tok * expr * stmt * stmt
  | Switch of tok * expr * case list
  | While of tok * expr * stmt
  | Do of tok * stmt * expr
  | For of tok * expr list * expr list * expr list * stmt
  (* 'foreach ($xs as $k)','... ($xs as $k => $v)', '... ($xs as list($...))'*)
  | Foreach of tok * expr * tok * foreach_pattern * stmt
  | Return of tok * expr option
  | Break of tok * expr option
  | Continue of tok * expr option
  | Label of ident * tok (* : *) * stmt
  | Goto of tok * ident
  | Try of tok * stmt * catch list * finally list
  (* only at toplevel in most of our code *)
  | ClassDef of class_def
  | FuncDef of func_def
  (* only at toplevel *)
  | ConstantDef of constant_def
  | TypeDef of type_def
  (* the qualified_ident below can not have a leading '\', it can also
   * be the root namespace *)
  | NamespaceDef of tok * qualified_ident * stmt list bracket
  | NamespaceUse of tok * qualified_ident * ident option (* when alias *)
  (* Note that there is no LocalVars constructor. Variables in PHP are
   * declared when they are first assigned. *)
  | StaticVars of tok * (var * expr option) list
  (* expr is most of the time a simple variable name *)
  | Global of tok * expr list

and case = Case of tok * expr * stmt list | Default of tok * stmt list

(* catch(Exception $exn) { ... } => ("Exception", "$exn", [...])
 * TODO: can now be a list of hint_type, Exn1 | Exn2 like in Java.
 *)
and catch = tok * hint_type * var * stmt
and finally = tok * stmt

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
(* TODO: factorize xx_name in an entity type like in AST_generic.ml,
 * which also lead to a cleaner Lambda and NewAnonClass.
 * TODO: factorize also the xx_modifiers and xx_attrs?
 *)

(* The func_def type below is actually used both for functions and methods.
 *
 * For methods, a few names are specials:
 *  - __construct, __destruct
 *  - __call, __callStatic
 *)
and func_def = {
  (* TODO: "_lambda" when used for lambda, see also AnonLambda for f_kind below *)
  f_name : ident;
  f_kind : function_kind wrap;
  f_params : parameter list; (* TODO bracket *)
  f_return_type : hint_type option;
  (* functions returning a ref are rare *)
  f_ref : bool;
  (* only for methods; always empty for functions *)
  m_modifiers : modifier list;
  (* only for AnonLambda (could also abuse parameter), not for ShortLambda *)
  l_uses : (bool (* is_ref *) * var) list;
  f_attrs : attribute list;
  f_body : stmt;
}

and function_kind =
  | Function
  | AnonLambda
  | ShortLambda (* they have different scoping rules for free variables *)
  | Method

and parameter =
  | ParamClassic of parameter_classic
  (* sgrep-ext: *)
  | ParamEllipsis of tok

and parameter_classic = {
  p_type : hint_type option;
  p_ref : tok option;
  p_name : var;
  p_default : expr option;
  p_attrs : attribute list;
  p_variadic : tok option;
}

(* for methods, and below for fields too *)
and modifier = keyword_modifier wrap

and keyword_modifier =
  | Public
  | Private
  | Protected
  | Abstract
  | Final
  | Static
  | Async

(* normally either an Id or Call with only static arguments *)
and attribute = expr

and constant_def = {
  cst_tok : tok;
  cst_name : ident;
  (* normally a static scalar *)
  cst_body : expr;
}

and enum_type = { e_base : hint_type; e_constraint : hint_type option }

and class_def = {
  c_name : ident;
  c_kind : class_kind wrap;
  c_extends : class_name option;
  c_implements : class_name list;
  c_uses : class_name list; (* traits *)
  (* If this class is an enum, what is the underlying type (and
   * constraint) of the enum? *)
  c_enum_type : enum_type option;
  c_modifiers : modifier list;
  c_attrs : attribute list;
  c_constants : constant_def list;
  c_variables : class_var list;
  c_methods : method_def list;
  c_braces : unit bracket;
}

and class_kind = Class | Interface | Trait | Enum
and xhp_field = class_var * bool

and class_var = {
  (* note that the name will contain a $ *)
  cv_name : var;
  cv_type : hint_type option;
  cv_value : expr option;
  cv_modifiers : modifier list;
}

and method_def = func_def
and type_def = { t_name : ident; t_kind : type_def_kind }
and type_def_kind = Alias of hint_type [@@deriving show { with_path = false }]

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

type program = stmt list [@@deriving show { with_path = false }]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type partial = PartialIf of tok * expr [@@deriving show { with_path = false }]

type any =
  | Program of program
  | Stmt of stmt
  | Expr2 of expr
  | Param of parameter
  | Partial of partial
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let unwrap x = fst x
let wrap_fake s = (s, Tok.fake_tok s)

(* TODO: replace builtin() by IdSpecial like I do in AST_generic.ml
 * builtin() is used for:
 *  - 'eval', and implicitly generated eval/reflection like functions:
 *     "eval_var" (e.g. for echo $$x, echo ${"x"."y"}),
 *  - 'clone',
 *  - 'exit', 'yield', 'yield_break' TODO 'yield_from?'
 *  - 'unset', 'isset', 'empty'
 *     http://php.net/manual/en/function.unset.php
 *     http://php.net/manual/en/function.empty.php
 *  - 'echo', 'print',
 *  - '@', '`',
 *  - 'include', 'require', 'include_once', 'require_once'.
 *  -  __LINE__/__FILE/__DIR/__CLASS/__TRAIT/__FUNCTION/__METHOD/
 *
 * See also data/php_stdlib/pfff.php which declares those builtins.
 * See also tests/php/semantic/ for example of uses of those builtins.
 *
 * coupling: if modify the string, git grep it because it's probably
 *  used in patterns too.
 *)
let builtin x = "__builtin__" ^ x

(* for 'self'/'parent', 'static', 'lambda', 'namespace', root namespace '\',
 * 'class' as in C::class
 * TODO: transform in IdSpecial!
 *)
let special x = "__special__" ^ x

(* AST helpers *)
let has_modifier cv = List.length cv.cv_modifiers > 0
let is_static modifiers = List.mem Static (List_.map unwrap modifiers)
let is_private modifiers = List.mem Private (List_.map unwrap modifiers)
let string_of_xhp_tag xs = ":" ^ String.concat ":" xs
let str_of_ident (s, _) = s
let tok_of_ident (_, x) = x

exception TodoNamespace of tok

let str_of_name = function
  | [ id ] -> str_of_ident id
  | [] -> raise Common.Impossible
  | x :: _xs -> raise (TodoNamespace (tok_of_ident x))

let tok_of_name = function
  | [ id ] -> tok_of_ident id
  | [] -> raise Common.Impossible
  (* pick first one *)
  | x :: _xs -> tok_of_ident x

(* we sometimes need to remove the '$' prefix *)
let remove_first_char s = String.sub s 1 (String.length s - 1)

let str_of_class_name x =
  match x with
  | Hint name -> str_of_name name
  | _ -> raise Common.Impossible

let name_of_class_name x =
  match x with
  | Hint [ name ] -> name
  | Hint [] -> raise Common.Impossible
  | Hint name -> raise (TodoNamespace (tok_of_name name))
  | _ -> raise Common.Impossible

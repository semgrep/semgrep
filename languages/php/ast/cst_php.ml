(* Yoann Padioleau
 *
 * Copyright (C) 2009-2013 Facebook
 * Copyright (C) 2020 R2C
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
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Concrete Syntax Tree (CST) for PHP.
 *
 * Currently, the CST supports mostly PHP 5.2 with
 * a few extensions from PHP 5.3 (e.g. closures, namespace, const) and
 * PHP 5.4 (e.g. traits) as well as support for many Facebook
 * extensions (XHP, generators, annotations, generics, collections,
 * type definitions, implicit fields via constructor parameters).
 *
 * Update: I removed many facebook extensions to simplify (e.g., XHP).
 * Moreover, this CST is mostly used for semgrep now, so we may
 * gradually transform it into ast_php.ml to avoid an extra intermediate
 * when converting from PHP to the generic AST.
 * For example, use Foo\Bar\{A, B} is not represented faithfully anymore
 * in this CST, and it's unsugared in use Foo\Bar\A and use Foo\Bar\B
 * (like in ast_php.ml).
 * The main requirement for this hybrid CST/AST
 * is to get the range of an expr/stmt/... correct so you must keep
 * the first and last token of an expr/stmt/... in the AST.
 *
 *
 * A CST is convenient in a refactoring context or code visualization
 * context, but if you need to do some heavy static analysis, consider
 * instead lang_php/analyze/foundation/pil.ml which defines a
 * PHP Intermediate Language a la CIL.
 *
 * todo:
 *  - unify toplevel statement vs statements? hmmm maybe not
 *  - maybe even in a refactoring context a PIL+comment
 *    (see pretty/ast_pp.ml) would make more sense.
 *
 * NOTE: data from this type are often marshalled in berkeley DB tables
 * which means that if you add a new constructor or field in the types below,
 * you must erase the berkeley DB databases otherwise pfff
 * will probably ends with a segfault (OCaml serialization is not
 * type-safe). A hacky solution is to add new constructors only at the end
 * of a type definition.
 *
 * COUPLING: some programs in other languages (e.g. Python) may
 * use some of the pfff binding, or json/sexp exporters, so if you
 * change the name of constructors in this file, don't forget
 * to regenerate the json/sexp exporters, but also to modify the
 * dependent programs !!!! An easier solution is to not change this
 * file, or to only add new constructors.
 *)

(*****************************************************************************)
(* Token (leaf) *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type tok = Tok.t

(* shortcuts to annotate some information with token/position information *)
and 'a wrap = 'a * tok
and 'a paren = tok * 'a * tok
and 'a brace = tok * 'a * tok
and 'a bracket = tok * 'a * tok
and 'a angle = tok * 'a * tok
and 'a single_angle = tok * 'a * tok
and 'a comma_list = ('a, tok (* the comma *)) Common.either list

and 'a comma_list_dots =
  ('a, tok (* ... in parameters *), tok (* the comma *)) Common.either3 list
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Ident/Name/LongName   *)
(* ------------------------------------------------------------------------- *)
(* See also analyze_php/namespace_php.ml *)

(* todo: was using XhpName but could simplify now that removed XHP *)
type ident = Name of string wrap
(* was called T_STRING in Zend, which are really just LABEL, see lexer.mll*)
[@@deriving show { with_path = false }]

(* The string does not contain the '$'. The info itself will usually
 * contain it, but not always! Indeed if the variable we build comes
 * from an encapsulated strings as in  echo "${x[foo]}" then the 'x'
 * will be parsed as a T_STRING_VARNAME, and eventually lead to a DName,
 * even if in the text it appears as a name.
 * So this token is kind of a FakeTok sometimes.
 *
 * If at some point you want to do some program transformation,
 * you may have to normalize this 'string wrap' before moving it
 * to another context !!!
 *)
type dname =
  | DName of string wrap
    (* D for dollar. Was called T_VARIABLE in the original PHP parser/lexer *)
[@@deriving show { with_path = false }]

(* The antislash is a separator but it can also be in the leading position.
 * The keyword 'namespace' can also be in a leading position.
 *)
type qualified_ident = qualified_ident_element list

and qualified_ident_element =
  | QI of ident (* the ident can be 'namespace' *)
  | QITok of tok (* '\' *)
[@@deriving show { with_path = false }]

type name =
  | XName of qualified_ident
  (* Could also transform at parsing time all occurences of self:: and
   * parent:: by their respective names. But I prefer to have all the
   * PHP features somehow explicitely represented in the AST.
   *)
  | Self of tok
  | Parent of tok
  (* php 5.3 late static binding (no idea why it's useful ...) *)
  | LateStatic of tok
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type hint_type =
  | Hint of name (* only self/parent, no static *) * type_args option
  | HintArray of tok
  | HintQuestion of (tok * hint_type)
  | HintTuple of hint_type comma_list paren
  | HintCallback of
      (tok (* "function" *)
      * hint_type comma_list_dots paren
      * (* params *)
      (tok * hint_type) option (* return type *))
      paren
  | HintTypeConst of hint_type (* lhs *) * tok (*'::'*) * hint_type (* rhs *)
  | HintVariadic of tok * hint_type option

and type_args = hint_type comma_list single_angle
and type_params = type_param comma_list single_angle

and type_param =
  | TParam of ident
  | TParamConstraint of ident * tok (* as *) * class_name

and class_name = hint_type

(* This is used in Cast. For type analysis see type_php.ml *)
and ptype =
  | BoolTy
  | IntTy
  | DoubleTy (* float *)
  | StringTy
  | ArrayTy
  | ObjectTy

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* I used to have a 'type expr = exprbis * exp_type_info' but it complicates
 * many patterns when working on expressions, and it turns out I never
 * implemented the type annotater. It's easier to do such an annotater on
 * a real AST like the PIL. So just have this file be a simple concrete
 * syntax tree and no more.
 *)
and expr =
  (* constant/function/class/method/field/class_cst name.
   *
   * Now that we've unified lvalue and expr, the use of Id is more
   * ambiguous; it can refer to a classname, a function name,
   * a constant, etc. You need to match the context of use of Id
   * to know in which situation you are (and take care if you use a visitor
   * to not always call recursively the visitor/continuation):
   *
   * - function: Call (Id, _)
   * - method: Call (ObjGet (_, Id), _),   Call (ClassGet (_, Id), _)
   * - class: ClassGet (Id, _), New (Id, _), AssignNew(Id,_, InstanceOf(_, Id)
   *   and also extends, implements, catch, type
   * - class_constant: ClassGet (_, Id) (including the special C::class)
   * - field: ObjGet(_, Id)
   * - constant: Id
   *
   * todo: just like we annotate IdVar with scope info, we could annotate
   * Id with a kind info.
   *)
  | Id of name
  (* less: maybe could unify.
   * note that IdVar is used not only for local variables
   * but also for globals, class variables, parameters, etc.
   *)
  | IdVar of dname
  | This of tok
  | Call of expr * arguments
  | ObjGet of expr * tok (* -> *) * expr
  | ClassGet of class_name_reference * tok (* :: *) * expr
  | ArrayGet of expr * expr option bracket
  | HashGet of expr * expr brace
  | BraceIdent of expr brace
  | Deref of tok (* $ *) * expr
  (* start of expr_without_variable in original PHP lexer/parser terminology *)
  | Sc of scalar
  | Binary of expr * binaryOp wrap * expr
  | Unary of unaryOp wrap * expr
  (* should be a statement ... *)
  | Assign of lvalue * tok (* = *) * expr
  | AssignOp of lvalue * assignOp wrap * expr
  | Postfix of rw_variable * fixOp wrap
  (* todo: should actually be called Prefix :) *)
  | Infix of fixOp wrap * rw_variable
  (* PHP 5.3 allows 'expr ?: expr' hence the 'option' type below
   * from www.php.net/manual/en/language.operators.comparison.php#language.operators.comparison.ternary:
   * "Since PHP 5.3, it is possible to leave out the middle part of the
   * ternary operator. Expression
   * expr1 ?: expr3 returns expr1 if expr1 evaluates to TRUE, and expr3
   * otherwise."
   *)
  | CondExpr of expr * tok (* ? *) * expr option * tok (* : *) * expr
  | AssignList of
      tok (* list *) * list_assign comma_list paren * tok (* = *) * expr
  | ArrayLong of tok (* array | shape *) * array_pair comma_list paren
  (* php 5.4: https://wiki.php.net/rfc/shortsyntaxforarrays *)
  | ArrayShort of array_pair comma_list bracket
  | New of tok * class_name_reference * arguments option
  | NewAnonClass of tok * arguments option * class_def (* c_name = "!ANON!" *)
  | Clone of tok * expr
  | AssignRef of lvalue * tok (* = *) * tok (* & *) * lvalue
  | AssignNew of
      lvalue
      * tok (* = *)
      * tok (* & *)
      * tok (* new *)
      * class_name_reference
      * arguments option
  | Cast of castOp wrap * expr
  | CastUnset of tok * expr (* ??? *)
  | InstanceOf of expr * tok * class_name_reference
  (* !The evil eval! *)
  | Eval of tok * expr paren
  (* Woohoo, viva PHP 5.3 *)
  | Lambda of lambda_def
  | ShortLambda of short_lambda_def
  (* should be a statement ... *)
  | Exit of tok * expr option paren option
  | At of tok (* @ *) * expr
  | Print of tok * expr
  | BackQuote of tok * encaps list * tok
  (* should be at toplevel *)
  | Include of tok * expr
  | IncludeOnce of tok * expr
  | Require of tok * expr
  | RequireOnce of tok * expr
  | Empty of tok * lvalue paren
  | Isset of tok * lvalue comma_list paren
  (* php-facebook-ext:
   *
   * todo: this should be at the statement level as there are only a few
   * forms of yield that HPHP supports (e.g. yield <expr>; and
   * <lval> = yield <expr>). One could then have a YieldReturn and YieldAssign
   * but this may change and none of the analysis in pfff need to
   * understand yield so for now just make it simple and add yield
   * at the expression level.
   *)
  | Yield of tok * array_pair (* should have no ref inside *)
  | YieldBreak of tok * tok
  (* php-facebook-ext: Just like yield, this should be at the statement level *)
  | Await of tok * expr
  (* semgrep-ext:  *)
  | Ellipsis of tok
  | DeepEllipsis of expr bracket
  (* unparser: *)
  | ParenExpr of expr paren

and scalar =
  | C of constant
  | Guil of
      tok (* double quote or b double quite' *)
      * encaps list
      * tok (* double quote *)
  | HereDoc of
      tok (* < < < EOF, or b < < < EOF *) * encaps list * tok (* EOF; *)

and constant =
  | Bool of bool wrap
  | Int of int option wrap (* decimal, hex, or binary int format *)
  | Double of float option wrap
  (* see also Guil for interpolated strings
   * The string does not contain the enclosing '"' or "'".
   * It does not contain either the possible 'b' prefix
   *)
  | String of string wrap
  | PreProcess of cpp_directive wrap
(* http://php.net/manual/en/language.constants.predefined.php *)

and cpp_directive =
  | Line
  | File
  | Dir
  | ClassC
  | TraitC
  | MethodC
  | FunctionC
  | NamespaceC

and encaps =
  | EncapsString of string wrap
  | EncapsVar of lvalue
  (* for "xx {$beer}s" *)
  | EncapsCurly of tok * lvalue * tok
  (* for "xx ${beer}s" *)
  | EncapsDollarCurly of tok (* '${' *) * lvalue * tok
  | EncapsExpr of tok * expr * tok

and fixOp = AST_generic.incr_decr

and binaryOp =
  | Arith of arithOp
  | Logical of logicalOp
  | BinaryConcat (* . *)
  | CombinedComparison

and arithOp =
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  | Pow
  | DecLeft
  | DecRight
  | And
  | Or
  | Xor

and logicalOp =
  | Inf
  | Sup
  | InfEq
  | SupEq
  | Eq
  | NotEq
  | Identical
  (* === *)
  | NotIdentical (* !== *)
  | AndLog
  | OrLog
  | XorLog
  | AndBool
  | OrBool (* diff with AndLog ? short-circuit operators ? *)

and assignOp = AssignOpArith of arithOp | AssignConcat (* .= *)
and unaryOp = UnPlus | UnMinus | UnBang | UnTilde
and castOp = ptype

(* less: merge with foreach_pattern, list($k => $v) = ... is allowed no? *)
and list_assign =
  | ListVar of lvalue
  | ListList of tok * list_assign comma_list paren
  | ListEmpty

and array_pair =
  | ArrayExpr of expr
  | ArrayRef of tok (* & *) * lvalue
  | ArrayArrowExpr of expr * tok (* => *) * expr
  | ArrayArrowRef of expr * tok (* => *) * tok (* & *) * lvalue

and argument =
  | Arg of expr
  | ArgRef of tok * w_variable
  | ArgUnpack of tok * expr
  (* named arguments, since PHP 8.0 *)
  | ArgLabel of ident * tok * expr

and arguments = argument comma_list paren

(* now unified with expr *)
and lvalue = expr
and class_name_reference = expr

(* semantic: those grammar rule names were used in the original PHP
 * lexer/parser but not enforced. It's just comments. *)
and rw_variable = lvalue
and r_variable = lvalue
and w_variable = lvalue

(* static_scalar used to be a special type allowing constants and
 * a restricted form of expressions. But it was yet
 * another type and it turned out it was making things like spatch
 * and visitors more complicated because stuff like "+ 1" could
 * be an expr or a static_scalar. We don't need this "isomorphism".
 * I never leveraged the specificities of static_scalar (maybe a compiler
 * would, but my checker/refactorers/... didn't).
 *
 * Note that it's not 'type static_scalar = scalar' because static_scalar
 * actually allows arrays (why the heck they called it a scalar then ...)
 * and plus/minus which are only in expr.
 *)
and static_scalar = expr

(* string_const_expr is for shape field names which are permitted to be either
 * literal strings or class constants. *)
and string_const_expr = expr

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)

(* By introducing Lambda, expr and stmt are now mutually recursive *)
and stmt =
  | ExprStmt of expr * tok (* ; *)
  | EmptyStmt of tok (* ; *)
  | Block of stmt_and_def list brace
  | If of
      tok
      * expr paren
      * stmt
      * (* elseif *)
      if_elseif list
      * (* else *)
      if_else option
  | IfColon of
      tok
      * expr paren
      * tok
      * stmt_and_def list
      * new_elseif list
      * new_else option
      * tok
      * tok
  (* if(cond):
   *   stmts; defs;
   * elseif(cond):
   *   stmts;..
   * else(cond):
   *   defs; stmst;
   * endif; *)
  | While of tok * expr paren * colon_stmt
  | Do of tok * stmt * tok * expr paren * tok
  | For of
      tok * tok * for_expr * tok * for_expr * tok * for_expr * tok * colon_stmt
  | Switch of tok * expr paren * switch_case_list
  (* if it's a expr_without_variable, the second arg must be a Right variable,
   * otherwise if it's a variable then it must be a foreach_variable
   *)
  | Foreach of
      tok
      * tok (*'('*)
      * expr
      * tok option (* await *)
      * tok (* as *)
      * foreach_pattern
      * tok (*')'*)
      * colon_stmt
  (* example: foreach(expr as $lvalue) { ... }
   *          foreach(expr as $foreach_varialbe => $lvalue) { ... }
   *          foreach(expr as list($x, $y)) { ... }
   *)
  | Break of tok * expr option * tok
  | Continue of tok * expr option * tok
  | Label of ident * tok (* : *) * stmt
  | Goto of tok * ident * tok (* ; *)
  | Return of tok * expr option * tok
  | Throw of tok * expr * tok
  | Try of tok * stmt_and_def list brace * catch list * finally list
  | Echo of tok * expr comma_list * tok
  | Globals of tok * global_var comma_list * tok
  | StaticVars of tok * static_var comma_list * tok
  | InlineHtml of string wrap
  | Use of tok * use_filename * tok
  | Unset of tok * lvalue comma_list paren * tok
  | Declare of tok * declare comma_list paren * colon_stmt
  (* nested funcs and classes are mostly used inside if() where the
   * if() actually behaves like an ifdef in C.
   *)
  (* was in stmt_and_def before *)
  | FuncDefNested of func_def
  (* traits are actually not allowed here *)
  | ClassDefNested of class_def

and switch_case_list =
  | CaseList of tok (* { *) * tok option (* ; *) * case list * tok (* } *)
  | CaseColonList of
      tok (* : *)
      * tok option (* ; *)
      * case list
      * tok (* endswitch *)
      * tok (* ; *)

and case =
  | Case of tok * expr * tok * stmt_and_def list
  | Default of tok * tok * stmt_and_def list

and if_elseif = tok * expr paren * stmt
and if_else = tok * stmt
and for_expr = expr comma_list (* can be empty *)

and foreach_pattern =
  | ForeachVar of foreach_variable
  | ForeachArrow of foreach_pattern * tok * foreach_pattern
  | ForeachList of tok (* list *) * list_assign comma_list paren

and foreach_variable = is_ref * lvalue
and catch = tok * (class_name * dname) paren * stmt_and_def list brace
and finally = tok * stmt_and_def list brace
and use_filename = UseDirect of string wrap | UseParen of string wrap paren
and declare = ident * static_scalar_affect

and colon_stmt =
  | SingleStmt of stmt
  | ColonStmt of
      tok (* : *) * stmt_and_def list * tok (* endxxx *) * tok (* ; *)

and new_elseif = tok * expr paren * tok * stmt_and_def list
and new_else = tok * tok * stmt_and_def list

(* stmt_and_def used to be a special type allowing Stmt or nested functions
 * or classes but it was introducing yet another, not so useful, intermediate
 * type.
 *)
and stmt_and_def = stmt

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
(* todo: split in entity * def_kind, like in AST_generic, which would
 * allow anon classes.
 *)

(* ------------------------------------------------------------------------- *)
(* Function (and method) definition *)
(* ------------------------------------------------------------------------- *)
and func_def = {
  f_attrs : attributes option;
  f_tok : tok; (* function *)
  f_type : function_type;
  (* "async" always valid ; others only valid for methods *)
  f_modifiers : modifier wrap list;
  f_ref : is_ref;
  (* can be a Name("__lambda", f_tok) when used for lambdas *)
  f_name : ident;
  f_tparams : type_params option;
  (* the dots should be only at the end (unless in semgrep mode) *)
  f_params : parameter comma_list_dots paren;
  (* static-php-ext: *)
  f_return_type : (tok (* : *) * hint_type) option;
  (* the opening/closing brace can be (fakeInfo(), ';') for abstract methods *)
  f_body : stmt_and_def list brace;
}

and function_type =
  | FunctionRegular
  | FunctionLambda
  | MethodRegular
  | MethodAbstract

and parameter = {
  p_attrs : attributes option;
  (* php-facebook-ext: implicit field via constructor parameter,
   * this is always None except for constructors and the modifier
   * can be only Public or Protected or Private (but never Static, etc).
   *)
  p_modifier : modifier wrap option;
  p_type : hint_type option;
  p_ref : is_ref;
  p_name : dname;
  p_default : static_scalar_affect option;
  p_variadic : tok (* ... *) option;
}

and is_ref = tok (* bool wrap ? *) option

(* the f_name in func_def should be a fake name *)
and lambda_def = lexical_vars option * func_def
and lexical_vars = tok (* use *) * lexical_var comma_list paren
and lexical_var = LexicalVar of is_ref * dname

(* todo? could factorize with func_def, but this will require many
 * elements to be fake token, e.g. the parenthesis for parameters
 * when have only one parameter, the brace and semicolon when the body
 * is a simple expression
 *)
and short_lambda_def = {
  (* "async" is the only valid modifier *)
  sl_modifiers : modifier wrap list;
  sl_params : short_lambda_params;
  sl_tok : tok (* ==> *) option; (* async { } doesn't use a ==> *)
  sl_body : short_lambda_body;
}

and short_lambda_params =
  | SLSingleParam of parameter
  | SLParams of parameter comma_list_dots paren
  | SLParamsOmitted (* for async { } lambdas *)

and short_lambda_body = SLExpr of expr | SLBody of stmt_and_def list brace

(* ------------------------------------------------------------------------- *)
(* Constant definition *)
(* ------------------------------------------------------------------------- *)
and constant_def = {
  cst_toks : tok (* const *) * tok (* = *) * tok (* ; *);
  cst_name : ident;
  cst_type : hint_type option;
  cst_val : static_scalar;
}

(* ------------------------------------------------------------------------- *)
(* Class (and interface/trait) definition *)
(* ------------------------------------------------------------------------- *)
(* I used to have a class_def and interface_def because interface_def
 * didn't allow certain forms of statements (methods with a body), but
 * with the introduction of traits, it does not make that much sense
 * to be so specific, so I factorized things. Classes/interfaces/traits
 * are not that different; interfaces are really just abstract traits.
 * We also now include enums, which share a bunch of machinery with classes.
 *)
and class_def = {
  c_attrs : attributes option;
  c_type : class_type;
  c_name : ident;
  c_tparams : type_params option;
  (* PHP uses single inheritance. Interfaces can also use 'extends'
   * but we use the c_implements field for that (because it can be a list).
   *)
  c_extends : extend option;
  (* For classes it's a list of interfaces, for interfaces a list of other
   * interfaces it extends. Traits can also now implement interfaces.
   *)
  c_implements : interface option;
  (* If this class is an enum, what is the underlying type (and
   * constraint) of the enum? *)
  c_enum_type : enum_type option;
  (* The class_stmt for interfaces are restricted to only abstract methods.
   * The class_stmt seems to be unrestricted for traits (it can even
   * contain some 'use') *)
  c_body : class_stmt list brace;
}

and class_type =
  | ClassRegular of tok (* class *)
  | ClassFinal of tok * tok (* final class *)
  | ClassAbstract of tok * tok (* abstract class *)
  | Interface of tok (* interface *)
  (* PHP 5.4 traits: http://php.net/manual/en/language.oop5.traits.php
   * Allow to mixin behaviors and data so it's really just
   * multiple inheritance with a cooler name.
   * note: traits are allowed only at toplevel.
   *)
  | Trait of tok (* trait *)
  | Enum of tok (* enum *)

and extend = tok * class_name
and interface = tok * class_name comma_list

and class_stmt =
  (* This is abused to represent class constants in enums, so sometimes
   * tok is actually fakeInfo. *)
  | ClassConstants of
      modifier wrap list
      * tok (* const *)
      * hint_type option
      * class_constant comma_list
      * tok (*;*)
  | ClassVariables of
      class_var_modifier
      * (* static-php-ext: *)
      hint_type option
      * class_variable comma_list
      * tok (* ; *)
  | Method of method_def
  (* php 5.4, 'use' can appear in classes/traits (but not interface) *)
  | UseTrait of
      tok (*use*)
      * class_name comma_list
      * (tok (* ; *), trait_rule list brace) Common.either
  (* ?? *)
  | ClassType of type_def
  (* semgrep-ext:  *)
  | DeclEllipsis of tok

and class_constant = ident * static_scalar_affect
and class_variable = dname * static_scalar_affect option

and class_var_modifier =
  | NoModifiers of tok (* 'var' *)
  | VModifiers of modifier wrap list
(* a few special names: __construct, __call, __callStatic
 * ugly: f_body is an empty stmt_and_def for abstract method
 * and the ';' is put for the info of the closing brace
 * (and the opening brace is a fakeInfo).
 *)

and method_def = func_def

and modifier =
  (* for constants, methods, variables *)
  | Public
  | Private
  | Protected
  (* for classes, ?? *)
  | Abstract
  | Final
  (* for ?? *)
  | Static
  | Async

(* those are bad features ... noone should use them. *)
and trait_rule =
  | InsteadOf of
      name
      * tok
      * ident
      * tok (* insteadof *)
      * class_name comma_list
      * tok (* ; *)
  | As of
      (ident, name * tok * ident) Common.either
      * tok (* as *)
      * modifier wrap list
      * ident option
      * tok (* ; *)

and trait_constraint_kind = MustExtend | MustImplement

and enum_type = {
  e_tok : tok; (* : *)
  e_base : hint_type;
  e_constraint : (tok (* as *) * hint_type) option;
}

(* ------------------------------------------------------------------------- *)
(* Type definition *)
(* ------------------------------------------------------------------------- *)
(* facebook-ext: *)
and type_def = {
  t_tok : tok; (* type *)
  t_name : ident;
  t_tparams : type_params option;
  t_tconstraint : (tok (* as *) * hint_type) option;
  t_tokeq : tok; (* = *)
  t_kind : type_def_kind;
  t_sc : tok; (* ; *)
}

and type_def_kind = Alias of hint_type

(* ------------------------------------------------------------------------- *)
(* Other declarations *)
(* ------------------------------------------------------------------------- *)
and global_var =
  | GlobalVar of dname
  | GlobalDollar of tok * r_variable
  | GlobalDollarExpr of tok * expr brace

and static_var = dname * static_scalar_affect option
and static_scalar_affect = tok (* = *) * static_scalar

(*****************************************************************************)
(* User attributes, a.k.a annotations *)
(*****************************************************************************)

(* HPHP extension similar to http://en.wikipedia.org/wiki/Java_annotation *)
and attribute =
  | Attribute of string wrap
  | AttributeWithArgs of string wrap * static_scalar comma_list paren

and attributes = attribute comma_list angle

(*****************************************************************************)
(* The toplevels elements *)
(*****************************************************************************)

(* Note that nested functions are usually under a if(defined(...)) at
 * the toplevel. There is no ifdef in PHP so they reuse 'if'.
 *)
and toplevel =
  | TopStmt of stmt
  | FuncDef of func_def
  | ClassDef of class_def
  (* PHP 5.3, see http://us.php.net/const *)
  | ConstantDef of constant_def
  (* facebook extension *)
  | TypeDef of type_def
  (* less: move in directive type? *)
  (* see http://www.php.net/manual/en/language.namespaces.rules.php*)
  (* the qualified_ident below can not have a leading '\' *)
  | NamespaceDef of tok * qualified_ident * tok (* ; *)
  (* when there is no qualified_ident, this means global scope *)
  | NamespaceBracketDef of tok * qualified_ident option * toplevel list brace
  | NamespaceUse of
      tok
      * tok option (* 'function|const' *)
      * namespace_use_rule comma_list
      * tok
  (* ; *)
  (* old:  | Halt of tok * unit paren * tok (* __halt__ ; *) *)
  | NotParsedCorrectly of tok list (* when Flag.error_recovery = true *)
  | FinalDef of tok (* EOF *)

(* the qualified_ident can have a leading '\' *)
and namespace_use_rule = qualified_ident * alias option
and alias = tok (* as *) * ident
and program = toplevel list [@@deriving show { with_path = false }]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type partial = PartialIf of tok * expr [@@deriving show { with_path = false }]

type any =
  | Expr of expr
  | Stmt2 of stmt
  | StmtAndDefs of stmt_and_def list
  | Toplevel of toplevel
  | Toplevels of toplevel list
  | Program of program
  | Partial of partial
  | Argument of argument
  | Arguments of argument comma_list
  | Parameter of parameter
  | Parameters of parameter comma_list_dots paren
  | Body of stmt_and_def list brace
  | ClassStmt of class_stmt
  | ClassConstant2 of class_constant
  | ClassVariable of class_variable
  | ListAssign of list_assign
  | ColonStmt2 of colon_stmt
  | Case2 of case
  | Info of tok
  | InfoList of tok list
  | Ident2 of ident
  | Hint2 of hint_type
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Some constructors *)
(*****************************************************************************)
(* TODO: reuse Tok.fake_tok ? *)
let fakeInfo ?(next_to = None) str = Tok.FakeTokStr (str, next_to)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

let unwrap = fst

let uncomma xs =
  Common.map_filter
    (function
      | Left e -> Some e
      | Right _info -> None)
    xs

let uncomma_dots xs =
  Common.map_filter
    (function
      | Left3 e -> Some e
      | Right3 _info
      | Middle3 _info ->
          None)
    xs

let unarg arg =
  match arg with
  | Arg e -> e
  | ArgRef _ -> failwith "Found a ArgRef"
  | ArgUnpack _ -> failwith "Found a ArgUnpack"
  | ArgLabel _ -> failwith "Found a ArgLabel"

let unargs xs =
  uncomma xs
  |> Common.partition_either (function
       | Arg e -> Left e
       | ArgRef (_, e)
       | ArgUnpack (_, e)
       | ArgLabel (_, _, e) ->
           Right e)

let unmodifiers class_vars =
  match class_vars with
  | NoModifiers _ -> []
  | VModifiers xs -> List.map unwrap xs

let map_paren f (lp, x, rp) = (lp, f x, rp)

let map_comma_list f xs =
  List.map
    (fun x ->
      match x with
      | Left e -> Left (f e)
      | Right tok -> Right tok)
    xs

(*****************************************************************************)
(* Abstract line *)
(*****************************************************************************)

(* When we have extended the AST to add some info about the tokens,
 * such as its line number in the file, we can not use anymore the
 * ocaml '=' to compare AST elements. To overcome this problem, to be
 * able to use again '=', we just have to get rid of all those extra
 * information, to "abstract those line" (al) information.
 *)

let al_info _x = Tok.Ab

(*****************************************************************************)
(* Views *)
(*****************************************************************************)
(* examples:
 * inline more static funcall in expr type or variable type
 *)

(*****************************************************************************)
(* Helpers (could also be put in lib_parsing.ml) *)
(*****************************************************************************)
let str_of_ident e =
  match e with
  | Name x -> unwrap x

let info_of_ident e =
  match e with
  | Name (_x, y) -> y

let str_of_dname (DName x) = unwrap x
let info_of_dname (DName (_x, y)) = y

let info_of_qualified_ident = function
  | [] -> raise Impossible
  | QI x :: _xs -> info_of_ident x
  | QITok tok :: _xs -> tok

let info_of_name x =
  match x with
  | Self tok
  | Parent tok
  | LateStatic tok ->
      tok
  | XName xs -> (
      match xs with
      | [] -> raise Impossible
      | x :: _ -> (
          match x with
          | QITok tok -> tok
          | QI id -> info_of_ident id))

exception TodoNamespace of tok

(* todo? copy the one in cmf/uses_module_helpers.ml now? *)
let str_of_name x =
  match x with
  | XName [ QI x ] -> str_of_ident x
  | Self tok
  | Parent tok
  | LateStatic tok ->
      Tok.content_of_tok tok
  | XName qu -> raise (TodoNamespace (info_of_qualified_ident qu))

let str_of_name_namespace x =
  match x with
  | Self tok
  | Parent tok
  | LateStatic tok ->
      Tok.content_of_tok tok
  | XName xs ->
      xs
      |> List.map (function
           | QITok _ -> "\\"
           | QI id -> str_of_ident id)
      |> Common.join ""

let name_of_class_name x =
  match x with
  | Hint (name, _targs) -> name
  | _ -> raise Impossible

let str_of_class_name x =
  let name = name_of_class_name x in
  str_of_name name

let ident_of_class_name x =
  let name = name_of_class_name x in
  match name with
  | XName [ QI x ] -> x
  | XName qu -> raise (TodoNamespace (info_of_qualified_ident qu))
  | Self _tok
  | Parent _tok
  | LateStatic _tok ->
      raise Impossible

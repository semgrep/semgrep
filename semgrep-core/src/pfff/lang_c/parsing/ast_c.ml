(* Yoann Padioleau
 *
 * Copyright (C) 2012, 2014 Facebook
 * Copyright (C) 2022 r2c
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
open Common2.Infix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A (real) Abstract Syntax Tree (AST) for C, not a Concrete Syntax Tree (CST)
 * as in cst_cpp.ml.
 *
 * This file contains a simplified AST for C. The original
 * C/C++ syntax tree (cst_cpp.ml) is good for code refactoring or
 * code visualization; the types used match exactly the source. However,
 * for other algorithms, the nature of the CST makes the code a bit
 * redundant. Moreover many analysis are far simpler to write on
 * C than C++. Hence the idea of a simple AST which is the
 * original CST where certain constructions have been factorized
 * or even removed.

 * Here is a list of the simplications/factorizations:
 *  - no C++ constructs, just plain C
 *  - no purely syntactical tokens in the AST like parenthesis, brackets,
 *    braces, commas, semicolons, etc. No ParenExpr. No FinalDef. No
 *    NotParsedCorrectly. The only token information kept is for identifiers
 *    for error reporting. See name below.
 *    update: actually we keep more tokens now because of semgrep.
 *  - Few cpp constructs and no ifdefs (they are skipped, and just
 *    one branch is converted to ast_c)
 *  - ...
 *  - no nested struct, they are lifted to the toplevel
 *  - no anonymous structure (an artificial name is gensym'ed)
 *  - no mix of typedef with decl
 *  - sugar is removed, no RecordAccess vs RecordPtAccess, ...
 *  - no init vs expr
 *  - no Case/Default in statement but instead a focused 'case' type
 *
 * less: ast_c_simple_build.ml is probably incomplete, but for now
 * good enough for codegraph purposes on xv6, plan9 and other small C
 * projects.
 *
 * related work:
 *  - CIL, but it works after preprocessing; it makes it harder to connect
 *    analysis results to tools like codemap. It also does not handle some of
 *    the kencc extensions and does not allow to analyze cpp constructs.
 *    CIL has two pointer analysis but they were written with bug finding
 *    in mind I think, not code comprehension which we really care about
 *    in pfff.
 *    In the end I thought generating datalog facts for plan9 using lang_c/
 *    was simpler that modifying CIL (moreover fixing lang_cpp/ and lang_c/
 *    to handle plan9 code was anyway needed for codemap).
 *  - SIL's monoidics. SIL looks a bit complicated, but it might be a good
 *    candidate, unforunately their API are not easily accessible in
 *    a findlib library form yet.
 *  - Clang, but like CIL it works after preprocessing, does not handle kencc,
 *    and does not provide by default a convenient ocaml AST. I could use
 *    clang-ocaml though but it's not easily accessible in a findlib
 *    library form yet.
 *  - we could also use the AST used by cc in plan9 :)
 *
 * See also lang_cpp/parsing/cst_cpp.ml.
*)

(*****************************************************************************)
(* Tokens *)
(*****************************************************************************)

type tok = Parse_info.t
[@@deriving show] (* with tarzan *)

type 'a wrap = 'a * tok
[@@deriving show] (* with tarzan *)

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok
[@@deriving show] (* with tarzan *)

(*****************************************************************************)
(* Name *)
(*****************************************************************************)

type name = string wrap
[@@deriving show] (* with tarzan *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* less: qualifier (const/volatile) *)
type type_ =
  | TBase of name (* int, float, etc *)
  | TPointer of tok * type_
  | TArray of const_expr option * type_
  | TFunction of function_type
  | TStructName of struct_kind * name
  (* hmmm but in C it's really like an int no? but scheck could be
   * extended at some point to do more strict type checking!
  *)
  | TEnumName of name
  | TTypeName of name
  (* tree-sitter-c: kind of poor's man generic via cpp *)
  | TMacroApply of name * type_ bracket

and function_type = (type_ * parameter list)

and parameter =
  | ParamClassic of parameter_classic
  (* varargs of c or semgrep ellipsis *)
  | ParamDots of tok

and parameter_classic = {
  p_type: type_;
  (* when part of a prototype, the name is not always mentionned *)
  p_name: name option;
}

and struct_kind = Struct | Union

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
and expr =
  | Int of int option wrap
  | Float of float option wrap
  | String of string wrap
  | Char of string wrap
  (* c-ext:? *)
  | Null of tok
  | Bool of bool wrap
  | ConcatString of string wrap list

  (* can be a cpp or enum constant (e.g. FOO), or a local/global/parameter
   * variable, or a function name.
  *)
  | Id of name

  | Call of expr * argument list bracket

  (* should be a statement ... but see Datalog_c.instr *)
  | Assign of Ast_cpp.assignOp * expr * expr

  | ArrayAccess of expr * expr bracket (* x[y] *)
  (* Why x->y instead of x.y choice? it's easier then with datalog
   * and it's more consistent with ArrayAccess where expr has to be
   * a kind of pointer too. That means x.y is actually unsugared in (&x)->y
  *)
  | RecordPtAccess of expr * tok * name (* x->y,  and not x.y!! *)

  | Cast of type_ * expr

  (* less: transform into Call (builtin ...) ? *)
  | Postfix of expr * Ast_cpp.fixOp wrap
  (* TODO: rename prefix and change order *)
  | Infix of expr * Ast_cpp.fixOp wrap
  (* less: contains GetRef and Deref!! lift up? *)
  (* TODO: change order *)
  | Unary of expr * Ast_cpp.unaryOp wrap
  | Binary of expr * Ast_cpp.binaryOp wrap * expr

  (* todo: tok *)
  | CondExpr of expr * expr * expr
  (* should be a statement ... *)
  | Sequence of expr * expr

  | SizeOf of tok * (expr, type_) Common.either

  (* should appear only in a variable initializer, or after GccConstructor *)
  | ArrayInit of (expr option * expr) list bracket
  | RecordInit of (name * expr) list bracket
  (* gccext: kenccext: *)
  | GccConstructor  of type_ * expr (* always an ArrayInit (or RecordInit?) *)

  (* tree-sitter-c:
   * only valid in cpp boolean expression context (e.g., #if argument).
   * This is actually not used because we skip ifdef directives anyway.
  *)
  | Defined of tok * name

  (* sgrep-ext: *)
  | Ellipses of tok
  | DeepEllipsis of expr bracket
  | TypedMetavar of name * type_

and argument =
  | Arg of expr

(* really should just contain constants and Id that are #define *)
and const_expr = expr

[@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
type stmt =
  | ExprSt of expr * tok
  | Block of stmt list bracket

  (* todo: tok * stmt option *)
  | If of tok * expr * stmt * stmt option
  | Switch of tok * expr * case list

  | While of tok * expr * stmt
  | DoWhile of tok * stmt * expr
  | For of tok * for_header * stmt

  | Return of tok * expr option
  | Continue of tok | Break of tok

  | Label of name * stmt
  | Goto of tok * name

  (* todo? remove and use DefStmt VarDef? *)
  | Vars of var_decl list
  (* todo: it's actually a special kind of format, not just an expr *)
  | Asm of expr list

  (* tree-sitter-c: used to be restricted to the toplevel *)
  | DefStmt of definition
  | DirStmt of directive

  (* this should never appear! this should be only inside Switch *)
  | CaseStmt of case

and case =
  | Case of tok * expr * stmt list
  | Default of tok * stmt list

and for_header =
  | ForClassic of (var_decl list, expr) Common.either *
                  expr option *
                  expr option
  (* sgrep-ext: *)
  | ForEllipsis of tok (* ... *)

(*****************************************************************************)
(* Variables *)
(*****************************************************************************)

and var_decl = {
  v_name: name;
  v_type: type_;
  (* todo: wrap? make it a more general v_attr? *)
  v_storage: storage;
  v_init: initialiser option;
}
(* can have ArrayInit and RecordInit here in addition to other expr *)
and initialiser = expr
and storage = Extern | Static | DefaultStorage

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)

(* This used to not be mutually recursive with stmt, but tree-sitter-c
 * allows nested directives and defs so simpler to allow this too.
 * Anyway, AST_generic allows this too.
*)

and definition =
  (* less: what about ForwardStructDecl? for mutually recursive structures?
   * probably can deal with it by using typedefs as intermediates.
  *)
  | StructDef of struct_def
  | TypeDef of type_def
  | EnumDef of enum_def
  | FuncDef of func_def
  | VarDef of var_decl (* also contain extern decl *)
  | Prototype of func_def (* empty body *)

and func_def = {
  f_name: name;
  (* less: in theory in C you can define 'typedef int F();' and then define a
   * function like 'F foo { return 1; }' which does not use parenthesis, so we
   * should not force function_type here and also allow typedefs.
  *)
  f_type: function_type;
  f_body: stmt list bracket;
  (* important for codegraph global name resolution to avoid conflicts *)
  f_static: bool;
}


and struct_def = {
  s_kind: struct_kind;
  (* gensym'ed when anonymous struct *)
  s_name: name;
  s_flds: field_def list bracket;
}
(* less: could merge with var_decl, but field have no storage normally *)
and field_def = {
  (* less: bitfield annotation
   * kenccext: the option on fld_name is for inlined anonymous structure.
   * less: nested include/macros
  *)
  fld_name: name option;
  fld_type: type_;
}

and enum_def = {
  (*e_tok: tok;*)
  e_name: name;
  e_consts: (name * const_expr option) list
}

and type_def = {
  (*t_tok: tok;*)
  t_name: name;
  t_type: type_
}

(*****************************************************************************)
(* Cpp *)
(*****************************************************************************)

and directive =
  | Include of tok * string wrap (* path *)
  | Define of tok * name * define_body option
  (* less:  handle also '...' paramater *)
  | Macro of tok * name * (name list) * define_body option
  | OtherDirective of string wrap * string wrap option

and define_body =
  | CppExpr of expr (* actually const_expr when in Define context *)
  (* todo: we want that? even dowhile0 are actually transformed in CppExpr.
   * We have no way to reference a CppStmt in 'stmt' since MacroStmt
   * is not here? So we can probably remove this constructor no?
  *)
  | CppStmt of stmt

(*****************************************************************************)
(* Program *)
(*****************************************************************************)
(* tree-sitter-c: used to be just DefStmt or DirStmt *)
and toplevel = stmt

[@@deriving show { with_path = false }] (* with tarzan *)

type program = toplevel list
[@@deriving show] (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type any =
  (* for semgrep *)
  | Expr of expr
  | Stmt of stmt
  | Stmts of stmt list

  | Type of type_
  | Program of program

[@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let str_of_name (s, _) = s

let looks_like_macro name =
  let s = str_of_name name in
  s =~ "^[A-Z][A-Z_0-9]*$"

let unwrap x = fst x

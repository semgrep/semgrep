(* Yoann Padioleau
 *
 * Copyright (C) 2012, 2014 Facebook
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
(* 
 * A (real) Abstract Syntax Tree for C, not a Concrete Syntax Tree
 * as in cst_cpp.ml.
 * 
 * This file contains a simplified C abstract syntax tree. The original
 * C/C++ syntax tree (cst_cpp.ml) is good for code refactoring or
 * code visualization; the types used match exactly the source. However,
 * for other algorithms, the nature of the AST makes the code a bit
 * redundant. Moreover many analysis are far simpler to write on
 * C than C++. Hence the idea of a SimpleAST which is the
 * original AST where certain constructions have been factorized
 * or even removed.

 * Here is a list of the simplications/factorizations:
 *  - no C++ constructs, just plain C
 *  - no purely syntactical tokens in the AST like parenthesis, brackets, 
 *    braces, commas, semicolons, etc. No ParenExpr. No FinalDef. No
 *    NotParsedCorrectly. The only token information kept is for identifiers
 *    for error reporting. See name below.
 *  - ...
 *  - no nested struct, they are lifted to the toplevel
 *  - no anonymous structure (an artificial name is gensym'ed)
 *  - no mix of typedef with decl
 *  - sugar is removed, no RecordAccess vs RecordPtAccess, ...
 *  - no init vs expr
 *  - no Case/Default in statement but instead a focused 'case' type
 * 
 * less: ast_c_simple_build.ml is probably incomplete, but for now
 * is good enough for codegraph purposes on xv6, plan9 and other small C
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
 * See lang_cpp/parsing/cst_cpp.ml.
 *
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

type tok = Parse_info.t
 [@@deriving show] (* with tarzan *)

type 'a wrap = 'a * tok
 [@@deriving show] (* with tarzan *)

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok
 [@@deriving show] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

type name = string wrap
 [@@deriving show] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

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

 (* less:  '...' varargs support *)
 and function_type = (type_ * parameter list)

  and parameter = {
    p_type: type_;
    (* when part of a prototype, the name is not always mentionned *)
    p_name: name option;
  }

 and struct_kind = Struct | Union

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
and expr =
  | Int of string wrap
  | Float of string wrap
  | String of string wrap
  | Char of string wrap

  (* can be a cpp or enum constant (e.g. FOO), or a local/global/parameter
   * variable, or a function name.
   *)
  | Id of name

  | Call of expr * argument list bracket

  (* should be a statement ... but see Datalog_c.instr *)
  | Assign of Cst_cpp.assignOp * expr * expr

  | ArrayAccess of expr * expr (* x[y] *)
  (* Why x->y instead of x.y choice? it's easier then with datalog
   * and it's more consistent with ArrayAccess where expr has to be
   * a kind of pointer too. That means x.y is actually unsugared in (&x)->y
   *)
  | RecordPtAccess of expr * tok * name (* x->y,  and not x.y!! *)

  | Cast of type_ * expr

  (* less: transform into Call (builtin ...) ? *)
  | Postfix of expr * Cst_cpp.fixOp wrap
  | Infix of expr * Cst_cpp.fixOp wrap
  (* contains GetRef and Deref!! todo: lift up? *)
  | Unary of expr * Cst_cpp.unaryOp wrap
  | Binary of expr * Cst_cpp.binaryOp wrap * expr

  | CondExpr of expr * expr * expr
  (* should be a statement ... *)
  | Sequence of expr * expr

  | SizeOf of (expr, type_) Common.either

  (* should appear only in a variable initializer, or after GccConstructor *)
  | ArrayInit of (expr option * expr) list bracket
  | RecordInit of (name * expr) list bracket
  (* gccext: kenccext: *)
  | GccConstructor  of type_ * expr (* always an ArrayInit (or RecordInit?) *)

  (* sgrep-ext: *)
  | Ellipses of tok

and argument = expr

(* really should just contain constants and Id that are #define *)
and const_expr = expr

 [@@deriving show { with_path = false }] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
type stmt = 
  | ExprSt of expr * tok
  | Block of stmt list bracket

  | If of tok * expr * stmt * stmt option
  | Switch of tok * expr * case list

  | While of tok * expr * stmt
  | DoWhile of tok * stmt * expr
  | For of tok * expr option * expr option * expr option * stmt

  | Return of tok * expr option
  | Continue of tok | Break of tok

  | Label of name * stmt
  | Goto of tok * name

  | Vars of var_decl list
  (* todo: it's actually a special kind of format, not just an expr *)
  | Asm of expr list

  and case =
    | Case of tok * expr * stmt list
    | Default of tok * stmt list

(* ------------------------------------------------------------------------- *)
(* Variables *)
(* ------------------------------------------------------------------------- *)

and var_decl = {
  v_name: name;
  v_type: type_;
  v_storage: storage;
  v_init: initialiser option;
}
 (* can have ArrayInit and RecordInit here in addition to other expr *)
 and initialiser = expr
 and storage = Extern | Static | DefaultStorage

 [@@deriving show { with_path = false }] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)

type func_def = {
  f_name: name;
  f_type: function_type;
  f_body: stmt list bracket;
  f_static: bool;
}
 [@@deriving show { with_path = false }] (* with tarzan *)


type struct_def = {
  s_name: name;
  s_kind: struct_kind;
  s_flds: field_def list bracket;
}
  (* less: could merge with var_decl, but field have no storage normally *)
  and field_def = { 
   (* less: bitfield annotation
    * kenccext: the option on fld_name is for inlined anonymous structure.
    *)
    fld_name: name option;
    fld_type: type_;
  }
 [@@deriving show { with_path = false }] (* with tarzan *)

(* less: use a record *)
type enum_def = name * (name * const_expr option) list
 [@@deriving show { with_path = false }] (* with tarzan *)

(* less: use a record *)
type type_def = name * type_
 [@@deriving show { with_path = false }] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Cpp *)
(* ------------------------------------------------------------------------- *)

type define_body = 
  | CppExpr of expr (* actually const_expr when in Define context *)
  (* todo: we want that? even dowhile0 are actually transformed in CppExpr.
   * We have no way to reference a CppStmt in 'stmt' since MacroStmt
   * is not here? So we can probably remove this constructor no?
   *)
  | CppStmt of stmt
 [@@deriving show { with_path = false }] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)
type toplevel =
  | Include of tok * string wrap (* path *)
  | Define of name * define_body 
  | Macro of name * (name list) * define_body

  (* less: what about ForwardStructDecl? for mutually recursive structures? 
   * probably can deal with it by using typedefs as intermediates.
   *)
  | StructDef of struct_def
  | TypeDef of type_def
  | EnumDef of enum_def
  | FuncDef of func_def
  | Global of var_decl (* also contain extern decl *)
  | Prototype of func_def (* empty body *)
 [@@deriving show { with_path = false }] (* with tarzan *)

type program = toplevel list
 [@@deriving show] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)
type any =
  | Expr of expr
  | Stmt of stmt
  | Stmts of stmt list
  | Type of type_
  | Toplevel of toplevel
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

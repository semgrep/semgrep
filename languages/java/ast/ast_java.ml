(* Joust: a Java lexer, parser, and pretty-printer written in OCaml.
 * Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
 * Copyright (C) 2022  Eric C. Cooper <ecc@cmu.edu>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *
 * Extended by Yoann Padioleau to support more recent versions of Java.
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2020-2024 Semgrep Inc.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An AST for Java.
 *
 * For Java we directly do an AST, as opposed to a CST (Concrete
 * Syntax Tree) as in lang_php/. This should be enough for higlight_java.ml
 * I think (we just need the full list of tokens + the AST with position
 * for the identifiers).
 *
 * todo:
 *  - support generic methods (there is support for generic classes though)
 *  - Look for featherweight Java
 *  - look for middleweight Java (mentioned in Coccinelle4J paper)
 *
 * history:
 * - 2010 port to the pfff infrastructure.
 * - 2012 heavily modified to support annotations, generics, enum, foreach, etc
 * - 2020 support lambdas
 * - 2022 support 'var' directly (via TVar)
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type 'a wrap = 'a * Tok.t [@@deriving show]
type 'a list1 = 'a list (* really should be 'a * 'a list *) [@@deriving show]

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = Tok.t * 'a * Tok.t [@@deriving show]

(* semicolon ";" *)
type sc = Tok.t [@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Ident, qualifier *)
(* ------------------------------------------------------------------------- *)
(* for class/interface/enum names, method/field names, type parameter, ... *)
type ident = string wrap [@@deriving show]

(* for package/import/attributes *)
type qualified_ident = ident list [@@deriving show]

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
type typ =
  (* 'void', 'int', and other primitive types; could be merged with TClass *)
  | TBasic of string wrap
  | TClass of class_type
  | TArray of typ bracket
  (* since Java 10 *)
  | TVar of Tok.t (* 'var' *)

(* class or interface or enum type actually *)
and class_type = (ident * type_arguments option) list1

and type_argument =
  | TArgument of ref_type
  | TWildCard of
      Tok.t (* '?' *)
      * (bool wrap (* extends|super, true = super *) * ref_type) option

and type_arguments = type_argument list bracket

(* A ref type should be a class type or an array of whatever, but not a
 * primitive type. We don't enforce this invariant in the AST to simplify
 * things.
 *)
and ref_type = typ [@@deriving show { with_path = false }]

type type_parameter =
  | TParam of ident * ref_type list (* extends *)
  (* sgrep-ext: *)
  | TParamEllipsis of Tok.t
[@@deriving show { with_path = false }]

(* ------------------------------------------------------------------------- *)
(* Modifier *)
(* ------------------------------------------------------------------------- *)
(* TODO: do as in AST_generic and have attribute = KeywordAttr | Annot of ...*)
type modifier =
  | Public
  | Protected
  | Private
  | Abstract
  | Final
  | Static
  | Transient
  | Volatile
  | Native
  | StrictFP
  | Synchronized
  (* java-ext: ?? *)
  | DefaultModifier
  (* since Java 15 *)
  | Sealed
  | NonSealed
  | Annotation of annotation

(* the wrap for Annotation is a copy of the @ tok already in annotation *)
and modifiers = modifier wrap list

(* ------------------------------------------------------------------------- *)
(* Annotation *)
(* ------------------------------------------------------------------------- *)
and annotation =
  Tok.t (* @ *) * qualified_ident * annotation_element bracket option

and annotation_element =
  | AnnotArgValue of element_value
  | AnnotArgPairInit of annotation_pair list
  | EmptyAnnotArg

and element_value =
  | AnnotExprInit of expr
  | AnnotNestedAnnot of annotation
  | AnnotArrayInit of element_value list bracket

and annotation_pair =
  | AnnotPair of (ident * element_value)
  | AnnotPairEllipsis of Tok.t

and name_or_class_type = identifier_ list

and identifier_ =
  | Id of ident
  | Id_then_TypeArgs of ident * type_arguments
  | TypeArgs_then_Id of type_arguments * identifier_

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* When do we need to have a name with actual type_argument?
 * For certain calls like List.<Int>of(), which are rare.
 * less: do a NameGeneric instead? the type_argument could then be
 *  only at the end?
 *)
and _name = (type_arguments option * ident) list1

(* Can have nested anon class (=~ closures) in expressions hence
 * the use of type ... and ... below
 *)
and expr =
  (* Name below was used for local variable, 'super' special name,
   * and statically computable entities such as Package1.subpackage.Class.
   * Field or method accesses should use Dot (see below). Unfortunately
   * the Java grammar is ambiguous and without contextual information,
   * there is no way to know whether x.y.z is an access to the field z
   * of field y of local variable x or the static field z of class y
   * in package x. See the note on Dot below.
   * Hence the use of Id instead of Name
   *
   * old: | Name of name
   *)
  | NameId of ident
  | This of Tok.t
  (* used in Dot but also can be in Call *)
  (* TODO: Super of tok *)
  (* This is used only in the context of annotations *)
  | NameOrClassType of name_or_class_type
  | Literal of literal
  (* Xxx.class *)
  | ClassLiteral of typ * Tok.t (* 'class' *)
  (* tree-sitter-only: not that ident can be the special "new" *)
  | MethodRef of expr_or_type * Tok.t (* :: *) * type_arguments option * ident
  (* the 'decls option' is for anon classes *)
  | NewClass of Tok.t (* new *) * typ * arguments * decl list bracket option
  (* see tests/java/parsing/NewQualified.java *)
  | NewQualifiedClass of
      expr
      * Tok.t (* . *)
      * Tok.t (* new *)
      * typ
      * arguments
      * decl list bracket option
  (* the int counts the number of [], new Foo[][] => 2 *)
  | NewArray of Tok.t * typ * expr list * int * init option
    (* TODO: QualifiedNew *)
  | Call of expr * arguments
  (* How is parsed X.y ? Could be a Name [X;y] or Dot (Name [X], y)?
   * The static part should be a Name and the more dynamic part a Dot.
   * So variable.field and variable.method should be parsed as
   * Dot (Name [variable], field|method). Unfortunately
   * variable.field1.field2 is currently parsed as
   * Dot (Name [variable;field1], field2). You need semantic information
   * about variable to disambiguate.
   *
   * Why the ambiguity? Names and packages are not
   * first class citizens, so one cant pass a class/package name as an
   * argument to a function, so when have X.Y.z in an expression, the
   * last element has to be a field or a method (if it's a class,
   * people should use X.Y.class), so it's safe to transform such
   * a Name at parsing time in a Dot.
   * The problem is that more things in x.y.z can be a Dot, but to know
   * that requires semantic information about the type of x and y.
   *)
  | Dot of expr * Tok.t * ident
  | ArrayAccess of expr * expr bracket
  | Unary of AST_generic.operator (* +/-/~/! *) wrap * expr
  | Postfix of expr * AST_generic.incr_decr wrap
  | Prefix of AST_generic.incr_decr wrap * expr
  | Infix of expr * AST_generic.operator wrap * expr
  | SwitchE of Tok.t * expr * (cases * stmts) list (* TODO bracket *)
  (* usually just a single typ, but can also have intersection type t1 & t2 *)
  | Cast of typ list1 bracket * expr
  | InstanceOf of expr * ref_type
  | Conditional of expr * expr * expr
  (* ugly java, like in C assignement is an expression not a statement :( *)
  | Assign of expr * Tok.t * expr
  | AssignOp of expr * AST_generic.operator wrap * expr
  (* javaext: 1.? *)
  | Lambda of parameters * Tok.t (* -> *) * stmt
  (* sgrep-ext: *)
  | Ellipsis of Tok.t
  | DeepEllipsis of expr bracket
  | TypedMetavar of ident * typ
  | ObjAccessEllipsis of expr * Tok.t (* ... *)

and literal =
  | Int of Parsed_int.t
  | Float of float option wrap
  | String of string wrap (* TODO: bracket *)
  | Char of string wrap
  | Bool of bool wrap
  | Null of Tok.t
  (* alt: merge with String? Java 15 *)
  (* TODO? the string contains the enclosing triple quotes for now *)
  | TextBlock of string wrap (* TODO bracket *)

and arguments = expr list bracket
and expr_or_type = (expr, typ) Either_.t

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt =
  | EmptyStmt of Tok.t (* could be Block [] *)
  | Block of stmts bracket
  | Expr of expr * Tok.t
  | If of Tok.t * expr * stmt * stmt option
  | Switch of Tok.t * expr * (cases * stmts) list (* TODO bracket *)
  | While of Tok.t * expr * stmt
  | Do of Tok.t * stmt * expr (* TODO * sc *)
  | For of Tok.t * for_control * stmt
  | Break of Tok.t * ident option
  | Continue of Tok.t * ident option
  | Return of Tok.t * expr option
  | Label of ident * stmt
  | Sync of Tok.t (* 'synchronized' *) * expr (* todo: bracket *) * stmt
  | Try of Tok.t * resources option * stmt * catches * (Tok.t * stmt) option
  | Throw of Tok.t * expr
  (* decl as statement *)
  | LocalVarList of var_with_init list * sc
  (* in recent Java, used to be only LocalClass *)
  | DeclStmt of decl
  | DirectiveStmt of directive
  (* javaext: http://java.sun.com/j2se/1.4.2/docs/guide/lang/assert.html *)
  | Assert of Tok.t * expr * expr option (* assert e or assert e : e2 *)

and stmts = stmt list
and case = Case of (Tok.t * expr) | Default of Tok.t
and cases = case list

and for_control =
  | ForClassic of for_init * expr list (* TODO: expr option? *) * expr list
  | Foreach of var_definition * expr
  (* sgrep-ext: *)
  | ForEllipsis of Tok.t

and for_init = ForInitVars of var_with_init list | ForInitExprs of expr list
and catch = Tok.t * catch_exn * stmt
and catches = catch list

and catch_exn =
  | CatchParam of var_definition * typ list (* union type *)
  (* sgrep-ext: *)
  | CatchEllipsis of Tok.t

(* javaext: java 8 *)
and resources = resource list bracket

(* the expr is a an id or a field access *)
and resource = (var_with_init, expr) Either_.t

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
(* TODO: use entity to factorize fields in method_decl, class_decl like
 * we do in AST_generic.ml
 *)
and entity = {
  name : ident;
  mods : modifiers;
  (* None for inferred lambda parameters or $...PARAMS *)
  type_ : typ option; (* todo? tparams: type_parameter list; *)
}

(* ------------------------------------------------------------------------- *)
(* variable (local var, parameter) declaration *)
(* ------------------------------------------------------------------------- *)
and var_definition = entity

(* less: could be merged with var *)
and var_with_init = { f_var : var_definition; f_init : init option }

(* less: could merge with expr *)
and init = ExprInit of expr | ArrayInit of init list bracket

(* ------------------------------------------------------------------------- *)
(* Methods, fields *)
(* ------------------------------------------------------------------------- *)

(* method or constructor *)
and method_decl = {
  (* m_var.type_ is None for a constructor *)
  m_var : var_definition;
  (* the var.mod in params can only be Final or Annotation *)
  m_formals : parameters;
  m_throws : typ list; (* todo: m_tparams *)
  (* Empty for methods in interfaces.
   * For constructor the first stmts can contain
   * explicit_constructor_invocations (which are currently parsed as
   * regular Call)
   *)
  m_body : stmt;
}

and constructor_decl = method_decl
and parameters = parameter_binding list (* TODO bracket *)

and parameter_binding =
  | ParamClassic of parameter_classic
  (* java-ext: ?? *)
  | ParamSpread of Tok.t (* ... *) * parameter_classic
  (* java-ext: 8, name is always 'this' in parameter *)
  | ParamReceiver of parameter_classic
  (* sgrep-ext: *)
  | ParamEllipsis of Tok.t

and parameter_classic = var_definition
and field = var_with_init

(* ------------------------------------------------------------------------- *)
(* Enum *)
(* ------------------------------------------------------------------------- *)

(* less: could merge with class_decl and add EnumConstant in decl *)
and enum_decl = {
  en_name : ident;
  en_mods : modifiers;
  en_impls : ref_type list;
  en_body : enum_body;
}

and enum_body = enum_constant list * enum_body_decl list
(* TODO bracket *)

(* http://docs.oracle.com/javase/1.5.0/docs/guide/language/enums.html *)
and enum_constant = ident * arguments option * class_body option

(* Not all kind of decls. Restrictions are ?? *)
and enum_body_decl = decl

(* ------------------------------------------------------------------------- *)
(* Class/Interface *)
(* ------------------------------------------------------------------------- *)
and class_decl = {
  cl_name : ident;
  cl_kind : class_kind wrap;
  (* TODO: bracket option *)
  cl_tparams : type_parameter list;
  cl_mods : modifiers;
  (* always at None for interface *)
  cl_extends : typ option;
  (* for interface this is actually the extends *)
  cl_impls : ref_type list;
  (* TODO: cl_permits: ref_type list; for classes and interfaces *)
  (* javaext: for Record *)
  cl_formals : parameters;
  (* javaext: the methods body used to be always empty for interface *)
  cl_body : class_body;
}

and class_kind =
  | ClassRegular
  | Interface
  (* @interface, a.k.a annotation type declaration *)
  (* java-ext: tree-sitter-only: *)
  | AtInterface
  (* java-ext: java 15 *)
  | Record

(* Not all kind of decls. Restrictions are ?? *)
and class_body = decl list bracket

(*****************************************************************************)
(* Declaration *)
(*****************************************************************************)
and decl =
  (* top decls *)
  | Class of class_decl
  | Enum of enum_decl
  (* inside class/interface/enum *)
  | Method of method_decl
  | Field of field
  | Init of Tok.t option (* static *) * stmt
  (* java-ext: tree-sitter-only: only in AtInterface class_decl  *)
  | AnnotationTypeElementTodo of Tok.t
  | EmptyDecl of Tok.t (* ; *)
  (* sgrep-ext: allows ... inside interface, class declarations *)
  | DeclEllipsis of Tok.t
  | DeclMetavarEllipsis of ident

(*****************************************************************************)
(* Directives *)
(*****************************************************************************)
(* old: this used to not be mutually recursive, but now that we have
 * DirectiveStmt we need the 'and' below.
 *)
and import =
  | ImportAll of Tok.t * qualified_ident * Tok.t (* * *)
  | ImportFrom of Tok.t * qualified_ident * ident

(* old: the Package and Import used to be allowed only at the toplevel and
 * followed by a unique class/interface first in a 'compilation_unit' record
 * type.
 *)
and directive =
  (* The qualified ident can also contain "*" at the very end. *)
  | Package of Tok.t * qualified_ident * Tok.t (* ; *)
  (* The Tok.t is for static import (javaext:) *)
  | Import of Tok.t option (* static *) * import
  | ModuleTodo of Tok.t
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

(* old: was compilation_unit with record of package/imports/decls but
 * tree-sitter-java (and probably recent Java) is more flexible *)
type program = stmts [@@deriving show]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type partial =
  (* the body will be empty in m_body or cl_body *)
  | PartialDecl of decl
  (* partial stmts *)
  | PartialIf of Tok.t * expr
  | PartialTry of Tok.t * stmt
  | PartialCatch of catch
  | PartialFinally of (Tok.t * stmt)
[@@deriving show { with_path = false }]

type any =
  (* useful one for semgrep *)
  | AExpr of expr
  | AStmt of stmt
  | AStmts of stmt list
  | ATyp of typ
  | AMod of modifier wrap
  | Partial of partial
  (* rest *)
  | AIdent of ident
  | AVar of var_definition
  | AInit of init
  | AMethod of method_decl
  | AField of field
  | AClass of class_decl
  | AProgram of program
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let unwrap = fst

let is_final xs =
  let xs = List_.map fst xs in
  List.mem Final xs

let is_final_static xs =
  let xs = List_.map fst xs in
  List.mem Final xs && List.mem Static xs

let basic_entity id mods = { name = id; mods; type_ = None }
let entity_of_id id = basic_entity id []

(* TODO: reuse Tok.fake_tok ? *)
let fakeInfo ?next_to str = Tok.FakeTok (str, next_to)

(* used for error reporting usually *)
let rec tok_of_identifier_ (id : identifier_) : Tok.t =
  match id with
  | Id id
  | Id_then_TypeArgs (id, _) ->
      snd id
  | TypeArgs_then_Id (_, id_) -> tok_of_identifier_ id_

let tok_of_name_or_class_type (x : name_or_class_type) : Tok.t =
  match x with
  | [] -> raise (Tok.NoTokenLocation "Ast_java.tok_of_name_or_class_type")
  | x :: _ -> tok_of_identifier_ x

(*****************************************************************************)
(* Parsing helpers *)
(*****************************************************************************)
(* those types and functions are used in parser_java.mly but also now in
 * semgrep/.../Parse_java_tree_sitter.ml
 *)

type var_decl_id = IdentDecl of ident | ArrayDecl of var_decl_id

let rec tok_of_var = function
  | IdentDecl i -> snd i
  | ArrayDecl v -> tok_of_var v

let mk_param_id id = ParamClassic (entity_of_id id)

(* Move array dimensions from variable name to type. *)
let rec canon_var mods t_opt v =
  match v with
  | IdentDecl str -> { mods; type_ = t_opt; name = str }
  | ArrayDecl v' -> (
      match t_opt with
      | None -> raise Common.Impossible
      | Some t ->
          canon_var mods (Some (TArray (Tok.fake_bracket (tok_of_var v') t))) v'
      )

let method_header mods mtype (v, formals) throws =
  {
    m_var = canon_var mods (Some mtype) v;
    m_formals = formals;
    m_throws = throws;
    m_body = EmptyStmt (Tok.fake_tok (tok_of_var v) ";");
  }

(* Return a list of field declarations in canonical form. *)
let decls f mods vtype vars =
  let dcl (v, init) =
    f { f_var = canon_var mods (Some vtype) v; f_init = init }
  in
  List_.map dcl vars

let typ_of_qualified_id xs = TClass (xs |> List_.map (fun id -> (id, None)))

let name_of_id id =
  (*Name ([[], id]) *)
  NameId id

(* TODO: use a special at some point *)
let super tok = name_of_id ("super", tok)
let new_id tok = ("new", tok)

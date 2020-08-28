(* Yoann Padioleau
 * 
 * Copyright (C) 2002-2005 Yoann Padioleau
 * Copyright (C) 2006-2007 Ecole des Mines de Nantes
 * Copyright (C) 2008-2009 University of Urbana Champaign
 * Copyright (C) 2010-2014 Facebook
 * Copyright (C) 2019-2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A Concrete Syntax Tree for C/C++/Cpp.
 *
 * This is a big file. C++ is a big and complicated language, and dealing
 * directly with preprocessor constructs from cpp makes the language 
 * even bigger.
 *
 * This file started as a simple AST for C. It was then extended
 * to deal with cpp idioms (see 'cppext:' tag) and converted to a CST.
 * Then, it was extented again to deal with gcc extensions (see gccext:),
 * and C++ constructs (see c++ext:), and a few kencc (the plan9 compiler)
 * extensions (see kenccext:). Finally it was extended to deal with
 * a few C++0x and C++11 extensions (see c++0x:).
 * 
 * gcc introduced StatementExpr which made 'expr' and 'stmt' mutually
 * recursive. It also added NestedFunc for even more mutual recursivity.
 * With C++ templates, because template arguments can be types or expressions
 * and because templates are also qualifiers, almost all types
 * are now mutually recursive ...
 *
 * Some stuff are tagged 'semantic:' which means that they can be computed
 * only after parsing. 
 * 
 * See also lang_c/parsing/ast_c.ml and lang_clang/parsing/ast_clang.ml
 * (as well as mini/ast_minic.ml).
 * 
 * todo: 
 *  - support C++0x11, e.g. lambdas
 * 
 * related work:
 *  - https://github.com/facebook/facebook-clang-plugins
 *    or https://github.com/Antique-team/clangml
 *    but by both using clang they work after preprocessing. This is
 *    fine for bug finding, but for codemap we need to parse as is,
 *    and we need to do it fast (calling clang is super expensive because
 *    calling cpp and parsing the end result is expensive)
 *  - EDG
 *  - see the CC'09 paper
 *)

(*****************************************************************************)
(* The CST related types *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

(* Contains among other things the position of the token through
 * the Parse_info.token_location embedded inside it, as well as the
 * transformation field that makes possible spatch on C/C++/cpp code.
 *)
type tok = Parse_info.t
 [@@deriving show]

(* a shortcut to annotate some information with token/position information *)
type 'a wrap  = 'a * tok

(* TODO: delete *)
and 'a wrapx  = 'a * tok list

and 'a paren   = tok * 'a * tok
and 'a brace   = tok * 'a * tok
and 'a bracket = tok * 'a * tok 
and 'a angle   = tok * 'a * tok 

and 'a comma_list = 'a wrapx list
and 'a comma_list2 = ('a, tok (* the comma *)) Common.either list

(* semicolon *)
and sc = tok

 [@@deriving show] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Ident, name, scope qualifier *)
(* ------------------------------------------------------------------------- *)

(* c++ext: in C 'name' and 'ident' are equivalent and are just strings.
 * In C++ 'name' can have a complex form like 'A::B::list<int>::size'.
 * I use Q for qualified. I also have a special type to make the difference
 * between intermediate idents (the classname or template_id) and final idents.
 * Note that sometimes final idents are also classnames and can have final
 * template_id.
 * 
 * Sometimes some elements are not allowed at certain places, for instance
 * converters can not have an associated Qtop. But I prefered to simplify
 * and have a unique type for all those different kinds of names.
 *)
type name = tok (*::*) option  * (qualifier * tok (*::*)) list * ident_or_op

 and ident_or_op =
   (* function name, macro name, variable, classname, enumname, namespace *)
   | IdIdent of ident
   (* c++ext: *)  
   | IdTemplateId of ident * template_arguments
   (* c++ext: for operator overloading *)
   | IdDestructor of tok(*~*) * ident
   | IdOperator of tok * (operator * tok list)
   | IdConverter of tok * type_

   and ident = string wrap
 
   and template_arguments = template_argument comma_list angle
    (* C++ allows integers for template arguments! (=~ dependent types) *)
    and template_argument = (type_, expr) Common.either

 and qualifier = 
   | QClassname of ident (* classname or namespacename *)
   | QTemplateId of ident * template_arguments

 (* special cases *)
 and class_name     = name (* only IdIdent or IdTemplateId *)
 and namespace_name = name (* only IdIdent *)
 and typedef_name   = name (* only IdIdent *)
 and enum_name      = name (* only IdIdent *)

 and ident_name = name (* only IdIdent *)

(* TODO: do like in parsing_c/
 * and ident_string = 
 *  | RegularName of string wrap
 *
 *  (* cppext: *)
 *  | CppConcatenatedName of (string wrap) wrap (* the ## separators *) list
 *  (* normally only used inside list of things, as in parameters or arguments
 *   * in which case, cf cpp-manual, it has a special meaning *)
 *  | CppVariadicName of string wrap (* ## s *)
 *  | CppIdentBuilder of string wrap (* s ( ) *) * 
 *                      ((string wrap) wrap list) (* arguments *)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* We could have a more precise type in type_, in expression, etc, but
 * it would require too much things at parsing time such as checking whether
 * there is no conflicts structname, computing value, etc. It's better to
 * separate concerns, so I put '=>' to mean what we would really like. In fact
 * what we really like is defining another type_, expression, etc
 * from scratch, because many stuff are just sugar.
 * 
 * invariant: Array and FunctionType have also typeQualifier but they
 * dont have sense. I put this to factorise some code. If you look in
 * grammar, you see that we can never specify const for the array
 * himself (but we can do it for pointer).
 *)
and type_ = typeQualifier * typeC
 (* less: rename to TBase, TPointer, etc *)
 and typeC =
  | BaseType        of baseType

  | Pointer         of tok (*'*'*) * type_
  (* c++ext: *)
  | Reference       of tok (*'&'*) * type_ 

  | Array           of constExpression option bracket * type_
  | FunctionType    of functionType

  | EnumName        of tok (* 'enum' *) * ident (*enum_name*)
  | StructUnionName of structUnion wrap * ident (*ident_name*)
  (* c++ext: TypeName can now correspond also to a classname or enumname
   * and it is a name so it can have some IdTemplateId in it.
   *)
  | TypeName of name(*typedef_name*)
  (* only to disambiguate I think *)
  | TypenameKwd of tok (* 'typename' *) * name(*typedef_name*)

  (* gccext: TypeOfType may seems useless, why declare a __typeof__(int)
   * x; ? But when used with macro, it allows to fix a problem of C which
   * is that type declaration can be spread around the ident. Indeed it
   * may be difficult to have a macro such as '#define macro(type,
   * ident) type ident;' because when you want to do a macro(char[256],
   * x), then it will generate invalid code, but with a '#define
   * macro(type, ident) __typeof(type) ident;' it will work. *)
  | TypeOf of tok * (type_, expr) Common.either paren

  (* should be really just at toplevel *)
  | EnumDef of enum_definition (* => string * int list *)
  (* c++ext: bigger type now *)
  | StructDef of class_definition 

  (* forunparser: *)
  | ParenType of type_ paren

  and  baseType = 
    | Void of tok
    | IntType   of intType   * tok (* TOFIX there should be * tok list *)
    | FloatType of floatType * tok (* TOFIX there should be * tok list *)

     (* stdC: type section. 'char' and 'signed char' are different *)
      and intType   = 
        | CChar (* obsolete? | CWchar  *)
        | Si of signed
         (* c++ext: maybe could be put in baseType instead ? *)
        | CBool | WChar_t 

        and signed = sign * base
         and base = 
           | CChar2 | CShort | CInt | CLong 
           (* gccext: *)
           | CLongLong 
         and sign = Signed | UnSigned

      and floatType = CFloat | CDouble | CLongDouble

and typeQualifier = 
  { const: tok option; volatile: tok option; }

(* TODO: like in parsing_c/
 * (* gccext: cppext: *)
 * and attribute = attributebis wrap
 *  and attributebis =
 *   | Attribute of string 
 *)

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)
(* Because of StatementExpr, we can have more 'new scope', but it's
 * rare I think. For instance with 'array of constExpression' we could
 * have an StatementExpr and a new (local) struct defined. Same for
 * Constructor.
 *)
and expr = 
  (* Id can be an enumeration constant, variable, function name.
   * cppext: Id can also be the name of a macro. sparse says
   *  "an identifier with a meaning is a symbol". 
   * c++ext: Id is now a 'name' instead of a 'string' and can be
   *  also an operator name.
   * todo: split in Id vs IdQualified like in ast_generic.ml?
   *)
  | Id of name * ident_info (* semantic: see check_variables_cpp.ml *) 
  | C of constant

  (* I used to have FunCallSimple but not that useful, and we want scope info
   * for FunCallSimple too because can have fn(...) where fn is actually
   * a local *)
  | Call of expr * argument comma_list paren

  (* gccext: x ? /* empty */ : y <=> x ? x : y; *)
  | CondExpr       of expr * tok * expr option * tok * expr

  (* should be considered as statements, bad C langage *)
  | Sequence       of expr * tok (* , *) * expr                   
  | Assign         of expr * assignOp * expr        

  | Postfix        of expr * fixOp wrap
  | Infix          of expr * fixOp wrap
  (* contains GetRef and Deref!! todo: lift up? *)
  | Unary          of expr * unaryOp wrap
  | Binary         of expr * binaryOp wrap * expr        

  | ArrayAccess    of expr * expr bracket

   (* The Pt is redundant normally, could be replace by DeRef RecordAccess *)
  | RecordAccess   of expr * tok (* . *)  * name
  | RecordPtAccess of expr * tok (* -> *) * name

  (* c++ext: note that second paramater is an expr, not a name *)
  | RecordStarAccess   of expr * tok (* .* *) * expr
  | RecordPtStarAccess of expr * tok (* ->* *) * expr

  | SizeOfExpr     of tok * expr
  | SizeOfType     of tok * type_ paren

  | Cast          of type_ paren * expr

  (* gccext: *)        
  | StatementExpr of compound paren (* ( {  } ) new scope*)
  (* gccext: kenccext: *)
  | GccConstructor  of type_ paren * initialiser comma_list brace

  (* c++ext: *)
  | This of tok
  | ConstructedObject of type_ * argument comma_list paren
  | TypeId     of tok * (type_, expr) Common.either paren
  | CplusplusCast of cast_operator wrap * type_ angle * expr paren
  | New of tok (*::*) option * tok * 
      argument comma_list paren option (* placement *) *
      type_ *
      argument comma_list paren option (* initializer *)

  | Delete      of tok (*::*) option * tok * expr
  | DeleteArray of tok (*::*) option * tok * unit bracket * expr
  | Throw of tok * expr option 

  (* forunparser: *)
  | ParenExpr of expr paren

  (* sgrep-ext: *)
  | Ellipses of tok

  | ExprTodo of tok

  (* see check_variables_cpp.ml *)
  and ident_info = {
    mutable i_scope: Scope_code.t 
        [@printer fun _fmt _ -> "??"];
  }

 (* cppext: normally should just have type argument = expr *)
  and argument = 
       | Arg of expr
       (* cppext: *)
       | ArgType of type_
       (* cppext: for really unparsable stuff ... we just bailout *)
       | ArgAction of action_macro
       and action_macro = 
         | ActMisc of tok list

  (* I put 'string' for Int and Float because 'int' would not be enough.
   * Indeed OCaml ints are 31 bits. So it's simpler to use 'string'. 
   * Same reason to have 'string' instead of 'int list' for the String case.
   * 
   * note: '-2' is not a constant; it is the unary operator '-'
   * applied to the constant '2'. So the string must represent a positive
   * integer only. 
   *)
  and constant = 
    | Int    of (string wrap  (* * intType*)) 
    | Float  of (string wrap * floatType)
    | Char   of (string wrap * isWchar) (* normally it is equivalent to Int *)
    | String of (string wrap * isWchar) 

    | MultiString of string wrap list  (* can contain MacroString *)
    (* c++ext: *)
    | Bool of bool wrap

    and isWchar = IsWchar | IsChar

  and unaryOp  = 
    | UnPlus |  UnMinus | Tilde | Not 
    (* less: could be lift up, those are really important operators *)
    | GetRef | DeRef 
    (* gccext: via &&label notation *)
    | GetRefLabel
  and assignOp = SimpleAssign of tok | OpAssign of arithOp wrap
  and fixOp    = Dec | Inc

  and binaryOp = Arith of arithOp | Logical of logicalOp
       and arithOp   = 
         | Plus | Minus | Mul | Div | Mod
         | DecLeft | DecRight 
         | And | Or | Xor
       and logicalOp = 
         | Inf | Sup | InfEq | SupEq 
         | Eq | NotEq 
         | AndLog | OrLog

  (* c++ext: used elsewhere but prefer to define it close to other operators *)
  and ptrOp = PtrStarOp | PtrOp
  and allocOp = NewOp | DeleteOp | NewArrayOp | DeleteArrayOp
  and accessop = ParenOp | ArrayOp
  and operator = 
    | BinaryOp of binaryOp
    | AssignOp of assignOp
    | FixOp of fixOp
    | PtrOpOp of ptrOp
    | AccessOp of accessop
    | AllocOp of allocOp
    | UnaryTildeOp | UnaryNotOp | CommaOp

 (* c++ext: *)
  and cast_operator = 
    | Static_cast | Dynamic_cast | Const_cast | Reinterpret_cast

 and constExpression = expr (* => int *)

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)
(* note: assignement is not a statement, it's an expr :(
 * (wonderful C language).
 * note: I use 'and' for type definition because gccext allows statements as
 * expressions, so we need mutual recursive type definition now.
 *)
and stmt =
  | Compound      of compound   (* new scope *)
  | ExprStatement of expr option * sc

  (* selection *)
  | If of tok * expr paren * stmt * (tok * stmt) option
   (* need to check that all elements in the compound start
    * with a case:, otherwise it's unreachable code.
    *)
  | Switch of tok * expr paren * stmt

  (* iteration *)
  | While   of tok * expr paren * stmt
  | DoWhile of tok * stmt * tok * expr paren * sc
  | For of 
      tok *
      (expr option * sc * expr option * sc * expr option) paren *
      stmt
  (* cppext: *)
  | MacroIteration of ident * argument comma_list paren * stmt

  | Jump          of jump * sc

  (* labeled *)
  | Label   of string wrap * tok (* : *) * stmt
  | Case      of tok * expr * tok (* : *) * stmt 
  (* gccext: *)
  | CaseRange of tok * expr * tok (* ... *) * expr * tok (* : *) * stmt 
  | Default of tok * tok (* : *) * stmt

  (* c++ext: in C this constructor could be outside the statement type, in a
   * decl type, because declarations are only at the beginning of a compound
   * normally. But in C++ we can freely mix declarations and statements.
   *)
  | DeclStmt  of block_declaration 
  (* c++ext: *)
  | Try of tok * compound * handler list
  (* gccext: *)
  | NestedFunc of func_definition
  (* cppext: *)
  | MacroStmt of tok

  | StmtTodo of tok

  (* cppext: c++ext:
   * old: compound = (declaration list * stmt list)
   * old: (declaration, stmt) either list 
   *)
  and compound = stmt_sequencable list brace

  and jump  = 
    | Goto of tok * string wrap
    | Continue of tok | Break of tok
    | Return of tok * expr option
    (* gccext: goto *exp *)
    | GotoComputed of tok * tok * expr

  (* c++ext: *)
  and handler = tok * exception_declaration paren * compound
   and exception_declaration = 
     | ExnDeclEllipsis of tok
     | ExnDecl of parameter

  (* easier to put at stmt_list level than stmt level *)
  and stmt_sequencable = 
    | StmtElem of stmt
    (* cppext: *) 
    | CppDirectiveStmt of cpp_directive
    | IfdefStmt of ifdef_directive (* * stmt list *)


(*****************************************************************************)
(* Declarations *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Block Declaration *)
(* ------------------------------------------------------------------------- *)
(* a.k.a declaration_stmt *)
and block_declaration = 
 (* Before I had a Typedef constructor, but why make this special case and not
  * have also StructDef, EnumDef, so that 'struct t {...} v' which would
  * then generate two declarations. 
  * If you want a cleaner C AST use ast_c.ml.
  * note: before the need for unparser, I didn't have a DeclList but just 
  * a Decl.
  *)
  | DeclList of onedecl comma_list * sc

  (* cppext: todo? now factorize with MacroTop ?  *)
  | MacroDecl of tok list * ident * argument comma_list paren * tok

  (* c++ext: using namespace *)
  | UsingDecl of (tok * name * sc)
  | UsingDirective of tok * tok (*'namespace'*) *  namespace_name * sc
  | NameSpaceAlias of tok * ident * tok (*=*) * namespace_name * sc
  (* gccext: *)
  | Asm of tok * tok option (*volatile*) * asmbody paren * sc

  (* gccext: *)
  and asmbody = string wrap list * colon list
      and colon = Colon of tok (* : *) * colon_option comma_list
      and colon_option = 
            | ColonExpr of tok list * expr paren
            | ColonMisc of tok list

(* ------------------------------------------------------------------------- *)
(* Variable definition (and also field definition) *)
(* ------------------------------------------------------------------------- *)

  (* note: onedecl includes prototype declarations and class_declarations! 
   * c++ext: onedecl now covers also field definitions as fields can have
   * storage in C++.
   *)
  and onedecl = {
    (* option cos can have empty declaration or struct tag declaration.
     * kenccext: name can also be empty because of anonymous fields.
    *)
    v_namei: (name * init option) option;
    v_type: type_;
    v_storage: storage; (* TODO: use for c++0x 'auto' inferred locals *)
    (* v_attr: attribute list; *) (* gccext: *)
  }
    and storage = NoSto | StoTypedef of tok | Sto of storageClass wrap
      and storageClass  = Auto | Static | Register | Extern
   (* Friend ???? Mutable? *)

   (*c++ext: TODO *)
   (* I am not sure what it means to declare a prototype inline, but gcc
    * accepts it. *)
   and _func_specifier = Inline | Virtual

   and init = 
     | EqInit of tok (*=*) * initialiser
     (* c++ext: constructed object *)
     | ObjInit of argument comma_list paren

    and initialiser =
      | InitExpr of expr 
      | InitList of initialiser comma_list brace
      (* gccext: *)
      | InitDesignators of designator list * tok (*=*) * initialiser
      | InitFieldOld  of ident * tok (*:*) * initialiser
      | InitIndexOld  of expr bracket * initialiser

      (* ex: [2].y = x,  or .y[2]  or .y.x. They can be nested *)
      and designator =
        | DesignatorField of tok(*:*) * ident
        | DesignatorIndex of expr bracket
        | DesignatorRange of (expr * tok (*...*) * expr) bracket
              
        
(* ------------------------------------------------------------------------- *)
(* Function definition *)
(* ------------------------------------------------------------------------- *)
(* Normally we should define another type functionType2 because there 
 * are more restrictions on what can define a function than a pointer 
 * function. For instance a function declaration can omit the name of the
 * parameter whereas a function definition can not. But, in some cases such
 * as 'f(void) {', there is no name too, so I simplified and reused the 
 * same functionType type for both declarations and function definitions.
 *)
and func_definition = {
   f_name: name;
   f_type: functionType;
   f_storage: storage;
   (* todo: gccext: inline or not:, f_inline: tok option *)
   f_body: compound;
   (*f_attr: attribute list;*) (* gccext: *)
  }
   and functionType = { 
     ft_ret: type_; (* fake return type for ctor/dtor *)
     ft_params: parameter comma_list paren;
     ft_dots: (tok(*,*) * tok(*...*)) option;
     (* c++ext: *) 
     ft_const: tok option; (* only for methods, TODO put in attribute? *)
     ft_throw: exn_spec option;
   }
     and parameter = {
        p_name: ident option;
        p_type: type_;
        p_register: tok option; (* TODO put in attribute? *)
        (* c++ext: *)
        p_val: (tok (*=*) * expr) option;
      }
    and exn_spec = (tok * name comma_list2 paren)

 (* less: simplify? need differentiate at this level? could have
  * is_ctor, is_dtor helper instead. 
  * TODO do via attributes?
  *)
 and func_or_else =
  | FunctionOrMethod of func_definition
  (* c++ext: special member function *)
  | Constructor of func_definition (* TODO explicit/inline, chain_call *)
  | Destructor of func_definition

 and method_decl =
   | MethodDecl of onedecl * (tok * tok) option (* '=' '0' *) * sc
   | ConstructorDecl of 
       ident * parameter comma_list paren * sc
   | DestructorDecl of 
       tok(*~*) * ident * tok option paren * exn_spec option * sc

(* ------------------------------------------------------------------------- *)
(* enum definition *)
(* ------------------------------------------------------------------------- *)
(* less: use a record *)
and enum_definition =
    tok (*enum*) * ident option * enum_elem comma_list brace  

    and enum_elem = {
      e_name: ident;
      e_val: (tok (*=*) * constExpression) option;
    }

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)
and class_definition = {
  c_kind: structUnion wrap; 
  (* the ident can be a template_id when do template specialization. *)
  c_name: ident_name(*class_name??*) option;
  (* c++ext: *)
  c_inherit: (tok (* ':' *) * base_clause comma_list) option;
  c_members: class_member_sequencable list brace (* new scope *);
  }
  and structUnion =
    | Struct | Union
    (* c++ext: *)
    | Class

  and base_clause = {
    i_name: class_name;
    i_virtual: tok option;
    i_access: access_spec wrap option;
  }

  (* used in inheritance spec (base_clause) and class_member *)
  and access_spec = Public | Private | Protected

  (* was called 'field wrap' before *)
  and class_member = 
    (* could put outside and take class_member list *)
    | Access of access_spec wrap * tok (*:*)

    (* before unparser, I didn't have a FieldDeclList but just a Field. *)
    | MemberField of fieldkind comma_list * sc
    | MemberFunc of func_or_else    
    | MemberDecl of method_decl
         
    | QualifiedIdInClass of name (* ?? *) * sc
         
    | TemplateDeclInClass of (tok * template_parameters * declaration)
    | UsingDeclInClass of (tok (*using*) * name * sc)

     (* gccext: and maybe c++ext: *)
    | EmptyField  of sc

     (* At first I thought that a bitfield could be only Signed/Unsigned.
      * But it seems that gcc allows char i:4. C rule must say that you
      * can cast into int so enum too, ... 
      * c++ext: FieldDecl was before Simple of string option * type_
      * but in c++ fields can also have storage (e.g. static) so now reuse
      * ondecl.
      *)
      and fieldkind = 
        | FieldDecl of onedecl
        | BitField of ident option * tok(*:*) * type_ * constExpression
            (* type_ => BitFieldInt | BitFieldUnsigned *) 
   
  and class_member_sequencable = 
    | ClassElem of class_member
    (* cppext: *)
    | CppDirectiveStruct of cpp_directive
    | IfdefStruct of ifdef_directive (*  * field list *)

(*****************************************************************************)
(* Cpp *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* cppext: cpp directives, #ifdef, #define and #include body *)
(* ------------------------------------------------------------------------- *)
and cpp_directive =
  | Define of tok (* #define*) * ident * define_kind * define_val
  (* less: should split tok in 2 *)
  | Include of tok (* #include s *) * inc_kind * string (* path *)
  | Undef of ident (* #undef xxx *)
  | PragmaAndCo of tok

  and define_kind =
   | DefineVar
   | DefineFunc   of string wrap comma_list paren
   and define_val = 
     | DefineExpr of expr
     | DefineStmt of stmt
     | DefineType of type_
     | DefineFunction of func_definition
     | DefineInit of initialiser (* in practice only { } with possible ',' *)
     (* ?? dead? DefineText of string wrap *)
     | DefineEmpty

     (* do ... while(0) *)
     | DefineDoWhileZero of tok * stmt * tok * tok paren
     | DefinePrintWrapper of tok (* if *) * expr paren * name

     | DefineTodo

  and inc_kind = 
    | Local (* "" *)
    | Standard (* <> *)
    | Weird (* ex: #include SYSTEM_H *)

  (* less: 'a ifdefed = 'a list wrap (* ifdef elsif else endif *) *)
  and ifdef_directive = ifdefkind wrap
     and ifdefkind = 
       | Ifdef (* todo? of string? *)
       (* less: IfIf of formula_cpp ? *)
       | IfdefElse
       | IfdefElseif
       | IfdefEndif 
  (* less:
   * set in Parsing_hacks.set_ifdef_parenthize_info. It internally use 
   * a global so it means if you parse the same file twice you may get
   * different id. I try now to avoid this pb by resetting it each 
   * time I parse a file.
   *
   *   and matching_tag = 
   *     IfdefTag of (int (* tag *) * int (* total with this tag *))
   *)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)

(* it's not really 'toplevel' because the elements below can be nested
 * inside namespaces or some extern. It's not really 'declaration'
 * either because it can defines stuff. But I keep the C++ standard
 * terminology.
 * 
 * note that we use 'block_declaration' below, not 'statement'.
 *)
and declaration = 
  | BlockDecl of block_declaration (* include struct/globals/... definitions *)
  | Func of func_or_else

  (* c++ext: *)
  | TemplateDecl of tok * template_parameters * declaration
  | TemplateSpecialization of tok * unit angle * declaration
  (* the list can be empty *)
  | ExternC     of tok * tok * declaration
  | ExternCList of tok * tok * declaration_sequencable list brace
  (* the list can be empty *)
  | NameSpace of tok * ident * declaration_sequencable list brace
  (* after have some semantic info *)
  | NameSpaceExtend of string * declaration_sequencable list 
  | NameSpaceAnon   of tok * declaration_sequencable list brace

  (* gccext: allow redundant ';' *)
  | EmptyDef of sc

  | DeclTodo

 (* c++ext: *)
 and template_parameter = parameter (* todo? more? *)
  and template_parameters = template_parameter comma_list angle

  (* easier to put at stmt_list level than stmt level *)
  and declaration_sequencable = 
    | DeclElem of declaration

    (* cppext: *) 
    | CppDirectiveDecl of cpp_directive
    | IfdefDecl of ifdef_directive (* * toplevel list *)
    (* cppext: *)
    | MacroTop of ident * argument comma_list paren * tok option
    | MacroVarTop of ident * sc

    (* could also be in decl *)
    | NotParsedCorrectly of tok list

  [@@deriving show { with_path = false }]

type toplevel = declaration_sequencable
  [@@deriving show]

(* finally *)
type program = toplevel list
  [@@deriving show]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type any = 
  | Program of program
  | Toplevel of toplevel
  | Cpp of cpp_directive
  | Stmt of stmt
  | Expr of expr
  | Type of type_
  | Name of name

  | BlockDecl2 of block_declaration
  | ClassDef of class_definition
  | FuncDef of func_definition
  | FuncOrElse of func_or_else
  | ClassMember of class_member
  | OneDecl of onedecl
  | Init of initialiser
  | Stmts of stmt list

  | Constant of constant

  | Argument of argument
  | Parameter of parameter

  | Body of compound

  | Info of tok
  | InfoList of tok list

  [@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Some constructors *)
(*****************************************************************************)
let nQ = {const=None; volatile= None}
let noIdInfo () = { i_scope = Scope_code.NoScope; }
let noQscope = []

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let unwrap x = fst x
let uncomma xs = List.map fst xs
let unparen (_, x, _) = x
let unbrace (_, x, _) = x

let unwrap_typeC (_qu, typeC) = typeC

(* When want add some info in AST that does not correspond to 
 * an existing C element.
 * old: when don't want 'synchronize' on it in unparse_c.ml
 * (now have other mark for tha matter).
 * used by parsing hacks
 *)
let make_expanded ii =
  let noVirtPos = ({Parse_info.str="";charpos=0;line=0;column=0;file=""},-1) in
  let (a, b) = noVirtPos in
  { ii with Parse_info.token = Parse_info.ExpandedTok 
      (Parse_info.get_original_token_location ii.Parse_info.token, a, b) }

(* used by parsing hacks *)
let rewrap_pinfo pi ii =  
  {ii with Parse_info.token = pi}


(* used while migrating the use of 'string' to 'name' in check_variables *)
let (string_of_name_tmp: name -> string) = fun name ->
  let (_opt, _qu, id) = name in
  match id with
  | IdIdent (s,_) -> s
  | _ -> failwith "TODO:string_of_name_tmp"

let (ii_of_id_name: name -> tok list) = fun name ->
  let (_opt, _qu, id) = name in
  match id with
  | IdIdent (_s,ii) -> [ii]
  | IdOperator (_, (_op, ii)) -> ii
  | IdConverter (_tok, _ft) -> failwith "ii_of_id_name: IdConverter"
  | IdDestructor (tok, (_s, ii)) -> [tok;ii]
  | IdTemplateId ((_s, ii), _args) -> [ii]

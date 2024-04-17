(* Joust: a Java lexer, parser, and pretty-printer written in OCaml
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
 * LALR(1) (ocamlyacc) grammar for Java
 *
 * Attempts to conform to:
 * The Java Language Specification, Second Edition
 * - James Gosling, Bill Joy, Guy Steele, Gilad Bracha
 *
 * Many modifications by Yoann Padioleau. Attempts to conform to:
 * The Java Language Specification, Third Edition, with some fixes from
 * http://www.cmis.brighton.ac.uk/staff/rnb/bosware/javaSyntax/syntaxV2.html
 * (broken link)
 *
 * Official (but incomplete) specification as of Java 14:
 * https://docs.oracle.com/javase/specs/jls/se14/html/jls-19.html
 *
 * More modifications by Yoann Padioleau to support more recent versions.
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2020-2022 r2c
 *
 * Support for:
 *  - generics (partial)
 *  - enums, foreach, ...
 *  - annotations (partial)
 *  - lambdas
 *)
%{
open Common
open Either_
open AST_generic (* for the arithmetic operator *)
open Ast_java

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let empty_body = Tok.unsafe_fake_bracket []
let fake_dot = Tok.unsafe_fake_tok "."

(* todo? use a Ast.special? *)
let super_ident ii = ("super", ii)

let named_type (str, ii) = TBasic (str,ii)
let void_type ii = named_type ("void", ii)

(* we have to use a 'name' to specify reference types in the grammar
 * because of some ambiguity but what we really wanted was an
 * identifier followed by some type arguments.
 *)
let (class_type: name_or_class_type -> typ) = fun xs ->
  let ys =
    xs |> List.map (function
    | Id x -> x, None
    | Id_then_TypeArgs (x, xs) -> x, Some xs
    | TypeArgs_then_Id _ -> raise Parsing.Parse_error
    )
  in
  TClass ys

let (name: name_or_class_type -> expr) = fun xs ->
  let ys =
    xs |> List.map (function
     | Id x -> None, x
     | Id_then_TypeArgs (x, xs) ->
      (* this is ok because of the ugly trick we do for Cast
       * where we transform a Name into a ref_type
       *)
        Some xs, x
     | TypeArgs_then_Id (xs, Id x) ->
        Some xs, x
     | TypeArgs_then_Id (_xs, _) ->
        raise Parsing.Parse_error
     )
  in
  (* TODO: we should not discard the type information *)
  let ys = ys |> List.map snd in

  (* x.y.z -> Dot (Dot (NameId x, y), z) *)
  let ys = List.rev ys in
  (* z.y.x -> Dot (Dot (NameId x, y), z) *)

  let rec aux xs =
    match xs with
    | [] -> raise Impossible
    | [x] -> NameId x
    | [y;x] -> Dot (NameId x, fake_dot, y)
    | z::xs ->
        let e = aux xs in
        Dot (e, fake_dot, z)
  in
  aux ys

let fix_name arg =
   (* Ambiguity. It could be a field access (Dot) or a qualified
    * name (Name). See ast_java.ml note on the Dot constructor for
    * more information.
    * The last dot has to be a Dot and not a Name at least,
    * but more elements of Name could be a Dot too.
    * Hence the switch to just NameId instead of the code below
    *)
   (*
   match List.rev arg with
   | (Id id)::x::xs ->
       Dot (Name (name (List.rev (x::xs))), Parse_info.fake_info ".", id)
   | _ ->
       Name (name arg)
   *)
   name arg

let (qualified_ident: name_or_class_type -> qualified_ident) = fun xs ->
  xs |> List.map (function
  | Id x -> x
  | Id_then_TypeArgs _ -> raise Parsing.Parse_error
  | TypeArgs_then_Id _ -> raise Parsing.Parse_error
  )


let expr_to_typename expr =
(*
    match expr with
    | Name name ->
        TClass (name |> List.map (fun (xs, id) -> id, xs))
    (* ugly, undo what was done in postfix_expression *)
    | Dot (Name name, _, id) ->
        TClass ((name @ [[], id]) |> List.map (fun (xs, id) -> id, xs))
    | _ ->
        pr2 "cast_expression pb";
        pr2_gen expr;
        raise Todo
*)
  (* TODO: we lost the type information with the switch to NameId and Dot *)
  let rec aux e =
    match e with
    | NameId id -> [id]
    | Dot (e, _, id) -> aux e @ [id]
    | _ ->
        UCommon.pr2 "cast_expression pb";
        UCommon.pr2_gen expr;
        raise Todo
   in
   let xs = aux expr in
   typ_of_qualified_id xs

let mk_stmt_or_stmts = function
  | [] -> AStmts []
  | [x] -> AStmt x
  | xs -> AStmts xs
%}

(*************************************************************************)
(* Tokens *)
(*************************************************************************)

(* classic *)
%token <Tok.t> TUnknown
%token <Tok.t> EOF

(*-----------------------------------------*)
(* The comment tokens *)
(*-----------------------------------------*)
(* Those tokens are not even used in this file because they are
 * filtered in some intermediate phases (in Parse_java.lexer_function
 * by using TH.is_comment(). But they still must be declared
 * because ocamllex may generate them, or some intermediate phases may also
 * generate them (like some functions in parsing_hacks.ml).
 *)
%token <Tok.t> TComment TCommentNewline TCommentSpace

(*-----------------------------------------*)
(* The normal tokens *)
(*-----------------------------------------*)

(* tokens with "values" *)
%token <Parsed_int.t> TInt
%token <float option * Tok.t> TFloat
%token <string * Tok.t> TChar TString

%token <(string * Tok.t)> IDENTIFIER
%token <(string * Tok.t)> PRIMITIVE_TYPE

%token <Tok.t> LP "("		(* ( *)
%token <Tok.t> RP ")"		(* ) *)
%token <Tok.t> LC "{"		(* { *)
%token <Tok.t> RC "}"		(* } *)
%token <Tok.t> LB "["		(* [ *)
%token <Tok.t> RB "]"		(* ] *)
%token <Tok.t> SM ";"		(* ; *)
%token <Tok.t> CM ","		(* , *)
%token <Tok.t> DOT "."		(* . *)

%token <Tok.t> EQ "="		(* = *)
%token <Tok.t> GT		(* > *)
%token <Tok.t> LT		(* < *)
%token <Tok.t> NOT		(* ! *)
%token <Tok.t> COMPL		(* ~ *)
%token <Tok.t> COND		(* ? *)
%token <Tok.t> COLON ":"		(* : *)
%token <Tok.t> EQ_EQ		(* == *)
%token <Tok.t> LE		(* <= *)
%token <Tok.t> GE		(* >= *)
%token <Tok.t> NOT_EQ		(* != *)
%token <Tok.t> AND_AND		(* && *)
%token <Tok.t> OR_OR		(* || *)
%token <Tok.t> INCR		(* ++ *)
%token <Tok.t> DECR		(* -- *)
%token <Tok.t> PLUS		(* + *)
%token <Tok.t> MINUS		(* - *)
%token <Tok.t> TIMES		(* * *)
%token <Tok.t> DIV		(* / *)
%token <Tok.t> AND		(* & *)
%token <Tok.t> OR		(* | *)
%token <Tok.t> XOR		(* ^ *)
%token <Tok.t> MOD		(* % *)
%token <Tok.t> LS		(* << *)
%token <Tok.t> SRS		(* >> *)
%token <Tok.t> URS		(* >>> *)

%token <Tok.t> AT "@"		(* @ *)
%token <Tok.t> DOTS "..."		(* ... *) LDots "<..." RDots "...>"
%token <Tok.t> ARROW "->"		(* -> *)
%token <Tok.t> COLONCOLON "::"		(* :: *)


%token <(AST_generic.operator * Tok.t)> OPERATOR_EQ
	(* += -= *= /= &= |= ^= %= <<= >>= >>>= *)

(* keywords tokens *)
%token <Tok.t>
 ABSTRACT BREAK CASE CATCH CLASS CONST CONTINUE
 DEFAULT DO ELSE EXTENDS FINAL FINALLY FOR GOTO
 IF IMPLEMENTS IMPORT INSTANCEOF INTERFACE
 NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN
 STATIC STRICTFP SUPER SWITCH SYNCHRONIZED
 THIS THROW THROWS TRANSIENT TRY VOID VOLATILE WHILE
 (* javaext: *)
 ASSERT
 ENUM
 TRUE FALSE NULL
 VAR
 (* This terminal is currently not used but lexed so that we generate parse
  * errors on code using records so that we switch to the tree-sitter
  * parser for files using this construct.
  *)
 RECORD

(*-----------------------------------------*)
(* Extra tokens: *)
(*-----------------------------------------*)

(* to avoid some conflicts *)
%token <Tok.t> LB_RB

(* Those fresh tokens are created in parsing_hacks_java.ml *)
%token <Tok.t> LT_GENERIC		(* < ... > *)
%token <Tok.t> LP_LAMBDA		(* ( ... ) ->  *)
%token <Tok.t> DEFAULT_COLON		(* default :  *)
%token <Tok.t> LP_PARAM		(* ( ) { }  *)

(*-----------------------------------------*)
(* semgrep-ext: *)
(*-----------------------------------------*)

%token <(string * Tok.t)> METAVAR_ELLIPSIS

(*************************************************************************)
(* Priorities *)
(*************************************************************************)

(*************************************************************************)
(* Rules type declaration *)
(*************************************************************************)
%start goal semgrep_pattern
%type <Ast_java.program> goal
%type <Ast_java.any>     semgrep_pattern

%%
(*************************************************************************)
(* Macros *)
(*************************************************************************)
optl(X):
 | (* empty *) { [] }
 | X           { $1 }

listc(X):
 | X { [$1] }
 | listc(X) "," X { $1 @ [$3] }

listc0(X): optl(listc(X)) { $1 }

list_sep(X,Sep):
 | X                      { [$1] }
 | list_sep(X,Sep) Sep X  { $1 @ [$3] }

(*************************************************************************)
(* TOC *)
(*************************************************************************)
(* TOC:
 *  goal
 *  name
 *  type
 *  expr
 *  statement
 *  declaration
 *  anotation
 *  class/interfaces
 *)

(*************************************************************************)
(* Toplevel *)
(*************************************************************************)

goal: compilation_unit EOF  { $1 }

(* conflicts: was simply
 *   package_declaration_opt import_declarations_opt type_declarations_opt
 * but with an annotation now possible on package_declaration, seeing an
 * '@' the LALR(1) parser does not know if it's the start of an annotation
 * for a package or class_declaration. So we need to unfold those _opt.
 *)
compilation_unit:
  | package_declaration import_declaration+ type_declaration*
    { [DirectiveStmt $1] @ ($2 |> List.map (fun x -> DirectiveStmt x))
       @ List.flatten $3 }
  | package_declaration                     type_declaration*
    { [DirectiveStmt $1] @ List.flatten $2 }
  |                     import_declaration+ type_declaration*
    { ($1 |> List.map (fun x -> DirectiveStmt x)) @ List.flatten $2 }
  |                                         type_declaration*
    { List.flatten $1 }

type_declaration:
 | class_and_co_declaration { [DeclStmt $1] }
 | ";"  { [] }

class_and_co_declaration:
 | class_declaration      { Class $1 }
 | interface_declaration  { Class $1 }
 (* javaext: 1.? *)
 | enum_declaration       { Enum $1 }
 | annotation_type_declaration { Class $1 }

(*************************************************************************)
(* Semgrep pattern *)
(*************************************************************************)

semgrep_pattern:
 | expression   EOF              { AExpr $1 }
 | item_no_dots EOF              { mk_stmt_or_stmts $1 }
 | item_no_dots item+ EOF        { mk_stmt_or_stmts ($1 @ (List.flatten $2)) }

 | annotation EOF { AMod (Annotation $1, Common2.fst3 $1) }

 | explicit_constructor_invocation_stmt EOF { AStmt $1 }
 | explicit_constructor_invocation EOF      { AExpr $1 }
 | static_initializer EOF { AStmt (DeclStmt $1) }

 (* partial defs *)
 | class_header          EOF { Partial (PartialDecl (Class $1)) }
 | method_header         EOF { Partial (PartialDecl (Method $1)) }
 (* partial stmts *)
 | IF "(" expression ")" EOF { Partial (PartialIf ($1, $3)) }
 | TRY block             EOF { Partial (PartialTry ($1, $2)) }
 | catch_clause          EOF { Partial (PartialCatch $1) }
 | finally               EOF { Partial (PartialFinally $1) }

item_no_dots:
 | statement_no_dots { [$1] }
 | item_other { $1 }

item:
 | statement { [$1] }
 | item_other { $1 }

item_other:
 | item_declaration         { [DeclStmt $1] }
 | import_declaration  { [DirectiveStmt $1] }
 | package_declaration { [DirectiveStmt $1] }
 | local_variable_declaration_statement { [$1] }

item_declaration:
 | class_and_co_declaration { $1 }
 | method_declaration       { Method $1 }
 | constructor_declaration_top  { $1 }

(* coupling: copy paste of statement, without dots *)
statement_no_dots:
 | statement_without_trailing_substatement  { $1 }
 | labeled_statement  { $1 }
 | if_then_statement  { $1 }
 | if_then_else_statement  { $1 }
 | while_statement  { $1 }
 | for_statement  { $1 }


(* Mostly a copy-paste of constructor_declaration but using LP_PARAM.
 * conflicts: without this trick, when reading 'foo(' there is no way to
 * know whether this is the start of a function call or a constructor
 * declaration. We need to look-ahead far to see whether there is
 * a {} after the closing parenthesis (which is what parsing_hacks_java.ml
 * does).
 *)
constructor_declaration_top:
  modifiers_opt constructor_declarator_top optl(throws) constructor_body
  { let (id, formals) = $2 in
    let var = { mods = $1; type_ = None; name = id } in
    Method { m_var = var; m_formals = formals; m_throws = $3;
	     m_body = $4 }
  }

constructor_declarator_top: identifier LP_PARAM listc0(formal_parameter) ")"
  { $1, $3}

(*************************************************************************)
(* Package, Import *)
(*************************************************************************)

(* ident_list *)
package_declaration: modifiers_opt PACKAGE qualified_ident ";"
  { Package ($2, $3, $4) (* TODO $1*)}

(* javaext: static_opt 1.? *)
import_declaration:
 | IMPORT STATIC? name ";"
    { (Import ($2,
      (match List.rev (qualified_ident $3) with
      | x::xs -> ImportFrom ($1, List.rev xs, x)
      | [] -> raise Impossible
      ))) }
 | IMPORT STATIC? name "." TIMES ";"
    { (Import ($2, ImportAll ($1, qualified_ident $3, $5)))}

(*************************************************************************)
(* Ident, namespace  *)
(*************************************************************************)
identifier: IDENTIFIER { $1 }

qualified_ident:
  | IDENTIFIER                     { [$1] }
  | qualified_ident "." IDENTIFIER { $1 @ [$3] }

name:
 | identifier_           { [$1] }
 | name "." identifier_  { $1 @ [$3] }
 | name "." LT_GENERIC listc(type_argument) GT identifier_
     { $1@[TypeArgs_then_Id(($3, $4, $5), $6)] }

identifier_:
 | identifier                                     { Id $1 }
 | identifier LT_GENERIC listc0(type_argument) GT { Id_then_TypeArgs($1, ($2, $3, $4)) }

(*************************************************************************)
(* Types *)
(*************************************************************************)

type_:
 | primitive_type  { $1 }
 | reference_type  { $1 }

primitive_type: PRIMITIVE_TYPE  { named_type $1 }

class_or_interface_type: name {
    match class_type $1 with
    (* since Java 10 *)
    | TClass [("var", ii), None] -> TVar ii
    | t -> t
 }

reference_type:
 | class_or_interface_type { $1 }
 | array_type { $1 }

array_type:
 | primitive_type          LB_RB                { TArray ($2, $1, $2) }
 | class_or_interface_type (* was name *) LB_RB { TArray ($2, $1, $2) }
 | array_type              LB_RB                { TArray ($2, $1, $2) }

(*----------------------------*)
(* Generics arguments *)
(*----------------------------*)

(* javaext: 1? *)
type_argument:
 | reference_type { TArgument $1 }
 | COND           { TWildCard ($1, None) }
 | COND EXTENDS reference_type { TWildCard ($1, Some ((false,$2), $3)) }
 | COND SUPER   reference_type { TWildCard ($1, Some ((true, $2), $3)) }

(*----------------------------*)
(* Generics parameters *)
(*----------------------------*)
(* javaext: 1? *)
type_parameters:
  | LT listc(type_parameter) GT { $2 }
  (* sgrep-ext: ugly, but <...> will be parsed as <... > *)
  | "<..." GT { Flag_parsing.sgrep_guard [TParamEllipsis $1] }
  | "<..." "," listc(type_parameter) GT
      { Flag_parsing.sgrep_guard (TParamEllipsis $1::$3) }
  | "<..." "," listc(type_parameter) "," "...>"
      { Flag_parsing.sgrep_guard ([TParamEllipsis $1]@$3@[TParamEllipsis $5])}
  | LT listc(type_parameter) "," "...>"
      { Flag_parsing.sgrep_guard ($2 @ [ TParamEllipsis $4]) }

type_parameter:
 | identifier               { TParam ($1, []) }
 | identifier EXTENDS bound { TParam ($1, $3) }
 (* sgrep-ext: *)
 | "..."                    { Flag_parsing.sgrep_guard (TParamEllipsis $1) }

bound: list_sep(reference_type, AND) { $1 }

(*************************************************************************)
(* Expressions *)
(*************************************************************************)

typed_metavar: "(" type_ IDENTIFIER ")"
   { Flag_parsing.sgrep_guard (TypedMetavar($3, $2))  }

(* Note that 'primary' does not include the simple identifier (Name) case.
 * 'postfix_expression' does.
 *)
primary:
 | primary_no_new_array       { $1 }
 | array_creation_expression  { $1 }

primary_no_new_array:
 | literal                            { $1 }
 | THIS                               { This $1 }
 | "(" expression ")"                 { $2 }
 | class_instance_creation_expression { $1 }
 | field_access                       { $1 }
 | method_invocation                  { $1 }
 | array_access                       { $1 }
 (* sgrep-ext: *)
 | typed_metavar       { $1 }
 | METAVAR_ELLIPSIS { NameId ($1) }
 (* just can use some reserved identifiers as field now? *)
 | name "." THIS       { Dot (name $1, $2, ("this", $3)) }
 (* javaext: ? *)
 | class_literal       { $1 }
 (* javaext: ? *)
 | method_reference { $1 }
 (* javaext: ? *)
 | array_creation_expression_with_initializer { $1 }

literal:
 | TRUE    { Literal (Bool (true, $1)) }
 | FALSE   { Literal (Bool (false, $1)) }
 | TInt    { Literal (Int ($1)) }
 | TFloat  { Literal (Float ($1)) }
 | TChar   { Literal (Char ($1)) }
 | TString { Literal (String ($1)) }
 | NULL    { Literal (Null $1) }

class_literal:
 | primitive_type "." CLASS  { ClassLiteral ($1, $3) }
 | name           "." CLASS  { ClassLiteral (class_type $1, $3) }
 | array_type     "." CLASS  { ClassLiteral ($1, $3) }
 | VOID           "." CLASS  { ClassLiteral (void_type $1, $3) }

class_instance_creation_expression:
 | NEW name "(" listc0(argument) ")" class_body?
   { NewClass ($1, class_type $2, ($3,$4,$5), $6) }
 (* javaext: ? *)
 | primary "." NEW identifier "(" listc0(argument) ")" class_body?
   { NewQualifiedClass ($1, $2, $3, TClass ([$4,None]), ($5,$6,$7), $8) }
 (* javaext: not in 2nd edition java language specification. *)
 | name "." NEW identifier "(" listc0(argument) ")" class_body?
   { NewQualifiedClass (((name $1)), $2, $3,TClass [$4,None],($5,$6,$7),$8)}

(* A new array that cannot be accessed right away by appending [index]:
 * new String[2][1]  // a 2-dimensional array
 *)
array_creation_expression:
 | NEW primitive_type dim_expr+ dims_opt
       { NewArray ($1, $2, $3, $4, None) }
 | NEW name           dim_expr+ dims_opt
       { NewArray ($1, class_type $2, $3, $4, None) }

(* A new array that can be accessed right away by appending [index] as follows:
 * new String[] { "abc", "def" }[1]  // a string
 *)
array_creation_expression_with_initializer:
 | NEW primitive_type dims array_initializer
       { NewArray ($1, $2, [], $3, Some $4) }
 | NEW name           dims array_initializer
       { NewArray ($1, class_type $2, [], $3, Some $4) }

dim_expr: "[" expression "]"  { $2 }

dims:
 |      LB_RB  { 1 }
 | dims LB_RB  { $1 + 1 }

field_access:
 | primary "." identifier        { Dot ($1, $2, $3) }
 | SUPER   "." identifier        { Dot (NameId (super_ident $1), $2, $3) }
 (* javaext: ? *)
 | name "." SUPER "." identifier { Dot (Dot (name $1,$2,super_ident $3),$2,$5)}

array_access:
 | name                 "[" expression "]"
    { ArrayAccess (((name $1)),($2, $3, $4)) }
 | primary_no_new_array "[" expression "]"
    { ArrayAccess ($1, ($2, $3, $4)) }

(*----------------------------*)
(* Method call *)
(*----------------------------*)

method_invocation:
 | name "(" listc0(argument) ")"
    { Call (name $1, ($2, $3, $4))
         (* match List.rev $1 with
          (* TODO: lose information of TypeArgs_then_Id *)
          | ((Id x) | (TypeArgs_then_Id (_, Id x)))::xs ->
              let tok = snd x in
              (match xs with
              (* old: I used to add an implicit this.x but we don't do that in
               * tree-sitter, so better not do it either *)
              | [] -> Call (Name (name $1), ($2, $3, $4))
              | _ -> Call (Dot (Name (name (List.rev xs)),tok,x),($2,$3,$4))
              )
          | _ ->
              UCommon.pr2 "method_invocation pb";
              UCommon.pr2_gen $1;
              raise Impossible
         *)
        }
 | primary "." identifier "(" listc0(argument) ")"
	{ Call ((Dot ($1, $2, $3)), ($4,$5,$6)) }
 | SUPER "." identifier "(" listc0(argument) ")"
	{ Call ((Dot (NameId (super_ident $1), $2, $3)), ($4,$5,$6)) }
 (* javaext: ? *)
 | name "." SUPER "." identifier "(" listc0(argument) ")"
	{ Call (Dot (Dot (name $1, $2, super_ident $3), $4, $5), ($6,$7,$8))}
 (* sgrep-ext: *)
 | primary "." "..."    { ObjAccessEllipsis ($1, $2) }
 | name    "." "..."    { ObjAccessEllipsis (fix_name $1, $2) }


argument: expression { $1 }

(*----------------------------*)
(* Arithmetic *)
(*----------------------------*)

postfix_expression:
 | primary  { $1 }
 | name     { fix_name $1 }

 | post_increment_expression  { $1 }
 | post_decrement_expression  { $1 }

post_increment_expression: postfix_expression INCR
  { Postfix ($1, (Incr, $2)) }

post_decrement_expression: postfix_expression DECR
  { Postfix ($1, (Decr, $2)) }

unary_expression:
 | pre_increment_expression  { $1 }
 | pre_decrement_expression  { $1 }
 | PLUS unary_expression     { Unary ((Plus,$1), $2) }
 | MINUS unary_expression    { Unary ((Minus,$1), $2) }
 | unary_expression_not_plus_minus  { $1 }

pre_increment_expression: INCR unary_expression
  { Prefix ((Incr, $1), $2) }

pre_decrement_expression: DECR unary_expression
  { Prefix ((Decr, $1), $2) }

(* see conflicts.txt Cast note to understand the need of this rule *)
unary_expression_not_plus_minus:
 | postfix_expression  { $1 }
 | COMPL unary_expression  { Unary ((BitNot,$1), $2) }
 | NOT unary_expression    { Unary ((Not,$1), $2) }
 | cast_expression  { $1 }

(* original rule:
 * | "(" primitive_type dims_opt ")" unary_expression
 * | "(" reference_type ")" unary_expression_not_plus_minus
 * Semantic action must ensure that '( expression )' is really '( name )'.
 * Conflict with regular paren expr; when see ')' dont know if
 * can reduce to expr or shift name, so have to use
 * expr in both cases (see primary_new_array for the other rule
 * using "(" expression ")")
 *)
cast_expression:
 | "(" primitive_type ")" unary_expression  { Cast (($1,[$2],$3), $4) }
 | "(" array_type ")" unary_expression_not_plus_minus { Cast(($1,[$2],$3),$4)}
 | "(" expression ")" unary_expression_not_plus_minus
	{  Cast (($1,[expr_to_typename $2],$3), $4) }

(* this can not be put inside cast_expression. See conflicts.txt*)
cast_lambda_expression: "(" expression ")" lambda_expression
     { Cast (($1,[expr_to_typename $2],$3), $4) }


multiplicative_expression:
 | unary_expression  { $1 }
 | multiplicative_expression TIMES unary_expression { Infix ($1, (Mult,$2),$3)}
 | multiplicative_expression DIV unary_expression   { Infix ($1, (Div,$2),$3)}
 | multiplicative_expression MOD unary_expression   { Infix ($1, (Mod,$2),$3)}

additive_expression:
 | multiplicative_expression  { $1 }
 | additive_expression PLUS multiplicative_expression  { Infix($1,(Plus,$2),$3)}
 | additive_expression MINUS multiplicative_expression { Infix($1,(Minus,$2),$3)}
 (* Must expand out ellipsis options in order to avoid resolution conflicts *)
 | "..."               PLUS multiplicative_expression  { Flag_parsing.sgrep_guard (Infix (Ellipsis $1, (Plus, $2), $3)) }
 | "..."               MINUS multiplicative_expression { Flag_parsing.sgrep_guard (Infix (Ellipsis $1, (Minus, $2), $3)) }
 | additive_expression PLUS "..."                      { Flag_parsing.sgrep_guard (Infix ($1, (Plus, $2), Ellipsis $3)) }
 | additive_expression MINUS "..."                     { Flag_parsing.sgrep_guard (Infix ($1, (Minus, $2), Ellipsis $3)) }

shift_expression:
 | additive_expression  { $1 }
 | shift_expression LS additive_expression  { Infix ($1, (LSL,$2), $3) }
 | shift_expression SRS additive_expression  { Infix ($1, (LSR,$2), $3) }
 | shift_expression URS additive_expression  { Infix ($1, (ASR,$2), $3) }

relational_expression:
 | shift_expression  { $1 }
 (* possible many conflicts if don't use a LT2 *)
 | relational_expression LT shift_expression  { Infix ($1, (Lt,$2), $3) }
 | relational_expression GT shift_expression  { Infix ($1, (Gt,$2), $3) }
 | relational_expression LE shift_expression  { Infix ($1, (LtE,$2), $3) }
 | relational_expression GE shift_expression  { Infix ($1, (GtE,$2), $3) }
 | relational_expression INSTANCEOF reference_type  { InstanceOf ($1, $3) }

equality_expression:
 | relational_expression  { $1 }
 | equality_expression EQ_EQ relational_expression  { Infix ($1, (Eq,$2), $3) }
 | equality_expression NOT_EQ relational_expression { Infix ($1,(NotEq,$2),$3)}

and_expression:
 | equality_expression  { $1 }
 | and_expression AND equality_expression  { Infix ($1, (BitAnd,$2), $3) }

exclusive_or_expression:
 | and_expression  { $1 }
 | exclusive_or_expression XOR and_expression  { Infix ($1, (BitXor,$2), $3) }

inclusive_or_expression:
 | exclusive_or_expression  { $1 }
 | inclusive_or_expression OR exclusive_or_expression  { Infix ($1, (BitOr,$2), $3) }

conditional_and_expression:
 | inclusive_or_expression  { $1 }
 | conditional_and_expression AND_AND inclusive_or_expression
     { Infix($1,(And,$2),$3) }

conditional_or_expression:
 | conditional_and_expression  { $1 }
 | conditional_or_expression OR_OR conditional_and_expression
     { Infix ($1, (Or, $2), $3) }

(*----------------------------*)
(* Ternary *)
(*----------------------------*)

conditional_expression:
 | conditional_or_expression
     { $1 }
 | conditional_or_expression COND expression ":" conditional_expression
     { Conditional ($1, $3, $5) }
 | conditional_or_expression COND expression ":" lambda_expression
     { Conditional ($1, $3, $5) }

(*----------------------------*)
(* Assign *)
(*----------------------------*)

assignment_expression:
 | conditional_expression  { $1 }
 | assignment              { $1 }
 (* sgrep-ext: *)
 | "..."                    { Flag_parsing.sgrep_guard (Ellipsis $1) }
 | "<..." expression "...>" { Flag_parsing.sgrep_guard (DeepEllipsis ($1,$2,$3))}


(* javaext: was assignment_expression for rhs, but we want lambdas there*)
assignment: left_hand_side assignment_operator expression  { $2 $1 $3 }

left_hand_side:
 | name          { (name $1) }
 | field_access  { $1 }
 | array_access  { $1 }
 (* sgrep-ext: *)
 | typed_metavar { $1 }

assignment_operator:
 | "="          { (fun e1 e2 -> Assign (e1, $1, e2))  }
 | OPERATOR_EQ  { (fun e1 e2 -> AssignOp (e1, $1, e2)) }

(*----------------------------*)
(* Lambdas *)
(*----------------------------*)
lambda_expression: lambda_parameters "->" lambda_body  { Lambda ($1, $2, $3) }

lambda_parameters:
 | IDENTIFIER                          { [mk_param_id $1] }
 | LP_LAMBDA lambda_parameter_list ")" { $2 }
 | LP_LAMBDA ")"                       { [] }

lambda_parameter_list:
 | listc(identifier)   { $1 |> List.map mk_param_id }
 | listc(lambda_param) { $1 }

lambda_param:
 | variable_modifier+ lambda_parameter_type variable_declarator_id
    { ParamClassic (canon_var $1 $2 $3)  }
 |                    lambda_parameter_type variable_declarator_id
    { ParamClassic (canon_var [] $1 $2) }
 | variable_arity_parameter { $1 }

lambda_parameter_type:
 | unann_type { Some $1 }
 | VAR        { None }

unann_type: type_ { $1 }

variable_arity_parameter:
 | variable_modifier+ unann_type "..." identifier
    { ParamSpread ($3, canon_var $1 (Some $2) (IdentDecl $4)) }
 |                    unann_type "..." identifier
    { ParamSpread ($2, canon_var [] (Some $1) (IdentDecl $3)) }

(* no need %prec LOW_PRIORITY_RULE as in parser_js.mly ?*)
lambda_body:
 | expression { Expr ($1, Tok.unsafe_sc) }
 | block      { $1 }

(*----------------------------*)
(* Method reference *)
(*----------------------------*)
(* javaext: ? *)
(* reference_type is inlined because of classic ambiguity with name *)
method_reference:
 | name       "::" identifier
    { (* TODO? probably a type? *)
       MethodRef (Right (class_type $1), $2, None, $3)
    }
 | primary    "::" identifier       { MethodRef (Left $1, $2, None, $3) }
 | array_type "::" identifier       { MethodRef (Right $1, $2, None, $3) }
 | name       "::" NEW
    { MethodRef (Right (class_type $1), $2, None, new_id $3) }
 | array_type "::" NEW              { MethodRef (Right $1, $2, None, new_id $3) }
 | SUPER      "::" identifier       { MethodRef (Left (super $1), $2, None, $3) }
 | name "." SUPER   "::" identifier
   { let e = Dot (fix_name $1, $2, super_ident $3) in
     MethodRef (Left e, $4, None, $5) }

(*----------------------------*)
(* Shortcuts *)
(*----------------------------*)
expression:
 | assignment_expression  { $1 }
 (* javaext: ? *)
 | lambda_expression { $1 }
 (* this can not be put inside cast_expression. See conflicts.txt*)
 | cast_lambda_expression { $1 }

constant_expression: expression  { $1 }

(*************************************************************************)
(* Statements *)
(*************************************************************************)

statement:
 | statement_without_trailing_substatement  { $1 }

 | labeled_statement  { $1 }
 | if_then_statement  { $1 }
 | if_then_else_statement  { $1 }
 | while_statement  { $1 }
 | for_statement  { $1 }
 (* sgrep-ext: *)
 | "..." { Flag_parsing.sgrep_guard (Expr (Ellipsis $1, Tok.sc $1)) }

statement_without_trailing_substatement:
 | block  { $1 }
 | empty_statement  { $1 }
 | expression_statement  { $1 }
 | switch_statement  { $1 }
 | do_statement  { $1 }
 | break_statement  { $1 }
 | continue_statement  { $1 }
 | return_statement  { $1 }
 | synchronized_statement  { $1 }
 | throw_statement  { $1 }
 | try_statement  { $1 }
 (* javaext:  *)
 | ASSERT expression ";"                { Assert ($1, $2, None) }
 | ASSERT expression ":" expression ";" { Assert ($1, $2, Some $4) }

block: "{" block_statement* "}"  { Block ($1, $2, $3) }

block_statement:
 | local_variable_declaration_statement  { $1 }
 | statement          { $1 }
 (* javaext: ? *)
 | class_declaration  { DeclStmt (Class $1) }

local_variable_declaration_statement: local_variable_declaration ";"
  { LocalVarList ($1, $2) }

(* cant factorize with variable_modifier_opt, conflicts otherwise *)
local_variable_declaration: modifiers_opt type_ listc(variable_declarator)
 (* javaext: 1.? actually should be variable_modifiers but conflict *)
     { decls (fun x -> x) $1 $2 $3 }

empty_statement: ";"                            { EmptyStmt $1 }

labeled_statement: identifier ":" statement     { Label ($1, $3) }

expression_statement: statement_expression ";"  { Expr ($1, $2) }

(* pad: good boy java! nice language! *)
statement_expression:
 | assignment  { $1 }
 | pre_increment_expression  { $1 }
 | pre_decrement_expression  { $1 }
 | post_increment_expression  { $1 }
 | post_decrement_expression  { $1 }
 | method_invocation  { $1 }
 | class_instance_creation_expression  { $1 }
 (* sgrep-ext: to allow '$S;' in sgrep *)
 | IDENTIFIER    { Flag_parsing.sgrep_guard (((name [Id $1])))  }
 | typed_metavar { $1 }


if_then_statement: IF "(" expression ")" statement   { If ($1, $3, $5, None) }

if_then_else_statement:
 IF "(" expression ")" statement_no_short_if ELSE statement
   { If ($1, $3, $5, Some $7) }


switch_statement: SWITCH "(" expression ")" switch_block { Switch($1, $3, $5)}

switch_block:
 | "{"                                             "}"  { [] }
 | "{"                               switch_label+ "}"  { [$2, []] }
 | "{" switch_block_statement_groups               "}"  { $2 }
 | "{" switch_block_statement_groups switch_label+ "}"
     { List.rev (($3, []) :: $2) }

switch_block_statement_group: switch_label+ block_statement+
  {$1, $2}

switch_label:
 | CASE constant_expression ":"        { Case ($1, $2) }
 | DEFAULT_COLON ":"                   { Default $1 }


while_statement: WHILE "(" expression ")" statement     { While ($1, $3, $5) }

do_statement: DO statement WHILE "(" expression ")" ";" { Do ($1, $2, $5) }

(*----------------------------*)
(* For *)
(*----------------------------*)

for_statement: FOR "(" for_control ")" statement { For ($1, $3, $5) }

for_control:
 | for_init_opt ";" expression? ";" optl(for_update)
     { ForClassic ($1, Common2.option_to_list $3, $5) }
 (* javeext: ? *)
 | for_var_control
     { let (a, b) = $1 in Foreach (a, b) }
 (* sgrep-ext: *)
 | "..." { Flag_parsing.sgrep_guard (ForEllipsis $1) }

for_init_opt:
 | (*empty*)  { ForInitExprs [] }
 | for_init   { $1 }

for_init:
| listc(statement_expression)   { ForInitExprs $1 }
| local_variable_declaration    { ForInitVars $1 }

for_update: listc(statement_expression)  { $1 }

for_var_control:
 modifiers_opt type_ variable_declarator_id for_var_control_rest
  (* actually only FINAL is valid here, but cant because get shift/reduce
   * conflict otherwise because for_init can be a local_variable_decl
   *)
     { canon_var $1 (Some $2) $3, $4 }

for_var_control_rest: ":" expression { $2 }

(*----------------------------*)
(* Other *)
(*----------------------------*)

break_statement:    BREAK    identifier? ";"  { Break ($1, $2) }
continue_statement: CONTINUE identifier? ";"  { Continue ($1, $2) }
return_statement:   RETURN expression? ";"  { Return ($1, $2) }

synchronized_statement: SYNCHRONIZED "(" expression ")" block { Sync ($1, $3, $5) }

(*----------------------------*)
(* Exceptions *)
(*----------------------------*)

throw_statement: THROW expression ";"  { Throw ($1, $2) }

try_statement:
 | TRY block catch_clause+          { Try ($1, None, $2, $3, None) }
 | TRY block catch_clause* finally  { Try ($1, None, $2, $3, Some $4) }
 (* javaext: ? *)
 | TRY resource_specification block catch_clause* finally?
    { Try ($1, Some $2, $3, $4, $5) }

finally: FINALLY block  { $1, $2 }

catch_clause:
 | CATCH "(" catch_formal_parameter ")" block            { $1, $3, $5 }
 (* javaext: not in 2nd edition java language specification.*)
 | CATCH "(" catch_formal_parameter ")" empty_statement  { $1, $3, $5 }

(* javaext: ? was just formal_parameter before *)
catch_formal_parameter:
  | variable_modifier+ catch_type variable_declarator_id
      { CatchParam (canon_var $1 (Some (fst $2)) $3, snd $2) }
  |                    catch_type variable_declarator_id
      { CatchParam (canon_var [] (Some (fst $1)) $2, snd $1) }
  (* sgrep-ext: *)
  | "..." { CatchEllipsis $1 }

(* javaext: ? *)
catch_type: list_sep(type_, OR) { List_.hd_exn "unexpected empty list" $1, List_.tl_exn "unexpected empty list" $1 }

(* javaext: ? *)
resource_specification: "(" list_sep(resource, ";") ";"? ")"
  { $1, $2, $4 }

resource:
 | variable_modifier+ local_variable_type identifier "=" expression
    { let var = canon_var $1 (Some $2) (IdentDecl $3) in
      Left { f_var = var; f_init = Some (ExprInit $5) }
    }
 |                    local_variable_type identifier "=" expression
    { let var = canon_var [] (Some $1) (IdentDecl $2) in
      Left { f_var = var; f_init = Some (ExprInit $4) }
    }
 | variable_access
    { Right $1 }

local_variable_type: unann_type { $1 }

variable_access:
 | field_access { $1 }
 | name         { fix_name $1 }

(*----------------------------*)
(* No short if *)
(*----------------------------*)

statement_no_short_if:
 | statement_without_trailing_substatement  { $1 }
 | labeled_statement_no_short_if            { $1 }
 | if_then_else_statement_no_short_if       { $1 }
 | while_statement_no_short_if              { $1 }
 | for_statement_no_short_if                { $1 }

labeled_statement_no_short_if: identifier ":" statement_no_short_if
   { Label ($1, $3) }

if_then_else_statement_no_short_if:
 IF "(" expression ")" statement_no_short_if ELSE statement_no_short_if
   { If ($1, $3, $5, Some $7) }

while_statement_no_short_if: WHILE "(" expression ")" statement_no_short_if
     { While ($1, $3, $5) }

for_statement_no_short_if:
  FOR "(" for_control ")" statement_no_short_if
	{ For ($1, $3, $5) }

(*************************************************************************)
(* Modifiers *)
(*************************************************************************)

(* To avoid shift/reduce conflicts, we accept all modifiers
 * in front of all declarations. The ones not applicable to
 * a particular kind of declaration must be detected in semantic actions.
 *)
modifier:
 | PUBLIC       { Public, $1 }
 | PROTECTED    { Protected, $1 }
 | PRIVATE      { Private, $1 }

 | ABSTRACT     { Abstract, $1 }
 | STATIC       { Static, $1 }
 | FINAL        { Final, $1 }

 | STRICTFP     { StrictFP, $1 }
 | TRANSIENT    { Transient, $1 }
 | VOLATILE     { Volatile, $1 }
 | SYNCHRONIZED { Synchronized, $1 }
 | NATIVE       { Native, $1 }

 | DEFAULT      { DefaultModifier, $1 }

 | annotation { Annotation $1, (Common2.fst3 $1) }

(*************************************************************************)
(* Annotation *)
(*************************************************************************)

annotation:
 | "@" qualified_ident { ($1, $2, None) }
 | "@" qualified_ident "(" annotation_element ")" { ($1, $2, Some ($3, $4, $5)) }

annotation_element:
 | (* empty *) { EmptyAnnotArg }
 | element_value { AnnotArgValue $1 }
 | listc(element_value_pair) { AnnotArgPairInit $1 }

element_value:
 | expr1      { AnnotExprInit $1 }
 | annotation { AnnotNestedAnnot $1 }
 | element_value_array_initializer { AnnotArrayInit $1 }

element_value_or_dots:
 | element_value { $1 }
 | "..." { Flag_parsing.sgrep_guard (AnnotExprInit (Ellipsis $1)) }

element_value_pair:
 | identifier "=" element_value { AnnotPair ($1, $3) }
 | "..." { Flag_parsing.sgrep_guard (AnnotPairEllipsis $1) }

element_value_array_initializer:
 | "{" "}" { ($1, [], $2) }
 | "{" listc(element_value_or_dots) ","? "}" { $1, $2, $4 }

(* should be statically a constant expression; can contain '+', '*', etc.*)
expr1: conditional_expression { $1 }

(*************************************************************************)
(* Class *)
(*************************************************************************)

class_declaration: class_header class_body
  { { $1 with cl_body = $2 }  }

class_header:
 modifiers_opt CLASS identifier optl(type_parameters) super? optl(interfaces)
  { { cl_name = $3; cl_kind = (ClassRegular, $2);
      cl_mods = $1; cl_tparams = $4;
      cl_extends = $5;  cl_impls = $6; cl_formals = [];
      cl_body = empty_body ;
     } }

super: EXTENDS type_ (* was class_type *)  { $2 }

(* was interface_type_list *)
interfaces: IMPLEMENTS listc(reference_type)   { $2 }

class_body: "{" class_body_declaration* "}"  { $1, List.flatten $2, $3 }

(*----------------------------*)
(* Class body *)
(*----------------------------*)

class_body_declaration:
 | class_member_declaration  { $1 }
 | constructor_declaration  { [$1] }
 | static_initializer  { [$1] }
 (* javaext: 1.? *)
 | instance_initializer  { [$1] }

class_member_declaration:
 | field_declaration  { $1 }
 | method_declaration  { [Method $1] }

 (* javaext: 1.? *)
 | generic_method_or_constructor_decl { [Method $1] }
 | class_and_co_declaration  { [$1] }
 | ";"  { [] }
 (* sgrep-ext: allows ... inside class body *)
 | "..." { [DeclEllipsis $1] }


static_initializer: STATIC block  { Init (Some $1, $2) }

instance_initializer: block       { Init (None, $1) }

(*----------------------------*)
(* Field *)
(*----------------------------*)

field_declaration: modifiers_opt type_ listc(variable_declarator) ";"
   { decls (fun x -> Field x) $1 $2 $3 }

variable_declarator:
 | variable_declarator_id  { $1, None }
 | variable_declarator_id "=" variable_initializer  { $1, Some $3 }

variable_declarator_id:
 | identifier                    { IdentDecl $1 }
 | variable_declarator_id LB_RB  { ArrayDecl $1 }

variable_initializer:
 | expression         { ExprInit $1 }
 | array_initializer  { $1 }

array_initializer:
 | "{" ","? "}"                        { ArrayInit ($1, [], $3) }
 | "{" listc(variable_initializer) ","? "}"  { ArrayInit ($1, $2, $4) }

(*----------------------------*)
(* Method *)
(*----------------------------*)

method_declaration: method_header method_body  { { $1 with m_body = $2 } }

method_header:
 | modifiers_opt type_ method_declarator optl(throws)
     { method_header $1 $2 $3 $4 }
 | modifiers_opt VOID method_declarator optl(throws)
     { method_header $1 (void_type $2) $3 $4 }

method_declarator:
 | identifier "(" listc0(formal_parameter) ")"  { (IdentDecl $1), $3 }
 | method_declarator LB_RB                      { (ArrayDecl (fst $1)), snd $1}

method_body:
 | block   { $1 }
 | ";"     { EmptyStmt $1 }

throws: THROWS listc(name) (* was class_type_list *)
  { List.map (fun x -> typ_of_qualified_id (qualified_ident x)) $2 }

generic_method_or_constructor_decl:
|  modifiers_opt type_parameters type_
   identifier formal_parameters optl(throws) method_body
    { let (t, mdecl, throws, body) = $3, (IdentDecl $4, $5), $6, $7 in
      let header = method_header $1 (* TODO $2 *) t mdecl throws in
      { header with m_body = body }
    }
|  modifiers_opt type_parameters VOID
   identifier formal_parameters optl(throws) method_body
   { let (t, mdecl, throws, body) = void_type $3, (IdentDecl $4, $5), $6, $7 in
      let header = method_header $1 (* TODO $2 *) t mdecl throws in
      { header with m_body = body }
    }

(*----------------------------*)
(* Constructors *)
(*----------------------------*)

constructor_declaration:
 modifiers_opt constructor_declarator optl(throws) constructor_body
  { let (id, formals) = $2 in
    let var = { mods = $1; type_ = None; name = id } in
    Method { m_var = var; m_formals = formals; m_throws = $3;
	     m_body = $4 }
  }

constructor_declarator: identifier "(" listc0(formal_parameter) ")" { $1, $3}

constructor_body:
 | "{" block_statement* "}"
    { Block ($1, $2, $3) }
 | "{" explicit_constructor_invocation_stmt block_statement* "}"
    { Block ($1, $2::$3, $4) }


explicit_constructor_invocation_stmt:
 explicit_constructor_invocation ";" { Expr ($1, $2) }

explicit_constructor_invocation:
 | THIS "(" listc0(argument) ")"
      { Call (This $1, ($2,$3,$4)) }
 | SUPER "(" listc0(argument) ")"
      { Call (NameId (super_ident $1), ($2,$3,$4)) }
 (* javaext: ? *)
 | primary "." SUPER "(" listc0(argument) ")"
      { Call ((Dot ($1, $2, super_ident $3)), ($4,$5,$6)) }
 (* not in 2nd edition java language specification. *)
 | name "." SUPER "(" listc0(argument) ")"
      { Call (Dot (name $1, $2, super_ident $3), ($4,$5,$6)) }

(*----------------------------*)
(* Method parameter *)
(*----------------------------*)

formal_parameters: "(" listc0(formal_parameter) ")" { $2 }

formal_parameter:
 | variable_modifiers type_ variable_declarator_id
  { ParamClassic (canon_var $1 (Some $2) $3) }
 (* javaext: 1.? *)
 | variable_modifiers type_ "..." variable_declarator_id
  { ParamSpread ($3, canon_var $1 (Some $2) $4) }
 (* sgrep-ext: *)
 | "..." { ParamEllipsis $1 }
 | IDENTIFIER
   { Flag_parsing.sgrep_guard
       (ParamClassic { name = $1; mods = []; type_ = None }) }
 | METAVAR_ELLIPSIS
   { Flag_parsing.sgrep_guard
       (ParamClassic { name = $1; mods = []; type_ = None }) }

(* conflicts: this is equivalent to variable_modifier*, but then
 * we get a s/r conflict with IDENTIFIER because reading an IDENTIFIER
 * menhir does not know whether to shift or to reduce variable_modifier*
 * as type_ can begin witn an IDENTIFIER, so expanding and inline
 * variable_modifier* helps.
 *)
%inline
variable_modifiers:
 | (* empty *) { [] }
 | variable_modifier variable_modifier* { $1::$2}

 (* javaext: 1.? *)
variable_modifier:
 | FINAL      { Final, $1 }
 | annotation { (Annotation $1), Common2.fst3 $1 }

(*************************************************************************)
(* Interface *)
(*************************************************************************)

interface_declaration:
 modifiers_opt INTERFACE identifier
 optl(type_parameters) optl(extends_interfaces)
 interface_body
  { { cl_name = $3; cl_kind = (Interface, $2);
      cl_mods = $1; cl_tparams = $4;
      cl_extends = None; cl_impls = $5; cl_formals = [];
      cl_body = $6;
    } }

extends_interfaces:
 | EXTENDS reference_type (* was interface_type *) { [$2] }
 | extends_interfaces "," reference_type           { $1 @ [$3] }

(*----------------------------*)
(* Interface body *)
(*----------------------------*)

interface_body: "{" interface_member_declaration* "}" { $1, List.flatten $2,$3}

interface_member_declaration:
 | constant_declaration  { $1 }
 (* javaext: was abstract_method_declaration *)
 | interface_method_declaration  { [Method $1] }

 (* javaext: 1.? *)
 | interface_generic_method_decl { [Method $1] }

 | class_and_co_declaration { [$1] }
 | ";"  { [] }
 (* sgrep-ext: allows ... inside interface body *)
 | "..." { [DeclEllipsis $1] }


(* note: semicolon is missing in 2nd edition java language specification.*)
(* less: could replace with field_declaration? was field_declaration *)
constant_declaration: modifiers_opt type_ listc(variable_declarator) ";"
    { decls (fun x -> Field x) $1 $2 $3 }

(* javaext:: was abstract_method_declaration only before *)
interface_method_declaration: method_declaration { $1 }

interface_generic_method_decl:
| modifiers_opt type_parameters type_
  identifier formal_parameters optl(throws) ";"
    { let (t, mdecl, throws) = $3, (IdentDecl $4, $5), $6 in
      method_header $1 (* TODO $2 *) t mdecl throws
    }
| modifiers_opt type_parameters VOID
  identifier formal_parameters optl(throws) ";"
    { let (t, mdecl, throws) = void_type $3, (IdentDecl $4, $5), $6 in
      method_header $1 (* TODO $2 *) t mdecl throws
    }

(*************************************************************************)
(* Enum *)
(*************************************************************************)

enum_declaration: modifiers_opt ENUM identifier optl(interfaces) enum_body
   { { en_name = $3; en_mods = $1; en_impls = $4; en_body = $5; } }

(* cant factorize in enum_constants_opt comma_opt .... *)
enum_body:
 | "{"                   optl(enum_body_declarations) "}" { [], $2 }
 | "{" listc(enum_constant)    optl(enum_body_declarations) "}" { $2, $3 }
 | "{" listc(enum_constant) "," optl(enum_body_declarations) "}" { $2, $4 }

enum_constant: modifiers_opt enum_constant_bis { $2 }

enum_constant_bis:
 | identifier                         { $1, None, None }
 | identifier "(" listc0(argument) ")" { $1, Some ($2,$3,$4), None }
 | identifier "{" method_declaration* "}"
    { $1, None, Some ($2, $3 |> List.map (fun x -> Method x) , $4) }

enum_body_declarations:
 | ";" class_body_declaration* { List.flatten $2 }
 | "..." { [DeclEllipsis $1] }

(*************************************************************************)
(* Annotation type decl *)
(*************************************************************************)

annotation_type_declaration:
  modifiers_opt "@" INTERFACE identifier annotation_type_body
     { { cl_name = $4; cl_kind = (AtInterface, $2); cl_mods = $1; cl_tparams = [];
         cl_extends = None; cl_impls = []; cl_formals = [];
         cl_body = $5
       } }

annotation_type_body: "{" annotation_type_element_declaration* "}"
  { $1, $2, $3 }

annotation_type_element_declaration: annotation_type_element_rest { $1 }

annotation_type_element_rest:
 | modifiers_opt type_ identifier annotation_method_or_constant_rest ";"
   { AnnotationTypeElementTodo (snd $3) }
 | class_and_co_declaration    { $1 }
 (* sgrep-ext: allows ... inside @interface body *)
 | "..." { DeclEllipsis $1 }

annotation_method_or_constant_rest:
 | "(" ")"                       { None }
 | "(" ")" DEFAULT element_value { Some ($3) }

(*************************************************************************)
(* xxx_list, xxx_opt *)
(*************************************************************************)

(* can't use modifier*, need %inline and separate modifiers rule *)
%inline
modifiers_opt:
 | (*empty*)  { [] }
 | modifiers  { List.rev $1 }

modifiers:
 | modifier  { [$1] }
 | modifiers modifier  { $2 :: $1 }

(* basic lists, at least one element *)

switch_block_statement_groups:
 | switch_block_statement_group  { [$1] }
 | switch_block_statement_groups switch_block_statement_group  { $2 :: $1 }

dims_opt:
 | (*empty*)  { 0 }
 | dims  { $1 }

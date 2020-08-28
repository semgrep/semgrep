%{
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
open Common

open Cst_cpp
open Parser_cpp_mly_helper

module PI = Parse_info

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* This file contains a grammar for C/C++/Cpp.
 * See cst_cpp.ml for more information.
 *
 * reference:
 *  - orig_c.mly and orig_cpp.mly in this directory
 *  - http://www.nongnu.org/hcb/ for an up-to-date hyperlinked C++ grammar
 *
 * TODO: 
 *  - http://www.externsoft.ch/download/cpp-iso.html
 *  - tree-sitter-cpp grammar
 *)
%}

(*************************************************************************)
(* Tokens *)
(*************************************************************************)
(* Some tokens below are not even used in this file because they are filtered
 * in some intermediate phases (e.g. the comment tokens). Some tokens
 * also appear only here and are not in the lexer because they are
 * created in some intermediate phases. They are called "fresh" tokens
 * and always contain a '_' in their name.
 *)

(* unrecognized token (use for error recovery during lexing) *)
%token <Parse_info.t> TUnknown

%token <Parse_info.t> EOF

(*-----------------------------------------*)
(* The space/comment tokens *)
(*-----------------------------------------*)
(* coupling: Token_helpers.is_real_comment and other related functions.
 * disappear in parse_cpp.ml via TH.is_comment in lexer_function *)
%token <Parse_info.t> TCommentSpace TCommentNewline TComment

(* fresh_token: cppext: appears after parsing_hack_pp and disappear *)
%token <(Token_cpp.cppcommentkind * Parse_info.t)> TComment_Pp
(* fresh_token: c++ext: appears after parsing_hack_pp and disappear *)
%token <(Token_cpp.cpluspluscommentkind * Parse_info.t)> TComment_Cpp

(*-----------------------------------------*)
(* The C tokens *)
(*-----------------------------------------*)
%token <string * Parse_info.t>                       TInt
%token <(string * Parse_info.t) * Cst_cpp.floatType> TFloat
%token <(string * Parse_info.t) * Cst_cpp.isWchar>   TChar TString

%token <string * Parse_info.t> TIdent 
(* fresh_token: appear after some fix_tokens in parsing_hack.ml *)
%token <string * Parse_info.t> TIdent_Typedef

(* coupling: some tokens like TOPar and TCPar are used as synchronisation  
 * point in parsing_hack.ml. So if you define a special token like
 * TOParDefine and TCParEOL, then you must take care to also modify 
 * token_helpers.ml
 *)
%token <Parse_info.t> TOPar "(" TCPar ")" 
%token <Parse_info.t> TOBrace "{" TCBrace "}" 
%token <Parse_info.t> TOCro "[" TCCro "]"

%token <Parse_info.t> TDot TPtrOp     TInc TDec
%token <Parse_info.t> TComma "," TPtVirg ";"
%token <Cst_cpp.assignOp> TAssign 
%token <Parse_info.t> TEq "=" TWhy "?"  TTilde TBang TCol ":" TEllipsis "..."
%token <Parse_info.t> 
  TOrLog TAndLog "&&" TOr TXor TAnd "&"  TEqEq TNotEq TInfEq TSupEq
  TShl TShr 
  TPlus TMinus TMul "*" TDiv TMod 

(*c++ext: see also TInf2 and TSup2 *)
%token <Parse_info.t> TInf TSup 

%token <Parse_info.t>
  Tchar Tshort Tint Tdouble Tfloat Tlong Tunsigned Tsigned Tvoid
  Tauto Tregister Textern Tstatic 
  Ttypedef 
  Tconst Tvolatile
  Tstruct Tunion Tenum 
  Tbreak Telse Tswitch Tcase Tcontinue Tfor Tdo Tif  Twhile Treturn
  Tgoto Tdefault
  Tsizeof  

(* C99 *)
%token <Parse_info.t> Trestrict

(*-----------------------------------------*)
(* gccext: extra tokens *)
(*-----------------------------------------*)
%token <Parse_info.t> Tasm Ttypeof
(* less: disappear in parsing_hacks_pp, not present in AST for now *)
%token <Parse_info.t> Tattribute
(* also c++ext: *)
%token <Parse_info.t> Tinline 

(*-----------------------------------------*)
(* cppext: extra tokens *)
(*-----------------------------------------*)

(* cppext: #define  *)
%token <Parse_info.t> TDefine
%token <(string * Parse_info.t)> TDefParamVariadic
(* transformed in TCommentSpace and disappear in parsing_hack.ml *)
%token <Parse_info.t> TCppEscapedNewline 
(* fresh_token: appear after fix_tokens_define in parsing_hack_define.ml *)
%token <(string * Parse_info.t)> TIdent_Define
%token <Parse_info.t> TOPar_Define
%token <Parse_info.t> TCommentNewline_DefineEndOfMacro
%token <Parse_info.t> TOBrace_DefineInit

(* cppext: #include  *)
%token <(string * string * Parse_info.t)> TInclude

(* cppext: #ifdef *)
(* coupling: Token_helpers.is_cpp_instruction *)
%token <Parse_info.t>          TIfdef TIfdefelse TIfdefelif TEndif
%token <(bool * Parse_info.t)> TIfdefBool TIfdefMisc TIfdefVersion

(* cppext: other *)
%token <string * Parse_info.t> TUndef
%token <Parse_info.t> TCppDirectiveOther

(* cppext: special macros *)
(* fresh_token: appear after fix_tokens in parsing_hacks_pp.ml *)
%token <Parse_info.t>            TIdent_MacroStmt
%token <Parse_info.t>            TIdent_MacroString 
%token <(string * Parse_info.t)> TIdent_MacroIterator
%token <(string * Parse_info.t)> TIdent_MacroDecl
%token <Parse_info.t>            Tconst_MacroDeclConst 

(* fresh_token: appear after parsing_hack_pp.ml, alt to TIdent_MacroTop *)
%token <Parse_info.t> TCPar_EOL
(* fresh_token: appear after parsing_hack_pp.ml *)
%token <Parse_info.t> TAny_Action

(*-----------------------------------------*)
(* c++ext: extra tokens *)
(*-----------------------------------------*)
%token <Parse_info.t>
   Tclass Tthis 
   Tnew Tdelete 
   Ttemplate Ttypeid Ttypename 
   Tcatch Ttry Tthrow 
   Toperator 
   Tpublic Tprivate Tprotected    Tfriend 
   Tvirtual 
   Tnamespace Tusing 
   Tbool    Tfalse Ttrue 
   Twchar_t 
   Tconst_cast Tdynamic_cast Tstatic_cast Treinterpret_cast 
   Texplicit Tmutable
%token <Parse_info.t> TPtrOpStar TDotStar

%token <Parse_info.t> TColCol "::"

(* fresh_token: for constructed object, in parsing_hacks_cpp.ml *)
%token <Parse_info.t> TOPar_CplusplusInit
(* fresh_token: for template *)
%token <Parse_info.t> TInf_Template TSup_Template
(* fresh_token: for new[] delete[] *)
%token <Parse_info.t> TOCro_new TCCro_new
(* fresh_token: for pure virtual method. TODO add stuff in parsing_hack *)
%token <Parse_info.t> TInt_ZeroVirtual
(* fresh_token: why can't use TypedefIdent? conflict? *)
%token <string * Parse_info.t> TIdent_ClassnameInQualifier
(* fresh_token: appears after solved if next token is a typedef *)
%token <string * Parse_info.t> TIdent_ClassnameInQualifier_BeforeTypedef
(* fresh_token: just before <> *)
%token <string * Parse_info.t> TIdent_Templatename
(* for templatename as qualifier, before a '::' TODO write heuristic! *)
%token <string * Parse_info.t> TIdent_TemplatenameInQualifier
(* fresh_token: appears after solved if next token is a typedef *)
%token <string * Parse_info.t> TIdent_TemplatenameInQualifier_BeforeTypedef
(* fresh_token: for methods with same name as classname *)
%token <string * Parse_info.t> TIdent_Constructor
(* for cast_constructor, before a '(', unused for now *)
%token <string * Parse_info.t> TIdent_TypedefConstr
(* fresh_token: for constructed (basic) objects *)
%token <Parse_info.t> 
  Tchar_Constr Tint_Constr Tfloat_Constr Tdouble_Constr Twchar_t_Constr
  Tshort_Constr Tlong_Constr Tbool_Constr
  Tsigned_Constr Tunsigned_Constr
(* fresh_token: appears after solved if next token is a typedef *)
%token <Parse_info.t> TColCol_BeforeTypedef

(*-----------------------------------------*)
(* c++0x: extra tokens *)
(*-----------------------------------------*)
%token <Parse_info.t>
   Tnullptr
   Tconstexpr
   Tthread_local
   Tdecltype

(* fresh_token: for new[] delete[] *)
%token <Parse_info.t> TOCro_Lambda

(*************************************************************************)
(* Priorities *)
(*************************************************************************)
(* must be at the top so that it has the lowest priority *)
%nonassoc LOW_PRIORITY_RULE
(* see conflicts.txt *)
%nonassoc Telse

(* TODO: remove those too but conflicts *)
%left TAnd 
%left TMul
%left TAndLog

(*************************************************************************)
(* Rules type declaration *)
(*************************************************************************)
%start <Cst_cpp.program> main 
%start <Cst_cpp.toplevel option> toplevel
%start <Cst_cpp.any> sgrep_spatch_pattern

%%
(*************************************************************************)
(* TOC *)
(*************************************************************************)
(* translation_unit (obsolete)
 * toplevel
 * sgrep
 * 
 * ident
 * expression
 * statement
 * types with 
 *   - left part (type_spec, qualif, template and its arguments), 
 *   - right part (declarator, abstract declarator)
 *   - aux part (parameters)
 * class/struct
 * enum
 * simple declaration (and storage, initializers)
 * block_declaration (and namespace, using, asm)
 * declaration (and template, namespace again)
 * function definition
 * cpp directives
 *)

(*************************************************************************)
(* Macros *)
(*************************************************************************)

listc(X):
 | X { [$1, []] }
 | listc(X) "," X { $1 @ [$3, [$2]] }

optl(X):
 | (* empty *) { [] }
 | X           { $1 }

(*************************************************************************)
(* translation_unit (unused) *)
(*************************************************************************)

(* no more used now that use error recovery, but good to keep *)
main: translation_unit EOF     { $1 }

translation_unit: external_declaration+ { $1 }

external_declaration: 
 | function_definition            { DeclElem (Func (FunctionOrMethod $1)) }
 | block_declaration              { DeclElem (BlockDecl $1) }

(*************************************************************************)
(* toplevel *)
(*************************************************************************)

toplevel: 
 | toplevel_aux { Some $1 }
 | EOF          { None }

toplevel_aux:
 | declaration         { DeclElem $1 }
 (* cppext: *)
 | cpp_directive       { CppDirectiveDecl $1 }
 | cpp_ifdef_directive (*external_declaration_list ...*) { IfdefDecl $1 }
 | cpp_other           { $1 }
 (* when have error recovery, we can end up skipping the
  * beginning of the file, and so get trailing unclosed } at the end *)
 | "}" { DeclElem (EmptyDef $1) }

(*************************************************************************)
(* sgrep *)
(*************************************************************************)

sgrep_spatch_pattern:
 | expr      EOF { Expr $1 }
 | statement EOF { Stmt $1 }
 | statement statement+ EOF { Stmts ($1::$2) }

(*************************************************************************)
(* Ident, scope *)
(*************************************************************************)

id_expression:
 | unqualified_id { noQscope, $1 }
 | qualified_id { $1 }

(* todo:
 * ~id class_name,  conflict  IdDestructor
 * template-id,   conflict
 *)
unqualified_id:
 | TIdent                 { IdIdent $1 }
 | operator_function_id   { $1 }
 | conversion_function_id { $1 }

operator_function_id: Toperator operator_kind { IdOperator ($1, $2) }

conversion_function_id: Toperator conversion_type_id { IdConverter ($1, $2) }

(* no deref getref operator (cos ambiguity with Mul and And)
 * no unaryplus/minus op either *)
operator_kind:
 (* != == *)
 | TEqEq  { BinaryOp (Logical Eq),    [$1] }
 | TNotEq { BinaryOp (Logical NotEq), [$1] }
 (* =    +=   -=   *=   /=   %=       ^=   &=   |=   >>=  <<=   *)
 | "="     { AssignOp (SimpleAssign $1), noii }     
 | TAssign { AssignOp ($1), noii } 
 (* ! ~ *)
 | TTilde { UnaryTildeOp, [$1] } | TBang { UnaryNotOp,   [$1] }
 (* , *)
 | "," { CommaOp,  [$1] }
 (* +    -    *    /    %  *)
 | TPlus { BinaryOp (Arith Plus),  [$1] } 
 | TMinus { BinaryOp (Arith Minus), [$1] }
 | "*"   { BinaryOp (Arith Mul),   [$1] }  
 | TDiv { BinaryOp (Arith Div), [$1] } | TMod { BinaryOp (Arith Mod),[$1] }
 (* ^ & |     <<   >>  *)
 | TOr { BinaryOp (Arith Or),  [$1] } | TXor { BinaryOp (Arith Xor), [$1] } 
 | "&"  { BinaryOp (Arith And), [$1]  } 
 | TShl { BinaryOp (Arith DecLeft), [$1] }
 | TShr { BinaryOp (Arith DecRight), [$1] }
 (* &&   || *)
 | TOrLog  { BinaryOp (Logical OrLog), [$1] } 
 | "&&" { BinaryOp (Logical AndLog), [$1] }
 (* < >  <=   >=  *)
 | TInf { BinaryOp (Logical Inf), [$1] } | TSup { BinaryOp (Logical Sup), [$1]}
 | TInfEq { BinaryOp (Logical InfEq), [$1] }   
 | TSupEq { BinaryOp (Logical SupEq), [$1] }
 (* ++   -- *)
 | TInc { FixOp Inc, [$1] } | TDec { FixOp Dec, [$1] }
 (* ->*  -> *)
 | TPtrOpStar { PtrOpOp PtrStarOp, [$1] } | TPtrOp { PtrOpOp PtrOp,     [$1] }
 (* () [] (double tokens) *)
 | "(" ")" { AccessOp ParenOp, [$1;$2] } | "[" "]" { AccessOp ArrayOp, [$1;$2]}
 (* new delete *)
 | Tnew    { AllocOp NewOp,    [$1] } | Tdelete { AllocOp DeleteOp, [$1] }
 (*new[] delete[] (tripple tokens) *)
 | Tnew    TOCro_new TCCro_new { AllocOp NewArrayOp,    [$1;$2;$3] }
 | Tdelete TOCro_new TCCro_new { AllocOp DeleteArrayOp, [$1;$2;$3] }


qualified_id: 
 | nested_name_specifier (*templateopt*) unqualified_id 
   { $1, $2 }

nested_name_specifier: 
 | class_or_namespace_name_for_qualifier "::" optl(nested_name_specifier)
   { ($1, $2)::$3 }

(* context dependent *)
class_or_namespace_name_for_qualifier:
 | TIdent_ClassnameInQualifier 
     { QClassname $1 }
 | TIdent_TemplatenameInQualifier 
     TInf_Template listc(template_argument) TSup_Template
     { QTemplateId ($1, ($2, $3, $4)) }


(* context dependent: in the original grammar there was one rule
 * for each names (e.g. typedef_name:, enum_name:, class_name:) but 
 * we don't have such contextual information and we can merge 
 * those rules anyway without introducing conflicts.
 *)
enum_name_or_typedef_name_or_simple_class_name: TIdent_Typedef { $1 }

(* used only with namespace/using rules. We use Tclassname for stuff
 * like std::... todo? or just TIdent_Typedef? *)
%inline
namespace_name: TIdent { $1 }

(*----------------------------*)
(* workarounds *)
(*----------------------------*)
nested_name_specifier2: 
 | class_or_namespace_name_for_qualifier2 
    TColCol_BeforeTypedef optl(nested_name_specifier2)
     { ($1, $2)::$3 }

class_or_namespace_name_for_qualifier2:
 | TIdent_ClassnameInQualifier_BeforeTypedef 
     { QClassname $1  }
 | TIdent_TemplatenameInQualifier_BeforeTypedef 
    TInf_Template listc(template_argument) TSup_Template
     { QTemplateId ($1, ($2, $3, $4)) }

(* Why this ? Why not s/ident/TIdent ? cos there is multiple namespaces in C, 
 * so a label can have the same name that a typedef, same for field and tags
 * hence sometimes the use of ident instead of TIdent. *)
ident: 
 | TIdent         { $1 }
 | TIdent_Typedef { $1 }

(*************************************************************************)
(* Expressions *)
(*************************************************************************)

expr: 
 | assign_expr             { $1 }
 | expr "," assign_expr { Sequence ($1, $2, $3) }

(* bugfix: in the C grammar they put 'unary_expr', but in fact it must be 
 * 'cast_expr', otherwise (int * ) xxx = &yy; is not allowed *)
assign_expr: 
 | cond_expr                     { $1 }
 | cast_expr TAssign assign_expr { Assign ($1, $2,$3)}
 | cast_expr "="     assign_expr { Assign ($1,SimpleAssign $2,$3)}
 (* c++ext: *)
 | Tthrow assign_expr?        { Throw ($1, $2) }

(* gccext: allow optional then part hence expr? 
 * bugfix: in C grammar they put '":" cond_expr', but in fact it must be
 * 'assign_expr', otherwise   pnp ? x : x = 0x388  is not allowed
 *)
cond_expr: 
 | logical_or_expr   { $1 }
 | logical_or_expr "?" expr? ":" assign_expr { CondExpr ($1,$2, $3, $4, $5)} 

(* old: was in single arith_expr rule with %left prio, but dypgen cant *)
multiplicative_expr:
 | pm_expr { $1 }
 | multiplicative_expr "*"  pm_expr { Binary($1,(Arith Mul,$2),$3) }
 | multiplicative_expr TDiv pm_expr { Binary($1,(Arith Div,$2),$3) }
 | multiplicative_expr TMod pm_expr { Binary($1,(Arith Mod,$2),$3) }

additive_expr:
 | multiplicative_expr { $1 }
 | additive_expr TPlus  multiplicative_expr { Binary($1,(Arith Plus,$2),$3) }
 | additive_expr TMinus multiplicative_expr { Binary($1,(Arith Minus,$2),$3) }

shift_expr:
 | additive_expr { $1 }
 | shift_expr TShl additive_expr { Binary($1,(Arith DecLeft,$2),$3) }
 | shift_expr TShr additive_expr { Binary($1,(Arith DecRight,$2),$3) }

relational_expr:
 | shift_expr { $1 }
 | relational_expr TInf shift_expr { Binary($1,(Logical Inf,$2),$3) }
 | relational_expr TSup shift_expr { Binary($1,(Logical Sup,$2),$3) }
 | relational_expr TInfEq shift_expr { Binary($1,(Logical InfEq,$2),$3) }
 | relational_expr TSupEq shift_expr { Binary($1,(Logical SupEq,$2),$3) }

equality_expr:
 | relational_expr { $1 }
 | equality_expr TEqEq relational_expr  { Binary($1,(Logical Eq,$2),$3) }
 | equality_expr TNotEq relational_expr { Binary($1,(Logical NotEq,$2),$3) }

and_expr:
 | equality_expr { $1 }
 | and_expr "&" equality_expr { Binary($1,(Arith And,$2),$3) }

exclusive_or_expr:
 | and_expr { $1 }
 | exclusive_or_expr TXor and_expr { Binary($1,(Arith Xor,$2),$3) }

inclusive_or_expr:
 | exclusive_or_expr { $1 }
 | inclusive_or_expr TOr exclusive_or_expr { Binary($1,(Arith Or,$2),$3) }

logical_and_expr:
 | inclusive_or_expr { $1 }
 | logical_and_expr "&&" inclusive_or_expr { Binary($1,(Logical AndLog,$2),$3) }

logical_or_expr:
 | logical_and_expr { $1 }
 | logical_or_expr TOrLog logical_and_expr { Binary($1,(Logical OrLog,$2),$3) }



pm_expr: 
 | cast_expr { $1 }
 (*c++ext: .* and ->*, note that not next to . and -> and take expr *)
 | pm_expr TDotStar   cast_expr   { RecordStarAccess   ($1, $2,$3) }
 | pm_expr TPtrOpStar cast_expr   { RecordPtStarAccess ($1, $2, $3) }

cast_expr: 
 | unary_expr                { $1 }
 | "(" type_id ")" cast_expr { Cast (($1, $2, $3), $4) }

unary_expr: 
 | postfix_expr            { $1 }
 | TInc unary_expr         { Infix ($2, (Inc, $1)) }
 | TDec unary_expr         { Infix ($2, (Dec, $1)) }
 | unary_op cast_expr      { Unary ($2, $1) }
 | Tsizeof unary_expr      { SizeOfExpr ($1, $2) }
 | Tsizeof "(" type_id ")" { SizeOfType ($1, ($2, $3, $4)) }
 (*c++ext: *)
 | new_expr      { $1 }
 | delete_expr   { $1 }

unary_op: 
 | "&"    { GetRef,     $1 }
 | "*"    { DeRef,      $1 }
 | TPlus  { UnPlus,     $1 }
 | TMinus { UnMinus,    $1 }
 | TTilde { Tilde,      $1 }
 | TBang  { Not,        $1 }
 (* gccext: have that a lot in old kernel to get address of local label.
  * See gcc manual "local labels as values". *)
 | "&&" { GetRefLabel, $1 }


postfix_expr: 
 | primary_expr               { $1 }

 | postfix_expr "[" expr "]"              { ArrayAccess ($1, ($2, $3,$4)) }
 | postfix_expr "(" optl(listc(argument)) ")" { mk_funcall $1 ($2, $3, $4) }

 (*c++ext: ident is now a id_expression *)
 | postfix_expr TDot   Ttemplate? "::"?  id_expression
     { let name = ($4, fst $5, snd $5) in RecordAccess ($1,$2,name) }
 | postfix_expr TPtrOp Ttemplate? "::"? id_expression  
     { let name = ($4, fst $5, snd $5) in RecordPtAccess($1,$2,name)  }

 | postfix_expr TInc          { Postfix ($1, (Inc, $2)) }
 | postfix_expr TDec          { Postfix ($1, (Dec, $2)) }

 (* gccext: also called compound literals *)
 | "(" type_id ")" braced_init_list { GccConstructor (($1, $2, $3), $4) }

 (* c++ext: *)
 | cast_operator_expr { $1 }
 | Ttypeid "(" unary_expr ")" { TypeId ($1, ($2, Right $3, $4)) }
 | Ttypeid "(" type_id    ")" { TypeId ($1, ($2, Left $3, $4)) }
 | cast_constructor_expr { $1 }

 (* c++0x: *)
 | simple_type_specifier braced_init_list  { ExprTodo (Common2.fst3 $2) }
 (* TODO: should be typename, but require new parsing_hack_typedef *)
 | TIdent braced_init_list { ExprTodo (Common2.fst3 $2) }


primary_expr: 
 | literal { $1 }
 (* c++ext: *)
 | Tthis { This $1 }
 (*c++ext: cf below now. old: TIdent { Ident  (fst $1) [snd $1] }  *)

 (* forunparser: *)
 | "(" expr ")" { ParenExpr ($1, $2, $3) }  
 (* gccext: allow statement as expressions via ({ statement }) *)
 | "(" compound ")"    { StatementExpr ($1, $2, $3) }

 (* contains identifier rule *)
 | primary_cplusplus_id { $1 }
 (* c++0x: *)
 | lambda_introducer compound { ExprTodo $1 }

literal:
 (* constants a.k.a literal *)
 | TInt    { C (Int    ($1)) }
 | TFloat  { C (Float  ($1)) }
 | TChar   { C (Char   ($1)) }
 | TString { C (String ($1)) }
 (* gccext: cppext: *)
 | string_elem string_elem+ { C (MultiString ($1 :: $2)) }
 (*c++ext: *)
 | Ttrue   { C (Bool (true, $1)) }
 | Tfalse  { C (Bool (false, $1)) }
 (*c++0x: *)
 | Tnullptr { ExprTodo $1 }

(*----------------------------*)
(* c++ext: *)
(*----------------------------*)

(* can't factorize with following rule :(
 * | "::"? optl(nested_name_specifier) TIdent
 *)
primary_cplusplus_id:
 | id_expression 
     { let name = (None, fst $1, snd $1) in 
       Id (name, noIdInfo()) }
 (* grammar_c++: is in qualified_id inside id_expression instead? *)
 | "::" TIdent  
     { let name = Some $1, noQscope, IdIdent $2 in 
       Id (name, noIdInfo()) }
 | "::" operator_function_id 
     { let qop = $2 in
       let name = (Some $1, noQscope, qop) in 
       Id (name, noIdInfo()) }
 | "::" qualified_id 
     { let name = (Some $1, fst $2, snd $2) in 
       Id (name, noIdInfo()) }

(*could use TInf here *)
cast_operator_expr: 
 | cpp_cast_operator TInf_Template type_id  TSup_Template "(" expr ")" 
     { CplusplusCast ($1, ($2, $3, $4), ($5, $6, $7)) }
(* TODO: remove once we don't skip template arguments *)
 | cpp_cast_operator "(" expr ")"
     { ExprTodo $2 }

(*c++ext:*)
cpp_cast_operator:
 | Tstatic_cast      { Static_cast, $1 }
 | Tdynamic_cast     { Dynamic_cast, $1 }
 | Tconst_cast       { Const_cast, $1 }
 | Treinterpret_cast { Reinterpret_cast, $1 }

(* c++ext: cast with function syntax, and also constructor, but conflict
 * hence the TIdent_TypedefConstr. But it's simpler to just consider
 * this as a function call. A semantic analysis could infer it was
 * actually a ConstructedObject.
 * 
 * TODO: can have nested specifier before the typedefident ... so 
 * need a classname3?
*)
cast_constructor_expr:
 | TIdent_TypedefConstr "(" optl(listc(argument)) ")" 
     { let name = None, noQscope, IdIdent $1 in
       let ft = nQ, (TypeName name) in
       ConstructedObject (ft, ($2, $3, $4))  
     }
 | basic_type_2 "(" optl(listc(argument)) ")" 
     { let ft = nQ, $1 in
       ConstructedObject (ft, ($2, $3, $4))
     }

(* c++ext: * simple case: new A(x1, x2); *)
new_expr:
 | "::"? Tnew new_placement?   new_type_id  new_initializer?
     { New ($1, $2, $3, $4, $5)  }
(* ambiguity then on the "("
 "::"? Tnew new_placement? "(" type_id ")" new_initializer?
  *)

delete_expr:
 | "::"? Tdelete cast_expr                     
    { Delete ($1, $2, $3) }
 | "::"? Tdelete TOCro_new TCCro_new cast_expr 
     { DeleteArray ($1,$2,($3,(),$4), $5) }

new_placement: "(" listc(argument) ")" { ($1, $2, $3) }

new_initializer: "(" optl(listc(argument)) ")" { ($1, $2, $3) }

(*----------------------------*)
(* c++0x: lambdas! *)
(*----------------------------*)
lambda_introducer: 
 | TOCro_Lambda                "]" { $1 }
 | TOCro_Lambda lambda_capture "]" { $1 }

lambda_capture:
 | capture_list { }
 | capture_default { }
 | capture_default "," capture_list { }

capture_default:
 | "&" { } 
 | "="  { }

capture_list:
 | capture { }
 | capture_list "," capture { }
 | capture_list "," capture "..." { }
 | capture "..." { }

capture:
 | ident { }
 | "&" ident { }
 | Tthis { }
 (* grammar_c++: not in latest *)
 | ident "=" assign_expr { }

(*----------------------------*)
(* gccext: *)
(*----------------------------*)

string_elem:
 | TString            { fst $1 }
 (* cppext:  ex= printk (KERN_INFO "xxx" UTS_RELEASE)  *)
 | TIdent_MacroString { "<MACRO>", $1 }

(*----------------------------*)
(* cppext: *)
(*----------------------------*)

argument:
 | assign_expr { Arg $1 }
(* cppext: *)
(* actually this can happen also when have a wrong typedef inference ...*)
 | type_id     { ArgType $1  }
 (* sgrep-ext: *)
 | "..." { Flag_parsing.sgrep_guard (Arg (Ellipses $1)) }

 (* put in comment while trying to parse plan9 *)
 (* was especially used for the Linux kernel *)
 (* c++ext: to remove some conflicts (from 13 to 4) pass from * to + *)
 | TAny_Action+ { ArgAction (ActMisc $1) }

(*----------------------------*)
(* workarounds *)
(*----------------------------*)

(* would like evalInt $1 but require too much info *)
const_expr: cond_expr { $1  }

basic_type_2: 
 | Tchar_Constr    { (BaseType (IntType (CChar, $1))) }
 | Tint_Constr     { (BaseType (IntType (Si (Signed,CInt), $1)))}
 | Tfloat_Constr   { (BaseType (FloatType (CFloat, $1))) }
 | Tdouble_Constr  { (BaseType (FloatType (CDouble, $1))) }

 | Twchar_t_Constr { (BaseType (IntType (WChar_t, $1))) }

 | Tshort_Constr   { (BaseType (IntType (Si (Signed, CShort), $1))) }
 | Tlong_Constr    { (BaseType (IntType (Si (Signed, CLong), $1))) }
 | Tbool_Constr    { (BaseType (IntType (CBool, $1))) }

(*************************************************************************)
(* Statements *)
(*************************************************************************)

statement: 
 | compound        { Compound $1 }
 | expr_statement  { ExprStatement (fst $1, snd $1) }
 | labeled         { $1 }
 | selection       { $1 }
 | iteration       { $1 }
 | jump ";"        { Jump         ($1, $2) }

 (* cppext: *)
 | TIdent_MacroStmt { MacroStmt $1 }

 (* cppext: c++ext: because of cpp, some stuff looks like declaration but are in
  * fact statement but too hard to figure out, and if parse them as
  * expression, then we force to have first decls and then exprs, then
  * will have a parse error. So easier to let mix decl/statement.
  * Moreover it helps to not make such a difference between decl and
  * statement for further coccinelle phases to factorize code.
  * 
  * update: now a c++ext and handle slightly differently. It's inlined
  * in statement instead of going through a stat_or_decl.
  *)
 | declaration_statement { $1 }
 (* gccext: if move in statement then can have r/r conflict with define *)
 | function_definition { NestedFunc $1 }
 (* c++ext: *)
 | try_block { $1 }
 (* sgrep-ext: *)
 | "..." { Flag_parsing.sgrep_guard (ExprStatement (Some (Ellipses $1), $1)) }

compound: "{" statement_cpp* "}" { ($1, $2, $3) }

expr_statement: expr? ";" { $1, $2 }

(* note that case 1: case 2: i++;    would be correctly parsed, but with 
 * a Case  (1, (Case (2, i++)))  :(  *)
labeled: 
 | ident            ":" statement   { Label ($1, $2, $3) }
 | Tcase const_expr ":" statement   { Case ($1, $2, $3, $4) }
  (* gccext: allow range *)
 | Tcase const_expr "..." const_expr ":" statement
     { CaseRange ($1, $2, $3, $4, $5, $6) } 
 | Tdefault         ":" statement   { Default ($1, $2, $3) } 

(* classic else ambiguity resolved by a %prec, see conflicts.txt *)
selection: 
 | Tif "(" condition ")" statement              %prec LOW_PRIORITY_RULE
     { If ($1, ($2, $3, $4), $5, None) }
 | Tif "(" condition ")" statement Telse statement 
     { If ($1, ($2, $3, $4), $5, Some ($6, $7)) }
 | Tswitch "(" condition ")" statement             
     { Switch ($1, ($2, $3, $4), $5) }

iteration: 
 | Twhile "(" condition ")" statement                             
     { While ($1, ($2, $3, $4), $5) }
 | Tdo statement Twhile "(" expr ")" ";"                 
     { DoWhile ($1, $2, $3, ($4, $5, $6), $7) }
 | Tfor "(" for_init_stmt expr_statement expr? ")" statement
     { For ($1, ($2, (fst $3, snd $3, fst $4, snd $4, $5), $6), $7) }
 (* c++ext: *)
 | Tfor "(" for_range_decl ":" for_range_init ")" statement
     { StmtTodo $1 }
 (* cppext: *)
 | TIdent_MacroIterator "(" optl(listc(argument)) ")" statement
     { MacroIteration ($1, ($2, $3, $4), $5) }

(* the ';' in the caller grammar rule will be appended to the infos *)
jump: 
 | Tgoto ident  { Goto ($1, $2) } 
 | Tcontinue    { Continue $1 }
 | Tbreak       { Break $1 }
 | Treturn      { Return ($1, None) } 
 | Treturn expr { Return ($1, Some $2) }
 | Tgoto "*" expr { GotoComputed ($1, $2, $3) }

(*----------------------------*)
(* cppext: *)
(*----------------------------*)

statement_cpp:
 | statement { StmtElem $1 }
 (* cppext: *)
 | cpp_directive                                  { CppDirectiveStmt $1 }
 | cpp_ifdef_directive(* stat_or_decl_list ...*)  { IfdefStmt $1 }

(*----------------------------*)
(* c++ext: *)
(*----------------------------*)

declaration_statement: block_declaration { DeclStmt $1 }

condition:
 | expr { $1 }
 (* c++ext: *)
 | decl_spec_seq declaratori "=" initializer_clause 
     { ExprTodo (PI.fake_info "TODO") }

for_init_stmt:
 | expr_statement { $1 }
 (* c++ext: for(int i = 0; i < n; i++)*)
 | simple_declaration { None, PI.fake_info ";" } 

for_range_decl: type_spec_seq2 declarator { }

for_range_init: expr { }

try_block: Ttry compound handler+ { Try ($1, $2, $3) }

handler: Tcatch "(" exception_decl ")" compound { ($1, ($2, $3, $4), $5) }

exception_decl:
 | parameter_decl { ExnDecl $1 }
 | "..."          { ExnDeclEllipsis $1 }

(*************************************************************************)
(* Types *)
(*************************************************************************)

(*-----------------------------------------------------------------------*)
(* Type spec, left part of a type *)
(*-----------------------------------------------------------------------*)

(* in c++ grammar they put 'cv_qualifier' here but I prefer keep as before *)
type_spec:
 | simple_type_specifier { $1 }
 | elaborated_type_specifier { $1 }
 | enum_specifier  { Right3 $1, noii }
 | class_specifier { Right3 (StructDef $1), noii }

simple_type_specifier:
 | Tvoid                { Right3 (BaseType (Void $1)),            noii }
 | Tchar                { Right3 (BaseType (IntType (CChar, $1))), noii}
 | Tint                 { Right3 (BaseType (IntType (Si (Signed,CInt), $1))), noii}
 | Tfloat               { Right3 (BaseType (FloatType (CFloat, $1))),  noii}
 | Tdouble              { Right3 (BaseType (FloatType (CDouble, $1))), noii }
 | Tshort               { Middle3 Short,  [$1]}
 | Tlong                { Middle3 Long,   [$1]}
 | Tsigned              { Left3 Signed,   [$1]}
 | Tunsigned            { Left3 UnSigned, [$1]}
 (*c++ext: *)
 | Tbool                { Right3 (BaseType (IntType (CBool, $1))), noii }
 | Twchar_t             { Right3 (BaseType (IntType (WChar_t, $1))), noii }

 (* gccext: *)
 | Ttypeof "(" assign_expr ")" { Right3(TypeOf ($1,($2,Right $3,$4))), noii}
 | Ttypeof "(" type_id     ")" { Right3(TypeOf ($1,($2,Left $3,$4))), noii}

 (* history: cant put TIdent {} cos it makes the grammar ambiguous and 
  * generates lots of conflicts => we must use some tricks. 
  * See parsing_hacks_typedef.ml. See also conflicts.txt
  *)
 | type_cplusplus_id { Right3 (TypeName $1), noii }

 (* c++0x: *)
 | decltype_specifier { Middle3 Long, [$1] }

decltype_specifier:
 (* c++0x: TODO *)
 | Tdecltype "(" expr ")"           { $1  }
 (* TODO: because of wrong typedef inference *)
 | Tdecltype "(" TIdent_Typedef ")" { $1 }

(*todo: can have a ::opt optl(nested_name_specifier) before ident*)
elaborated_type_specifier: 
 | Tenum ident                  { Right3 (EnumName ($1, $2)), noii }
 | class_key ident              { Right3 (StructUnionName ($1, $2)), noii }
 (* c++ext:  *)
 | Ttypename type_cplusplus_id  { Right3 (TypenameKwd ($1, $2)), noii }

(*----------------------------*)
(* c++ext:  *)
(*----------------------------*)

(* cant factorize with a tcolcol_opt2 *)
type_cplusplus_id:
 | type_name                        { None, noQscope, $1 }
 | nested_name_specifier2 type_name { None, $1, $2 }
 | TColCol_BeforeTypedef type_name  { Some $1, noQscope, $2 }
 | TColCol_BeforeTypedef nested_name_specifier2 type_name { Some $1, $2, $3 }

(* in c++ grammar they put 
 *  typename: enum-name | typedef-name | class-name 
 *  class-name:  identifier | template-id
 *  template-id: template-name < template-argument-list > 
 * 
 * But in my case I don't have the contextual info so when I see an ident
 * it can be a typedef-name, enum-name, or class-name (but not template-name
 * because I detect them as they have a '<' just after),
 * so here type_name is simplified in consequence.
 *)
type_name:
 | enum_name_or_typedef_name_or_simple_class_name { IdIdent $1 }
 | template_id { $1 }

template_id:
 | TIdent_Templatename TInf_Template listc(template_argument) TSup_Template
    { IdTemplateId ($1, ($2, $3, $4)) }

(*c++ext: in the c++ grammar they have also 'template-name' but this is 
 * catched in my case by type_id and its generic TypedefIdent, or will be
 * parsed as an Ident and so assign_expr. In this later case may need an AST 
 * post-disambiguation analysis for some false positives.
 *)
template_argument:
 | type_id     { Left $1 }
 | assign_expr { Right $1 }

(*-----------------------------------------------------------------------*)
(* Qualifiers *)
(*-----------------------------------------------------------------------*)

(* was called type_qualif before *)
cv_qualif: 
 | Tconst    { {const=Some $1; volatile=None} }
 | Tvolatile { {const=None ; volatile=Some $1} }
 (* C99 *)
 | Trestrict { (* TODO *) {const=None ; volatile=None} }

(*-----------------------------------------------------------------------*)
(* Declarator, right part of a type + second part of decl (the ident)   *)
(*-----------------------------------------------------------------------*)
(* declarator return a couple: 
 *  (name, partial type (a function to be applied to return type))
 *
 * note that with 'int* f(int)' we must return Func(Pointer int,int) and not
 * Pointer (Func(int,int)).
 *)

declarator: 
 | pointer direct_d { (fst $2, fun x -> x |> $1 |> (snd $2)  ) }
 | direct_d         { $1  }

(* so must do  int * const p; if the pointer is constant, not the pointee *)
pointer: 
 | "*"                        { fun x ->(nQ,         (Pointer ($1, x)))}
 | "*" cv_qualif_list         { fun x ->($2.qualifD, (Pointer ($1, x)))}
 | "*" pointer                { fun x ->(nQ,         (Pointer ($1, $2 x)))}
 | "*" cv_qualif_list pointer { fun x ->($2.qualifD, (Pointer ($1, $3 x)))}
 (*c++ext: no qualif for ref *)
 | "&"                        { fun x ->(nQ,    (Reference ($1, x)))}
 | "&" pointer                { fun x ->(nQ,    (Reference ($1, $2 x)))}
 (* c++0x: TODO AST *)
 | "&&"                        { fun x ->(nQ,    (Reference ($1, x)))}
 | "&&" pointer                { fun x ->(nQ,    (Reference ($1, $2 x)))}

direct_d: 
 | declarator_id
     { ($1, fun x -> x) }
 | "(" declarator ")"      (* forunparser: old: $2 *) 
     { (fst $2, fun x -> (nQ, (ParenType ($1, (snd $2) x, $3)))) }
 | direct_d "["            "]"         
     { (fst $1, fun x->(snd $1) (nQ,(Array (($2,None,$3),x)))) }
 | direct_d "[" const_expr "]"
     { (fst $1, fun x->(snd $1) (nQ,(Array (($2, Some $3, $4),x)))) }
 | direct_d "(" ")" const_opt exn_spec?
     { (fst $1, fun x-> (snd $1) 
         (nQ, (FunctionType {
           ft_ret= x; ft_params = ($2, [], $3);
           ft_dots = None; ft_const = $4; ft_throw = $5; })))
     }
 | direct_d "(" parameter_type_list ")" const_opt exn_spec?
     { (fst $1, fun x-> (snd $1) 
          (nQ,(FunctionType { 
            ft_ret = x; ft_params = ($2,fst $3,$4); 
            ft_dots = snd $3; ft_const = $5; ft_throw = $6; })))
     }

(*----------------------------*)
(* c++ext: *)
(*----------------------------*)
declarator_id:
 | "::"? id_expression  { ($1, fst $2, snd $2) }
(* TODO ::opt nested-name-specifieropt type-name*)

(*-----------------------------------------------------------------------*)
(* Abstract Declarator (right part of a type, no ident) *)
(*-----------------------------------------------------------------------*)
abstract_declarator: 
 | pointer                            { $1 }
 |         direct_abstract_declarator { $1 }
 | pointer direct_abstract_declarator { fun x -> x |> $2 |> $1 }

direct_abstract_declarator: 
 | "(" abstract_declarator ")" (* forunparser: old: $2 *)
     { (fun x -> (nQ, (ParenType ($1, $2 x, $3)))) }
 | "["            "]"                            
     { fun x ->   (nQ, (Array (($1,None, $2), x)))}
 | "[" const_expr "]"                            
     { fun x ->   (nQ, (Array (($1, Some $2, $3), x)))}
 | direct_abstract_declarator "["            "]" 
     { fun x ->$1 (nQ, (Array (($2, None, $3), x))) }
 | direct_abstract_declarator "[" const_expr "]"
     { fun x ->$1 (nQ, (Array (($2, Some $3, $4), x))) }
 | "(" ")"                                       
     { fun x -> (nQ, (FunctionType {
       ft_ret = x; ft_params = ($1,[],$2); 
       ft_dots = None; ft_const = None; ft_throw = None;})) }
 | "(" parameter_type_list ")"
     { fun x -> (nQ, (FunctionType {
         ft_ret = x; ft_params = ($1,fst $2,$3); 
         ft_dots = snd $2; ft_const = None; ft_throw = None; })) }
 | direct_abstract_declarator "(" ")" const_opt exn_spec?
     { fun x -> $1 (nQ, (FunctionType {
         ft_ret = x; ft_params = ($2,[],$3); 
         ft_dots = None; ft_const = $4; ft_throw = $5; })) }
 | direct_abstract_declarator "(" parameter_type_list ")" const_opt exn_spec?
     { fun x -> $1 (nQ, (FunctionType {
         ft_ret = x; ft_params = ($2,fst $3,$4); 
         ft_dots = snd $3; ft_const = $5; ft_throw = $6; })) }

(*-----------------------------------------------------------------------*)
(* Parameters (use decl_spec_seq not type_spec just for 'register') *)
(*-----------------------------------------------------------------------*)
parameter_type_list: 
 | parameter_list           { $1, None }
 | parameter_list "," "..." { $1, Some ($2,$3) }

parameter_decl: 
 | decl_spec_seq declarator
     { let (t_ret,reg) = type_and_register_from_decl $1 in
       let (name, ftyp) = fixNameForParam $2 in
       { p_name = Some name; p_type = ftyp t_ret;
         p_register = reg; p_val = None } }
 | decl_spec_seq abstract_declarator
     { let (t_ret, reg) = type_and_register_from_decl $1 in
       { p_name = None; p_type = $2 t_ret; 
         p_register = reg; p_val = None } }
 | decl_spec_seq
     { let (t_ret, reg) = type_and_register_from_decl $1 in
       { p_name = None; p_type = t_ret; p_register = reg; p_val = None } }

(*c++ext: default parameter value, copy paste *)
 | decl_spec_seq declarator "=" assign_expr
     { let (t_ret, reg) = type_and_register_from_decl $1 in 
       let (name, ftyp) = fixNameForParam $2 in
       { p_name = Some name; p_type = ftyp t_ret; 
         p_register = reg; p_val = Some ($3, $4) } }
 | decl_spec_seq abstract_declarator "=" assign_expr
     { let (t_ret, reg) = type_and_register_from_decl $1 in
       { p_name = None; p_type = $2 t_ret; 
         p_register = reg; p_val = Some ($3, $4) } }
 | decl_spec_seq "=" assign_expr
     { let (t_ret, reg) = type_and_register_from_decl $1 in
       { p_name = None; p_type = t_ret; 
         p_register = reg; p_val = Some($2,$3) } }

(*----------------------------*)
(* workarounds *)
(*----------------------------*)

parameter_list: 
 | parameter_decl2                    { [$1, []] }
 | parameter_list "," parameter_decl2 { $1 @ [$3,  [$2]] }

parameter_decl2:
 | parameter_decl { $1 }
 (* when the typedef inference didn't work *)
 | TIdent
     { let t = nQ, (TypeName (None, [], IdIdent $1)) in
       { p_name = None; p_type = t; p_val = None; p_register = None; } }

(*----------------------------*)
(* c++ext: *)
(*----------------------------*)
(*c++ext: specialisation 
 * TODO should be type-id-listopt. Also they can have qualifiers!
 * need typedef heuristic for throw() but can be also an expression ...
 *)
exn_spec: 
 | Tthrow "(" ")"                        { ($1, ($2, [], $3)) }
 | Tthrow "(" exn_name ")"               { ($1, ($2, [Left $3], $4)) }
 | Tthrow "(" exn_name "," exn_name ")" 
     { ($1, ($2, [Left $3; Right $4; Left $5], $6))  }

exn_name: ident { None, [], IdIdent $1 }

(*c++ext: in orig they put cv-qualifier-seqopt but it's never volatile so *)
const_opt:
 | Tconst        { Some $1 }
 | (*empty*) { None }

(*-----------------------------------------------------------------------*)
(* helper type rules *)
(*-----------------------------------------------------------------------*)
(* For type_id. No storage here. Was used before for field but 
 * now structure fields can have storage so fields now use decl_spec. *)
spec_qualif_list: 
 | type_spec                    { addTypeD $1 nullDecl }
 | cv_qualif                    { {nullDecl with qualifD = $1} }
 | type_spec   spec_qualif_list { addTypeD $1 $2   }
 | cv_qualif   spec_qualif_list { addQualifD $1 $2 }

(* for pointers in direct_declarator and abstract_declarator *)
cv_qualif_list: 
 | cv_qualif                  { {nullDecl with qualifD = $1 } }
 | cv_qualif_list cv_qualif   { addQualifD $2 $1 }

(* grammar_c++: should be type_spec_seq but conflicts
 * could solve with special TOPar_foreach *)
%inline
type_spec_seq2: decl_spec_seq { $1 }

(*-----------------------------------------------------------------------*)
(* type_id *)
(*-----------------------------------------------------------------------*)

(* For cast, sizeof, throw. Was called type_name in old C grammar. *)
type_id: 
 | spec_qualif_list
     { let (t_ret, _, _) = type_and_storage_from_decl $1 in  t_ret }
 | spec_qualif_list abstract_declarator
     { let (t_ret, _, _) = type_and_storage_from_decl $1 in $2 t_ret }
(* used for the type passed to new(). 
 * There is ambiguity with '*' and '&' cos when have new int *2, it can
 * be parsed as (new int) * 2 or (new int * ) 2.
 * cf p62 of Ellis. So when see a "*" or "&" don't reduce here,
 * shift, hence the prec when are to decide wether or not to enter
 * in new_declarator and its leading ptr_operator
 *)
new_type_id: 
 | spec_qualif_list %prec LOW_PRIORITY_RULE   
     { let (t_ret, _, _) = type_and_storage_from_decl $1 in  t_ret }
 | spec_qualif_list new_declarator 
     { let (t_ret, _, _) = type_and_storage_from_decl $1 in (* TODOAST *) t_ret }

new_declarator: 
 | ptr_operator new_declarator 
     { () }
 | ptr_operator %prec LOW_PRIORITY_RULE
     { () }
 | direct_new_declarator 
     { () }

ptr_operator:
 | "*" { () }
 (* c++ext: *)
 | "&" { () }
 (* c++0x: TODO AST *)
 | "&&" { () }

direct_new_declarator:
 | "[" expr "]"                       { () }
 | direct_new_declarator "[" expr "]" { () }

(* in c++ grammar they do 'type_spec_seq conversion_declaratoropt'. We
 * can not replace with a simple 'type_id' cos here we must not allow
 * functionType otherwise there is conflicts on "(".
 * TODO: right now do simple_type_specifier because conflict 
 * when do full type_spec.
 *   type_spec conversion_declaratoropt
 *)
conversion_type_id: 
 | simple_type_specifier conversion_declarator 
     { let tx = addTypeD $1 nullDecl in
       let (t_ret, _, _) = type_and_storage_from_decl tx in t_ret 
     }
 | simple_type_specifier %prec LOW_PRIORITY_RULE 
     { let tx = addTypeD $1 nullDecl in
       let (t_ret, _, _) = type_and_storage_from_decl tx in t_ret 
     }

conversion_declarator: 
 | ptr_operator conversion_declarator 
     { () }
 | ptr_operator %prec LOW_PRIORITY_RULE
     { () }

(*************************************************************************)
(* Class and struct definitions *)
(*************************************************************************)

(* this can come from a simple_declaration/decl_spec *)
class_specifier: class_head "{" optl(member_specification) "}" 
     { let (kind, nameopt, baseopt) = $1 in
       { c_kind = kind; c_name = nameopt; 
         c_inherit = baseopt; c_members = ($2, $3, $4) } }

(* todo in grammar they allow anon class with base_clause, weird. 
 * bugfix_c++: in c++ grammar they put identifier but when we do template
 * specialization then we can get some template_id. Note that can
 * not introduce a class_key_name intermediate cos they get a 
 * r/r conflict as there is another place with a 'class_key ident'
 * in elaborated specifier. So need to duplicate the rule for
 * the template_id case.
*)
class_head: 
 | class_key 
     { $1, None, None }
 | class_key ident base_clause?
     { let name = None, noQscope, IdIdent $2 in
       $1, Some name, $3 }
 | class_key nested_name_specifier ident base_clause?
     { let name = None, $2, IdIdent $3 in
       $1, Some name, $4 }

(* was called struct_union before *)
class_key: 
 | Tstruct   { Struct, $1 }
 | Tunion    { Union, $1 }
 (*c++ext: *)
 | Tclass    { Class, $1 }

(*----------------------------*)
(* c++ext: inheritance rules *)
(*----------------------------*)
base_clause: ":" listc(base_specifier) { $1, $2 }

(* base-specifier:
   *  ::opt nested-name-specifieropt class-name
   *  virtual access-specifieropt ::opt nested-name-specifieropt class-name
   *  access-specifier virtualopt ::opt nested-name-specifieropt class-name
   * specialisation
   *)
base_specifier:
 | class_name 
     { { i_name = $1; i_virtual = None; i_access = None } }
 | access_specifier class_name 
     { { i_name = $2; i_virtual = None; i_access = Some $1 } }
 | Tvirtual access_specifier class_name 
     { { i_name = $3; i_virtual = Some $1; i_access = Some $2 } }

(* TODO? specialisation | ident { $1 }, do heuristic so can remove rule2 *)
class_name:
 | type_cplusplus_id  { $1 }
 | TIdent             { None, noQscope, IdIdent $1 }

(*----------------------------*)
(* c++ext: members *)
(*----------------------------*)

(* todo? add cpp_directive possibility here too *)
member_specification:
 | member_declaration optl(member_specification)
     { ClassElem $1::$2 }
 | access_specifier ":" optl(member_specification)
     { ClassElem (Access ($1, $2))::$3 }

access_specifier:
 | Tpublic    { Public, $1 }
 | Tprivate   { Private, $1 }
 | Tprotected { Protected, $1 }


(* in c++ grammar there is a ;opt after function_definition but 
   * there is a conflict as it can also be an EmptyField *)
member_declaration:
 | field_declaration      { fixFieldOrMethodDecl $1 }
 | function_definition    { MemberFunc (FunctionOrMethod $1) }
 | qualified_id ";"   
     { let name = (None, fst $1, snd $1) in
       QualifiedIdInClass (name, $2)
     }
 | using_declaration      { UsingDeclInClass $1 }
 | template_declaration   { TemplateDeclInClass $1 }

 (* not in c++ grammar as merged with function_definition, but I can't *)
 | ctor_dtor_member       { $1 }

 (* cppext: as some macro sometimes have a trailing ';' we must allow
    * them here. Generates conflicts if keep the opt_ptvirg mentionned 
    * before.
    * c++ext: in c++ grammar they put a double optional but I prefer
    * to force the presence of a decl_spec. I don't know what means
    * 'x;' in a structure, maybe default to int but not practical for my way of
    * parsing
    *)
 | ";"    { EmptyField $1 }

(*-----------------------------------------------------------------------*)
(* field declaration *)
(*-----------------------------------------------------------------------*)
field_declaration:
 | decl_spec_seq ";" 
     { (* gccext: allow empty elements if it is a structdef or enumdef *)
       let (t_ret, sto, _inline) = type_and_storage_from_decl $1 in
       let onedecl = { v_namei = None; v_type = t_ret; v_storage = sto } in
       ([(FieldDecl onedecl),noii], $2)
     }
 | decl_spec_seq listc(member_declarator) ";" 
     { let (t_ret, sto, _inline) = type_and_storage_from_decl $1 in
       ($2 |> (List.map (fun (f, iivirg) -> f t_ret sto, iivirg)), $3)
     }

(* was called struct_declarator before *)
member_declarator:
 | declarator                    
     { let (name, partialt) = $1 in (fun t_ret sto -> 
       FieldDecl {
         v_namei = Some (name, None);
         v_type = partialt t_ret; v_storage = sto; })
     }
 (* can also be an abstract when it's =0 on a function type *)
 | declarator "=" const_expr
     { let (name, partialt) = $1 in (fun t_ret sto -> 
       FieldDecl {
         v_namei = Some (name, Some (EqInit ($2, InitExpr $3)));
         v_type = partialt t_ret; v_storage = sto;
       })
     }

 (* normally just ident, but ambiguity so solve by inspetcing declarator *)
 | declarator ":" const_expr
     { let (name, _partialt) = fixNameForParam $1 in (fun t_ret _stoTODO -> 
       BitField (Some name, $2, t_ret, $3))
     }
 | ":" const_expr            
     { (fun t_ret _stoTODO -> BitField (None, $1, t_ret, $2)) }

(*************************************************************************)
(* Enum definition *)
(*************************************************************************)

(* gccext: c++0x: most ","? are trailing commas extensions (as in Perl) *)
enum_specifier: 
 | enum_head "{" listc(enumerator) ","? "}"
     { EnumDef ($1, None(* TODO *), ($2, $3, $5)) (*$4*) }
 (* c++0x: *)
 | enum_head "{" "}"
     { EnumDef ($1, None(* TODO *), ($2, [], $3)) }

enum_head:
 | enum_key ident? (*ioption(enum_base)*) { $1 }

enumerator: 
 | ident                { { e_name = $1; e_val = None; } }
 | ident "=" const_expr { { e_name = $1; e_val = Some ($2, $3); } }

(*-----------------------------------------------------------------------*)
(* c++ext: constructor special case *)
(*-----------------------------------------------------------------------*)
%inline
enum_key:
 | Tenum { $1 }
 | Tenum Tclass { $1 }
 | Tenum Tstruct { $1 }

(* TODO conflicts
enum_base: ":" type_spec_seq2 { }
*)

(*************************************************************************)
(* Simple declaration, initializers *)
(*************************************************************************)

simple_declaration:
 | decl_spec_seq ";"
     { let (t_ret, sto, _inline) = type_and_storage_from_decl $1 in 
       DeclList ([{v_namei = None; v_type = t_ret; v_storage = sto},noii],$2)
     }
 | decl_spec_seq listc(init_declarator) ";" 
     { let (t_ret, sto, _inline) = type_and_storage_from_decl $1 in
       DeclList (
         ($2 |> List.map (fun (((name, f), iniopt), iivirg) ->
           (* old: if fst (unwrap storage)=StoTypedef then LP.add_typedef s; *)
           { v_namei = Some (name, iniopt);
             v_type = f t_ret; v_storage = sto
           },
           iivirg
         )), $3)
     } 
 (* cppext: *)
 | TIdent_MacroDecl "(" listc(argument) ")" ";" 
     { MacroDecl ([], $1, ($2, $3, $4), $5) }
 | Tstatic TIdent_MacroDecl "(" listc(argument) ")" ";" 
     { MacroDecl ([$1], $2, ($3, $4, $5), $6) }
 | Tstatic Tconst_MacroDeclConst 
    TIdent_MacroDecl "(" listc(argument) ")" ";" 
     { MacroDecl ([$1;$2], $3, ($4, $5, $6), $7) }

(*-----------------------------------------------------------------------*)

decl_spec_seq:
 | decl_spec               { $1 nullDecl }
 | decl_spec decl_spec_seq { $1 $2 }

decl_spec:
 | storage_class_spec { addStorageD $1 }
 | type_spec          { addTypeD  $1 }
 | function_spec      { addInlineD (snd $1)(*TODO*) }

(* grammar_c++: cv_qualif is not here but instead inline in type_spec. 
 * I prefer to keep as before but I take care when
 * they speak about type_spec to translate instead in type+qualif_spec
 * (which is spec_qualif_list)*)
 | cv_qualif          { addQualifD $1 }

 | Ttypedef           { addStorageD (StoTypedef $1) }
 | Tfriend            { addInlineD $1 (*TODO*) }
 | Tconstexpr         { addInlineD $1 (*TODO*) }

(* grammar_c++: they put 'explicit' in function_spec, 'typedef' and 'friend' 
 * in decl_spec. But it's just estethic as no other rules directly
 * mention function_spec or storage_spec. They just want to say that 
 * 'virtual' applies only to functions, but they have no way to check that 
 * syntaxically. I could keep as before, as in the C grammar. 
 * For 'explicit' I prefer to put it directly
 * with the ctor as I already have a special heuristic for constructor. 
 *)
function_spec:
 (*gccext: and c++ext: *)
 | Tinline { Inline, $1 }
 (*c++ext: *)
 | Tvirtual { Virtual, $1 }

storage_class_spec: 
 | Tstatic      { Sto (Static,  $1) }
 | Textern      { Sto (Extern,  $1) }
 (* c++ext: now really used, not as in C, for type inferred variables *)
 | Tauto        { Sto (Auto,    $1) }
 | Tregister    { Sto (Register,$1) }
 (* c++ext: *)
 | Tmutable     { Sto (Register,$1) (*TODO*) }
 (* c++0x: *)
 | Tthread_local { Sto (Register,$1) (*TODO*) }

(*-----------------------------------------------------------------------*)
(* declarators (right part of type and variable) *)
(*-----------------------------------------------------------------------*)
init_declarator:  
 | declaratori                  { ($1, None) }
 | declaratori "=" initializer_clause   { ($1, Some (EqInit ($2, $3))) }

 (* c++ext: c++ initializer via call to constructor. Note that this
  * is different from TypedefIdent2, here the declaratori is an ident,
  * not the constructorname hence the need for a TOPar_CplusplusInit
  *)
 | declaratori TOPar_CplusplusInit optl(listc(argument)) ")" 
     { ($1, Some (ObjInit ($2, $3, $4))) }

(*----------------------------*)
(* gccext: *)
(*----------------------------*)
declaratori: 
 | declarator                { $1 }
 (* gccext: *) 
 | declarator gcc_asm_decl   { $1 }

gcc_asm_decl: 
 | Tasm Tvolatile? "(" asmbody ")"        {  }
			  
(*-----------------------------------------------------------------------*)
(* initializers *)
(*-----------------------------------------------------------------------*)
initializer_clause: 
 | assign_expr      { InitExpr $1 }
 | braced_init_list { InitList $1 }

braced_init_list:
 | "{" "}"                       { ($1, [], $2) }
 | "{" initialize_list ","? "}"  { ($1, List.rev $2, $4) (*$3*) }

(* opti: This time we use the weird order of non-terminal which requires in 
 * the "caller" to do a List.rev cos quite critical. With this wierd order it
 * allows yacc to use a constant stack space instead of exploding if we would
 * do a  'initialize2 Tcomma initialize_list'.
 *)
initialize_list: 
 | initialize2                        { [$1,   []] }
 | initialize_list "," initialize2 { ($3,  [$2])::$1 }


(* gccext: condexpr and no assign_expr cos can have ambiguity with comma *)
initialize2: 
 | cond_expr        { InitExpr $1 } 
 | braced_init_list { InitList $1 }

 (* gccext: labeled elements, a.k.a designators *)
 | designator+ "=" initialize2  { InitDesignators ($1, $2, $3) }
 (* gccext: old format, in old kernel for instance *)
 | ident ":" initialize2       { InitFieldOld ($1, $2, $3) }

(* kenccext: c++ext:, but conflict with array designators and lambdas! *)
 | "[" const_expr "]"  "=" initialize2
     { InitDesignators ([DesignatorIndex($1, $2, $3)], $4, $5) }
(* conflicts with c++0x lambda! *)
 | "[" const_expr "]" initialize2  { InitIndexOld (($1, $2, $3), $4) }

(* they can be nested, can have a .x.[3].y *)
designator: 
 | TDot ident   { DesignatorField ($1, $2) } 
(* conflict with kenccext
 | "[" const_expr "]"     %prec LOW_PRIORITY_RULE
     { DesignatorIndex ($1, $2, $3) }
 | "[" const_expr "..." const_expr "]" 
     { DesignatorRange ($1, ($2, $3, $4), $5) }
*)

(*************************************************************************)
(* Block declaration (namespace and asm) *)
(*************************************************************************)

block_declaration:
 | simple_declaration { $1 }
 (*gccext: *)
 | asm_definition     { $1 }
 (*c++ext: *)
 | namespace_alias_definition { $1 }
 | using_declaration { UsingDecl $1 }
 | using_directive   { $1 }


(*----------------------------*)
(* c++ext: *)
(*----------------------------*)

namespace_alias_definition: Tnamespace TIdent "=" qualified_namespace_spec ";"
  { NameSpaceAlias ($1, $2, $3, $4, $5) }

using_directive: Tusing Tnamespace qualified_namespace_spec ";"
  { UsingDirective ($1, $2, $3, $4) }

qualified_namespace_spec: "::"? optl(nested_name_specifier) namespace_name
  { $1, $2, IdIdent $3 }

(* conflict on TColCol in 'Tusing TColCol unqualified_id ";"'
 * need LALR(2) to see if after tcol have a nested_name_specifier
 * or put opt on nested_name_specifier too *)
using_declaration:
 | Tusing Ttypename? "::"? nested_name_specifier unqualified_id ";"
     { let name = ($3, $4, $5) in $1, name, $6 (*$2*) }
(* TODO: remove once we don't skip qualifier ? *)
 | Tusing Ttypename? "::"? unqualified_id ";" 
     { let name = ($3, [], $4) in $1, name, $5 (*$2*) }
  
(*----------------------------*)
(* gccext: c++ext: *)
(*----------------------------*)

(* gccext: c++ext: also apparently *)
asm_definition: Tasm Tvolatile? "(" asmbody ")" ";" {Asm($1,$2,($3,$4,$5),$6)}

asmbody: 
 | string_elem+ colon_asm+  { $1, $2 }
 | string_elem+ { $1, [] } (* in old kernel *)

colon_asm: ":" listc(colon_option) { Colon ($1, $2) }

colon_option: 
 | TString                  { ColonMisc [snd (fst $1)] }
 | TString "(" asm_expr ")" { ColonExpr ([snd (fst $1)], ($2, $3, $4)) } 
 (* cppext: certainly a macro *)
 | "[" TIdent "]" TString "(" asm_expr ")"
     { ColonExpr ([$1;snd $2;$3;snd (fst $4)], ($5, $6, $7))  }
 | TIdent                   { ColonMisc [snd $1] }
 | (* empty *)              { ColonMisc [] }

asm_expr: assign_expr { $1 }

(*************************************************************************)
(* Declaration, in c++ sense *)
(*************************************************************************)
(* in grammar they have 'explicit_instantiation' but it is equal to 
 * to template_declaration and so is ambiguous.
 * 
 * declaration > block_declaration > simple_declaration, hmmm
 * could be renamed declaration_or_definition
 *)
declaration:
 | block_declaration                 { BlockDecl $1 }

 | function_definition               { Func (FunctionOrMethod $1) }

 (* not in c++ grammar as merged with function_definition, but I can't *)
 | ctor_dtor { $1 }
 | template_declaration              { let (a,b,c) = $1 in TemplateDecl (a,b,c)}
 | explicit_specialization           { $1 }
 | linkage_specification             { $1 }
 | namespace_definition              { $1 }

 (* sometimes the function ends with }; instead of just } *)
 | ";"    { EmptyDef $1 } 

(*----------------------------*)
(* cppext: *)
(*----------------------------*)

declaration_cpp:
 | declaration { DeclElem $1 }
 (* cppext: *)
 | cpp_directive                                 { CppDirectiveDecl $1 }
 | cpp_ifdef_directive(* stat_or_decl_list ...*) { IfdefDecl $1 }

(*----------------------------*)
(* c++ext: *)
(*----------------------------*)

template_declaration: 
  Ttemplate TInf_Template listc(template_parameter) TSup_Template declaration
   { ($1, ($2, $3, $4), $5) }

explicit_specialization: Ttemplate TInf_Template TSup_Template declaration 
   { TemplateSpecialization ($1, ($2, (), $3), $4) }

(*todo: '| type_paramter' 
   * ambiguity with parameter_decl cos a type can also be 'class X'
 | Tclass ident { raise Todo }
   *)
template_parameter: 
 | parameter_decl { $1 }


(* c++ext: could also do a extern_string_opt to factorize stuff *)
linkage_specification:
 | Textern TString declaration 
     { ExternC ($1, (snd (fst $2)), $3) }
 | Textern TString "{" optl(declaration_cpp+) "}" 
     { ExternCList ($1, (snd (fst $2)), ($3, $4, $5)) }


namespace_definition:
 | named_namespace_definition   { $1 }
 | unnamed_namespace_definition { $1 }

(* in c++ grammar they make diff between 'original' and 'extension' namespace
 * definition but they require some contextual information to know if
 * an identifier was already a namespace. So here I have just a single rule.
 *)
named_namespace_definition: 
 | Tnamespace TIdent "{" optl(declaration_cpp+) "}" 
     { NameSpace ($1, $2, ($3, $4, $5)) }

unnamed_namespace_definition: Tnamespace "{" optl(declaration_cpp+) "}" 
     { NameSpaceAnon ($1, ($2, $3, $4)) }

(*************************************************************************)
(* Function definition *)
(*************************************************************************)

function_definition: 
 | decl_spec_seq declarator function_body 
     { let (t_ret, sto) = type_and_storage_for_funcdef_from_decl $1 in
       let x = (fst $2, fixOldCDecl ((snd $2) t_ret), sto) in
       fixFunc (x, $3) 
     }
 (* c++0x: TODO 2 more s/r conflicts and regressions! *)
(*
 | decl_spec_seq declarator "=" Tdefault ";"  
     { let (t_ret, sto) = type_and_storage_for_funcdef_from_decl $1 in
       let x = (fst $2, fixOldCDecl ((snd $2) t_ret), sto) in
       let body = ($3, [], $4) in(* TODO *)
       fixFunc (x, body) 
     }
 | decl_spec_seq declarator "=" Tdelete ";"  
     { let (t_ret, sto) = type_and_storage_for_funcdef_from_decl $1 in
       let x = (fst $2, fixOldCDecl ((snd $2) t_ret), sto) in
       let body = ($3, [], $4) in(* TODO *)
       fixFunc (x, body) 
     }
*)
function_body: 
 | compound { $1 }

(*-----------------------------------------------------------------------*)
(* c++ext: constructor special case *)
(*-----------------------------------------------------------------------*)

(* Special case cos ctor/dtor do not have return type. *)
ctor_dtor:
 | nested_name_specifier TIdent_Constructor "(" parameter_type_list? ")"
     ctor_mem_initializer_list_opt
     compound
     { DeclTodo }
 (* new_type_id, could also introduce a Tdestructorname or forbidy the
      TypedefIdent2 transfo by putting a guard in the lalr(k) rule by
      checking if have a ~ before
   *)
 | nested_name_specifier TTilde ident "(" Tvoid? ")" compound
     { DeclTodo }

(* TODO: remove once we don't skip qualifiers *)
 | Tinline? TIdent_Constructor "(" parameter_type_list? ")"
     ctor_mem_initializer_list_opt
     compound
     { DeclTodo }
 | TTilde ident "(" Tvoid? ")" exn_spec? compound
     { DeclTodo }



(* special case for ctor/dtor because they don't have a return type.
   * TODOAST on the ctor_spec and chain of calls
   *)
ctor_dtor_member: 
 | ctor_spec TIdent_Constructor "(" parameter_type_list? ")"
     ctor_mem_initializer_list_opt
     compound
     { MemberFunc (Constructor (mk_constructor $2 ($3, $4, $5) $7)) }
 | ctor_spec TIdent_Constructor "(" parameter_type_list? ")" ";" 
     { MemberDecl (ConstructorDecl ($2, ($3, opt_to_list_params $4, $5), $6)) }
 | ctor_spec TIdent_Constructor "(" parameter_type_list? ")" "=" Tdelete ";" 
     { MemberDecl (ConstructorDecl ($2, ($3, opt_to_list_params $4, $5), $6)) }
 | ctor_spec TIdent_Constructor "(" parameter_type_list? ")" "=" Tdefault ";" 
     { MemberDecl (ConstructorDecl ($2, ($3, opt_to_list_params $4, $5), $6)) }

 | dtor_spec TTilde ident "(" Tvoid? ")" exn_spec? compound
     { MemberFunc (Destructor (mk_destructor $2 $3 ($4, $5, $6) $7 $8)) }
 | dtor_spec TTilde ident "(" Tvoid? ")" exn_spec? ";"
     { MemberDecl (DestructorDecl ($2, $3, ($4, $5, $6), $7, $8)) }
 | dtor_spec TTilde ident "(" Tvoid? ")" exn_spec? "=" Tdelete ";"
     { MemberDecl (DestructorDecl ($2, $3, ($4, $5, $6), $7, $8)) }
 | dtor_spec TTilde ident "(" Tvoid? ")" exn_spec? "=" Tdefault ";"
     { MemberDecl (DestructorDecl ($2, $3, ($4, $5, $6), $7, $8)) }


ctor_spec:
 | Texplicit { }
 | Tinline { }
 | (*empty*) { }

dtor_spec:
 | Tvirtual { }
 | Tinline { }
 | (*empty*) { }

ctor_mem_initializer_list_opt: 
 | ":" listc(mem_initializer) { () }
 | (* empty *) { () }

mem_initializer: 
 | mem_initializer_id "(" optl(listc(argument)) ")" { () }

(* factorize with declarator_id ? specialisation *)
mem_initializer_id:
(* specialsiation | TIdent { () } *)
 | primary_cplusplus_id { () }

(*************************************************************************)
(* Cpp directives *)
(*************************************************************************)

(* cppext: *)
cpp_directive: 
 | TInclude 
     { let (_include_str, filename, tok) = $1 in
       (* redo some lexing work :( *)
       let inc_kind, path = 
         match () with
         | _ when filename =~ "^\"\\(.*\\)\"$" ->  Local, matched1 filename
         | _ when filename =~ "^\\<\\(.*\\)\\>$" -> Standard, matched1 filename
         | _ -> Weird, filename
       in
       Include (tok, inc_kind, path)
     }

 | TDefine TIdent_Define define_val TCommentNewline_DefineEndOfMacro
     { Define ($1, $2, DefineVar, $3) (*$4??*) }

 (* The TOPar_Define is introduced to avoid ambiguity with previous rules.
  * A TOPar_Define is a TOPar that was just next to the ident (no space).
  * See parsing_hacks_define.ml
  *)
 | TDefine TIdent_Define TOPar_Define optl(listc(param_define)) ")" 
    define_val TCommentNewline_DefineEndOfMacro
     { Define ($1, $2, (DefineFunc ($3, $4, $5)), $6) (*$7*) }

 | TUndef             { Undef $1 }
 | TCppDirectiveOther { PragmaAndCo $1 }

define_val: 
 (* perhaps better to use assign_expr? but in that case need 
    * do a assign_expr_of_string'in parse_c.
    * c++ext: update, now statement include simple declarations
    * so maybe can parse $1 and generate the previous DefineDecl
    * and DefineFunction? cos nested_func is also now inside statement.
    *)
 | expr      { DefineExpr $1 }
 | statement { DefineStmt $1 }
 (* for statement-like macro with fixed number of arguments *)
 | Tdo statement Twhile "(" expr ")" 
     { match $5 with
       | (C (Int ("0", tok))) -> 
         DefineDoWhileZero ($1, $2, $3, ($4, tok, $6))
       | _ -> raise Parsing.Parse_error
     }
 (* for statement-like macro with varargs *)
 | Tif "(" condition ")" id_expression
     { let name = (None, fst $5, snd $5) in 
       DefinePrintWrapper ($1, ($2, $3, $4), name) 
     }
 | TOBrace_DefineInit initialize_list "}" ","?
    { DefineInit (InitList ($1, List.rev $2, $3) (*$4*))  }
 | (* empty *) { DefineEmpty }


param_define:
 | ident                { $1 }
 | TDefParamVariadic    { $1 } 
 | "..."                { "...", $1 }
 (* they reuse keywords :(  *)
 | Tregister            { "register", $1 }
 | Tnew                 { "new", $1 }


cpp_ifdef_directive: 
 | TIfdef     { Ifdef, $1 }
 | TIfdefelse { IfdefElse, $1 }
 | TIfdefelif { IfdefElseif, $1 }
 | TEndif     { IfdefEndif, $1 }

 | TIfdefBool  { Ifdef, snd $1 }
 | TIfdefMisc  { Ifdef, snd $1 }
 | TIfdefVersion { Ifdef, snd $1 }

cpp_other:
(* cppext: *)
 | TIdent "(" listc(argument) ")" ";"   { MacroTop ($1, ($2, $3, $4),Some $5)} 
 (* TCPar_EOL to fix the end-of-stream bug of ocamlyacc *)
 | TIdent "(" listc(argument) TCPar_EOL { MacroTop ($1, ($2, $3, $4),None) } 
  (* ex: EXPORT_NO_SYMBOLS; *)
 | TIdent ";"                           { MacroVarTop ($1, $2) }

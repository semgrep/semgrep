%{
(* Yoann Padioleau
 *
 * Copyright (C) 2002-2005 Yoann Padioleau
 * Copyright (C) 2006-2007 Ecole des Mines de Nantes
 * Copyright (C) 2008-2009 University of Urbana Champaign
 * Copyright (C) 2010-2014 Facebook
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
open Common
open Either_

open Ast_cpp
open Parser_cpp_mly_helper

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* This file contains a grammar for C/C++/Cpp.
 * See ast_cpp.ml for more information.
 *
 * reference:
 *  - orig_c.mly and orig_cpp.mly in this directory
 *  - http://www.nongnu.org/hcb/ for an up-to-date hyperlinked C++ grammar
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
%token <Tok.t> TUnknown

%token <Tok.t> EOF

(*-----------------------------------------*)
(* The space/comment tokens *)
(*-----------------------------------------*)
(* coupling: Token_helpers.is_real_comment and other related functions.
 * disappear in parse_cpp.ml via TH.is_comment in lexer_function *)
%token <Tok.t> TCommentSpace TCommentNewline TComment

(* fresh_token: cppext: appears after parsing_hack_pp and disappear *)
%token <(Token_cpp.cppcommentkind * Tok.t)> TComment_Pp
(* fresh_token: c++ext: appears after parsing_hack_pp and disappear *)
%token <(Token_cpp.cpluspluscommentkind * Tok.t)> TComment_Cpp

(*-----------------------------------------*)
(* The C tokens *)
(*-----------------------------------------*)
%token <Parsed_int.t>   TInt
%token <float option * Tok.t> TFloat
%token <string * Tok.t>       TChar TString

%token <string * Tok.t> TIdent
(* fresh_token: appear after some fix_tokens in parsing_hack.ml *)
%token <string * Tok.t> TIdent_Typedef

(* coupling: some tokens like TOPar and TCPar are used as synchronisation
 * point in parsing_hack.ml. So if you define a special token like
 * TOParDefine and TCParEOL, then you must take care to also modify
 * token_helpers.ml
 *)
%token <Tok.t> TOPar "(" TCPar ")"
%token <Tok.t> TOBrace "{" TCBrace "}"
%token <Tok.t> TOCro "[" TCCro "]"

%token <Tok.t> TDot TPtrOp     TInc TDec
%token <Tok.t> TComma "," TPtVirg ";"
%token <Ast_cpp.arithOp * Tok.t> TAssign
%token <Tok.t> TEq "=" TWhy "?"  TTilde TBang TCol ":" TEllipsis "..."
%token <Tok.t>
  TOrLog TAndLog "&&" TOr TXor TAnd "&"  TEqEq TNotEq TInfEq TSupEq
  TShl TShr
  TPlus TMinus TMul "*" TDiv TMod

(* sgrep-ext: *)
%token <Tok.t> LDots "<..." RDots "...>"

(*c++ext: see also TInf2 and TSup2 *)
%token <Tok.t> TInf TSup

%token <Tok.t>
  Tchar Tshort Tint Tdouble Tfloat Tlong Tunsigned Tsigned Tvoid
  Tauto Tregister Textern Tstatic
  Ttypedef
  Tconst Tvolatile
  Tstruct Tunion Tenum
  Tbreak Telse Tswitch Tcase Tcontinue Tfor Tdo Tif  Twhile Treturn
  Tgoto Tdefault
  Tsizeof

(* C99 *)
%token <Tok.t> Trestrict

(*-----------------------------------------*)
(* gccext: extra tokens *)
(*-----------------------------------------*)
%token <Tok.t> Tasm Ttypeof
(* less: disappear in parsing_hacks_pp, not present in AST for now *)
%token <Tok.t> Tattribute
(* also c++ext: *)
%token <Tok.t> Tinline

(*-----------------------------------------*)
(* cppext: extra tokens *)
(*-----------------------------------------*)

(* cppext: #define  *)
%token <Tok.t> TDefine
%token <(string * Tok.t)> TDefParamVariadic
(* transformed in TCommentSpace and disappear in parsing_hack.ml *)
%token <Tok.t> TCppEscapedNewline
(* fresh_token: appear after fix_tokens_define in parsing_hack_define.ml *)
%token <(string * Tok.t)> TIdent_Define
%token <Tok.t> TOPar_Define
%token <Tok.t> TCommentNewline_DefineEndOfMacro
%token <Tok.t> TOBrace_DefineInit

(* cppext: #include  *)
%token <(string * string * Tok.t)> TInclude

(* cppext: #ifdef *)
(* coupling: Token_helpers.is_cpp_instruction *)
%token <Tok.t>          TIfdef TIfdefelse TIfdefelif TEndif
%token <(bool * Tok.t)> TIfdefBool TIfdefMisc TIfdefVersion

(* cppext: other *)
%token <string * Tok.t> TUndef
%token <Tok.t> TCppDirectiveOther

(* cppext: special macros *)
(* fresh_token: appear after fix_tokens in parsing_hacks_pp.ml *)
%token <Tok.t>            TIdent_MacroStmt
%token <Tok.t>            TIdent_MacroString
%token <string * Tok.t> TIdent_MacroIterator
%token <string * Tok.t> TIdent_MacroDecl
%token <Tok.t>            Tconst_MacroDeclConst

(* fresh_token: appear after parsing_hack_pp.ml, alt to TIdent_MacroTop *)
%token <Tok.t> TCPar_EOL
(* fresh_token: appear after parsing_hack_pp.ml *)
%token <Tok.t> TAny_Action

(*-----------------------------------------*)
(* c++ext: extra tokens *)
(*-----------------------------------------*)
%token <Tok.t>
   Tclass Tthis
   Tnew Tdelete
   Ttemplate Ttypeid Ttypename
   Tcatch Ttry Tthrow
   Toperator
   Tpublic Tprivate Tprotected    Tfriend
   Tvirtual
   Tfinal Toverride
   Tnamespace Tusing
   Tbool    Tfalse Ttrue
   Twchar_t
   Tconst_cast Tdynamic_cast Tstatic_cast Treinterpret_cast
   Texplicit Tmutable
%token <Tok.t> TPtrOpStar TDotStar

%token <Tok.t> Tnull

%token <Tok.t> TColCol "::"

(* fresh_token: for constructed object, in parsing_hacks_cpp.ml *)
%token <Tok.t> TOPar_CplusplusInit
(* fresh_token: for template *)
%token <Tok.t> TInf_Template TSup_Template
(* fresh_token: for new[] delete[] *)
%token <Tok.t> TOCro_new TCCro_new
(* fresh_token: for pure virtual method. TODO add stuff in parsing_hack *)
%token <Tok.t> TInt_ZeroVirtual
(* fresh_token: why can't use TypedefIdent? conflict? *)
%token <string * Tok.t> TIdent_ClassnameInQualifier
(* fresh_token: appears after solved if next token is a typedef *)
%token <string * Tok.t> TIdent_ClassnameInQualifier_BeforeTypedef
(* fresh_token: just before <> *)
%token <string * Tok.t> TIdent_Templatename
(* for templatename as qualifier, before a '::' TODO write heuristic! *)
%token <string * Tok.t> TIdent_TemplatenameInQualifier
(* fresh_token: appears after solved if next token is a typedef *)
%token <string * Tok.t> TIdent_TemplatenameInQualifier_BeforeTypedef
(* fresh_token: for methods with same name as classname *)
%token <string * Tok.t> TIdent_Constructor
(* for cast_constructor, before a '(', unused for now *)
%token <string * Tok.t> TIdent_TypedefConstr
(* fresh_token: for constructed (basic) objects *)
%token <Tok.t>
  Tchar_Constr Tint_Constr Tfloat_Constr Tdouble_Constr Twchar_t_Constr
  Tshort_Constr Tlong_Constr Tbool_Constr
  Tsigned_Constr Tunsigned_Constr
(* fresh_token: appears after solved if next token is a typedef *)
%token <Tok.t> TColCol_BeforeTypedef

(*-----------------------------------------*)
(* c++0x: extra tokens *)
(*-----------------------------------------*)
%token <Tok.t>
   Tnullptr
   Tconstexpr
   Tthread_local
   Tdecltype

(* fresh_token: for new[] delete[] *)
%token <Tok.t> TOCro_Lambda

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
%start <Ast_cpp.program> main
%start <Ast_cpp.toplevel option> toplevel
%start <Ast_cpp.any> semgrep_pattern

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
 | X { [$1] }
 | listc(X) "," X { $1 @ [$3] }

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
 | function_definition            { X (D (Func ($1))) }
 | block_declaration              { X (D $1) }

(*************************************************************************)
(* toplevel *)
(*************************************************************************)

toplevel:
 | toplevel_aux { Some $1 }
 | EOF          { None }

toplevel_aux:
 | declaration         { X (D $1) }

 (* cppext: *)
 | cpp_directive       { CppDirective $1 }
 | cpp_ifdef_directive (*external_declaration_list ...*) { CppIfdef $1 }
 | cpp_other           { $1 }

 (* with error-recovery on, we can end up skipping the
  * beginning of the file, and so we can get trailing } unclosd at the end *)
 | "}" { X (D (EmptyDef $1)) }

(*************************************************************************)
(* semgrep *)
(*************************************************************************)

(* this is the entry point used by semgrep to parse patterns *)
semgrep_pattern:
 | expr                                         EOF  { Expr $1 }
 (* TODO: statement_or_decl_cpp below internally calls block_declaration
  * which is not the same than declaration, hence some extra rules
  * below which are 'declaration - block_declaration'
  *)
 | statement_or_decl_cpp                        EOF  { Toplevel ($1) }
 | statement_or_decl_cpp statement_or_decl_cpp+ EOF  { Toplevels ($1::$2) }
 (* declaration - block_declaration *)
 | namespace_definition EOF { Toplevel (X (D $1)) }

(*************************************************************************)
(* Ident, scope *)
(*************************************************************************)

id_expression:
 | unqualified_id { noQscope, $1 }
 | qualified_id   { $1 }

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
 | TEqEq  { BinaryOp (Logical Eq),    $1 }
 | TNotEq { BinaryOp (Logical NotEq), $1 }
 (* =    +=   -=   *=   /=   %=       ^=   &=   |=   >>=  <<=   *)
 | "="     { AssignOp (SimpleAssign $1), $1 }
 | TAssign { AssignOp (OpAssign $1), (snd $1) }
 (* ! ~ *)
 | TTilde { UnaryTildeOp, $1 } | TBang { UnaryNotOp,   $1 }
 (* , *)
 | "," { CommaOp,  $1 }
 (* +    -    *    /    %  *)
 | TPlus { BinaryOp (Arith Plus),  $1 }
 | TMinus { BinaryOp (Arith Minus), $1 }
 | "*"   { BinaryOp (Arith Mul),   $1 }
 | TDiv { BinaryOp (Arith Div), $1 } | TMod { BinaryOp (Arith Mod), $1 }
 (* ^ & |     <<   >>  *)
 | TOr { BinaryOp (Arith Or),  $1 } | TXor { BinaryOp (Arith Xor), $1 }
 | "&"  { BinaryOp (Arith And), $1  }
 | TShl { BinaryOp (Arith DecLeft), $1 }
 | TShr { BinaryOp (Arith DecRight), $1 }
 (* &&   || *)
 | TOrLog  { BinaryOp (Logical OrLog), $1 }
 | "&&" { BinaryOp (Logical AndLog), $1 }
 (* < >  <=   >=  *)
 | TInf { BinaryOp (Logical Inf), $1 } | TSup { BinaryOp (Logical Sup), $1}
 | TInfEq { BinaryOp (Logical InfEq), $1 }
 | TSupEq { BinaryOp (Logical SupEq), $1 }
 (* ++   -- *)
 | TInc { FixOp Inc, $1 } | TDec { FixOp Dec, $1 }
 (* ->*  -> *)
 | TPtrOpStar { PtrOpOp PtrStarOp, $1 } | TPtrOp { PtrOpOp PtrOp,     $1 }
 (* () [] (double tokens) *)
 | "(" ")" { AccessOp ParenOp, Tok.combine_toks $1 [$2] }
 | "[" "]" { AccessOp ArrayOp, Tok.combine_toks $1 [$2] }
 (* new delete *)
 | Tnew    { AllocOp NewOp,    $1 } | Tdelete { AllocOp DeleteOp, $1 }
 (*new[] delete[] (tripple tokens) *)
 | Tnew    TOCro_new TCCro_new
    { AllocOp NewArrayOp,  Tok.combine_toks $1 [$2;$3] }
 | Tdelete TOCro_new TCCro_new
    { AllocOp DeleteArrayOp, Tok.combine_toks $1 [$2;$3] }


qualified_id:
 | nested_name_specifier (*templateopt*) unqualified_id
   { $1, $2 }

nested_name_specifier:
 | class_or_namespace_name_for_qualifier "::" optl(nested_name_specifier)
   { ($1)::$3 }

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
     { ($1)::$3 }

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
 | cast_expr TAssign assign_expr { Assign ($1,OpAssign $2, Left $3)}
 | cast_expr "="     assign_expr { Assign ($1,SimpleAssign $2,Left $3)}
 (* c++ext: in treesitter it's a stmt, but in the spec it is an expr *)
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
 | pm_expr TDotStar   cast_expr   { DotStarAccess   ($1, (Dot, $2),$3) }
 | pm_expr TPtrOpStar cast_expr   { DotStarAccess ($1, (Arrow, $2), $3) }

cast_expr:
 | unary_expr                { $1 }
 | "(" type_id ")" cast_expr { Cast (($1, $2, $3), $4) }
 (* sgrep-ext: typed metavariable *)
 | "(" type_id TIdent ")" { Flag_parsing.sgrep_guard (TypedMetavar ($3, $2)) }

unary_expr:
 | postfix_expr            { $1 }
 | TInc unary_expr         { Prefix ((Inc, $1), $2) }
 | TDec unary_expr         { Prefix ((Dec, $1), $2) }
 | unary_op cast_expr      { Unary ($1, $2) }
 | Tsizeof unary_expr      { Call (IdSpecial (SizeOf, $1), Tok.unsafe_fake_bracket [Arg $2]) }
 | Tsizeof "(" type_id ")" { Call (IdSpecial (SizeOf, $1), ($2, [ArgType $3], $4)) }
 (* sgrep-ext: *)
 | Tsizeof "(" "..." ")"   { Call (IdSpecial (SizeOf, $1), ($2, [Arg (Ellipsis $3)], $4)) }
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

 | postfix_expr "[" expr "]"              { ArrayAccess ($1, ($2, [InitExpr $3],$4)) }
 | postfix_expr "(" optl(listc(argument)) ")" { mk_funcall $1 ($2, $3, $4) }

 (*c++ext: ident is now a id_expression *)
 | postfix_expr TDot   Ttemplate? "::"?  id_expression
     { let name = ($4, fst $5, snd $5) in DotAccess ($1,(Dot, $2),name) }
 | postfix_expr TPtrOp Ttemplate? "::"? id_expression
     { let name = ($4, fst $5, snd $5) in DotAccess($1,(Arrow, $2),name)  }

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
 | simple_type_specifier braced_init_list
    { let tx = addTypeD $1 nullDecl in
      let (ty, _, _) = type_and_storage_from_decl tx in
      ConstructedObject (ty, Inits $2) }
 (* less: should be typename, but require new parsing_hack_typedef *)
 | TIdent braced_init_list
    { let ty = nQ, TypeName (name_of_id $1) in
      ConstructedObject (ty, Inits $2)
    }


primary_expr:
 | literal { $1 }
 (* c++ext: *)
 | Tthis { IdSpecial (This, $1) }
 (*c++ext: cf below now. old: TIdent { Ident  (fst $1) [snd $1] }  *)

 (* forunparser: *)
 | "(" expr ")" { ParenExpr ($1, $2, $3) }
 (* gccext: allow statement as expressions via ({ statement }) *)
 | "(" compound ")"    { StatementExpr ($1, $2, $3) }

 (* contains identifier rule *)
 | primary_cplusplus_id { $1 }
 (* c++0x: *)
 | lambda_introducer compound
    { let (l, xs, r) = $1 in
      let ft_ret = nQ, TAuto l in
      let f_type = { ft_ret; ft_params = (l, [], r);
                     ft_specs = []; ft_const = None; ft_throw = []; ft_requires = None}
      in
      let fdef = { f_type; f_body = FBDef (Normal $2); f_specs = []}
      in
      Lambda ((l, xs, r), fdef)
    }
 | LDots expr RDots { DeepEllipsis ($1, $2, $3) }

literal:
 (* constants a.k.a literal *)
 | TInt    { C (Int    ($1)) }
 | TFloat  { C (Float  ($1)) }
 | TChar   { C (Char   ($1)) }
 | TString { C (String ($1)) }
 (* gccext: cppext: *)
 | string_elem string_elem+ { C (MultiString (List_.map (fun x -> StrLit x) ($1 :: $2))) }
 (*c++ext: *)
 | Ttrue   { C (Bool (true, $1)) }
 | Tfalse  { C (Bool (false, $1)) }
 (*c++0x: *)
 | Tnullptr { C (Nullptr $1) }
 | Tnull    { C (Nullptr $1) }

(*----------------------------*)
(* c++ext: *)
(*----------------------------*)

primary_cplusplus_id:
  | primary_cplusplus_id_inner { N ($1) }

(* can't factorize with following rule :(
 * | "::"? optl(nested_name_specifier) TIdent
 *)
primary_cplusplus_id_inner:
 | id_expression
     { let name = (None, fst $1, snd $1) in
       name }
 (* grammar_c++: is in qualified_id inside id_expression instead? *)
 | "::" TIdent
     { let name = Some $1, noQscope, IdIdent $2 in
       name }
 | "::" operator_function_id
     { let qop = $2 in
       let name = (Some $1, noQscope, qop) in
       name }
 | "::" qualified_id
     { let name = (Some $1, fst $2, snd $2) in
       name }

(*could use TInf here *)
cast_operator_expr:
 | cpp_cast_operator TInf_Template type_id  TSup_Template "(" expr ")"
     { CplusplusCast ($1, ($2, $3, $4), ($5, $6, $7)) }
(* TODO: remove once we don't skip template arguments *)
 | cpp_cast_operator "(" expr ")"
     { ExprTodo (("CppCast", $2), [$3]) }

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
     { let name = name_of_id $1 in
       let ft = nQ, (TypeName name) in
       ConstructedObject (ft, Args ($2, $3, $4))
     }
 | basic_type_2 "(" optl(listc(argument)) ")"
     { let ft = nQ, $1 in
       ConstructedObject (ft, Args ($2, $3, $4))
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
    { Delete ($1, $2, None, $3) }
 | "::"? Tdelete TOCro_new TCCro_new cast_expr
    { Delete ($1,$2, Some ($3,(),$4), $5) }

new_placement: "(" listc(argument) ")" { ($1, $2, $3) }

new_initializer: "(" optl(listc(argument)) ")" { Args($1, $2, $3) }

(*----------------------------*)
(* c++0x: lambdas! *)
(*----------------------------*)
lambda_introducer:
 | TOCro_Lambda                "]" { $1, [], $2 }
 | TOCro_Lambda lambda_capture "]" { $1, $2, $3 }

lambda_capture:
 | capture_list { $1 }
 | capture_default { [$1] }
 | capture_default "," capture_list { $1::$3 }

capture_default:
 | "&"  { CaptureRef $1 }
 | "="  { CaptureEq $1 }

capture_list:
 | capture                        { [CaptureOther $1] }
 | capture_list "," capture       { $1 @ [CaptureOther $3] }
 | capture_list "," capture "..."
    { $1 @ [CaptureOther (ParamPackExpansion ($3, $4))] }
 | capture "..."
    { [CaptureOther (ParamPackExpansion ($1, $2))] }

(* todo? could also be more general, like in tree-sitter-cpp and just
 * use 'expr', but then I get an additional s/r conflict
 *)
capture:
 (* expr subset *)
 | ident     { expr_of_id $1 }
 | "&" ident { Unary ((GetRef, $1), expr_of_id $2) }
 | Tthis     { IdSpecial (This, $1) }
 (* grammar_c++: not in latest *)
 | ident "=" assign_expr { Assign (expr_of_id $1, SimpleAssign $2, Left $3) }

(*----------------------------*)
(* gccext: *)
(*----------------------------*)

string_elem:
 | TString            { $1 }
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
 (* sgrep-ext: TODO would be better in primary_expr *)
 | "..." { Flag_parsing.sgrep_guard (Arg (Ellipsis $1)) }

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
 | Tchar_Constr    { TPrimitive (TChar, $1) }
 | Tint_Constr     { TPrimitive (TInt, $1) }
 | Tfloat_Constr   { TPrimitive (TFloat, $1) }
 | Tdouble_Constr  { TPrimitive (TDouble, $1) }

 | Twchar_t_Constr { TypeName (name_of_id ("wchar_t", $1)) }

 | Tshort_Constr   { TSized ([TShort, $1], None) }
 | Tlong_Constr    { TSized ([TLong, $1], None) }
 | Tbool_Constr    { TPrimitive (TBool, $1) }

(*************************************************************************)
(* Statements *)
(*************************************************************************)

statement:
 | compound        { Compound $1 }
 | expr_statement  { ExprStmt (fst $1, snd $1) }
 | labeled         { $1 }
 | selection       { $1 }
 | iteration       { $1 }
 | jump ";"        { Jump         ($1, $2) }

 (* cppext: *)
 | TIdent_MacroStmt { MacroStmt $1 }

 (* c++ext: *)
 | try_block { $1 }
 (* sgrep-ext: *)
 | "..." { Flag_parsing.sgrep_guard (ExprStmt (Some (Ellipsis $1), $1)) }

compound: "{" statement_or_decl_cpp* "}" { ($1, $2, $3) }

(* TODO: call declaration here? to factorize more? *)
statement_or_decl:
 | statement { (S $1) }

 (* gccext: Nested functions *)
 | function_definition { (D (Func $1)) }

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
 | block_declaration { (D $1) }

expr_statement: expr? ";" { $1, $2 }

(* Note that case 1: case 2: i++;    would be correctly parsed, but with
 * a Case  (1, (Case (2, i++)))  :(
 * TODO: there is a single statement, but in fact what follows ':' is
 * a list of stmt (tree-sitter-cpp does that), so
 * case 1: i++; break' is not parsed as Case (1, [i++; break]) but instead
 * as [Case (1, i++); break] so take care!
*)
labeled:
 | ident            ":" statement   { Label ($1, $2, $3) }
 (* always a single statement [S $X] in pfff, but a list in tree-sitter-cpp *)
 | Tcase const_expr ":" statement   { Case ($1, $2, $3, [S $4]) }
 | Tdefault         ":" statement   { Default ($1, $2, [S $3]) }
  (* gccext: allow range *)
 | Tcase const_expr "..." const_expr ":" statement
     { CaseRange ($1, $2, $3, $4, $5, [S $6]) }

(* classic else ambiguity resolved by a %prec, see conflicts.txt *)
selection:
 | Tif "(" condition ")" statement              %prec LOW_PRIORITY_RULE
     { If ($1, None, ($2, $3, $4), $5, None) }
 | Tif "(" condition ")" statement Telse statement
     { If ($1, None, ($2, $3, $4), $5, Some ($6, $7)) }
 | Tswitch "(" condition ")" statement
     { Switch ($1, ($2, $3, $4), $5) }

iteration:
 | Twhile "(" condition ")" statement
     { While ($1, ($2, $3, $4), $5) }
 | Tdo statement Twhile "(" expr ")" ";"
     { DoWhile ($1, $2, $3, ($4, $5, $6), $7) }
 | Tfor "(" for_init_stmt expr_statement expr? ")" statement
     { For ($1, ($2, ForClassic ($3, fst $4, $5), $6), $7) }
 (* c++ext: *)
 | Tfor "(" for_range_decl ":" for_range_init ")" statement
     { For ($1, ($2, ForRange (None, $3, $4, $5), $6), $7) }
 (* sgrep-ext: *)
 | Tfor "(" "..." ")" statement
     { For ($1, ($2, ForEllipsis $3, $4), $5) }
 (* cppext: *)
 | TIdent_MacroIterator "(" optl(listc(argument)) ")" statement
     { MacroIteration ($1, ($2, $3, $4), $5) }

(* the ';' in the caller grammar rule will be appended to the infos *)
jump:
 | Tgoto ident  { Goto ($1, $2) }
 | Tcontinue    { Continue $1 }
 | Tbreak       { Break $1 }
 | Treturn      { Return ($1, None) }
 | Treturn expr { Return ($1, Some (Arg $2)) }
 | Tgoto "*" expr { GotoComputed ($1, $2, $3) }

(*----------------------------*)
(* cppext: *)
(*----------------------------*)

statement_or_decl_cpp:
 | statement_or_decl { X $1 }

 (* cppext: *)
 | cpp_directive                                  { CppDirective $1 }
 | cpp_ifdef_directive(* stat_or_decl_list ...*)  { CppIfdef $1 }
 | cpp_macro_decl { $1 }

(*----------------------------*)
(* c++ext: *)
(*----------------------------*)

%inline
condition:
 | expr { None, CondClassic $1 }
 (* c++ext: *)
 | decl_spec_seq declaratori "=" initializer_clause
     { let (t_ret, _sto, mods) = type_and_storage_from_decl $1 in
       let (name, ftyp) = $2 in
       let ent = { name; specs = mods |> List.map (fun x -> M x) } in
       let var = { v_type = ftyp t_ret; v_init = Some (EqInit ($3, $4)) } in
       None, CondOneDecl (ent, var) }


for_init_stmt:
 | expr_statement { Left $1 }
 (* c++ext: for(int i = 0; i < n; i++)*)
 | simple_declaration { Right $1 }

for_range_decl: type_spec_seq2 declarator
  { let (t_ret, _sto, mods) = type_and_storage_from_decl $1 in
    let (name, ftyp) = $2 in
    let ent = { name; specs = mods |> List.map (fun x -> M x) } in
    ftyp t_ret, ent }

for_range_init: expr { InitExpr $1 }

try_block: Ttry compound handler+ { Try ($1, $2, $3) }

handler: Tcatch "(" exception_decl ")" compound { ($1, ($2, $3, $4), $5) }

exception_decl:
 | parameter_decl { ExnDecl $1 }
 (* old: but now handled via sgrep-ext: in parameter_decl
 | "..."          { [ExnDeclEllipsis $1] }
 *)

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
 | enum_specifier  { Right3 $1 }
 | class_specifier { Right3 (ClassDef $1) }

simple_type_specifier:
 | Tvoid                { Right3 (TPrimitive (TVoid, $1)) }
 | Tchar                { Right3 (TPrimitive (TChar, $1)) }
 | Tint                 { Right3 (TPrimitive (TInt, $1)) }
 | Tfloat               { Right3 (TPrimitive (TFloat, $1)) }
 | Tdouble              { Right3 (TPrimitive (TDouble, $1)) }
 | Tshort               { Middle3 (Short $1)}
 | Tlong                { Middle3 (Long $1)}
 | Tsigned              { Left3 (Signed $1)}
 | Tunsigned            { Left3 (UnSigned $1)}
 (*c++ext: *)
 | Tbool                { Right3 (TPrimitive (TBool, $1)) }
 | Twchar_t             { Right3 (TypeName (name_of_id ("wchar_t", $1)))}

 (* gccext: *)
 | Ttypeof "(" assign_expr ")" { Right3(TypeOf ($1,($2,Right $3,$4)))}
 | Ttypeof "(" type_id     ")" { Right3(TypeOf ($1,($2,Left $3,$4)))}

 (* history: cant put TIdent {} cos it makes the grammar ambiguous and
  * generates lots of conflicts => we must use some tricks.
  * See parsing_hacks_typedef.ml. See also conflicts.txt
  *)
 | type_cplusplus_id { Right3 (TypeName $1) }

 (* c++0x: *)
 | decltype_specifier { $1 }

decltype_specifier:
 (* c++0x: *)
 | Tdecltype "(" expr ")"           { Right3 (TypeTodo(("decltype", $1), [])) }
 (* TODO: because of wrong typedef inference *)
 | Tdecltype "(" TIdent_Typedef ")" { Right3 (TypeTodo(("decltype", $1), [])) }

(*todo: can have a ::opt optl(nested_name_specifier) before ident*)
elaborated_type_specifier:
 | Tenum ident                  { Right3 (EnumName ($1, name_of_id $2)) }
 | class_key ident              { Right3 (ClassName ($1, name_of_id $2)) }
 (* c++ext:  *)
 | Ttypename type_cplusplus_id  { Right3 (TypenameKwd ($1, (nQ, TypeName $2)))}

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
    { IdTemplated(IdIdent $1, ($2, $3, $4)) }

(*c++ext: in the c++ grammar they have also 'template-name' but this is
 * caught in my case by type_id and its generic TypedefIdent, or will be
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
 | Tconst    { Const, $1 }
 | Tvolatile { Volatile, $1 }
 (* C99 *)
 | Trestrict { Restrict, $1 }

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
 | direct_d         { $1 }

(* so must do  int * const p; if the pointer is constant, not the pointee *)
pointer:
 | "*"                        { fun x ->(nQ,         (TPointer ($1, x, [])))}
 | "*" cv_qualif_list         { fun x ->($2.qualifD, (TPointer ($1, x, [])))}
 | "*" pointer                { fun x ->(nQ,         (TPointer ($1, $2 x, [])))}
 | "*" cv_qualif_list pointer { fun x ->($2.qualifD, (TPointer ($1, $3 x, [])))}
 (*c++ext: no qualif for ref *)
 | "&"                        { fun x ->(nQ,    (TReference ($1, x)))}
 | "&" pointer                { fun x ->(nQ,    (TReference ($1, $2 x)))}
 (* c++0x: *)
 | "&&"                        { fun x ->(nQ,    (TRefRef ($1, x)))}
 | "&&" pointer                { fun x ->(nQ,    (TRefRef ($1, $2 x)))}

direct_d:
 | declarator_id
     { ($1, fun x -> x) }
 | "(" declarator ")"      (* forunparser: old: $2 *)
     { (fst $2, fun x -> (nQ, (ParenType ($1, (snd $2) x, $3)))) }
 | direct_d "["            "]"
     { (fst $1, fun x->(snd $1) (nQ,(TArray (($2,None,$3),x)))) }
 | direct_d "[" const_expr "]"
     { (fst $1, fun x->(snd $1) (nQ,(TArray (($2, Some $3, $4),x)))) }
 | direct_d "(" ")" const_opt exn_spec? optl(virtual_specifier+)
     { (fst $1, fun x-> (snd $1)
         (nQ, (TFunction {
           ft_ret= x; ft_params = ($2, [], $3);
           ft_specs = $6; ft_const = $4; ft_throw = Option.to_list $5; ft_requires = None; })))
     }
 | direct_d "(" parameter_type_list ")" const_opt exn_spec? optl(virtual_specifier+)
     { (fst $1, fun x-> (snd $1)
          (nQ,(TFunction {
            ft_ret = x; ft_params = ($2,$3,$4);
            ft_specs = $7; ft_const = $5; ft_throw = Option.to_list $6; ft_requires = None;})))
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
 (* TODO: bug? seems different order than in declarator *)
 | pointer direct_abstract_declarator { fun x -> x |> $2 |> $1 }

direct_abstract_declarator:
 | "(" abstract_declarator ")" (* forunparser: old: $2 *)
     { (fun x -> (nQ, (ParenType ($1, $2 x, $3)))) }
 | "["            "]"
     { fun x ->   (nQ, (TArray (($1,None, $2), x)))}
 | "[" const_expr "]"
     { fun x ->   (nQ, (TArray (($1, Some $2, $3), x)))}
 | direct_abstract_declarator "["            "]"
     { fun x ->$1 (nQ, (TArray (($2, None, $3), x))) }
 | direct_abstract_declarator "[" const_expr "]"
     { fun x ->$1 (nQ, (TArray (($2, Some $3, $4), x))) }
 | "(" ")"
     { fun x -> (nQ, (TFunction {
       ft_ret = x; ft_params = ($1,[],$2);
       ft_specs = []; ft_const = None; ft_throw = []; ft_requires = None })) }
 | "(" parameter_type_list ")"
     { fun x -> (nQ, (TFunction {
         ft_ret = x; ft_params = ($1,$2,$3);
         ft_specs = []; ft_const = None; ft_throw = []; ft_requires = None })) }
 | direct_abstract_declarator "(" ")" const_opt exn_spec?
     { fun x -> $1 (nQ, (TFunction {
         ft_ret = x; ft_params = ($2,[],$3);
         ft_specs = []; ft_const = $4; ft_throw = Option.to_list $5; ft_requires = None; })) }
 | direct_abstract_declarator "(" parameter_type_list ")" const_opt exn_spec?
     { fun x -> $1 (nQ, (TFunction {
         ft_ret = x; ft_params = ($2,$3,$4);
         ft_specs = []; ft_const = $5; ft_throw = Option.to_list $6; ft_requires = None })) }

(*-----------------------------------------------------------------------*)
(* Parameters (use decl_spec_seq not type_spec just for 'register') *)
(*-----------------------------------------------------------------------*)
%inline
parameter_type_list:
 | parameter_list           { $1 }
 (* old: but with sgrep-ext: part of parameter_decl
 | parameter_list "," "..." { $1 @ [ParamDots $3] }
 *)

parameter_decl:
 | decl_spec_seq declarator
     { let (t_ret, p_specs) = type_and_specs_from_decl $1 in
       let ii = Tok.unsafe_fake_tok "" in
       let (name, ftyp) = fixNameForParam ii $2 in
       P (make_param (ftyp t_ret) ~p_name:name ~p_specs) }
 | decl_spec_seq abstract_declarator
     { let (t_ret, p_specs) = type_and_specs_from_decl $1 in
       P (make_param ($2 t_ret) ~p_specs ) }

 | decl_spec_seq
     { let (t_ret, p_specs) = type_and_specs_from_decl $1 in
       P (make_param t_ret ~p_specs) }

(*c++ext: default parameter value, copy paste *)
 | decl_spec_seq declarator "=" assign_expr
     { let (t_ret, p_specs) = type_and_specs_from_decl $1 in
       let (name, ftyp) = fixNameForParam $3 $2 in
       P (make_param (ftyp t_ret) ~p_name:name ~p_specs ~p_val:($3, $4)) }
 | decl_spec_seq abstract_declarator "=" assign_expr
     { let (t_ret, p_specs) = type_and_specs_from_decl $1 in
       P (make_param ($2 t_ret) ~p_specs ~p_val:($3, $4)) }
 | decl_spec_seq "=" assign_expr
     { let (t_ret, p_specs) = type_and_specs_from_decl $1 in
       P (make_param t_ret ~p_specs ~p_val:($2,$3) ) }
 (* sgrep-ext: allowed only in last position in C, or in exn in C++ *)
 | "..." { ParamEllipsis $1 }

(*----------------------------*)
(* workarounds *)
(*----------------------------*)

parameter_list:
 | parameter_decl2                    { [$1] }
 | parameter_list "," parameter_decl2 { $1 @ [$3] }

parameter_decl2:
 | parameter_decl { $1 }
 (* when the typedef inference didn't work *)
 | TIdent
     { let t = nQ, (TypeName (name_of_id $1)) in
       P { p_name = None; p_type = t; p_val = None; p_specs = [] } }

(*----------------------------*)
(* c++ext: *)
(*----------------------------*)
(*c++ext: specialisation
 * TODO should be type-id-listopt. Also they can have qualifiers!
 * need typedef heuristic for throw() but can be also an expression ...
 *)
exn_spec:
 | Tthrow "(" ")"                        { ThrowSpec ($1, ($2, [], $3)) }
 | Tthrow "(" exn_name ")"               { ThrowSpec ($1, ($2, [$3], $4)) }
 | Tthrow "(" exn_name "," exn_name ")"  { ThrowSpec ($1, ($2, [$3; $5], $6)) }

exn_name: ident { nQ, TypeName (name_of_id $1) }

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
 | cv_qualif                    { {nullDecl with qualifD = [$1]} }
 | type_spec   spec_qualif_list { addTypeD $1 $2   }
 | cv_qualif   spec_qualif_list { addQualifD $1 $2 }

(* for pointers in direct_declarator and abstract_declarator *)
cv_qualif_list:
 | cv_qualif                  { {nullDecl with qualifD = [$1] } }
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
     { let (t_ret, _, _) = type_and_storage_from_decl $1 in
       $2 t_ret }

(* similar to abstract_declarator *)
new_declarator:
 | ptr_operator      %prec LOW_PRIORITY_RULE { $1 }
 |              direct_new_declarator  { $1 }
 (* TODO? bug? different order than in declarator? *)
 (* direct_new_declarator? typo? *)
 | ptr_operator new_declarator          { (fun t -> t |> $2 |> $1) }

ptr_operator:
 | "*" { fun t -> nQ, TPointer ($1, t, []) }
 (* c++ext: *)
 | "&" { fun t -> nQ, TReference ($1, t) }
 (* c++0x: *)
 | "&&" { fun t -> nQ, TRefRef ($1, t)  }

direct_new_declarator:
 | "[" expr "]"
    { (fun t -> nQ, TArray (($1, Some $2, $3), t)) }
 | direct_new_declarator "[" expr "]"
    { (fun t -> $1 (nQ, TArray (($2, Some $3, $4), t))) }

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
       let (t_ret, _, _) = type_and_storage_from_decl tx in
       $2 t_ret
     }
 | simple_type_specifier %prec LOW_PRIORITY_RULE
     { let tx = addTypeD $1 nullDecl in
       let (t_ret, _, _) = type_and_storage_from_decl tx in
       t_ret
     }

conversion_declarator:
 | ptr_operator %prec LOW_PRIORITY_RULE   { $1 }
 | ptr_operator conversion_declarator     { (fun x -> x |> $2 |> $1) }

(*************************************************************************)
(* Class and struct definitions *)
(*************************************************************************)

(* this can come from a simple_declaration/decl_spec *)
class_specifier: class_head "{" optl(member_specification) "}"
     { let (kind, nameopt, baseopt) = $1 in
       nameopt, { c_kind = kind;
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
     { $1, None, [] }
 | class_key ident base_clause?
     { let name = name_of_id $2 in
       $1, Some name, List_.optlist_to_list $3 }
 | class_key nested_name_specifier ident base_clause?
     { let name = name_of_id $3 in
       $1, Some name, List_.optlist_to_list $4 }

(* was called struct_union before *)
class_key:
 | Tstruct   { Struct, $1 }
 | Tunion    { Union, $1 }
 (*c++ext: *)
 | Tclass    { Class, $1 }

(*----------------------------*)
(* c++ext: inheritance rules *)
(*----------------------------*)
base_clause: ":" listc(base_specifier) { $2 }

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
 (* pfffonly? tree-sitter-cpp allows final or override but not virtual *)
 | Tvirtual access_specifier class_name
     { { i_name = $3; i_virtual = Some (Virtual $1); i_access = Some $2 } }

(* TODO? specialisation | ident { $1 }, do heuristic so can remove rule2 *)
class_name:
 | type_cplusplus_id  { $1 }
 | TIdent             { name_of_id $1 }

(*----------------------------*)
(* c++ext: members *)
(*----------------------------*)

(* todo? add cpp_directive possibility here too *)
member_specification:
 | member_declaration optl(member_specification)
     { X $1::$2 }
 | access_specifier ":" optl(member_specification)
     { X (Access ($1, $2))::$3 }

access_specifier:
 | Tpublic    { Public, $1 }
 | Tprivate   { Private, $1 }
 | Tprotected { Protected, $1 }


(* in c++ grammar there is a ;opt after function_definition but
   * there is a conflict as it can also be an EmptyField *)
member_declaration:
 | field_declaration      { fixFieldOrMethodDecl $1 }
 | function_definition    { F (Func ($1)) }
 | qualified_id ";"
     { let name = (None, fst $1, snd $1) in
       QualifiedIdInClass (name, $2)
     }
 | using_declaration      { F (UsingDecl $1) }
 | template_declaration   { F ($1) }

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
 | ";"    { F (EmptyDef $1) }

(*-----------------------------------------------------------------------*)
(* field declaration *)
(*-----------------------------------------------------------------------*)
field_declaration:
 | decl_spec_seq ";"
     { (* gccext: allow empty elements if it is a structdef or enumdef *)
       let (t_ret, sto, mods) = type_and_storage_from_decl $1 in
       let onedecl = make_onedecl t_ret ~v_namei:None ~sto ~mods in
       ([onedecl], $2)
     }
 | decl_spec_seq listc(member_declarator) ";"
     { let (t_ret, sto, _inline) = type_and_storage_from_decl $1 in
       ($2 |> (List.map (fun (f) -> f t_ret sto)), $3)
     }

(* was called struct_declarator before *)
member_declarator:
 | declarator
     { let (name, partialt) = $1 in
       (fun t_ret sto ->
         make_onedecl (partialt t_ret)
           ~v_namei:(Some (DN name, None)) ~sto ~mods:[]
         )
     }
 (* can also be an abstract when it's =0 on a function type *)
 | declarator "=" const_expr
     { let (name, partialt) = $1 in
       (fun t_ret sto ->
         make_onedecl (partialt t_ret)
          ~v_namei:(Some (DN name, Some (EqInit ($2, InitExpr $3)))) ~sto
          ~mods:[]
       )
     }

 (* normally just ident, but ambiguity so solve by inspetcing declarator *)
 | declarator ":" const_expr
     { let (name, _partialt) = fixNameForParam $2 $1 in
       (fun t_ret _stoTODO ->
         (BitField (Some name, $2, t_ret, $3)))
     }
 | ":" const_expr
     { (fun t_ret _stoTODO -> BitField (None, $1, t_ret, $2)) }

(*************************************************************************)
(* Enum definition *)
(*************************************************************************)

(* gccext: c++0x: most ","? are trailing commas extensions (as in Perl) *)
enum_specifier:
 | enum_head "{" listc(enumerator) ","? "}"
     { EnumDef ({enum_kind = fst $1; enum_name = snd $1;
                enum_body = ($2, List.flatten $3, $5)}) (*$4*) }
 (* c++0x: *)
 | enum_head "{" "}"
     { EnumDef ({enum_kind = fst $1; enum_name = snd $1;
                 enum_body = ($2, [], $3) }) }

enum_head:
 | enum_key ident? (*ioption(enum_base)*)
     { $1, $2 |> Option.map name_of_id }

enumerator:
 | ident                { [ X { e_name = $1; e_val = None; } ] }
 | ident "=" const_expr { [ X { e_name = $1; e_val = Some ($2, $3); } ] }

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
     { let (t_ret, sto, mods) = type_and_storage_from_decl $1 in
       ([make_onedecl t_ret ~v_namei:None ~mods ~sto],$2)
     }
 | decl_spec_seq listc(init_declarator) ";"
     { let (t_ret, sto, mods) = type_and_storage_from_decl $1 in
       (
         $2 |> List.map (fun (((name, f), iniopt)) ->
           (* old: if fst (unwrap storage)=StoTypedef then LP.add_typedef s; *)
           make_onedecl (f t_ret) ~v_namei:(Some (DN name, iniopt)) ~mods ~sto
         ), $3)
     }


(*-----------------------------------------------------------------------*)

decl_spec_seq:
 | decl_spec               { $1 nullDecl }
 | decl_spec decl_spec_seq { $1 $2 }

decl_spec:
 | storage_class_spec { addStorageD $1 }
 | type_spec          { addTypeD  $1 }
 | function_spec      { addModifierD $1 }
 (* c++ext: *)
 | type_qualifier     { (fun x -> x) (* addSpecifierD $1*) (* TODOAST *) }

(* grammar_c++: cv_qualif is not here but instead inline in type_spec.
 * I prefer to keep as before but I take care when
 * they speak about type_spec to translate instead in type+qualif_spec
 * (which is spec_qualif_list)*)
 | cv_qualif          { addQualifD $1 }

 | Ttypedef           { addStorageD (StoTypedef $1) }
 | Tfriend            { fun x -> x (* addModifierD (Friend $1) TODOAST *) }

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
 | Tinline { Inline $1 }
 (*c++ext: *)
 | Tvirtual { Virtual $1 }

storage_class_spec:
 | Tstatic      { Sto (Static,  $1) }
 | Textern      { Sto (Extern,  $1) }
 (* c++ext: now really used, not as in C, for type inferred variables *)
 | Tauto        { Sto (Auto,    $1) }
 | Tregister    { Sto (Register,$1) }
 (* c++11 *)
 | Tthread_local { Sto (ThreadLocal, $1) }

type_qualifier:
 (* c++ext: also considered a storage class specifier in the spec *)
 | Tmutable     { TQ (Mutable, $1) }
 (* c++?: *)
 | Tconstexpr   { TQ (Constexpr, $1) }

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
     { ($1, Some (ObjInit (Args ($2, $3, $4)))) }

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
 | initialize2                        { [$1] }
 | initialize_list "," initialize2 { ($3)::$1 }


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
 | TDot ident   { DesignatorField (Some $1, $2) }
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
 | simple_declaration { DeclList $1 }
 (*gccext: *)
 | asm_definition     { $1 }
 (*c++ext: *)
 | namespace_alias_definition { $1 }
 | using_declaration { UsingDecl $1 }
 | using_directive   { UsingDecl $1 }

(*----------------------------*)
(* c++ext: *)
(*----------------------------*)

namespace_alias_definition: Tnamespace TIdent "=" qualified_namespace_spec ";"
  { NamespaceAlias ($1, $2, $3, $4, $5) }

using_directive: Tusing Tnamespace qualified_namespace_spec ";"
  { $1, UsingNamespace ($2, $3), $4 }

qualified_namespace_spec: "::"? optl(nested_name_specifier) namespace_name
  { ($1, $2, IdIdent $3) }

(* conflict on TColCol in 'Tusing TColCol unqualified_id ";"'
 * need LALR(2) to see if after tcol have a nested_name_specifier
 * or put opt on nested_name_specifier too *)
using_declaration:
 | Tusing Ttypename? "::"? nested_name_specifier unqualified_id ";"
     { let name = ($3, $4, $5) in $1, UsingName name, $6 (*$2*) }
(* TODO: remove once we don't skip qualifier ? *)
 | Tusing Ttypename? "::"? unqualified_id ";"
     { let name = ($3, [], $4) in $1, UsingName name, $5 (*$2*) }

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
 | TString                  { ColonMisc [snd $1] }
 | TString "(" asm_expr ")" { ColonExpr ([snd $1], ($2, $3, $4)) }
 (* cppext: certainly a macro *)
 | "[" TIdent "]" TString "(" asm_expr ")"
     { ColonExpr ([$1;snd $2;$3;snd $4], ($5, $6, $7))  }
 | TIdent                   { ColonMisc [snd $1] }
 | (* empty *)              { ColonMisc [] }

asm_expr: assign_expr { $1 }

(*************************************************************************)
(* Declaration, in C++ sense *)
(*************************************************************************)
(* In the C++ grammar they have 'explicit_instantiation' but it is equal to
 * template_declaration and so is ambiguous.
 *
 * declaration > block_declaration > simple_declaration, hmmm
 * could be renamed declaration_or_definition
 *)
declaration:
 | block_declaration                 { $1 }
 | function_definition               { Func ($1) }

 (* not in c++ grammar as merged with function_definition, but I can't *)
 | ctor_dtor { $1 }
 | template_declaration              { $1 }
 | linkage_specification             { $1 }
 | namespace_definition              { $1 }

 (* sometimes the function ends with }; instead of just } *)
 | ";"    { EmptyDef $1 }

(*----------------------------*)
(* cppext: *)
(*----------------------------*)

declaration_cpp:
 | declaration { X (D $1) }
 (* cppext: *)
 | cpp_directive                                 { CppDirective $1 }
 | cpp_ifdef_directive(* stat_or_decl_list ...*) { CppIfdef $1 }
 (* sgrep-ext: *)
 | "..." { Flag_parsing.sgrep_guard (X (S (ExprStmt (Some (Ellipsis $1), $1)))) }

(*----------------------------*)
(* c++ext: *)
(*----------------------------*)

template_declaration:
  Ttemplate TInf_Template optl(listc(template_parameter)) TSup_Template declaration
   { TemplateDecl ($1, ($2, $3, $4), None, $5) }

(*todo: '| type_parameter'
   * ambiguity with parameter_decl cos a type can also be 'class X'
 | Tclass ident { raise Todo }
   *)
template_parameter:
 | parameter_decl { TP $1 }


(* c++ext: could also do a extern_string_opt to factorize stuff *)
linkage_specification:
 | Textern TString declaration
     { ExternDecl ($1, $2, $3) }
 | Textern TString "{" optl(declaration_cpp+) "}"
     { ExternList ($1, $2, ($3, $4, $5))}


namespace_definition:
 | named_namespace_definition   { $1 }
 | unnamed_namespace_definition { $1 }

(* in c++ grammar they make diff between 'original' and 'extension' namespace
 * definition but they require some contextual information to know if
 * an identifier was already a namespace. So here I have just a single rule.
 *)
named_namespace_definition:
 | Tnamespace TIdent "{" optl(declaration_cpp+) "}"
     { Namespace ($1, Some (None, [], IdIdent $2), ($3, $4, $5)) }

unnamed_namespace_definition: Tnamespace "{" optl(declaration_cpp+) "}"
     { Namespace ($1, None, ($2, $3, $4)) }

(*************************************************************************)
(* Function definition *)
(*************************************************************************)
virtual_specifier:
| Tfinal { M (Final ($1)) }
| Toverride { M (Override ($1)) }

function_definition:
 | decl_spec_seq declarator function_body
     { let (t_ret, sto) = type_and_storage_for_funcdef_from_decl $1 in
       let ii = Tok.unsafe_fake_tok "" in
       let x = (fst $2, fixOldCDecl ii ((snd $2) t_ret), sto) in
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
 | compound { FBDef (Normal $1) }

(*-----------------------------------------------------------------------*)
(* c++ext: constructor special case *)
(*-----------------------------------------------------------------------*)

(* Special case cos ctor/dtor because they do not have a return type.
 * This is when the ctor/dtor are defined outside the class.
 *)
ctor_dtor:
 | nested_name_specifier TIdent_Constructor "(" parameter_type_list? ")"
     ctor_mem_initializer_list
     compound
     { (Func ((mk_constructor [] $2 ($3, $4, $5) (FBDef (Constr ($6, $7)))))) (*TODO$1*) }
  (* new_type_id, could also introduce a Tdestructorname or forbidy the
      TypedefIdent2 transfo by putting a guard in the lalr(k) rule by
      checking if have a ~ before
   *)
 | nested_name_specifier TTilde ident "(" Tvoid? ")" exn_spec? compound
    { (Func ((mk_destructor [] $2 $3 ($4, $5, $6) $7 (FBDef (Normal $8))))) }

(* TODO: remove once we don't skip qualifiers *)
 | Tinline? TIdent_Constructor "(" parameter_type_list? ")"
     ctor_mem_initializer_list
     compound
     { DeclTodo ("DeclCtor", $3) }
 | TTilde ident "(" Tvoid? ")" exn_spec? compound
     { (Func ((mk_destructor [] $1 $2 ($3, $4, $5) $6 (FBDef (Normal $7))))) }



(* Special case for ctor/dtor because they do not have a return type.
 * This is when the ctor/dtor are defined inside the class.
 * TODOAST on the ctor_spec and chain of calls
*)
ctor_dtor_member:
 | ctor_spec TIdent_Constructor "(" parameter_type_list? ")"
     ctor_mem_initializer_list
     compound
     { F (Func ((mk_constructor $1 $2 ($3, $4, $5) (FBDef (Constr ($6, $7)))))) }
 | ctor_spec TIdent_Constructor "(" parameter_type_list? ")" ";"
     { F (Func (mk_constructor $1 $2 ($3, $4, $5) (FBDecl $6))) }
 | ctor_spec TIdent_Constructor "(" parameter_type_list? ")" "=" Tdelete ";"
     { F (Func (mk_constructor $1 $2 ($3, $4, $5) (FBDelete ($6, $7, $8)))) }
 | ctor_spec TIdent_Constructor "(" parameter_type_list? ")" "=" Tdefault ";"
     { F (Func (mk_constructor $1 $2 ($3, $4, $5) (FBDefault ($6, $7, $8)))) }

 | dtor_spec TTilde ident "(" Tvoid? ")" exn_spec? compound
     { F (Func ((mk_destructor $1 $2 $3 ($4, $5, $6) $7 (FBDef (Normal $8))))) }
 | dtor_spec TTilde ident "(" Tvoid? ")" exn_spec? ";"
     { F (Func (mk_destructor $1 $2 $3 ($4, $5, $6) $7 (FBDecl $8))) }
 | dtor_spec TTilde ident "(" Tvoid? ")" exn_spec? "=" Tdelete ";"
     { F (Func (mk_destructor $1 $2 $3 ($4, $5, $6) $7 (FBDelete ($8, $9, $10)))) }
 | dtor_spec TTilde ident "(" Tvoid? ")" exn_spec? "=" Tdefault ";"
     { F (Func (mk_destructor $1 $2 $3 ($4, $5, $6) $7 (FBDelete ($8, $9, $10)))) }


ctor_spec:
 | Texplicit { [M (Explicit ($1, None))] }
 | Tinline   { [M (Inline $1)] }
 | (*empty*) { [] }

dtor_spec:
 | Tvirtual { [M (Virtual $1)] }
 | Tinline  { [M (Inline $1)] }
 | (*empty*) { [] }

ctor_mem_initializer_list:
 | ":" listc(mem_initializer) { $2 }
 | (* empty *) { [] }

mem_initializer:
 | mem_initializer_id "(" optl(listc(argument)) ")" { $1, Args ($2, $3, $4) }

(* factorize with declarator_id ? specialisation *)
mem_initializer_id:
(* specialisation | TIdent { () } *)
 | primary_cplusplus_id_inner { $1 }

(*************************************************************************)
(* Cpp directives *)
(*************************************************************************)

(* cppext: *)
cpp_directive:
 | TInclude
     { let (_include_str, filename, tok) = $1 in
       (* redo some lexing work :( *)
       let inc_kind =
         match () with
         | _ when filename =~ "^\"\\(.*\\)\"$" ->
          IncLocal (Common.matched1 filename, tok)
         | _ when filename =~ "^\\(\\<.*\\>\\)$" ->
          IncSystem (Common.matched1 filename, tok)
         | _ ->
          IncOther (N (name_of_id (filename, tok)))
       in
       Include (tok, inc_kind)
     }

 | TDefine TIdent_Define define_val TCommentNewline_DefineEndOfMacro
     { Define ($1, $2, DefineVar, $3) (*$4??*) }

 (* The TOPar_Define is introduced to avoid ambiguity with previous rules.
  * A TOPar_Define is a TOPar that was just next to the ident (no space).
  * See parsing_hacks_define.ml
  *)
 | TDefine TIdent_Define TOPar_Define optl(listc(param_define)) ")"
    define_val TCommentNewline_DefineEndOfMacro
     { Define ($1, $2, (DefineMacro ($3, $4, $5)), $6) (*$7*) }

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
       | (C (Int ((_, tok) as pi))) when Parsed_int.eq_const pi 0 ->
         DefineDoWhileZero ($1, $2, $3, ($4, tok, $6))
       | _ -> raise Parsing.Parse_error
     }
 (* for statement-like macro with varargs *)
 | Tif "(" expr ")" id_expression
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
 | TIfdef     { Ifdef $1 }
 | TIfdefelse { IfdefElse $1 }
 | TIfdefelif { IfdefElseif $1 }
 | TEndif     { IfdefEndif $1 }

 | TIfdefBool  { Ifdef (snd $1) }
 | TIfdefMisc  { Ifdef (snd $1) }
 | TIfdefVersion { Ifdef (snd $1) }

(* cppext: *)
cpp_macro_decl:
 | TIdent_MacroDecl "(" listc(argument) ")" ";"
     { MacroDecl ([], $1, ($2, $3, $4), $5) }
 | Tstatic TIdent_MacroDecl "(" listc(argument) ")" ";"
     { MacroDecl ([ST (Static, $1)], $2, ($3, $4, $5), $6) }
 | Tstatic Tconst_MacroDeclConst
    TIdent_MacroDecl "(" listc(argument) ")" ";"
     { MacroDecl ([ST (Static, $1); TQ (Const, $2)], $3, ($4, $5, $6), $7) }

cpp_other:
(* cppext: *)
 | TIdent "(" listc(argument) ")" ";"   { MacroDecl ([],$1, ($2, $3, $4), $5)}
 (* TCPar_EOL to fix the end-of-stream bug of ocamlyacc *)
 | TIdent "(" listc(argument) TCPar_EOL { MacroDecl ([], $1, ($2, $3, $4),$4)}
  (* ex: EXPORT_NO_SYMBOLS; *)
 | TIdent ";"                           { MacroVar ($1, $2) }

%{
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

(* Empty grammar. Use tree-sitter-csharp instead!
 *
 * This grammar is just used to define the tokens for the C# lexer
 * in lexer_csharp.mll used in efuns/codemap to at least colorize C# tokens.
 *
 * references:
 *  - http://www.jaggersoft.com/csharp_grammar.html
 *)

%}

/*(*************************************************************************)*/
/*(* tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(* the comment tokens *)*/
/*(*-----------------------------------------*)*/

/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Parse_info.t> TCommentSpace TCommentNewline   TComment
%token <Parse_info.t> TCommentMisc

/*(*-----------------------------------------*)*/
/*(* the normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with "values" *)*/
%token <string * Parse_info.t> TInt
%token <string * Parse_info.t> TFloat
%token <string * Parse_info.t> TChar
%token <string * Parse_info.t> TString

%token <string * Parse_info.t> TIdent

/*(* keywords tokens *)*/
%token <Parse_info.t>
  Tbool Tbyte Tchar Tvoid Tdouble Tfloat Tshort Tint Tlong Tstring Tsbyte
  Tushort Tuint Tulong
  Tclass Tabstract Tvirtual Tdelegate Tthis   Tinterface Tnew Tobject
  Tprivate Tprotected Tpublic
  Treturn
  Tbreak Tcontinue
  Tswitch Tcase Tdefault
  Tenum   Tstruct
  Tconst
  Tunsafe
  Tnamespace Tusing
  Tstatic Tvolatile Textern
  Tif Telse
  Tdo  Twhile
  Tfor Tforeach
  Tgoto
  Tthrow  Ttry  Tcatch  Tfinally
  Tchecked Tunchecked
  Tnull
  Ttrue Tfalse
  Tref Tout

  Tas Tbase  Tdecimal Tevent Texplicit Tfixed Timplicit
  Tin Tinternal Tis Tlock
  Toperator  Toverride Tparams Treadonly
  Tsealed Tsizeof Tstackalloc  Ttypeof

/*(* cpp *)*/
%token <Parse_info.t>
  TCppLine TCppError TCppWarning
  TCppRegion TCppEndRegion
  TDefine TUndef
  TIfdefIf TIfdefElif TIfdefElse TIfdefEndif

/*(* syntax *)*/
%token <Parse_info.t> TOParen TCParen
%token <Parse_info.t> TOBracket TCBracket
%token <Parse_info.t> TOBrace TCBrace
%token <Parse_info.t> TOAngle TCAngle


%token <Parse_info.t>
 TComma TColon TDot TSemiColon
 TStar TDiv TPercent
 TEq TEqEq TNotEq
 TPlus TMinus
 TTilde
 TAnd TOr TXor
 TLess TMore TMoreEq TLessEq
 TQuestion
 TInc TDec
 TBang TTilde
 TAndAnd TOrOr
 TArrow

%token <string * Parse_info.t> TAssignOp


/*(* operators *)*/

/*(*-----------------------------------------*)*/
/*(* extra tokens: *)*/
/*(*-----------------------------------------*)*/

/*(* classic *)*/
%token <Parse_info.t> TUnknown
%token <Parse_info.t> EOF

/*(*-----------------------------------------*)*/
/*(* priorities *)*/
/*(*-----------------------------------------*)*/

/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <Ast_csharp.program> main

%%

/*(*************************************************************************)*/
/*(* Toplevel, compilation units *)*/
/*(*************************************************************************)*/

main: EOF { () }

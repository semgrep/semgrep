%{
(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
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
(*
 *)

%}
/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

/*(* unrecognized token, will generate parse error *)*/
%token <Parse_info.t> TUnknown

%token <Parse_info.t> EOF

/*(*-----------------------------------------*)*/
/*(*2 The space/comment tokens *)*/
/*(*-----------------------------------------*)*/

/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Parse_info.t> TCommentSpace TCommentNewline   TComment

/*(*-----------------------------------------*)*/
/*(*2 The normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with "values" *)*/
%token <string * Parse_info.t> TInt TFloat TChar TString
%token <string * Parse_info.t> TLowerIdent TUpperIdent

/*(* keywords tokens *)*/
%token <Parse_info.t>
 Tconst Tfun
 Talias
 Tasync Tawait
 Ttry Tcatch
 Tclass Ttrait Textends Tchildren Tfinal
 Tprivate Tprotected
 Tthis
 Tif Telse
 Tmatch
 Tfrom
 Tmodule
 Tmutable
 Tnative
 Tuses
 Tstatic
 Tthrow
 Tas

 Ttype
 Tvoid
 Twatch
 Twhen Twith

      /* conditional keywords */
 Tbase
 Tcapture
 Tdefault
 Tdeferred
 Tinst
 TnonNullable
 Tthis
 Tuntracked
 Tvalue

      /* not in original spec */
 Ttrue
 Tfalse
 Toverridable Treadonly Tmacro
 Tfor Tin
 Textension
 Tyield Tbreak Tcontinue
 Tmemoized Tfrozen
 Tdo Twhile Tloop


/*(* syntax *)*/
%token <Parse_info.t>
 TEq TEqDot
 TOParen TCParen TOBrace TCBrace TOBracket TCBracket
 TDot  TArrow TTildeArrow TEqualArrow
 TComma
 TColon TColonColon
 TSemiColon TSemiColonSemiColon
 TPipe
 TQuestion TAt TSharp TDollar TBackquote

/*(* operators *)*/
%token <Parse_info.t>
  TPlus TMinus TStar TDiv TMod
  TEqEq TBangEq
  TLess TGreater TLessEq TGreaterEq
  TAnd THat TBang /* TPipe above */
  TAndAnd TPipePipe


/*(* attributes *)*/

/*(*-----------------------------------------*)*/
/*(*2 extra tokens: *)*/
/*(*-----------------------------------------*)*/


/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <Ast_skip.program> main

%%

/*(*************************************************************************)*/
/*(*1 Toplevel *)*/
/*(*************************************************************************)*/

main:      EOF                        { raise Parsing.Parse_error }


/*(*************************************************************************)*/
/*(*1 Names *)*/
/*(*************************************************************************)*/


/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

/*(*----------------------------*)*/
/*(*2 Constants *)*/
/*(*----------------------------*)*/


/*(*************************************************************************)*/
/*(*1 Patterns *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(*1 Types *)*/
/*(*************************************************************************)*/

/*(*----------------------------*)*/
/*(*2 Types definitions *)*/
/*(*----------------------------*)*/

/*(*----------------------------*)*/
/*(*2 Types expressions *)*/
/*(*----------------------------*)*/

/*(*************************************************************************)*/
/*(*1 Functions *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(*1 Classes *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(*1 Modules *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(*1 Attributes *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(*1 xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

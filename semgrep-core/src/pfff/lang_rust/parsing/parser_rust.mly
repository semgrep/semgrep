%{
(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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
 * http://doc.rust-lang.org/rust.html ??
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
 Tif Telse
 Twhile Tfor Tloop
 Tbreak  Tcontinue
 Treturn
 Tlet Tin Tmatch
 Ttrue Tfalse
 Tfn
 Ttype Tenum
 Tstruct Ttrait Timpl
 Tself Tsuper
 Tcrate Tuse
 Tstatic  Textern
 Tpub Tpriv

 Tmut
 Tas
 Tbox
 Tref
 Tmod
 Tproc
 Tunsafe

/*(* cpp *)*/
%token <Parse_info.t>
  TCppLine

/*(* syntax *)*/
%token <Parse_info.t> TOParen TCParen
%token <Parse_info.t> TOBracket TCBracket
%token <Parse_info.t> TOBrace TCBrace
%token <Parse_info.t> TOAngle TCAngle


%token <Parse_info.t>
 TComma TColon TColonColon TSemiColon
 TStar TDiv TPercent
 TEq TEqEq TNotEq
 TPlus TMinus
 TTilde
 TAnd TOr TXor
 TLess TMore TMoreEq TLessEq
 TAndAnd TOrOr
 TArrow TDot
 TPound

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
%type <Ast_rust.program> main

%%

/*(*************************************************************************)*/
/*(* TOC *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* Toplevel, compilation units *)*/
/*(*************************************************************************)*/

main: EOF { () }

/*(*************************************************************************)*/
/*(* Names *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* Expressions *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* Classes *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* Misc *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

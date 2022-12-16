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

(*
 * src: http://www.erlang.org/download/erl_spec47.ps.gz appendix E
 * and erlang-otp/lib/compiler/src/core_parse.yrl
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

/*(* they call atom TIdent, but I prefer TIdent for consistency *)*/
%token <string * Parse_info.t> TIdent TVariable

/*(* keywords tokens *)*/
%token <Parse_info.t>
 Tif Tcond Twhen Tcase
 Tbegin Tend
 Tlet Tof
 Tfun
 Tafter
 Tquery Tcatch Treceive

/*(* syntax *)*/
%token <Parse_info.t> TOParen TCParen
%token <Parse_info.t> TOBracket TCBracket
%token <Parse_info.t> TOBrace TCBrace

%token <Parse_info.t>
 TDot TColon TSemiColon TComma TQuestion
 TPipe TPipePipe TArrow TSharp
 TUnderscore

/*(* operators *)*/
%token <Parse_info.t>
 TPlus TMinus TStar TDiv
 Tdiv Trem Tor Txor Tbor Tbxor Tbsl Tbsr Tand Tband Tnot Tbnot
 TEqEq TSlashEq
 TEqColonEq TEqSlashEq
 TLess TMore
 TLessEq TMoreEq
 TInc TDec
 TEq TBang TAssign

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
%type <Ast_erlang.program> main

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

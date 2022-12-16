/* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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
 */
%{

(*************************************************************************)
(* Prelude *)
(*************************************************************************)

(*
 * This file is mainly used to define the tokens. We could have
 * moved the token definitions directly in lexer_html.mll, as it was
 * done originally in ocamlnet/netstring, but we now also use this
 * file to defines a basic grammar to parse strict html.
 * Moreover it is more symetric with what we do in the other
 * lang_xxx/ directories.
 * Finally it's a good exercise to define a simple grammar using the
 * tokens; it is as a side effect documenting some of those tokens.
 *)
%}

/*(*************************************************************************)*/
/*(* Tokens *)*/
/*(*************************************************************************)*/

%token <Ast_html.info> TComment  /*(* <!-- ... --> *)*/
%token <Ast_html.info> TDoctype  /*(* <! ... > *)*/
%token <Ast_html.info> TPi       /*(* <? ... ?> or >  *)*/

%token <Ast_html.info * string>   Lelement
%token <Ast_html.info * string>   Lelementend
%token <Ast_html.info> Relement  /*(* > *)*/
%token <Ast_html.info> Relement_empty   /*(* />, for XML compat *)*/
%token <Ast_html.info * string> Cdata
%token <Ast_html.info * string> CdataSpecial
%token <Ast_html.info * string> Space
%token <Ast_html.info * string> Name
%token <Ast_html.info> Eq
%token <Ast_html.info * string> Literal
%token <Ast_html.info> Other

/*(*-----------------------------------------*)*/
%token <Ast_html.info> EOF

/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <unit> main

%%

/*(* assume comments and doctype are filtered out and pi are forbidden *)*/
main:
 | html_list EOF { }

html:
 | Lelement attr_list Relement_empty { }
 | Lelement attr_list Relement html_list Lelementend Relement { }
 | Cdata { }

attr:
 | Name Eq value { }

value:
 | Literal { }

/*(*************************************************************************)*/
/*(* xxx_list, xxx_opt *)*/
/*(*************************************************************************)*/

html_list:
 | html_list html { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

attr_list:
 | attr_list attr { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

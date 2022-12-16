/*
 * Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
 * Copyright (C) 2011 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in the license.txt file.
 */
%{
open Ast_css

(*************************************************************************)
(* Prelude *)
(*************************************************************************)

(*
 * spec: http://www.w3.org/TR/CSS2/grammar.html
 * The official grammar contains a space token ... hmmm
 * old spec: http://www.w3.org/TR/REC-CSS1/#appendix-b
 * see also: http://www.w3.org/TR/CSS2/syndata.html
 *
 * Most of the code in this file is copy pasted from Dario Teixera
 * css parser and preprocessor: http://forge.ocamlcore.org/projects/ccss/
 * I've mostly converted it from menhir to ocamlyacc.
 *)
%}

/*(*************************************************************************)*/
/*(* Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(* the comment tokens *)*/
/*(*-----------------------------------------*)*/
/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Ast_css.info> TComment

/*(* pad: space, the css grammar is space sensitive, hmmm *)*/
%token <Ast_css.info> S

/*(*-----------------------------------------*)*/
/*(* the normal tokens *)*/
/*(*-----------------------------------------*)*/

%token <Ast_css.info> CHARSET IMPORT MEDIA PAGE FONTFACE

%token <Ast_css.info> OPEN_CURLY CLOSE_CURLY
%token <Ast_css.info> OPEN_ROUND CLOSE_ROUND
%token <Ast_css.info> OPEN_SQUARE CLOSE_SQUARE
%token <Ast_css.info> SEMICOLON COLON DOUBLE_COLON COMMA PERIOD SLASH
%token <Ast_css.info> ASTERISK QUOTIENT PLUS MINUS
%token <Ast_css.info> TILDE GT IMPORTANT

%token <Ast_css.info> ATTR_EQUALS ATTR_INCLUDES ATTR_DASHMATCH
%token <Ast_css.info> ATTR_PREFIX ATTR_SUFFIX ATTR_SUBSTRING

%token <Ast_css.info> URI
%token <string * Ast_css.info> TString
%token <string * Ast_css.info> IDENT
%token <string * Ast_css.info> NTH
%token <string * Ast_css.info> HASH
%token <string * Ast_css.info> VAR

%token <string * Ast_css.info> SEL_FUNC
%token <string * Ast_css.info> TERM_FUNC

%token <string * Ast_css.info> QUANTITY

/*(*-----------------------------------------*)*/
%token <Ast_css.info> TUnknown
%token <Ast_css.info> EOF

/*(*-----------------------------------------*)*/
/*(* priorities *)*/
/*(*-----------------------------------------*)*/

%left PLUS MINUS
%left ASTERISK QUOTIENT

/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%type <Ast_css.stylesheet> stylesheet
%start stylesheet

%%

/*(*************************************************************************)*/
/*(* TOC *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* Toplevel *)*/
/*(*************************************************************************)*/

stylesheet:
  | s_star statement_star EOF { $2 }

statement:
 | rule { $1 (* `Rule *) }

rule:
 | selector_list declaration_block                               {($1, $2)}

/*(*************************************************************************)*/
/*(* Selectors *)*/
/*(*************************************************************************)*/

selector_list:
 | selector_separated_nonempty_list_COMMA                        {$1}

selector:
 | simple_selector combination_star                             {($1, $2)}

combination:
 | combinator simple_selector                                    {($1, $2)}

combinator:
 | S       { Descendant }
 | TILDE   { GeneralSibling }
 | PLUS    { AdjacentSibling }
 | GT      { Child }

simple_selector:
 | element qualifier_star
     { Explicit ($1, $2) }
 | qualifier_plus
     { let hd, tl =
       match $1 with
       | hd::tl -> hd, tl
       | _ -> failwith "Generic"
       in
       Generic (hd, tl)
   }

element:
 | IDENT    { Tag (fst $1) }
 | ASTERISK { Universal }

qualifier:
 | HASH
     { Id (fst $1) }
 | PERIOD IDENT
     { Class (fst $2) }
 | OPEN_SQUARE IDENT attr_operation CLOSE_SQUARE
     { Attr (fst $2, $3) }
 | COLON IDENT
     { PseudoClass (fst $2) }
 | DOUBLE_COLON IDENT
     { PseudoElement (fst $2) }
 | SEL_FUNC function_args CLOSE_ROUND
     { SelFunc (fst $1, $2) }

function_args:
 | qualifier_plus { Qualified $1 }
 | NTH            { Nth (fst $1) }
 | IDENT          { Nth (fst $1) }

attr_operation:
 | /* empty */                  { AttrExists }
 | ATTR_EQUALS attr_operand     { AttrEquals $2 }
 | ATTR_INCLUDES attr_operand   { AttrIncludes $2 }
 | ATTR_DASHMATCH attr_operand  { AttrDashmatch $2 }
 | ATTR_PREFIX attr_operand     { AttrPrefix $2 }
 | ATTR_SUFFIX attr_operand     { AttrSuffix $2 }
 | ATTR_SUBSTRING attr_operand  { AttrSubstring $2 }

attr_operand:
 | IDENT   { fst $1 }
 | TString { fst $1 }


/*(*************************************************************************)*/
/*(* Declarations *)*/
/*(*************************************************************************)*/

declaration_block:
 | OPEN_CURLY declaration_plus CLOSE_CURLY       { $2 }

declaration:
 | IDENT COLON expr boption_IMPORTANT SEMICOLON  { (fst $1, $3, $4) }

expr: sentence_separated_nonempty_list_COMMA  { $1 }

sentence: term_separated_nonempty_list_sopt   { $1 }

term:
 | calc    { Calc $1 }
 | TString { String (fst $1) }
 | IDENT   { Ident (fst $1) }
 | URI TString CLOSE_ROUND { Uri (fst $2) }
 | HASH    { Hash (fst $1) }
 | TERM_FUNC expr CLOSE_ROUND { TermFunc (fst $1, $2) }
 | SLASH   { Slash }

calc:
 | VAR      { Varref (fst ($1)) }
 | QUANTITY { Quantity (0., Some (fst $1)) (* TODO parse_quantity *) }

/*(*************************************************************************)*/
/*(* maybe one day, was in dario original grammar *)*/
/*(*************************************************************************)*/

/*(*
stylesheet:
| s_star charset_opt statement_star EOF { [] (* ($2, $3) *)}

charset:
 | CHARSET TString SEMICOLON                                      {$2}

statement:
 | IMPORT source s_opt media_list_opt SEMICOLON
     {`Import ($2, $4)}
 | MEDIA media_list OPEN_CURLY rule_plus CLOSE_CURLY
     {`Media ($2, $4)}
 | PAGE pseudo_page_opt declaration_block
     {`Page ($2, $3)}
 | FONTFACE declaration_block
     {`Fontface $2}
 | VAR COLON expr SEMICOLON
     { raise Todo (* `Vardecl ($startpos($1), $1, $3) *)}

source:
 | TString
     {`String $1}
 | URI TString CLOSE_ROUND
     {`Uri $2}

media_list:
 | medium_separated_nonempty_list_COMMA                  {$1}

medium:
 | IDENT                                                         {$1}

pseudo_page:
 | COLON IDENT                                                   {$2}



calc:
 | calc ASTERISK calc
     {`Mul ($startpos($2), $1, $3)}
 | calc QUOTIENT calc
     {`Div ($startpos($2), $1, $3)}
 | calc PLUS calc
     {`Sum ($startpos($2), $1, $3)}
 | calc MINUS calc
     {`Sub ($startpos($2), $1, $3)}
 | OPEN_ROUND calc CLOSE_ROUND
     {$2}

medium_separated_nonempty_list_COMMA: S { }
charset_opt: S { }
s_opt: S { }
media_list_opt: S { }
rule_plus: S { }
pseudo_page_opt: S { }

*)*/

/*(*************************************************************************)*/
/*(* xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

selector_separated_nonempty_list_COMMA:
 | selector { [$1] }
 | selector_separated_nonempty_list_COMMA COMMA selector { $1 @ [$3] }

sentence_separated_nonempty_list_COMMA:
 | sentence { [$1] }
 | sentence_separated_nonempty_list_COMMA COMMA sentence { $1 @ [$3] }

term_separated_nonempty_list_sopt:
 | term { [$1] }
 | term_separated_nonempty_list_sopt S term { $1 @ [$3] }


statement_star:
 | statement_star statement { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

combination_star:
 | combination_star combination { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

qualifier_star:
 | qualifier_star qualifier { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

s_star:
 | s_star S { [] }
 | /*(*empty*)*/ { [] }


qualifier_plus:
 | qualifier { [$1] }
 | qualifier_plus qualifier { $1 @ [$2] }

declaration_plus:
 | declaration { [$1] }
 | declaration_plus declaration { $1 @ [$2] }


boption_IMPORTANT:
 | /*(*empty*)*/ { false }
 | IMPORTANT { true }

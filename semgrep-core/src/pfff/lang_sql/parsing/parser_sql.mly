/* Yoann Padioleau
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
 */
%{
(*
 * src: http://docs.openlinksw.com/virtuoso/GRAMMAR.html
 *
 *)

%}

/*(*************************************************************************)*/
/*(* Tokens *)*/
/*(*************************************************************************)*/

/*(* keywords tokens *)*/
%token <Ast_sql.info> T_SELECT T_INSERT T_UPDATE T_DELETE

%token <Ast_sql.info> T_FROM T_WHERE
%token <Ast_sql.info> T_AS
%token <Ast_sql.info> T_SET

%token <Ast_sql.info> T_INTO T_REPLACING T_SOFT
%token <Ast_sql.info> T_VALUES

%token <Ast_sql.info> T_JOIN T_ON T_LEFT T_OUTER

%token <Ast_sql.info> T_DUPLICATE T_KEY

%token <Ast_sql.info> T_ALL T_DISTINCT

%token <Ast_sql.info> T_IN
%token <Ast_sql.info> T_UNION

%token <Ast_sql.info> T_ORDER T_BY
%token <Ast_sql.info> T_ASC T_DESC
%token <Ast_sql.info> T_LIMIT

%token <Ast_sql.info> T_LIKE

%token <Ast_sql.info> T_NULLX


%token <Ast_sql.info> T_COMMIT T_BEGIN T_ROLLBACK

%token <Ast_sql.info> T_MEMCACHE_DIRTY


/*(* have values *)*/
%token <Ast_sql.info> T_NAME
%token <Ast_sql.info> T_INTNUM
%token <Ast_sql.info> T_STRING

%token <Ast_sql.info> T_DNUMBER

%token <Ast_sql.info> APPROXNUM

/*(* operators *)*/
%token <Ast_sql.info> TPLUS TMINUS
%token <Ast_sql.info> TMUL TDIV
%token <Ast_sql.info> TOPAR TCPAR
%token <Ast_sql.info> T_OR T_AND T_NOT

%token <Ast_sql.info> TOPNOT TOPOR TOPAND TOPNEG

%token <Ast_sql.info> T_COMPARISON

/*(* syntax *)*/
%token <Ast_sql.info> TSEMICOLON TCOMMA TDOT


/*(* classic *)*/
%token <Ast_sql.info> TUnknown

%token <Ast_sql.info> EOF

/*(* priorities *)*/

%nonassoc SHIFTHERE
%nonassoc  T_AS

%left      TOPOR
%left      TOPAND
%left      T_OR
%left      T_AND
%left      TPLUS TMINUS
%left      TMUL TDIV

%nonassoc  UMINUS
%right  T_NOT
%right  TOPNOT
%right  TOPNEG


/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <unit> main

%%

/*(*************************************************************************)*/
/*(* Toplevel *)*/
/*(*************************************************************************)*/

main: toplevel EOF {  }

toplevel:
 | select_statement { }
 | insert_statement { }
 | update_statement_searched { }
 | delete_statement_searched { }

 | TOPAR select_statement TCPAR { }

 | T_COMMIT { }
 | T_BEGIN { }
 | T_ROLLBACK { }



/*(*************************************************************************)*/
/*(* Entities, names *)*/
/*(*************************************************************************)*/

table:
 | q_table_name { }
 | q_table_name T_AS T_NAME { }
 | q_table_name T_NAME { }

q_table_name:
 | T_NAME { }
 | T_NAME TDOT T_NAME { }
 | T_NAME TDOT T_NAME TDOT T_NAME { }
 | T_NAME TDOT  TDOT T_NAME { }


column:
 | T_NAME { }


column_ref:
 | T_NAME { }
 | T_NAME TDOT T_NAME { }

 | TMUL { }

 | T_NAME TDOT T_NAME TDOT T_NAME { }
 | T_NAME TDOT T_NAME TDOT T_NAME TDOT T_NAME { }
 | T_NAME TDOT TDOT T_NAME TDOT T_NAME { }


 | T_NAME TDOT TMUL { }
 | T_NAME TDOT T_NAME TDOT TMUL { }

 | T_NAME TDOT T_NAME TDOT T_NAME TDOT TMUL { }

 | T_NAME TDOT TDOT T_NAME TDOT TMUL { }



/*(*************************************************************************)*/
/*(* SELECT *)*/
/*(*************************************************************************)*/

select_statement:
 | T_SELECT all_distinct_opt selection table_exp
     { }
/*
 | T_SELECT all_distinct_opt selection T_INTO
     target_list table_exp with_opt_cursor_options_list
     { }
*/

all_distinct:
 | T_ALL { }
 | T_DISTINCT  { }

selection: scalar_exp_list { }


table_exp:
 | from_clause
   where_clause_opt
/*
     group_by_clause_opt
     having_clause_opt
     lock_mode_opt
*/
   order_by_clause_opt
   limit_clause_opt
   { }


from_clause: T_FROM table_ref_list { }

where_clause: T_WHERE search_condition { }

order_by_clause: T_ORDER T_BY ordering_spec_list { }

ordering_spec:
 | T_INTNUM asc_desc_opt { }
 | column_ref asc_desc_opt { }
/*
 | function_ref asc_desc_opt
 */

asc_desc:
 | T_ASC { }
 | T_DESC { }


table_ref:
 | table { }
 | TOPAR query_exp TCPAR T_NAME { }
 | joined_table { }


search_condition:
 | { }

 | search_condition T_OR search_condition { }
 | search_condition T_AND search_condition { }
 | T_NOT search_condition { }
 | TOPAR search_condition TCPAR { }

 | predicate { }

predicate:
 | comparison_predicate { }
 | in_predicate { }
 | like_predicate { }
 /*(* 1 reduce/reduce conflict :( *)*/
 | scalar_exp_predicate { }


/*
 | between_predicate { }
 | test_for_null { }
 | all_or_any_predicate { }
 | existence_test { }
 */

scalar_exp_predicate:
 | scalar_exp { }

comparison_predicate:
 | scalar_exp T_COMPARISON scalar_exp { }
 | scalar_exp T_COMPARISON subquery { }

like_predicate:
 | scalar_exp T_NOT T_LIKE scalar_exp opt_escape { }
 | scalar_exp T_LIKE scalar_exp opt_escape { }


opt_escape:
 |  /* empty */ { }
/*
 | T_ESCAPE atom { }
 | BEGINX ESCAPE atom ENDX
*/


in_predicate:
 | scalar_exp T_NOT T_IN TOPAR scalar_exp_list TCPAR { }
 | scalar_exp T_IN TOPAR scalar_exp_list TCPAR { }
 | scalar_exp T_NOT T_IN subquery { }
 | scalar_exp T_IN subquery { }


subquery:
 | TOPAR T_SELECT all_distinct_opt selection table_exp TCPAR { }



joined_table:
 | table_ref jtype outer_opt T_JOIN table_ref_nj T_ON search_condition
     { }
/*
 | BEGIN_OJ_X table_ref jtype outer_opt JOIN table_ref_nj
     ON search_condition ENDX
*/

jtype:
 | T_LEFT { }


outer_opt:
 | /* empty */ { }
 | T_OUTER { }

table_ref_nj:
 | table           { }
 | subquery T_NAME { }


/*(*pad: mysql extension *)*/
limit_clause:
 | T_LIMIT int_list { }



/*(*************************************************************************)*/
/*(* CREATE *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* INSERT *)*/
/*(*************************************************************************)*/

insert_statement:
 | T_INSERT insert_mode table priv_opt_column_commalist values_or_query_spec
    on_duplicate_opt
    memcache_opt
     { }
/*(*pad: mysql extension ? *)*/
 | T_INSERT insert_mode table T_SET
     assignment_list { }


insert_mode:
 | T_INTO { }
 | T_REPLACING { }
 | T_SOFT { }


priv_opt_column_commalist:
 | /* empty */ { }
 | TOPAR column_list TCPAR { }

values_or_query_spec:
 | T_VALUES TOPAR insert_atom_list TCPAR { }
 | query_spec { }

insert_atom: scalar_exp { }

query_exp:
 | query_term { }
 | query_exp T_UNION query_term { }
 | query_exp T_UNION T_ALL query_term { }


query_term:
 | query_spec  { }
 | TOPAR query_exp TCPAR { }

query_spec:
 | T_SELECT all_distinct_opt selection table_exp { }




/* pad: facebook extension ? */
memcache:
 | T_MEMCACHE_DIRTY string_list { }



/* pad: mysql extension */
on_duplicate_opt:
 | { }
 | T_ON T_DUPLICATE T_KEY update_statement_duplicate { }

update_statement_duplicate:
 | T_UPDATE table T_SET assignment_update_list { }
 | T_UPDATE assignment_update_list { }

assignment_update:
 | column T_COMPARISON scalar_exp { }
 | column T_COMPARISON T_VALUES TOPAR insert_atom_list TCPAR { }

assignment_update_list:
 | assignment_update { }
 | assignment_update_list TCOMMA assignment_update { }

/*(*************************************************************************)*/
/*(* UPDATE *)*/
/*(*************************************************************************)*/

update_statement_searched:
 | T_UPDATE table T_SET assignment_list where_clause_opt
    memcache_opt
{ }

assignment:
 | column T_COMPARISON scalar_exp { }


/*(*************************************************************************)*/
/*(* DELETE *)*/
/*(*************************************************************************)*/

delete_statement_searched:
 | T_DELETE T_FROM table where_clause_opt
    memcache_opt
 { }

/*(*************************************************************************)*/
/*(* MISC *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* Expressions *)*/
/*(*************************************************************************)*/


scalar_exp:
 | scalar_exp TPLUS scalar_exp { }
 | scalar_exp TMINUS scalar_exp { }
 | scalar_exp TMUL scalar_exp { }
 | scalar_exp TDIV scalar_exp { }

 | scalar_exp TOPAND scalar_exp { }
 | scalar_exp TOPOR scalar_exp { }

 | TOPNEG scalar_exp  { }
 | TOPNOT scalar_exp  { }

 | TPLUS scalar_exp %prec UMINUS { }
 | TMINUS scalar_exp %prec UMINUS { }
 | atom { }
 | column_ref { }
 | as_expression { }
 | TOPAR scalar_exp_list TCPAR { }
 | function_call { }
/*
 | function_ref
 | assignment_statement
 | cvt_exp
*/

atom:
 | literal { }
/*
 | parameter_ref
 | USER
 | obe_literal
*/


function_call:
 | q_table_name TOPAR scalar_exp_list_opt TCPAR { }
/*

 | BEGIN_FN_X NAME '(' opt_scalar_exp_commalist ')' ENDX
 | BEGIN_FN_X USER '(' opt_scalar_exp_commalist ')' ENDX
 | BEGIN_FN_X CHARACTER '(' opt_scalar_exp_commalist ')' ENDX
 | CALL '(' scalar_exp ')' '(' opt_scalar_exp_commalist ')'
*/



literal:
 | T_STRING { }
 | T_INTNUM { }

 | APPROXNUM { }
 | T_NULLX { }

/*(* pad, float *)*/
 | T_DNUMBER { }

as_expression:
 | scalar_exp T_AS T_NAME { }
/*
 | scalar_exp T_AS T_NAME data_type { }
*/


/*(*************************************************************************)*/
/*(* xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

/*(* comma list *)*/
scalar_exp_list:
 | scalar_exp                        { [$1] }
 | scalar_exp_list TCOMMA scalar_exp { $1 @ [$3] }

assignment_list:
 | assignment                        { [$1] }
 | assignment_list TCOMMA assignment { $1 @ [$3] }

ordering_spec_list:
 | ordering_spec                        { [$1] }
 | ordering_spec_list TCOMMA ordering_spec { $1 @ [$3] }

table_ref_list:
 | table_ref { }
 | table_ref_list TCOMMA table_ref { }

insert_atom_list:
 | insert_atom { }
 | insert_atom_list TCOMMA insert_atom { }

column_list:
 | column { }
 | column_list TCOMMA column { }

string_list:
 | T_STRING { }
 | string_list TCOMMA T_STRING { }

int_list:
 | T_INTNUM { }
 | int_list TCOMMA T_INTNUM { }



/*(* opt *)*/
all_distinct_opt:
  | /*(*empty*)*/       { None }
  | all_distinct        { Some $1 }

scalar_exp_list_opt:
 |                 {  }
 | scalar_exp_list {  }

where_clause_opt:
 |              { }
 | where_clause { }


order_by_clause_opt:
 | /* empty */ { }
 | order_by_clause { }

memcache_opt:
 | { }
 | memcache { }

asc_desc_opt:
 | { }
 | asc_desc { }

limit_clause_opt:
 | { }
 | limit_clause  { }

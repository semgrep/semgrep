%{
(*
 * src: ocamlyaccified from Marcel Laverdet 'fbjs2' via Emacs macros, itself
 * extracted from the official ECMAscript specification at:
 * http://www.ecma-international.org/publications/standards/ecma-262.htm
 *
 *)

open Common

open Ast_js

%}

/*(*************************************************************************)*/
/*(* Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(* the comment tokens *)*/
/*(*-----------------------------------------*)*/
/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Ast_js.info> TCommentSpace TCommentNewline   TComment

/*(*-----------------------------------------*)*/
/*(* the normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with a value *)*/
%token<string * Ast_js.info> T_NUMBER
%token<string * Ast_js.info> T_IDENTIFIER
%token<string * Ast_js.info> T_STRING
%token<string * Ast_js.info> T_REGEX

/*(* keywords tokens *)*/
%token <Ast_js.info>
 T_FUNCTION T_IF T_IN T_INSTANCEOF T_RETURN T_SWITCH T_THIS T_THROW T_TRY
 T_VAR T_WHILE T_WITH T_CONST T_NULL T_FALSE T_TRUE
 T_BREAK T_CASE T_CATCH T_CONTINUE T_DEFAULT T_DO T_FINALLY T_FOR

%token <Ast_js.info> T_ELSE

%token <Ast_js.info> T_NEW

/*(* syntax *)*/
%token <Ast_js.info>
 T_LCURLY T_RCURLY
 T_LPAREN T_RPAREN
 T_LBRACKET T_RBRACKET
 T_SEMICOLON
 T_COMMA
 T_PERIOD

/*(* operators *)*/

/*(*-----------------------------------------*)*/
/*(* extra tokens: *)*/
/*(*-----------------------------------------*)*/

%token <Ast_js.info> T_VIRTUAL_SEMICOLON

/*(* classic *)*/
%token <Ast_js.info> TUnknown
%token <Ast_js.info> EOF

/*(*-----------------------------------------*)*/
/*(* priorities *)*/
/*(*-----------------------------------------*)*/

/*(* must be at the top so that it has the lowest priority *)*/
%nonassoc SHIFTHERE

/*(* Special if / else associativity*)*/
%nonassoc p_IF
%nonassoc T_ELSE

%nonassoc p_POSTFIX

%right
 T_RSHIFT3_ASSIGN T_RSHIFT_ASSIGN T_LSHIFT_ASSIGN
 T_BIT_XOR_ASSIGN T_BIT_OR_ASSIGN T_BIT_AND_ASSIGN T_MOD_ASSIGN T_DIV_ASSIGN
 T_MULT_ASSIGN T_MINUS_ASSIGN T_PLUS_ASSIGN T_ASSIGN
%token T_PLING T_COLON
%left T_OR
%left T_AND
%left T_BIT_OR
%left T_BIT_XOR
%left T_BIT_AND
%left T_EQUAL T_NOT_EQUAL T_STRICT_EQUAL T_STRICT_NOT_EQUAL
%left
 T_LESS_THAN_EQUAL T_GREATER_THAN_EQUAL T_LESS_THAN T_GREATER_THAN
 T_IN T_INSTANCEOF
%left T_LSHIFT T_RSHIFT T_RSHIFT3
%left T_PLUS T_MINUS
%left T_DIV T_MULT T_MOD
%right T_NOT T_BIT_NOT T_INCR T_DECR T_DELETE T_TYPEOF T_VOID

/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <unit> main

%%

/*(*************************************************************************)*/
/*(* Toplevel *)*/
/*(*************************************************************************)*/

main: program EOF {  }

program: statement_list { }

source_element:
 | statement { }
 | function_declaration { }

/*(*************************************************************************)*/
/*(* statement *)*/
/*(*************************************************************************)*/

statement:
 | block                { }
 | variable_statement   { }
 | empty_statement      { }
 | expression_statement { }
 | if_statement         { }
 | iteration_statement  { }
 | continue_statement   { }
 | break_statement      { }
 | return_statement     { }
 | with_statement       { }
 | labelled_statement   { }
 | switch_statement     { }
 | throw_statement      { }
 | try_statement        { }


block:
 | T_LCURLY statement_list T_RCURLY { }
 | T_LCURLY T_RCURLY { }


variable_statement:
 | T_VAR variable_declaration_list semicolon { }

variable_declaration:
 | identifier initializeur { }
 | identifier { }

initializeur:
 | T_ASSIGN assignment_expression { }


empty_statement:
 | semicolon { }

expression_statement:
 | expression_no_statement semicolon { }


if_statement:
 | T_IF T_LPAREN expression T_RPAREN statement T_ELSE statement { }
 | T_IF T_LPAREN expression T_RPAREN statement %prec p_IF { }


iteration_statement:
 | T_DO statement T_WHILE T_LPAREN expression T_RPAREN semicolon { }
 | T_WHILE T_LPAREN expression T_RPAREN statement { }
 | T_FOR T_LPAREN
     expression_no_in_opt T_SEMICOLON
     expression_opt T_SEMICOLON
     expression_opt
     T_RPAREN statement { }
 | T_FOR T_LPAREN
     T_VAR variable_declaration_list_no_in T_SEMICOLON
     expression_opt T_SEMICOLON
     expression_opt
     T_RPAREN statement { }
 | T_FOR T_LPAREN left_hand_side_expression T_IN expression T_RPAREN statement
     { }
 | T_FOR
     T_LPAREN T_VAR variable_declaration_list_no_in T_IN expression T_RPAREN
     statement { }

variable_declaration_no_in:
 | identifier initializer_no_in { }
 | identifier { }

initializer_no_in:
 | T_ASSIGN assignment_expression_no_in { }


continue_statement:
 | T_CONTINUE identifier semicolon { }
 | T_CONTINUE semicolon { }


break_statement:
 | T_BREAK identifier semicolon { }
 | T_BREAK semicolon { }


return_statement:
 | T_RETURN expression semicolon { }
 | T_RETURN semicolon { }


with_statement:
 | T_WITH T_LPAREN expression T_RPAREN statement { }


switch_statement:
 | T_SWITCH T_LPAREN expression T_RPAREN case_block { }



labelled_statement:
 | identifier T_COLON statement { }


throw_statement:
 | T_THROW expression semicolon { }


try_statement:
 | T_TRY block catch { }
 | T_TRY block finally { }
 | T_TRY block catch finally { }


catch:
 | T_CATCH T_LPAREN identifier T_RPAREN block { }


finally:
 | T_FINALLY block { }

/*(*----------------------------*)*/
/*(* auxillary statements *)*/
/*(*----------------------------*)*/

case_block:
 | T_LCURLY case_clauses_opt T_RCURLY { }
 | T_LCURLY case_clauses_opt default_clause case_clauses_opt T_RCURLY { }


case_clause:
 | T_CASE expression T_COLON statement_list { }
 | T_CASE expression T_COLON { }


default_clause:
 | T_DEFAULT T_COLON { }
 | T_DEFAULT T_COLON statement_list { }

/*(*************************************************************************)*/
/*(* function declaration *)*/
/*(*************************************************************************)*/

function_declaration:
 | T_FUNCTION identifier T_LPAREN formal_parameter_list T_RPAREN
     T_LCURLY function_body T_RCURLY
     { }
 | T_FUNCTION identifier T_LPAREN T_RPAREN
     T_LCURLY function_body T_RCURLY
     { }


function_expression:
 | T_FUNCTION identifier T_LPAREN formal_parameter_list T_RPAREN
     T_LCURLY function_body T_RCURLY
     { }
 | T_FUNCTION identifier T_LPAREN T_RPAREN
     T_LCURLY function_body T_RCURLY
     { }
 | T_FUNCTION T_LPAREN formal_parameter_list T_RPAREN
     T_LCURLY function_body T_RCURLY
     { }
 | T_FUNCTION T_LPAREN T_RPAREN
     T_LCURLY function_body T_RCURLY
     { }

formal_parameter_list:
 | identifier { }
 | formal_parameter_list T_COMMA identifier { }

function_body:
 | /*(* empty *)*/ { }
 | statement_list { }

/*(*************************************************************************)*/
/*(* expression *)*/
/*(*************************************************************************)*/

expression:
 | assignment_expression { }
 | expression T_COMMA assignment_expression { }

assignment_expression:
 | conditional_expression { }
 | left_hand_side_expression assignment_operator assignment_expression { }

assignment_operator:
 | T_ASSIGN { }
 | T_MULT_ASSIGN { }
 | T_DIV_ASSIGN { }
 | T_MOD_ASSIGN { }
 | T_PLUS_ASSIGN { }
 | T_MINUS_ASSIGN { }
 | T_LSHIFT_ASSIGN { }
 | T_RSHIFT_ASSIGN { }
 | T_RSHIFT3_ASSIGN { }
 | T_BIT_AND_ASSIGN { }
 | T_BIT_XOR_ASSIGN { }
 | T_BIT_OR_ASSIGN { }

left_hand_side_expression:
 | new_expression { }
 | call_expression { }

conditional_expression:
 | post_in_expression { }
 | post_in_expression
     T_PLING assignment_expression
     T_COLON assignment_expression
     { }

post_in_expression:
 | pre_in_expression { }
 | post_in_expression T_LESS_THAN post_in_expression { }
 | post_in_expression T_GREATER_THAN post_in_expression { }
 | post_in_expression T_LESS_THAN_EQUAL post_in_expression { }
 | post_in_expression T_GREATER_THAN_EQUAL post_in_expression { }
 | post_in_expression T_INSTANCEOF post_in_expression { }
 | post_in_expression T_IN post_in_expression { }
 | post_in_expression T_EQUAL post_in_expression { }
 | post_in_expression T_NOT_EQUAL post_in_expression { }
 | post_in_expression T_STRICT_EQUAL post_in_expression { }
 | post_in_expression T_STRICT_NOT_EQUAL post_in_expression { }
 | post_in_expression T_BIT_AND post_in_expression { }
 | post_in_expression T_BIT_XOR post_in_expression { }
 | post_in_expression T_BIT_OR post_in_expression { }
 | post_in_expression T_AND post_in_expression { }
 | post_in_expression T_OR post_in_expression { }

pre_in_expression:
 | left_hand_side_expression { }
 | pre_in_expression T_INCR %prec p_POSTFIX { }
 | pre_in_expression T_DECR %prec p_POSTFIX { }
 | T_DELETE pre_in_expression { }
 | T_VOID pre_in_expression { }
 | T_TYPEOF pre_in_expression { }
 | T_INCR pre_in_expression { }
 | T_DECR pre_in_expression { }
 | T_PLUS pre_in_expression { }
 | T_MINUS pre_in_expression { }
 | T_BIT_NOT pre_in_expression { }
 | T_NOT pre_in_expression { }
 | pre_in_expression T_MULT pre_in_expression { }
 | pre_in_expression T_DIV pre_in_expression { }
 | pre_in_expression T_MOD pre_in_expression { }
 | pre_in_expression T_PLUS pre_in_expression { }
 | pre_in_expression T_MINUS pre_in_expression { }
 | pre_in_expression T_LSHIFT pre_in_expression { }
 | pre_in_expression T_RSHIFT pre_in_expression { }
 | pre_in_expression T_RSHIFT3 pre_in_expression { }

call_expression:
 | member_expression arguments { }
 | call_expression arguments { }
 | call_expression T_LBRACKET expression T_RBRACKET { }
 | call_expression T_PERIOD identifier { }

new_expression:
 | member_expression { }
 | T_NEW new_expression { }

member_expression:
 | primary_expression { }
 | member_expression T_LBRACKET expression T_RBRACKET { }
 | member_expression T_PERIOD identifier { }
 | T_NEW member_expression arguments { }

primary_expression:
 | primary_expression_no_statement { }
 | object_literal { }
 | function_expression { }

primary_expression_no_statement:
 | T_THIS { }
 | identifier { }
 | null_literal { }
 | boolean_literal { }
 | numeric_literal { }
 | string_literal { }
 /*(* marcel: this isn't an expansion of literal in ECMA-262... mistake? *)*/
 | regex_literal  { }
 | array_literal { }
 | T_LPAREN expression T_RPAREN { }

/*(*----------------------------*)*/
/*(* no in *)*/
/*(*----------------------------*)*/
expression_no_in:
 | assignment_expression_no_in { }
 | expression_no_in T_COMMA assignment_expression_no_in { }

assignment_expression_no_in:
 | conditional_expression_no_in { }
 | left_hand_side_expression assignment_operator assignment_expression_no_in { }

conditional_expression_no_in:
 | post_in_expression_no_in { }
 | post_in_expression_no_in
     T_PLING assignment_expression_no_in
     T_COLON assignment_expression_no_in
     { }

post_in_expression_no_in:
 | pre_in_expression { }
 | post_in_expression_no_in T_LESS_THAN post_in_expression { }
 | post_in_expression_no_in T_GREATER_THAN post_in_expression { }
 | post_in_expression_no_in T_LESS_THAN_EQUAL post_in_expression { }
 | post_in_expression_no_in T_GREATER_THAN_EQUAL post_in_expression { }
 | post_in_expression_no_in T_INSTANCEOF post_in_expression { }
 | post_in_expression_no_in T_EQUAL post_in_expression { }
 | post_in_expression_no_in T_NOT_EQUAL post_in_expression { }
 | post_in_expression_no_in T_STRICT_EQUAL post_in_expression { }
 | post_in_expression_no_in T_STRICT_NOT_EQUAL post_in_expression { }
 | post_in_expression_no_in T_BIT_AND post_in_expression { }
 | post_in_expression_no_in T_BIT_XOR post_in_expression { }
 | post_in_expression_no_in T_BIT_OR post_in_expression { }
 | post_in_expression_no_in T_AND post_in_expression { }
 | post_in_expression_no_in T_OR post_in_expression { }


/*(*----------------------------*)*/
/*(* (no statement)*)*/
/*(*----------------------------*)*/
expression_no_statement:
 | assignment_expression_no_statement { }
 | expression_no_statement T_COMMA assignment_expression { }

assignment_expression_no_statement:
 | conditional_expression_no_statement { }
 | left_hand_side_expression_no_statement assignment_operator assignment_expression { }

conditional_expression_no_statement:
 | post_in_expression_no_statement { }
 | post_in_expression_no_statement
     T_PLING assignment_expression
     T_COLON assignment_expression
     { }



post_in_expression_no_statement:
 | pre_in_expression_no_statement { }
 | post_in_expression_no_statement T_LESS_THAN post_in_expression { }
 | post_in_expression_no_statement T_GREATER_THAN post_in_expression { }
 | post_in_expression_no_statement T_LESS_THAN_EQUAL post_in_expression { }
 | post_in_expression_no_statement T_GREATER_THAN_EQUAL post_in_expression { }
 | post_in_expression_no_statement T_INSTANCEOF post_in_expression { }
 | post_in_expression_no_statement T_IN post_in_expression { }
 | post_in_expression_no_statement T_EQUAL post_in_expression { }
 | post_in_expression_no_statement T_NOT_EQUAL post_in_expression { }
 | post_in_expression_no_statement T_STRICT_EQUAL post_in_expression { }
 | post_in_expression_no_statement T_STRICT_NOT_EQUAL post_in_expression { }
 | post_in_expression_no_statement T_BIT_AND post_in_expression { }
 | post_in_expression_no_statement T_BIT_XOR post_in_expression { }
 | post_in_expression_no_statement T_BIT_OR post_in_expression { }
 | post_in_expression_no_statement T_AND post_in_expression { }
 | post_in_expression_no_statement T_OR post_in_expression { }


pre_in_expression_no_statement:
 | left_hand_side_expression_no_statement { }
 | pre_in_expression_no_statement T_INCR { }
 | pre_in_expression_no_statement T_DECR { }
 | T_DELETE pre_in_expression { }
 | T_VOID pre_in_expression { }
 | T_TYPEOF pre_in_expression { }
 | T_INCR pre_in_expression { }
 | T_DECR pre_in_expression { }
 | T_PLUS pre_in_expression { }
 | T_MINUS pre_in_expression { }
 | T_BIT_NOT pre_in_expression { }
 | T_NOT pre_in_expression { }
 | pre_in_expression_no_statement T_MULT pre_in_expression { }
 | pre_in_expression_no_statement T_DIV pre_in_expression { }
 | pre_in_expression_no_statement T_MOD pre_in_expression { }
 | pre_in_expression_no_statement T_PLUS pre_in_expression { }
 | pre_in_expression_no_statement T_MINUS pre_in_expression { }
 | pre_in_expression_no_statement T_LSHIFT pre_in_expression { }
 | pre_in_expression_no_statement T_RSHIFT pre_in_expression { }
 | pre_in_expression_no_statement T_RSHIFT3 pre_in_expression { }

left_hand_side_expression_no_statement:
 | new_expression_no_statement { }
 | call_expression_no_statement { }

new_expression_no_statement:
 | member_expression_no_statement { }
 | T_NEW new_expression { }

call_expression_no_statement:
 | member_expression_no_statement arguments { }
 | call_expression_no_statement arguments { }
 | call_expression_no_statement T_LBRACKET expression T_RBRACKET { }
 | call_expression_no_statement T_PERIOD identifier { }

member_expression_no_statement:
 | primary_expression_no_statement { }
 | member_expression_no_statement T_LBRACKET expression T_RBRACKET { }
 | member_expression_no_statement T_PERIOD identifier { }
 | T_NEW member_expression arguments { }

/*(*----------------------------*)*/
/*(* scalar *)*/
/*(*----------------------------*)*/
null_literal:
 | T_NULL { }

boolean_literal:
 | T_TRUE { }
 | T_FALSE { }

numeric_literal:
 | T_NUMBER { }

regex_literal:
 | T_REGEX { }

string_literal:
 | T_STRING { }

/*(*----------------------------*)*/
/*(* array *)*/
/*(*----------------------------*)*/

array_literal:
 | T_LBRACKET elison T_RBRACKET { }
 | T_LBRACKET T_RBRACKET { }
 | T_LBRACKET element_list T_RBRACKET { }
 | T_LBRACKET element_list elison T_RBRACKET { }


element_list:
 | elison assignment_expression { }
 | assignment_expression { }
 | element_list elison assignment_expression { }



object_literal:
 | T_LCURLY T_RCURLY { }
 | T_LCURLY property_name_and_value_list T_VIRTUAL_SEMICOLON T_RCURLY { }




property_name_and_value_list:
 | property_name T_COLON assignment_expression { }
 | property_name_and_value_list T_COMMA
     property_name T_COLON assignment_expression { }

/*(*----------------------------*)*/
/*(* variable *)*/
/*(*----------------------------*)*/

/*(*----------------------------*)*/
/*(* function call *)*/
/*(*----------------------------*)*/

arguments:
 | T_LPAREN T_RPAREN { }
 | T_LPAREN argument_list T_RPAREN { }

argument_list:
 | assignment_expression { }
 | argument_list T_COMMA assignment_expression { }

/*(*----------------------------*)*/
/*(* auxillary bis *)*/
/*(*----------------------------*)*/

/*(*************************************************************************)*/
/*(* Entities, names *)*/
/*(*************************************************************************)*/
identifier:
 | T_IDENTIFIER { }

property_name:
 | identifier { }
 | string_literal { }
 | numeric_literal { }

/*(*************************************************************************)*/
/*(* xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

semicolon:
 | T_SEMICOLON { }
 | T_VIRTUAL_SEMICOLON { }

elison:
 | T_COMMA { }
 | elison T_COMMA { }



statement_list:
 | source_element { }
 | statement_list source_element {  }

case_clauses:
 | case_clause { }
 | case_clauses case_clause { }



variable_declaration_list:
 | variable_declaration { }
 | variable_declaration_list T_COMMA variable_declaration { }

variable_declaration_list_no_in:
 | variable_declaration_no_in { }
 | variable_declaration_list_no_in T_COMMA variable_declaration_no_in { }



expression_opt:
 | /*(* empty *)*/ { }
 | expression { }

expression_no_in_opt:
 | /*(* empty *)*/ { }
 | expression_no_in { }

case_clauses_opt:
 | /*(* empty *)*/ { }
 | case_clauses { }

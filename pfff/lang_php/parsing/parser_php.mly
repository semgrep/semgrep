/* Yoann Padioleau
 *
 * Copyright (C) 2009-2013 Facebook
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
(* src: originally ocamlyaccified from zend_language_parser.y in Zend PHP.
 * updates:
 *  - extended to deal with XHP based on the XHP bison grammar
 *  - added support for a few PHP 5.3 extensions (e.g. lambda, const), but
 *    not namespace
 *  - added support for yield (facebook extension)
 *  - added support for a few PHP 5.4 extensions (e.g. traits, short array)
 *  - added support for generics (another facebook extension)
 *  - added support for attributes (a.k.a annotations)
 *  - factorized some rules (e.g. parameters, arguments)
 *  - added support for trailing comma in function calls and definitions
 *  - added support for (new Foo)->method() (id() not needed anymore)
 *  - heavy rewrite of expr and lvalue rules, to avoid the many conflicts
 *    regarding f()[] sugar and also to allow more forms of expressions
 *    such as (<whatever expr>)->method()
 *  - added support for typedefs (another facebook extension)
 *  - added support for implicit fields via constructor parameters
 *    (facebook extension)
 *  - added support for namespace (a PHP 5.3 extension)
 /*
  * +----------------------------------------------------------------------+
  * | Zend Engine                                                          |
  * +----------------------------------------------------------------------+
  * | Copyright (c) 1998-2006 Zend Technologies Ltd. (http://www.zend.com) |
  * +----------------------------------------------------------------------+
  * | This source file is subject to version 2.00 of the Zend license,     |
  * | that is bundled with this package in the file LICENSE, and is        |
  * | available through the world-wide-web at the following url:           |
  * | http://www.zend.com/license/2_00.txt.                                |
  * | If you did not receive a copy of the Zend license and are unable to  |
  * | obtain it through the world-wide-web, please send a note to          |
  * | license@zend.com so we can mail you a copy immediately.              |
  * +----------------------------------------------------------------------+
  * | Authors: Andi Gutmans <andi@zend.com>                                |
  * |          Zeev Suraski <zeev@zend.com>                                |
  * +----------------------------------------------------------------------+
  */
 *)
open Common

open Cst_php
module Ast = Cst_php
module H = Parser_php_mly_helper
module PI = Parse_info

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

%token <Cst_php.info> TUnknown /*(* unrecognized token *)*/
%token <Cst_php.info> EOF

/*(*-----------------------------------------*)*/
/*(*2 The space/comment tokens *)*/
/*(*-----------------------------------------*)*/
/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Cst_php.info> TSpaces TNewline

/*(* not mentionned in this grammar. filtered in parse_php.ml *)*/
%token <Cst_php.info> T_COMMENT T_DOC_COMMENT

/*(* when use preprocessor and want to mark removed tokens as commented *)*/
%token <Cst_php.info> TCommentPP

/*(*-----------------------------------------*)*/
/*(*2 The normal tokens *)*/
/*(*-----------------------------------------*)*/
%token <string * Cst_php.info>
 T_LNUMBER T_DNUMBER
 /*(* T_IDENT is for a regular ident and  T_VARIABLE is for a dollar ident.
   * Note that with XHP if you want to add a rule using T_IDENT, you should
   * probably use 'ident' instead.
   *)*/
 T_IDENT T_VARIABLE
 T_CONSTANT_ENCAPSED_STRING   T_ENCAPSED_AND_WHITESPACE  T_INLINE_HTML
 /*(* used only for offset of array access inside strings *)*/
 T_NUM_STRING
 T_STRING_VARNAME
/*(*in original: %token <Cst_php.info> T_CHARACTER T_BAD_CHARACTER *)*/

/*(*-----------------------------------------*)*/
/*(*2 Keyword tokens *)*/
/*(*-----------------------------------------*)*/

%token <Cst_php.info>
 T_IF T_ELSE T_ELSEIF T_ENDIF
 T_DO  T_WHILE   T_ENDWHILE  T_FOR     T_ENDFOR T_FOREACH T_ENDFOREACH
 T_SWITCH  T_ENDSWITCH T_CASE T_DEFAULT    T_BREAK T_CONTINUE
 T_RETURN  T_TRY  T_CATCH  T_FINALLY  T_THROW
 T_EXIT T_DECLARE T_ENDDECLARE T_USE T_GLOBAL T_AS T_FUNCTION T_CONST T_VAR
/*(* ugly: because of my hack around the implicit echo when use <?=,
   * this T_ECHO might have a string different than "echo"
   *)*/
 T_ECHO  T_PRINT
 /*(* pad: was declared via right ... ??? mean token ? *)*/
 T_ASYNC T_STATIC  T_ABSTRACT  T_FINAL  T_PRIVATE T_PROTECTED T_PUBLIC
 T_UNSET T_ISSET T_EMPTY
 T_CLASS   T_INTERFACE  T_EXTENDS T_IMPLEMENTS
 T_TRAIT T_INSTEADOF
 T_NAMESPACE
 T_LIST T_ARRAY
 T_CLASS_C T_METHOD_C T_FUNC_C T_LINE   T_FILE T_DIR T_TRAIT_C T_NAMESPACE_C
 T_LOGICAL_OR   T_LOGICAL_AND   T_LOGICAL_XOR
 T_NEW T_CLONE T_INSTANCEOF
 T_INCLUDE T_INCLUDE_ONCE T_REQUIRE T_REQUIRE_ONCE
 T_EVAL
 /*(* not in original grammar *)*/
 T_SELF T_PARENT
 /*(* facebook extension *)*/
 T_TYPE T_NEWTYPE T_SHAPE
%left T_TYPE
%left T_IDENT
%right T_ELLIPSIS
/*(*-----------------------------------------*)*/
/*(*2 Punctuation tokens *)*/
/*(*-----------------------------------------*)*/

%token <Cst_php.info>
 T_OBJECT_OPERATOR T_ARROW T_DOUBLE_ARROW
 T_OPEN_TAG  T_CLOSE_TAG T_OPEN_TAG_WITH_ECHO T_CLOSE_TAG_OF_ECHO
 T_START_HEREDOC    T_END_HEREDOC
 T_DOLLAR_OPEN_CURLY_BRACES T_CURLY_OPEN
 TCOLCOL TANTISLASH
 /*(* pad: was declared as left/right, without a token decl in orig gram *)*/
 TCOLON TCOMMA TDOT TBANG TTILDE TQUESTION
 TOBRA
 TPLUS TMINUS TMUL TDIV TMOD
 TAND TOR TXOR
 TEQ
 /*(* now also used for types/generics, as in vector<int> *)*/
 TSMALLER TGREATER
 T_PLUS_EQUAL  T_MINUS_EQUAL  T_MUL_EQUAL  T_DIV_EQUAL
 T_CONCAT_EQUAL  T_MOD_EQUAL
 T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL
 T_INC    T_DEC
 T_BOOLEAN_OR   T_BOOLEAN_AND T_BOOLEAN_PIPE
 T_SL    T_SR
 T_IS_SMALLER_OR_EQUAL    T_IS_GREATER_OR_EQUAL
 T_BOOL_CAST T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST
 T_UNSET_CAST
 T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_IS_EQUAL     T_IS_NOT_EQUAL
 T__AT
 /*(* was declared implicitely because was using directly the character *)*/
 TOPAR TCPAR  TOBRACE TCBRACE
 TCBRA TBACKQUOTE
/*(* ugly: because of my hack around the implicit ';' when use ?>,
   * this TSEMICOLON might have a string different than ';'
   *)*/
 TSEMICOLON
 TDOLLAR /*(* see also T_VARIABLE *)*/
 TDOLLARDOLLAR
 TGUIL
 T_ROCKET

/*(*-----------------------------------------*)*/
/*(*2 Extra tokens: *)*/
/*(*-----------------------------------------*)*/
%token <Cst_php.info> T_CLASS_XDEBUG  T_RESOURCE_XDEBUG

/*(*-----------------------------------------*)*/
/*(*2 PHP language extensions: *)*/
/*(*-----------------------------------------*)*/
%token <Cst_php.info> T_YIELD
%token <Cst_php.info> T_AWAIT
%token <Cst_php.info> T_SUPER

/*(* phpext: for hack and also for sgrep *)*/
%token <Cst_php.info> T_ELLIPSIS

/*(* lexing hack to parse lambda params properly *)*/
%token <Cst_php.info> T_LAMBDA_OPAR T_LAMBDA_CPAR

/*(*-----------------------------------------*)*/
/*(*2 XHP tokens *)*/
/*(*-----------------------------------------*)*/

/*(* xhp: token for ':frag:foo'; quite similiar to T_IDENT *)*/
%token <string list * Cst_php.info> T_XHP_COLONID_DEF
/*(* xhp: token for '%frag:foo' *)*/
%token <string list * Cst_php.info> T_XHP_PERCENTID_DEF

/*(* xhp: e.g. for '<x:frag', note that the real end of the tag is
   * in another token, either T_XHP_GT or T_XHP_SLASH_GT.
   *)*/
%token <Cst_php.xhp_tag * Cst_php.info> T_XHP_OPEN_TAG

/*(* ending part of the opening tag *)*/
%token <Cst_php.info> T_XHP_GT T_XHP_SLASH_GT

/*(* xhp: e.g. for '</x:frag>'. The 'option' is for closing tags like </> *)*/
%token <Cst_php.xhp_tag option * Cst_php.info> T_XHP_CLOSE_TAG

%token <string * Cst_php.info> T_XHP_ATTR T_XHP_TEXT

/*(* xhp keywords. If you add one don't forget to update the 'ident' rule. *)*/
%token <Cst_php.info>
 T_XHP_ATTRIBUTE T_XHP_CHILDREN T_XHP_CATEGORY
 T_ENUM T_XHP_REQUIRED
 T_XHP_ANY /*(* T_XHP_EMPTY is T_EMPTY *)*/
 T_XHP_PCDATA

/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/
/*(* must be at the top so that it has the lowest priority *)*/
%nonassoc LOW_PRIORITY_RULE

/*(* those are low priority, especially lower than ?: *)*/
%nonassoc  T_YIELD
%nonassoc  T_AWAIT

%left T_ARROW

/*(* http://www.php.net/manual/en/language.operators.precedence.php *)*/
%left      T_INCLUDE T_INCLUDE_ONCE T_EVAL T_REQUIRE T_REQUIRE_ONCE
/*(* php-facebook-ext: lambda (short closure) syntax *)*/
%right     T_DOUBLE_ARROW
%left      TCOMMA
%left      T_LOGICAL_OR
%left      T_LOGICAL_XOR
%left      T_LOGICAL_AND
%right     T_PRINT

/*(* now that we've unified expr with lvalue, this should really be a %right
   * and have higher priority than '&&' otherwise '1 && $x = 2'  would be
   * parsed as (1 && $x) = 2. We achieve the correct behavior not by
   * using %right but by rewriting the rule regarding TEQ to be
   * 'simple_expr TEQ expr' and not 'expr TEQ expr' (like in parser_cpp.mly).
   *)*/
%left     TEQ  T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL

%left      TQUESTION TCOLON
%left      T_BOOLEAN_OR
%left      T_BOOLEAN_AND
%left      TOR
%left      TXOR
%left      TAND
%nonassoc  T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_ROCKET
%nonassoc  TSMALLER T_IS_SMALLER_OR_EQUAL TGREATER T_IS_GREATER_OR_EQUAL
%left      T_SL T_SR
%left      TPLUS TMINUS TDOT
%left      TMUL TDIV TMOD


%right     TBANG
%nonassoc  T_INSTANCEOF
%right     TTILDE T_INC T_DEC T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST
%right     T__AT
%right     TOBRA
%nonassoc  T_NEW T_CLONE
%left      T_ELSEIF
%left      T_ELSE
%left      T_ENDIF

/*(* not in original grammar *)*/
%left TCOLCOL
%left TDOLLAR
%left T_OBJECT_OPERATOR

/*(* xhp: this is used only to remove some shift/reduce ambiguities on the
   * error-rule trick.
   *)*/
%left T_XHP_PERCENTID_DEF

/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/
%start main sgrep_spatch_pattern
%type <Cst_php.toplevel list> main
%type <Cst_php.any>           sgrep_spatch_pattern

%%

/*(*************************************************************************)*/
/*(*1 Toplevel *)*/
/*(*************************************************************************)*/
main: top_statement_list EOF { H.squash_stmt_list $1 @ [FinalDef $2] }

top_statement:
 | statement                            { StmtList [$1] }
 | constant_declaration_statement       { ConstantDef $1 }
 | function_declaration_statement	{ FuncDef $1 }
 | class_declaration_statement		{ ClassDef $1 }
 | type_declaration                     { TypeDef $1 }
 | namespace_declaration                { $1 }
 | use_declaration                      { $1 }

sgrep_spatch_pattern:
 | expr EOF      { Expr $1 }
 /*(* less: a bit obsolete, use generalized sgrep instead for that *)*/
 | statement EOF { Stmt2 $1 }
 | function_declaration_statement { Toplevel (FuncDef $1) }
 | TCOLON type_php { Hint2 $2 }

/*(*************************************************************************)*/
/*(*1 Statements *)*/
/*(*************************************************************************)*/
statement:
 | expr           TSEMICOLON		  { ExprStmt($1,$2) }
 | /*(* empty*)*/ TSEMICOLON              { EmptyStmt($1) }

 | TOBRACE inner_statement_list TCBRACE   { Block($1,$2,$3) }

 | T_IF TOPAR expr TCPAR statement elseif_list else_single
     { If($1,($2,$3,$4),$5,$6,$7) }
 | T_IF TOPAR expr TCPAR TCOLON
     inner_statement_list new_elseif_list new_else_single
     T_ENDIF TSEMICOLON
     { IfColon($1,($2,$3,$4),$5,$6,$7,$8,$9,$10)  }

 | T_WHILE TOPAR expr  TCPAR while_statement
     { While($1,($2,$3,$4),$5) }
 | T_DO statement T_WHILE TOPAR expr TCPAR TSEMICOLON
     { Do($1,$2,$3,($4,$5,$6),$7) }
 | T_FOR TOPAR for_expr TSEMICOLON  for_expr TSEMICOLON for_expr TCPAR
     for_statement
     { For($1,$2,$3,$4,$5,$6,$7,$8,$9) }

 | T_SWITCH TOPAR expr TCPAR	switch_case_list
     { Switch($1,($2,$3,$4),$5) }

 | T_FOREACH TOPAR expr T_AS foreach_pattern TCPAR foreach_statement
     { Foreach($1,$2,$3,None, $4,$5,$6,$7) }
 | T_FOREACH TOPAR expr T_AWAIT T_AS foreach_pattern TCPAR foreach_statement
     { Foreach($1,$2,$3, Some $4, $5,$6,$7, $8) }

 | T_BREAK      TSEMICOLON     	{ Break($1,None,$2) }
 | T_BREAK expr TSEMICOLON	{ Break($1,Some $2, $3) }
 | T_CONTINUE      TSEMICOLON	{ Continue($1,None,$2) }
 | T_CONTINUE expr TSEMICOLON	{ Continue($1,Some $2, $3) }

 | T_RETURN TSEMICOLON	     { Return ($1,None, $2) }
 | T_RETURN expr TSEMICOLON  { Return ($1,Some ($2), $3)}

 | T_TRY   TOBRACE inner_statement_list TCBRACE
   T_CATCH TOPAR class_name  T_VARIABLE TCPAR
     TOBRACE inner_statement_list TCBRACE
     additional_catches optional_finally_clause
     { let try_block = ($2,$3,$4) in
       let catch_block = ($10, $11, $12) in
       let catch = ($5, ($6, ($7, DName $8), $9), catch_block) in
       Try($1, try_block, [catch] @ $13, $14)
     }
 | T_TRY TOBRACE inner_statement_list TCBRACE finally_clause
     { let try_block = ($2,$3,$4) in
       Try($1, try_block, [], [$5])
     }
 | T_THROW expr TSEMICOLON { Throw($1,$2,$3) }

 | T_ECHO echo_expr_list TSEMICOLON     { Echo($1,$2,$3) }
 | T_INLINE_HTML			{ InlineHtml($1) }

 | T_OPEN_TAG_WITH_ECHO expr T_CLOSE_TAG_OF_ECHO {
     (* ugly: the 2 tokens will have a wrong string *)
     Echo ($1, [Left $2], $3)
   }
 /*(* ugly: php allows that too  *)*/
 | T_OPEN_TAG_WITH_ECHO expr TSEMICOLON T_CLOSE_TAG_OF_ECHO {
     Echo ($1, [Left $2], $4)
   }

 | T_GLOBAL global_var_list TSEMICOLON { Globals($1,$2,$3) }
 | T_STATIC static_var_list TSEMICOLON { StaticVars($1,$2,$3) }

 | T_UNSET TOPAR unset_variables TCPAR TSEMICOLON { Unset($1,($2,$3,$4),$5) }

 | T_USE use_filename TSEMICOLON		  { Use($1,$2,$3) }
 | T_DECLARE  TOPAR declare_list TCPAR declare_statement
     { Declare($1,($2,$3,$4),$5) }

inner_statement:
 | statement                            { $1 }
 | function_declaration_statement	{ FuncDefNested $1 }
 | class_declaration_statement		{ ClassDefNested $1 }

/*(*----------------------------*)*/
/*(*2 auxillary statements *)*/
/*(*----------------------------*)*/
for_expr:
 | /*(*empty*)*/    	{ [] }
 | non_empty_for_expr	{ $1 }

/*(* can not factorize with a is_reference otherwise s/r conflict on LIST *)*/
foreach_variable:
 |  expr      { None, $1 }
 | TAND expr  { Some $1, $2 }

foreach_pattern:
  | foreach_variable
      { ForeachVar $1 }
  | foreach_variable T_ARROW foreach_pattern
      { ForeachArrow(ForeachVar $1,$2,$3) }
  | T_LIST TOPAR assignment_list TCPAR
     { ForeachList($1,($2,$3,$4)) }

switch_case_list:
 | TOBRACE            case_list TCBRACE
     { CaseList($1,None,$2,$3) }
 | TOBRACE TSEMICOLON case_list TCBRACE
     { CaseList($1, Some $2, $3, $4) }
 | TCOLON             case_list T_ENDSWITCH TSEMICOLON
     { CaseColonList($1,None,$2, $3, $4) }
 | TCOLON TSEMICOLON  case_list T_ENDSWITCH TSEMICOLON
     { CaseColonList($1, Some $2, $3, $4, $5) }

 | T_XHP_COLONID_DEF { H.failwith_xhp_ambiguity_colon (snd $1) }

case_list: case_list_rev { List.rev $1 }
case_list_rev:
 | /*(*empty*)*/	{ [] }
 | case_list_rev    T_CASE expr case_separator inner_statement_list
     { Case($2,$3,$4,$5)::$1   }
 | case_list_rev    T_DEFAULT   case_separator inner_statement_list
     { Default($2,$3,$4)::$1 }

case_separator:
 | TCOLON     { $1 }
 /*(* ugly php ... but reported in check_misc_php.ml *)*/
 | TSEMICOLON { $1 }

 | T_XHP_COLONID_DEF { H.failwith_xhp_ambiguity_colon (snd $1) }


while_statement:
 | statement                                         { SingleStmt $1 }
 | TCOLON inner_statement_list T_ENDWHILE TSEMICOLON { ColonStmt($1,$2,$3,$4) }

for_statement:
 | statement                                       { SingleStmt $1 }
 | TCOLON inner_statement_list T_ENDFOR TSEMICOLON { ColonStmt($1,$2,$3,$4) }

foreach_statement:
 | statement                                           { SingleStmt $1 }
 | TCOLON inner_statement_list T_ENDFOREACH TSEMICOLON { ColonStmt($1,$2,$3,$4)}

declare_statement:
 | statement                                           { SingleStmt $1 }
 | TCOLON inner_statement_list T_ENDDECLARE TSEMICOLON { ColonStmt($1,$2,$3,$4)}

elseif_list:
 | /*(*empty*)*/ { [] }
 | elseif_list  T_ELSEIF TOPAR expr TCPAR statement { $1 @ [$2,($3,$4,$5),$6]}

new_elseif_list:
 | /*(*empty*)*/ { [] }
 | new_elseif_list    T_ELSEIF TOPAR expr TCPAR TCOLON inner_statement_list
     { $1 @ [$2,($3,$4,$5),$6,$7] }

/*(* classic dangling else ambiguity resolved by a %prec. See conflicts.txt*)*/
else_single:
 | T_ELSE statement                        { Some($1,$2) }
 | /*(*empty*)*/  %prec LOW_PRIORITY_RULE  { None }

new_else_single:
 | /*(*empty*)*/                      { None }
 | T_ELSE TCOLON inner_statement_list { Some($1,$2,$3) }


additional_catch:
 | T_CATCH TOPAR class_name T_VARIABLE TCPAR
           TOBRACE inner_statement_list TCBRACE
     { let catch_block = ($6, $7, $8) in
       let catch = ($1, ($2, ($3, DName $4), $5), catch_block) in
       catch
     }

finally_clause:
 | T_FINALLY TOBRACE inner_statement_list TCBRACE
     { ($1, ($2, $3, $4)) }

/*(*----------------------------*)*/
/*(*2 auxillary bis *)*/
/*(*----------------------------*)*/

declare: ident   TEQ static_scalar { Name $1, ($2, $3) }

global_var:
 | T_VARIABLE			{ GlobalVar (DName $1) }
 | TDOLLAR expr  		{ GlobalDollar ($1, $2) }
 | TDOLLAR TOBRACE expr TCBRACE	{ GlobalDollarExpr ($1, ($2, $3, $4)) }

static_var:
 | T_VARIABLE                   { (DName $1, None) }
 | T_VARIABLE TEQ static_scalar { (DName $1, Some ($2, $3)) }

unset_variable: expr	{ $1 }

use_filename:
 |       T_CONSTANT_ENCAPSED_STRING		{ UseDirect $1 }
 | TOPAR T_CONSTANT_ENCAPSED_STRING TCPAR	{ UseParen ($1, $2, $3) }

/*(*************************************************************************)*/
/*(*1 Constant declaration *)*/
/*(*************************************************************************)*/

/*(* PHP 5.3 *)*/
constant_declaration_statement:
 | T_CONST           ident TEQ static_scalar TSEMICOLON
   { { cst_toks = ($1, $3, $5); cst_name = Name $2; cst_val = $4;
       cst_type = None} }
 /*(* can not factorize with a 'type_opt', see conflict.txt *)*/
 | T_CONST type_php  ident TEQ static_scalar TSEMICOLON
   { { cst_toks = ($1, $4, $6); cst_name = Name $3; cst_val = $5;
       cst_type = Some $2 } }

/*(*************************************************************************)*/
/*(*1 Function declaration *)*/
/*(*************************************************************************)*/
function_declaration_statement:
 |            unticked_function_declaration_statement { $1 }
 /*(* can not factorize with a 'attributes_opt', see conflict.txt *)*/
 | attributes unticked_function_declaration_statement
     { { $2 with f_attrs = Some $1 } }

unticked_function_declaration_statement:
 async_opt T_FUNCTION is_reference ident type_params_opt
   TOPAR parameter_list TCPAR
   return_type_opt function_body
   {  H.validate_parameter_list $7;
      { f_tok = $2; f_ref = $3; f_name = Name $4; f_params = ($6, $7, $8);
       f_tparams = $5;
       f_return_type = $9; f_body = $10;
       f_attrs = None;
       f_type = FunctionRegular; f_modifiers = $1;
    } }

function_body:
 | TOBRACE inner_statement_list TCBRACE     { ($1, $2, $3)  }
 | TSEMICOLON { (* ugly: *) (Ast.fakeInfo"", [], $1) }

async_opt:
 | /*(*empty*)*/ { [] }
 | T_ASYNC { [Async,($1)] }

parameter_list:
 | /*(*empty*)*/                     { [] }
 | parameter                         { [$1] }
 /*(* php-facebook-ext: trailing comma *)*/
 | parameter TCOMMA parameter_list   { $1 :: (Right3 $2) :: $3 }

parameter: attributes_opt ctor_modifier_opt at_opt type_php_opt parameter_bis
      {
        match $5 with
          Left3 param ->
            let hint = match param.p_type with
              | Some(HintVariadic (tok, _)) -> Some(HintVariadic (tok, $4))
              | _ -> $4
            in
            Left3 { param with p_modifier = $2; p_attrs = $1; p_type = hint; p_soft_type= $3; }
        | _ -> match ($1, $2, $3, $4) with
                 (None, None, None, None) -> $5
               | _ -> raise Parsing.Parse_error
      }

parameter_bis:
 | T_VARIABLE
     { Left3 (H.mk_param $1) }
 | TAND T_VARIABLE
     { let p = H.mk_param $2 in Left3 {p with p_ref=Some $1} }
 | T_VARIABLE TEQ static_scalar
     { let p = H.mk_param $1 in Left3 {p with p_default=Some($2,$3)} }
 | TAND T_VARIABLE TEQ static_scalar
     { let p = H.mk_param $2 in Left3 {p with p_ref=Some $1; p_default=Some($3,$4)} }
 | T_ELLIPSIS T_VARIABLE
     { let p = H.mk_param $2 in Left3 {p with p_variadic=Some $1; p_type=Some(HintVariadic ($1, None))} }
 | TAND T_ELLIPSIS T_VARIABLE
     { let p = H.mk_param $3 in Left3 {p with p_ref=Some $1; p_variadic=Some $2; p_type=Some(HintVariadic ($2, None))} }
 /*(* varargs extension *)*/
 | T_ELLIPSIS
     { Middle3 $1 }

/*(* php-facebook-ext: implicit field via constructor parameter *)*/
ctor_modifier:
 | T_PUBLIC    { Public,($1) } | T_PROTECTED { Protected,($1) }
 | T_PRIVATE   { Private,($1) }


is_reference:
 | /*(*empty*)*/  { None }
 | TAND		  { Some $1 }

/*(* PHP 5.3 *)*/
lexical_vars:
 | /*(*empty*)*/  { None }
 | T_USE TOPAR non_empty_lexical_var_list TCPAR {
     Some ($1, ($2, ($3 |> List.map (function
     | Right info -> Right info
     | Left (a,b) -> Left (LexicalVar (a,b)))), $4))
   }

non_empty_lexical_var_list:
 | non_empty_lexical_var_list_bis { $1 }
 /*(* php-facebook-ext: trailing comma *)*/
 | non_empty_lexical_var_list_bis TCOMMA { $1 @ [Right $2] }

non_empty_lexical_var_list_bis:
 | lexical_var
     { [Left $1] }
 | non_empty_lexical_var_list_bis TCOMMA lexical_var
     { $1 @ [Right $2; Left $3] }

lexical_var:
 |      T_VARIABLE  { (None, DName $1) }
 | TAND T_VARIABLE  { (Some $1, DName $2) }

/*(*************************************************************************)*/
/*(*1 Class declaration *)*/
/*(*************************************************************************)*/
class_declaration_statement:
 |            unticked_class_declaration_statement
     { $1 }
 | attributes unticked_class_declaration_statement
     { { $2 with c_attrs = Some $1 } }

unticked_class_declaration_statement:
 | class_entry_type  ident_class_name  type_params_opt
     extends_from   implements_list
     TOBRACE class_statement_list TCBRACE
     { { c_type = $1; c_name = $2; c_extends = $4; c_tparams = $3;
         c_implements = $5; c_body = $6, $7, $8;
         c_attrs = None;
         c_enum_type = None;
       }
     }
 | T_INTERFACE ident_class_name type_params_opt
     interface_extends_list
     TOBRACE class_statement_list TCBRACE
     { { c_type = Interface $1; c_name = $2; c_extends = None; c_tparams = $3;
         (* we use c_implements for interface extension because
          * it can be a list. ugly?
          *)
         c_implements = $4; c_body = $5, $6, $7;
         c_attrs = None;
         c_enum_type = None;
     } }
 | T_TRAIT ident_class_name type_params_opt
   implements_list
    TOBRACE class_statement_list TCBRACE
     { { c_type = Trait $1; c_name = $2; c_extends = None; c_tparams = $3;
         c_implements = $4; c_body = ($5, $6, $7);
         c_attrs = None;
         c_enum_type = None;
       }
     }
 | T_ENUM ident_class_name TCOLON type_php type_constr_opt
    TOBRACE enum_statement_list TCBRACE
     { { c_type = Enum $1; c_name = $2; c_extends = None; c_tparams = None;
         c_implements = None; c_body = ($6, $7, $8);
         c_attrs = None;
         c_enum_type = Some { e_tok = $3; e_base = $4; e_constraint = $5; }
       }
     }


class_entry_type:
 | T_CLASS  	      { ClassRegular $1 }
 | T_ABSTRACT T_FINAL T_CLASS { ClassAbstractFinal ($1, $2, $3) }
 | T_FINAL T_ABSTRACT T_CLASS { ClassAbstractFinal ($1, $2, $3) }
 | T_ABSTRACT T_CLASS { ClassAbstract ($1, $2) }
 | T_FINAL    T_CLASS { ClassFinal ($1, $2) }

extends_from:
 | /*(*empty*)*/	         { None }
 | T_EXTENDS class_name_no_array { Some ($1, $2)  }

interface_extends_list:
 | /*(*empty*)*/            { None }
 | T_EXTENDS class_name_list { Some($1,$2) }

implements_list:
 | /*(*empty*)*/               { None }
 | T_IMPLEMENTS class_name_list { Some($1, $2) }

/*(*----------------------------*)*/
/*(*2 class statement *)*/
/*(*----------------------------*)*/

class_statement:
/*(* facebook-ext: abstract constants *)*/
 | T_ABSTRACT T_CONST          ident  TSEMICOLON
     { let const = (Name $3, None) in
       let const_list = [Left const] in
       ClassConstants(Some $1, $2, None, const_list, $4) }
 | T_ABSTRACT T_CONST type_php ident  TSEMICOLON
     { let const = (Name $4, None) in
       let const_list = [Left const] in
       ClassConstants(Some $1, $2, Some $3, const_list, $5) }

/*(* facebook-ext: type constants *)*/
 | T_ABSTRACT T_CONST T_TYPE T_IDENT T_AS type_php TSEMICOLON
     { ClassType (* TODO (t6384084) add some fields here *) {
         t_tok = $3;
         t_name = Name $4;
         t_tparams = None;
         t_tconstraint = Some($5, $6);
         t_tokeq = $5; (* doesn't exist here *)
         t_kind = ClassConstType None;
         t_sc = $7; }
     }
 | T_ABSTRACT T_CONST T_TYPE T_IDENT TSEMICOLON
     { ClassType (* TODO (t6384084) add some fields here *) {
         t_tok = $3;
         t_name = Name $4;
         t_tparams = None;
         t_tconstraint = None;
         t_tokeq = $5; (* doesn't exist here *)
         t_kind = ClassConstType None;
         t_sc = $5; }
     }
 | T_CONST T_TYPE T_IDENT type_constr_opt TEQ type_php TSEMICOLON
     { ClassType (* TODO (t6384084) add some fields here *) {
         t_tok = $2;
         t_name = Name $3;
         t_tparams = None;
         t_tconstraint = $4;
         t_tokeq = $5;
         t_kind = ClassConstType (Some $6);
         t_sc = $7; }
     }

/*(* class constants *)*/
 | T_CONST          class_constants_declaration  TSEMICOLON
     { ClassConstants(None, $1, None, $2, $3) }
 | T_CONST type_php class_constants_declaration  TSEMICOLON
     { ClassConstants(None, $1, Some $2, $3, $4) }

/*(* class variables (aka properties) *)*/
 | variable_modifiers          class_variable_declaration TSEMICOLON
     { ClassVariables($1, None, $2, $3) }
 | variable_modifiers type_php class_variable_declaration TSEMICOLON
     { ClassVariables($1, Some $2, $3, $4)  }

/*(* class methods *)*/
 |            method_declaration { Method $1 }
 | attributes method_declaration { Method { $2 with f_attrs = Some $1 } }

/*(* XHP *)*/
 | T_XHP_ATTRIBUTE xhp_attribute_decls TSEMICOLON
     { XhpDecl (XhpAttributesDecl ($1, $2, $3)) }
 | T_XHP_CHILDREN  xhp_children_decl  TSEMICOLON
     { XhpDecl (XhpChildrenDecl ($1, $2, $3)) }
 | T_XHP_CATEGORY xhp_category_list TSEMICOLON
     { XhpDecl (XhpCategoriesDecl ($1, $2, $3)) }

/*(* php 5.4 traits *)*/
 | T_USE class_name_list TSEMICOLON
     { UseTrait ($1, $2, Left $3) }
 | T_USE class_name_list TOBRACE trait_rules TCBRACE
     { UseTrait ($1, $2, Right ($3, $4, $5)) }
/*(* facebook-ext: *)*/
 | T_REQUIRE trait_constraint_kind type_php TSEMICOLON
     { TraitConstraint ($1, $2, $3, $4) }

enum_statement:
   class_constant_declaration TSEMICOLON
     { ClassConstants(None, $2, None, [Left $1], $2) }

method_declaration:
     method_modifiers T_FUNCTION is_reference ident_method_name type_params_opt
     TOPAR parameter_list TCPAR
     return_type_opt
     method_body
     { H.validate_parameter_list $7;
       let body, function_type = $10 in
       ({ f_tok = $2; f_ref = $3; f_name = Name $4; f_tparams = $5;
          f_params = ($6, $7, $8); f_return_type = $9;
          f_body = body; f_type = function_type; f_modifiers = $1;
          f_attrs = None;
        })
     }

class_constant_declaration: ident TEQ static_scalar { ((Name $1), Some ($2, $3)) }

variable_modifiers:
 | T_VAR				{ NoModifiers $1 }
 | non_empty_member_modifiers		{ VModifiers $1 }


class_variable:
 | T_VARIABLE			{ (DName $1, None) }
 | T_VARIABLE TEQ static_scalar	{ (DName $1, Some ($2, $3)) }

member_modifier:
 | T_PUBLIC    { Public,($1) } | T_PROTECTED { Protected,($1) }
 | T_PRIVATE   { Private,($1) }
 | T_STATIC    { Static,($1) }
 | T_ABSTRACT { Abstract,($1) } | T_FINAL{ Final,($1) }
 | T_ASYNC { Async,($1) }

method_body:
 | TOBRACE inner_statement_list TCBRACE	{ ($1, $2, $3), MethodRegular }
 | TSEMICOLON { (* ugly: *) (Ast.fakeInfo"", [], $1), MethodAbstract }

/*(*----------------------------*)*/
/*(*2 XHP attributes *)*/
/*(*----------------------------*)*/
/*(* mostly a copy paste of the original XHP grammar *)*/

xhp_attribute_decl:
 | T_XHP_COLONID_DEF
     { XhpAttrInherit $1 }
 | xhp_attribute_decl_type xhp_attr_name xhp_attribute_default
     xhp_attribute_is_required
     { XhpAttrDecl ($1, ((PI.str_of_info $2, $2)), $3, $4) }

xhp_attribute_decl_type:
 | T_ENUM TOBRACE xhp_enum_list TCBRACE
     { XhpAttrEnum ($1, ($2, $3, $4)) }
 | T_VAR        { XhpAttrVar $1 }
 | type_php { XhpAttrType $1 }

xhp_attribute_default:
 | /*(*empty*)*/     { None }
 | TEQ static_scalar { Some ($1, $2) }

xhp_attribute_is_required:
 | /*(*empty*)*/  { None }
 | T_XHP_REQUIRED { Some $1 }

xhp_enum:
 | constant { $1 }

/*(* was called xhp_label_pass in original grammar *)*/
xhp_attr_name:
 | ident_xhp_attr_name_atom { $1 }
 /*(* ugly, but harder to lex foo-name as a single token without
    * introducing lots of ambiguities. It's ok for :foo:bar but not
    * for attribute name.
    *
    * less: could check that there is no whitespace between those
    * tokens.
    *)*/
 | xhp_attr_name TMINUS ident_xhp_attr_name_atom
     { let s = PI.str_of_info $1 ^  PI.str_of_info $2 ^ PI.str_of_info $3 in
       PI.rewrap_str s $1
     }
 /*(* this is used by only one attribute right now, xlink:href which
    * the w3 spec imposed to have this format *)*/
 | xhp_attr_name TCOLON ident_xhp_attr_name_atom
     { let s = PI.str_of_info $1 ^  PI.str_of_info $2 ^ PI.str_of_info $3 in
       PI.rewrap_str s $1
     }

/*(*----------------------------*)*/
/*(*2 XHP children *)*/
/*(*----------------------------*)*/
/*(* Mostly a copy paste of the original XHP grammar.
   * Not sure why it needs to be that complicated. We could factorize
   * rules for instance for the parenthesis stuff.
   *)*/
xhp_children_decl:
 | T_XHP_ANY { XhpChildAny $1 }
 | T_EMPTY   { XhpChildEmpty $1 }
 | xhp_children_paren_expr { $1 }

xhp_children_paren_expr:
 | TOPAR xhp_children_decl_expr TCPAR
     { XhpChildParen ($1, $2, $3) }
 | TOPAR xhp_children_decl_expr TCPAR TMUL
     { XhpChildMul (XhpChildParen ($1, $2, $3), $4) }
 | TOPAR xhp_children_decl_expr TCPAR TQUESTION
     { XhpChildOption (XhpChildParen ($1, $2, $3), $4) }
 | TOPAR xhp_children_decl_expr TCPAR TPLUS
     { XhpChildPlus (XhpChildParen ($1, $2, $3), $4) }

xhp_children_decl_expr:
 | xhp_children_paren_expr { $1 }
 | xhp_children_decl_tag { $1 }
 | xhp_children_decl_tag TMUL      { XhpChildMul ($1, $2)  }
 | xhp_children_decl_tag TQUESTION { XhpChildOption ($1, $2) }
 | xhp_children_decl_tag TPLUS     { XhpChildPlus ($1, $2) }

 | xhp_children_decl_expr TCOMMA xhp_children_decl_expr
     { XhpChildSequence ($1, $2, $3) }
 | xhp_children_decl_expr TOR xhp_children_decl_expr
     { XhpChildAlternative ($1, $2, $3) }

xhp_children_decl_tag:
 | T_XHP_ANY           { XhpChildAny ($1) }
 | T_XHP_PCDATA        { XhpChildPcdata ($1) }
 | T_XHP_COLONID_DEF   { XhpChild $1 }
 | T_XHP_PERCENTID_DEF { XhpChildCategory $1 }

/*(*----------------------------*)*/
/*(*2 XHP category *)*/
/*(*----------------------------*)*/

xhp_category:
 | T_XHP_PERCENTID_DEF { $1 }

/*(*----------------------------*)*/
/*(*2 Traits *)*/
/*(*----------------------------*)*/

trait_rule:
 | trait_precedence_rule  { $1 }
 | trait_alias_rule       { $1 }

trait_precedence_rule:
 | qualified_name_for_traits TCOLCOL T_IDENT T_INSTEADOF class_name_list TSEMICOLON
   { InsteadOf ($1, $2, Name $3, $4, $5, $6) }

trait_alias_rule:
 | trait_alias_rule_method T_AS method_modifiers T_IDENT TSEMICOLON
   { As ($1, $2, $3, Some (Name $4), $5) }

 | trait_alias_rule_method T_AS non_empty_member_modifiers TSEMICOLON
   { As ($1, $2, $3, None, $4) }

trait_alias_rule_method:
 | qualified_name_for_traits TCOLCOL T_IDENT { Right ($1, $2, Name $3) }
 | T_IDENT { Left (Name $1) }

trait_constraint_kind:
 | T_EXTENDS    { MustExtend, $1 }
 | T_IMPLEMENTS { MustImplement, $1 }

/*(*************************************************************************)*/
/*(*1 Type definitions *)*/
/*(*************************************************************************)*/
type_declaration:
 | T_TYPE   ident type_params_opt type_constr_opt TEQ type_php TSEMICOLON
     { { t_tok = $1; t_name = Name $2; t_tparams = $3; t_tconstraint = $4;
         t_tokeq = $5; t_kind = Alias $6; t_sc = $7; }
     }
 | T_NEWTYPE ident type_params_opt type_constr_opt TEQ type_php TSEMICOLON
     { { t_tok = $1; t_name = Name $2; t_tparams = $3; t_tconstraint = $4;
         t_tokeq = $5; t_kind = Newtype $6; t_sc = $7; }
     }

type_constr_opt:
 | T_AS type_php  { Some ($1, $2) }
 | /*(*empty*) */ { None }

/*(*************************************************************************)*/
/*(*1 Generics parameters *)*/
/*(*************************************************************************)*/
type_params_opt:
  | /*(*empty*)*/                      { None }
  | TSMALLER type_params_list TGREATER { Some ($1, $2, $3) }

type_params_list:
  | type_param                         { [Left $1] }
  | type_param TCOMMA type_params_list { [Left $1; Right $2] @ $3 }

type_param:
  | variance_opt ident                 { TParam (Name $2) }
  | variance_opt ident T_AS TQUESTION class_name { TParamConstraint (Name $2, $3, HintQuestion ($4, $5)) }
  | variance_opt ident T_AS class_name { TParamConstraint (Name $2, $3, $4) }
  | variance_opt ident T_SUPER TQUESTION class_name { TParamConstraint (Name $2, $3, HintQuestion ($4, $5)) }
  | variance_opt ident T_SUPER class_name { TParamConstraint (Name $2, $3, $4) }

variance_opt:
  | /*(*nothing*)*/ {None}
  | TMINUS{ Some $1 }
  | TPLUS { Some $1 }


/*(*************************************************************************)*/
/*(*1 Types *)*/
/*(*************************************************************************)*/

type_php:
 | primary_type_php { $1 }
/*(*facebook-ext: classes can define type constants referenced using `::`*)*/
 | type_php TCOLCOL primary_type_php { HintTypeConst ($1, $2, $3) }
 | T_SHAPE TOPAR shape_field_list TCPAR { HintShape ($1, ($2, $3, $4)) }

primary_type_php:
 | class_name { $1 }
 | T_SELF     { Hint (Self $1, None) }
 | T_PARENT   { Hint (Parent $1, None) }
 /*(* hack extensions *)*/
 | TQUESTION type_php
     { HintQuestion ($1, $2)  }
 | TOPAR non_empty_type_php_list TCPAR
     { HintTuple ($1, $2, $3) }
 | TOPAR T_FUNCTION TOPAR type_php_or_dots_list TCPAR return_type TCPAR
     { HintCallback ($1, ($2, ($3, $4, $5), Some $6), $7)}


/*(* similar to parameter_list, but without names for the parameters *)*/
type_php_or_dots_list:
 | /*(*empty*)*/                     { [] }
 | non_empty_type_php_or_dots_list   { $1 }
 /*(* php-facebook-ext: trailing comma *)*/
 | non_empty_type_php_or_dots_list TCOMMA { $1 @ [Right3 $2] }

type_php_or_dots:
 | type_php { Left3 $1 }
 | T_ELLIPSIS    { Middle3 $1 }

/*(* Do not confuse type_parameters and type_arguments. Type parameters
   * can only be simple identifiers, as in class Foo<T1, T2> { ... },
   * and are used in a 'definition' context, whereas type arguments
   * can be complex types, as in class X extends Foo<int, vector<float>>,
   * and are used in a 'use' context.
   *)*/
type_arguments:
  | /*(*empty*)*/             { None }
  | TSMALLER type_arg_list TGREATER { Some ($1, $2, $3) }

type_arg_list:
  | type_php
      { [Left $1]}
  | type_php TCOMMA type_arg_list
      { (Left $1)::(Right $2):: $3 }

return_type: TCOLON at_opt type_php                 { $1, $2, $3 }

/*(*************************************************************************)*/
/*(*1 Attributes *)*/
/*(*************************************************************************)*/
 /*(* HPHP extension. *)*/
attributes: T_SL attribute_list T_SR { ($1, $2, $3) }

attribute:
 | ident
     { Attribute $1 }
 | ident TOPAR attribute_argument_list TCPAR
     { AttributeWithArgs ($1, ($2, $3, $4)) }

attribute_argument: static_scalar { $1 }

/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

expr:
 | simple_expr { $1 }

 /*(* the left part of TEQ used to be 'lvalue'. After the lvalue/expr
    * unification, I put 'expr' but that was wrong because
    * code like '1 && $x = 2'  was wrongly parsed. So we need to
    * force to have a 'simple_expr' on the left side.
    *)*/
 | simple_expr TEQ expr	{ Assign($1,$2, $3) }
 | simple_expr TEQ TAND expr   { AssignRef($1,$2,$3, $4) }

 | simple_expr T_PLUS_EQUAL   expr { AssignOp($1,(AssignOpArith Plus,$2),$3) }
 | simple_expr T_MINUS_EQUAL  expr { AssignOp($1,(AssignOpArith Minus,$2),$3) }
 | simple_expr T_MUL_EQUAL    expr { AssignOp($1,(AssignOpArith Mul,$2),$3) }
 | simple_expr T_DIV_EQUAL    expr { AssignOp($1,(AssignOpArith Div,$2),$3) }
 | simple_expr T_MOD_EQUAL    expr { AssignOp($1,(AssignOpArith Mod,$2),$3) }
 | simple_expr T_AND_EQUAL    expr { AssignOp($1,(AssignOpArith And,$2),$3) }
 | simple_expr T_OR_EQUAL     expr { AssignOp($1,(AssignOpArith Or,$2),$3) }
 | simple_expr T_XOR_EQUAL    expr { AssignOp($1,(AssignOpArith Xor,$2),$3) }
 | simple_expr T_SL_EQUAL     expr { AssignOp($1,(AssignOpArith DecLeft,$2),$3) }
 | simple_expr T_SR_EQUAL     expr { AssignOp($1,(AssignOpArith DecRight,$2),$3) }

 | simple_expr T_CONCAT_EQUAL expr { AssignOp($1,(AssignConcat,$2),$3) }

 | expr T_INC { Postfix($1, (AST_generic.Incr, $2)) }
 | expr T_DEC { Postfix($1, (AST_generic.Decr, $2)) }
 | T_INC expr { Infix((AST_generic.Incr, $1), $2) }
 | T_DEC expr { Infix((AST_generic.Decr, $1), $2) }

 | expr T_BOOLEAN_OR   expr { Binary($1,(Logical OrBool ,$2),$3) }
 | expr T_BOOLEAN_AND  expr { Binary($1,(Logical AndBool,$2),$3) }
 | expr T_LOGICAL_OR   expr { Binary($1,(Logical OrLog,  $2),$3) }
 | expr T_LOGICAL_AND  expr { Binary($1,(Logical AndLog, $2),$3) }
 | expr T_LOGICAL_XOR  expr { Binary($1,(Logical XorLog, $2),$3) }

 | expr TPLUS expr 	{ Binary($1,(Arith Plus ,$2),$3) }
 | expr TMINUS expr 	{ Binary($1,(Arith Minus,$2),$3) }
 | expr TMUL expr	{ Binary($1,(Arith Mul,$2),$3) }
 | expr TDIV expr	{ Binary($1,(Arith Div,$2),$3) }
 | expr TMOD expr 	{ Binary($1,(Arith Mod,$2),$3) }

 | expr T_XHP_PERCENTID_DEF 	{ H.failwith_xhp_ambiguity_percent (snd $2) }

 | expr TAND expr	{ Binary($1,(Arith And,$2),$3) }
 | expr TOR expr	{ Binary($1,(Arith Or,$2),$3) }
 | expr TXOR expr	{ Binary($1,(Arith Xor,$2),$3) }
 | expr T_SL expr	{ Binary($1,(Arith DecLeft,$2),$3) }
 | expr T_SR expr	{ Binary($1,(Arith DecRight,$2),$3) }

 | expr TDOT expr 	{ Binary($1,(BinaryConcat,$2),$3) }

 | expr T_IS_IDENTICAL        expr { Binary($1,(Logical Identical,$2),$3) }
 | expr T_IS_NOT_IDENTICAL    expr { Binary($1,(Logical NotIdentical,$2),$3) }
 | expr T_IS_EQUAL            expr { Binary($1,(Logical Eq,$2),$3) }
 | expr T_IS_NOT_EQUAL        expr { Binary($1,(Logical NotEq,$2),$3) }
 | expr TSMALLER              expr { Binary($1,(Logical Inf,$2),$3) }
 | expr T_IS_SMALLER_OR_EQUAL expr { Binary($1,(Logical InfEq,$2),$3) }
 | expr TGREATER              expr { Binary($1,(Logical Sup,$2),$3) }
 | expr T_IS_GREATER_OR_EQUAL expr { Binary($1,(Logical SupEq,$2),$3) }
 | expr T_ROCKET              expr { Binary($1,(CombinedComparison,$2),$3) }

 | TPLUS  expr    %prec T_INC           { Unary((UnPlus,$1),$2) }
 | TMINUS expr    %prec T_INC           { Unary((UnMinus,$1),$2) }
 | TBANG  expr                          { Unary((UnBang,$1),$2) }
 | TTILDE expr                          { Unary((UnTilde,$1),$2) }

 | expr TQUESTION  expr TCOLON  expr	 { CondExpr($1,$2,Some $3,$4,$5) }
 /*(* PHP 5.3 *)*/
 | expr TQUESTION  TCOLON  expr	 { CondExpr($1,$2,None,$3,$4) }
 | expr TQUESTION TQUESTION expr { CondExpr($1,$2,None,$3,$4) }

/*(* I don't parse XHP elements defs in the same way than the original
   * XHP parser, which simplifies the grammar, but introduce possible
   * ambiguities. See tests/xhp_pb_but_ok/colon_ambiguity*.php
   * I don't want to solve those ambiguities but I can at least print a
   * useful parsing error message with those fake rules.
   *
   * Everywhere in this grammar where we use TCOLON we should add
   * an error rule similar to the one below.
   *)*/
 | expr TQUESTION  expr T_XHP_COLONID_DEF
     { H.failwith_xhp_ambiguity_colon (snd $4) }

 | expr T_INSTANCEOF expr  { InstanceOf($1, $2, $3) }


 /*(* less: it would be nicer to have TOPAR TTypename TCPAR
    * but this would require some parsing tricks to sometimes return
    * a TIdent and TTypename like in pfff/lang_cpp/parsing/.
    *)*/
 | T_BOOL_CAST   expr	{ Cast((BoolTy,$1),$2) }
 | T_INT_CAST    expr 	{ Cast((IntTy,$1),$2) }
 | T_DOUBLE_CAST expr 	{ Cast((DoubleTy,$1),$2) }
 | T_STRING_CAST expr	{ Cast((StringTy,$1),$2) }
 | T_ARRAY_CAST  expr 	{ Cast((ArrayTy,$1),$2) }
 | T_OBJECT_CAST expr 	{ Cast((ObjectTy,$1),$2) }

 | T_UNSET_CAST  expr	{ CastUnset($1,$2) }

 | T_EXIT exit_expr	{ Exit($1,$2) }
 | T__AT expr           { At($1,$2) }
 | T_PRINT expr  { Print($1,$2) }


 | T_CLONE expr { Clone($1,$2) }

 /*(* PHP 5.3 Closures *)*/
 | async_opt T_FUNCTION is_reference TOPAR parameter_list TCPAR return_type_opt
   lexical_vars
   TOBRACE inner_statement_list TCBRACE
   { H.validate_parameter_list $5;
     let params = ($4, $5, $6) in
       let body = ($9, $10, $11) in
       Lambda ($8, { f_tok = $2;f_ref = $3;f_params = params; f_body = body;
                     f_tparams = None;
                     f_name = Name("__lambda__", $2);
                     f_return_type = $7; f_type = FunctionLambda;
                     f_modifiers = $1;
                     f_attrs = None;
       })
   }
 /*(* php-facebook-ext: lambda (short closure)s *)*/
 | lambda_expr { $1 }

 /*(* php-facebook-ext: in hphp.y yield are at the statement level
    * and are restricted to a few forms *)*/
 | T_YIELD expr              { Yield ($1, ArrayExpr $2) }
 | T_YIELD expr T_ARROW expr { Yield ($1, ArrayArrowExpr ($2, $3, $4)) }
 | T_YIELD T_BREAK { YieldBreak ($1, $2) }
 /*(* php-facebook-ext: Just like yield, await is at the statement level *) */
 | T_AWAIT expr { Await ($1, $2) }

 /*(* sgrep_ext: *)*/
 | T_ELLIPSIS { Flag_parsing.sgrep_guard (SgrepExprDots $1) }

 | T_INCLUDE      expr 		       { Include($1,$2) }
 | T_INCLUDE_ONCE expr 	               { IncludeOnce($1,$2) }
 | T_REQUIRE      expr		       { Require($1,$2) }
 | T_REQUIRE_ONCE expr		       { RequireOnce($1,$2) }

 | T_EMPTY TOPAR expr TCPAR	       { Empty($1,($2,$3,$4)) }

 | T_EVAL TOPAR expr TCPAR 	       { Eval($1,($2,$3,$4)) }

 | T_ISSET TOPAR expr_list TCPAR { Isset($1, ($2, $3, $4)) }

 | T_LIST TOPAR assignment_list TCPAR TEQ expr
     { AssignList($1,($2,$3,$4),$5,$6) }

/*(* inspired by parser_js.mly *)*/
simple_expr:
 | new_expr { $1 }
 | call_expr { $1 }
 /* TODO: 1 s/r conflict, can not be in primary_expr otherwise
  * $this->fld{...} is not parsed correctly
  */
 | qualified_class_name TOBRACE array_pair_list TCBRACE
     { Collection ($1, ($2, $3, $4)) }
new_expr:
 | member_expr { $1 }
 | T_NEW member_expr { New ($1, $2, None) }
 | T_NEW member_expr arguments { New ($1, $2, Some $3) }

call_expr:
 | member_expr arguments { Call ($1, $2) }
 | call_expr arguments { Call ($1, $2) }
 | call_expr TOBRA dim_offset TCBRA { ArrayGet($1, ($2, $3, $4)) }
 | call_expr TOBRACE expr TCBRACE   { HashGet($1, ($2, $3, $4)) }
 | call_expr T_OBJECT_OPERATOR primary_expr { ObjGet($1, $2, $3) }
 | call_expr T_OBJECT_OPERATOR TOBRACE expr TCBRACE
     { ObjGet($1,$2, (BraceIdent ($3, $4, $5))) }

member_expr:
 | primary_expr { $1 }
 | member_expr TOBRA dim_offset TCBRA { ArrayGet($1, ($2, $3, $4)) }
 | member_expr TOBRACE expr TCBRACE   { HashGet($1, ($2, $3, $4)) }
 | member_expr T_OBJECT_OPERATOR primary_expr {  ObjGet($1, $2, $3) }
 | member_expr T_OBJECT_OPERATOR TOBRACE expr TCBRACE
     { ObjGet($1,$2, (BraceIdent ($3, $4, $5))) }
 | member_expr TCOLCOL primary_expr { ClassGet($1, $2, $3) }
 /*(* php 5.5 extension *)*/
 | member_expr TCOLCOL T_CLASS
     { ClassGet($1, $2, Id (XName [QI (Name("class", $3))])) }


primary_expr:
 | constant { Sc (C $1) }

 | qualified_class_name { Id $1  }
 | T_SELF               { Id (Self $1) }
 | T_PARENT             { Id (Parent $1) }
/*(* php 5.3 late static binding *)*/
 | T_STATIC             { Id (LateStatic $1) }

 | T_VARIABLE { H.mk_var $1 }
 | TDOLLARDOLLAR { H.mk_var ("$$", $1) }

 | TDOLLAR primary_expr         { Deref($1, $2) }
 | TDOLLAR TOBRACE expr TCBRACE { Deref($1, BraceIdent($2, $3, $4)) }

 | T_ARRAY TOPAR array_pair_list TCPAR
     { ArrayLong($1,($2,$3,$4)) }
 | T_SHAPE TOPAR array_pair_list TCPAR
     { ArrayLong($1,($2,$3,$4)) }
 | TOBRA array_pair_list TCBRA
     { ArrayShort($1, $2, $3) }


 | TGUIL encaps_list TGUIL
     { Sc (Guil ($1, $2, $3)) }
 | TBACKQUOTE encaps_list TBACKQUOTE
     { BackQuote($1,$2,$3) }
 | T_START_HEREDOC encaps_list T_END_HEREDOC
     { Sc (HereDoc ($1, $2, $3)) }
 /*(* generated by lexer for special case of ${beer}s. So it's really
    * more a variable than a constant. So I've decided to inline this
    * special case rule in encaps. Maybe this is too restrictive.
    *)*/
  /*(* | T_STRING_VARNAME {  } *)*/

 /*(* xhp: do not put in 'expr', otherwise can't have xhp
    * in function arguments
    *)*/
 | xhp_html { XhpHtml $1 }

 | TOPAR expr TCPAR     { ParenExpr($1,$2,$3) }

constant:
 | T_LNUMBER 			{ Int($1) }
 | T_DNUMBER 			{ Double($1) }
 | T_CONSTANT_ENCAPSED_STRING	{ String($1) }

 | T_LINE { PreProcess(Line, $1) }
 | T_FILE { PreProcess(File, $1) } | T_DIR { PreProcess(Dir, $1) }
 | T_CLASS_C { PreProcess(ClassC, $1) } | T_TRAIT_C { PreProcess(TraitC, $1)}
 | T_FUNC_C { PreProcess(FunctionC, $1) }|T_METHOD_C { PreProcess(MethodC, $1)}
 | T_NAMESPACE_C { PreProcess(NamespaceC, $1) }

static_scalar: expr { $1 }

/*(*----------------------------*)*/
/*(*2 list/array *)*/
/*(*----------------------------*)*/

assignment_list_element:
 | expr				{ ListVar $1 }
 | T_LIST TOPAR assignment_list TCPAR	{ ListList ($1, ($2, $3, $4)) }
 | /*(*empty*)*/			{ ListEmpty }

array_pair_list: array_pair_list_rev { List.rev $1 }
array_pair:
 | expr 			       { (ArrayExpr $1) }
 | TAND expr 		       { (ArrayRef ($1,$2)) }
 | expr T_ARROW expr	       { (ArrayArrowExpr($1,$2,$3)) }
 | expr T_ARROW TAND expr { (ArrayArrowRef($1,$2,$3,$4)) }

/*(*----------------------------*)*/
/*(*2 Calls *)*/
/*(*----------------------------*)*/

arguments: TOPAR function_call_argument_list TCPAR { ($1, $2, $3) }

function_call_argument:
 | expr	{ (Arg ($1)) }
 | TAND expr 		{ (ArgRef($1, $2)) }
 | T_ELLIPSIS expr      { (ArgUnpack($1, $2)) }

/*(*----------------------------*)*/
/*(*2 encaps *)*/
/*(*----------------------------*)*/

encaps:
 | T_ENCAPSED_AND_WHITESPACE
     { EncapsString $1 }
 | T_VARIABLE
     { EncapsVar (H.mk_var $1)  }
 | T_VARIABLE TOBRA encaps_var_offset TCBRA
     { EncapsVar (ArrayGet (H.mk_var $1,($2,Some $3,$4)))}
 | T_VARIABLE T_OBJECT_OPERATOR T_IDENT
     { EncapsVar (ObjGet(H.mk_var $1, $2, Id (XName [QI (Name $3)])))}

 /*(* for ${beer}s. Note that this rule does not exist in the original PHP
    * grammar. Instead only the case with a TOBRA after the T_STRING_VARNAME
    * is covered. The case with only a T_STRING_VARNAME is handled
    * originally in the scalar rule, but it does not makes sense to me
    * as it's really more a variable than a scaler. So for now I have
    * defined this rule. maybe it's too restrictive, we'll see.
    *)*/
 | T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME TCBRACE
     {
       (* this is not really a T_VARIABLE, bit it's still conceptually
        * a variable so we build it almost like above
        *)
       let var = H.mk_var $2 in
       EncapsDollarCurly ($1, var, $3)
     }

 | T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME  TOBRA expr TCBRA  TCBRACE
     {
       let lval = ArrayGet(H.mk_var $2, ($3, Some $4, $5))
       in
       EncapsDollarCurly ($1,  lval, $6)
     }

 /*(* for {$beer}s *)*/
 | T_CURLY_OPEN expr TCBRACE           { EncapsCurly($1, $2, $3) }
 /*(* for ? *)*/
 | T_DOLLAR_OPEN_CURLY_BRACES expr TCBRACE { EncapsExpr ($1, $2, $3) }

encaps_var_offset:
 | T_IDENT	{
     (* It looks like an ident but as we are in encaps_var_offset,
      * PHP allows array access inside strings to omit the quote
      * around fieldname, so it's actually really a Constant (String)
      * rather than an ident, as we usually do for other T_IDENT
      * cases.
      *)
     let cst = String $1 in (* will not have enclosing "'"  as usual *)
     Sc (C cst)
   }
 | T_VARIABLE	{ H.mk_var $1 }
 | T_NUM_STRING	{
     (* the original php lexer does not return some numbers for
      * offset of array access inside strings. Not sure why ...
      *)
     let cst = String $1 in (* will not have enclosing "'"  as usual *)
     Sc (C cst)
   }

/*(*----------------------------*)*/
/*(*2 XHP embedded html *)*/
/*(*----------------------------*)*/
xhp_html:
 | T_XHP_OPEN_TAG xhp_attributes T_XHP_GT xhp_children T_XHP_CLOSE_TAG
     { Xhp ($1, $2, $3, $4, $5)  }
 | T_XHP_OPEN_TAG xhp_attributes T_XHP_SLASH_GT
     { XhpSingleton ($1, $2, $3) }

xhp_child:
 | T_XHP_TEXT           { XhpText $1 }
 | xhp_html             { XhpNested $1 }
 | TOBRACE expr TCBRACE { XhpExpr ($1, $2, $3) }

xhp_attribute:
 | T_XHP_ATTR TEQ xhp_attribute_value { $1, $2, $3 }

xhp_attribute_value:
 | TGUIL encaps_list TGUIL { XhpAttrString ($1, $2, $3) }
 | TOBRACE expr TCBRACE    { XhpAttrExpr ($1, $2, $3) }

 /*(* ugly: one cannot use T_IDENT here, because the lexer is still in
    * XHP mode which means every ident is transformed in a xhp attribute
    *)*/
 /*(* sgrep_ext: *)*/
 | T_XHP_ATTR { Flag_parsing.sgrep_guard (SgrepXhpAttrValueMvar ($1)) }

/*(*----------------------------*)*/
/*(*2 Lambda: succinct closure syntax *)*/
/*(*----------------------------*)*/

lambda_expr:
 /*(* facebook-ext: lambdas (short closures), as in ($x ==> $x + 1) *)*/
 | T_VARIABLE lambda_body
     {
       let sl_tok, sl_body = $2 in
       let sl_params = SLSingleParam (H.mk_param $1) in
       ShortLambda { sl_params; sl_tok; sl_body; sl_modifiers = [] }
     }
 | T_ASYNC T_VARIABLE lambda_body
     {
       let sl_tok, sl_body = $3 in
       let sl_params = SLSingleParam (H.mk_param $2) in
       ShortLambda { sl_params; sl_tok; sl_body; sl_modifiers = [Async,($1)] }
     }
 | T_LAMBDA_OPAR parameter_list T_LAMBDA_CPAR return_type_opt lambda_body
     {
       H.validate_parameter_list $2;
       let sl_tok, sl_body = $5 in
       let sl_params = SLParams ($1, $2, $3) in
       ShortLambda { sl_params; sl_tok; sl_body; sl_modifiers = []; }
     }
 | T_ASYNC T_LAMBDA_OPAR parameter_list T_LAMBDA_CPAR return_type_opt lambda_body
     {
       H.validate_parameter_list $3;
       let sl_tok, sl_body = $6 in
       let sl_params = SLParams ($2, $3, $4) in
       ShortLambda { sl_params; sl_tok; sl_body; sl_modifiers = [Async,($1)]; }
     }
 | T_ASYNC TOBRACE inner_statement_list TCBRACE
     {
       let sl_body = SLBody ($2, $3, $4) in
       ShortLambda { sl_params = SLParamsOmitted;
                     sl_tok = None;
                     sl_body;
                     sl_modifiers = [Async,($1)];
                   }
     }

lambda_body:
 | T_DOUBLE_ARROW TOBRACE inner_statement_list TCBRACE { (Some $1, SLBody ($2, $3, $4)) }
 /*(* An explicit case required for when/if awaits become statements, not expr *)
   (* | T_DOUBLE_ARROW T_AWAIT expr { ($1, SLExpr (Await ($2, $3))) } *)*/
 /*(* see conflicts.txt for why the %prec *)*/
 | T_DOUBLE_ARROW expr { (Some $1, SLExpr $2) }

/*(*----------------------------*)*/
/*(*2 auxillary bis *)*/
/*(*----------------------------*)*/

dim_offset:
 | /*(*empty*)*/   { None }
 | expr		   { Some $1 }

exit_expr:
 | /*(*empty*)*/	{ None }
 | TOPAR TCPAR		{ Some($1, None, $2) }
 | TOPAR expr TCPAR	{ Some($1, Some $2, $3) }


/*(*************************************************************************)*/
/*(*1 Ident *)*/
/*(*************************************************************************)*/

ident:
 | T_IDENT { $1 }
/*(* xhp: it is ok to use XHP keywords in place where regular PHP names
   * are expected as in 'function children($x) { ... }'.
   *
   * We extend here the grammar to support those "overloading". An
   * alternative would be to extend the lexer to only lex XHP keywords
   * in certain context, but this would force to share some states between
   * the lexer and parser.
   *
   * less? emit a warning when the user use XHP keywords for regular idents ?
   *)*/
 | T_XHP_ATTRIBUTE { PI.str_of_info $1, $1 }
 | T_XHP_CATEGORY  { PI.str_of_info $1, $1 }
 | T_XHP_CHILDREN  { PI.str_of_info $1, $1 }
 | T_ENUM   { PI.str_of_info $1, $1 }
 | T_XHP_ANY    { PI.str_of_info $1, $1 }
 | T_XHP_PCDATA { PI.str_of_info $1, $1 }

 | T_TYPE      { PI.str_of_info $1, $1 }
 | T_NEWTYPE   { PI.str_of_info $1, $1 }
 | T_SUPER     { PI.str_of_info $1, $1 }

ident_class_name:
  | ident             { Name $1 }
 /*(*s: class_name grammar rule hook *)*/
  /*(* xhp: an XHP element def *)*/
  | T_XHP_COLONID_DEF { XhpName $1 }
 /*(*e: class_name grammar rule hook *)*/

/*(* ugly, php allows method names which should be IMHO reserved keywords *)*/
ident_method_name:
 | ident { $1 }
 | T_PARENT { "parent", $1 }
 | T_SELF   { "self", $1 }
 | T_ASYNC  { "async", $1 }

ident_xhp_attr_name_atom:
 /*(* could put T_IDENT but even XHP keywords are accepted as XHP attributes*)*/
 | ident { snd $1 }

 /*(* Just like it's ok (but not good IMHO) to use XHP keywords in place
    * of regular PHP idents, it's ok to use PHP keywords in place
    * of XHP attribute names (but again not good IMHO).
    *
    * The list of tokens below are all identifier-like keywords mentioned in
    * the 'keyword tokens' section at the beginning of this file
    * (which roughly correspond to the tokens in Lexer_php.keywords_table).
    * There is no conflict introducing this big list of tokens.
    *
    * less? emit a warning when the user use PHP keywords for XHP attribute ?
    *)*/
 | T_ECHO { $1 } | T_PRINT { $1 } | T_IF { $1 } | T_ELSE { $1 }
 | T_ELSEIF { $1 } | T_ENDIF { $1 } | T_DO { $1 } | T_WHILE { $1 }
 | T_ENDWHILE { $1 } | T_FOR { $1 } | T_ENDFOR { $1 } | T_FOREACH { $1 }
 | T_ENDFOREACH { $1 } | T_SWITCH { $1 } | T_ENDSWITCH { $1 } | T_CASE { $1 }
 | T_DEFAULT { $1 } | T_BREAK { $1 } | T_CONTINUE { $1 } | T_RETURN { $1 }
 | T_TRY { $1 } | T_CATCH { $1 } | T_FINALLY { $1 } | T_THROW { $1 }
 | T_EXIT { $1 } | T_DECLARE { $1 } | T_ENDDECLARE { $1 } | T_USE { $1 }
 | T_GLOBAL { $1 } | T_AS { $1 } | T_FUNCTION { $1 } | T_CONST { $1 }
 | T_STATIC { $1 } | T_ABSTRACT { $1 } | T_FINAL { $1 } | T_PRIVATE { $1 }
 | T_PROTECTED { $1 } | T_PUBLIC { $1 } | T_VAR { $1 } | T_UNSET { $1 }
 | T_ISSET { $1 } | T_EMPTY { $1 } | T_CLASS { $1 }
 | T_INTERFACE { $1 } | T_EXTENDS { $1 } | T_IMPLEMENTS { $1 } | T_LIST { $1 }
 | T_ARRAY { $1 } | T_CLASS_C { $1 } | T_METHOD_C { $1 } | T_FUNC_C { $1 }
 | T_LINE { $1 } | T_FILE { $1 } | T_LOGICAL_OR { $1 } | T_LOGICAL_AND { $1 }
 | T_LOGICAL_XOR { $1 } | T_NEW { $1 } | T_CLONE { $1 } | T_INSTANCEOF { $1 }
 | T_INCLUDE { $1 } | T_INCLUDE_ONCE { $1 } | T_REQUIRE { $1 }
 | T_REQUIRE_ONCE { $1 } | T_EVAL { $1 } | T_SELF { $1 } | T_PARENT { $1 }
 | T_TRAIT { $1 } | T_INSTEADOF { $1 } | T_TRAIT_C { $1 }
 | T_NAMESPACE { $1 } | T_NAMESPACE_C { $1 }
 | T_ASYNC { $1 } | T_AWAIT { $1 }

/*(*************************************************************************)*/
/*(*1 Namespace *)*/
/*(*************************************************************************)*/

namespace_declaration:
 | T_NAMESPACE namespace_name TSEMICOLON
     { NamespaceDef ($1, $2, $3) }
 | T_NAMESPACE namespace_name TOBRACE top_statement_list TCBRACE
     { NamespaceBracketDef ($1, Some $2, ($3, H.squash_stmt_list $4, $5)) }
 | T_NAMESPACE                TOBRACE top_statement_list TCBRACE
     { NamespaceBracketDef ($1, None, ($2, H.squash_stmt_list $3, $4)) }

use_declaration:
 | T_USE use_declaration_name_list TSEMICOLON { NamespaceUse ($1, $2, $3) }

namespace_name:
 | ident                           { [QI (Name $1)] }
 | namespace_name TANTISLASH ident { $1 @ [QITok $2; QI (Name $3)] }

use_declaration_name:
 | namespace_name { ImportNamespace $1 }
 | namespace_name T_AS ident { AliasNamespace ($1, $2, Name $3) }
 | TANTISLASH namespace_name { ImportNamespace (QITok $1::$2) }
 | TANTISLASH namespace_name T_AS ident
     { AliasNamespace (QITok $1::$2, $3, Name $4) }


qualified_name:
 | namespace_name                        { XName $1 }
 | TANTISLASH namespace_name             { XName (QITok $1::$2) }
 | T_NAMESPACE TANTISLASH namespace_name
     { XName (QI (Name ("namespace", $1))::QITok $2::$3) }

/*(* Should we have 'ident type_arguments' below? No because
   * we allow type arguments only at a few places, for instance
   * in 'class X extends A<int> { ... }' but not inside expressions
   * as in 'new X<int>(...)' so we need both.
   * This is currently equivalent to 'ident_class_name' but adding
   * namespace at some point may change that.
   *)*/
qualified_class_name:
  | qualified_name { $1 }
  /*(* xhp: an XHP element use *)*/
  | T_XHP_COLONID_DEF { XName [QI (XhpName $1)] }

qualified_class_name_or_array:
 | qualified_class_name { $1 }
 | T_ARRAY { XName [QI (Name ("array", $1))] }

qualified_name_for_traits: qualified_class_name { $1 }

/*(*************************************************************************)*/
/*(*1 Name *)*/
/*(*************************************************************************)*/

class_name: qualified_class_name_or_array type_arguments
  { Hint ($1, $2) }

class_name_no_array: qualified_class_name type_arguments
  { Hint ($1, $2) }

/*(*************************************************************************)*/
/*(*1 xxx_list, xxx_opt *)*/
/*(*************************************************************************)*/
top_statement_list:
 | top_statement_list  top_statement { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

inner_statement_list:
 | inner_statement_list  inner_statement { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

class_statement_list:
 | class_statement_list class_statement { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

enum_statement_list:
 | enum_statement_list enum_statement { $1 @ [$2] }
 | /*(*empty*)*/ { [] }




trait_rules:
 | trait_rules trait_rule { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

additional_catches:
 | non_empty_additional_catches { $1 }
 | /*(*empty*)*/ { [] }

non_empty_additional_catches:
 | additional_catch                              { [$1] }
 | non_empty_additional_catches additional_catch { $1 @ [$2] }

optional_finally_clause:
 | finally_clause { [$1] }
 | /*(*empty*)*/  { [] }

method_modifiers:
 | /*(*empty*)*/				{ [] }
 | non_empty_member_modifiers			{ $1 }

non_empty_member_modifiers:
 | member_modifier				{ [$1] }
 | non_empty_member_modifiers member_modifier	{ $1 @ [$2] }



unset_variables:
 | unset_variable { [Left $1] }
 | unset_variables TCOMMA unset_variable { $1 @ [Right $2; Left $3] }

global_var_list:
 | global_var				{ [Left $1] }
 | global_var_list TCOMMA global_var	{ $1 @ [Right $2; Left $3] }

echo_expr_list:
 | expr				   { [Left $1] }
 | echo_expr_list TCOMMA expr      { $1 @ [Right $2; Left $3] }

expr_list:
 | expr				   { [Left $1] }
 | expr_list TCOMMA expr      { $1 @ [Right $2; Left $3] }

use_declaration_name_list:
 | use_declaration_name
     { [Left $1] }
 | use_declaration_name_list TCOMMA use_declaration_name
     { $1@[Right $2;Left $3] }

declare_list:
 | declare                    	{ [Left $1] }
 | declare_list TCOMMA declare	{ $1 @ [Right $2; Left $3] }

non_empty_for_expr:
  | expr      			     { [Left $1] }
  | non_empty_for_expr TCOMMA	expr { $1 @ [Right $2; Left $3] }

xhp_attribute_decls:
 | xhp_attribute_decl { [Left $1] }
 | xhp_attribute_decls TCOMMA xhp_attribute_decl { $1 @ [Right $2; Left $3] }

xhp_enum_list:
 | xhp_enum { [Left $1] }
 | xhp_enum_list TCOMMA xhp_enum { $1 @ [Right $2; Left $3] }

xhp_category_list:
 | xhp_category { [Left $1] }
 | xhp_category_list TCOMMA xhp_category { $1 @ [Right $2; Left $3] }

attribute_list:
 | attribute				   { [Left $1] }
 | attribute_list TCOMMA attribute         { $1 @ [Right $2; Left $3] }

attribute_argument_list:
 | /*(*empty*)*/ { [] }
 | attribute_argument { [Left $1] }
 | attribute_argument_list TCOMMA attribute_argument { $1@[Right $2; Left $3]}

static_var_list:
 | static_var                        { [Left $1] }
 | static_var_list TCOMMA static_var { $1 @ [Right $2; Left $3] }

class_variable_declaration:
 | class_variable    { [Left $1] }
 /*(*s: repetitive class_variable_declaration with comma *)*/
 | class_variable_declaration TCOMMA class_variable { $1@[Right $2;Left $3] }
 /*(*e: repetitive class_variable_declaration with comma *)*/

/*(* less: should we allow the DOTS only for the end? *)*/
non_empty_type_php_or_dots_list:
 | type_php_or_dots                                     { [$1] }
 | non_empty_type_php_or_dots_list TCOMMA type_php_or_dots { $1 @ [Right3 $2; $3]}

non_empty_type_php_list:
 | non_empty_type_php_list_bis { $1 }
 | non_empty_type_php_list_bis TCOMMA { $1 @ [Right $2] }

non_empty_type_php_list_bis:
 | type_php                                { [Left $1] }
 | non_empty_type_php_list_bis TCOMMA type_php  { $1 @ [Right $2; Left $3] }

class_name_list:
 | class_name_no_array { [Left $1] }
 | class_name_list TCOMMA class_name_no_array { $1 @ [Right $2; Left $3]}

class_constants_declaration:
 | class_constant_declaration { [Left $1] }
 | class_constants_declaration TCOMMA class_constant_declaration
     { $1 @ [Right $2; Left $3] }

possible_comma:
 | /*(*empty*)*/ { [] }
 | TCOMMA        { [Right $1] }

return_type_opt:
 | return_type       { Some $1 }
 | /*(*empty*)*/     { None }

attributes_opt:
  | attributes    { Some $1 }
  | /*(*empty*)*/ { None }

type_php_opt:
  | type_php      { Some $1 }
  | /*(*empty*)*/ { None }

at_opt:
  | T__AT         { Some $1 }
  | /*(*empty*)*/ { None }

ctor_modifier_opt:
  | ctor_modifier      { Some $1 }
  | /*(*empty*)*/ { None }

function_call_argument_list:
 | /*(*empty*)*/                              { [] }
 | non_empty_function_call_argument_list      { $1 }
 /*(* php-facebook-ext: trailing comma *)*/
 | non_empty_function_call_argument_list TCOMMA  { $1 @ [Right $2] }

non_empty_function_call_argument_list:
 | function_call_argument { [Left $1] }
 /*(*s: repetitive non_empty_function_call_parameter_list *)*/
 | non_empty_function_call_argument_list TCOMMA function_call_argument
      { $1 @ [Right $2; Left $3] }
 /*(*e: repetitive non_empty_function_call_parameter_list *)*/

assignment_list:
 | assignment_list_element                        { [Left $1] }
 | assignment_list TCOMMA assignment_list_element { $1 @ [Right $2; Left $3] }

shape_field_list:
 | /*(*empty*)*/ { [] }
 | non_empty_shape_field_list { $1 }
 | non_empty_shape_field_list TCOMMA { $1 @ [Right $2] }

non_empty_shape_field_list:
 | shape_field   { [Left $1] }
 | non_empty_shape_field_list TCOMMA shape_field  { $1 @ [Right $2; Left $3] }

shape_field: expr T_ARROW type_php { $1, $2, $3 }

non_empty_array_pair_list_rev:
 | array_pair { [Left $1] }
 /*(*s: repetitive non_empty_array_pair_list *)*/
 | non_empty_array_pair_list_rev TCOMMA array_pair  { Left $3::Right $2::$1 }
 /*(*e: repetitive non_empty_array_pair_list *)*/

array_pair_list_rev:
 | /*(*empty*)*/ { [] }
 | non_empty_array_pair_list_rev possible_comma	{ $2@$1 }

encaps_list:
 | encaps_list encaps                  { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

xhp_attributes:
 | xhp_attributes xhp_attribute { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

xhp_children:
 | xhp_children xhp_child { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

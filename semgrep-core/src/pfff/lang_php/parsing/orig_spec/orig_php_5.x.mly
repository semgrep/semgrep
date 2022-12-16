%{
(* src: ocamlyaccified from zend_language_parser.y in php source. *)

open Common
open AbstractSyntax
exception Parsing of string
%}

/*
   +----------------------------------------------------------------------+
   | Zend Engine                                                          |
   +----------------------------------------------------------------------+
   | Copyright c 1998-2006 Zend Technologies Ltd. (http://www.zend.com) |
   +----------------------------------------------------------------------+
   | This source file is subject to version 2.00 of the Zend license,     |
   | that is bundled with this package in the file LICENSE, and is        |
   | available through the world-wide-web at the following url:           |
   | http://www.zend.com/license/2_00.txt.                                |
   | If you did not receive a copy of the Zend license and are unable to  |
   | obtain it through the world-wide-web, please send a note to          |
   | license@zend.com so we can mail you a copy immediately.              |
   +----------------------------------------------------------------------+
   | Authors: Andi Gutmans <andi@zend.com>                                |
   |          Zeev Suraski <zeev@zend.com>                                |
   +----------------------------------------------------------------------+
*/

/* $Id: zend_language_parser.y 263383 2008-07-24 11:47:14Z dmitry $ */

/*
 * LALR shift/reduce conflicts and how they are resolved:
 *
 * - 2 shift/reduce conflicts due to the dangeling elseif/else ambiguity.  Solved by shift.
 *
 */



/*
%pure_parser
%expect 2
*/



%token T_LNUMBER
%token T_DNUMBER
%token T_STRING
%token T_STRING_VARNAME
%token T_VARIABLE
%token T_NUM_STRING

%token T_INLINE_HTML
%token T_CHARACTER
%token T_BAD_CHARACTER
%token T_ENCAPSED_AND_WHITESPACE
%token T_CONSTANT_ENCAPSED_STRING

%token T_ECHO

%token T_DO
%token T_WHILE
%token T_ENDWHILE
%token T_FOR
%token T_ENDFOR
%token T_FOREACH
%token T_ENDFOREACH
%token T_DECLARE
%token T_ENDDECLARE
%token T_AS
%token T_SWITCH
%token T_ENDSWITCH
%token T_CASE
%token T_DEFAULT
%token T_BREAK
%token T_CONTINUE
%token T_FUNCTION
%token T_CONST
%token T_RETURN
%token T_TRY
%token T_CATCH
%token T_THROW
%token T_USE
%token T_GLOBAL

/* pad: was declared via right ... ??? mean token ? */
%token T_STATIC T_ABSTRACT T_FINAL T_PRIVATE T_PROTECTED T_PUBLIC

%token T_VAR
%token T_UNSET
%token T_ISSET
%token T_EMPTY
%token T_HALT_COMPILER
%token T_CLASS
%token T_INTERFACE
%token T_EXTENDS
%token T_IMPLEMENTS
%token T_OBJECT_OPERATOR
%token T_DOUBLE_ARROW
%token T_LIST
%token T_ARRAY
%token T_CLASS_C
%token T_METHOD_C
%token T_FUNC_C

%token T_LINE
%token T_FILE

%token T_COMMENT
%token T_DOC_COMMENT
%token T_OPEN_TAG
%token T_OPEN_TAG_WITH_ECHO
%token T_CLOSE_TAG
%token T_WHITESPACE
%token T_START_HEREDOC
%token T_END_HEREDOC
%token T_DOLLAR_OPEN_CURLY_BRACES
%token T_CURLY_OPEN
%token T_PAAMAYIM_NEKUDOTAYIM

%token T_EXIT
%token T_IF
/* pad: was only declared as left/right, without a token decl in orig gram */

/* declared implicitely cos was using directly the character */
%token TOPAR TCPAR
%token TOBRACE TCBRACE
%token TCBRA
%token TBACKQUOTE
%token TSEMICOLON
%token TDOLLAR
%token TGUIL






%left T_INCLUDE T_INCLUDE_ONCE T_EVAL T_REQUIRE T_REQUIRE_ONCE
%left TCOMMA
%left T_LOGICAL_OR
%left T_LOGICAL_XOR
%left T_LOGICAL_AND
%right T_PRINT
%left TEQ T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL
%left TQUESTION TCOLON
%left T_BOOLEAN_OR
%left T_BOOLEAN_AND
%left TOR
%left THAT
%left TAND
%nonassoc T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL
%nonassoc TSMALLER T_IS_SMALLER_OR_EQUAL TGREATER T_IS_GREATER_OR_EQUAL
%left T_SL T_SR
%left TPLUS TMINUS TDOT
%left TMUL TDIV TMOD
%right TBANG
%nonassoc T_INSTANCEOF
%right TTILDE T_INC T_DEC T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST
%right TAT
%right TOBRA
%nonassoc T_NEW T_CLONE
%left T_ELSEIF
%left T_ELSE
%left T_ENDIF



%start start
%type <int list> start


%%

start:
	top_statement_list { [] }
;

top_statement_list:
		top_statement_list  top_statement { }
	|	/* empty */ { }
;


top_statement:
		statement { }
	|	function_declaration_statement	{  }
	|	class_declaration_statement		{  }
	|	T_HALT_COMPILER TOPAR TCPAR TSEMICOLON   {  }
;


inner_statement_list:
		inner_statement_list  inner_statement {  }
	|	/* empty */ { }
;


inner_statement:
		statement { }
	|	function_declaration_statement { }
	|	class_declaration_statement { }
	|	T_HALT_COMPILER TOPAR TCPAR TSEMICOLON   {  }
;


statement:
		unticked_statement {  }
;

unticked_statement:
		TOBRACE inner_statement_list TCBRACE { }
	|	T_IF TOPAR expr TCPAR statement elseif_list else_single { }
	|	T_IF TOPAR expr TCPAR TCOLON inner_statement_list new_elseif_list new_else_single T_ENDIF TSEMICOLON { }
	|	T_WHILE TOPAR expr  TCPAR while_statement { }
	|	T_DO statement T_WHILE TOPAR expr TCPAR TSEMICOLON { }
	|	T_FOR
			TOPAR
				for_expr
			TSEMICOLON
				for_expr
			TSEMICOLON
				for_expr
			TCPAR
			for_statement {  }
	|	T_SWITCH TOPAR expr TCPAR	switch_case_list {}
	|	T_BREAK TSEMICOLON				{}
	|	T_BREAK expr TSEMICOLON		{}
	|	T_CONTINUE TSEMICOLON			{}
	|	T_CONTINUE expr TSEMICOLON		{}
	|	T_RETURN TSEMICOLON						{}
	|	T_RETURN expr_without_variable TSEMICOLON	{}
	|	T_RETURN variable TSEMICOLON				{}
	|	T_GLOBAL global_var_list TSEMICOLON { }
	|	T_STATIC static_var_list TSEMICOLON { }
	|	T_ECHO echo_expr_list TSEMICOLON { }
	|	T_INLINE_HTML			{}
	|	expr TSEMICOLON				{}
	|	T_USE use_filename TSEMICOLON		{}
	|	T_UNSET TOPAR unset_variables TCPAR TSEMICOLON { }
	|	T_FOREACH TOPAR variable T_AS
		foreach_variable foreach_optional_arg TCPAR
		foreach_statement {}
	|	T_FOREACH TOPAR expr_without_variable T_AS
		variable foreach_optional_arg TCPAR
		foreach_statement {}
	|	T_DECLARE  TOPAR declare_list TCPAR declare_statement {}
	|	TSEMICOLON		/* empty statement */ { }
	|	T_TRY  TOBRACE inner_statement_list TCBRACE
		T_CATCH TOPAR
		fully_qualified_class_name
		T_VARIABLE TCPAR
		TOBRACE inner_statement_list TCBRACE
		additional_catches { }
	|	T_THROW expr TSEMICOLON {}
;


additional_catches:
		non_empty_additional_catches {}
	|	/* empty */ {}
;

non_empty_additional_catches:
		additional_catch {}
	|	non_empty_additional_catches additional_catch {}
;


additional_catch:
	T_CATCH TOPAR fully_qualified_class_name T_VARIABLE TCPAR TOBRACE inner_statement_list TCBRACE { }
;


unset_variables:
		unset_variable { }
	|	unset_variables TCOMMA unset_variable { }
;

unset_variable:
		variable	{}
;

use_filename:
		T_CONSTANT_ENCAPSED_STRING			{}
	|	TOPAR T_CONSTANT_ENCAPSED_STRING TCPAR	{}
;


function_declaration_statement:
		unticked_function_declaration_statement	{}
;

class_declaration_statement:
		unticked_class_declaration_statement	{}
;


is_reference:
		/* empty */	{}
	|	TAND			{}
;


unticked_function_declaration_statement:
		T_FUNCTION is_reference T_STRING
			TOPAR parameter_list TCPAR TOBRACE inner_statement_list TCBRACE { }
;

unticked_class_declaration_statement:
		class_entry_type T_STRING extends_from
			implements_list
			TOBRACE
				class_statement_list
			TCBRACE { }
	|	interface_entry T_STRING
			interface_extends_list
			TOBRACE
				class_statement_list
			TCBRACE {}
;


class_entry_type:
		T_CLASS			{}
	|	T_ABSTRACT T_CLASS {}
	|	T_FINAL T_CLASS {}
;

extends_from:
		/* empty */					{}
	|	T_EXTENDS fully_qualified_class_name	{}
;

interface_entry:
	T_INTERFACE		{}
;

interface_extends_list:
		/* empty */ { }
	|	T_EXTENDS interface_list { }
;

implements_list:
		/* empty */ { }
	|	T_IMPLEMENTS interface_list { }
;

interface_list:
		fully_qualified_class_name			{}
	|	interface_list TCOMMA fully_qualified_class_name {}
;

foreach_optional_arg:
		/* empty */						{}
	|	T_DOUBLE_ARROW foreach_variable	{}
;


foreach_variable:
		variable			{}
	|	TAND variable		{}
;

for_statement:
		statement { }
	|	TCOLON inner_statement_list T_ENDFOR TSEMICOLON { }
;


foreach_statement:
		statement { }
	|	TCOLON inner_statement_list T_ENDFOREACH TSEMICOLON { }
;


declare_statement:
		statement { }
	|	TCOLON inner_statement_list T_ENDDECLARE TSEMICOLON { }
;


declare_list:
		T_STRING TEQ static_scalar					{}
	|	declare_list TCOMMA T_STRING TEQ static_scalar	{}
;


switch_case_list:
		TOBRACE case_list TCBRACE					{}
	|	TOBRACE TSEMICOLON case_list TCBRACE				{}
	|	TCOLON case_list T_ENDSWITCH TSEMICOLON		{}
	|	TCOLON TSEMICOLON case_list T_ENDSWITCH TSEMICOLON	{}
;


case_list:
		/* empty */	{}
	|	case_list T_CASE expr case_separator inner_statement_list {}
	|	case_list T_DEFAULT case_separator inner_statement_list {}
;


case_separator:
		TCOLON { }
	|	TSEMICOLON { }
;


while_statement:
		statement { }
	|	TCOLON inner_statement_list T_ENDWHILE TSEMICOLON { }
;



elseif_list:
		/* empty */ { }
	|	elseif_list T_ELSEIF TOPAR expr TCPAR statement {}
;


new_elseif_list:
		/* empty */ { }
	|	new_elseif_list T_ELSEIF TOPAR expr TCPAR TCOLON inner_statement_list {}
;


else_single:
		/* empty */ { }
	|	T_ELSE statement { }
;


new_else_single:
		/* empty */ { }
	|	T_ELSE TCOLON inner_statement_list { }
;


parameter_list:
		non_empty_parameter_list { }
	|	/* empty */ { }
;


non_empty_parameter_list:
		optional_class_type T_VARIABLE				{}
	|	optional_class_type TAND T_VARIABLE			{}
	|	optional_class_type TAND T_VARIABLE TEQ static_scalar			{}
	|	optional_class_type T_VARIABLE TEQ static_scalar				{}
	|	non_empty_parameter_list TCOMMA optional_class_type T_VARIABLE 	{}
	|	non_empty_parameter_list TCOMMA optional_class_type TAND T_VARIABLE	{}
	|	non_empty_parameter_list TCOMMA optional_class_type TAND T_VARIABLE	 TEQ static_scalar {}
	|	non_empty_parameter_list TCOMMA optional_class_type T_VARIABLE TEQ static_scalar 	{}
;


optional_class_type:
		/* empty */		{}
	|	T_STRING		{}
	|	T_ARRAY		{}
;


function_call_parameter_list:
		non_empty_function_call_parameter_list	{}
	|	/* empty */				{}
;


non_empty_function_call_parameter_list:
		expr_without_variable	{}
	|	variable				{}
	|	TAND w_variable 				{}
	|	non_empty_function_call_parameter_list TCOMMA expr_without_variable	{}
	|	non_empty_function_call_parameter_list TCOMMA variable					{}
	|	non_empty_function_call_parameter_list TCOMMA TAND w_variable			{}
;

global_var_list:
		global_var_list TCOMMA global_var	{}
	|	global_var						{}
;


global_var:
		T_VARIABLE			{}
	|	TDOLLAR r_variable		{}
	|	TDOLLAR TOBRACE expr TCBRACE	{}
;


static_var_list:
		static_var_list TCOMMA T_VARIABLE {}
	|	static_var_list TCOMMA T_VARIABLE TEQ static_scalar {}
	|	T_VARIABLE  {}
	|	T_VARIABLE TEQ static_scalar {}

;


class_statement_list:
		class_statement_list class_statement { }
	|	/* empty */ { }
;


class_statement:
		variable_modifiers class_variable_declaration TSEMICOLON { }
	|	class_constant_declaration TSEMICOLON { }
	|	method_modifiers T_FUNCTION is_reference T_STRING  TOPAR
			parameter_list TCPAR method_body { }
;


method_body:
		TSEMICOLON /* abstract method */		{}
	|	TOBRACE inner_statement_list TCBRACE	{}
;

variable_modifiers:
		non_empty_member_modifiers		{}
	|	T_VAR							{}
;

method_modifiers:
		/* empty */							{}
	|	non_empty_member_modifiers			{}
;

non_empty_member_modifiers:
		member_modifier						{}
	|	non_empty_member_modifiers member_modifier	{}
;

member_modifier:
		T_PUBLIC				{}
	|	T_PROTECTED				{}
	|	T_PRIVATE				{}
	|	T_STATIC				{}
	|	T_ABSTRACT				{}
	|	T_FINAL					{}
;

class_variable_declaration:
		class_variable_declaration TCOMMA T_VARIABLE					{}
	|	class_variable_declaration TCOMMA T_VARIABLE TEQ static_scalar	{}
	|	T_VARIABLE						{}
	|	T_VARIABLE TEQ static_scalar	{}
;

class_constant_declaration:
		class_constant_declaration TCOMMA T_STRING TEQ static_scalar	{}
	|	T_CONST T_STRING TEQ static_scalar	{}
;

echo_expr_list:
		echo_expr_list TCOMMA expr {}
	|	expr					{}
;


for_expr:
		/* empty */			{}
	|	non_empty_for_expr	{}
;

non_empty_for_expr:
		non_empty_for_expr TCOMMA	expr {}
	|	expr					{}
;

expr_without_variable:
		T_LIST TOPAR assignment_list TCPAR TEQ expr {}
	|	variable TEQ expr		{}
	|	variable TEQ TAND variable {}
	|	variable TEQ TAND T_NEW class_name_reference ctor_arguments {}
	|	T_NEW class_name_reference ctor_arguments {}
	|	T_CLONE expr {}
	|	variable T_PLUS_EQUAL expr 	{}
	|	variable T_MINUS_EQUAL expr	{}
	|	variable T_MUL_EQUAL expr		{}
	|	variable T_DIV_EQUAL expr		{}
	|	variable T_CONCAT_EQUAL expr	{}
	|	variable T_MOD_EQUAL expr		{}
	|	variable T_AND_EQUAL expr		{}
	|	variable T_OR_EQUAL expr 		{}
	|	variable T_XOR_EQUAL expr 		{}
	|	variable T_SL_EQUAL expr	{}
	|	variable T_SR_EQUAL expr	{}
	|	rw_variable T_INC {}
	|	T_INC rw_variable {}
	|	rw_variable T_DEC {}
	|	T_DEC rw_variable {}
	|	expr T_BOOLEAN_OR expr {}
	|	expr T_BOOLEAN_AND  expr {}
	|	expr T_LOGICAL_OR  expr {}
	|	expr T_LOGICAL_AND  expr {}
	|	expr T_LOGICAL_XOR expr {}
	|	expr TOR expr	{}
	|	expr TAND expr	{}
	|	expr THAT expr	{}
	|	expr TDOT expr 	{}
	|	expr TPLUS expr 	{}
	|	expr TMINUS expr 	{}
	|	expr TMUL expr	{}
	|	expr TDIV expr	{}
	|	expr TMOD expr 	{}
	| 	expr T_SL expr	{}
	|	expr T_SR expr	{}
	|	TPLUS expr %prec T_INC {}
	|	TMINUS expr %prec T_INC {}
	|	TBANG expr {}
	|	TTILDE expr {}
	|	expr T_IS_IDENTICAL expr		{}
	|	expr T_IS_NOT_IDENTICAL expr	{}
	|	expr T_IS_EQUAL expr			{}
	|	expr T_IS_NOT_EQUAL expr 		{}
	|	expr TSMALLER expr 					{}
	|	expr T_IS_SMALLER_OR_EQUAL expr {}
	|	expr TGREATER expr 					{}
	|	expr T_IS_GREATER_OR_EQUAL expr {}
	|	expr T_INSTANCEOF class_name_reference {}
	|	TOPAR expr TCPAR 	{}
	|	expr TQUESTION
		expr TCOLON
		expr	 {}
	|	internal_functions_in_yacc {}
	|	T_INT_CAST expr 	{}
	|	T_DOUBLE_CAST expr 	{}
	|	T_STRING_CAST expr	{}
	|	T_ARRAY_CAST expr 	{}
	|	T_OBJECT_CAST expr 	{}
	|	T_BOOL_CAST expr	{}
	|	T_UNSET_CAST expr	{}
	|	T_EXIT exit_expr	{}
	|	TAT expr {}
	|	scalar				{}
	|	T_ARRAY TOPAR array_pair_list TCPAR {}
	|	TBACKQUOTE encaps_list TBACKQUOTE {}
	|	T_PRINT expr  {}
;

function_call:
		T_STRING	TOPAR
				function_call_parameter_list
				TCPAR {}
	|	fully_qualified_class_name T_PAAMAYIM_NEKUDOTAYIM T_STRING TOPAR
			function_call_parameter_list
			TCPAR {}
	|	fully_qualified_class_name T_PAAMAYIM_NEKUDOTAYIM variable_without_objects TOPAR
			function_call_parameter_list
			TCPAR {}
	|	variable_without_objects  TOPAR
			function_call_parameter_list TCPAR
			{}
;

fully_qualified_class_name:
		T_STRING {}
;

class_name_reference:
		T_STRING				{}
	|	dynamic_class_name_reference	{}
;


dynamic_class_name_reference:
		base_variable T_OBJECT_OPERATOR
			object_property dynamic_class_name_variable_properties
			{}
	|	base_variable {}
;


dynamic_class_name_variable_properties:
		dynamic_class_name_variable_properties dynamic_class_name_variable_property { }
	|	/* empty */ { }
;


dynamic_class_name_variable_property:
		T_OBJECT_OPERATOR object_property {}
;

exit_expr:
		/* empty */	{}
	|	TOPAR TCPAR		{}
	|	TOPAR expr TCPAR	{}
;


ctor_arguments:
		/* empty */	{}
	|	TOPAR function_call_parameter_list TCPAR	{}
;


common_scalar:
		T_LNUMBER 					{}
	|	T_DNUMBER 					{}
	|	T_CONSTANT_ENCAPSED_STRING	{}
	|	T_LINE 						{}
	|	T_FILE 						{}
	|	T_CLASS_C					{}
	|	T_METHOD_C					{}
	|	T_FUNC_C					{}
;


static_scalar: /* compile-time evaluated scalars */
		common_scalar		{}
	|	T_STRING 		{}
	|	TPLUS static_scalar	{}
	|	TMINUS static_scalar	{}
	|	T_ARRAY TOPAR static_array_pair_list TCPAR {}
	|	static_class_constant {}
;

static_class_constant:
		T_STRING T_PAAMAYIM_NEKUDOTAYIM T_STRING {}
;

scalar:
		T_STRING 				{}
	|	T_STRING_VARNAME		{}
	|	class_constant	{}
	|	common_scalar			{}
	|	TGUIL encaps_list TGUIL 	{}
	|	T_START_HEREDOC encaps_list T_END_HEREDOC {}
;


static_array_pair_list:
		/* empty */ {}
	|	non_empty_static_array_pair_list possible_comma	{}
;

possible_comma:
		/* empty */ { }
	|	TCOMMA { }
;

non_empty_static_array_pair_list:
		non_empty_static_array_pair_list TCOMMA static_scalar T_DOUBLE_ARROW static_scalar	{}
	|	non_empty_static_array_pair_list TCOMMA static_scalar {}
	|	static_scalar T_DOUBLE_ARROW static_scalar {}
	|	static_scalar {}
;

expr:
		r_variable					{}
	|	expr_without_variable		{}
;


r_variable:
	variable {}
;


w_variable:
	variable	{}
;

rw_variable:
	variable	{}
;

variable:
		base_variable_with_function_calls T_OBJECT_OPERATOR
			object_property method_or_not variable_properties
			{}
	|	base_variable_with_function_calls {}
;

variable_properties:
		variable_properties variable_property {}
	|	/* empty */ {}
;


variable_property:
		T_OBJECT_OPERATOR object_property method_or_not {}
;

method_or_not:
		TOPAR
				function_call_parameter_list TCPAR
			{ }
	|	/* empty */ {}
;

variable_without_objects:
		reference_variable {}
	|	simple_indirect_reference reference_variable {}
;

static_member:
		fully_qualified_class_name T_PAAMAYIM_NEKUDOTAYIM variable_without_objects {}
;


base_variable_with_function_calls:
		base_variable		{}
	|	function_call {}
;


base_variable:
		reference_variable {}
	|	simple_indirect_reference reference_variable {}
	|	static_member {}
;

reference_variable:
		reference_variable TOBRA dim_offset TCBRA	{}
	|	reference_variable TOBRACE expr TCBRACE		{}
	|	compound_variable			{}
;


compound_variable:
		T_VARIABLE			{}
	|	TDOLLAR TOBRACE expr TCBRACE	{}
;

dim_offset:
		/* empty */		{}
	|	expr			{}
;


object_property:
		object_dim_list {}
	|	variable_without_objects {}
;

object_dim_list:
		object_dim_list TOBRA dim_offset TCBRA	{}
	|	object_dim_list TOBRACE expr TCBRACE		{}
	|	variable_name {}
;

variable_name:
		T_STRING		{}
	|	TOBRACE expr TCBRACE	{}
;

simple_indirect_reference:
		TDOLLAR {}
	|	simple_indirect_reference TDOLLAR {}
;

assignment_list:
		assignment_list TCOMMA assignment_list_element { }
	|	assignment_list_element { }
;


assignment_list_element:
		variable								{}
	|	T_LIST TOPAR assignment_list TCPAR	{}
	|	/* empty */							{}
;


array_pair_list:
		/* empty */ {}
	|	non_empty_array_pair_list possible_comma	{}
;

non_empty_array_pair_list:
		non_empty_array_pair_list TCOMMA expr T_DOUBLE_ARROW expr	{}
	|	non_empty_array_pair_list TCOMMA expr			{}
	|	expr T_DOUBLE_ARROW expr	{}
	|	expr 				{}
	|	non_empty_array_pair_list TCOMMA expr T_DOUBLE_ARROW TAND w_variable {}
	|	non_empty_array_pair_list TCOMMA TAND w_variable {}
	|	expr T_DOUBLE_ARROW TAND w_variable	{}
	|	TAND w_variable 			{}
;

encaps_list:
		encaps_list encaps_var {}
	|	encaps_list T_ENCAPSED_AND_WHITESPACE	{}
	|	/* empty */			{}

;



encaps_var:
		T_VARIABLE {}
	|	T_VARIABLE TOBRA encaps_var_offset TCBRA	{}
	|	T_VARIABLE T_OBJECT_OPERATOR T_STRING {}
	|	T_DOLLAR_OPEN_CURLY_BRACES expr TCBRACE {}
	|	T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME TOBRA expr TCBRA TCBRACE {}
	|	T_CURLY_OPEN variable TCBRACE {}
;


encaps_var_offset:
		T_STRING		{}
	|	T_NUM_STRING	{}
	|	T_VARIABLE		{}
;


internal_functions_in_yacc:
		T_ISSET TOPAR isset_variables TCPAR {}
	|	T_EMPTY TOPAR variable TCPAR	{}
	|	T_INCLUDE expr 			{}
	|	T_INCLUDE_ONCE expr 	{}
	|	T_EVAL TOPAR expr TCPAR 	{}
	|	T_REQUIRE expr			{}
	|	T_REQUIRE_ONCE expr		{}
;

isset_variables:
		variable 				{}
	|	isset_variables TCOMMA variable {}
;

class_constant:
		fully_qualified_class_name T_PAAMAYIM_NEKUDOTAYIM T_STRING {}
;

%%

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * indent-tabs-mode: t
 * End:
 */

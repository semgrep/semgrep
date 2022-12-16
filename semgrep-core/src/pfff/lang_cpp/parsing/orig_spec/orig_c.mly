%{
(* src: ocamlyaccified from
 *  http://www.lysator.liu.se/c/ANSI-C-grammar-y.html
 *)
open Common
open AbstractSyntax
exception Parsing of string
%}

%token <string * AbstractSyntax.fullType> TString
%token <string> TIdent
%token <int * AbstractSyntax.intType> TInt
%token <float * AbstractSyntax.floatType> TFloat

/*(* conflicts *)*/
%token <string> TypedefIdent

%token TOPar TCPar TOBrace TCBrace TOCro TCCro
%token TDot TComma TPtrOp
%token TInc TDec
%token <AbstractSyntax.assignOp> TAssign
%token TEq
%token TWhy TDotDot TPtVirg TTilde TBang
%token TEllipsis

%token TOrLog TAndLog TOrIncl TOrExcl TAnd  TEqEq TNotEq TInf TSup TInfEq TSupEq  TShl TShr
       TPlus TMinus TMul TDiv TMod

%token Tchar Tshort Tint Tdouble Tfloat Tlong Tunsigned Tsigned Tvoid
       Tauto Tregister Textern Tstatic
       Tconst Tvolatile
       Tstruct Tenum Ttypedef Tunion
       Tbreak Telse Tswitch Tcase Tcontinue Tfor Tdo Tif  Twhile Treturn Tgoto Tdefault
       Tsizeof

%token EOF


%left TOrLog
%left TAndLog
%left TOrIncl
%left TOrExcl
%left TAnd
%left TEqEq TNotEq
%left TInf TSup TInfEq TSupEq
%left TShl TShr
%left TPlus TMinus
%left TMul TDiv TMod

%start main
%type <int list> main
%%

main:  translation_unit EOF     { [] }

/********************************************************************************/
/*
 expression
 statement
 declaration
 main
*/

/********************************************************************************/

expr: assign_expr             {  }
    | expr TComma assign_expr {  }

assign_expr: cond_expr                      {  }
           | unary_expr TAssign assign_expr {  }
           | unary_expr TEq     assign_expr {  }

cond_expr: arith_expr                             {}
	 | arith_expr TWhy expr TDotDot cond_expr {}

arith_expr: cast_expr {}
	  | arith_expr TMul    arith_expr {}
	  | arith_expr TDiv    arith_expr {}
	  | arith_expr TMod    arith_expr {}
	  | arith_expr TPlus   arith_expr {}
	  | arith_expr TMinus  arith_expr {}
	  | arith_expr TShl    arith_expr {}
	  | arith_expr TShr    arith_expr {}
	  | arith_expr TInf    arith_expr {}
	  | arith_expr TSup    arith_expr {}
	  | arith_expr TInfEq  arith_expr {}
	  | arith_expr TSupEq  arith_expr {}
	  | arith_expr TEqEq   arith_expr {}
	  | arith_expr TNotEq  arith_expr {}
	  | arith_expr TAnd    arith_expr {}
	  | arith_expr TOrExcl arith_expr {}
	  | arith_expr TOrIncl arith_expr {}
	  | arith_expr TAndLog arith_expr {}
	  | arith_expr TOrLog arith_expr {}

cast_expr: unary_expr {}
	 | TOPar type_name TCPar cast_expr {}

unary_expr: postfix_expr {}
	  | TInc unary_expr {}
	  | TDec unary_expr {}
	  | unary_op cast_expr {}
	  | Tsizeof unary_expr {}
	  | Tsizeof TOPar type_name TCPar {}

unary_op: TAnd  {}
	| TMul  {}
	| TPlus {}
	| TMinus{}
	| TTilde{}
	| TBang {}

postfix_expr: primary_expr  {}
	    | postfix_expr TOCro expr TCCro {}
	    | postfix_expr TOPar argument_expr_list TCPar  {}
	    | postfix_expr TOPar  TCPar  {}
	    | postfix_expr TDot TIdent {}
	    | postfix_expr TPtrOp TIdent {}
	    | postfix_expr TInc {}
	    | postfix_expr TDec {}

argument_expr_list: assign_expr { }
	          | argument_expr_list TComma assign_expr {}

primary_expr: TIdent  {}
            | TInt    {}
	    | TFloat  {}
	    | TString {}
	    | TOPar expr TCPar {}

const_expr: cond_expr {}
/********************************************************************************/

statement: labeled   {}
	 | compound  {}
	 | expr_statement {}
	 | selection {}
	 | iteration {}
	 | jump TPtVirg      {}

labeled: TIdent TDotDot statement {}
       | Tcase const_expr TDotDot statement {}
       | Tdefault TDotDot statement {}

compound: TOBrace                          TCBrace {}
        | TOBrace statement_list           TCBrace {}
        | TOBrace decl_list                TCBrace {}
        | TOBrace decl_list statement_list TCBrace {}

decl_list: decl {}
	 | decl decl_list {}

statement_list: statement {}
	      | statement statement_list {}

expr_statement: TPtVirg {}
	      |	expr TPtVirg {}

selection: Tif TOPar expr TCPar statement {}
	 | Tif TOPar expr TCPar statement Telse statement {}
	 | Tswitch TOPar expr TCPar statement {}

iteration: Twhile TOPar expr TCPar statement {}
	 | Tdo statement Twhile TOPar expr TCPar TPtVirg {}
	 | Tfor TOPar expr_statement expr_statement TCPar statement {}
	 | Tfor TOPar expr_statement expr_statement expr TCPar statement {}

jump: Tgoto TIdent {}
    | Tcontinue {}
    | Tbreak {}
    | Treturn {}
    | Treturn expr {}

/********************************************************************************/

/*------------------------------------------------------------------------------*/
decl: decl_spec TPtVirg {}
    | decl_spec init_declarator_list TPtVirg {}

/*------------------------------------------------------------------------------*/
decl_spec: storage_class_spec {}
         | storage_class_spec decl_spec {}
	 | type_spec {}
	 | type_spec decl_spec {}
	 | type_qualif {}
	 | type_qualif decl_spec {}

storage_class_spec: Tstatic {}
	          | Textern {}
		  | Tauto {}
		  | Tregister {}
		  | Ttypedef {}
type_spec: Tvoid {}
         | Tchar {}
	 | Tshort {}
	 | Tint {}
	 | Tlong {}
	 | Tfloat {}
	 | Tdouble {}
	 | Tsigned {}
	 | Tunsigned {}
	 | struct_or_union_spec {}
	 | enum_spec {}
/*TODO	 | TIdent {} */
         | TypedefIdent {}

type_qualif: Tconst {}
	   | Tvolatile {}

/*------------------------------------------------------------------------------*/
struct_or_union_spec: struct_or_union TIdent TOBrace struct_decl_list TCBrace {}
	            | struct_or_union        TOBrace struct_decl_list TCBrace {}
		    | struct_or_union TIdent {}

struct_or_union: Tstruct {}
	       | Tunion  {}

struct_decl_list: struct_decl {}
	        | struct_decl_list struct_decl  {}

struct_decl: spec_qualif_list struct_declarator_list TPtVirg {}

spec_qualif_list: type_spec {}
		| type_spec spec_qualif_list {}
		| type_qualif {}
		| type_qualif spec_qualif_list {}

struct_declarator_list: struct_declarator {}
		      | struct_declarator_list TComma struct_declarator {}
struct_declarator: declarator  {}
		 | TDotDot const_expr {}
		 | declarator TDotDot const_expr {}
/*------------------------------------------------------------------------------*/
enum_spec: Tenum        TOBrace enumerator_list TCBrace {}
         | Tenum TIdent TOBrace enumerator_list TCBrace {}
         | Tenum TIdent   {}

enumerator_list: enumerator {}
	       | enumerator_list TComma enumerator {}

enumerator: TIdent {}
          | TIdent TEq const_expr {}
/*------------------------------------------------------------------------------*/

init_declarator_list: init_declarator {}
	            | init_declarator_list TComma init_declarator {}

init_declarator: declarator {}
	       | declarator TEq initialize {}

/*------------------------------------------------------------------------------*/
declarator: pointer direct_declarator {}
          | direct_declarator {}

pointer: TMul  {}
       | TMul type_qualif_list {}
       | TMul pointer {}
       | TMul type_qualif_list pointer {}

direct_declarator: TIdent {}
                 | TOPar declarator TCPar {}
		 | direct_declarator TOCro const_expr TCCro {}
		 | direct_declarator TOCro            TCCro {}
		 | direct_declarator TOPar TCPar {}
		 | direct_declarator TOPar parameter_type_list TCPar {}
		 | direct_declarator TOPar identifier_list     TCPar {}

type_qualif_list: type_qualif {}
		| type_qualif_list type_qualif {}

parameter_type_list: parameter_list {}
		   | parameter_list TComma TEllipsis {}

parameter_list: parameter_decl {}
	      | parameter_list TComma parameter_decl {}

parameter_decl: decl_spec declarator {}
	      |	decl_spec abstract_declarator {}
	      |	decl_spec {}
identifier_list:  TIdent {}
	        | identifier_list TComma TIdent {}
/*------------------------------------------------------------------------------*/

type_name: spec_qualif_list {}
	 | spec_qualif_list abstract_declarator {}

abstract_declarator: pointer {}
	           |         direct_abstract_declarator {}
		   | pointer direct_abstract_declarator {}

direct_abstract_declarator: TOPar abstract_declarator TCPar {}
		          | TOCro            TCCro {}
		          | TOCro const_expr TCCro {}
			  | direct_abstract_declarator TOCro            TCCro {}
			  | direct_abstract_declarator TOCro const_expr TCCro {}
			  | TOPar TCPar {}
			  | TOPar parameter_type_list TCPar {}
			  | direct_abstract_declarator TOPar TCPar {}
			  | direct_abstract_declarator TOPar parameter_type_list TCPar {}

/*------------------------------------------------------------------------------*/
initialize: assign_expr {}
          | TOBrace initialize_list TCBrace {}
          | TOBrace initialize_list TComma TCBrace {}

initialize_list: initialize {}
	       | initialize_list TComma initialize {}

/********************************************************************************/

translation_unit: external_declaration {}
	        | translation_unit external_declaration {}

external_declaration: function_definition {}
		    | decl {}

function_definition: decl_spec declarator decl_list compound {}
		   | decl_spec declarator           compound {}
		   | declarator decl_list compound {}
		   | declarator compound {}

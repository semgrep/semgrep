# src: ruby/parse.y processed by script sample/exyacc.rb
#  and edited manually to add %token, and a few other directives.

%expect 0

%token <id>
  keyword_class        "`class'"
  keyword_module       "`module'"
  keyword_def          "`def'"
  keyword_undef        "`undef'"
  keyword_begin        "`begin'"
  keyword_rescue       "`rescue'"
  keyword_ensure       "`ensure'"
  keyword_end          "`end'"
  keyword_if           "`if'"
  keyword_unless       "`unless'"
  keyword_then         "`then'"
  keyword_elsif        "`elsif'"
  keyword_else         "`else'"
  keyword_case         "`case'"
  keyword_when         "`when'"
  keyword_while        "`while'"
  keyword_until        "`until'"
  keyword_for          "`for'"
  keyword_break        "`break'"
  keyword_next         "`next'"
  keyword_redo         "`redo'"
  keyword_retry        "`retry'"
  keyword_in           "`in'"
  keyword_do           "`do'"
  keyword_do_cond      "`do' for condition"
  keyword_do_block     "`do' for block"
  keyword_do_LAMBDA    "`do' for lambda"
  keyword_return       "`return'"
  keyword_yield        "`yield'"
  keyword_super        "`super'"
  keyword_self         "`self'"
  keyword_nil          "`nil'"
  keyword_true         "`true'"
  keyword_false        "`false'"
  keyword_and          "`and'"
  keyword_or           "`or'"
  keyword_not          "`not'"
  modifier_if          "`if' modifier"
  modifier_unless      "`unless' modifier"
  modifier_while       "`while' modifier"
  modifier_until       "`until' modifier"
  modifier_rescue      "`rescue' modifier"
  keyword_alias        "`alias'"
  keyword_defined      "`defined?'"
  keyword_BEGIN        "`BEGIN'"
  keyword_END          "`END'"
  keyword__LINE__      "`__LINE__'"
  keyword__FILE__      "`__FILE__'"
  keyword__ENCODING__  "`__ENCODING__'"

%token <id>   tIDENTIFIER    "local variable or method"
%token <id>   tFID           "method"
%token <id>   tGVAR          "global variable"
%token <id>   tIVAR          "instance variable"
%token <id>   tCONSTANT      "constant"
%token <id>   tCVAR          "class variable"
%token <id>   tLABEL
%token <node> tINTEGER       "integer literal"
%token <node> tFLOAT         "float literal"
%token <node> tRATIONAL      "rational literal"
%token <node> tIMAGINARY     "imaginary literal"
%token <node> tCHAR          "char literal"
%token <node> tNTH_REF       "numbered reference"
%token <node> tBACK_REF      "back reference"
%token <node> tSTRING_CONTENT "literal content"
%token <num>  tREGEXP_END

%token END_OF_INPUT 0   "end-of-input"
%token <id> '.'
/* escaped chars, should be ignored otherwise */
%token <id> '\\'    "backslash"
%token tSP      "escaped space"
%token <id> '\t'    "escaped horizontal tab"
%token <id> '\f'    "escaped form feed"
%token <id> '\r'    "escaped carriage return"
%token <id> '\13'   "escaped vertical tab"

%token tUPLUS       RUBY_TOKEN(UPLUS)  "unary+"
%token tUMINUS      RUBY_TOKEN(UMINUS) "unary-"
%token tPOW     RUBY_TOKEN(POW)    "**"
%token tCMP     RUBY_TOKEN(CMP)    "<=>"
%token tEQ      RUBY_TOKEN(EQ)     "=="
%token tEQQ     RUBY_TOKEN(EQQ)    "==="
%token tNEQ     RUBY_TOKEN(NEQ)    "!="
%token tGEQ     RUBY_TOKEN(GEQ)    ">="
%token tLEQ     RUBY_TOKEN(LEQ)    "<="
%token tANDOP       RUBY_TOKEN(ANDOP)  "&&"
%token tOROP        RUBY_TOKEN(OROP)   "||"
%token tMATCH       RUBY_TOKEN(MATCH)  "=~"
%token tNMATCH      RUBY_TOKEN(NMATCH) "!~"
%token tDOT2        RUBY_TOKEN(DOT2)   ".."
%token tDOT3        RUBY_TOKEN(DOT3)   "..."
%token tBDOT2       RUBY_TOKEN(BDOT2)   "(.."
%token tBDOT3       RUBY_TOKEN(BDOT3)   "(..."
%token tAREF        RUBY_TOKEN(AREF)   "[]"
%token tASET        RUBY_TOKEN(ASET)   "[]="
%token tLSHFT       RUBY_TOKEN(LSHFT)  "<<"
%token tRSHFT       RUBY_TOKEN(RSHFT)  ">>"
%token <id> tANDDOT RUBY_TOKEN(ANDDOT) "&."
%token <id> tCOLON2 RUBY_TOKEN(COLON2) "::"
%token tCOLON3      ":: at EXPR_BEG"
%token <id> tOP_ASGN    "operator-assignment" /* +=, -=  etc. */
%token tASSOC       "=>"
%token tLPAREN      "("
%token tLPAREN_ARG  "( arg"
%token tRPAREN      ")"
%token tLBRACK      "["
%token tLBRACE      "{"
%token tLBRACE_ARG  "{ arg"
%token tSTAR        "*"
%token tDSTAR       "**arg"
%token tAMPER       "&"
%token tLAMBDA      "->"
%token tSYMBEG      "symbol literal"
%token tSTRING_BEG  "string literal"
%token tXSTRING_BEG "backtick literal"
%token tREGEXP_BEG  "regexp literal"
%token tWORDS_BEG   "word list"
%token tQWORDS_BEG  "verbatim word list"
%token tSYMBOLS_BEG "symbol list"
%token tQSYMBOLS_BEG    "verbatim symbol list"
%token tSTRING_END  "terminator"
%token tSTRING_DEND "'}'"
%token tSTRING_DBEG tSTRING_DVAR tLAMBEG tLABEL_END

/*
 *  precedence table
 */

%nonassoc tLOWEST
%nonassoc tLBRACE_ARG

%nonassoc  modifier_if modifier_unless modifier_while modifier_until keyword_in
%left      keyword_or keyword_and
%right     keyword_not
%nonassoc  keyword_defined
%right     '=' tOP_ASGN
%left      modifier_rescue
%right     '?' ':'
%nonassoc  tDOT2 tDOT3 tBDOT2 tBDOT3
%left      tOROP
%left      tANDOP
%nonassoc  tCMP tEQ tEQQ tNEQ tMATCH tNMATCH
%left      '>' tGEQ '<' tLEQ
%left      '|' '^'
%left      '&'
%left      tLSHFT tRSHFT
%left      '+' '-'
%left      '*' '/' '%'
%right     tUMINUS_NUM tUMINUS
%right     tPOW
%right     '!' '~' tUPLUS

%token tLAST_TOKEN

%%

program: top_compstmt

top_compstmt: top_stmts opt_terms

top_stmts: none
        | top_stmt
        | top_stmts terms top_stmt
        | error top_stmt

top_stmt: stmt
        | keyword_BEGIN begin_block

begin_block: '{' top_compstmt '}'

bodystmt: compstmt opt_rescue k_else  compstmt opt_ensure
        | compstmt opt_rescue                  opt_ensure

compstmt: stmts opt_terms

stmts: none
     | stmt_or_begin
     | stmts terms stmt_or_begin
     | error stmt

stmt_or_begin: stmt
              | keyword_BEGIN  begin_block

stmt: keyword_alias fitem  fitem
    | keyword_alias tGVAR tGVAR
    | keyword_alias tGVAR tBACK_REF
    | keyword_alias tGVAR tNTH_REF
    | keyword_undef undef_list
    | stmt modifier_if     expr_value
    | stmt modifier_unless expr_value
    | stmt modifier_while  expr_value
    | stmt modifier_until  expr_value
    | stmt modifier_rescue stmt
    | keyword_END '{' compstmt '}'
    | command_asgn
    | mlhs '=' command_call
    | lhs '=' mrhs
    | mlhs '=' mrhs_arg modifier_rescue stmt
    | mlhs '=' mrhs_arg
    | expr

command_asgn: lhs '=' command_rhs
        | var_lhs tOP_ASGN command_rhs
        | primary_value '[' opt_call_args rbracket tOP_ASGN command_rhs
        | primary_value call_op tIDENTIFIER tOP_ASGN command_rhs
        | primary_value call_op tCONSTANT tOP_ASGN command_rhs
        | primary_value tCOLON2 tCONSTANT tOP_ASGN command_rhs
        | primary_value tCOLON2 tIDENTIFIER tOP_ASGN command_rhs
        | backref tOP_ASGN command_rhs

command_rhs: command_call   %prec tOP_ASGN
        | command_call modifier_rescue stmt
        | command_asgn

expr: command_call
    | expr keyword_and expr
    | expr keyword_or  expr
    | keyword_not opt_nl expr
    | '!' command_call
    | arg keyword_in p_expr
    | arg %prec tLBRACE_ARG

expr_value: expr

expr_value_do: expr_value do

command_call: command
        | block_command

block_command: block_call
        | block_call call_op2 operation2 command_args

cmd_brace_block: tLBRACE_ARG brace_body '}'

fcall: operation

command: fcall command_args       %prec tLOWEST
       | fcall command_args cmd_brace_block
       | primary_value call_op operation2 command_args %prec tLOWEST
       | primary_value call_op operation2 command_args cmd_brace_block
       | primary_value tCOLON2 operation2 command_args %prec tLOWEST
       | primary_value tCOLON2 operation2 command_args cmd_brace_block
       | keyword_super command_args
       | keyword_yield command_args
       | k_return call_args
       | keyword_break call_args
       | keyword_next call_args

mlhs: mlhs_basic
    | tLPAREN mlhs_inner rparen

mlhs_inner: mlhs_basic
        | tLPAREN mlhs_inner rparen

mlhs_basic: mlhs_head
        | mlhs_head mlhs_item
        | mlhs_head tSTAR mlhs_node
        | mlhs_head tSTAR mlhs_node ',' mlhs_post
        | mlhs_head tSTAR
        | mlhs_head tSTAR ',' mlhs_post
        | tSTAR mlhs_node
        | tSTAR mlhs_node ',' mlhs_post
        | tSTAR
        | tSTAR ',' mlhs_post

mlhs_item: mlhs_node
        | tLPAREN mlhs_inner rparen

mlhs_head: mlhs_item ','
        | mlhs_head mlhs_item ','

mlhs_post: mlhs_item
        | mlhs_post ',' mlhs_item

mlhs_node: user_variable
        | keyword_variable
        | primary_value '[' opt_call_args rbracket
        | primary_value call_op tIDENTIFIER
        | primary_value tCOLON2 tIDENTIFIER
        | primary_value call_op tCONSTANT
        | primary_value tCOLON2 tCONSTANT
        | tCOLON3 tCONSTANT
        | backref

lhs: user_variable
   | keyword_variable
   | primary_value '[' opt_call_args rbracket
   | primary_value call_op tIDENTIFIER
   | primary_value tCOLON2 tIDENTIFIER
   | primary_value call_op tCONSTANT
   | primary_value tCOLON2 tCONSTANT
   | tCOLON3 tCONSTANT
   | backref

cname: tIDENTIFIER
     | tCONSTANT

cpath: tCOLON3 cname
        | cname
        | primary_value tCOLON2 cname

fname: tIDENTIFIER
     | tCONSTANT
     | tFID
     | op
     | reswords

fitem: fname
     | symbol

undef_list: fitem
        | undef_list ','  fitem

op: '|'       | '^'       | '&'
  | tCMP      | tEQ       | tEQQ
  | tMATCH    | tNMATCH
  | '>'       | tGEQ
  | '<'       | tLEQ
  | tNEQ
  | tLSHFT    | tRSHFT
  | '+'       | '-'       | '*'       | tSTAR     | '/'       | '%'
  | tPOW
  | tDSTAR
  | '!'       | '~'
  | tUPLUS    | tUMINUS
  | tAREF     | tASET
  | '`'

reswords: keyword__LINE__ | keyword__FILE__ | keyword__ENCODING__
        | keyword_BEGIN | keyword_END
        | keyword_alias | keyword_and | keyword_begin
        | keyword_break | keyword_case | keyword_class | keyword_def
        | keyword_defined | keyword_do | keyword_else | keyword_elsif
        | keyword_end | keyword_ensure | keyword_false
        | keyword_for | keyword_in | keyword_module | keyword_next
        | keyword_nil | keyword_not | keyword_or | keyword_redo
        | keyword_rescue | keyword_retry | keyword_return | keyword_self
        | keyword_super | keyword_then | keyword_true | keyword_undef
        | keyword_when | keyword_yield | keyword_if | keyword_unless
        | keyword_while | keyword_until

arg: lhs '=' arg_rhs
   | var_lhs tOP_ASGN arg_rhs
   | primary_value '[' opt_call_args rbracket tOP_ASGN arg_rhs
   | primary_value call_op tIDENTIFIER tOP_ASGN arg_rhs
   | primary_value call_op tCONSTANT tOP_ASGN arg_rhs
   | primary_value tCOLON2 tIDENTIFIER tOP_ASGN arg_rhs
   | primary_value tCOLON2 tCONSTANT tOP_ASGN arg_rhs
   | tCOLON3 tCONSTANT tOP_ASGN arg_rhs
   | backref tOP_ASGN arg_rhs
   | arg tDOT2 arg
   | arg tDOT3 arg
   | arg tDOT2
   | arg tDOT3
   | tBDOT2 arg
   | tBDOT3 arg
   | arg '+' arg
   | arg '-' arg
   | arg '*' arg
   | arg '/' arg
   | arg '%' arg
   | arg tPOW arg
   | tUMINUS_NUM simple_numeric tPOW arg
   | tUPLUS arg
   | tUMINUS arg
   | arg '|' arg
   | arg '^' arg
   | arg '&' arg
   | arg tCMP arg
   | rel_expr   %prec tCMP
   | arg tEQ arg
   | arg tEQQ arg
   | arg tNEQ arg
   | arg tMATCH arg
   | arg tNMATCH arg
   | '!' arg
   | '~' arg
   | arg tLSHFT arg
   | arg tRSHFT arg
   | arg tANDOP arg
   | arg tOROP arg
   | keyword_defined opt_nl  arg
   | arg '?' arg opt_nl ':' arg
   | primary

relop: '>'
     | '<'
     | tGEQ
     | tLEQ

rel_expr: arg relop arg   %prec '>'
        | rel_expr relop arg   %prec '>'

arg_value: arg

aref_args: none
        | args trailer
        | args ',' assocs trailer
        | assocs trailer

arg_rhs: arg   %prec tOP_ASGN
       | arg modifier_rescue arg

paren_args: '(' opt_call_args rparen
        | '(' args_forward rparen

opt_paren_args: none
        | paren_args

opt_call_args: none
        | call_args
        | args ','
        | args ',' assocs ','
        | assocs ','

call_args: command
        | args opt_block_arg
        | assocs opt_block_arg
        | args ',' assocs opt_block_arg
        | block_arg

command_args: call_args

block_arg: tAMPER arg_value

opt_block_arg: ',' block_arg
        | none

args: arg_value
    | tSTAR arg_value
    | args ',' arg_value
    | args ',' tSTAR arg_value

mrhs_arg: mrhs
        | arg_value

mrhs: args ',' arg_value
        | args ',' tSTAR arg_value
        | tSTAR arg_value

primary: literal
       | strings
       | xstring
       | regexp
       | words
       | qwords
       | symbols
       | qsymbols
       | var_ref
       | backref
       | tFID
       | k_begin  bodystmt k_end
       | tLPAREN_ARG  rparen
       | tLPAREN_ARG stmt  rparen
       | tLPAREN compstmt ')'
       | primary_value tCOLON2 tCONSTANT
       | tCOLON3 tCONSTANT
       | tLBRACK aref_args ']'
       | tLBRACE assoc_list '}'
       | k_return
       | keyword_yield '(' call_args rparen
       | keyword_yield '(' rparen
       | keyword_yield
       | keyword_defined opt_nl '('  expr rparen
       | keyword_not '(' expr rparen
       | keyword_not '(' rparen
       | fcall brace_block
       | method_call
       | method_call brace_block
       | tLAMBDA lambda
       | k_if expr_value then
         compstmt
         if_tail
         k_end
       | k_unless expr_value then
         compstmt
         opt_else
         k_end
       | k_while expr_value_do
         compstmt
         k_end
       | k_until expr_value_do
         compstmt
         k_end
       | k_case expr_value opt_terms
         case_body
         k_end
       | k_case opt_terms
         case_body
         k_end
       | k_case expr_value opt_terms
         p_case_body
         k_end
       | k_for for_var keyword_in expr_value_do
         compstmt
         k_end
       | k_class cpath superclass
         bodystmt
         k_end
       | k_class tLSHFT expr
         term
         bodystmt
         k_end
       | k_module cpath
         bodystmt
         k_end
       | k_def fname
         f_arglist
         bodystmt
         k_end
       | k_def singleton dot_or_colon  fname
         f_arglist
         bodystmt
         k_end
       | keyword_break
       | keyword_next
       | keyword_redo
       | keyword_retry

primary_value: primary

k_begin: keyword_begin
k_if: keyword_if
k_unless: keyword_unless
k_while: keyword_while
k_until: keyword_until
k_case: keyword_case
k_for: keyword_for
k_class: keyword_class
k_module: keyword_module
k_def: keyword_def
k_do: keyword_do
k_do_block: keyword_do_block
k_rescue: keyword_rescue
k_ensure: keyword_ensure
k_when: keyword_when
k_else: keyword_else
k_elsif: keyword_elsif
k_end: keyword_end
k_return: keyword_return

then: term
    | keyword_then
    | term keyword_then

do: term
  | keyword_do_cond

if_tail: opt_else
        | k_elsif expr_value then
          compstmt
          if_tail

opt_else: none
        | k_else compstmt

for_var: lhs
       | mlhs

f_marg: f_norm_arg
      | tLPAREN f_margs rparen

f_marg_list: f_marg
        | f_marg_list ',' f_marg

f_margs: f_marg_list
       | f_marg_list ',' f_rest_marg
       | f_marg_list ',' f_rest_marg ',' f_marg_list
       | f_rest_marg
       | f_rest_marg ',' f_marg_list

f_rest_marg: tSTAR f_norm_arg
        | tSTAR

block_args_tail: f_block_kwarg ',' f_kwrest opt_f_block_arg
        | f_block_kwarg opt_f_block_arg
        | f_kwrest opt_f_block_arg
        | f_no_kwarg opt_f_block_arg
        | f_block_arg

opt_block_args_tail: ',' block_args_tail
        |

block_param: f_arg ',' f_block_optarg ',' f_rest_arg opt_block_args_tail
        | f_arg ',' f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail
        | f_arg ',' f_block_optarg opt_block_args_tail
        | f_arg ',' f_block_optarg ',' f_arg opt_block_args_tail
        | f_arg ',' f_rest_arg opt_block_args_tail
        | f_arg ','
        | f_arg ',' f_rest_arg ',' f_arg opt_block_args_tail
        | f_arg opt_block_args_tail
        | f_block_optarg ',' f_rest_arg opt_block_args_tail
        | f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail
        | f_block_optarg opt_block_args_tail
        | f_block_optarg ',' f_arg opt_block_args_tail
        | f_rest_arg opt_block_args_tail
        | f_rest_arg ',' f_arg opt_block_args_tail
        | block_args_tail

opt_block_param: none
        | block_param_def

block_param_def: '|' opt_bv_decl '|'
               | '|' block_param opt_bv_decl '|'

opt_bv_decl: opt_nl
           | opt_nl ';' bv_decls opt_nl

bv_decls: bvar
        | bv_decls ',' bvar

bvar: tIDENTIFIER
    | f_bad_arg

lambda: f_larglist  lambda_body

f_larglist: '(' f_args opt_bv_decl ')'
          | f_args

lambda_body: tLAMBEG compstmt '}'
          | keyword_do_LAMBDA bodystmt k_end

do_block: k_do_block do_body k_end

block_call: command do_block
        | block_call call_op2 operation2 opt_paren_args
        | block_call call_op2 operation2 opt_paren_args brace_block
        | block_call call_op2 operation2 command_args do_block

method_call: fcall paren_args
        | primary_value call_op operation2 opt_paren_args
        | primary_value tCOLON2 operation2 paren_args
        | primary_value tCOLON2 operation3
        | primary_value call_op paren_args
        | primary_value tCOLON2 paren_args
        | keyword_super paren_args
        | keyword_super
        | primary_value '[' opt_call_args rbracket

brace_block: '{' brace_body '}'
           | k_do do_body k_end

brace_body: opt_block_param compstmt

do_body: opt_block_param bodystmt

case_args: arg_value
        | tSTAR arg_value
        | case_args ',' arg_value
        | case_args ',' tSTAR arg_value

case_body: k_when case_args then
          compstmt
          cases

cases: opt_else
     | case_body

p_case_body: keyword_in
          p_top_expr then
          compstmt
          p_cases

p_cases: opt_else
       | p_case_body

p_top_expr: p_top_expr_body
        | p_top_expr_body modifier_if expr_value
        | p_top_expr_body modifier_unless expr_value

p_top_expr_body: p_expr
        | p_expr ','
        | p_expr ',' p_args
        | p_args_tail
        | p_kwargs

p_expr: p_as

p_as: p_expr tASSOC p_variable
    | p_alt

p_alt: p_alt '|' p_expr_basic
     | p_expr_basic

p_lparen: '(' ;
p_lbracket: '[' ;

p_expr_basic: p_value
        | p_const p_lparen p_args rparen
        | p_const p_lparen p_kwargs rparen
        | p_const '(' rparen
        | p_const p_lbracket p_args rbracket
        | p_const p_lbracket p_kwargs rbracket
        | p_const '[' rbracket
        | tLBRACK  p_args rbracket
        | tLBRACK rbracket
        | tLBRACE  p_kwargs '}'
        | tLBRACE '}'
        | tLPAREN  p_expr rparen

p_args: p_expr
      | p_args_head
      | p_args_head p_arg
      | p_args_head tSTAR tIDENTIFIER
      | p_args_head tSTAR tIDENTIFIER ',' p_args_post
      | p_args_head tSTAR
      | p_args_head tSTAR ',' p_args_post
      | p_args_tail

p_args_head: p_arg ','
           | p_args_head p_arg ','

p_args_tail: tSTAR tIDENTIFIER
        | tSTAR tIDENTIFIER ',' p_args_post
        | tSTAR
        | tSTAR ',' p_args_post

p_args_post: p_arg
        | p_args_post ',' p_arg

p_arg: p_expr

p_kwargs: p_kwarg ',' p_kwrest
        | p_kwarg
        | p_kwrest
        | p_kwarg ',' p_kwnorest
        | p_kwnorest

p_kwarg: p_kw
       | p_kwarg ',' p_kw

p_kw: p_kw_label p_expr
    | p_kw_label

p_kw_label: tLABEL
          | tSTRING_BEG string_contents tLABEL_END

p_kwrest: kwrest_mark tIDENTIFIER
        | kwrest_mark

p_kwnorest: kwrest_mark keyword_nil

p_value: p_primitive
       | p_primitive tDOT2 p_primitive
       | p_primitive tDOT3 p_primitive
       | p_primitive tDOT2
       | p_primitive tDOT3
       | p_variable
       | p_var_ref
       | p_const
       | tBDOT2 p_primitive
       | tBDOT3 p_primitive

p_primitive: literal
        | strings
        | xstring
        | regexp
        | words
        | qwords
        | symbols
        | qsymbols
        | keyword_variable
        | tLAMBDA
          lambda

p_variable: tIDENTIFIER

p_var_ref: '^' tIDENTIFIER

p_const: tCOLON3 cname
       | p_const tCOLON2 cname
       | tCONSTANT

opt_rescue: k_rescue exc_list exc_var then
          compstmt
          opt_rescue
        | none

exc_list: arg_value
        | mrhs
        | none

exc_var: tASSOC lhs
        | none

opt_ensure: k_ensure compstmt
        | none

literal: numeric
       | symbol

strings: string

string: tCHAR
      | string1
      | string string1

string1: tSTRING_BEG string_contents tSTRING_END

xstring: tXSTRING_BEG xstring_contents tSTRING_END

regexp: tREGEXP_BEG regexp_contents tREGEXP_END

words: tWORDS_BEG ' ' word_list tSTRING_END

word_list: none
        | word_list word ' '

word: string_content
    | word string_content

symbols: tSYMBOLS_BEG ' ' symbol_list tSTRING_END

symbol_list: none
        | symbol_list word ' '

qwords: tQWORDS_BEG ' ' qword_list tSTRING_END

qsymbols: tQSYMBOLS_BEG ' ' qsym_list tSTRING_END

qword_list: none
        | qword_list tSTRING_CONTENT ' '

qsym_list: none
        | qsym_list tSTRING_CONTENT ' '

string_contents: none
        | string_contents string_content

xstring_contents: none
        | xstring_contents string_content

regexp_contents: none
        | regexp_contents string_content

string_content: tSTRING_CONTENT
        | tSTRING_DVAR  string_dvar
        | tSTRING_DBEG  compstmt tSTRING_DEND

string_dvar: tGVAR
        | tIVAR
        | tCVAR
        | backref

symbol: ssym
      | dsym

ssym: tSYMBEG sym

sym: fname
   | tIVAR
   | tGVAR
   | tCVAR

dsym: tSYMBEG string_contents tSTRING_END

numeric: simple_numeric
        | tUMINUS_NUM simple_numeric   %prec tLOWEST

simple_numeric: tINTEGER
        | tFLOAT
        | tRATIONAL
        | tIMAGINARY

user_variable: tIDENTIFIER
        | tIVAR
        | tGVAR
        | tCONSTANT
        | tCVAR

keyword_variable: keyword_nil
        | keyword_self
        | keyword_true
        | keyword_false
        | keyword__FILE__
        | keyword__LINE__
        | keyword__ENCODING__

var_ref: user_variable
       | keyword_variable

var_lhs: user_variable
       | keyword_variable

backref: tNTH_REF
       | tBACK_REF

superclass: '<' expr_value term
        | none

f_arglist: '(' f_args rparen
        | '(' args_forward rparen
        | f_args term

args_tail: f_kwarg ',' f_kwrest opt_f_block_arg
        | f_kwarg opt_f_block_arg
        | f_kwrest opt_f_block_arg
        | f_no_kwarg opt_f_block_arg
        | f_block_arg

opt_args_tail: ',' args_tail
        | none

f_args: f_arg ',' f_optarg ',' f_rest_arg opt_args_tail
      | f_arg ',' f_optarg ',' f_rest_arg ',' f_arg opt_args_tail
      | f_arg ',' f_optarg opt_args_tail
      | f_arg ',' f_optarg ',' f_arg opt_args_tail
      | f_arg ',' f_rest_arg opt_args_tail
      | f_arg ',' f_rest_arg ',' f_arg opt_args_tail
      | f_arg opt_args_tail
      | f_optarg ',' f_rest_arg opt_args_tail
      | f_optarg ',' f_rest_arg ',' f_arg opt_args_tail
      | f_optarg opt_args_tail
      | f_optarg ',' f_arg opt_args_tail
      | f_rest_arg opt_args_tail
      | f_rest_arg ',' f_arg opt_args_tail
      | args_tail
      |

args_forward: tBDOT3

f_bad_arg: tCONSTANT
        | tIVAR
        | tGVAR
        | tCVAR

f_norm_arg: f_bad_arg
        | tIDENTIFIER

f_arg_asgn: f_norm_arg

f_arg_item: f_arg_asgn
        | tLPAREN f_margs rparen

f_arg: f_arg_item
        | f_arg ',' f_arg_item

f_label: tLABEL

f_kw: f_label arg_value
    | f_label

f_block_kw: f_label primary_value
        | f_label

f_block_kwarg: f_block_kw
        | f_block_kwarg ',' f_block_kw

f_kwarg: f_kw
       | f_kwarg ',' f_kw

kwrest_mark: tPOW
        | tDSTAR

f_no_kwarg: kwrest_mark keyword_nil

f_kwrest: kwrest_mark tIDENTIFIER
        | kwrest_mark

f_opt: f_arg_asgn '=' arg_value

f_block_opt: f_arg_asgn '=' primary_value

f_block_optarg: f_block_opt
        | f_block_optarg ',' f_block_opt

f_optarg: f_opt
        | f_optarg ',' f_opt

restarg_mark: '*'
        | tSTAR

f_rest_arg: restarg_mark tIDENTIFIER
        | restarg_mark

blkarg_mark: '&'
        | tAMPER

f_block_arg: blkarg_mark tIDENTIFIER

opt_f_block_arg: ',' f_block_arg
        | none

singleton: var_ref
        | '('  expr rparen

assoc_list: none
        | assocs trailer

assocs: assoc
        | assocs ',' assoc

assoc: arg_value tASSOC arg_value
        | tLABEL arg_value
        | tSTRING_BEG string_contents tLABEL_END arg_value
        | tDSTAR arg_value

operation: tIDENTIFIER
        | tCONSTANT
        | tFID

operation2: tIDENTIFIER
        | tCONSTANT
        | tFID
        | op

operation3: tIDENTIFIER
        | tFID
        | op

dot_or_colon: '.'
        | tCOLON2

call_op: '.'
        | tANDDOT

call_op2: call_op
        | tCOLON2

opt_terms:
        | terms

opt_nl: none
        | '\n'

rparen: opt_nl ')'

rbracket: opt_nl ']'

trailer: none
        | '\n'
        | ','

term: ';'
    | '\n'

terms: term
     | terms ';'

none:

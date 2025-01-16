%{
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2011-2015 Tomohiro Matsuyama
 * Copyright (C) 2019-2020 r2c
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

(* This file contains a grammar for Python 3
 * (which is mostly a superset of Python 2).
 *
 * original src:
 *  https://github.com/m2ym/ocaml-pythonlib/blob/master/src/python2_parser.mly
 * reference:
 *  - https://docs.python.org/3/reference/grammar.html
 *  - http://docs.python.org/release/2.5.2/ref/grammar.txt
 * old src:
 *  - http://inst.eecs.berkeley.edu/~cs164/sp10/python-grammar.html
 *)
open Common
open AST_python

(* intermediate helper type *)
type single_or_tuple =
  | Single of expr
  | Tup of expr list

let cons e = function
  | Single e' -> Tup (e::[e'])
  | Tup l -> Tup (e::l)

let tuple_expr = function
  | Single e -> e
  (* the fake could be set later in rewrap_paren below *)
  | Tup l -> Tuple (CompList (Tok.unsafe_fake_bracket l), Load)

let to_list = function
  | Single e -> [e]
  | Tup l -> l

(* this is important for semgrep, to get the right range (and for autofix) *)
let rewrap_paren l e r =
  match e with
  | Tuple (CompList (_, xs, _), Load) ->
      Tuple (CompList (l, xs, r), Load)
  | Tuple (CompForIf (_, x, _), Load) ->
      Tuple (CompForIf (l, x, r), Load)
  | _ -> ParenExpr (l, e, r)

(* TODO: TypedExpr? ExprStar? then can appear as lvalue
 * CompForIf though is not an lvalue.
*)
let rec set_expr_ctx ctx = function
  | Name (id, _) ->
      Name (id, ctx)
  | Attribute (value, t, attr, _) ->
      Attribute (value, t, attr, ctx)
  | Subscript (value, slice, _) ->
      Subscript (value, slice, ctx)

  | List (CompList (t1, elts, t2), _) ->
      List (CompList ((t1, List.map (set_expr_ctx ctx) elts, t2)), ctx)
  | Tuple (CompList (t1, elts, t2), _) ->
      Tuple (CompList ((t1, List.map (set_expr_ctx ctx) elts, t2)), ctx)

  | e -> e

let expr_store = set_expr_ctx Store
and expr_del = set_expr_ctx Del

let tuple_expr_store l =
  let e = tuple_expr l in
    match AST_python.context_of_expr e with
    | Some Param -> e
    | _ -> expr_store e

let mk_str ii =
  let s = Tok.content_of_tok ii in
  Literal( Str (s, ii))

%}

(*************************************************************************)
(* Tokens *)
(*************************************************************************)
%token <AST_python.tok> TUnknown  (* unrecognized token *)
%token <AST_python.tok> EOF

(*-----------------------------------------*)
(* The space/comment tokens *)
(*-----------------------------------------*)
(* coupling: Token_helpers.is_comment *)
%token <AST_python.tok> TCommentSpace TComment
(* see the extra token below NEWLINE instead of TCommentNewline *)

(*-----------------------------------------*)
(* The normal tokens *)
(*-----------------------------------------*)

(* tokens with "values" *)
%token <string * AST_python.tok> NAME
%token <Parsed_int.t> INT LONGINT
%token <float option  * AST_python.tok> FLOAT
%token <string * AST_python.tok> IMAG
%token <string * string * AST_python.tok> STR

(*-----------------------------------------*)
(* Keyword tokens *)
(*-----------------------------------------*)
%token <AST_python.tok>
 IF ELSE ELIF
 WHILE FOR
 RETURN CONTINUE BREAK PASS
 DEF LAMBDA CLASS GLOBAL
 TRY FINALLY EXCEPT RAISE
 AND NOT OR
 IMPORT FROM AS
 DEL IN IS WITH YIELD
 ASSERT
 NONE TRUE FALSE
 ASYNC AWAIT
 NONLOCAL
 (* python2: *)
 PRINT EXEC

(*-----------------------------------------*)
(* Punctuation tokens *)
(*-----------------------------------------*)

(* syntax *)
%token <AST_python.tok>
 LPAREN "("     RPAREN ")"
 LBRACK "["     RBRACK "]"
 LBRACE "{"     RBRACE "}"
 COLON  ":"
 SEMICOL ";"
 DOT     "."
 COMMA   ","
 BACKQUOTE "`"
 AT        "@"
 ELLIPSES  "..."
 LDots "<..." RDots "...>"

(* operators *)
%token <AST_python.tok>
  ADD            (* + *)  SUB            (* - *)
  MULT "*"       (* * *)  DIV "/"        (* / *)
  MOD            (* % *)
  POW  "**"      (* ** *)  FDIV           (* // *)
  BITOR          (* | *)  BITAND         (* & *)  BITXOR         (* ^ *)
  BITNOT         (* ~ *)  LSHIFT         (* << *)  RSHIFT         (* >> *)

%token <AST_python.tok>
  EQ "="             (* = *)
  COLONEQ ":="   (* := *)
  ADDEQ          (* += *) SUBEQ          (* -= *)
  MULTEQ         (* *= *) DIVEQ          (* /= *)
  MODEQ          (* %= *)
  POWEQ          (* **= *) FDIVEQ         (* //= *)
  ANDEQ          (* &= *) OREQ           (* |= *) XOREQ          (* ^= *)
  LSHEQ          (* <<= *) RSHEQ          (* >>= *)

  EQUAL          (* == *) NOTEQ          (* !=, <> *)
  LT             (* < *) GT             (* > *)
  LEQ            (* <= *) GEQ            (* >= *)

(*-----------------------------------------*)
(* Extra tokens: *)
(*-----------------------------------------*)
(* fstrings *)
%token <AST_python.tok> FSTRING_START FSTRING_END
%token <AST_python.tok> FSTRING_LBRACE
%token <string * AST_python.tok> FSTRING_STRING
%token <AST_python.tok> BANG

(* layout *)
%token <AST_python.tok> INDENT DEDENT
%token <AST_python.tok> NEWLINE

(*************************************************************************)
(* Rules type declaration *)
(*************************************************************************)
%start <AST_python.program> main
%start <AST_python.any> sgrep_spatch_pattern
%start <AST_python.lsif_type> type_for_lsif
%%

(*************************************************************************)
(* Macros *)
(*************************************************************************)

list_sep(X,Sep):
 | X                      { [$1] }
 | list_sep(X,Sep) Sep X  { $1 @ [$3] }

(* list separated by Sep and possibly terminated by trailing Sep.
 * This has to be recursive on the right, otherwise s/r conflict.
 *)
list_sep_term(X,Sep):
 | X                       { [$1] }
 | X Sep                   { [$1] }
 | X Sep list_sep_term(X,Sep)  { $1::$3 }

list_comma(X): list_sep_term(X, ",") { $1 }

(* In most cases  it should be used with tuple(expr_or_star_expr) *)
tuple(X):
 | X               { Single $1 }
 | X ","           { Tup [$1] }
 | X "," tuple(X)  { cons $1 $3 }

(*************************************************************************)
(* Toplevel *)
(*************************************************************************)

main:
 | file_input EOF { $1 }
 (* Handles trailing indentation.
  * Note that I couldn't figure out why the lexer spits this out,
  * but I didn't want to mess with its state machine too much,
  * so I just match against the relavent output here.
  *)
 | file_input INDENT NEWLINE DEDENT NEWLINE EOF { $1 }

file_input: nl_or_stmt* { List.flatten $1 }

nl_or_stmt:
 | NEWLINE { [] }
 | stmt    { $1 }

sgrep_spatch_pattern:
 (* for decorator patterns @$NAME(...) *)
 | decorator EOF { Flag_parsing.sgrep_guard (Decorator $1) }
 | stmt NEWLINE? EOF   {
   match $1 with
   | [ExprStmt x] -> Expr x
   | [x] -> Stmt x
   | xs -> Stmts xs
 }
 | stmt stmt+ NEWLINE? EOF { Stmts ($1 @ (List.flatten $2)) }

(*************************************************************************)
(* Import *)
(*************************************************************************)
(* In Python, imports can actually appear not just at the toplevel *)
import_stmt:
  | import_name { $1 }
  | import_from { $1 }


import_name: IMPORT list_sep(dotted_as_name, ",")
  { $2 |> List.map (fun (v1, v2) -> let dots = None in
         ImportAs ($1, (v1, dots), v2))   }

dotted_as_name:
  | dotted_name         { $1, None }
  | dotted_name AS NAME { $1, Some $3 }

dotted_name:
  | NAME { [$1] }
  | NAME "." dotted_name { $1::$3 }


import_from:
  | FROM name_and_level IMPORT "*"
      { [ImportAll ($1, $2, $4)] }
  | FROM name_and_level IMPORT "(" list_comma(import_as_name) ")"
      { [ImportFrom ($1, $2, $5)] }
  | FROM name_and_level IMPORT list_comma(import_as_name)
      { [ImportFrom ($1, $2, $4)] }

name_and_level:
  | dot_level dotted_name {
    match $1 with
    | [] -> $2, None
    | dl -> $2, Some dl
  }
  | "." dot_level         { [("",$1(*TODO*))], Some ($1 :: $2) }
  | "..." dot_level         { [("",$1(*TODO*))], Some ($1:: $2) }

dot_level:
  | (*empty *) { [] }
  | "." dot_level  { $1::$2 }
  | "..." dot_level { $1::$2 }

import_as_name:
  | NAME         { $1, None }
  | NAME AS NAME { $1, Some $3 }

(*************************************************************************)
(* Variable definition *)
(*************************************************************************)

expr_stmt:
  | tuple(test_or_star_expr)
      { ExprStmt (tuple_expr $1) }
  (* typing-ext: *)
  | tuple(test_or_star_expr) ":" test
      { Cast (tuple_expr $1, $2, $3) }
  | tuple(test_or_star_expr) ":" test "=" test
      { Assign ([let expr = tuple_expr_store $1 in (expr, Some ($2, $3))], $4, $5) }

  | tuple(test_or_star_expr) augassign yield_expr
      { AugAssign (tuple_expr_store $1, $2, $3) }
  | tuple(test_or_star_expr) augassign tuple(test)
      { AugAssign (tuple_expr_store $1, $2, tuple_expr $3) }
  | tuple(test_or_star_expr) "=" expr_stmt_rhs_list
      { Assign ((tuple_expr_store $1, None)::(fst $3), $2, snd $3) }

test_or_star_expr:
  | test      { $1 }
  | star_expr { $1 }

expr_or_star_expr:
  | expr      { $1 }
  | star_expr { $1 }

exprlist: tuple(expr_or_star_expr) { $1 }

expr_stmt_rhs_list:
  | expr_stmt_rhs                         { [], $1 }
  | expr_stmt_rhs "=" expr_stmt_rhs_list  { (expr_store $1, None)::(fst $3), snd $3 }

expr_stmt_rhs:
  | yield_expr               { $1 }
  | tuple(test_or_star_expr) { tuple_expr $1 }

augassign:
  | ADDEQ   { Add, $1 }
  | SUBEQ   { Sub, $1 }
  | MULTEQ  { Mult, $1 }
  | DIVEQ   { Div, $1 }
  | POWEQ   { Pow, $1 }
  | MODEQ   { Mod, $1 }
  | LSHEQ   { LShift, $1 }
  | RSHEQ   { RShift, $1 }
  | OREQ    { BitOr, $1 }
  | XOREQ   { BitXor, $1 }
  | ANDEQ   { BitAnd, $1 }
  | FDIVEQ  { FloorDiv, $1 }


namedexpr_test:
  | test { $1 }
  | test ":=" test { NamedExpr ($1, $2, $3) }

namedexpr_or_star_expr:
  | namedexpr_test { $1 }
  | star_expr      { $1 }

(*************************************************************************)
(* Function definition *)
(*************************************************************************)
(* this rule is referenced in compound_stmt shown later *)
funcdef: DEF NAME parameters return_type? ":" suite
    { FunctionDef ($1, $2, $3, $4, $6, []) }

async_funcdef: ASYNC DEF NAME parameters return_type? ":" suite
    { FunctionDef ($2, $3, $4, $5, $7, [] (* TODO $1 *)) }

(* typing-ext: *)
return_type:
  | SUB GT test     { $3 }

(*----------------------------*)
(* parameters *)
(*----------------------------*)

parameters: "(" typedargslist ")" { $2 }

(* typing-ext: *)
typedargslist:
  | (*empty*)                       { [] }
  | typed_parameter                     { [$1] }
  | typed_parameter "," typedargslist { $1::$3 }

(* the original grammar enforces more restrictions on the order between
   * Param, ParamStar, and ParamPow, but each language version relaxed it *)
typed_parameter:
  | tfpdef_or_fpdef { ParamPattern (fst $1, snd $1) }
  (* TODO check default args come after variable args later *)
  | tfpdef "=" test { ParamDefault ((PatternName (fst $1), snd $1), $3) }
  | "*" tfpdef      { ParamStar ($1, $2) }
  | "*"             { ParamSingleStar $1 }
  (* python3-ext: https://www.python.org/dev/peps/pep-0570/ *)
  | "/"             { ParamSlash $1 }
  | "**" tfpdef     { ParamPow ($1, $2) }
  (* sgrep-ext: *)
  | "..."           { Flag_parsing.sgrep_guard (ParamEllipsis $1) }

tfpdef:
  | NAME            { $1, None }
  (* typing-ext: *)
  | NAME ":" test   { $1, Some $3 }

tfpdef_or_fpdef:
  | tfpdef          { PatternName (fst $1), snd $1 }
  (* python2-ext:
   * Note that this allows mixed typed and pattern parameters,
   * which are actually exclusive between Python 2 and 3
   *)
  | "(" fplist ")"  { PatternTuple $2, None }


(* without types, as in lambda *)
varargslist:
  | (*empty*)               { [] }
  | parameter                   { [$1] }
  | parameter "," varargslist { $1::$3 }

(* python3-ext: can be in any order, ParamStar before or after Classic *)
parameter:
  | fpdef           { ParamPattern ($1, None) }
  | NAME "=" test   { ParamDefault ((PatternName $1, None), $3) }
  | "*" NAME        { ParamStar ($1, ($2, None)) }
  (* python3-ext: https://www.python.org/dev/peps/pep-0570/ *)
  | "/"             { ParamSlash $1 }
  | "**" NAME       { ParamPow ($1, ($2, None)) }

fpdef:
  | NAME           { PatternName $1 }
  | "(" fplist ")" { PatternTuple $2 }

fplist:
  | fpdef            { [$1] }
  | fpdef "," fplist { $1::$3 }

(*************************************************************************)
(* Class definition *)
(*************************************************************************)

classdef: CLASS NAME arglist_paren_opt ":" suite
  { ClassDef ($1, $2, $3, $5, []) }

arglist_paren_opt:
 | (* empty *) { [] }
 | "(" ")"     { [] }
 (* python3-ext: was expr_list before *)
 | "(" list_comma(argument) ")" { $2 }

(*************************************************************************)
(* Annotations *)
(*************************************************************************)

decorator: "@" namedexpr_test NEWLINE
    { $1, $2 }

(*************************************************************************)
(* Statement *)
(*************************************************************************)

stmt:
  | simple_stmt { $1 }
  | compound_stmt { [$1] }

simple_stmt:
  | small_stmt NEWLINE { $1 }
  | small_stmt ";" NEWLINE { $1 }
  | small_stmt ";" simple_stmt { $1 @ $3 }

small_stmt:
  | expr_stmt   { [$1] }
  | del_stmt    { [$1] }
  | pass_stmt   { [$1] }
  | flow_stmt   { [$1] }
  | import_stmt { $1 }
  | global_stmt { [$1] }
  | nonlocal_stmt { [$1] }
  | assert_stmt { [$1] }
  (* python2: *)
  | print_stmt { [$1] }
  | exec_stmt { [$1] }

(* for expr_stmt see above *)

(* python2: *)
print_stmt:
  | PRINT                     { Print ($1, None, [], true) }
  | PRINT test print_testlist { Print ($1, None, $2::(fst $3), snd $3) }
  | PRINT RSHIFT test { Print ($1, Some $3, [], true) }
  | PRINT RSHIFT test "," test print_testlist
      { Print ($1, Some $3, $5::(fst $6), snd $6) }

print_testlist:
  | (* empty *)  { [], true }
  | ","          { [], false }
  | "," test print_testlist { $2::(fst $3), snd $3 }

exec_stmt:
  | EXEC expr { Exec ($1, $2, None, None) }
  | EXEC expr IN test { Exec ($1, $2, Some $4, None) }
  | EXEC expr IN test "," test { Exec ($1, $2, Some $4, Some $6) }


del_stmt: DEL exprlist { Delete ($1, List.map expr_del (to_list $2)) }

pass_stmt: PASS { Pass $1 }


flow_stmt:
  | break_stmt    { $1 }
  | continue_stmt { $1 }
  | return_stmt   { $1 }
  | raise_stmt    { $1 }
  | yield_stmt    { $1 }

break_stmt:    BREAK    { Break $1  }
continue_stmt: CONTINUE { Continue $1 }

return_stmt:
  | RETURN          { Return ($1, None) }
  | RETURN tuple(test_or_star_expr) { Return ($1, Some (tuple_expr $2)) }

yield_stmt: yield_expr { ExprStmt ($1) }

raise_stmt:
  | RAISE                           { Raise ($1, None) }
  | RAISE test                      { Raise ($1, Some ($2, None)) }
  (* python3-ext: *)
  | RAISE test FROM test            { Raise ($1, Some ($2, Some $4)) }
  (* python2-ext: *)
  | RAISE test "," test             { RaisePython2 ($1, $2, Some $4, None) }
  | RAISE test "," test "," test    { RaisePython2 ($1, $2, Some $4, Some $6) }


global_stmt: GLOBAL list_sep(NAME, ",") { Global ($1, $2) }

(* python3-ext: *)
nonlocal_stmt: NONLOCAL list_sep(NAME, ",") { NonLocal ($1, $2) }

assert_stmt:
  | ASSERT test          { Assert ($1, $2, None) }
  | ASSERT test "," test { Assert ($1, $2, Some $4) }



compound_stmt:
  | if_stmt     { $1 }
  | while_stmt  { $1 }
  | for_stmt    { $1 }
  | try_stmt    { $1 }
  | with_stmt   { $1 }

  | funcdef     { $1 }
  | classdef    { $1 }
  | decorated   { $1 }
  (* Note that there is no async_funcdef above. To avoid conflict
     * with async_stmt below, Python enforces the def to be decorated.
     *)
  | async_stmt  { $1 }

decorated:
  | decorator+ classdef {
     match $2 with
     | ClassDef (t, a, b, c, d) -> ClassDef (t, a, b, c, $1 @ d)
     | _ -> raise Impossible
   }
  | decorator+ funcdef {
     match $2 with
     | FunctionDef (t, a, b, c, d, e) -> FunctionDef (t, a, b, c, d, $1 @ e)
     | _ -> raise Impossible
   }
  | decorator+ async_funcdef {
     match $2 with
     | FunctionDef (t, a, b, c, d, e) -> FunctionDef (t, a, b, c, d, $1 @ e)
     | _ -> raise Impossible
  }

(* this is always preceded by a ":" *)
suite:
  | simple_stmt { $1 }
  | NEWLINE INDENT stmt* DEDENT { List.flatten $3 }


if_stmt: IF namedexpr_test ":" suite elif_stmt_list { If ($1, $2, $4, $5) }

elif_stmt_list:
  | (*empty *)                                   { None }
  | ELIF namedexpr_test ":" suite elif_stmt_list { Some [If ($1, $2, $4, $5)] }
  | ELSE ":" suite                               { Some ($3) }


while_stmt:
  | WHILE namedexpr_test ":" suite                { While ($1, $2, $4, []) }
  | WHILE namedexpr_test ":" suite ELSE ":" suite { While ($1, $2, $4, $7) }


for_stmt:
  | FOR exprlist IN tuple(test) ":" suite
      { For ($1, tuple_expr_store $2, $3, tuple_expr $4, $6, []) }
  | FOR exprlist IN tuple(test) ":" suite ELSE ":" suite
      { For ($1, tuple_expr_store $2, $3, tuple_expr $4, $6, $9) }


try_stmt:
  | TRY ":" suite excepthandler+
      { TryExcept ($1, $3, $4, None, None) }
  | TRY ":" suite excepthandler+ ELSE ":" suite
      { TryExcept ($1, $3, $4, Some ($5, $7), None) }
  | TRY ":" suite excepthandler+ ELSE ":" suite FINALLY ":" suite
      { TryExcept ($1, $3, $4, Some ($5, $7), Some ($8, $10)) }
  | TRY ":" suite excepthandler+ FINALLY ":" suite
      { TryExcept ($1, $3, $4, None, Some ($5, $7)) }
  | TRY ":" suite FINALLY ":" suite
      { TryExcept ($1, $3, [], None, Some ($4, $6)) }

excepthandler:
  | EXCEPT              ":" suite { ExceptHandler ($1, None, None, $3) }
  | EXCEPT test         ":" suite { ExceptHandler ($1, Some $2, None, $4) }
  | EXCEPT test AS NAME ":" suite { ExceptHandler ($1, Some $2, Some $4, $6)}
  (* python2: *)
  | EXCEPT test "," NAME ":" suite { ExceptHandler ($1, Some $2, Some $4, $6) }

with_stmt:
  | WITH with_inner ":" suite                         { $2 ($1, $4) }
  | WITH "(" with_inner_in_parens ")" ":" suite       { $3 ($1, $6) }

with_inner:
  | test                        { fun (t, body) -> With (t, ($1, None), body) }
  | test AS expr                { fun (t, body) -> With (t, ($1, Some $3), body) }
  | test         "," with_inner { fun (t, body) -> With (t, ($1, None), [$3 (t, body)]) }
  | test AS expr "," with_inner { fun (t, body) -> With (t, ($1, Some $3), [$5 (t, body)]) }

with_inner_in_parens:
  | test                                  { fun (t, body) -> With (t, ($1, None), body) }
  | test AS expr                          { fun (t, body) -> With (t, ($1, Some $3), body) }
  | test         ","                      { fun (t, body) -> With (t, ($1, None), body) }
  | test AS expr ","                      { fun (t, body) -> With (t, ($1, Some $3), body) }
  | test         "," with_inner_in_parens { fun (t, body) -> With (t, ($1, None), [$3 (t, body)]) }
  | test AS expr "," with_inner_in_parens { fun (t, body) -> With (t, ($1, Some $3), [$5 (t, body)]) }

(* python3-ext: *)
async_stmt:
  | ASYNC funcdef   { Async ($1, $2) }
  | ASYNC with_stmt { Async ($1, $2) }
  | ASYNC for_stmt  { Async ($1, $2) }

(*************************************************************************)
(* Expressions *)
(*************************************************************************)

expr:
  | xor_expr            { $1 }
  | expr BITOR xor_expr { BinOp ($1, (BitOr,$2), $3) }


xor_expr:
  | and_expr                 { $1 }
  | xor_expr BITXOR and_expr { BinOp ($1, (BitXor,$2), $3) }

and_expr:
  | shift_expr                 { $1 }
  | shift_expr BITAND and_expr { BinOp ($1, (BitAnd,$2), $3) }


shift_expr:
  | arith_expr                   { $1 }
  | shift_expr LSHIFT arith_expr { BinOp ($1, (LShift,$2), $3) }
  | shift_expr RSHIFT arith_expr { BinOp ($1, (RShift,$2), $3) }

arith_expr:
  | term                { $1 }
  | arith_expr ADD term { BinOp ($1, (Add,$2), $3) }
  | arith_expr SUB term { BinOp ($1, (Sub,$2), $3) }


term:
  | factor              { $1 }
  | factor term_op term { BinOp ($1, $2, $3) }

term_op:
  | "*"     { Mult, $1 }
  | DIV     { Div, $1 }
  | MOD     { Mod, $1 }
  | FDIV    { FloorDiv, $1 }
  | "@"     { MatMult, $1 }

factor:
  | ADD factor    { UnaryOp ((UAdd,$1), $2) }
  | SUB factor    { UnaryOp ((USub,$1), $2) }
  | BITNOT factor { UnaryOp ((Invert,$1), $2) }
  | power         { $1 }

power:
  | atom_expr            { $1 }
  | atom_expr "**" factor { BinOp ($1, (Pow,$2), $3) }

(*----------------------------*)
(* Atom expr *)
(*----------------------------*)

atom_expr:
  | atom_and_trailers        { $1 }
  | AWAIT atom_and_trailers  { Await ($1, $2) }

atom_and_trailers:
  | atom { $1 }

  | atom_and_trailers "("          ")"             { Call ($1, ($2,[],$3)) }
  | atom_and_trailers "(" list_comma(argument) ")" { Call ($1, ($2,$3,$4)) }

  | atom_and_trailers "[" list_comma(subscript)   "]"
      { match $3 with
        | [s] -> Subscript ($1, ($2, [s], $4), Load)
        | l ->
          (* Comma separated values within brackets as part of an array (or
           * other) access are treated as tuple elements, even when one or more
           * of them are slices. Unfortunately, the current Python AST cannot
           * represent a slice as a top level expression, and neither can
           * Semgrep's generic AST. So, for now, let's continue representing
           * sequences containing a slice as we were before, but for sequences
           * that only contain ordinary expressions let's convert to a tuple.
           *
           * See https://gist.github.com/nmote/cd32eb4795af7e915486124b74ba9d6a
           *
           * TODO parse this as a tuple in all cases
           * *)
          let index_exprs =
            List.filter_map
              (function | Index x -> Some x | Slice _ -> None)
              l
          in
          if List.length l =|= List.length index_exprs then
            (* If everything in the brackets is a normal expression, return a
             * single tuple *)
            let tuple =
              Tuple (CompList (Tok.unsafe_fake_bracket index_exprs), Load)
            in
            Subscript (
              $1,
              ($2, [Index tuple], $4),
              Load)
          else
            (* Otherwise, return separate items, some of which are slices, even
             * though this is not correct (see above) *)
            Subscript ($1, ($2, l, $4), Load)
      }

  | atom_and_trailers "." NAME { Attribute ($1, $2, $3, Load) }
  (* sgrep-ext: *)
  | atom_and_trailers "." "..."
    { Flag_parsing.sgrep_guard (DotAccessEllipsis ($1, $3)) }

type_for_lsif:
  | expr EOF { Type $1 }
  (* This introduces some shift-reduce conflicts. But, it should be a relatively
     unimportant warning, as this doesn't affect normal Python parsing.

     We need this because our normal Python type parser does not understand function
     types. However, `semgrep-proprietary` needs to use it to parse type hover info,
     which may contain function types. So we need to be able to interpret it.
   *)
  | parameters SUB GT expr EOF { Arrow ($1, $4) }

(*----------------------------*)
(* Atom *)
(*----------------------------*)

atom:
  | NAME        { Name ($1, Load) }

  | INT         { Literal (Num (Int $1)) }
  | LONGINT     { Literal (Num (LongInt ($1))) }
  | FLOAT       { Literal (Num (Float ($1))) }
  | IMAG        { Literal (Num (Imag ($1))) }

  | TRUE        { Literal (Bool (true, $1)) }
  | FALSE       { Literal (Bool (false, $1)) }

  | NONE        { Literal (None_ $1) }

  | string+ {
     match $1 with
     | [] ->  raise Common.Impossible
     | [x] -> x
     | xs -> ConcatenatedString xs
   }

  | atom_tuple  { $1 }
  | atom_list   { $1 }
  | atom_dict   { $1 }

  | atom_repr   { $1 }

  (* typing-ext: sgrep-ext: do not put Flag_parsing.sgrep_guard for '...' *)
  | "..."              { Ellipsis $1 }
  | "<..." test "...>" { Flag_parsing.sgrep_guard (DeepEllipsis ($1, $2, $3)) }

atom_repr: "`" testlist1 "`" { Repr ($1, tuple_expr $2, $3) }

testlist1:
  | test                 { Single $1 }
  | test "," testlist1 { cons $1 $3 }

(*----------------------------*)
(* strings *)
(*----------------------------*)

string:
  | STR { let (s, pre, tok) = $1 in
          if pre = "" then Literal (Str (s, tok)) else EncodedStr ((s, tok), pre) }
  | FSTRING_START interpolated* FSTRING_END { InterpolatedString ($1, $2, $3) }

interpolated:
  | FSTRING_STRING { Literal (Str $1) }
  | FSTRING_LBRACE interpolant fstring_print_spec "}" { InterpolatedString ($1, $2::$3, $4) }

fstring_print_spec:
  |     fstring_format_clause { $1 }
  | "=" fstring_format_clause { mk_str $1::$2 }

fstring_format_clause:
  | (*empty*) { [] }
  | fstring_format_delimeter format_specifier { mk_str $1::$2 }

fstring_format_delimeter:
  | ":" { $1 }
  | BANG { $1 }

interpolant:
  (* Note that the f-string mini-language at
   * https://docs.python.org/3/library/string.html#format-string-syntax
   * simply states that this is parsed as an "expression"; we are now
   * left trying to determine to which grammar rule an "expression"
   * corresponds. However, testing with the cpython interpreter indicates
   * that the testlist rule is what applies, as strings like:
   *   f"{not True, 1}"
   * are parsed by the interpreter.
   *
   * "interpolant" is the "value" inside one of:
   * f"{value}"
   * f"{value:format}"
   * f"{value!format}"
   *)
  | tuple(test) { tuple_expr $1 }

(* todo: maybe need another lexing state when ":" inside FSTRING_LBRACE*)
format_specifier: format_token+ { $1 }

(* see "Format Specification Mini-Language" at
 * https://docs.python.org/3/library/string.html#format-string-syntax
 *)
format_token:
  | INT   { mk_str (snd $1) }
  | FLOAT { mk_str (snd $1) }
  | NAME  { mk_str (snd $1) }
  | MOD { mk_str $1 } (* for dates *)
  | LT { mk_str $1 } | GT { mk_str $1 }
  | BITXOR { mk_str $1 }
  | ADD  { mk_str $1 } | SUB { mk_str $1 }
  | DIV { mk_str $1 }
  | "."   { mk_str $1 }
  | "=" { mk_str $1 }
  | "," { mk_str $1 }
  | ":" { mk_str $1 }

  | "{" test "}" { $2 }


(*----------------------------*)
(* containers *)
(*----------------------------*)

atom_tuple:
  | "("               ")"         { Tuple (CompList ($1, [], $2), Load) }
  | "(" testlist_comp_or_expr ")" { rewrap_paren $1 $2 $3 }
  | "(" yield_expr    ")"         { $2 }

atom_list:
  | "["               "]" { List (CompList ($1, [], $2), Load) }
  | "[" testlist_comp "]" { List ($2 ($1, $3), Load) }

atom_dict:
  | "{"                "}" { DictOrSet (CompList ($1, [], $2)) }
  | "{" dictorsetmaker "}" { DictOrSet ($2 ($1, $3)) }

dictorsetmaker:
  | dictorset_elem comp_for    { fun (t1, t2) -> CompForIf (t1, ($1, $2), t2) }
  | list_comma(dictorset_elem) { fun (t1, t2) -> CompList (t1, $1, t2) }

dictorset_elem:
  | test ":" test { KeyVal ($1, $3) }
  | test            { Key $1 }
  | star_expr       { Key $1 }
  (* python3-ext: *)
  | "**" expr        { PowInline $2 }

(*----------------------------*)
(* Array access *)
(*----------------------------*)

subscript:
  | test { Index ($1) }
  | test? ":" test? { Slice ($1, $3, None) }
  | test? ":" test? ":" test? { Slice ($1, $3, $5) }

(*----------------------------*)
(* test *)
(*----------------------------*)

test:
  | or_test                      { $1 }
  | or_test IF or_test ELSE test { IfExp ($3, $1, $5) }
  | lambdadef                    { $1 }


or_test:
  | and_test                           { $1 }
  | and_test OR list_sep(and_test, OR) { BoolOp ((Or,$2), $1::$3) }

and_test:
  | not_test                             { $1 }
  | not_test AND list_sep(not_test, AND) { BoolOp ((And,$2), $1::$3) }


not_test:
  | NOT not_test { UnaryOp ((Not,$1), $2) }
  | comparison   { $1 }

comparison:
  | expr                         { $1 }
  | expr comp_op comparison_list { Compare ($1, ($2)::(fst $3), snd $3) }

comparison_list:
  | expr                         { [], [$1] }
  | expr comp_op comparison_list { ($2)::(fst $3), $1::(snd $3) }

comp_op:
  | EQUAL   { Eq, $1 }
  | NOTEQ   { NotEq, $1 }
  | LT      { Lt, $1 }
  | LEQ     { LtE, $1 }
  | GT      { Gt, $1 }
  | GEQ     { GtE, $1 }
  | IS      { Is, $1 }
  | IN      { In, $1 }
  (* TODO: PI.combine_parse_info ? *)
  | IS NOT  { IsNot, $1 }
  | NOT IN  { NotIn, $1 }

(*----------------------------*)
(* Advanced features *)
(*----------------------------*)

(* python3-ext: *)
star_expr: "*" expr { ExprStar $2 }


yield_expr:
  | YIELD           { Yield ($1, None, false) }
  | YIELD FROM test { Yield ($1, Some $3, true) }
  | YIELD tuple(test)  { Yield ($1, Some (tuple_expr $2), false) }

lambdadef: LAMBDA varargslist ":" test { Lambda ($1, $2, $3, $4) }

(*----------------------------*)
(* Comprehensions *)
(*----------------------------*)

testlist_comp:
  | namedexpr_or_star_expr listcomp_for
      { fun (t1, t2) -> CompForIf (t1, ($1, $2), t2) }
  | tuple(namedexpr_or_star_expr)
      { fun (t1, t2) -> CompList (t1, to_list $1, t2) }

(* mostly equivalent to testlist_comp, but transform a single expression
 * in parenthesis, e.g., (1) in a regular expr, not a tuple *)
testlist_comp_or_expr:
  | namedexpr_or_star_expr comp_for
     { Tuple (CompForIf (Tok.unsafe_fake_bracket ($1, $2)), Load) }
  | tuple(namedexpr_or_star_expr)
     { tuple_expr $1 }

(* supports comp_for when used generically -- not inside atom_list
 * Note that the division here is necessary to solve a shift-reduce conflict between:
 *   foo(x for x in bar, baz)
 * in Python 3, and
 *   foo = [x for x in 1, 2]
 * in Python 2
 *)
comp_for:
 | sync_comp_for       { $1 }
 | ASYNC sync_comp_for { (* TODO *) $2 }

sync_comp_for:
  | FOR exprlist IN or_test
    { [CompFor (tuple_expr_store $2, $4)] }
  | FOR exprlist IN or_test comp_iter
    { [CompFor (tuple_expr_store $2, $4)] @ $5 }


(* support mixed python2 / python3 comp_for only when used inside atom_list *)
listcomp_for:
 | listsync_comp_for       { $1 }
 | ASYNC listsync_comp_for { (* TODO *) $2 }

list_for:
  or_test "," list_for_rest { tuple_expr (Tup ($1::$3)) }

list_for_rest:
  | or_test                   { [$1] }
  | or_test "," list_for_rest { $1::$3 }

listsync_comp_for:
  | sync_comp_for { $1 }
  (* python2-ext: [x for x in 1, 2] *)
  | FOR exprlist IN list_for { [CompFor (tuple_expr_store $2, $4) ] }

(* /comp_for *)

comp_iter:
  | comp_for { $1 }
  | comp_if  { $1 }

comp_if:
  | IF test_nocond           { [CompIf ($2)] }
  | IF test_nocond comp_iter { [CompIf ($2)] @ $3 }

test_nocond:
  | or_test          { $1 }
  | lambdadef_nocond { $1 }

lambdadef_nocond: LAMBDA varargslist ":" test_nocond
    { Lambda ($1, $2, $3, $4) }


(*----------------------------*)
(* Arguments *)
(*----------------------------*)

(* python3-ext: can be any order, ArgStar before or after ArgKwd *)
argument:
  | test           { Arg $1 }
  | test comp_for  { ArgComp ($1, $2) }

  (* python3-ext: *)
  | test ":=" test  { Arg (NamedExpr ($1, $2, $3)) }
  | "*" test           { ArgStar ($1, $2) }
  | "**" test          { ArgPow  ($1, $2) }

  (* sgrep-ext: difficult to move in atom without s/r conflict so restricted
     * to argument for now *)
  | NAME ":" test
    { Flag_parsing.sgrep_guard (Arg (TypedMetavar ($1, $2, $3))) }

  | test "=" test
      { match $1 with
        | Name (id, _) -> ArgKwd (id, $3)
        | _ -> raise Parsing.Parse_error
      }

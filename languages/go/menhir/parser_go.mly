%{
(* // Copyright 2009 The Go Authors. All rights reserved.
 * // Use of this source code is governed by a BSD-style
 * // license that can be found in the LICENSE file.
 *
 * // Go language grammar.
 * //
 * // The Go semicolon rules are:
 * //
 * //  1. all statements and declarations are terminated by semicolons.
 * //  2. semicolons can be omitted before a closing ) or }.
 * //  3. semicolons are inserted by the lexer before a newline
 * //      following a specific list of tokens.
 * //
 * // Rules #1 and #2 are accomplished by writing the lists as
 * // semicolon-separated lists with an optional trailing semicolon.
 * // Rule #3 is implemented in yylex.
 *
 * src: this is mostly an ocamlyacc port of the Go yacc grammar in
 *  src/cmd/compile/internal/gc/go.y in commit
 *  b5fe07710f4a31bfc100fbc2e344be11e4b4d3fc^ in the golang source code
 *  at https://github.com/golang/go
 *)
open Common
open Either_
open AST_generic (* for the arithmetic operator *)
open Ast_go

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error tok s =
  raise (Parsing_error.Other_error (s, tok))

let rev = true

let mk_bin e1 op tok e2 =
  Binary (e1, (op, tok), e2)
let mk_unary op tok e =
  Unary ((op, tok), e)
let mk_arg x =
  match x with
  | Left e -> Arg e
  | Right t -> ArgType t

let condition_of_stmt tok stmt =
  match stmt with
  | ExprStmt e -> e
  | _ -> error tok "condition is not an expression"

let mk_else elseifs else_ =
  let elseifs = List.rev elseifs in
  List.fold_right (fun elseif accu ->
      let ((tok, stopt, cond), body) = elseif in
      Some (If (tok, stopt, cond, body, accu))
  ) elseifs else_

let rec expr_to_type tok e =
  match e with
  | Id (id) -> TName [id]
  | Deref (t, e) -> TPtr (t, expr_to_type tok e)
  | Selector (Id (id1), _, id2) -> TName [id1;id2]
  | ParenType t -> t
  | _ -> error tok ("TODO: expr_to_type: " ^ Dumper.dump e)

let expr_or_type_to_type tok x =
  match x with
  | Right t -> t
  | Left e -> expr_to_type tok e

(* some casts such as ( *byte)(foo) are actually parsed as
 * Calls with a ParenType. We need to convert back those in
 * Cast.
 *)
let mk_call_or_cast (e, targs_opt, args) =
  match e, targs_opt, args with
  | ParenType t, None, (l, [Arg e], r) -> Cast (t, (l, e, r))
  | _ -> Call (e, targs_opt, args)

let type_to_id x =
  match x with
  | TName [id] -> id
  | _ -> failwith ("type_to_id: was expecting an id" ^ Dumper.dump x)

(* see golang spec on signatures. If you have
 * func foo(a, b, c) then it means a, b, and c are types. If you have once
 * an identifier and a type, as in
 * func foo(a, b, c, d e) this means a, b, c, and d are of type e.
 *)
let adjust_signatures params =
  let params = List.rev params in
  let all_types =
   params |> List.for_all (function
      | ParamClassic {pname = None; _} -> true
      (* sgrep-ext: ellipsis count as a type *)
      | ParamEllipsis _ -> true
      | ParamMetavarEllipsis _ -> true
      | _ ->false) in
  if all_types
  then params
  else
    let rec aux acc xs =
      match xs with
      | [] -> if acc =*= []
              then []
              else begin
                failwith ("last parameter should have a type and id" ^
                    Dumper.dump acc)
              end
      | x::xs ->
        (match x with
          | ParamClassic { pname = Some _; ptype = t; _ } ->
             ((acc |> List.rev |> List.map (fun id ->
               ParamClassic { pname = Some id; ptype = t; pdots = None })) @
               [x]) @
               aux [] xs
          | ParamClassic { pname = None; ptype = id_typ; _ } ->
            let id = type_to_id id_typ in
            aux (id::acc) xs
          | ParamEllipsis t -> (ParamEllipsis t):: aux [] xs
          | ParamMetavarEllipsis id -> (ParamMetavarEllipsis id) :: aux [] xs
        )
    in
    aux [] params

(* bugfix: the parser used to add some extra Empty at the end of some Block
 * because of the way Go handles semicolons (see tests/go/misc_empty.go).
 * Indeed stmt_list accept ';' as a separator (and with ASI it can even
 * be ommitted), but to allow also ';' as a terminator, the grammar
 * allow empty stmt. This does not interact well with sgrep because
 * a pattern like 'foo(); bar();' will implicitely have an Empty
 * at the end which will prevent it to match many things. This is why
 * we remove those implicitely added Empty.
 *)
let rev_and_fix_stmts xs =
  List.rev (
    match xs with
    | Empty::xs -> xs
    | _ -> xs
  )

let rev_and_fix_items xs =
  List.rev (
    match xs with
    | IStmt Empty::xs -> xs
    | _ -> xs
  )

%}

(*************************************************************************)
(* Tokens *)
(*************************************************************************)
%token <Ast_go.tok> TUnknown  (* unrecognized token *)
%token <Ast_go.tok> EOF

(*-----------------------------------------*)
(* The space/comment tokens *)
(*-----------------------------------------*)
(* coupling: Token_helpers.is_comment *)
%token <Ast_go.tok> TCommentSpace TComment TCommentNewline

(*-----------------------------------------*)
(* The normal tokens *)
(*-----------------------------------------*)

(* tokens with "values" (was LLITERAL before) *)
%token  <Parsed_int.t> LINT
%token  <float option * Ast_go.tok>  LFLOAT
%token  <string * Ast_go.tok>  LIMAG  LRUNE LSTR
%token  <AST_generic.operator * Ast_go.tok> LASOP
%token  <string * Ast_go.tok> LNAME

(*-----------------------------------------*)
(* Keyword tokens *)
(*-----------------------------------------*)

%token  <Ast_go.tok>
  LIF LELSE
  LFOR
  LRETURN LBREAK LCONTINUE LFALL
  LSWITCH LCASE LDEFAULT
  LGOTO
  LFUNC LCONST LVAR LTYPE LSTRUCT LINTERFACE
  LGO LCHAN LSELECT
  LDEFER
  LPACKAGE LIMPORT
  LMAP
  LRANGE

(*-----------------------------------------*)
(* Punctuation tokens *)
(*-----------------------------------------*)
(* syntax *)
%token <Ast_go.tok>
  LPAREN "(" RPAREN  ")"
  LBRACE "{" RBRACE "}"
  LBRACKET "[" RBRACKET "]"
  LCOLON ":" LEQ "=" LDOT "." LCOMMA ","
  LCOLAS ":="
  LDDD "..."
  LDots "<..." RDots "...>"

(* operators *)
%token <Ast_go.tok>
  LPLUS LMINUS LMULT "*" LDIV LPERCENT
  LPIPE LAND "&" LHAT
  LANDAND LOROR
  LANDNOT
  LINC LDEC
  LEQEQ LNE
  LGE LGT LLE LLT
  LLSH LRSH
  LBANG
  LCOMM "<-"

(*-----------------------------------------*)
(* Extra tokens: *)
(*-----------------------------------------*)
%token <Ast_go.tok>
  LBODY (* LBRACE parsing hack *)
  LSEMICOLON ";" (* sometimes implicitely inserted, see Parsing_hacks_go.ml *)

(* sgrep-ext: *)
%token <Ast_go.tok> LBRACE_SEMGREP LCOLON_SEMGREP LPAREN_SEMGREP

(*************************************************************************)
(* Priorities *)
(*************************************************************************)

%left       LCOMM (*outside the usual hierarchy; here for good error msg*)

%left       LOROR
%left       LANDAND
%left       LEQEQ LNE LLE LGE LLT LGT
%left       LPLUS LMINUS LPIPE LHAT
%left       LMULT LDIV LPERCENT LAND LLSH LRSH LANDNOT

(*
 // manual override of shift/reduce conflicts.
 // the general form is that we assign a precedence
 // to the token being shifted and then introduce
 // NotToken with lower precedence or PreferToToken with higher
 // and annotate the reducing rule accordingly.
 *)

%left       NotParen
%left       LPAREN

(* %left       RPAREN *)
(* %left       PreferToRightParen *)

(*************************************************************************)
(* Rules type declaration *)
(*************************************************************************)

%start file sgrep_spatch_pattern
%type <Ast_go.program> file
%type <Ast_go.any>     sgrep_spatch_pattern

%%

(*************************************************************************)
(* Macros *)
(*************************************************************************)

list_sep(X,Sep):
 | X                      { [$1] }
 | list_sep(X,Sep) Sep X  { $3 :: $1  }

(* TODO: use List.rev here to avoid forcing it at the caller?
 * Or maybe rewrite to avoid using List.rev (even if quadratic @)
 * because if the X returns a list (e.g., mk_vars does), then
 * you may reverse it wrong or just partially!
 *)
listsc(X): list_sep(X, ";") { $1 }
listc(X): list_sep(X, ",") { $1 }


(* list separated by Sep and possibly terminated by trailing Sep.
 * This has to be recursive on the right, otherwise s/r conflict.
 *)
list_sep_term(X,Sep):
 | X                       { [$1] }
 | X Sep                   { [$1] }
 | X Sep list_sep_term(X,Sep)  { $1 :: $3 }

listsc_t(X): list_sep_term(X, ";") { $1 }

(*************************************************************************)
(* Toplevel *)
(*************************************************************************)

file: package ";" imports xdcl_list EOF
  { ($1)::($3 |> List.map (fun x -> Import x)) @ (List.rev $4) }

package: LPACKAGE sym { Package ($1, $2) }

(* Go does some ASI so we do not need like in Java to use stmt_no_dots
 * to allow '...' without trailing semicolon and avoid ambiguities.
 *)
sgrep_spatch_pattern:
   (* make version with and without ";" which is inserted
    * if the item as a newline before the EOF (which leads to ASI)
    *)
 | item ";"? EOF  {
    match $1 with
    | [IStmt (SimpleStmt (ExprStmt x))] -> E x
    | [ITop top_decl] ->
        (match top_decl with
        (* the user probably wanted a Partial here. xfndcl allows
         * empty functions/methods, but I doubt people want explicitely
         * to match that => better to return a Partial
         *)
        | DFunc (_, _, (_, Empty)) | DMethod (_, _, (_, Empty))
           -> Partial (PartialDecl top_decl)
        | _ -> item1 $1
        )
    | _ -> item1 $1
    }
 | item ";" item ";" item_list EOF
    { Items ($1 @ $3 @ rev_and_fix_items $5) }

 (* In interfaces, methods do not require the 'func' keyword, but
  * that makes them possibly ambiguous with function calls,
  * hence the need for LPAREN_SEMGREP and lalr(k) hack in Parsing_hacks_go.ml.
  * Note that we must use 'name' here and not 'sym' or 'new_name' because
  * of the %prec NotParen used for 'name' used in 'pexpr'. Otherwise
  * with 'sym "(" oarg_type_list_ocomma ...' we would always shift.
  * Note that with LPAREN_SEMGREP this is less needed and we could
  * use sym again.
  * This is mostly a copy paste of 'interfacedcl', but here I require
  * a fnret_type (not a fnres which has a return type as an option)
  * otherwise it can be ambiguous with a function call in certain
  * situations (e.g., how to parse 'foo(...)'? at least 'foo(...) int'
  * is not ambiguous.
  *)
 | name LPAREN_SEMGREP oarg_type_list_ocomma ")" fnret_type  ";"? EOF
    { let pret =
         [ParamClassic { pname = None; ptype = $5; pdots = None }] in
      let ftype = { ftok = $2; fparams = ($2, $3, $4); fresults = pret } in
      let top_decl = DFunc ($1, None, (ftype, Empty)) in
      Partial (PartialDecl top_decl)
    }

 (* partials! *)
 | partial EOF { Partial $1 }

partial:
 | LBRACE_SEMGREP braced_keyval_list  "}" ";"?
     { PartialInitBraces ($1, $2, $3) }
 | LNAME LCOLON_SEMGREP complitexpr ";"?
     { PartialSingleField ($1, $2, $3) }

item:
 | stmt   { [IStmt $1] }
 | import { $1 |> List.map (fun x -> IImport x) }
 | package { [ITop $1] }
 | xfndcl { [ITop $1] }

item_list:
|   item { $1 }
|   item_list ";" item { $3 @ $1 }

(*************************************************************************)
(* Import *)
(*************************************************************************)

import:
|   LIMPORT import_stmt
      { [$2 $1] }
|   LIMPORT "(" listsc_t(import_stmt_or_dots) ")"
      { $3 |> List_.filter_some |> List_.map (fun f -> f $1) }
|   LIMPORT "(" ")" { [] }

import_stmt_or_dots:
 | import_stmt { Some $1 }
 (* sgrep-ext: simpler to just filter them.
  * Anyway 'import (a; b)' is translated in 'import a; import b'
 *)
 | "..." { Flag_parsing.sgrep_guard None }

import_stmt:
|        LSTR
    { fun i_tok -> { i_tok; i_path = $1; i_kind = ImportOrig }
      (*// import with original name*) }
|   sym  LSTR
    { fun i_tok -> { i_tok; i_path = $2; i_kind = ImportNamed $1 }
      (*// import with given name*)  }
|   "." LSTR
    { fun i_tok -> { i_tok; i_path = $2; i_kind = ImportDot $1 }
      (*// import into my name space *) }

(*************************************************************************)
(* Declarations *)
(*************************************************************************)

xdcl:
|   common_dcl { $1 |> List.map (fun decl -> DTop decl) }
|   xfndcl     { [$1] }

common_dcl:
|   LVAR vardcl                   { $2 }
|   LVAR "(" listsc_t(vardcl) ")" { List.flatten $3 }
|   LVAR "(" ")"                  { [] }

    (* at least the first const has a value *)
|   LCONST constdcl { $2 }
|   LCONST "(" constdcl ";"? ")" { $3 }
|   LCONST "(" constdcl ";" constdcl1_list ";"? ")"   { $3 @ (List.rev $5) }
|   LCONST "(" ")" { [] }

|   LTYPE typedcl                   { [$2] }
|   LTYPE "(" listsc_t(typedcl) ")" { $3 }
|   LTYPE "(" ")"                   { [] }


vardcl:
|   listc(dcl_name) ntype               { mk_vars $1 (Some $2) None }
|   listc(dcl_name) ntype "=" listc(expr) { mk_vars $1 (Some $2) (Some $4) }
|   listc(dcl_name)       "=" listc(expr) { mk_vars $1 None      (Some $3) }

(* this enforces the const has a value *)
constdcl:
|   listc(dcl_name) ntype "=" listc(expr) { mk_consts ~rev $1 (Some $2) (Some $4)  }
|   listc(dcl_name)       "=" listc(expr) { mk_consts ~rev $1 None (Some $3) }

constdcl1:
|   constdcl            { $1 }
|   listc(dcl_name) ntype { mk_consts ~rev $1 (Some $2) None }
|   listc(dcl_name)       { mk_consts ~rev $1 None None }


typedcl:
| typedclname ntype     { DTypeDef ($1, None, $2) }
(* alias decl, go 1.?? *)
| typedclname "=" ntype { DTypeAlias ($1, $2, $3) }

(*************************************************************************)
(* Statements *)
(*************************************************************************)

stmt:
(* stmt_list requires ; as a separator, so Go allows empty stmt
   * so that it can also be used as a terminator (hacky).
   * See rev_and_fix_stmts for more information.
   *)
| (*empty*)   { Empty }

| compound_stmt   { $1 }
| common_dcl      { DeclStmts $1 }
| non_dcl_stmt    { $1 }

compound_stmt: "{" listsc(stmt) "}"
  { Block ($1, rev_and_fix_stmts $2, $3) }

non_dcl_stmt:
|   simple_stmt { SimpleStmt $1 }

|   if_stmt { $1 }
|   for_stmt { $1 }
|   switch_stmt { $1 }
|   select_stmt { $1 }

|   labelname ":" stmt    { Label ($1, $3) }
|   LGOTO new_name        { Goto ($1, $2) }

|   LBREAK    new_name? { Break ($1, $2) }
|   LCONTINUE new_name? { Continue ($1, $2) }
|   LRETURN oexpr_list  { Return ($1, $2) }
|   LFALL { Fallthrough $1 }

|   LGO pseudocall    { Go ($1, $2) }
|   LDEFER pseudocall { Defer ($1, $2) }


simple_stmt:
|   expr                       { ExprStmt $1 }
|   expr LASOP expr            { AssignOp ($1, $2, $3) }
|   listc(expr) "=" listc(expr)    { Assign (List.rev $1, $2, List.rev $3)  }
|   listc(expr) ":=" listc(expr) { DShortVars (List.rev $1, $2, List.rev $3) }
|   expr LINC                  { IncDec ($1, (Incr, $2), Postfix) }
|   expr LDEC                  { IncDec ($1, (Decr, $2), Postfix) }



(* IF cond body (ELSE IF cond body)* (ELSE block)? *)
if_stmt: LIF  if_header loop_body elseif_list else_
    { match $2 with
      | stopt, Some st ->
        If ($1, stopt, condition_of_stmt $1 st, $3, mk_else $4 $5)
      | _, None ->
        error $1 "missing condition in if statement"
    }

if_header:
|   simple_stmt? { (None, $1) }
|   simple_stmt? ";" simple_stmt? { ($1, $3) }


elseif: LELSE LIF  if_header loop_body
    { match $3 with
      | stopt, Some st ->
        ($2, stopt, condition_of_stmt $2 st), $4
      | _, None ->
        error $2 "missing condition in if statement"
    }

else_:
| (*empty*)         { None }
|   LELSE compound_stmt { Some $2 }


for_stmt:
 | LFOR simple_stmt? ";" simple_stmt? ";" simple_stmt? loop_body
    { For ($1, ForClassic ($2, Option.map (condition_of_stmt $1) $4, $6), $7) }
 | LFOR simple_stmt loop_body
    { let cond = condition_of_stmt $1 $2 in
      let for_header =
        match cond with
        (* sgrep-ext: *)
        | Ellipsis t -> ForEllipsis t
        (* general case *)
        | _ -> ForClassic (None, Some cond, None)
      in
      For ($1, for_header, $3)
    }
 | LFOR              loop_body
    { For ($1, ForClassic (None, None, None), $2) }
 | LFOR listc(expr) "="    LRANGE expr loop_body
    { For ($1, ForRange (Some (List.rev $2, $3), $4, $5), $6)  }
 | LFOR listc(expr) ":=" LRANGE expr loop_body
    { For ($1, ForRange (Some (List.rev $2, $3), $4, $5), $6) }
 | LFOR                  LRANGE expr loop_body
    { For ($1, ForRange (None, $2, $3), $4) }


loop_body: LBODY listsc(stmt) "}" { Block ($1, rev_and_fix_stmts $2, $3) }


(* split in 2, switch expr and switch types *)
switch_stmt: LSWITCH if_header LBODY caseblock_list "}"
    { let stopt1, stopt2 = $2 in
      Switch ($1, stopt1, stopt2 ,List.rev $4)
    }

select_stmt:  LSELECT LBODY caseblock_list "}"
    { Select ($1, List.rev $3) }

case:
|   LCASE listc(expr_or_type) ":"           { CaseExprs ($1, $2) }
|   LCASE listc(expr_or_type) "=" expr ":"  { CaseAssign ($1, $2, $3, $4) }
|   LCASE listc(expr_or_type) ":=" expr ":" { CaseAssign ($1, $2, $3, $4) }
|   LDEFAULT ":"                            { CaseDefault $1 }

caseblock_list:
| (*empty*)  { [] }
| caseblock_list caseblock { $2::$1 }

caseblock:
| case listsc(stmt)
    {
      CaseClause ($1, unsafe_stmt1 (rev_and_fix_stmts $2))
      (*
        // If the last token read by the lexer was consumed
        // as part of the case, clear it (parser has cleared yychar).
        // If the last token read by the lexer was the lookahead
        // leave it alone (parser has it cached in yychar).
        // This is so that the stmt_list action doesn't look at
        // the case tokens if the stmt_list is empty.
        yylast = yychar;
        $1.Xoffset = int64(block);

        // This is the only place in the language where a statement
        // list is not allowed to drop the final semicolon, because
        // it's the only place where a statement list is not followed
        // by a closing brace.  Handle the error for pedantry.

        // Find the final token of the statement list.
        // yylast is lookahead; yyprev is last of stmt_list
        last := yyprev;

        if last > 0 && last != ';' && yychar != '}' {
            Yyerror("missing statement after label");
        }
        $$ = $1;
        $$.Nbody = $3;
        popdcl();
      *)
    }
(* sgrep-ext:
 * We can't use just '...' because this cause s/r conflict with
 *   case foo:
 *      bar()
 *   ...
 * where we don't know if the ... should be part of the list of
 * stmts of the foo: case, or an ellipsis for extra cases.
 *
 * I've added the ";"? because parsing_hacks adds them after a '...'\n.
 * alt: we could detect if there's a case before and disable it.
 *)
| LCASE "..." ";"? { CaseEllipsis ($1, $2) }

(*************************************************************************)
(* Expressions *)
(*************************************************************************)

expr:
|   uexpr              { $1 }

|   expr LOROR expr    { mk_bin $1 Or $2 $3 }
|   expr LANDAND expr  { mk_bin $1 And $2 $3 }
|   expr LEQEQ expr    { mk_bin $1 Eq $2 $3 }
|   expr LNE expr      { mk_bin $1 NotEq $2 $3 }
|   expr LLT expr      { mk_bin $1 Lt $2 $3 }
|   expr LLE expr      { mk_bin $1 LtE $2 $3 }
|   expr LGE expr      { mk_bin $1 GtE $2 $3 }
|   expr LGT expr      { mk_bin $1 Gt $2 $3 }
|   expr LPLUS expr    { mk_bin $1 Plus $2 $3 }
|   expr LMINUS expr   { mk_bin $1 Minus $2 $3 }
|   expr LPIPE expr    { mk_bin $1 BitOr $2 $3 }
|   expr LHAT expr     { mk_bin $1 BitXor $2 $3 }
|   expr "*" expr    { mk_bin $1 Mult $2 $3 }
|   expr LDIV expr     { mk_bin $1 Div $2 $3 }
|   expr LPERCENT expr { mk_bin $1 Mod $2 $3 }
|   expr "&" expr     { mk_bin $1 BitAnd $2 $3 }
|   expr LANDNOT expr  { mk_bin $1 BitNot (* BitAndNot aka BitClear *) $2 $3 }
|   expr LLSH expr     { mk_bin $1 LSL $2 $3 }
|   expr LRSH expr     { mk_bin $1 LSR $2 $3 }

(* old: was in expression, to give better error message, but better here *)
|   expr "<-" expr    { Send ($1, $2, $3) }

 (* sgrep-ext: *)
 | "..." { Flag_parsing.sgrep_guard (Ellipsis $1) }
 | "<..." expr "...>" { Flag_parsing.sgrep_guard (DeepEllipsis ($1, $2, $3)) }

uexpr:
|   pexpr { $1 }

|   "*" uexpr { Deref ($1, $2)}
|   "&" uexpr
    {
           (* // Special case for &T{...}: turn into ( *T){...}. *)
      Ref ($1, $2)
    }
|   LPLUS  uexpr { mk_unary Plus $1 $2 }
|   LMINUS uexpr { mk_unary Minus $1 $2 }
|   LBANG  uexpr { mk_unary Not $1 $2 }
|   LHAT uexpr { mk_unary BitXor $1 $2  }
|   "<-" uexpr { Receive ($1, $2) }

pexpr:
|   pexpr_no_paren { $1 }
|   "(" expr_or_type ")"
    { match $2 with
      | Left e -> e
      | Right t -> ParenType t
    }


pexpr_no_paren:
|   basic_literal { BasicLit $1 }

|   name { Id ($1) }
    (* sgrep-ext: *)
|   "(" name ":" ntype ")" { Flag_parsing.sgrep_guard (TypedMetavar($2, $3, $4)) }
    (* can be many things *)
|   pexpr "." sym { Selector ($1, $2, $3) }
(* sgrep-ext: *)
|   pexpr "." "..." { Flag_parsing.sgrep_guard (DotAccessEllipsis ($1, $3)) }

|   pexpr "." "(" expr_or_type ")"
    { TypeAssert ($1, ($3, expr_or_type_to_type $2 $4, $5)) }
    (* less: only inside a TypeSwitch, rewrite grammar? *)
|   pexpr "." "(" LTYPE ")"
    { TypeSwitchExpr ($1, $3) }

|   pexpr "[" expr "]" { Index ($1, ($2, $3, $4)) }
|   pexpr "[" expr? ":" expr? "]" { Slice ($1, ($2, ($3, $5, None), $6)) }
|   pexpr "[" expr? ":" expr? ":" expr? "]"
    { Slice ($1, ($2, ($3, $5, $7), $6))
        (*if $5 == nil {
            Yyerror("middle index required in 3-index slice");
        }
        if $7 == nil {
            Yyerror("final index required in 3-index slice");
        }
        *)
    }

|   pseudocall { mk_call_or_cast $1 }

|   convtype "(" expr ","? ")" { Cast ($1, ($2, $3, $5)) }

|   comptype       lbrace braced_keyval_list "}"
    { CompositeLit ($1, ($2, $3, $4)) }
|   pexpr_no_paren "{" braced_keyval_list "}"
    { CompositeLit (expr_to_type $2 $1, ($2, $3, $4)) }

|   fnliteral { $1 }


basic_literal:
| LINT   { Int $1 }
| LFLOAT { Float $1 }
| LIMAG  { Imag $1 }
| LRUNE  { Rune $1 }
| LSTR   { String $1 }


(*
 * call-like statements that
 * can be preceded by 'defer' and 'go'
 * the None below are for Call's type_arguments option, which are not handled
 * in this file yet (but are handled in tree-sitter-go).
 *)
pseudocall:
|   pexpr "(" ")"
      { ($1, None, ($2,[],$3)) }
|   pexpr "(" arguments ","? ")"
      { ($1, None, ($2, $3 |> List.rev |> List.map mk_arg, $5)) }
|   pexpr "(" arguments "..." ","? ")"
      { let args =
          match $3 |> List.map mk_arg with
          | [] -> raise Impossible
          | (Arg e)::xs -> List.rev ((ArgDots (e, $4))::xs)
          | (ArgDots _)::_ -> raise Impossible
          | (ArgType _t)::_ -> raise Impossible
         in
         $1, None, ($2, args, $6)
      }

(* was expr_or_type_list before *)
arguments:
 | argument { [$1] }
 | arguments "," argument { $3 :: $1 }

argument: expr_or_type { $1 }



braced_keyval_list:
|(*empty*)         { [] }
|   keyval_list ","? { List.rev $1 }

(*
 * list of combo of keyval and val
 *)
keyval_list:
|   keyval                              { [$1] }
|   bare_complitexpr                    { [$1] }

|   keyval_list "," keyval           { $3 :: $1 }
|   keyval_list "," bare_complitexpr { $3 :: $1 }

keyval: complitexpr ":" complitexpr { InitKeyValue ($1, $2, $3) }

complitexpr:
|   expr { InitExpr $1 }
|   "{" braced_keyval_list "}" { InitBraces ($1, $2, $3) }

bare_complitexpr:
(* Special case to deal with automatic semicolon insertion. We want to parse
   &http.Transport{
     ...
   }
   but ASI turns this into
   &http.Transport{
     ...;
   }
*)
|   "..." ";" { InitExpr (Ellipsis $1)}
|   expr { InitExpr $1 }
|   "{" braced_keyval_list "}" { InitBraces ($1, $2, $3) }





(* less: I don't think we need that with a good fix_tokens_lbody *)
lbrace:
|   LBODY { $1 }
|   "{" { $1 }

(*************************************************************************)
(* Names *)
(*************************************************************************)

sym:
|   LNAME
    {
        (*// during imports, unqualified non-exported identifiers are from builtinpkg
        if importpkg != nil && !exportname($1.Name) {
            $$ = Pkglookup($1.Name, builtinpkg);
        }
        *)
      $1
    }

(*
 *  newname is used before declared
 *  oldname is used after declared
 *)
%inline
new_name: sym { $1 }

%inline
dcl_name: sym { $1 }

name: sym %prec NotParen { $1 }

%inline
labelname: new_name { $1 }

%inline
typedclname:  sym
    { $1
        (*
        // different from dclname because the name
        // becomes visible right here, not at the end
        // of the declaration.
        *)
    }


dotname:
|   name { [$1] }
|   name "." sym { [$1; $3] }

packname:
|   LNAME { [$1] }
|   LNAME "." sym { [$1; $3] }

(*************************************************************************)
(* Types *)
(*************************************************************************)

(*
 // to avoid parsing conflicts, type is split into
 //  channel types
 //  function types
 //  parenthesized types
 //  any other type
 // the type system makes additional restrictions,
 // but those are not implemented in the grammar.
 *)

ntype:
|   dotname      { TName $1 }

|   ptrtype      { $1 }
|   recvchantype { $1 }
|   fntype       { TFunc $1 }

|   othertype           { $1 }
|   "(" ntype ")" { $2 }

non_recvchantype:
|   dotname { TName $1 }

|   ptrtype { $1 }
|   fntype  { TFunc $1 }

|   othertype { $1 }
|   "(" ntype ")" { $2 }


ptrtype: "*" ntype { TPtr ($1, $2) }

recvchantype: "<-" LCHAN ntype { TChan ($2, TRecv, $3) }

fntype: LFUNC "(" oarg_type_list_ocomma ")" fnres
  { { ftok = $1; fparams = ($2, $3, $4); fresults = $5 } }

fnres:
| (*empty *)    %prec NotParen      { [] }
|   fnret_type   { [ParamClassic { pname = None; ptype = $1; pdots = None }] }
|   "(" oarg_type_list_ocomma ")"   { $2 }

fnret_type:
|   dotname      { TName $1 }

|   ptrtype      { $1 }
|   recvchantype { $1 }
|   fntype       { TFunc $1 }

|   othertype    { $1 }



othertype:
|   "[" oexpr_no_dots "]" ntype { TArray (($1, $2, $3), $4) }
|   "[" "..." "]" ntype         { TArrayEllipsis (($1, $2, $3), $4) }

|   LCHAN non_recvchantype { TChan ($1, TBidirectional, $2) }
|   LCHAN "<-" ntype       { TChan ($1, TSend, $3) }

|   LMAP "[" ntype "]" ntype { TMap ($1, ($2, $3, $4), $5) }

|   structtype    { $1 }
|   interfacetype { $1 }

oexpr_no_dots:
|(*empty*) { None }
|   expr_no_dots       { Some $1 }
expr_no_dots:
|   uexpr              { $1 }
|   expr LOROR expr    { mk_bin $1 Or $2 $3 }
|   expr LANDAND expr  { mk_bin $1 And $2 $3 }
|   expr LEQEQ expr    { mk_bin $1 Eq $2 $3 }
|   expr LNE expr      { mk_bin $1 NotEq $2 $3 }
|   expr LLT expr      { mk_bin $1 Lt $2 $3 }
|   expr LLE expr      { mk_bin $1 LtE $2 $3 }
|   expr LGE expr      { mk_bin $1 GtE $2 $3 }
|   expr LGT expr      { mk_bin $1 Gt $2 $3 }
|   expr LPLUS expr    { mk_bin $1 Plus $2 $3 }
|   expr LMINUS expr   { mk_bin $1 Minus $2 $3 }
|   expr LPIPE expr    { mk_bin $1 BitOr $2 $3 }
|   expr LHAT expr     { mk_bin $1 BitXor $2 $3 }
|   expr "*" expr    { mk_bin $1 Mult $2 $3 }
|   expr LDIV expr     { mk_bin $1 Div $2 $3 }
|   expr LPERCENT expr { mk_bin $1 Mod $2 $3 }
|   expr "&" expr     { mk_bin $1 BitAnd $2 $3 }
|   expr LANDNOT expr  { mk_bin $1 BitNot (* BitAndNot aka BitClear *) $2 $3 }
|   expr LLSH expr     { mk_bin $1 LSL $2 $3 }
|   expr LRSH expr     { mk_bin $1 LSR $2 $3 }
|   expr "<-" expr    { Send ($1, $2, $3) }



dotdotdot:
|   "..." ntype { $1, $2 }


convtype:
|   fntype    { TFunc $1 }
|   othertype { $1 }

comptype:
| othertype { $1 }


expr_or_type:
|   expr                                     { Left $1 }
|   non_expr_type (*   %prec PreferToRightParen *) { Right $1 }

non_expr_type:
|   fntype              { TFunc $1 }
|   recvchantype        { $1 }
|   othertype           { $1 }
|   "*" non_expr_type { TPtr ($1, $2) }

(*************************************************************************)
(* Struct/Interface *)
(*************************************************************************)

structtype:
|   LSTRUCT lbrace listsc_t(structdcl) "}"
    { TStruct ($1, ($2, List.flatten $3, $4)) }
|   LSTRUCT lbrace "}"
    { TStruct ($1, ($2, [], $3)) }

structdcl:
|   listc(new_name) ntype LSTR?
    { $1 |> List.map (fun id -> Field (id, $2), $3) }
|       packname      LSTR? { [EmbeddedField (None, $1), $2] }
|   "*" packname      LSTR? { [EmbeddedField (Some $1, $2), $3] }
(* sgrep-ext: *)
| "..." { Flag_parsing.sgrep_guard [FieldEllipsis $1, None] }


interfacetype:
|   LINTERFACE lbrace listsc_t(interfacedcl) "}"
    { TInterface ($1, ($2, $3, $4)) }
|   LINTERFACE lbrace "}"
    { TInterface ($1, ($2, [], $3)) }

interfacedcl:
|   new_name indcl { Method ($1, $2) }
|   packname       { EmbeddedInterface $1 }
(* sgrep-ext: *)
| "..."            { Flag_parsing.sgrep_guard (FieldEllipsis2 $1) }

(* fntype // without func keyword *)
indcl: "(" oarg_type_list_ocomma ")" fnres
   { { ftok = $1; fparams = ($1, $2, $3); fresults = $4; } }

(*************************************************************************)
(* Function *)
(*************************************************************************)

(* // all in one place to show how crappy it all is *)
xfndcl: LFUNC fndcl fnbody
    { $2 $1 $3 }

fndcl:
|   sym "(" oarg_type_list_ocomma ")" fnres
     { fun ftok body ->
        DFunc ($1, None, ({ ftok; fparams=($2, $3, $4); fresults = $5; },body))
     }
|   "(" oarg_type_list_ocomma ")" sym
    "(" oarg_type_list_ocomma ")" fnres
     {
      fun ftok body ->
        match $2 with
        | [ParamClassic x] ->
            DMethod ($4, x, ({ ftok; fparams = ($5, $6, $7); fresults = $8 }, body))
        | [] -> error $1 "method has no receiver"
        | [ParamEllipsis _] -> error $1 "method has ... for receiver"
        | [ParamMetavarEllipsis _] -> error $1 "method has metavar ellipsis for receiver"
        | _::_::_ -> error $1 "method has multiple receivers"
    }

fnbody:
|  (*empty *)          { Empty }
|  "{" listsc(stmt) "}" { Block ($1, rev_and_fix_stmts $2, $3) }


fnliteral: fnlitdcl lbrace listsc(stmt) "}"
    { FuncLit ($1, Block ($2, rev_and_fix_stmts $3, $4)) }

fnlitdcl: fntype { $1 }

arg_type:
|       name_or_type {
    (match $1 with
    (* This means a param of the form $...<ID>.
     * Ordinarily, in Golang, a singular identifier is interpreted as an
     * anonymous argument of the type of that identifier.
     * So func foo(int) is allowed.

     * When we see a metavariable ellipsis, it's a little silly to consider that
     * as a type. If someone writes func foo ($...ARGS), they probably just meant to
     * capture all of the args of the function, and not an anonymous argument of type $...ARGS.

     * So we parse it as such here.
     *)
    | TName [ (s, _) as id ] when AST_generic.is_metavar_ellipsis s ->
        Flag_parsing.sgrep_guard (ParamMetavarEllipsis id)
    | __else__ ->
     ParamClassic { pname= None; ptype = $1; pdots = None }
     )
    }
|   sym name_or_type { ParamClassic { pname= Some $1; ptype = $2; pdots = None } }
|   sym dotdotdot    { ParamClassic { pname= Some $1; ptype = snd $2; pdots = Some (fst $2)}}
|       dotdotdot    { ParamClassic { pname= None; ptype = snd $1; pdots = Some (fst $1)} }
 (* sgrep-ext: *)
 | "..." { Flag_parsing.sgrep_guard (ParamEllipsis $1) }


name_or_type:  ntype { $1 }

arg_type_list:
|   arg_type                      { [$1] }
|   arg_type_list "," arg_type { $3::$1 }

oarg_type_list_ocomma:
|(*empty*)  { [] }
|   arg_type_list ","? { adjust_signatures $1  }

(*************************************************************************)
(* xxx_list *)
(*************************************************************************)

(*
 * lists of things
 * note that they are left recursive
 * to conserve yacc stack. they need to
 * be reversed to interpret correctly
 *)

(* basic lists, 0 element allowed *)
elseif_list:
| (*empty*)      { [] }
| elseif_list elseif { $2::$1 }


(* lists with ending ";", 0 element allowed *)
xdcl_list:
| (*empty*)    { [] }
|   xdcl_list xdcl ";" { $2 @ $1 }

(* note that this does not require List.rev in the caller! *)
imports:
| (* empty *)        { [] }
| imports import ";" { $1 @ $2 }

(* lists with ";" separator, at least 1 element, usually followed
 * by trailing ; which currently cause s/r conflict when trying
 * to use the listsc() macro. Maybe have to do like in parser_ml.mly
 * to a right recursive rules intead of left-recursive.
 *)
constdcl1_list:
|   constdcl1 { $1 }
|   constdcl1_list ";" constdcl1 { $3 @ $1 }


(*
 * optional things
 *)
oexpr_list:
| (*empty*)    { None }
| listc(expr)  { Some (List.rev $1) }

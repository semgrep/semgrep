%{
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)
open AST_ocaml
open Either_

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* This file contains a grammar for OCaml
 * (mostly OCaml 3.07 with some extensions until OCaml 4.08)
 *
 * src: adapted from the official source of OCaml in its
 * parsing/ subdirectory. All semantic actions are new. Only the
 * grammar structure was copied.
 * was: $Id: parser.mly 10536 2010-06-07 15:32:32Z doligez $
 *
 * reference:
 * - http://caml.inria.fr/pub/docs/manual-ocaml/language.html
 *   (note that it unfortunately contains conflicts when translated into yacc).
 *
 * other sources:
 * - http://www.cs.ru.nl/~tews/htmlman-3.10/full-grammar.html
 *   itself derived from the official ocaml reference manual
 *   (also contains conflicts when translated into yacc).
 * - http://www.mpi-sws.org/~rossberg/sml.html
 *   (also contains conflicts when translated into yacc).
 * - http://www.mpi-sws.org/~rossberg/hamlet/
 *   solves ambiguities
 * - linear-ML parser
 * - tree-sitter-ocaml parser
 *)

(*************************************************************************)
(* Helpers *)
(*************************************************************************)
let (qufix: name -> Tok.t -> ident -> name) =
 fun longname _dottok ident ->
  match longname with
  | xs, ident2 -> xs @ [ident2], ident

let optlist_to_list = function
  | None -> []
  | Some xs -> xs


let seq1 = function
  | [x] -> x
  | xs -> Sequence (Tok.unsafe_fake_bracket xs)

let topseqexpr v1 = mki (TopExpr (seq1 v1))

%}
(*************************************************************************)
(* Tokens *)
(*************************************************************************)

(* unrecognized token, will generate parse error *)
%token <Tok.t> TUnknown
%token <Tok.t> EOF

(*-----------------------------------------*)
(* The space/comment tokens *)
(*-----------------------------------------*)

(* coupling: Token_helpers.is_real_comment *)
%token <Tok.t> TCommentSpace TCommentNewline   TComment
%token <Tok.t> TCommentMisc

(*-----------------------------------------*)
(* The normal tokens *)
(*-----------------------------------------*)

(* tokens with "values" *)
%token <Parsed_int.t> TInt
%token <float option * Tok.t> TFloat
%token <string * Tok.t> TChar TString
%token <string * Tok.t> TLowerIdent TUpperIdent
%token <string * Tok.t> TLabelUse TLabelDecl TOptLabelUse TOptLabelDecl

(* keywords tokens *)
%token <Tok.t>
 Tfun Tfunction Trec Ttype Tof Tif Tthen Telse
 Tmatch Twith Twhen
 Tlet Tin Tas
 Ttry Texception
 Tbegin Tend Tfor Tdo Tdone Tdownto Twhile Tto
 Tval Texternal
 Ttrue Tfalse
 Tmodule Topen Tfunctor Tinclude Tsig Tstruct
 Tclass Tnew Tinherit Tconstraint Tinitializer Tmethod Tobject Tprivate
 Tvirtual
 Tlazy Tmutable Tassert
 Tand
 Tor Tmod Tlor Tlsl Tlsr Tlxor Tasr Tland

(* syntax *)
%token <Tok.t>
 TOParen "(" TCParen ")" TOBrace "{" TCBrace "}" TOBracket "[" TCBracket "]"
 TOBracketPipe "[|" TPipeCBracket "|]"  TOBracketLess "[<" TGreaterCBracket ">]"
 TOBraceLess "{<" TGreaterCBrace ">}"
 TOBracketGreater "[>" TColonGreater ":>"
 TDot "." TDotDot ".."
 TComma "," TEq "=" TAssign ":=" TAssignMutable "<-"
 TColon ":" TColonColon "::"
 TBang "!" TBangEq "!=" TTilde "~" TPipe "|"
 TSemiColon ";" TSemiColonSemiColon ";;"
 TQuestion "?" TQuestionQuestion "??"
 TUnderscore "_" TStar "*" TArrow "->" TQuote "'" TBackQuote "`"
 TAnd TAndAnd
 TSharp "#"
 TMinusDot TPlusDot

(* operators *)
%token <Tok.t> TPlus "+" TMinus "-" TLess "<" TGreater ">"
%token <string * Tok.t> TPrefixOperator TInfixOperator
%token <string * Tok.t> LETOP ANDOP (* monadic let, since 4.08 *)

(* attributes *)
%token <Tok.t> TBracketAt "[@" TBracketAtAt "[@@" TBracketAtAtAt "[@@@"
%token <Tok.t> TBracketPercent "[%" TBracketPercentPercent "[%%"

(*-----------------------------------------*)
(* extra tokens: *)
(*-----------------------------------------*)
%token <Tok.t> TSharpDirective

(* sgrep-ext: *)
%token <Tok.t> TDots "..." LDots "<..." RDots "...>"

(*************************************************************************)
(* Priorities *)
(*************************************************************************)
(* Precedences and associativities.
 *
 * Tokens and rules have precedences.  A reduce/reduce conflict is resolved
 * in favor of the first rule (in source file order).  A shift/reduce conflict
 * is resolved by comparing the precedence and associativity of the token to
 * be shifted with those of the rule to be reduced.
 *
 * By default, a rule has the precedence of its rightmost terminal (if any).
 *
 * When there is a shift/reduce conflict between a rule and a token that
 * have the same precedence, it is resolved using the associativity:
 * if the token is left-associative, the parser will reduce; if
 * right-associative, the parser will shift; if non-associative,
 * the parser will declare a syntax error.
 *
 * We will only use associativities with operators of the kind  x * x -> x
 * for example, in the rules of the form    expr: expr BINOP expr
 * in all other cases, we define two precedences if needed to resolve
 * conflicts.
 *
 * The precedences must be listed from low to high.
 *)

%nonassoc below_SEMI
%nonassoc TSemiColon                     (* below TEq ({lbl=...; lbl=...}) *)
%nonassoc Tlet                           (* above TSemiColon ( ...; let ... in ...) *)
%nonassoc below_WITH
%nonassoc Tfunction Twith                 (* below TPipe  (match ... with ...) *)
%nonassoc Tthen                          (* below Telse (if ... then ...) *)
%nonassoc Telse                          (* (if ... then ... else ...) *)
%nonassoc TAssignMutable                 (* below TAssign (lbl <- x := e) *)
%right    TAssign                        (* expr (e := e := e) *)
%nonassoc Tas
%left     TPipe                          (* pattern (p|p|p) *)
%nonassoc below_COMMA
%left     TComma                         (* expr/expr_comma_list (e,e,e) *)
%right    TArrow                         (* core_type (t -> t -> t) *)

%right    Tor                            (* expr (e || e || e) *)
%right    TAnd TAndAnd                   (* expr (e && e && e) *)
%nonassoc below_EQUAL
%left     TEq TLess TGreater    (* expr (e OP e OP e) *)
%left     TBangEq
%right    TColonColon                    (* expr (e :: e :: e) *)
%left     TPlus TPlusDot TMinus TMinusDot  (* expr (e OP e OP e) *)
%left     TStar                 (* expr (e OP e OP e) *)
%left     TInfixOperator (* pad: *)
%left     Tmod Tlor Tlxor Tland
%right    Tlsr Tasr Tlsl

%nonassoc prec_unary_minus prec_unary_plus (* unary - *)
%nonassoc prec_constant_constructor      (* cf. simple_expr (C versus C x) *)
%nonassoc prec_constr_appl               (* above Tas TPipe TColonColon TComma *)
%nonassoc TSharp                         (* simple_expr/toplevel_directive *)
%nonassoc below_DOT
%nonassoc TDot
(* Finally, the first tokens of simple_expr are above everything else. *)
%nonassoc TBackQuote TBang Tbegin TChar Tfalse TFloat TInt
          TOBrace TOBraceLess TOBracket TOBracketPipe TLowerIdent TOParen
          Tnew TPrefixOperator TString Ttrue TUpperIdent
          TDots LDots

(*************************************************************************)
(* Rules type declaration *)
(*************************************************************************)
%start <AST_ocaml.item list> interface
%start <AST_ocaml.item list> implementation
%start <AST_ocaml.any> semgrep_pattern
%start <AST_ocaml.type_> type_for_lsp

%%
(*************************************************************************)
(* Macros *)
(*************************************************************************)
(* todo? optimize to avoid being quadratic with @? do List.rev internally? *)
list_sep(X,Sep):
 | X                      { [$1] }
 | list_sep(X,Sep) Sep X  { $1 @ [$3] }

(* does not work
list_sep2(X,Sep):
 | X                      { [Left $1] }
 | X Sep                  { [Left $1; Right $2] }
 | list_sep2(X,Sep) Sep X  { $1 @ [Right $2; Left $3] }
*)

listr_sep(X,Sep):
 | X                       { [$1] }
 | X Sep listr_sep(X,Sep)  { $1 :: $3 }

(* list separated by Sep and possibly terminated by trailing Sep.
 * This has to be recursive on the right, otherwise s/r conflict.
 *)
list_sep_term(X,Sep):
 | X                       { [$1] }
 | X Sep                   { [$1] }
 | X Sep list_sep_term(X,Sep)  { $1 :: $3 }

list_and(X): list_sep(X, Tand) { $1 }

qualified(X, Y):
 | Y       { [], $1 }
 | X "." Y { qufix $1 $2 $3 }

(*************************************************************************)
(* TOC *)
(*************************************************************************)
(* - toplevel
 * - signature
 * - structure
 * - names
 *
 * - expression
 * - type
 * - pattern
 * with for the last 3 sections subsections around values:
 *    - constants
 *    - constructors
 *    - lists
 *    - records
 *    - tuples
 *    - arrays
 *    - name tags (`Foo)
 *
 * - let/fun
 * - classes (not in AST)
 * - modules
 * - attributes
 *)

(*************************************************************************)
(* Toplevel, compilation units *)
(*************************************************************************)

interface:      signature EOF              { $1 }
implementation: structure EOF              { $1 }

(* this is the entry point used by semgrep to parse patterns *)
semgrep_pattern:
 | expr                                EOF { E $1 }
 | signature_or_structure_common post_item_attribute* EOF
     { I { i = $1; iattrs = $2 } }
 | just_in_signature post_item_attribute*  EOF
     { I { i = $1; iattrs = $2 } }
 | just_in_structure post_item_attribute*  EOF
     { I { i = $1; iattrs = $2 } }
 | ":" core_type_no_attr               EOF { T $2 }
 | "|" pattern                         EOF { P $2 }
 | "|" pattern match_action            EOF { MC ($2, $3) }
 (* partial *)
 | partial EOF { Partial $1 }

(* this is used by semgrep -lsp to parse the types returned by ocamllsp *)
type_for_lsp: core_type_no_attr EOF { $1 }

signature_or_structure_common:
 | Ttype list_and(type_declaration)
     { Type ($1, $2) }
 | Texternal val_ident ":" core_type_no_attr "=" primitive_declaration
     { External ($1, $2, $4, $6) }
 | Texception TUpperIdent generalized_constructor_arguments
     { Exception ($1, $2, $3) }
 | Topen "!"? mod_longident
     { Open ($1, $3) }
 | Tmodule Ttype ident "=" module_type
     { ItemTodo (("ModuleType", $1), [$5]) }
 | Tclass Ttype list_and(class_type_declaration)
     { ItemTodo (("ClassType", $1), $3) }

partial:
 | Tif expr    { PartialIf ($1, $2) }
 | Tmatch expr { PartialMatch ($1, $2) }
 | Ttry expr   { PartialTry ($1, $2) }
 (* conflicts: I would like to just write 'Tlet let_binding in' but then we
  * get s/r conflicts, so I have to be more general like in the 'expr:'
  * and 'structure:' grammar rules.
  *
  * Note that semgrep_pattern allows also to search for 'let x = $BODY'
  * because this is a valid structure item, and this will actually also match
  * LetIn expressions because of the way LetIn are converted in
  * ml_to_generic.ml in a sequence of VarDef and ExprStmt.
  * So this PartialLetIn is a bit superficial, but maybe one day
  * those partial will be restricted to match only LetIn (local vars).
  *)
 | Tlet Trec? list_and(let_binding) Tin  { PartialLetIn ($1, $2, $3, $4 ) }

(*************************************************************************)
(* Signature *)
(*************************************************************************)

signature:
 | (* empty *)                   { [] }
 | signature signature_item      { $1 @ [$2] }
 | signature signature_item ";;" { $1 @ [$2] }

signature_item: signature_item_noattr post_item_attribute*
  { { i = $1; iattrs = $2 } }

signature_item_noattr:
 | signature_or_structure_common { $1 }
 | just_in_signature { $1 }

just_in_signature:
 | Tval val_ident ":" core_type_no_attr                { Val ($1, $2, $4) }
 (* modules *)
 | Tmodule TUpperIdent module_declaration
    { ItemTodo (("ModuleDecl", $1), [$3]) }
 (* objects *)
 | Tclass list_and(class_description)
   { ItemTodo (("Class",$1), $2)  }

(*----------------------------*)
(* Misc *)
(*----------------------------*)

primitive_declaration: TString+ { $1 }

(*************************************************************************)
(* Structure *)
(*************************************************************************)

(* pad: should not allow those toplevel seq_expr *)
structure:
 |          structure_tail                     { $1 }
 | seq_expr structure_tail                     { topseqexpr $1::$2 }

structure_tail:
 | (* empty *)                             { [] }
 | ";;"                                    { [] }
 | ";;" seq_expr structure_tail            { topseqexpr $2::$3 }
 | ";;" structure_item structure_tail      { $2::$3 }
 | ";;" TSharpDirective  structure_tail    { $3 }

 | structure_item  structure_tail          { $1::$2 }
 | TSharpDirective structure_tail          { $2 }

structure_item: structure_item_noattr post_item_attribute*
  { { i = $1; iattrs = $2 } }

structure_item_noattr:
 | signature_or_structure_common { $1 }
 | just_in_structure { $1 }

just_in_structure:
 | Tlet Trec? list_and(let_binding)              { Let ($1, $2, $3) }
 | Texception TUpperIdent "=" mod_longident
    { ItemTodo (("ExnAlias",$1), []) }

 (* modules *)
 | Tmodule TUpperIdent module_binding
      { match $3 with
        | None -> ItemTodo (("AbstractModule?", $1), [])
        | Some (_x, y) -> Module ($1, { mname = $2; mbody = y })
      }
 | Tinclude module_expr
   { ItemTodo (("Include",$1), [] (* TODOAST $2 *)) }

 (* objects *)
 | Tclass list_and(class_declaration)
    { ItemTodo (("Class",$1), $2)  }

 | floating_attribute { $1 }

(*************************************************************************)
(* Names *)
(*************************************************************************)

val_ident:
 | TLowerIdent                        { $1 }
 | "(" operator ")"                   { ("TODOOPERATOR", $1) }

operator:
 | TPrefixOperator      { }
 | TInfixOperator       { }
 | "*"     { } | "="       { } | ":="   { } | "!"     { }
  (* but not Tand, because of conflict ? *)
 | Tor       { } | TAnd      { }
 | Tmod      { } | Tland     { } | Tlor      { } | Tlxor     { }
 | Tlsl      { } | Tlsr      { } | Tasr      { }
 | "+"     { } | TPlusDot  { } | "-"    { } | TMinusDot { }
 | "<"     { } | ">"  { }
 | TAndAnd { } | TBangEq { }
 | LETOP { } | ANDOP { }

(* for polymorphic types both 'a and 'A is valid. Same for module types. *)
ident:
 | TUpperIdent                                      { $1 }
 | TLowerIdent                                      { $1 }

(* this is used in a constructor declaration context; I guess in some
 * prelude files false and true are defined as type bool = false | true
 *)
constr_ident:
 | TUpperIdent     { $1 }
 | "(" ")"         { "()", $1 }
 | "::"            { "::", $1 }
 | Tfalse          { "false", $1 }
 | Ttrue           { "true", $1 }
(*  | "[" "]"                           { } *)
(*  | "(" "::" ")"                    { "::" } *)

(* record field name (not olabl label) *)
label: TLowerIdent  { $1 }

(* name tag extension (polymorphic variant) *)
name_tag: "`" ident   { $1, $2 }

(*----------------------------*)
(* Labels (olabl labels) *)
(*----------------------------*)

label_var: TLowerIdent    { }

(* for label arguments like ~x or ?x *)
label_ident: TLowerIdent   { $1 }

(*----------------------------*)
(* Qualified names *)
(*----------------------------*)

mod_longident:
 | TUpperIdent                      { [], $1 }
 | mod_longident "." TUpperIdent    { qufix $1 $2 $3 }

mod_ext_longident:
 | TUpperIdent                                 { [], $1 }
 | mod_ext_longident "." TUpperIdent           { qufix $1 $2 $3 }
 (* TODOAST *)
 | mod_ext_longident "(" mod_ext_longident ")" { [], ("TODOEXTMO", $2) }


constr_longident:
 | mod_longident   %prec below_DOT     { Left $1 }
 | "[" "]"                             { Left ([], ("[]", $1)) }
 | "(" ")"                             { Right (Unit ($1, $2)) }
 | Tfalse                              { Right (Bool (false, $1)) }
 | Ttrue                               { Right (Bool (true, $1)) }

type_longident: qualified(mod_ext_longident, TLowerIdent) { $1 }
val_longident:  qualified(mod_longident, val_ident) { $1 }
(* record field name *)
label_longident: qualified(mod_longident, TLowerIdent) { $1 }
class_longident: qualified(mod_longident, TLowerIdent) { $1 }
mty_longident:   qualified(mod_ext_longident, ident) { $1 }
(* it's mod_ext_longident, not mod_longident *)
clty_longident: qualified(mod_ext_longident, TLowerIdent) { $1 }

(*************************************************************************)
(* Expressions *)
(*************************************************************************)

seq_expr:
 | expr      %prec below_SEMI   { [$1] }
 | expr ";" seq_expr            { $1::$3 }
 (* bad? should be removed ? but it's convenient in certain contexts like
  * begin end to allow ; as a terminator *)
 | expr ";"                     { [$1] }


expr:
 | simple_expr                               { $1 }
 (* function application *)
 | simple_expr labeled_simple_expr+          { Call ($1, $2) }

 | Tlet Trec? list_and(let_binding) Tin seq_expr  { LetIn ($1,$2,$3,seq1 $5)}

 | Tfun labeled_simple_pattern fun_def
     { let (params, (_tok, e)) = $3 in
       Fun ($1, $2::params, e) }

 (* TODOAST: (type a) is ignored for now *)
 | Tfun "(" abstract_type ")" fun_def
     { let (params, (_tok, e)) = $5 in
       Fun ($1, params, e) }

 | Tfunction "|"? match_cases                { Function ($1, $3) }

 | expr_comma_list        %prec below_COMMA  { Tuple ($1) }
 | constr_longident simple_expr
     { match $1 with
       | Left x -> Constructor (x, Some $2)
       | Right _ -> failwith "Impossible, literal with constructor argument"
     }

 | expr "::" expr            { Infix ($1, ("::", $2), $3)}

 | expr TInfixOperator expr  { Infix ($1, $2, $3) }

 | expr Tmod expr            { Infix ($1, ("mod", $2), $3) }
 | expr Tland expr           { Infix ($1, ("land", $2), $3) }
 | expr Tlor expr            { Infix ($1, ("lor", $2), $3) }
 | expr Tlxor expr           { Infix ($1, ("lxor", $2), $3) }

 | expr Tlsl expr            { Infix ($1, ("lsl", $2), $3) }
 | expr Tlsr expr            { Infix ($1, ("lsr", $2), $3) }
 | expr Tasr expr            { Infix ($1, ("asr", $2), $3) }

 | expr TBangEq expr         { Infix ($1, ("!=", $2), $3) }

 | Tif seq_expr Tthen expr Telse expr   { If ($1, seq1 $2, $4, Some ($6)) }
 | Tif seq_expr Tthen expr              { If ($1, seq1 $2, $4, None) }

 | Tmatch seq_expr Twith "|"? match_cases   { Match ($1, seq1 $2, $5) }

 | Ttry seq_expr Twith "|"? match_cases     { Try ($1, seq1 $2, $5) }

 | Twhile seq_expr Tdo seq_expr Tdone       { While ($1, seq1 $2, seq1 $4) }
 | Tfor val_ident "=" seq_expr direction_flag seq_expr Tdo seq_expr Tdone
     { For ($1, $2, seq1 $4, $5, seq1 $6, seq1 $8)  }

 | expr ":=" expr { RefAssign ($1, $2, $3) }

 | expr "=" expr   { Infix ($1, ("=", $2), $3) }

 | expr "+" expr     { Infix ($1, ("+", $2), $3)  }
 | expr "-" expr    { Infix ($1, ("-", $2), $3) }
 | expr TPlusDot expr  { Infix ($1, ("+.", $2), $3) }
 | expr TMinusDot expr { Infix ($1, ("-.", $2), $3) }
 | expr "*" expr        { Infix ($1, ("*", $2), $3) }
 | expr "<" expr     { Infix ($1, ("<", $2), $3) }
 | expr ">" expr  { Infix ($1, (">", $2), $3) }
 | expr Tor expr       { Infix ($1, ("or", $2), $3) }
 | expr TAnd expr      { Infix ($1, ("&", $2), $3) }
 | expr TAndAnd expr   { Infix ($1, ("&&", $2), $3) }

 | subtractive expr   %prec prec_unary_minus    { Prefix ($1, $2) }
 | additive expr      %prec prec_unary_plus     { Prefix ($1, $2) }

 | simple_expr "." label_longident "<-" expr  { FieldAssign ($1,$2,$3,$4,$5) }

 (* extensions *)

 (* array extension *)
 | simple_expr "." "(" seq_expr ")" "<-" expr
     { ExprTodo (("Array",$2), [$1] @ $4 @ [$7]) }
 | simple_expr "." "[" seq_expr "]" "<-" expr
     { ExprTodo (("Array",$2), [$1] @ $4 @ [$7]) }
 (* bigarray extension, a.{i} <- v *)
 | simple_expr "." "{" expr "}" "<-" expr
     { ExprTodo (("BigArray",$2), [$1;$4;$7]) }

 (* local open *)
 | Tlet Topen mod_longident Tin seq_expr
     { ExprTodo (("LocalOpen",$1), $5) }
 | Tlet Tmodule TUpperIdent module_binding Tin seq_expr
     { ExprTodo (("LocalModule", $1), $6) }

 (* TODO: very partial support for monadic let *)
 | LETOP list_and(let_binding) Tin seq_expr { LetIn (snd $1, None, $2,seq1 $4)}

 | Tassert simple_expr
     { ExprTodo (("Assert",$1), [$2]) }
 | name_tag simple_expr
     { PolyVariant ($1, Some $2) }
 | Tlazy simple_expr
     { ExprTodo (("Lazy",$1), [$2]) }

  (* objects *)
 | label "<-" expr
     { ExprTodo (("ObjUpdate",$2), [$3]) }
 | object_expression { $1 }



simple_expr:
 | constant          { L $1 }
 | val_longident     { Name $1 }
 (* this includes 'false' *)
 | constr_longident      %prec prec_constant_constructor
    { match $1 with
      | Left x -> Constructor (x, None)
      | Right lit -> L lit
    }

 | simple_expr "." label_longident  { FieldAccess ($1, $2, $3) }

 (* if only one expr then prefer to generate a ParenExpr *)
 | "(" seq_expr ")"
     { match $2 with
     | [] -> Sequence ($1, [], $3)
     (* Ocaml_to_generic will do the right thing if x is a tuple or
      * if this expression is part of a Constructor call.
      *)
     | [x] -> ParenExpr ($1, x, $3)
     | _ -> Sequence ($1, $2, $3)
     }

 | Tbegin seq_expr Tend     { Sequence ($1, $2, $3)  }
 | Tbegin Tend              { Sequence ($1, [], $2) }

 (* bugfix: must be in simple_expr. Originally made the mistake to put it
  * in expr: and the parser would then not recognize things like 'foo !x' *)
 | TPrefixOperator simple_expr   { Prefix ($1, $2) }
 | "!" simple_expr               { RefAccess ($1, $2) }

 | "{" record_expr               "}"  { Record (fst $2, ($1, snd $2, $3)) }
 | "[" list_sep_term(expr, ";") "]"   { List ($1, $2, $3) }

 (* extensions *)

 (* array extension *)
 | "[|" list_sep_term(expr, ";")? "|]"
    { ExprTodo (("ArrayLiteral",$1), optlist_to_list $2)  }
 | simple_expr "." "(" seq_expr ")"
    { ExprTodo (("Array", $2), [$1] @ $4) }
 | simple_expr "." "[" seq_expr "]"
    { ExprTodo (("Array", $2), [$1] @ $4) }
 (* bigarray extension *)
 | simple_expr "." "{" expr "}"
    { ExprTodo (("BigArray", $2), [$1;$4]) }

 (* object extension *)
 | simple_expr "#" label             { ObjAccess ($1, $2, $3) }
 | Tnew class_longident              { New ($1, $2) }
 | "{<" list_sep_term(field_expr, ";")? ">}"
      { ExprTodo (("LiteralObj", $1), $2 |> optlist_to_list |> List.map snd) }
 (* name tag extension *)
 | name_tag        %prec prec_constant_constructor
     { PolyVariant ($1, None) }
 (* misc *)
 | "(" seq_expr type_constraint ")"
     { TypedExpr (seq1 $2, fst $3, snd $3)  }
 (* scoped open, 3.12 *)
 | mod_longident "." "(" seq_expr ")"
     { ExprTodo (("LocalOpen", $2), $4) }
 (* sgrep-ext: *)
 | "..."              { Ellipsis $1 }
 | "<..." expr "...>" { DeepEllipsis ($1, $2, $3) }


labeled_simple_expr:
 | simple_expr     { Arg $1 }
 | label_expr      { $1 }

(* a bit different than list_sep() *)
expr_comma_list:
 | expr_comma_list "," expr                  { $1 @ [$3] }
 | expr "," expr                             { [$1; $3] }


record_expr:
 |                   list_sep_term(lbl_expr, ";")  { (None, $1) }
 | simple_expr Twith list_sep_term(lbl_expr, ";")  { (Some $1, $3) }

lbl_expr:
 | label_longident "=" expr { ($1, $3) }
 (* new 3.12 feature! *)
 | label_longident          { ($1, name_of_id (snd $1)) }

additive:
  | "+"                                        { "+", $1 }
  | TPlusDot                                     { "+.", $1 }

subtractive:
  | "-"                                       { "-", $1 }
  | TMinusDot                                    { "-.", $1 }

direction_flag:
 | Tto                                          { To $1 }
 | Tdownto                                      { Downto $1 }

(*----------------------------*)
(* Constants *)
(*----------------------------*)

constant:
 | TInt     { Int $1 }
 | TChar    { Char $1 }
 | TString  { String $1 }
 | TFloat   { Float $1 }

(*----------------------------*)
(* Labels *)
(*----------------------------*)

label_expr:
 (* ~xxx: expr *)
 | TLabelDecl simple_expr
    { ArgKwd ($1 (* TODO del ~/:? *), $2) }
 (* ?xxx: expr *)
 | TOptLabelDecl simple_expr
    { ArgQuestion ($1, $2) }
 | "~" label_ident
    { ArgKwd ($2, name_of_id $2) }
 | "?" label_ident
    { ArgQuestion ($2, name_of_id $2) }

(*----------------------------*)
(* objects *)
(*----------------------------*)

field_expr: label "=" expr { $1, $3 }

(*************************************************************************)
(* Patterns *)
(*************************************************************************)

match_case: pattern match_action { ($1, $2) }

(* cant factorize with list_sep, or listr_sep *)
match_cases:
 | match_case                   { [$1] }
 | match_cases "|" match_case   { $1 @ [$3] }

match_action:
 |                "->" seq_expr   { None, $1, seq1 $2 }
 | Twhen seq_expr "->" seq_expr   { Some (seq1 $2), $3, seq1 $4 }


pattern:
 | simple_pattern   { $1 }

 | constr_longident pattern %prec prec_constr_appl
     { match $1 with
       | Left x -> PatConstructor (x, Some $2)
       | Right _lit -> failwith "Impossible, literal with pattern argument"
     }
 (* the fake will be replace by real parens in simple_pattern last case *)
 | pattern_comma_list       %prec below_COMMA     { PatTuple (Tok.unsafe_fake_bracket $1) }
 | pattern "::" pattern                           { PatConsInfix ($1, $2, $3) }

 | pattern Tas val_ident                          { PatAs ($1, $3) }

 (* nested patterns *)
 | pattern "|" pattern                            { PatDisj ($1, $3) }

 (* extensions *)

 (* name tag extension *)
 | name_tag pattern %prec prec_constr_appl
    { PatPolyVariant ($1, Some $2) }
 | Tlazy simple_pattern { PatTodo(("Lazy", $1), [$2]) }


simple_pattern:
 | val_ident %prec below_EQUAL      { PatVar ($1) }
 | constr_longident
     { match $1 with
       | Either.Left x -> PatConstructor (x, None)
       | Either.Right lit -> PatLiteral lit
     }

 | "_"                              { PatUnderscore $1 }
 | signed_constant                  { PatLiteral $1 }

 | "{" lbl_pattern_list record_pattern_end "}" { PatRecord ($1,$2,(*$3*) $4) }
 | "["  list_sep_term(pattern, ";")  "]"       { PatList ($1, $2, $3) }
 | "[|" list_sep_term(pattern, ";")? "|]"
    { PatTodo (("ArrayLiteral",$1), optlist_to_list $2) }

 (* note that let (x:...) a =  will trigger this rule *)
 | "(" pattern ":" core_type_no_attr ")"               { PatTyped ($2, $3, $4)}

 (* extensions *)
 (* name tag extension *)
 | name_tag                    { PatPolyVariant ($1, None) }
 (* range extension *)
 | TChar ".." TChar            { PatTodo (("Range", $2), []) }
 (* scoped open for pattern *)
 | mod_longident "." "(" pattern ")"
     { PatTodo (("LocalOpen", $2), [$4]) }
 (* first-class modules *)
 | "(" Tmodule TUpperIdent ":" module_type ")"
     { PatTodo (("Module", $2), []) }
 (* sgrep-ext: *)
 | "..."              { PatEllipsis $1 }

 | "(" pattern ")"
     { match $2 with
       | PatTuple (_, xs, _) -> PatTuple ($1, xs, $3)
       | p -> p
     }

lbl_pattern:
 | label_longident "=" pattern               { ($1, $3) }
 | label_longident                           { ($1, PatVar (snd $1)) }

(* cant factorize with list_sep or list_sep_term *)
lbl_pattern_list:
 | lbl_pattern { [$1] }
 | lbl_pattern_list ";" lbl_pattern { $1 @ [$3] }

record_pattern_end:
 | ";"?                      { }
 (* new 3.12 feature! *)
 | ";" "_" ";"?              { }

(* not exactly like list_sep() *)
pattern_comma_list:
 | pattern_comma_list "," pattern            { $1 @ [$3] }
 | pattern "," pattern                       { [$1; $3] }


signed_constant:
 | constant       { $1 }
 (* TODO should use -/+, use PI.rewrap_str? *)
 | "-" TInt    { Int $2 }
 | "-" TFloat  { Float $2 }
 | "+" TInt     { Int $2 }
 | "+" TFloat   { Float $2 }

(*************************************************************************)
(* Types *)
(*************************************************************************)

type_constraint:
 | ":" poly_type_no_attr     { $1, $2 }
 (* object cast extension *)
 | ":>" core_type_no_attr    { $1, $2 }

(*----------------------------*)
(* Types definitions *)
(*----------------------------*)

type_declaration: type_parameters TLowerIdent type_kind (*TODOAST constraints*)
   { match $3 with
     | None ->
         TyDecl { tname = $2; tparams = $1; tbody = AbstractType }
     | Some (_tok_eq, type_kind) ->
         TyDecl { tname = $2; tparams = $1; tbody = type_kind }
   }


type_kind:
 | (*empty*)
      { None }
 | "=" core_type
      { Some ($1, CoreType $2) }
 | "=" ioption(Tprivate) ioption("|") list_sep(constructor_declaration, "|")
      { Some ($1, AlgebraicType $4) }
 | "=" ioption(Tprivate) "{" label_declarations "}"
      { Some ($1, RecordType ($3, ($4), $5)) }


constructor_declaration: constr_ident generalized_constructor_arguments
  { $1, $2 }

generalized_constructor_arguments:
 | (*empty*)                                { [] }
 | Tof constructor_arguments      { $2 }

constructor_arguments:
 | list_sep(simple_core_type, "*") { $1 }
 | "{" label_declarations "}" { [ TyTodo(("InlineRecord", $1), []) ] }

type_parameters:
 |  (*empty*)                          { None  }
 | type_parameter                      { Some (Tok.unsafe_fake_bracket [$1]) }
 | "(" list_sep(type_parameter, ",") ")" { Some ($1, $2, $3) }

type_parameter: ioption(type_variance) "'" ident   { TyParam $3 }

(* old: list_sep_term(label_declaration, ";") but accept attr after ; *)
label_declarations:
 | label_declaration { [$1] }
 | label_declaration ";" attribute* { [$1] }
 | label_declaration ";" attribute* label_declarations { $1 :: $4 }

label_declaration: Tmutable? label ":" poly_type_no_attr attribute*
   { $2, $4, $1 }

(*----------------------------*)
(* Types expressions *)
(*----------------------------*)

(* from the 4.10 grammar:
   Atomic types are the most basic level in the syntax of types.
   Atomic types include:
   - types between parentheses:           (int -> int)
   - first-class module types:            (module S)
   - type variables:                      'a
   - applications of type constructors:   int, int list, int option list
   - variant types:                       [`A]
  TODO: used only in fun_def for now, but should be used at more places
 *)
atomic_type: simple_core_type_or_tuple { $1 }

core_type: core_type_no_attr attribute* { $1 }

core_type_no_attr:
 | simple_core_type_or_tuple
     { $1 }
 | core_type_no_attr "->" core_type_no_attr
     { TyFunction ($1, $3) }

 (* ext: olabl *)
 | TLowerIdent     ":" core_type_no_attr "->" core_type_no_attr
     { TyFunction ($3, $5) (* TODOAST $1 $2 *)  }
 | "?" TLowerIdent ":" core_type_no_attr "->" core_type_no_attr
     { TyFunction ($4, $6) (* TODOAST $1 $2 *)  }
 (* pad: only because of lexer hack around labels *)
 | TOptLabelDecl    core_type_no_attr "->" core_type_no_attr
     { TyFunction ($2, $4) (* TODOAST $1 $2 *)  }


simple_core_type_or_tuple:
 | simple_core_type                        { $1 }
 | simple_core_type "*" list_sep(simple_core_type, "*")
     { TyTuple ($1::$3) }


simple_core_type:
 | simple_core_type2   { $1 }
 (* weird diff between 'Foo of a * b' and 'Foo of (a * b)' *)
 | "(" list_sep(core_type_no_attr, ",") ")" { TyTuple ($2) }

simple_core_type2:
 | type_variable                                 { $1 }
 | type_longident                                { TyName ($1) }
 | simple_core_type2 type_longident              { TyApp (Tok.unsafe_fake_bracket [$1], $2) }
 | "(" list_sep(core_type_no_attr, ",") ")" type_longident
      { TyApp (($1, $2, $3), $4) }

 (* name tag extension *)
 | polymorphic_variant_type                       { $1 }

 (* objects types *)
 | "<" meth_list ">"                    { TyTodo(("Methods",$1), $2) }
 | "<"           ">"                    { TyTodo(("Methods",$1), []) }
 | simple_core_type2 Tas type_variable
    { TyTodo (("As", $2), [$1;$3]) }

 (* sgrep-ext: *)
 | "..."              { TyEllipsis $1 }

type_variable:
 | "'" ident          { TyVar $2 }
 | "_"                { TyAny $1 }

polymorphic_variant_type:
 | "[" tag_field "]"
     { TyTodo(("Tag",$1), [$2])}
 | "[" row_field? "|"  list_sep(row_field,"|") "]"
     { TyTodo(("Rows|",$1),(Option.to_list $2)@ $4)}
 | "[>" "|"?  list_sep(row_field,"|") "]"
     { TyTodo(("Rows>",$1), $3)}
 | "[<" "|"?  list_sep(row_field,"|") "]"
     { TyTodo(("Rows<",$1), $3)}


(*----------------------------*)
(* Advanced types *)
(*----------------------------*)

(* TODO: same as poly_type_no_attr but use core_type in body *)
poly_type: poly_type_no_attr { $1 }

poly_type_no_attr:
 | type_parameter+ "." core_type_no_attr { TyTodo(("Poly",$2), [$3]) }
 | abstract_type "." core_type_no_attr   { TyTodo(("Poly", $2), [$3]) }
 | core_type_no_attr { $1 }

(* TODOAST *)
abstract_type: Ttype TLowerIdent+ { }

row_field:
 | tag_field                                   { $1 }
 | simple_core_type2                           { $1 }

tag_field:
 | name_tag Tof TAnd? list_and(core_type_no_attr)   { TyTodo (("Tag",fst $1),$4)}
 | name_tag                                  { TyTodo (("Tag",fst $1),[]) }

meth_list:
  | field ";" meth_list                     { $1::$3 }
  | field ";"?                              { [$1] }
  | ".."                                    { [TyTodo(("..",$1), [])] }

field: label ":" poly_type_no_attr attribute*       { TyTodo(("Field",$2), [$3]) }

type_variance:
  | "+" { }
  | "-" { }

(*************************************************************************)
(* Let/Fun definitions *)
(*************************************************************************)

let_binding:
 | val_ident fun_binding
      { let (lparams, (lrettype, _teq, body)) = $2 in
        LetClassic { lname = $1; lparams; lrettype; lbody = seq1 body; lattrs = [] } }
 | pattern "=" seq_expr
      { LetPattern ($1, seq1 $3) }


fun_binding:
 | strict_binding               { $1 }
 (* let x arg1 arg2 : t = e *)
 | type_constraint "=" seq_expr { [], (Some (snd $1), $2, $3) }

strict_binding:
 (* simple values, e.g. 'let x = 1' *)
 | "=" seq_expr  { [], (None, $1, $2) }
 (* function values, e.g. 'let x a b c = 1' *)
 | labeled_simple_pattern fun_binding { let (args, body) = $2 in $1::args,body}
 (* TODOAST *)
 | "(" abstract_type ")" fun_binding { $4 }

fun_def:
 | "->" expr                       { [], ($1, $2) }
 (* TODOAST: $2 *)
 | ":" atomic_type "->" expr       { [], ($3, $4) }
 | labeled_simple_pattern fun_def  { let (args, body) = $2 in $1::args, body }


labeled_simple_pattern:
  | simple_pattern { Param $1 }
  | label_pattern  { $1 }

opt_default:
 | (*empty*)               { None  }
 | "=" seq_expr            { Some ($1, $2) }

(*----------------------------*)
(* Labels *)
(*----------------------------*)

label_pattern:
  | "~" label_var                                 { ParamTodo ("Label", $1) }
  (* ex: let x ~foo:a *)
  | TLabelDecl simple_pattern                     { ParamTodo ("Label", snd $1) }
  | TOptLabelDecl simple_expr                     { ParamTodo ("Label", snd $1) }
  | "~" "(" label_let_pattern ")"                 { ParamTodo ("LabelParen", $1) }
  | "?" "(" label_let_pattern opt_default ")"     { ParamTodo ("LabelParen", $1) }
  | "?" label_var                                 { ParamTodo ("Label", $1) }

label_let_pattern:
 | label_var                { }
 | label_var ":" core_type_no_attr  { }

(*************************************************************************)
(* Classes *)
(*************************************************************************)

(*----------------------------*)
(* Class types *)
(*----------------------------*)
class_description: Tvirtual? class_type_parameters TLowerIdent ":" class_type
  { mki (ItemTodo (("ClassDescr", $4), [])) (* TODOAST $5 *)}

class_type_declaration:
  Tvirtual? class_type_parameters TLowerIdent "=" class_signature
  { mki (ItemTodo (("ClassTypeDecl", $4), [])) (* TODOAST $5 *) }

class_type:
  | class_signature { }
  | simple_core_type_or_tuple "->" class_type { }

class_signature:
  | actual_class_parameters  clty_longident               {  }
  | Tobject class_sig_body Tend  {  }

class_sig_body: class_self_type class_sig_fields { }

class_self_type:
  | (*empty*) {  }
  | "(" core_type_no_attr ")"  { }

class_sig_fields: class_sig_field* { }

class_sig_field:
  | Tinherit class_signature    {  }
  | virtual_method_type        {  }
  | method_type                {  }
  | Tval value_type            {  }

method_type: Tmethod Tprivate? label ":" poly_type { }

virtual_method_type:
  | Tmethod Tprivate Tvirtual label ":" poly_type    {  }
  | Tmethod Tvirtual Tprivate? label ":" poly_type   {  }

value_type:
  | Tvirtual Tmutable? label ":" core_type_no_attr     { }
  | Tmutable Tvirtual? label ":" core_type_no_attr  {  }
  | label ":" core_type_no_attr     {  }

(*----------------------------*)
(* Class expressions *)
(*----------------------------*)

(*----------------------------*)
(* Class definitions *)
(*----------------------------*)

class_declaration:
 Tvirtual? class_type_parameters TLowerIdent class_fun_binding
   { mki (ItemTodo (("ClassDecl", (snd $3)), [$4])) }

class_type_parameters:
  | (*empty*)                              { }
  | "[" list_sep(type_parameter, ",") "]"  { }

class_fun_binding:
  | class_typed? "=" class_expr
    { mki (ItemTodo (("ClassExpr", $2), [] (* TODOAST *))) }
  | labeled_simple_pattern  class_fun_binding
    { (* TODOAST $1 *) $2 }

class_typed: ":" class_type { }

class_expr:
  | class_simple_expr                         { }
  | Tfun class_fun_def                        { }
  | class_simple_expr labeled_simple_expr+    { }
  | Tlet Trec? list_and(let_binding) Tin class_expr    { }

%inline
actual_class_parameters:
 | "[" list_sep(core_type_no_attr, ",") "]"  { }
 | (* empty *) { }

class_simple_expr:
  | actual_class_parameters class_longident   { }
  | object_expression { let _ = $1 in () }
  | "(" class_expr ")"                             { }

object_expression: Tobject class_structure Tend
  { ExprTodo (("Object", $1), [](* TODOAST *)) }


class_fun_def:
  | labeled_simple_pattern "->" class_expr   { }
  | labeled_simple_pattern class_fun_def     { }

class_structure: class_self_pattern class_fields { }

class_self_pattern:
  | "(" pattern ")"                { }
  | "(" pattern ":" core_type_no_attr ")"  { }
  | (*empty*)                      { }


class_fields: class_field* { }

class_field:
  | Tinherit "!"? class_expr parent_binder   { }
  | Tval virtual_value  { }
  | Tval value          { }
  | virtual_method      { }
  | concrete_method     { }
  | Tinitializer seq_expr  { }

parent_binder:
  | Tas TLowerIdent { }
  | (* empty *) { }

virtual_value:
  | "!"? Tmutable Tvirtual label ":" core_type_no_attr  { }
  |      Tvirtual Tmutable label ":" core_type_no_attr                { }

value:
  | "!"? ioption(Tmutable) label "=" seq_expr      { }
  | "!"? ioption(Tmutable) label type_constraint "=" seq_expr  { }

virtual_method:
  | Tmethod "!"? Tprivate Tvirtual label ":" poly_type  { }
  | Tmethod "!"? Tvirtual Tprivate? label ":" poly_type { }

concrete_method:
  | Tmethod "!"? Tprivate? label strict_binding  { }
  | Tmethod "!"? Tprivate? label ":" poly_type "=" seq_expr { }

(*************************************************************************)
(* Modules *)
(*************************************************************************)

module_binding:
 |                 "=" module_expr                      { Some ($1, $2) }
(* TODOAST params and signature  *)
 | "(" TUpperIdent ":" module_type ")" module_binding  { $6 }
 | ":" module_type "=" module_expr                     { (*$1 *) Some($3, $4) }

module_declaration:
 | ":" module_type
    { $2 }
 | "(" TUpperIdent ":" module_type ")" module_declaration
     { { i = ItemTodo (("ModuleTypedDecl", $1), [$4; $6]); iattrs = [] } }

(*----------------------------*)
(* Module types *)
(*----------------------------*)

module_type: module_type_noattr { { i = $1; iattrs = [] } }

module_type_noattr:
 | mty_longident
    { let (_, (_, t)) = $1 in ItemTodo (("ModuleTypeAlias", t), []) }
 | Tsig signature Tend
    { ItemTodo (("Sig", $1), $2) }
 | Tfunctor "(" TUpperIdent ":" module_type ")" "->" module_type
    %prec below_WITH
      { ItemTodo (("FunctorType", $1), [$5; $8]) }
 | module_type Twith list_and(with_constraint)
      { ItemTodo (("ModuleTypeWith", $2), [$1]) }
 | "(" module_type_noattr ")"
      { $2 }


with_constraint:
 | Ttype type_parameters label_longident with_type_binder core_type_no_attr
    (*constraints*)
   { }

with_type_binder:
 | "="           {  }
 | "=" Tprivate  {  }

(*----------------------------*)
(* Module expressions *)
(*----------------------------*)

module_expr:
  (* when just do a module aliasing *)
  | mod_longident       { ModuleName $1 }
  (* nested modules *)
  | Tstruct structure Tend { ModuleStruct ($2) }

  (* functor definition *)
  | Tfunctor "(" TUpperIdent ":" module_type ")" "->" module_expr
     { ModuleTodo (("Functor", $1), [$8]) }
  (* module/functor application *)
  | module_expr "(" module_expr? ")"
    { ModuleTodo (("FunctorApply", $2), $1::(Option.to_list $3)) }

(*************************************************************************)
(* Attributes *)
(*************************************************************************)

(*pad: this is a limited implementation for now; just enough for efuns/pfff *)
floating_attribute:  "[@@@" attr_id payload "]"
  { ItemTodo (("Attribute", $1), $3) }

post_item_attribute: "[@@"  attr_id payload "]"
  { NamedAttr ($1, ($2, $3), $4) }
attribute:           "[@"   attr_id payload "]"
  { NamedAttr ($1, ($2, $3), $4) }


attr_id: listr_sep(single_attr_id, ".") { $1 }

payload: structure { $1  }

single_attr_id:
  | TLowerIdent { $1 }
  | TUpperIdent { $1 }
  (* should also put all keywords here, but bad practice no? *)

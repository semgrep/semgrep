(* Mike Furr
 *
 * Copyright (C) 2010 Mike Furr
 * Copyright (C) 2020 r2c
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the <organization> nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree for Ruby 1.9
 *
 * Most of the code in this file derives from code from 
 * Mike Furr in diamondback-ruby.
 *
 * less: 
 *  - [AST Format of the Whitequark parser](https://github.com/whitequark/parser/blob/master/doc/AST_FORMAT.md)
 *  - https://rubygems.org/gems/ast
 *  - new AST format in RubyVM in ruby 2.6 (see wikipedia page on Ruby)
 * 
 * history:
 *  - 2010 diamondback-ruby latest version
 *  - 2020 integrate in pfff diamondback-ruby parser, AST and IL (called cfg)
 *  - lots of small refactorings, see modif-orig.txt
 *  - lots of big refactorings when using this file to generate the AST
 *    from the tree-sitter CST for Ruby, use more precise types
 *    like method_name, method_kind, rescue_clause, scope_resolution instead
 *    of the very broad 'expr'. This will help also when converting to 
 *    the generic AST.
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type tok = Parse_info.t
 [@@deriving show] (* with tarzan *)

(* Below we derive also eq, and ord, which is unusual compared to our other
 * parsers. Indeed, we use the GLR parser generator dypgen to parse Ruby
 * and in case of ambiguities dypgen needs to _compare_ resulting ASTs
 * and filter out equivalence trees.
 *)

(* we don't care about difference in token positions *)
let compare_tok _a _b = 0
let equal_tok _a _b = true

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * tok
 [@@deriving show, eq, ord]

(* round(), square[], curly{}, angle<>, and also pipes|| brackets  *)
type 'a bracket = tok * 'a * tok
 [@@deriving show, eq, ord]

(* ------------------------------------------------------------------------- *)
(* Ident/name *)
(* ------------------------------------------------------------------------- *)
type ident = string wrap
  and uident = ident 
  and _lident = ident
 [@@deriving show, eq, ord]

(* less: Self of tok | Id of lident | Cst of uident | ...  *)
type variable = ident * id_kind 
 and id_kind = 
  | ID_Self 
  (* treesitter: *)
  | ID_Super
  | ID_Lowercase (* prefixed by [a-z] or _ *)
  (* less: rename constant *)
  | ID_Uppercase (* prefixed by [A-Z] *) (* a.k.a "constant" in Ruby *)
  | ID_Instance  (* prefixed by @ *)
  | ID_Class     (* prefixed by @@ *)
  (* pattern: \\$-?(([!@&`'+~=/\\\\,;.<>*$?:\"])|([0-9]* )|([a-zA-Z_][a-zA-Z0-9_]* ))" 
   * old: was split in 2 before with a ID_Builtin but was not in tree-sitter
   *)
  | ID_Global    (* prefixed by $ *)
 [@@deriving show { with_path = false }, eq, ord]

(* ------------------------------------------------------------------------- *)
(* Operators *)
(* ------------------------------------------------------------------------- *)
type unary_op = 
  (* unary and msg_id *)
  | U of unary_msg
  (* not in msg_id *)
  | Op_UNot      (* not x, like Op_UBang but lower precedence *)
  | Op_DefinedQuestion (* defined? *)

  (* only in argument *)
  | Op_UAmper    (* & *) 
  (* tree-sitter: in argument and hash *)
  | Op_UStarStar (* ** *)

  and unary_msg = 
  | Op_UMinus    (* -x, -@ when in msg_id *)  | Op_UPlus (* +x, +@ in msg_id *)
  | Op_UBang     (* !x *) | Op_UTilde    (* ~x *)
 [@@deriving show { with_path = false }, eq, ord]


type binary_op = 
  | B of binary_msg
  (* not in msg_id, like op_AND/Op_OR but lower precedence *)
  | Op_kAND     (* and *)  | Op_kOR      (* or *)
  (* not in msg_id but in Op_OP_ASGN *)
  | Op_AND      (* && *)  | Op_OR   (* || *)

  (* less: could move out! Assign and AssignOp of lhs * tok * expr *)
  | Op_ASSIGN   (* = *)
  | Op_OP_ASGN of binary_op  (* +=, -=, ... *)

  (* less: move out, in hash or arguments *)
  | Op_ASSOC    (* => *)

  (* sugar for .. and = probably *)
  | Op_DOT3     (* ... *)

  and binary_msg = 
  (* binary and msg_id and assign op *)
  | Op_PLUS     (* + *)  | Op_MINUS    (* - *)
  | Op_TIMES    (* * *)  | Op_REM      (* % *)  | Op_DIV      (* / *)

  | Op_LSHIFT   (* < < *)  | Op_RSHIFT   (* > > *)

  | Op_BAND     (* & *)  | Op_BOR      (* | *)
  | Op_XOR      (* ^ *)
  | Op_POW      (* ** *)

  (* binary and msg_id (but not in assign op) *)
  | Op_CMP      (* <=> *)
  | Op_EQ       (* == *)  | Op_EQQ      (* === *)
  | Op_NEQ      (* != *)
  | Op_GEQ      (* >= *)  | Op_LEQ      (* <= *)
  | Op_LT       (* < *)  | Op_GT       (* > *)

  | Op_MATCH    (* =~ *)
  | Op_NMATCH   (* !~ *)

  | Op_DOT2     (* .. *)

  (* only in DotAccess method_name or MethodDef; Never in Binop *)
  | Op_AREF     (* [] *)
  | Op_ASET     (* []= *)

 [@@deriving show { with_path = false }, eq, ord]

(* ------------------------------------------------------------------------- *)
(* Method name, scope resolution, class/module name are later *)
(* ------------------------------------------------------------------------- *)
(* mutually dependent on expr with MethodSymbol and MethodDynamic *)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

type expr = 
  | Literal of literal

  (* Both constructors below are similar to class_or_module_name *)
  | Id of variable
  (* old: was Binop(e1, Op_SCOPE, e2) or Unary(Op_UScope. e) *)
  | ScopedId of scope_resolution

  | Hash of bool * expr list bracket
  | Array of expr list bracket
  | Tuple of expr list

  | Unary of unary_op wrap * expr
  | Binop of expr * binary_op wrap * expr
  | Ternary of expr * tok (* ? *) * expr * tok (* : *) * expr

  | Call of expr * expr list * expr option
  (* TODO: ArrayAccess of expr * expr list bracket *)
  (* old: was Binop(e1, Op_DOT, e2) before *)
  | DotAccess of expr * tok (* . or &. *) * method_name

  (* in argument, pattern, exn, assignment lhs or rhs.
   * old: was Unary(Op_UStar), or UOperator(Op_UStar).
   * expr is None only when Splat is used as last element in assign.
   *)
  | Splat of tok (* '*', but also ',' in mlhs *) * expr option

  (* true = {}, false = do/end *)
  | CodeBlock of bool bracket * formal_param list option * stmts

  | S of stmt
  | D of definition

  (* sgrep-ext: *)
  | Ellipsis of tok (* should be only in .pyi, types Dict[str,...], or sgrep *)
  | DeepEllipsis of expr bracket
  (* TODO: unused for now, need find a syntax *)
  | TypedMetavar of ident * tok * type_

  (* less: use for Assign, can be Id, Tuple, Array, more? *)
  and lhs = expr 

and literal = 
  (* [tT]rue, [fF]alse *)
  | Bool of bool wrap
  (* pattern: 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0x[0-9a-fA-F](_?[0-9a-fA-F])* *)
  | Num of string wrap
  (* pattern: \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)* )? *)
  | Float of string wrap

  (* treesitter: TODO add in dyp *)
  (* pattern: (\d+)?(\+|-)?(\d+)i *)
  | Complex of string wrap 
  | Rational of string wrap * tok (* r *) 
 
  | String of string_kind wrap
  | Regexp of (string_contents list * string) wrap
  (* treesitter: TODO add in dyp *)
  (* pattern: \?(\\\S({[0-9]*}|[0-9]*|-\S([MC]-\S)?)?|\S) *)
  | Char of string wrap 
  (* the atom string includes the ':' prefix *)
  | Atom of atom

  | Nil of tok 

  and string_kind = 
    | Single of string
    | Double of string_contents list
    | Tick of string_contents list

    and string_contents = 
      | StrChars of string
      | StrExpr of expr

  and atom = string_contents list wrap

(* ------------------------------------------------------------------------- *)
(* Method name *)
(* ------------------------------------------------------------------------- *)

(* old: was just expr before *)
and method_name = 
  | MethodId of variable (* all except Self and Super *)
  | MethodIdAssign of ident * tok * id_kind (* = *)
  | MethodAtom of atom
  | MethodUOperator of unary_msg wrap
  | MethodOperator of binary_msg wrap
  (* tree-sitter: and only in Call, not in definitions *)
  | MethodDynamic of expr (* actually an expr list inside () encoded as Tuple*)

(* ------------------------------------------------------------------------- *)
(* Class or module name *)
(* ------------------------------------------------------------------------- *)
and class_or_module_name = 
   | NameConstant of uident 
   | NameScope of scope_resolution

(* ------------------------------------------------------------------------- *)
(* Scope resolution *)
(* ------------------------------------------------------------------------- *)
(* The variable below is actually either an ID_Lowercase or ID_Uppercase
 * less: replace variable with ident? 
 *)
and scope_resolution =
  (* old: was called Op_UScope before *)
  | TopScope of tok (* :: *) * variable
  (* old: was called Op_SCOPE before *)
  | Scope of expr * tok (* :: *) * variable_or_method_name

  and variable_or_method_name = 
   | SV of variable
   (* TODO: this is not in tree-sitter *)
   | SM of method_name


(*****************************************************************************)
(* pattern *)
(*****************************************************************************)
(* arg or splat_argument in case/when, but
 * also tuple in lhs of Assign.
 *)
and pattern = expr

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
and type_ = expr

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
(* Note that in Ruby everything is an expr, but I still like to split expr
 * with the different "subtypes" 'stmt' and 'definition'.
 * Note that ../analyze/il_ruby.ml has proper separate expr and stmt types.
 *)
and stmt =
  | Block of stmts bracket (* ( ) *)

  | If of tok * expr * stmts * (tok (* else/elif *) * stmts) option
  | While of tok * bool * expr * stmts
  | Until of tok * bool * expr * stmts
  | Unless of tok * expr * stmts * (tok (* else *) * stmts) option
  (* old: was formal_param list instead of pattern before *)
  | For of tok * pattern * tok * expr * stmts

  (* stmt and also as "command" *)
  | Return of tok * expr list (* bracket option *)
  | Yield of tok * expr list (* option *)
  (* treesitter: TSNOTDYP *)
  | Break of tok * expr list | Next of tok * expr list
  (* not as "command" *)
  | Redo of tok * expr list | Retry of tok * expr list

  | Case of tok * case_block

  | ExnBlock of body_exn (* less: bracket *)

  and case_block = {
    case_guard : expr option;
    (* the pattern list is a comma separated list of expressions and
     * is converted in a || list by Ruby. The use of such comma is 
     * actually deprecated *)
    case_whens: (tok (* when *) * pattern list * stmts) list;
    case_else: (tok (* else *) * stmts) option;
  }
  
  (* tokens around body_exn are usually begin/end or do/end or
   * <nothing>/end for class and module defs *)
  and body_exn = {
    body_exprs: stmts;
    rescue_exprs: rescue_clause list;
    ensure_expr: (tok (* ensure *) * stmts) option;
    else_expr: (tok (* else *) * stmts) option;
  }
    (* less: the list can be empty, in which case it maybe mean
     * implicitely StandardError exn? *)
    and rescue_clause = 
      tok * exception_ list * exception_variable option * stmts
        (* usually an Id, or a Splat *)
        and exception_ = expr
        (* lhs is usually an Id *)
        and exception_variable = tok (* => *) * lhs

and stmts = expr list

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
and definition =
  | ModuleDef of tok * class_or_module_name * body_exn (* less: * tok *)
  | ClassDef of tok * class_kind * body_exn  (* less: * tok *)
  | MethodDef of tok * method_kind * formal_param list * body_exn  (* less: * tok *)

  | BeginBlock of tok * stmts bracket
  | EndBlock of tok * stmts bracket

  | Alias of tok * method_name * method_name
  | Undef of tok * method_name list

  (* treesitter: TODO stuff with ; and identifier list? in block params? *)
  and formal_param = 
    (* old: was of expr before *)
    | Formal_id of ident (* usually just xx but sometimes also @xx or $xx *)
    | Formal_amp of tok * ident

    (* less: Formal_splat of tok * ident option *)
    | Formal_star of tok * ident (* as in *x *)
    | Formal_rest of tok (* just '*' *)

    | Formal_tuple of formal_param list bracket
    | Formal_default of ident * tok (* = *) * expr

    (* treesitter: TSNOTDYP *)
    | Formal_hash_splat of tok * ident option
    | Formal_kwd of ident * tok * expr option

     (* sgrep-ext: *)
     | ParamEllipsis of tok
  
  and class_kind = 
   | C of class_or_module_name * (tok (* < *) * expr) option
   | SingletonC of tok (* << *) * expr

  (* old: was just expr before *)
  and method_kind =
   | M of method_name
   | SingletonM of expr (* TODO (variable | expr) * scope_op * method_name *)

 [@@deriving show { with_path = false }, eq, ord] (* with tarzan *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
(* Was called Annotation in diamondback-ruby but was using its own specific
 * comment format. 
 * less: maybe leverage the new work on gradual typing of Ruby in
 * Sorbet and steep?
 *)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)

type program = stmts
 [@@deriving show, eq, ord] (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any = 
  (* main any for semgrep *)
  | E of expr
  | S2 of stmt
  | Ss of stmts
  (* other *)
  | Mn of method_name
  | Def of definition
  | Pa of formal_param
  | Pr of program

 [@@deriving show { with_path = false}, eq] (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let empty_body_exn = {
    body_exprs = [];
    rescue_exprs = [];
    ensure_expr = None;
    else_expr = None;
}

let sm = function
  | MethodId id -> SV id
  | x -> SM x

let cmn = function
  | NameConstant id -> Id (id, ID_Uppercase)
  | NameScope x -> ScopedId x

let opt_stmts_to_stmts = function
  | None -> []
  | Some (_, xs) -> xs

(* x: v  <=>  :x => v  in Ruby in hash and calls *)
let keyword_arg_to_expr id tk arg =
  let (s,t) = id in
  Binop ((Literal (Atom ([StrChars (":"^s)],t))), (Op_ASSOC, tk), arg)

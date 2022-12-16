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
(* Ruby Intermediate Language for Ruby 1.9.
 *
 * Most of the code in this file derives from code from
 * Mike Furr in diamondback-ruby.
 *
 * For more information, See "The Ruby Intermediate Language" paper at DLS'09.
*)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type tok = Parse_info.t
[@@deriving show] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Names *)
(* ------------------------------------------------------------------------- *)

type identifier =
  | Var of (var_kind * string)
  | Self
  | Scope of identifier * string
  | UScope of string
  | Nil
  | True
  | False

and var_kind =
  | Local
  | Instance
  | Class
  | Global
  | Constant
  (* old: | Builtin, now merged in Global *)
[@@deriving show { with_path = false }] (* with tarzan *)

(* convenience alias that is a subtype of identifier *)
type builtin_or_global = var_kind (* [`Var_Builtin|`Var_Global] *) * string
[@@deriving show] (* with tarzan *)

type msg_id =
  | ID_UOperator of unary_op
  | ID_Operator of binary_op
  | ID_MethodName of string
  | ID_Assign of string
  | ID_Super

and unary_op =
  | Op_UMinus | Op_UPlus
  | Op_UTilde

and binary_op =
  | Op_Plus | Op_Minus
  | Op_Times | Op_Rem | Op_Div
  | Op_Pow
  | Op_CMP
  | Op_EQ | Op_EQQ
  | Op_GEQ | Op_LEQ
  | Op_LT | Op_GT
  | Op_BAnd  | Op_BOr
  | Op_LShift | Op_RShift

  | Op_Match
  | Op_XOR
  | Op_ARef
  | Op_ASet
[@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

type expr =
  | EId of identifier
  | ELit of literal

and literal =
  (* atomic *)
  | Num of string
  | Float of string
  | String of string

  | Atom of string
  | Regexp of string * string

  (* composite *)
  | Range of bool * expr * expr
  | Array of star_expr list
  | Hash of (expr * expr) list

(* a star_expr is either an expr or a (`Star of expr), i.e., no
   nested Star's are allowed *)
and star_expr =
  | SE of expr
  | SStar of expr

[@@deriving show { with_path = false }] (* with tarzan *)


(*****************************************************************************)
(* Instruction *)
(*****************************************************************************)
type instr =
  | Expression of expr
  | Assign of lhs * tuple_expr
  | Call of lhs option * method_call

(* lhs is like a tuple expression, but no literals are allowed *)
and lhs =
  | LId of identifier
  | LTup of lhs list
  | LStar of identifier

and tuple_expr =
  | TTup of tuple_expr list
  | TE of expr
  | TStar of tuple_expr  (* again, no nested stars *)

and method_call = {
  mc_target : expr option;
  mc_msg : msg_id;
  mc_args : star_expr list;
  mc_cb : codeblock option;
}
and codeblock =
  | CB_Arg of expr
  | CB_Block of block_formal_param list * stmt (* recurse stmt *)

and block_formal_param =
  | Formal_block_id of var_kind * string
  | Formal_star2 of string
  | Formal_tuple of block_formal_param list

(*****************************************************************************)
(* Statement (and CFG) *)
(*****************************************************************************)

and stmt = {
  snode : stmt_node;
  pos : tok;
  sid : int;
  mutable lexical_locals : Utils_ruby.StrSet.t
                           [@printer fun fmt _ -> fprintf fmt "lexical_locals:??"];
  mutable preds : stmt Set_.t
                  [@printer fun fmt _ -> fprintf fmt "preds:??"];
  mutable succs : stmt Set_.t
                  [@printer fun fmt _ -> fprintf fmt "succs:??"];
}

and stmt_node =
  | I of instr
  | D of definition

  | Seq of stmt list (* a.k.a Block *)
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of block_formal_param list * expr * stmt
  | Case of case_block

  | Return of tuple_expr option
  | Yield of lhs option * star_expr list
  | Break of tuple_expr option
  | Next of tuple_expr option
  | Redo
  | Retry

  | ExnBlock of exn_block

  | Begin of stmt
  | End of stmt

and exn_block = {
  exn_body : stmt;
  exn_rescue : rescue_block list;
  exn_else : stmt option;
  exn_ensure : stmt option;
}

and rescue_block = {
  rescue_guards : rescue_guard list;
  rescue_body : stmt;
}

and rescue_guard =
  | Rescue_Expr of tuple_expr
  | Rescue_Bind of tuple_expr * identifier

and case_block = {
  case_guard : expr;
  case_whens: (tuple_expr * stmt) list;
  case_else: stmt option;
}

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
and definition =
  | ModuleDef  of lhs option * identifier * stmt
  | ClassDef of lhs option * class_kind * stmt
  | MethodDef of def_name * method_formal_param list * stmt

  | Defined of identifier * stmt
  | Alias of alias_kind
  | Undef of msg_id list

and class_kind =
  | MetaClass of identifier
  | NominalClass of identifier * identifier option

and def_name =
  | Instance_Method of msg_id
  | Singleton_Method of identifier * msg_id

and method_formal_param =
  | Formal_meth_id of string
  | Formal_amp of string
  | Formal_star of string
  | Formal_default of string * tuple_expr


and alias_kind =
  | Alias_Method of msg_id * msg_id
  | Alias_Global of builtin_or_global * builtin_or_global

[@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)

type t = stmt
[@@deriving show] (* with tarzan *)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

type any_formal =
  | B of block_formal_param
  | M of method_formal_param

[@@deriving show { with_path = false }] (* with tarzan *)

let b_to_any x = B x
let m_to_any x = M x

(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
module AST = Ast_c
module CST = Tree_sitter_c.CST
open Ast_cpp
open Ast_c
module G = AST_generic
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* C parser using tree-sitter-lang/semgrep-c and converting
 * to ../ast/ast_c.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * ../generic/c_to_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type extra = {
  (* gensym *)
  mutable cnt : int;
  mutable struct_defs_toadd : struct_def list;
  mutable enum_defs_toadd : enum_def list;
  mutable typedefs_toadd : type_def list;
}

(* similar to Ast_c_build env *)
let default_extra_env =
  { cnt = 0; struct_defs_toadd = []; enum_defs_toadd = []; typedefs_toadd = [] }

let gensym_struct cnt = spf "__anon_struct_%d" cnt
let gensym_enum cnt = spf "__anon_enum_%d" cnt

type env = extra H.env

let _fake = G.fake
let fb = Tok.unsafe_fake_bracket
let token = H.token
let str = H.str

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-go/.../Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the c CST
   to another type of tree.
*)

let imm_tok_pat_509ec78 (env : env) (tok : CST.imm_tok_pat_509ec78) =
  (* pattern \r?\n *) token env tok

let pat_ca8830e (env : env) (tok : CST.pat_ca8830e) =
  (* pattern #[ 	]*include *) token env tok

let pat_0307ca2 (env : env) (tok : CST.pat_0307ca2) =
  (* pattern #[ 	]*elifdef *) token env tok

let pat_a6d4183 (env : env) (tok : CST.pat_a6d4183) =
  (* pattern #[ 	]*elifndef *) token env tok

let anon_choice_signed_a0bfc19 (env : env) (x : CST.anon_choice_signed_a0bfc19)
    =
  match x with
  | `Signed tok -> (* "signed" *) str env tok
  | `Unsi tok -> (* "unsigned" *) str env tok
  | `Long tok -> (* "long" *) str env tok
  | `Short tok -> (* "short" *) str env tok

let anon_choice_BANG_67174d6 (env : env) (x : CST.anon_choice_BANG_67174d6) =
  match x with
  | `BANG tok -> (Not, token env tok) (* "!" *)
  | `TILDE tok -> (Tilde, token env tok) (* "~" *)
  | `DASH tok -> (UnMinus, token env tok) (* "-" *)
  | `PLUS tok -> (UnPlus, token env tok)

(* "+" *)

let type_qualifier (env : env) (x : CST.type_qualifier) =
  match x with
  | `Const tok -> token env tok (* "const" *)
  | `Cons tok -> (* "constexpr" *) token env tok
  | `Vola tok -> token env tok (* "volatile" *)
  | `Rest tok -> token env tok (* "restrict" *)
  | `X___rest__ tok -> (* "__restrict__" *) token env tok
  | `X___exte__ tok -> (* "__extension__" *) token env tok
  | `X__Atomic tok -> token env tok
  | `Nore tok -> (* "noreturn" *) token env tok
  | `X__Nore tok -> (* "_Noreturn" *) token env tok

(* "_Atomic" *)

let identifier (env : env) (tok : CST.identifier) : name = str env tok

(* pattern [a-zA-Z_]\w* *)

let storage_class_specifier (env : env) (x : CST.storage_class_specifier) =
  match x with
  | `Extern tok -> token env tok (* "extern" *)
  | `Static tok -> token env tok (* "static" *)
  | `Auto tok -> token env tok (* "auto" *)
  | `Regi tok -> token env tok (* "register" *)
  | `Inline tok -> token env tok
  | `Thread_local tok -> (* "thread_local" *) token env tok
  (* the difference between these two is just which implementation you are using *)
  | `X___inline tok -> token env tok
  | `X___inline__ tok -> token env tok
  | `X___forc tok -> token env tok
  | `X___thread tok -> token env tok

(* "inline" *)

let ms_call_modifier (env : env) (x : CST.ms_call_modifier) =
  match x with
  | `X___cdecl tok -> token env tok (* "__cdecl" *)
  | `X___clrc tok -> token env tok (* "__clrcall" *)
  | `X___stdc tok -> token env tok (* "__stdcall" *)
  | `X___fast tok -> token env tok (* "__fastcall" *)
  | `X___this tok -> token env tok (* "__thiscall" *)
  | `X___vect tok -> token env tok

(* "__vectorcall" *)

let ms_unaligned_ptr_modifier (env : env) (x : CST.ms_unaligned_ptr_modifier) =
  match x with
  | `X__unal tok -> token env tok (* "_unaligned" *)
  | `X___unal tok -> token env tok

(* "__unaligned" *)

let anon_choice_DASHDASH_d11def2 (env : env)
    (x : CST.anon_choice_DASHDASH_d11def2) =
  match x with
  | `DASHDASH tok -> (Dec, token env tok) (* "--" *)
  | `PLUSPLUS tok -> (Inc, token env tok)

(* "++" *)

let string_literal (env : env) ((v1, v2, v3) : CST.string_literal) : string wrap
    =
  let v1 =
    match v1 with
    | `LDQUOT tok -> token env tok (* "L\"" *)
    | `UDQUOT_c163aae tok -> token env tok (* "u\"" *)
    | `UDQUOT_df3447d tok -> token env tok (* "U\"" *)
    | `U8DQUOT tok -> token env tok (* "u8\"" *)
    | `DQUOT tok -> token env tok
    (* "\"" *)
  in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Imm_tok_prec_p1_pat_c7f65b4 tok ->
            str env tok (* pattern "[^\\\\\"\\n]+" *)
        | `Esc_seq tok -> str env tok
        (* escape_sequence *))
      v2
  in
  let v3 = token env v3 (* "\"" *) in
  let s = v2 |> Common.map fst |> String.concat "" in
  (s, Tok.combine_toks v1 (Common.map snd v2 @ [ v3 ]))

let gnu_asm_qualifier (env : env) (x : CST.gnu_asm_qualifier) =
  match x with
  | `Vola tok -> (* "volatile" *) str env tok
  | `Inline tok -> (* "inline" *) str env tok
  | `Goto tok -> (* "goto" *) str env tok

let char_literal (env : env) ((v1, v2, v3) : CST.char_literal) : string wrap =
  let v1 =
    match v1 with
    | `LSQUOT tok -> token env tok (* "L'" *)
    | `USQUOT_d861d39 tok -> token env tok (* "u'" *)
    | `USQUOT_2701bdc tok -> token env tok (* "U'" *)
    | `U8SQUOT tok -> token env tok (* "u8'" *)
    | `SQUOT tok -> token env tok
    (* "'" *)
  in
  let v2 =
    match v2 with
    | `Esc_seq tok -> str env tok (* escape_sequence *)
    | `Imm_tok_pat_36637e2 tok -> str env tok
    (* pattern "[^\\n']" *)
  in
  let v3 = token env v3 (* "'" *) in
  let s = fst v2 in
  (s, Tok.combine_toks v1 [ snd v2; v3 ])

let anon_choice_pat_25b90ba_4a37f8c (env : env)
    (x : CST.anon_choice_pat_25b90ba_4a37f8c) =
  match x with
  | `Pat_25b90ba tok -> token env tok (* pattern #[ 	]*ifdef *)
  | `Pat_9d92f6a tok -> token env tok

(* pattern #[ 	]*ifndef *)

let ms_pointer_modifier (env : env) (x : CST.ms_pointer_modifier) =
  match x with
  | `Ms_unal_ptr_modi x -> ms_unaligned_ptr_modifier env x
  | `Ms_rest_modi tok -> token env tok (* "__restrict" *)
  | `Ms_unsi_ptr_modi tok -> token env tok (* "__uptr" *)
  | `Ms_signed_ptr_modi tok -> token env tok

(* "__sptr" *)

(* can actually contain a complex expression, but just parsed as a string
 * until non-escaped newline in tree-sitter-c.
 *)
let preproc_arg env tok = str env tok

let anon_choice_pat_0307ca2_dbf6a9d (env : env)
    (x : CST.anon_choice_pat_0307ca2_dbf6a9d) =
  match x with
  | `Pat_0307ca2 x -> pat_0307ca2 env x
  | `Pat_a6d4183 x -> pat_a6d4183 env x

let preproc_call (env : env) ((v1, v2, v3) : CST.preproc_call) =
  let v1 = identifier env v1 (* pattern #[ \t]*[a-zA-Z]\w* *) in
  let _v3 = token env v3 (* "\n" *) in
  let v2 =
    match v2 with
    | Some tok -> Some (preproc_arg env tok) (* preproc_arg *)
    | None -> None
  in
  OtherDirective (v1, v2)

let field_designator (env : env) ((v1, v2) : CST.field_designator) : name =
  let _v1 = token env v1 (* "." *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  v2

let gnu_asm_goto_list (env : env) ((v1, v2) : CST.gnu_asm_goto_list) =
  let _v1 = (* ":" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 =
          (* pattern (\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
          str env v1
        in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 =
                (* pattern (\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
                str env v2
              in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  v2

let preproc_defined (env : env) (x : CST.preproc_defined) : tok * name =
  match x with
  | `Defi_LPAR_id_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "defined" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 = identifier env v3 (* pattern [a-zA-Z_]\w* *) in
      let _v4 = token env v4 (* ")" *) in
      (v1, v3)
  | `Defi_id (v1, v2) ->
      let v1 = token env v1 (* "defined" *) in
      let v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
      (v1, v2)

let anon_choice_type_id_d3c4b5f (env : env)
    (x : CST.anon_choice_type_id_d3c4b5f) =
  match x with
  | `Id tok -> str env tok (* pattern [a-zA-Z_]\w* *)
  | `DOTDOTDOT tok -> ("...", token env tok)

(* "..." *)

let anon_choice_type_id_fe6e1ce (env : env)
    (x : CST.anon_choice_type_id_fe6e1ce) ty_hashtbl =
  match x with
  | `Id tok -> (
      (* pattern (\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
      let s, _ = str env tok in
      match Hashtbl.find_opt ty_hashtbl s with
      | None -> None
      | Some ty ->
          Some (ParamClassic { p_type = ty; p_name = Some (str env tok) }))
  | `Vari_param x ->
      let tk = (* "..." *) token env x in
      Some (ParamDots tk)

let ms_declspec_modifier (env : env)
    ((v1, v2, v3, v4) : CST.ms_declspec_modifier) =
  let _v1 = token env v1 (* "__declspec" *) in
  let _v2 = token env v2 (* "(" *) in
  let _v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  let _v4 = token env v4 (* ")" *) in
  ()

let concatenated_string (env : env) ((v1, v2, v3) : CST.concatenated_string) =
  (* Here for macros which resolves to literal strings. *)
  let v1 =
    match v1 with
    | `Id tok ->
        (* pattern (\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
        StrIdent (str env tok)
    | `Str_lit x -> StrLit (string_literal env x)
  in
  let v2 = StrLit (string_literal env v2) in
  let v3 =
    List.map
      (fun x ->
        match x with
        | `Str_lit x -> StrLit (string_literal env x)
        | `Id tok ->
            (* pattern (\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
            StrIdent (str env tok))
      v3
  in
  ConcatString (v1 :: v2 :: v3)

let gnu_asm_clobber_list (env : env) ((v1, v2) : CST.gnu_asm_clobber_list) =
  let _v1 = (* ":" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = string_literal env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = string_literal env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  v2

let old_style_parameter_list (env : env)
    ((v1, v2, v3) : CST.old_style_parameter_list) ty_hashtbl =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        v1 :: Common.map snd v2
        |> List.filter_map (fun x ->
               anon_choice_type_id_fe6e1ce env x ty_hashtbl)
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

let gnu_asm_output_operand (env : env)
    ((v1, v2, v3, v4, v5) : CST.gnu_asm_output_operand) =
  let v1 =
    match v1 with
    | Some (v1, v2, v3) ->
        Some
          (let v1 = (* "[" *) token env v1 in
           let v2 =
             (* pattern (\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
             str env v2
           in
           let v3 = (* "]" *) token env v3 in
           (v1, v2, v3))
    | None -> None
  in
  let v2 = string_literal env v2 in
  let v3 = (* "(" *) token env v3 in
  let v4 =
    (* pattern (\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
    str env v4
  in
  let v5 = (* ")" *) token env v5 in
  (v1, v2, (v3, v4, v5))

let gnu_asm_output_operand_list (env : env)
    ((v1, v2) : CST.gnu_asm_output_operand_list) =
  let _v1 = (* ":" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = gnu_asm_output_operand env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = gnu_asm_output_operand env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  v2

let preproc_def (env : env) ((v1, v2, v3, v4) : CST.preproc_def) : directive =
  let v1 = token env v1 (* pattern #[ 	]*define *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    match v3 with
    | Some tok -> Some (preproc_arg env tok) (* preproc_arg *)
    | None -> None
  in
  let _v4 = token env v4 (* "\n" *) in
  match v3 with
  | Some x -> Define (v1, v2, Some (CppExpr (String x)))
  | None -> Define (v1, v2, None)

let rec preproc_argument_list (env : env)
    ((v1, v2, v3) : CST.preproc_argument_list) : argument list bracket =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = preproc_expression env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = preproc_expression env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2 |> Common.map (fun x -> Arg x), v3)

and preproc_binary_expression (env : env) (x : CST.preproc_binary_expression) :
    expr =
  match x with
  | `Prep_exp_PLUS_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = preproc_expression env v3 in
      let op = Arith Plus in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_DASH_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = preproc_expression env v3 in
      let op = Arith Minus in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_STAR_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = preproc_expression env v3 in
      let op = Arith Mul in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_SLASH_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = preproc_expression env v3 in
      let op = Arith Div in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_PERC_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = preproc_expression env v3 in
      let op = Arith Mod in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_BARBAR_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = preproc_expression env v3 in
      let op = Logical OrLog in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_AMPAMP_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = preproc_expression env v3 in
      let op = Logical AndLog in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_BAR_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = preproc_expression env v3 in
      let op = Arith Or in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_HAT_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = preproc_expression env v3 in
      let op = Arith Xor in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_AMP_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = preproc_expression env v3 in
      let op = Arith And in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_EQEQ_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = preproc_expression env v3 in
      let op = Logical Eq in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_BANGEQ_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = preproc_expression env v3 in
      let op = Logical NotEq in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_GT_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = preproc_expression env v3 in
      let op = Logical Sup in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_GTEQ_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = preproc_expression env v3 in
      let op = Logical SupEq in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_LTEQ_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = preproc_expression env v3 in
      let op = Logical InfEq in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_LT_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = preproc_expression env v3 in
      let op = Logical Inf in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_LTLT_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = preproc_expression env v3 in
      let op = Arith DecLeft in
      Binary (v1, (op, v2), v3)
  | `Prep_exp_GTGT_prep_exp (v1, v2, v3) ->
      let v1 = preproc_expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = preproc_expression env v3 in
      let op = Arith DecRight in
      Binary (v1, (op, v2), v3)

and preproc_call_expression (env : env) ((v1, v2) : CST.preproc_call_expression)
    =
  let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
  let v2 = preproc_argument_list env v2 in
  Call (Id v1, v2)

(* Int or Float ! *)
and number_literal env tok =
  let s, t = str env tok in
  match Common2.int_of_string_c_octal_opt s with
  | Some i -> Int (Some i, t)
  | None -> (
      match float_of_string_opt s with
      | Some f -> Float (Some f, t)
      (* could be None because of a suffix in the string *)
      | None -> Int (None, t))

and preproc_expression (env : env) (x : CST.preproc_expression) : expr =
  match x with
  | `Id tok ->
      let id = identifier env tok (* pattern [a-zA-Z_]\w* *) in
      Id id
  | `Prep_call_exp x -> preproc_call_expression env x
  | `Num_lit tok -> number_literal env tok (* number_literal *)
  | `Char_lit x ->
      let c = char_literal env x in
      Char c
  | `Prep_defi x ->
      let t, id = preproc_defined env x in
      Defined (t, id)
  | `Prep_un_exp (v1, v2) ->
      let v1 = anon_choice_BANG_67174d6 env v1 in
      let v2 = preproc_expression env v2 in
      Unary (v2, v1)
  | `Prep_bin_exp x -> preproc_binary_expression env x
  | `Prep_paren_exp (v1, v2, v3) ->
      let _v1 = token env v1 (* "(" *) in
      let v2 = preproc_expression env v2 in
      let _v3 = token env v3 (* ")" *) in
      v2

let preproc_params (env : env) ((v1, v2, v3) : CST.preproc_params) : name list =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = anon_choice_type_id_d3c4b5f env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = anon_choice_type_id_d3c4b5f env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = token env v3 (* ")" *) in
  v2

let preproc_include (env : env) ((v1, v2, v3) : CST.preproc_include) =
  let v1 = pat_ca8830e env v1 in
  let v2 =
    match v2 with
    | `Str_lit x -> IncludePath (string_literal env x)
    | `System_lib_str tok ->
        (* system_lib_string *)
        IncludePath (str env tok)
    | `Id tok ->
        (* pattern (\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
        IncludePath (str env tok)
    | `Prep_call_exp x -> IncludeCall (preproc_call_expression env x)
  in
  let _v3 = imm_tok_pat_509ec78 env v3 in
  DirStmt (Include (v1, v2))

let preproc_function_def (env : env)
    ((v1, v2, v3, v4, v5) : CST.preproc_function_def) : directive =
  let v1 = token env v1 (* pattern #[ 	]*define *) in
  let v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = preproc_params env v3 in
  let v4 =
    match v4 with
    | Some tok -> Some (preproc_arg env tok) (* preproc_arg *)
    | None -> None
  in
  let _v5 = token env v5 (* "\n" *) in
  match v4 with
  | Some x -> Macro (v1, v2, v3, Some (CppExpr (String x)))
  | None -> Macro (v1, v2, v3, None)

(* takes left part of the type as a parameter and return final type *)
let rec abstract_declarator (env : env) (x : CST.abstract_declarator) :
    type_ -> type_ =
  match x with
  | `Abst_poin_decl (v1, v2, v3) ->
      let v1 = token env v1 (* "*" *) in
      let _v2 = Common.map (type_qualifier env) v2 in
      let v3 =
        match v3 with
        | Some x ->
            fun t -> t |> abstract_declarator env x |> fun t -> TPointer (v1, t)
        | None -> fun t -> TPointer (v1, t)
      in
      v3
  | `Abst_func_decl (v1, v2) ->
      let v2 = parameter_list env v2 in
      let v1 =
        match v1 with
        | Some x ->
            let f = abstract_declarator env x in
            fun t -> f (TFunction (t, v2))
        | None -> fun t -> TFunction (t, v2)
      in
      v1
  | `Abst_array_decl (v1, v2, v3, v4, v5) ->
      let _v2 = token env v2 (* "[" *) in
      let _v3 = Common.map (type_qualifier env) v3 in
      let v4 =
        match v4 with
        | Some x -> Some (anon_choice_exp_508611b env x)
        | None -> None
      in
      let _v5 = token env v5 (* "]" *) in
      let v1 =
        match v1 with
        | Some x ->
            let f = abstract_declarator env x in
            fun t -> f (TArray (v4, t))
        | None -> fun t -> TArray (v4, t)
      in
      v1
  | `Abst_paren_decl (v1, v2, v3) ->
      let _v1 = token env v1 (* "(" *) in
      let v2 = abstract_declarator env v2 in
      let _v3 = token env v3 (* ")" *) in
      v2

and anon_choice_exp_508611b (env : env) (x : CST.anon_choice_exp_508611b) =
  match x with
  | `Exp x ->
      let e = expression env x in
      e
  (* less: what is that? *)
  | `STAR tok ->
      let t = token env tok (* "*" *) in
      Id ("*", t)

and anon_choice_exp_55b4dba (env : env) (x : CST.anon_choice_exp_55b4dba) : expr
    =
  match x with
  | `Exp x -> expression env x
  | `Comma_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "," *) in
      let v3 = anon_choice_exp_55b4dba env v3 in
      Sequence (v1, v3)

and gnu_asm_input_operand (env : env)
    ((v1, v2, v3, v4, v5) : CST.gnu_asm_input_operand) =
  let v1 =
    match v1 with
    | Some (v1, v2, v3) ->
        Some
          (let v1 = (* "[" *) token env v1 in
           let v2 =
             (* pattern (\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
             str env v2
           in
           let v3 = (* "]" *) token env v3 in
           (v1, v2, v3))
    | None -> None
  in
  let v2 = string_literal env v2 in
  let v3 = (* "(" *) token env v3 in
  let v4 = expression env v4 in
  let v5 = (* ")" *) token env v5 in
  (v1, v2, (v3, v4, v5))

and gnu_asm_input_operand_list (env : env)
    ((v1, v2) : CST.gnu_asm_input_operand_list) =
  let _v1 = (* ":" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = gnu_asm_input_operand env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = gnu_asm_input_operand env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  v2

and map_anon_choice_opt___exte___exp_2bc8eaa (env : env)
    (x : CST.anon_choice_opt___exte___exp_2bc8eaa) =
  match x with
  | `Opt___exte___exp (v1, v2) ->
      let _v1_TODO =
        match v1 with
        | Some tok -> Some ((* "__extension__" *) token env tok)
        | None -> None
      in
      let v2 = expression env v2 in
      Arg v2
  | `Comp_stmt x ->
      let x = compound_statement env x in
      ArgBlock x

and anon_choice_init_pair_1a6981e (env : env)
    (x : CST.anon_choice_init_pair_1a6981e) =
  match x with
  | `Init_pair (v1, v2, v3) ->
      let v1 =
        Common.map
          (fun x ->
            match x with
            | `Subs_desi x ->
                let lp, e, rp = subscript_designator env x in
                Left (lp, e, rp)
            | `Field_desi x ->
                let fld = field_designator env x in
                Right fld)
          v1
      in
      let v2 = token env v2 (* "=" *) in
      let v3 =
        match v3 with
        | `Exp x -> expression env x
        | `Init_list x -> initializer_list env x
      in
      Left (v1, v2, v3)
  | `Exp x -> Right (expression env x)
  | `Init_list x -> Right (initializer_list env x)

and anon_choice_param_decl_4ac2852 (env : env)
    (x : CST.anon_choice_param_decl_4ac2852) : parameter =
  match x with
  | `Param_decl (v1, v2) ->
      let v1 = declaration_specifiers env v1 in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `Decl x ->
                let id, f = declarator env x in
                { p_type = f v1; p_name = Some id }
            | `Abst_decl x ->
                let f = abstract_declarator env x in
                { p_type = f v1; p_name = None })
        | None -> { p_type = v1; p_name = None }
      in
      ParamClassic v2
  | `Vari_param tok ->
      let t = token env tok (* "..." *) in
      ParamDots t

and anon_choice_prep_else_in_field_decl_list_97ea65e (env : env)
    (x : CST.anon_choice_prep_else_in_field_decl_list_97ea65e) =
  match x with
  | `Prep_else_in_field_decl_list (v1, v2) ->
      let _v1 = token env v1 (* pattern #[ 	]*else *) in
      let _v2 = Common.map (field_declaration_list_item env) v2 in
      ()
  | `Prep_elif_in_field_decl_list (v1, v2, v3, v4, v5) ->
      let _v1 = token env v1 (* pattern #[ 	]*elif *) in
      let _v2 = preproc_expression env v2 in
      let _v3 = token env v3 (* "\n" *) in
      let _v4 = Common.map (field_declaration_list_item env) v4 in
      let _v5 =
        match v5 with
        | Some x ->
            Some (anon_choice_prep_else_in_field_decl_list_97ea65e env x)
        | None -> None
      in
      ()

and declaration_modifiers (env : env) (x : CST.declaration_modifiers) =
  match x with
  | `Stor_class_spec x ->
      let _ = storage_class_specifier env x in
      ()
  | `Type_qual x ->
      let _ = type_qualifier env x in
      ()
  | `Attr_spec x ->
      let _ = attribute_specifier env x in
      ()
  | `Attr_decl x ->
      let _ = attribute_declaration env x in
      ()
  | `Ms_decl_modi x ->
      let _ = ms_declspec_modifier env x in
      ()

and map_anon_choice_type_id_opt_field_decl_list_9aebd83 (env : env)
    (x : CST.anon_choice_type_id_opt_field_decl_list_9aebd83) :
    name option * field_def list bracket =
  match x with
  | `Id_opt_field_decl_list (v1, v2) ->
      (* named struct/union *)
      let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
      let v2 =
        match v2 with
        | Some x -> field_declaration_list env x
        | None -> Tok.unsafe_fake_bracket []
      in
      (Some v1, v2)
  | `Field_decl_list x -> (None, field_declaration_list env x)

and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) :
    argument list bracket =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_opt___exte___exp_2bc8eaa env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_opt___exte___exp_2bc8eaa env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and assignment_left_expression (env : env) (x : CST.assignment_left_expression)
    : expr =
  match x with
  | `Id tok ->
      let id = identifier env tok (* pattern [a-zA-Z_]\w* *) in
      Id id
  | `Call_exp x -> call_expression env x
  | `Field_exp x -> field_expression env x
  | `Poin_exp x -> pointer_expression env x
  | `Subs_exp x -> subscript_expression env x
  | `Paren_exp x -> parenthesized_expression env x

and attribute_specifier (env : env) ((v1, v2, v3, v4) : CST.attribute_specifier)
    =
  let _v1 = token env v1 (* "__attribute__" *) in
  let _v2 = token env v2 (* "(" *) in
  let _v3 = argument_list env v3 in
  let _v4 = token env v4 (* ")" *) in
  ()

(* TODO: treat attributes properly *)
and attribute (env : env) ((v1, v2, v3) : CST.attribute) =
  let _v1 =
    match v1 with
    | Some (v1, v2) ->
        Some
          (let v1 = (* pattern [a-zA-Z_]\w* *) token env v1 in
           let v2 = (* "::" *) token env v2 in
           (v1, v2))
    | None -> None
  in
  let _v2 = (* pattern [a-zA-Z_]\w* *) token env v2 in
  let _v3 =
    match v3 with
    | Some x -> Some (argument_list env x)
    | None -> None
  in
  ()

and attribute_declaration (env : env)
    ((v1, v2, v3, v4) : CST.attribute_declaration) =
  let _v1 = (* "[[" *) token env v1 in
  let _v2 = attribute env v2 in
  let _v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = attribute env v2 in
        (v1, v2))
      v3
  in
  let _v4 = (* "]]" *) token env v4 in
  ()

and binary_expression (env : env) (x : CST.binary_expression) : expr =
  match x with
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = expression env v3 in
      let op = Arith Plus in
      Binary (v1, (op, v2), v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = expression env v3 in
      let op = Arith Minus in
      Binary (v1, (op, v2), v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = expression env v3 in
      let op = Arith Mul in
      Binary (v1, (op, v2), v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = expression env v3 in
      let op = Arith Div in
      Binary (v1, (op, v2), v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = expression env v3 in
      let op = Arith Mod in
      Binary (v1, (op, v2), v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = expression env v3 in
      let op = Logical OrLog in
      Binary (v1, (op, v2), v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      let op = Logical AndLog in
      Binary (v1, (op, v2), v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = expression env v3 in
      let op = Arith Or in
      Binary (v1, (op, v2), v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = expression env v3 in
      let op = Arith Xor in
      Binary (v1, (op, v2), v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = expression env v3 in
      let op = Arith And in
      Binary (v1, (op, v2), v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = expression env v3 in
      let op = Logical Eq in
      Binary (v1, (op, v2), v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = expression env v3 in
      let op = Logical NotEq in
      Binary (v1, (op, v2), v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = expression env v3 in
      let op = Logical Sup in
      Binary (v1, (op, v2), v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = expression env v3 in
      let op = Logical SupEq in
      Binary (v1, (op, v2), v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = expression env v3 in
      let op = Logical InfEq in
      Binary (v1, (op, v2), v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = expression env v3 in
      let op = Logical Inf in
      Binary (v1, (op, v2), v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = expression env v3 in
      let op = Arith DecLeft in
      Binary (v1, (op, v2), v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = expression env v3 in
      let op = Arith DecRight in
      Binary (v1, (op, v2), v3)

and bitfield_clause (env : env) ((v1, v2) : CST.bitfield_clause) : expr =
  let _v1 = token env v1 (* ":" *) in
  let v2 = expression env v2 in
  v2

and block_item (env : env) (x : CST.block_item) =
  match x with
  | `Func_defi x ->
      let def = function_definition env x in
      [ DefStmt (FuncDef def) ]
  | `Old_style_func_defi x -> old_style_function_definition env x
  | `Link_spec x -> linkage_specification env x
  | `Decl x ->
      let vars = declaration env x in
      vars |> Common.map (fun v -> DefStmt (VarDef v))
  | `Choice_case_stmt x ->
      let st = statement env x in
      [ st ]
  | `Attr_stmt x -> [ attributed_statement env x ]
  | `Type_defi x ->
      let xs = type_definition env x in
      xs |> Common.map (fun x -> DefStmt (TypeDef x))
  | `Empty_decl (v1, v2) ->
      (* I don't know why we do this but it's the same as the other EmptyDecl below *)
      let _v1 = type_specifier env v1 in
      let _v2 = token env v2 (* ";" *) in
      []
  | `Prep_if x -> [ preproc_if env x ]
  | `Prep_ifdef x -> [ preproc_ifdef env x ]
  | `Prep_incl x -> [ preproc_include env x ]
  | `Prep_def x -> [ DirStmt (preproc_def env x) ]
  | `Prep_func_def x -> [ DirStmt (preproc_function_def env x) ]
  | `Prep_call x -> [ DirStmt (preproc_call env x) ]

and call_expression (env : env) ((v1, v2) : CST.call_expression) : expr =
  let v1 = expression env v1 in
  let v2 = argument_list env v2 in
  Call (v1, v2)

and declaration_specifiers (env : env)
    ((v1, v2, v3) : CST.declaration_specifiers) : type_ =
  let _v1 = Common.map (declaration_modifiers env) v1 in
  let v2 = type_specifier env v2 in
  let _v3 = Common.map (declaration_modifiers env) v3 in
  v2

(* return a couple (name * partial type (to be applied to return type)) *)
and declarator (env : env) (x : CST.declarator) : name * (type_ -> type_) =
  match x with
  | `Attr_decl (v1, v2) ->
      let v1 = declarator env v1 in
      let _v2_TODO = List.map (attribute_declaration env) v2 in
      v1
  | `Poin_decl (v1, v2, v3, v4, v5) ->
      let _v1 =
        match v1 with
        | Some x -> Some (ms_based_modifier env x)
        | None -> None
      in
      let v2 = token env v2 (* "*" *) in
      let _v3 = Common.map (ms_pointer_modifier env) v3 in
      let _v4 = Common.map (type_qualifier env) v4 in
      let id, f = declarator env v5 in
      (id, fun t -> TPointer (v2, t) |> f)
  | `Func_decl (v1, v2, v3, v4) ->
      let id, f = declarator env v1 in
      let v2 = parameter_list env v2 in
      (* TODO: why is this allowed here??? *)
      let _v3_TODO =
        match v3 with
        | Some x -> Some (gnu_asm_statement env x)
        | None -> None
      in
      let _v4 = Common.map (attribute_specifier env) v4 in
      (id, fun t -> f (TFunction (t, v2)))
  | `Array_decl (v1, v2, v3, v4, v5) ->
      let id, f = declarator env v1 in
      let _v2 = token env v2 (* "[" *) in
      let _v3 = Common.map (type_qualifier env) v3 in
      let v4 =
        match v4 with
        | Some x -> Some (anon_choice_exp_508611b env x)
        | None -> None
      in
      let _v5 = token env v5 (* "]" *) in
      (id, fun t -> f (TArray (v4, t)))
  | `Paren_decl (v1, v2, v3) ->
      let _v1 = token env v1 (* "(" *) in
      let v2 = declarator env v2 in
      let _v3 = token env v3 (* ")" *) in
      v2
  | `Id tok ->
      let id = identifier env tok (* pattern [a-zA-Z_]\w* *) in
      (id, fun t -> t)

and enumerator (env : env) ((v1, v2) : CST.enumerator) : name * expr option =
  let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* "=" *) in
        let v2 = expression env v2 in
        Some v2
    | None -> None
  in
  (v1, v2)

and enumerator_list (env : env) ((v1, v2, v3, v4) : CST.enumerator_list) =
  let _v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = enumerator env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = enumerator env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 =
    match v3 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let _v4 = token env v4 (* "}" *) in
  v2

and expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Exp_not_bin x -> expression_not_binary env x
  | `Bin_exp x -> binary_expression env x

and expression_not_binary (env : env) (x : CST.expression_not_binary) =
  match x with
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "?" *) in
      let v3 =
        match v3 with
        | None -> None
        | Some x -> Some (expression env x)
      in
      let _v4 = token env v4 (* ":" *) in
      let v5 = expression env v5 in
      CondExpr (v1, v3, v5)
  | `Assign_exp (v1, v2, v3) ->
      let v1 = assignment_left_expression env v1 in
      let v2 =
        match v2 with
        | `EQ tok -> SimpleAssign (token env tok) (* "=" *)
        | `STAREQ tok -> OpAssign (Mul, token env tok) (* "*=" *)
        | `SLASHEQ tok -> OpAssign (Div, token env tok) (* "/=" *)
        | `PERCEQ tok -> OpAssign (Mod, token env tok) (* "%=" *)
        | `PLUSEQ tok -> OpAssign (Plus, token env tok) (* "+=" *)
        | `DASHEQ tok -> OpAssign (Minus, token env tok) (* "-=" *)
        | `LTLTEQ tok -> OpAssign (DecLeft, token env tok) (* "<<=" *)
        | `GTGTEQ tok -> OpAssign (DecRight, token env tok) (* ">>=" *)
        | `AMPEQ tok -> OpAssign (And, token env tok) (* "&=" *)
        | `HATEQ tok -> OpAssign (Xor, token env tok) (* "^=" *)
        | `BAREQ tok -> OpAssign (Or, token env tok)
        (* "|=" *)
      in
      let v3 = expression env v3 in
      Assign (v2, v1, v3)
  | `Un_exp (v1, v2) ->
      let v1 = anon_choice_BANG_67174d6 env v1 in
      let v2 = expression env v2 in
      Unary (v2, v1)
  | `Update_exp x -> update_expression env x
  | `Cast_exp (v1, v2, v3, v4) ->
      let _v1 = token env v1 (* "(" *) in
      let v2 = type_descriptor env v2 in
      let _v3 = token env v3 (* ")" *) in
      let v4 = expression env v4 in
      Cast (v2, v4)
  | `Poin_exp x -> pointer_expression env x
  | `Sizeof_exp (v1, v2) ->
      let v1 = token env v1 (* "sizeof" *) in
      let v2 =
        match v2 with
        | `Exp x -> fb [ Arg (expression env x) ]
        | `LPAR_type_desc_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 = type_descriptor env v2 in
            let v3 = token env v3 (* ")" *) in
            (v1, [ ArgType v2 ], v3)
      in
      Call (IdSpecial (SizeOf, v1), v2)
  | `Alig_exp (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | `X___alig__ tok -> token env tok
        | `X___alig tok -> token env tok
        | `X__alig tok -> token env tok
        | `Alig tok -> token env tok
        | `X__Alig tok -> token env tok
      in
      let v2 = (* "(" *) token env v2 in
      let v3 = type_descriptor env v3 in
      let v4 = (* ")" *) token env v4 in
      Call (IdSpecial (AlignOf, v1), (v2, [ ArgType v3 ], v4))
  | `Offs_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "offsetof" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = type_descriptor env v3 in
      let _v4 = (* "," *) token env v4 in
      let v5 =
        (* pattern (\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
        str env v5
      in
      let v6 = (* ")" *) token env v6 in
      Call (IdSpecial (OffsetOf, v1), (v2, [ ArgType v3; Arg (Id v5) ], v6))
  | `Gene_exp (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = (* "_Generic" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = expression env v3 in
      let _v4 = (* "," *) token env v4 in
      let v5 = type_descriptor env v5 in
      let _v6 = (* ":" *) token env v6 in
      let v7 = expression env v7 in
      let v8 =
        List.map
          (fun (v1, v2, v3, v4) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = type_descriptor env v2 in
            let _v3 = (* ":" *) token env v3 in
            let v4 = expression env v4 in
            (v2, v4))
          v8
      in
      let v9 = (* ")" *) token env v9 in
      Generic (v1, (v2, (v3, (v5, v7) :: v8), v9))
  | `Subs_exp x -> subscript_expression env x
  | `Call_exp x -> call_expression env x
  | `Field_exp x -> field_expression env x
  | `Comp_lit_exp (v1, v2, v3, v4) ->
      let _v1 = token env v1 (* "(" *) in
      let v2 = type_descriptor env v2 in
      let _v3 = token env v3 (* ")" *) in
      let v4 = initializer_list env v4 in
      GccConstructor (v2, v4)
  | `Id tok ->
      let id = identifier env tok (* pattern [a-zA-Z_]\w* *) in
      Id id
  | `Num_lit tok -> number_literal env tok (* number_literal *)
  | `Str_lit x ->
      let s = string_literal env x in
      String s
  | `True tok ->
      let t = token env tok (* true *) in
      Bool (true, t)
  | `False tok ->
      let t = token env tok (* false *) in
      Bool (false, t)
  | `Null x -> (
      match x with
      | `NULL tok ->
          let t = token env tok (* "NULL" *) in
          Null t
      | `Null tok ->
          let t = token env tok (* "nullptr" *) in
          Null t)
  | `Conc_str x -> concatenated_string env x
  | `Char_lit x ->
      let c = char_literal env x in
      Char c
  | `Paren_exp x -> parenthesized_expression env x
  (* GNU inline assembly is explicitly not an expression.
     This should be instead intercepted by expression_statement and toplevel_statement.
  *)
  | `Gnu_asm_exp _ ->
      (* TODO: add test *)
      failwith "found invalid inline assembler statement as expression"

and field_declaration_list (env : env)
    ((v1, v2, v3) : CST.field_declaration_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.concat_map (field_declaration_list_item env) v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and field_declaration_list_item (env : env)
    (x : CST.field_declaration_list_item) : field_def list =
  match x with
  | `Field_decl (v1, v2, v3, v4) ->
      let v1 = declaration_specifiers env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = field_declarator env v1 in
            let _v2_TODO =
              match v2 with
              | Some x -> Some (bitfield_clause env x)
              | None -> None
            in
            let v3 =
              Common.map
                (fun (v1, v2, v3) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = field_declarator env v2 in
                  let _v3_TODO =
                    match v3 with
                    | Some x -> Some (bitfield_clause env x)
                    | None -> None
                  in
                  v2)
                v3
            in
            v1 :: v3
        | None -> []
      in
      (* TODO *)
      let _v3 =
        match v3 with
        | Some x -> Some (attribute_specifier env x)
        | None -> None
      in
      let _v4 = token env v4 (* ";" *) in
      v2 |> Common.map (fun (id, f) -> { fld_name = Some id; fld_type = f v1 })
  | `Prep_def x ->
      let _ = preproc_def env x in
      []
  | `Prep_func_def x ->
      let _ = preproc_function_def env x in
      []
  | `Prep_call x ->
      let _ = preproc_call env x in
      []
  (* just taking the true branch *)
  | `Prep_if_in_field_decl_list (v1, v2, v3, v4, v5, v6) ->
      let _v1 = token env v1 (* pattern #[ 	]*if *) in
      let _v2 = preproc_expression env v2 in
      let _v3 = token env v3 (* "\n" *) in
      let v4 = List.concat_map (field_declaration_list_item env) v4 in
      let _v5 =
        match v5 with
        | Some x ->
            Some (anon_choice_prep_else_in_field_decl_list_97ea65e env x)
        | None -> None
      in
      let _v6 = token env v6 (* pattern #[ 	]*endif *) in
      v4
  (* just taking the first branch *)
  | `Prep_ifdef_in_field_decl_list (v1, v2, v3, v4, v5) ->
      let _v1 = anon_choice_pat_25b90ba_4a37f8c env v1 in
      let _v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
      let v3 = List.concat_map (field_declaration_list_item env) v3 in
      let _v4 =
        match v4 with
        | Some x -> (
            match x with
            | `Choice_prep_else_in_field_decl_list x ->
                Some (anon_choice_prep_else_in_field_decl_list_97ea65e env x)
            | `Prep_elif x ->
                let _ = preproc_elifdef env x in
                None)
        | None -> None
      in
      let _v5 = token env v5 (* pattern #[ 	]*endif *) in
      v3

(* argh, diff with regular declarator ? *)
and field_declarator (env : env) (x : CST.field_declarator) :
    name * (type_ -> type_) =
  match x with
  | `Attr_field_decl (v1, v2) ->
      let v1 = field_declarator env v1 in
      let _v2_TODO = List.map (attribute_declaration env) v2 in
      v1
  | `Poin_field_decl (v1, v2, v3, v4, v5) ->
      let _v1 =
        match v1 with
        | Some x -> Some (ms_based_modifier env x)
        | None -> None
      in
      let v2 = token env v2 (* "*" *) in
      let _v3 = Common.map (ms_pointer_modifier env) v3 in
      let _v4 = Common.map (type_qualifier env) v4 in
      let id, f = field_declarator env v5 in
      (id, fun t -> TPointer (v2, t) |> f)
  | `Func_field_decl (v1, v2) ->
      let id, f = field_declarator env v1 in
      let v2 = parameter_list env v2 in
      (id, fun t -> f (TFunction (t, v2)))
  | `Array_field_decl (v1, v2, v3, v4, v5) ->
      let id, f = field_declarator env v1 in
      let _v2 = token env v2 (* "[" *) in
      let _v3 = Common.map (type_qualifier env) v3 in
      let v4 =
        match v4 with
        | Some x -> Some (anon_choice_exp_508611b env x)
        | None -> None
      in
      let _v5 = token env v5 (* "]" *) in
      (id, fun t -> f (TArray (v4, t)))
  | `Paren_field_decl (v1, v2, v3) ->
      let _v1 = token env v1 (* "(" *) in
      let v2 = field_declarator env v2 in
      let _v3 = token env v3 (* ")" *) in
      v2
  | `Id tok ->
      let id = identifier env tok (* pattern [a-zA-Z_]\w* *) in
      (id, fun t -> t)

and field_expression (env : env) ((v1, v2, v3) : CST.field_expression) : expr =
  let v1 = expression env v1 in
  let v3 = identifier env v3 (* pattern [a-zA-Z_]\w* *) in
  let v2 =
    match v2 with
    | `DOT tok ->
        let t = token env tok (* "." *) in
        let v1 = Unary (v1, (GetRef, t)) in
        RecordPtAccess (v1, t, v3)
    | `DASHGT tok -> RecordPtAccess (v1, token env tok (* "->" *), v3)
  in
  v2

and initializer_list (env : env) ((v1, v2, v3, v4) : CST.initializer_list) :
    expr =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = anon_choice_init_pair_1a6981e env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = anon_choice_init_pair_1a6981e env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 =
    match v3 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let v4 = token env v4 (* "}" *) in
  (* TODO: can be RecordInit too! *)
  let elems =
    v2
    |> Common.map (function
         | Right e -> (None, e)
         | Left (designators, _tkeq, e) -> (
             match designators with
             | [ Left (_lc, idx, _rc) ] -> (Some idx, e)
             | _TODO -> (None, e)))
  in
  ArrayInit (v1, elems, v4)

and linkage_specification (env : env) ((v1, v2, v3) : CST.linkage_specification)
    =
  let _v1_TODO = (* "extern" *) token env v1 in
  let _v2_TODO = string_literal env v2 in
  let v3 =
    match v3 with
    | `Func_defi x ->
        let def = function_definition env x in
        [ DefStmt (FuncDef def) ]
    | `Decl x ->
        let vars = declaration env x in
        vars |> Common.map (fun v -> DefStmt (VarDef v))
    | `Decl_list x -> declaration_list env x
  in
  v3

and ms_based_modifier (env : env) ((v1, v2) : CST.ms_based_modifier) =
  let _v1 = token env v1 (* "__based" *) in
  let _v2 = argument_list env v2 in
  ()

and old_style_function_declarator (env : env)
    ((v1, v2) : CST.old_style_function_declarator) ty_hashtbl =
  let v1 = declarator env v1 in
  let v2 = old_style_parameter_list env v2 ty_hashtbl in
  (v1, v2)

and old_style_function_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.old_style_function_definition) =
  (* This is a K&R style function declaration, such as:
     int foo(a, p)
       int a;
       char *p;
     {
        return 0;
     }
  *)
  let _v1 =
    match v1 with
    | Some x -> Some (ms_call_modifier env x)
    | None -> None
  in
  let v2 = declaration_specifiers env v2 in
  let ty_hashtbl =
    (* This is a map from names of variables to their types.
       For instance, in the example above, it would map `a` to `int` and
       `p` to `char *`.
       We use this to instantiate the parameters to the function in
       `old_style_function_declarator` below.
    *)
    List.concat_map (declaration env) v4
    |> Common.map (arg_of_var_decl env)
    |> Common.map (fun ((s, _), ty) -> (s, ty))
    |> List.to_seq |> Hashtbl.of_seq
  in
  let (n, ret_ty_f), (_, params, _) =
    old_style_function_declarator env v3 ty_hashtbl
  in
  let v5 = compound_statement env v5 in
  let ret_ty = ret_ty_f v2 in
  [
    DefStmt
      (FuncDef
         {
           f_name = n;
           f_type = (ret_ty, params);
           f_body = v5;
           f_static = false (* TODO *);
         });
  ]

and parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) :
    parameter list =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = anon_choice_param_decl_4ac2852 env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = anon_choice_param_decl_4ac2852 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = token env v3 (* ")" *) in
  v2

and parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) =
  let _v1 = token env v1 (* "(" *) in
  let v2 = anon_choice_exp_55b4dba env v2 in
  let _v3 = token env v3 (* ")" *) in
  v2

and pointer_expression (env : env) ((v1, v2) : CST.pointer_expression) : expr =
  let v1 =
    match v1 with
    | `STAR tok -> (DeRef, token env tok) (* "*" *)
    | `AMP tok -> (GetRef, token env tok)
    (* "&" *)
  in
  let v2 = expression env v2 in
  Unary (v2, v1)

and preproc_elifdef (env : env) ((v1, v2, v3, v4) : CST.preproc_elifdef) =
  let v1 = anon_choice_pat_0307ca2_dbf6a9d env v1 in
  let v2 =
    (* pattern (\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *)
    Id (str env v2)
  in
  let v3 = List.concat_map (block_item env) v3 in
  let elifs, el =
    match v4 with
    | Some x -> anon_choice_prep_else_8b52b0f env x
    | None -> ([], None)
  in
  ((v1, v2, v3) :: elifs, el)

and preproc_if (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if) =
  let v1 = token env v1 (* pattern #[ 	]*if *) in
  let v2 = preproc_expression env v2 in
  let _v3 = token env v3 (* "\n" *) in
  let p_stmts = List.concat_map (block_item env) v4 in
  let p_elifs, p_else =
    match v5 with
    | Some x -> anon_choice_prep_else_8b52b0f env x
    | None -> ([], None)
  in
  let p_endif = token env v6 (* pattern #[ 	]*endif *) in
  PreprocStmt
    { p_condition = PreprocIf (v1, v2); p_stmts; p_elifs; p_else; p_endif }

and preproc_ifdef (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef) =
  let v1 = anon_choice_pat_25b90ba_4a37f8c env v1 in
  let v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
  let p_stmts = List.concat_map (block_item env) v3 in
  let p_elifs, p_else =
    match v4 with
    | Some x -> (
        match x with
        | `Choice_prep_else x -> anon_choice_prep_else_8b52b0f env x
        | `Prep_elif x -> preproc_elifdef env x)
    | None -> ([], None)
  in
  let p_endif = token env v5 (* pattern #[ 	]*endif *) in
  PreprocStmt
    { p_condition = PreprocIfdef (v1, v2); p_stmts; p_elifs; p_else; p_endif }

and subscript_designator (env : env) ((v1, v2, v3) : CST.subscript_designator) =
  let v1 = token env v1 (* "[" *) in
  let v2 = expression env v2 in
  let v3 = token env v3 (* "]" *) in
  (v1, v2, v3)

and subscript_expression (env : env)
    ((v1, v2, v3, v4) : CST.subscript_expression) : expr =
  let v1 = expression env v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = expression env v3 in
  let v4 = token env v4 (* "]" *) in
  ArrayAccess (v1, (v2, v3, v4))

and type_definition_declarators (env : env)
    ((v1, v2) : CST.type_definition_declarators) =
  let v1 = type_declarator env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = type_declarator env v2 in
        v2)
      v2
  in
  v1 :: v2

and type_definition_type (env : env) ((v1, v2, v3) : CST.type_definition_type) =
  let _v1_TODO = List.map (type_qualifier env) v1 in
  let v2 = type_specifier env v2 in
  let _v3_TODO = List.map (type_qualifier env) v3 in
  v2

and type_descriptor (env : env) ((v1, v2, v3, v4) : CST.type_descriptor) : type_
    =
  let _v1 = Common.map (type_qualifier env) v1 in
  let v2 = type_specifier env v2 in
  let _v3 = Common.map (type_qualifier env) v3 in
  let v4 =
    match v4 with
    | Some x -> (abstract_declarator env x) v2
    | None -> v2
  in
  v4

and type_specifier (env : env) (x : CST.type_specifier) : type_ =
  match x with
  | `Struct_spec (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "struct" *) in
      let _v2_TODO =
        match v2 with
        | Some x -> Some (attribute_specifier env x)
        | None -> None
      in
      let _v3 =
        match v3 with
        | Some x -> Some (ms_declspec_modifier env x)
        | None -> None
      in
      let nameopt, flds =
        map_anon_choice_type_id_opt_field_decl_list_9aebd83 env v4
      in
      let _v5_TODO =
        match v5 with
        | Some x -> Some (attribute_specifier env x)
        | None -> None
      in
      let env = env.extra in
      let name =
        match nameopt with
        | Some n -> n
        | None ->
            env.cnt <- env.cnt + 1;
            let s = gensym_struct env.cnt in
            (s, v1)
      in
      let def = { s_name = name; s_kind = Struct; s_flds = flds } in
      env.struct_defs_toadd <- def :: env.struct_defs_toadd;
      TStructName (Struct, name)
  | `Union_spec (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "union" *) in
      let _v2 =
        match v2 with
        | Some x -> Some (ms_declspec_modifier env x)
        | None -> None
      in
      let nameopt, flds =
        map_anon_choice_type_id_opt_field_decl_list_9aebd83 env v3
      in
      let _v4_TODO =
        match v4 with
        | Some x -> Some (attribute_specifier env x)
        | None -> None
      in
      let env = env.extra in
      let name =
        match nameopt with
        | Some n -> n
        | None ->
            env.cnt <- env.cnt + 1;
            let s = gensym_struct env.cnt in
            (s, v1)
      in
      let def = { s_name = name; s_kind = Union; s_flds = flds } in
      env.struct_defs_toadd <- def :: env.struct_defs_toadd;
      TStructName (Union, name)
  | `Enum_spec (v1, v2, v3) ->
      let v1 = token env v1 (* "enum" *) in
      let nameopt, tyopt, xs =
        match v2 with
        | `Id_opt_COLON_prim_type_opt_enum_list (v1, v2, v3) ->
            let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
            let v2 =
              match v2 with
              | Some (v1, v2) ->
                  (* This is specifying the size of the enum by choosing the underlying type. *)
                  let _v1 = (* ":" *) token env v1 in
                  let v2 = (* primitive_type *) str env v2 in
                  Some v2
              | None -> None
            in
            let v3 =
              match v3 with
              | Some x -> enumerator_list env x
              | None -> []
            in
            (Some v1, v2, v3)
        | `Enum_list x -> (None, None, enumerator_list env x)
      in
      let _v3_TODO =
        match v3 with
        | Some x -> Some (attribute_specifier env x)
        | None -> None
      in
      let env = env.extra in
      let name =
        match nameopt with
        | Some n -> n
        | None ->
            env.cnt <- env.cnt + 1;
            let s = gensym_enum env.cnt in
            (s, v1)
      in
      let def = { e_name = name; e_type = tyopt; e_consts = xs } in
      env.enum_defs_toadd <- def :: env.enum_defs_toadd;
      TEnumName name
  | `Macro_type_spec (v1, v2, v3, v4) ->
      let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = type_descriptor env v3 in
      let v4 = token env v4 (* ")" *) in
      TMacroApply (v1, (v2, v3, v4))
  | `Sized_type_spec (v1, v2) ->
      let v1 =
        (* repeat1 in grammar.js so at least one *)
        Common.map
          (fun x ->
            match x with
            | `Signed tok -> str env tok (* "signed" *)
            | `Unsi tok -> str env tok (* "unsigned" *)
            | `Long tok -> str env tok (* "long" *)
            | `Short tok -> str env tok
            (* "short" *))
          v1
      in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `Id tok -> [ identifier env tok ] (* pattern [a-zA-Z_]\w* *)
            | `Prim_type tok -> [ str env tok ] (* primitive_type *))
        | None -> []
      in
      let xs = v1 @ v2 in
      let s = xs |> Common.map fst |> String.concat " " in
      let ys = xs |> Common.map snd in
      (* repeat1 in grammar.js so Common.hd_exn "unexpected empty list" is safe *)
      let tk =
        Tok.combine_toks
          (Common.hd_exn "impossible!!!" ys)
          (Common.tl_exn "unexpected empty list" ys)
      in
      TBase (s, tk)
  | `Prim_type tok ->
      let t = str env tok (* primitive_type *) in
      TBase t
  | `Id tok ->
      let id = identifier env tok (* pattern [a-zA-Z_]\w* *) in
      TTypeName id

and update_expression (env : env) (x : CST.update_expression) =
  match x with
  | `Choice_DASHDASH_exp (v1, v2) ->
      let v1 = anon_choice_DASHDASH_d11def2 env v1 in
      let v2 = expression env v2 in
      Infix (v2, v1)
  | `Exp_choice_DASHDASH (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = anon_choice_DASHDASH_d11def2 env v2 in
      Postfix (v1, v2)

and expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v2 = token env v2 (* ";" *) in
  match v1 with
  | Some (`Exp (`Exp_not_bin (`Gnu_asm_exp x))) -> gnu_asm_statement env x v2
  | Some x -> ExprSt (anon_choice_exp_55b4dba env x, v2)
  | None -> ExprSt (Null v2, v2)

(* This is currently just used for the initialization statement in a for-loop, which I
   (brandon) have tested to not compile with an inline assembly expression.
*)
and expression_statement_as_expression (env : env)
    ((v1, v2) : CST.expression_statement) =
  let v2 = token env v2 (* ";" *) in
  match v1 with
  | Some (`Exp (`Exp_not_bin (`Gnu_asm_exp _))) ->
      failwith "expected expression, but got inline assembly"
  | Some x -> anon_choice_exp_55b4dba env x
  | None -> Null v2

(* diff with declarator? *)
and type_declarator (env : env) (x : CST.type_declarator) :
    name * (type_ -> type_) =
  match x with
  | `Attr_type_decl (v1, v2) ->
      let v1 = type_declarator env v1 in
      let _v2_TODO = List.map (attribute_declaration env) v2 in
      v1
  | `Poin_type_decl (v1, v2, v3, v4, v5) ->
      let _v1 =
        match v1 with
        | Some x -> Some (ms_based_modifier env x)
        | None -> None
      in
      let v2 = token env v2 (* "*" *) in
      let _v3 = Common.map (ms_pointer_modifier env) v3 in
      let _v4 = Common.map (type_qualifier env) v4 in
      let id, f = type_declarator env v5 in
      (id, fun t -> TPointer (v2, t) |> f)
  | `Func_type_decl (v1, v2) ->
      let id, f = type_declarator env v1 in
      let v2 = parameter_list env v2 in
      (id, fun t -> f (TFunction (t, v2)))
  | `Array_type_decl (v1, v2, v3, v4, v5) ->
      let id, f = type_declarator env v1 in
      let _v2 = token env v2 (* "[" *) in
      let _v3 = Common.map (type_qualifier env) v3 in
      let v4 =
        match v4 with
        | Some x -> Some (anon_choice_exp_508611b env x)
        | None -> None
      in
      let _v5 = token env v5 (* "]" *) in
      (id, fun t -> f (TArray (v4, t)))
  | `Paren_type_decl (v1, v2, v3) ->
      let _v1 = token env v1 (* "(" *) in
      let v2 = type_declarator env v2 in
      let _v3 = token env v3 (* ")" *) in
      v2
  | `Id tok ->
      let id = identifier env tok (* pattern [a-zA-Z_]\w* *) in
      (id, fun t -> t)
  (* THINK: Is this right? *)
  | `Choice_signed x -> (anon_choice_signed_a0bfc19 env x, fun t -> t)
  | `Prim_type tok ->
      (* primitive_type *)
      (str env tok, fun t -> t)

and anon_choice_decl_opt_gnu_asm_exp_2c80446 (env : env)
    (x : CST.anon_choice_decl_opt_gnu_asm_exp_2c80446) =
  match x with
  | `Decl_opt_gnu_asm_exp (v1, v2) ->
      let id, f = declarator env v1 in
      let _v2_TODO =
        match v2 with
        | Some x -> Some (gnu_asm_statement env x)
        | None -> None
      in
      fun t ->
        { v_name = id; v_type = f t; v_storage = DefaultStorage; v_init = None }
  | `Init_decl (v1, v2, v3) ->
      let id, f = declarator env v1 in
      let _v2 = token env v2 (* "=" *) in
      let v3 =
        match v3 with
        | `Init_list x -> initializer_list env x
        | `Exp x -> expression env x
      in
      fun t ->
        {
          v_name = id;
          v_type = f t;
          v_storage = DefaultStorage;
          v_init = Some v3;
        }

and type_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.type_definition)
    : type_def list =
  let _v1 =
    match v1 with
    | Some tok -> Some ((* "__extension__" *) token env tok)
    | None -> None
  in
  let _v2 = token env v2 (* "typedef" *) in
  let v3 = type_definition_type env v3 in
  let v4 = type_definition_declarators env v4 in
  let _v5_TODO = List.map (attribute_specifier env) v5 in
  let _v6 = (* ";" *) token env v6 in
  v4 |> Common.map (fun (id, f) -> { t_name = id; t_type = f v3 })

and declaration (env : env) ((v1, v2, v3) : CST.declaration) : var_decl list =
  let ty = declaration_specifiers env v1 in
  let v2 = declaration_declarator env v2 in
  let _v3 = (* ";" *) token env v3 in
  v2 |> Common.map (fun f -> f ty)

and arg_of_var_decl (_env : env)
    ({ v_name; v_type; v_storage = _; v_init = _ } : var_decl) =
  (v_name, v_type)

and declaration_declarator (env : env) ((v1, v2) : CST.declaration_declarator) =
  let v1 = anon_choice_decl_opt_gnu_asm_exp_2c80446 env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = anon_choice_decl_opt_gnu_asm_exp_2c80446 env v2 in
        v2)
      v2
  in
  v1 :: v2

and anon_choice_prep_else_8b52b0f (env : env)
    (x : CST.anon_choice_prep_else_8b52b0f) :
    (tok * expr * stmt list) list * stmt list option =
  match x with
  | `Prep_else (v1, v2) ->
      let _v1 = token env v1 (* pattern #[ 	]*else *) in
      let v2 = List.concat_map (block_item env) v2 in
      ([], Some v2)
  | `Prep_elif (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* pattern #[ 	]*elif *) in
      let v2 = preproc_expression env v2 in
      let _v3 = token env v3 (* "\n" *) in
      let v4 = List.concat_map (block_item env) v4 in
      let v5 =
        match v5 with
        | Some x ->
            let elifs, el = anon_choice_prep_else_8b52b0f env x in
            ((v1, v2, v4) :: elifs, el)
        | None -> ([], None)
      in
      v5

and case_statement (env : env) ((v1, v2, v3) : CST.case_statement) =
  let mk_case stmts =
    match v1 with
    | `Case_exp (v1, v2) ->
        let v1 = (* "case" *) token env v1 in
        let v2 = expression env v2 in
        Case (v1, v2, stmts)
    | `Defa tok -> Default ((* "default" *) token env tok, stmts)
  in
  let _v2 = (* ":" *) token env v2 in
  let v3 =
    List.concat_map
      (fun x ->
        match x with
        | `Choice_attr_stmt x -> [ non_case_statement env x ]
        | `Decl x ->
            let vars = declaration env x in
            vars |> Common.map (fun v -> DefStmt (VarDef v))
        | `Type_defi x ->
            type_definition env x
            |> Common.map (fun tydef -> DefStmt (TypeDef tydef)))
      v3
  in
  mk_case v3

and compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.concat_map (block_item env) v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

(* I've added this function *)
and compound_statement_for_switch (env : env)
    ((v1, v2, v3) : CST.compound_statement) : case list =
  let _v1 = token env v1 (* "{" *) in
  let v2 = List.concat_map (block_item env) v2 in
  let _v3 = token env v3 (* "}" *) in
  v2
  |> Common.map_filter (function
       | CaseStmt x -> Some x
       (* todo: we drop all the other stuff ... *)
       | _ -> None)

(* for extern "C" { ... } *)
and declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let _v1 = token env v1 (* "{" *) in
  let v2 = List.concat_map (block_item env) v2 in
  let _v3 = token env v3 (* "}" *) in
  v2

and function_definition (env : env) ((v1, v2, v3, v4) : CST.function_definition)
    : func_def =
  let _v1 =
    match v1 with
    | Some x -> Some (ms_call_modifier env x)
    | None -> None
  in
  let v2 = declaration_specifiers env v2 in
  let id, f = declarator env v3 in
  let v4 = compound_statement env v4 in
  match f v2 with
  | TFunction ft ->
      { f_name = id; f_type = ft; f_body = v4; f_static = false (* TODO *) }
  (* who does that ... probably a typedef *)
  | t ->
      {
        f_name = id;
        f_type = (t, []);
        f_body = v4;
        f_static = false (* TODO *);
      }

and attributed_statement (env : env) ((v1, v2) : CST.attributed_statement) =
  let _v1_TODO = List.map (attribute_declaration env) v1 in
  let v2 = statement env v2 in
  v2

and labeled_statement (env : env) ((v1, v2, v3) : CST.labeled_statement) : stmt
    =
  let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
  let _v2 = token env v2 (* ":" *) in
  let v3 = statement env v3 in
  Label (v1, v3)

and if_statement (env : env) ((v1, v2, v3, v4) : CST.if_statement) : stmt =
  let v1 = token env v1 (* "if" *) in
  let v2 = parenthesized_expression env v2 in
  let v3 = statement env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* "else" *) in
        let v2 = statement env v2 in
        Some v2
    | None -> None
  in
  If (v1, v2, v3, v4)

and switch_statement (env : env) ((v1, v2, v3) : CST.switch_statement) : stmt =
  let v1 = token env v1 (* "switch" *) in
  let v2 = parenthesized_expression env v2 in
  let v3 = compound_statement_for_switch env v3 in
  Switch (v1, v2, v3)

and do_statement (env : env) ((v1, v2, v3, v4, v5) : CST.do_statement) : stmt =
  let v1 = token env v1 (* "do" *) in
  let v2 = statement env v2 in
  let _v3 = token env v3 (* "while" *) in
  let v4 = parenthesized_expression env v4 in
  let _v5 = token env v5 (* ";" *) in
  DoWhile (v1, v2, v4)

and while_statement (env : env) ((v1, v2, v3) : CST.while_statement) : stmt =
  let v1 = token env v1 (* "while" *) in
  let v2 = parenthesized_expression env v2 in
  let v3 = statement env v3 in
  While (v1, v2, v3)

and for_statement (env : env) ((v1, v2, v3, v4, v5) : CST.for_statement) : stmt
    =
  let v1 = token env v1 (* "for" *) in
  let _v2 = token env v2 (* "(" *) in
  let x, y, z = for_statement_body env v3 in
  let _v4 = token env v4 (* ")" *) in
  let v5 = statement env v5 in
  For (v1, ForClassic (x, y, z), v5)

and for_statement_body (env : env) ((v1, v2, v3, v4) : CST.for_statement_body) =
  let v1 =
    (* both decl and expr_stmt contains the ending semicolon *)
    match v1 with
    | `Decl x ->
        let vars = declaration env x in
        Left vars
    | `Opt_choice_exp_SEMI x ->
        let e = expression_statement_as_expression env x in
        Right e
  in
  let v2 =
    match v2 with
    | Some x -> Some (anon_choice_exp_55b4dba env x)
    | None -> None
  in
  let _v3 = token env v3 (* ";" *) in
  let v4 =
    match v4 with
    | Some x -> Some (anon_choice_exp_55b4dba env x)
    | None -> None
  in
  (v1, v2, v4)

and return_statement (env : env) ((v1, v2, v3) : CST.return_statement) : stmt =
  let v1 = token env v1 (* "return" *) in
  let v2 =
    match v2 with
    | Some x -> Some (anon_choice_exp_55b4dba env x)
    | None -> None
  in
  let _v3 = token env v3 (* ";" *) in
  Return (v1, v2)

and break_statement (env : env) ((v1, v2) : CST.break_statement) : stmt =
  let v1 = token env v1 (* "break" *) in
  let _v2 = token env v2 (* ";" *) in
  Break v1

and continue_statement (env : env) ((v1, v2) : CST.continue_statement) : stmt =
  let v1 = token env v1 (* "continue" *) in
  let _v2 = token env v2 (* ";" *) in
  Continue v1

and goto_statement (env : env) ((v1, v2, v3) : CST.goto_statement) : stmt =
  let v1 = token env v1 (* "goto" *) in
  let v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
  let _v3 = token env v3 (* ";" *) in
  Goto (v1, v2)

and non_case_statement (env : env) (x : CST.non_case_statement) : stmt =
  match x with
  | `Attr_stmt x -> attributed_statement env x
  | `Labe_stmt x -> labeled_statement env x
  | `Comp_stmt x -> Block (compound_statement env x)
  | `Exp_stmt x -> expression_statement env x
  | `If_stmt x -> if_statement env x
  | `Switch_stmt x -> switch_statement env x
  | `Do_stmt x -> do_statement env x
  | `While_stmt x -> while_statement env x
  | `For_stmt x -> for_statement env x
  | `Ret_stmt x -> return_statement env x
  | `Brk_stmt x -> break_statement env x
  | `Cont_stmt x -> continue_statement env x
  | `Goto_stmt x -> goto_statement env x

and gnu_asm_statement (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.gnu_asm_expression) sc =
  let v1 =
    match v1 with
    | `Asm tok -> (* "asm" *) token env tok
    | `X___asm__ tok -> (* "__asm__" *) token env tok
  in
  let _v2_TODO = List.map (gnu_asm_qualifier env) v2 in
  let v3 = (* "(" *) token env v3 in
  let a_template =
    match v4 with
    | `Str_lit x -> Id (string_literal env x)
    | `Conc_str x -> concatenated_string env x
  in
  let a_outputs, a_inputs, a_clobbers, a_gotos =
    match v5 with
    | Some (v1, v2) ->
        let outputs = gnu_asm_output_operand_list env v1 in
        let inputs, clobbers, gotos =
          match v2 with
          | Some (v1, v2) ->
              let inputs = gnu_asm_input_operand_list env v1 in
              let clobbers, gotos =
                match v2 with
                | Some (v1, v2) ->
                    let clobbers = gnu_asm_clobber_list env v1 in
                    let gotos =
                      match v2 with
                      | Some x -> gnu_asm_goto_list env x
                      | None -> []
                    in
                    (clobbers, gotos)
                | None -> ([], [])
              in
              (inputs, clobbers, gotos)
          | None -> ([], [], [])
        in
        (outputs, inputs, clobbers, gotos)
    | None -> ([], [], [], [])
  in
  let v6 = (* ")" *) token env v6 in
  AsmStmt
    (v1, (v3, { a_template; a_inputs; a_outputs; a_clobbers; a_gotos }, v6), sc)

and top_level_statement (env : env) (x : CST.top_level_statement) =
  match x with
  | `Case_stmt x -> CaseStmt (case_statement env x)
  | `Attr_stmt x -> attributed_statement env x
  | `Labe_stmt x -> labeled_statement env x
  | `Comp_stmt x -> Block (compound_statement env x)
  | `Top_level_exp_stmt (`Gnu_asm_exp v1, v2) ->
      gnu_asm_statement env v1 (token env v2)
  | `Top_level_exp_stmt (v1, v2) ->
      let v1 = expression_not_binary env v1 in
      let v2 = (* ";" *) token env v2 in
      ExprSt (v1, v2)
  | `If_stmt x -> if_statement env x
  | `Switch_stmt x -> switch_statement env x
  | `Do_stmt x -> do_statement env x
  | `While_stmt x -> while_statement env x
  | `For_stmt x -> for_statement env x
  | `Ret_stmt x -> return_statement env x
  | `Brk_stmt x -> break_statement env x
  | `Cont_stmt x -> continue_statement env x
  | `Goto_stmt x -> goto_statement env x

and statement env x : stmt =
  match statement2 env x with
  | Left case ->
      (* TODO: weird case where regular stmt expected *)
      case_to_stmt case
  | Right st -> st

and case_to_stmt x = CaseStmt x

and statement2 (env : env) (x : CST.statement) : (case, stmt) Common.either =
  match x with
  | `Case_stmt (v1, v2, v3) ->
      let _v2 = token env v2 (* ":" *) in
      let v3 =
        List.concat_map
          (fun x ->
            match x with
            | `Choice_attr_stmt x -> [ non_case_statement env x ]
            | `Decl x ->
                let vars = declaration env x in
                [ Vars vars ]
            | `Type_defi x ->
                let xs = type_definition env x in
                xs |> Common.map (fun t -> DefStmt (TypeDef t)))
          v3
      in

      let v1 =
        match v1 with
        | `Case_exp (v1, v2) ->
            let v1 = token env v1 (* "case" *) in
            let v2 = expression env v2 in
            Left (Case (v1, v2, v3))
        | `Defa tok ->
            let t = token env tok (* "default" *) in
            Left (Default (t, v3))
      in
      v1
  | `Choice_attr_stmt x -> Right (non_case_statement env x)

and top_level_item (env : env) (x : CST.top_level_item) : toplevel list =
  match x with
  | `Func_defi x ->
      let def = function_definition env x in
      [ DefStmt (FuncDef def) ]
  | `Old_style_func_defi x -> old_style_function_definition env x
  (* less: could transform as yet another annotation *)
  | `Link_spec (v1, v2, v3) ->
      let _v1 = token env v1 (* "extern" *) in
      let _v2 = string_literal env v2 in
      let v3 =
        match v3 with
        | `Func_defi x -> [ DefStmt (FuncDef (function_definition env x)) ]
        | `Decl x ->
            let vars = declaration env x in
            vars |> Common.map (fun v -> DefStmt (VarDef v))
        | `Decl_list x -> declaration_list env x
      in
      v3
  | `Decl x ->
      let vars = declaration env x in
      vars |> Common.map (fun v -> DefStmt (VarDef v))
  | `Choice_case_stmt x ->
      let st = top_level_statement env x in
      [ st ]
  | `Attr_stmt x -> [ attributed_statement env x ]
  | `Type_defi x ->
      let xs = type_definition env x in
      xs |> Common.map (fun x -> DefStmt (TypeDef x))
  | `Empty_decl (v1, v2) ->
      let _v1 = type_specifier env v1 in
      let _v2 = token env v2 (* ";" *) in
      []
  (* skipping else part *)
  | `Prep_if x -> [ preproc_if env x ]
  | `Prep_ifdef x -> [ preproc_ifdef env x ]
  | `Prep_incl (v1, v2, v3) ->
      let v1 = token env v1 (* pattern #[ 	]*include *) in
      let v2 =
        match v2 with
        (* "foo.h" *)
        | `Str_lit x ->
            let s = string_literal env x in
            IncludePath s
        (* <foo.h> *)
        | `System_lib_str tok ->
            let s = str env tok (* system_lib_string *) in
            IncludePath s
        | `Id tok ->
            let id = identifier env tok (* pattern [a-zA-Z_]\w* *) in
            IncludePath id
        | `Prep_call_exp x ->
            let x = preproc_call_expression env x in
            IncludeCall x
      in
      let _v3 = token env v3 (* "\n" *) in
      [ DirStmt (Include (v1, v2)) ]
  | `Prep_def x -> [ DirStmt (preproc_def env x) ]
  | `Prep_func_def x -> [ DirStmt (preproc_function_def env x) ]
  | `Prep_call x -> [ DirStmt (preproc_call env x) ]

and translation_unit (env : env) (xs : CST.translation_unit) : program =
  List.concat_map
    (fun x ->
      let res = top_level_item env x in
      let env = env.extra in

      let structs = env.struct_defs_toadd in
      let enums = env.enum_defs_toadd in
      let typedefs = env.typedefs_toadd in
      env.struct_defs_toadd <- [];
      env.enum_defs_toadd <- [];
      env.typedefs_toadd <- [];
      ((structs |> Common.map (fun x -> StructDef x))
       @ (enums |> Common.map (fun x -> EnumDef x))
       @ (typedefs |> Common.map (fun x -> TypeDef x))
      |> Common.map (fun x -> DefStmt x))
      @ res)
    xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_c.Parse.file file)
    (fun cst ->
      let env =
        { H.file; conv = H.line_col_to_pos file; extra = default_extra_env }
      in
      translation_unit env cst)

let parse_pattern str =
  (* Note that we're using cpp tree-sitter here. Technically, cpp
     tree-sitter also covers C syntax, and we have implemented a more
     mature Semgrep syntax extension within cpp tree-sitter. *)
  let result = Parse_cpp_tree_sitter.parse_pattern str in
  match result with
  | { program = Some any; _ } ->
      { result with program = Some (Ast_c_build.any any) }
  | { program = None; _ } -> { result with program = None }

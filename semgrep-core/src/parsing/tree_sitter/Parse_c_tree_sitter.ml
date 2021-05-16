(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
module AST = Ast_c
module CST = Tree_sitter_c.CST
module PI = Parse_info
open Cst_cpp
open Ast_c
module G = AST_generic
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* C parser using tree-sitter-lang/semgrep-c and converting
 * to pfff/lang_c/parsing/ast_c.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * pfff/lang_c/analyze/c_to_generic.ml
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
  | `Vola tok -> token env tok (* "volatile" *)
  | `Rest tok -> token env tok (* "restrict" *)
  | `X__Atomic tok -> token env tok

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
    List.map
      (fun x ->
        match x with
        | `Imm_tok_pat_c7f65b4 tok -> str env tok (* pattern "[^\\\\\"\\n]+" *)
        | `Esc_seq tok -> str env tok
        (* escape_sequence *))
      v2
  in
  let v3 = token env v3 (* "\"" *) in
  let s = v2 |> List.map fst |> String.concat "" in
  (s, PI.combine_infos v1 (List.map snd v2 @ [ v3 ]))

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
  (s, PI.combine_infos v1 [ snd v2; v3 ])

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

let ms_declspec_modifier (env : env)
    ((v1, v2, v3, v4) : CST.ms_declspec_modifier) =
  let _v1 = token env v1 (* "__declspec" *) in
  let _v2 = token env v2 (* "(" *) in
  let _v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  let _v4 = token env v4 (* ")" *) in
  ()

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
          List.map
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
  (v1, v2 |> List.map (fun x -> Arg x), v3)

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
  match H.int_of_string_c_octal_opt s with
  | Some i -> Int (Some i, t)
  | None -> (
      match float_of_string_opt s with
      | Some f -> Float (Some f, t)
      (* could be None because of a suffix in the string *)
      | None -> Int (None, t) )

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
          List.map
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
      let _v2 = List.map (type_qualifier env) v2 in
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
      let _v3 = List.map (type_qualifier env) v3 in
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

and anon_choice_init_pair_1a6981e (env : env)
    (x : CST.anon_choice_init_pair_1a6981e) =
  match x with
  | `Init_pair (v1, v2, v3) ->
      let v1 =
        List.map
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

and anon_choice_param_decl_bdc8cc9 (env : env)
    (x : CST.anon_choice_param_decl_bdc8cc9) : parameter =
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
                { p_type = f v1; p_name = None } )
        | None -> { p_type = v1; p_name = None }
      in
      ParamClassic v2
  | `DOTDOTDOT tok ->
      let t = token env tok (* "..." *) in
      ParamDots t

and anon_choice_prep_else_in_field_decl_list_97ea65e (env : env)
    (x : CST.anon_choice_prep_else_in_field_decl_list_97ea65e) =
  match x with
  | `Prep_else_in_field_decl_list (v1, v2) ->
      let _v1 = token env v1 (* pattern #[ 	]*else *) in
      let _v2 = List.map (field_declaration_list_item env) v2 in
      ()
  | `Prep_elif_in_field_decl_list (v1, v2, v3, v4, v5) ->
      let _v1 = token env v1 (* pattern #[ 	]*elif *) in
      let _v2 = preproc_expression env v2 in
      let _v3 = token env v3 (* "\n" *) in
      let _v4 = List.map (field_declaration_list_item env) v4 in
      let _v5 =
        match v5 with
        | Some x ->
            Some (anon_choice_prep_else_in_field_decl_list_97ea65e env x)
        | None -> None
      in
      ()

and anon_choice_stor_class_spec_5764fed (env : env)
    (x : CST.anon_choice_stor_class_spec_5764fed) =
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
  | `Ms_decl_modi x ->
      let _ = ms_declspec_modifier env x in
      ()

and anon_choice_type_id_opt_field_decl_list_9aebd83 (env : env)
    (x : CST.anon_choice_type_id_opt_field_decl_list_9aebd83) :
    name option * field_def list bracket =
  match x with
  | `Id_opt_field_decl_list (v1, v2) ->
      (* named struct/union *)
      let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
      let v2 =
        match v2 with
        | Some x -> field_declaration_list env x
        | None -> G.fake_bracket []
      in
      (Some v1, v2)
  | `Field_decl_list x -> (None, field_declaration_list env x)

and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) :
    argument list bracket =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = expression env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = expression env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2 |> List.map (fun x -> Arg x), v3)

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

and call_expression (env : env) ((v1, v2) : CST.call_expression) : expr =
  let v1 = expression env v1 in
  let v2 = argument_list env v2 in
  Call (v1, v2)

and declaration_specifiers (env : env)
    ((v1, v2, v3) : CST.declaration_specifiers) : type_ =
  let _v1 = List.map (anon_choice_stor_class_spec_5764fed env) v1 in
  let v2 = type_specifier env v2 in
  let _v3 = List.map (anon_choice_stor_class_spec_5764fed env) v3 in
  v2

(* return a couple (name * partial type (to be applied to return type)) *)
and declarator (env : env) (x : CST.declarator) : name * (type_ -> type_) =
  match x with
  | `Poin_decl (v1, v2, v3, v4, v5) ->
      let _v1 =
        match v1 with Some x -> Some (ms_based_modifier env x) | None -> None
      in
      let v2 = token env v2 (* "*" *) in
      let _v3 = List.map (ms_pointer_modifier env) v3 in
      let _v4 = List.map (type_qualifier env) v4 in
      let id, f = declarator env v5 in
      (id, fun t -> TPointer (v2, t) |> f)
  | `Func_decl (v1, v2, v3) ->
      let id, f = declarator env v1 in
      let v2 = parameter_list env v2 in
      let _v3 = List.map (attribute_specifier env) v3 in
      (id, fun t -> f (TFunction (t, v2)))
  | `Array_decl (v1, v2, v3, v4, v5) ->
      let id, f = declarator env v1 in
      let _v2 = token env v2 (* "[" *) in
      let _v3 = List.map (type_qualifier env) v3 in
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
          List.map
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
    match v3 with Some tok -> Some (token env tok) (* "," *) | None -> None
  in
  let _v4 = token env v4 (* "}" *) in
  v2

and expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "?" *) in
      let v3 = expression env v3 in
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
  | `Bin_exp x -> binary_expression env x
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
        | `Exp x -> Left (expression env x)
        | `LPAR_type_desc_RPAR (v1, v2, v3) ->
            let _v1 = token env v1 (* "(" *) in
            let v2 = type_descriptor env v2 in
            let _v3 = token env v3 (* ")" *) in
            Right v2
      in
      SizeOf (v1, v2)
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
  | `Null tok ->
      let t = token env tok (* "NULL" *) in
      Null t
  | `Conc_str (v1, v2) ->
      let v1 = string_literal env v1 in
      let v2 = List.map (string_literal env) v2 in
      ConcatString (v1 :: v2)
  | `Char_lit x ->
      let c = char_literal env x in
      Char c
  | `Paren_exp x -> parenthesized_expression env x

and field_declaration_list (env : env)
    ((v1, v2, v3) : CST.field_declaration_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (field_declaration_list_item env) v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, List.flatten v2, v3)

and field_declaration_list_item (env : env)
    (x : CST.field_declaration_list_item) : field_def list =
  match x with
  | `Field_decl (v1, v2, v3, v4) ->
      let v1 = declaration_specifiers env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = field_declarator env v1 in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = field_declarator env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let _v3 =
        match v3 with Some x -> Some (bitfield_clause env x) | None -> None
      in
      let _v4 = token env v4 (* ";" *) in
      v2 |> List.map (fun (id, f) -> { fld_name = Some id; fld_type = f v1 })
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
      let v4 = List.map (field_declaration_list_item env) v4 in
      let _v5 =
        match v5 with
        | Some x ->
            Some (anon_choice_prep_else_in_field_decl_list_97ea65e env x)
        | None -> None
      in
      let _v6 = token env v6 (* pattern #[ 	]*endif *) in
      v4 |> List.flatten
  (* just taking the first branch *)
  | `Prep_ifdef_in_field_decl_list (v1, v2, v3, v4, v5) ->
      let _v1 = anon_choice_pat_25b90ba_4a37f8c env v1 in
      let _v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
      let v3 = List.map (field_declaration_list_item env) v3 in
      let _v4 =
        match v4 with
        | Some x ->
            Some (anon_choice_prep_else_in_field_decl_list_97ea65e env x)
        | None -> None
      in
      let _v5 = token env v5 (* pattern #[ 	]*endif *) in
      v3 |> List.flatten

(* argh, diff with regular declarator ? *)
and field_declarator (env : env) (x : CST.field_declarator) :
    name * (type_ -> type_) =
  match x with
  | `Poin_field_decl (v1, v2, v3, v4, v5) ->
      let _v1 =
        match v1 with Some x -> Some (ms_based_modifier env x) | None -> None
      in
      let v2 = token env v2 (* "*" *) in
      let _v3 = List.map (ms_pointer_modifier env) v3 in
      let _v4 = List.map (type_qualifier env) v4 in
      let id, f = field_declarator env v5 in
      (id, fun t -> TPointer (v2, t) |> f)
  | `Func_field_decl (v1, v2) ->
      let id, f = field_declarator env v1 in
      let v2 = parameter_list env v2 in
      (id, fun t -> f (TFunction (t, v2)))
  | `Array_field_decl (v1, v2, v3, v4, v5) ->
      let id, f = field_declarator env v1 in
      let _v2 = token env v2 (* "[" *) in
      let _v3 = List.map (type_qualifier env) v3 in
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
          List.map
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
    match v3 with Some tok -> Some (token env tok) (* "," *) | None -> None
  in
  let v4 = token env v4 (* "}" *) in
  (* TODO: can be RecordInit too! *)
  let elems =
    v2
    |> List.map (function
         | Right e -> (None, e)
         | Left (designators, _tkeq, e) -> (
             match designators with
             | [ Left (_lc, idx, _rc) ] -> (Some idx, e)
             | _TODO -> (None, e) ))
  in
  ArrayInit (v1, elems, v4)

and ms_based_modifier (env : env) ((v1, v2) : CST.ms_based_modifier) =
  let _v1 = token env v1 (* "__based" *) in
  let _v2 = argument_list env v2 in
  ()

and parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) :
    parameter list =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = anon_choice_param_decl_bdc8cc9 env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = anon_choice_param_decl_bdc8cc9 env v2 in
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

and type_descriptor (env : env) ((v1, v2, v3, v4) : CST.type_descriptor) : type_
    =
  let _v1 = List.map (type_qualifier env) v1 in
  let v2 = type_specifier env v2 in
  let _v3 = List.map (type_qualifier env) v3 in
  let v4 =
    match v4 with Some x -> (abstract_declarator env x) v2 | None -> v2
  in
  v4

and type_specifier (env : env) (x : CST.type_specifier) : type_ =
  match x with
  | `Struct_spec (v1, v2, v3) ->
      let v1 = token env v1 (* "struct" *) in
      let _v2 =
        match v2 with
        | Some x -> Some (ms_declspec_modifier env x)
        | None -> None
      in
      let nameopt, flds =
        anon_choice_type_id_opt_field_decl_list_9aebd83 env v3
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
  | `Union_spec (v1, v2, v3) ->
      let v1 = token env v1 (* "union" *) in
      let _v2 =
        match v2 with
        | Some x -> Some (ms_declspec_modifier env x)
        | None -> None
      in
      let nameopt, flds =
        anon_choice_type_id_opt_field_decl_list_9aebd83 env v3
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
  | `Enum_spec (v1, v2) ->
      let v1 = token env v1 (* "enum" *) in
      let nameopt, xs =
        match v2 with
        | `Id_opt_enum_list (v1, v2) ->
            let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
            let v2 =
              match v2 with Some x -> enumerator_list env x | None -> []
            in
            (Some v1, v2)
        | `Enum_list x -> (None, enumerator_list env x)
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
      let def = { e_name = name; e_consts = xs } in
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
        List.map
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
            | `Prim_type tok -> [ str env tok ] (* primitive_type *) )
        | None -> []
      in
      let xs = v1 @ v2 in
      let s = xs |> List.map fst |> String.concat " " in
      let ys = xs |> List.map snd in
      (* repeat1 in grammar.js so List.hd is safe *)
      let tk = PI.combine_infos (List.hd ys) (List.tl ys) in
      TBase (s, tk)
  | `Prim_type tok ->
      let t = str env tok (* primitive_type *) in
      TBase t
  | `Id tok ->
      let id = identifier env tok (* pattern [a-zA-Z_]\w* *) in
      TBase id

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

let expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 =
    match v1 with
    | Some x -> Some (anon_choice_exp_55b4dba env x)
    | None -> None
  in
  let v2 = token env v2 (* ";" *) in
  match v1 with Some e -> (e, v2) | None -> (Null v2, v2)

(* diff with declarator? *)
let rec type_declarator (env : env) (x : CST.type_declarator) :
    name * (type_ -> type_) =
  match x with
  | `Poin_type_decl (v1, v2, v3, v4, v5) ->
      let _v1 =
        match v1 with Some x -> Some (ms_based_modifier env x) | None -> None
      in
      let v2 = token env v2 (* "*" *) in
      let _v3 = List.map (ms_pointer_modifier env) v3 in
      let _v4 = List.map (type_qualifier env) v4 in
      let id, f = type_declarator env v5 in
      (id, fun t -> TPointer (v2, t) |> f)
  | `Func_type_decl (v1, v2) ->
      let id, f = type_declarator env v1 in
      let v2 = parameter_list env v2 in
      (id, fun t -> f (TFunction (t, v2)))
  | `Array_type_decl (v1, v2, v3, v4, v5) ->
      let id, f = type_declarator env v1 in
      let _v2 = token env v2 (* "[" *) in
      let _v3 = List.map (type_qualifier env) v3 in
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

let anon_choice_decl_f8b0ff3 (env : env) (x : CST.anon_choice_decl_f8b0ff3) =
  match x with
  | `Decl x ->
      let id, f = declarator env x in
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

let type_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.type_definition)
    : type_def list =
  let _v1 = token env v1 (* "typedef" *) in
  let _v2 = List.map (type_qualifier env) v2 in
  let v3 = type_specifier env v3 in
  let v4 = type_declarator env v4 in
  let v5 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_declarator env v2 in
        v2)
      v5
  in
  let xs = v4 :: v5 in
  let _v6 = token env v6 (* ";" *) in
  xs |> List.map (fun (id, f) -> { t_name = id; t_type = f v3 })

let declaration (env : env) ((v1, v2, v3, v4) : CST.declaration) : var_decl list
    =
  let v1 = declaration_specifiers env v1 in
  let v2 = anon_choice_decl_f8b0ff3 env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = anon_choice_decl_f8b0ff3 env v2 in
        v2)
      v3
  in
  let xs = v2 :: v3 in
  let _v4 = token env v4 (* ";" *) in
  xs |> List.map (fun f -> f v1)

let rec anon_choice_prep_else_8b52b0f (env : env)
    (x : CST.anon_choice_prep_else_8b52b0f) =
  match x with
  | `Prep_else (v1, v2) ->
      let _v1 = token env v1 (* pattern #[ 	]*else *) in
      let _v2 = translation_unit env v2 in
      ()
  | `Prep_elif (v1, v2, v3, v4, v5) ->
      let _v1 = token env v1 (* pattern #[ 	]*elif *) in
      let _v2 = preproc_expression env v2 in
      let _v3 = token env v3 (* "\n" *) in
      let _v4 = translation_unit env v4 in
      let _v5 =
        match v5 with
        | Some x -> Some (anon_choice_prep_else_8b52b0f env x)
        | None -> None
      in
      ()

and compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = token env v1 (* "{" *) in
  let v2 = translation_unit env v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

(* I've added this function *)
and compound_statement_for_switch (env : env)
    ((v1, v2, v3) : CST.compound_statement) : case list =
  let _v1 = token env v1 (* "{" *) in
  let v2 = translation_unit env v2 in
  let _v3 = token env v3 (* "}" *) in
  v2
  |> Common.map_filter (function
       | CaseStmt x -> Some x
       (* todo: we drop all the other stuff ... *)
       | _ -> None)

(* for extern "C" { ... } *)
and declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let _v1 = token env v1 (* "{" *) in
  let v2 = translation_unit env v2 in
  let _v3 = token env v3 (* "}" *) in
  v2

and function_definition (env : env) ((v1, v2, v3, v4) : CST.function_definition)
    : func_def =
  let _v1 =
    match v1 with Some x -> Some (ms_call_modifier env x) | None -> None
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

and non_case_statement (env : env) (x : CST.non_case_statement) : stmt =
  match x with
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
      let _v2 = token env v2 (* ":" *) in
      let v3 = statement env v3 in
      Label (v1, v3)
  | `Comp_stmt x -> Block (compound_statement env x)
  | `Exp_stmt x ->
      let e, t = expression_statement env x in
      ExprSt (e, t)
  | `If_stmt (v1, v2, v3, v4) ->
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
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = compound_statement_for_switch env v3 in
      Switch (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = statement env v2 in
      let _v3 = token env v3 (* "while" *) in
      let v4 = parenthesized_expression env v4 in
      let _v5 = token env v5 (* ";" *) in
      DoWhile (v1, v2, v4)
  | `While_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement env v3 in
      While (v1, v2, v3)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "for" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 =
        (* both decl and expr_stmt contains the ending semicolon *)
        match v3 with
        | `Decl x ->
            let vars = declaration env x in
            Left vars
        | `Opt_choice_exp_SEMI x ->
            let e, _semi = expression_statement env x in
            Right e
      in
      let v4 =
        match v4 with Some x -> Some (expression env x) | None -> None
      in
      let _v5 = token env v5 (* ";" *) in
      let v6 =
        match v6 with
        | Some x -> Some (anon_choice_exp_55b4dba env x)
        | None -> None
      in
      let _v7 = token env v7 (* ")" *) in
      let v8 = statement env v8 in
      For (v1, v3, v4, v6, v8)
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        match v2 with
        | Some x -> Some (anon_choice_exp_55b4dba env x)
        | None -> None
      in
      let _v3 = token env v3 (* ";" *) in
      Return (v1, v2)
  | `Brk_stmt (v1, v2) ->
      let v1 = token env v1 (* "break" *) in
      let _v2 = token env v2 (* ";" *) in
      Break v1
  | `Cont_stmt (v1, v2) ->
      let v1 = token env v1 (* "continue" *) in
      let _v2 = token env v2 (* ";" *) in
      Continue v1
  | `Goto_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "goto" *) in
      let v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
      let _v3 = token env v3 (* ";" *) in
      Goto (v1, v2)

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
        List.map
          (fun x ->
            match x with
            | `Choice_labe_stmt x -> [ non_case_statement env x ]
            | `Decl x ->
                let vars = declaration env x in
                [ Vars vars ]
            | `Type_defi x ->
                let xs = type_definition env x in
                xs |> List.map (fun t -> DefStmt (TypeDef t)))
          v3
        |> List.flatten
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
  | `Choice_labe_stmt x -> Right (non_case_statement env x)

and top_level_item (env : env) (x : CST.top_level_item) : toplevel list =
  match x with
  | `Func_defi x ->
      let def = function_definition env x in
      [ DefStmt (FuncDef def) ]
  (* less: could transform as yet another annotation *)
  | `Link_spec (v1, v2, v3) ->
      let _v1 = token env v1 (* "extern" *) in
      let _v2 = string_literal env v2 in
      let v3 =
        match v3 with
        | `Func_defi x -> [ DefStmt (FuncDef (function_definition env x)) ]
        | `Decl x ->
            let vars = declaration env x in
            vars |> List.map (fun v -> DefStmt (VarDef v))
        | `Decl_list x -> declaration_list env x
      in
      v3
  | `Decl x ->
      let vars = declaration env x in
      vars |> List.map (fun v -> DefStmt (VarDef v))
  | `Choice_case_stmt x ->
      let st = statement env x in
      [ st ]
  | `Type_defi x ->
      let xs = type_definition env x in
      xs |> List.map (fun x -> DefStmt (TypeDef x))
  | `Empty_decl (v1, v2) ->
      let _v1 = type_specifier env v1 in
      let _v2 = token env v2 (* ";" *) in
      []
  (* skipping else part *)
  | `Prep_if (v1, v2, v3, v4, v5, v6) ->
      let _v1 = token env v1 (* pattern #[ 	]*if *) in
      let _v2 = preproc_expression env v2 in
      let _v3 = token env v3 (* "\n" *) in
      let v4 = translation_unit env v4 in
      let _v5 =
        match v5 with
        | Some x -> Some (anon_choice_prep_else_8b52b0f env x)
        | None -> None
      in
      let _v6 = token env v6 (* pattern #[ 	]*endif *) in
      v4
  | `Prep_ifdef (v1, v2, v3, v4, v5) ->
      let _v1 = anon_choice_pat_25b90ba_4a37f8c env v1 in
      let _v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
      let v3 = translation_unit env v3 in
      let _v4 =
        match v4 with
        | Some x -> Some (anon_choice_prep_else_8b52b0f env x)
        | None -> None
      in
      let _v5 = token env v5 (* pattern #[ 	]*endif *) in
      v3
  | `Prep_incl (v1, v2, v3) ->
      let v1 = token env v1 (* pattern #[ 	]*include *) in
      let v2 =
        match v2 with
        (* "foo.h" *)
        | `Str_lit x ->
            let s = string_literal env x in
            s
        (* <foo.h> *)
        | `System_lib_str tok ->
            let s = str env tok (* system_lib_string *) in
            s
        | `Id tok ->
            let id = identifier env tok (* pattern [a-zA-Z_]\w* *) in
            id
        | `Prep_call_exp x ->
            let _ = preproc_call_expression env x in
            ("PREPROC_EXPR", v1)
      in
      let _v3 = token env v3 (* "\n" *) in
      [ DirStmt (Include (v1, v2)) ]
  | `Prep_def x -> [ DirStmt (preproc_def env x) ]
  | `Prep_func_def x -> [ DirStmt (preproc_function_def env x) ]
  | `Prep_call x -> [ DirStmt (preproc_call env x) ]

and translation_unit (env : env) (xs : CST.translation_unit) : program =
  List.map
    (fun x ->
      let res = top_level_item env x in
      let env = env.extra in

      let structs = env.struct_defs_toadd in
      let enums = env.enum_defs_toadd in
      let typedefs = env.typedefs_toadd in
      env.struct_defs_toadd <- [];
      env.enum_defs_toadd <- [];
      env.typedefs_toadd <- [];
      ( (structs |> List.map (fun x -> StructDef x))
        @ (enums |> List.map (fun x -> EnumDef x))
        @ (typedefs |> List.map (fun x -> TypeDef x))
      |> List.map (fun x -> DefStmt x) )
      @ res)
    xs
  |> List.flatten

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_c.Parse.file file ())
    (fun cst ->
      let env =
        { H.file; conv = H.line_col_to_pos file; extra = default_extra_env }
      in
      translation_unit env cst)

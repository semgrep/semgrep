(* Yoann Padioleau
 *
 * Copyright (c) 2021 R2C
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
open Common
module PI = Parse_info
module CST = Tree_sitter_cpp.CST
module H = Parse_tree_sitter_helpers
module H2 = Parser_cpp_mly_helper
open Ast_cpp

(* module G2 = AST_generic_ *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* OCaml parser using tree-sitter-lang/semgrep-cpp and converting
 * to pfff/lang_cpp/parsing/cst_cpp.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * a future cpp_to_generic.ml
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let token = H.token

let _fake = PI.fake_info

let fb = PI.fake_bracket

let str = H.str

(* for declarators *)
let id x = x

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

let parse_operator _env (s, t) = failwith "TODO"

let parse_number_literal (s, t) = failwith "TODO"

let parse_primitive_type (s, t) = failwith "TODO"

(* name builder helpers *)

let name_for_param name =
  match name with
  | None, [], IdIdent id -> id
  | _ -> raise (Parse_info.Parsing_error (ii_of_name name))

let name_for_typedef name =
  match name with
  | None, [], IdIdent id -> id
  | _ -> raise (Parse_info.Parsing_error (ii_of_name name))

let name_scoped nameopt tcolcol id_or_op : name =
  match nameopt with
  | None -> (Some tcolcol, [], id_or_op)
  | Some _nTODO -> (None, [], id_or_op)

let name_add_template_args name args =
  let top, qu, id_or_op = name in
  let id_or_op =
    match id_or_op with
    | IdIdent id -> IdTemplateId (id, args)
    | _ -> raise Impossible
  in
  (top, qu, id_or_op)

let complicated (env : env) _ = failwith "not implemented"

let todo (env : env) _ = failwith "not implemented"

let trailing_comma env v =
  match v with Some tok -> token env tok (* "," *) |> ignore | None -> ()

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-cpp/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the cpp CST
   to another type of tree.
*)

let map_ms_call_modifier (env : env) (x : CST.ms_call_modifier) =
  match x with
  | `X___cdecl tok -> str env tok (* "__cdecl" *)
  | `X___clrc tok -> str env tok (* "__clrcall" *)
  | `X___stdc tok -> str env tok (* "__stdcall" *)
  | `X___fast tok -> str env tok (* "__fastcall" *)
  | `X___this tok -> str env tok (* "__thiscall" *)
  | `X___vect tok -> str env tok

(* "__vectorcall" *)

let map_anon_choice_DASHDASH_d11def2 (env : env)
    (x : CST.anon_choice_DASHDASH_d11def2) =
  match x with
  | `DASHDASH tok -> (Dec, token env tok) (* "--" *)
  | `PLUSPLUS tok -> (Inc, token env tok)

(* "++" *)

let map_lambda_default_capture (env : env) (x : CST.lambda_default_capture) =
  match x with
  | `EQ tok -> CaptureEq (token env tok) (* "=" *)
  | `AMP tok -> CaptureRef (token env tok)

(* "&" *)

let map_virtual_specifier (env : env) (x : CST.virtual_specifier) =
  match x with
  | `Final tok -> Final (token env tok) (* "final" *)
  | `Over tok -> Override (token env tok)

(* "override" *)

let map_type_qualifier (env : env) (x : CST.type_qualifier) :
    type_qualifier wrap =
  match x with
  | `Choice_const x -> (
      match x with
      | `Const tok -> (Const, token env tok) (* "const" *)
      | `Vola tok -> (Volatile, token env tok) (* "volatile" *)
      | `Rest tok -> (Restrict, token env tok) (* "restrict" *)
      | `X__Atomic tok -> (Atomic, token env tok) (* "_Atomic" *))
  | `Muta tok -> (Mutable, token env tok) (* "mutable" *)
  | `Cons tok -> (Constexpr, token env tok)

(* "constexpr" *)

let map_virtual_function_specifier (env : env)
    (x : CST.virtual_function_specifier) =
  match x with `Virt tok -> Virtual (token env tok)

(* "virtual" *)

let map_storage_class_specifier (env : env) (x : CST.storage_class_specifier) :
    storage wrap =
  match x with
  | `Extern tok -> (Extern, token env tok) (* "extern" *)
  | `Static tok -> (Static, token env tok) (* "static" *)
  | `Regi tok -> (Register, token env tok) (* "register" *)
  | `Inline tok -> (StoInline, token env tok)

(* "inline" *)

let map_anon_choice_DOT_2ad1dab (env : env) (x : CST.anon_choice_DOT_2ad1dab) =
  match x with
  | `DOT tok -> (Dot, token env tok) (* "." *)
  | `DASHGT tok -> (Arrow, token env tok)

(* "->" *)

let map_anon_choice_BANG_67174d6 (env : env) (x : CST.anon_choice_BANG_67174d6)
    =
  match x with
  | `BANG tok -> (Not, token env tok) (* "!" *)
  | `TILDE tok -> (Tilde, token env tok) (* "~" *)
  | `DASH tok -> (UnMinus, token env tok) (* "-" *)
  | `PLUS tok -> (UnPlus, token env tok)

(* "+" *)

let map_ms_unaligned_ptr_modifier (env : env)
    (x : CST.ms_unaligned_ptr_modifier) =
  match x with
  | `X__unal tok -> Unaligned (token env tok) (* "_unaligned" *)
  | `X___unal tok -> Unaligned (token env tok)

(* "__unaligned" *)

let map_anon_choice_type_a2fe5d4 (env : env) (x : CST.anon_choice_type_a2fe5d4)
    =
  match x with
  | `Type tok -> token env tok (* "typename" *)
  | `Class tok -> token env tok

(* "class" *)

let map_anon_choice_public_c9638d9 (env : env)
    (x : CST.anon_choice_public_c9638d9) =
  match x with
  | `Public tok -> (Public, token env tok) (* "public" *)
  | `Priv tok -> (Private, token env tok) (* "private" *)
  | `Prot tok -> (Protected, token env tok)

(* "protected" *)

let map_anon_choice_AMP_c92c117 (env : env) (x : CST.anon_choice_AMP_c92c117) =
  match x with
  | `AMP tok ->
      let t = token env tok (* "&" *) in
      fun x -> (nQ, TReference (t, x))
  | `AMPAMP tok ->
      let t = token env tok (* "&&" *) in
      fun x -> (nQ, TRefRef (t, x))

let map_anon_choice_pat_25b90ba_4a37f8c (env : env)
    (x : CST.anon_choice_pat_25b90ba_4a37f8c) =
  match x with
  | `Pat_25b90ba tok -> Ifdef (token env tok) (* pattern #[ 	]*ifdef *)
  (* TODO Ifndef *)
  | `Pat_9d92f6a tok -> Ifdef (token env tok)

(* pattern #[ 	]*ifndef *)

let map_char_literal (env : env) ((v1, v2, v3) : CST.char_literal) =
  let iswchar, v1 =
    match v1 with
    | `LSQUOT tok -> (IsWchar, token env tok) (* "L'" *)
    | `USQUOT_d861d39 tok -> (IsWchar, token env tok) (* "u'" *)
    | `USQUOT_2701bdc tok -> (IsWchar, token env tok) (* "U'" *)
    | `U8SQUOT tok -> (IsChar, token env tok) (* "u8'" *)
    | `SQUOT tok -> (IsChar, token env tok)
    (* "'" *)
  in
  let s, v2 =
    match v2 with
    | `Esc_seq tok -> str env tok (* escape_sequence *)
    | `Imm_tok_pat_36637e2 tok -> str env tok
    (* pattern "[^\\n']" *)
  in
  let v3 = token env v3 (* "'" *) in
  let t = PI.combine_infos v1 [ v2; v3 ] in
  Char ((s, t), iswchar)

let map_preproc_call (env : env) ((v1, v2, v3) : CST.preproc_call) =
  let v1 = token env v1 (* pattern #[ \t]*[a-zA-Z]\w* *) in
  let _v2 =
    match v2 with
    | Some tok -> Some (str env tok) (* preproc_arg *)
    | None -> None
  in
  let _v3 = token env v3 (* "\n" *) in
  PragmaAndCo v1

let map_ms_pointer_modifier (env : env) (x : CST.ms_pointer_modifier) =
  match x with
  | `Ms_unal_ptr_modi x -> map_ms_unaligned_ptr_modifier env x
  | `Ms_rest_modi tok -> PtrRestrict (token env tok) (* "__restrict" *)
  | `Ms_unsi_ptr_modi tok -> Uptr (token env tok) (* "__uptr" *)
  | `Ms_signed_ptr_modi tok -> Sptr (token env tok)

(* "__sptr" *)

let map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) :
    string wrap =
  let _isWchar, v1 =
    match v1 with
    | `LDQUOT tok -> (IsWchar, token env tok) (* "L\"" *)
    | `UDQUOT_c163aae tok -> (IsWchar, token env tok) (* "u\"" *)
    | `UDQUOT_df3447d tok -> (IsWchar, token env tok) (* "U\"" *)
    | `U8DQUOT tok -> (IsChar, token env tok) (* "u8\"" *)
    | `DQUOT tok -> (IsChar, token env tok)
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
  let s = v2 |> List.map fst |> String.concat "" in
  let xs = v2 |> List.map snd in
  let v3 = token env v3 (* "\"" *) in
  let t = PI.combine_infos v1 (xs @ [ v3 ]) in
  (s, t)

let map_preproc_def (env : env) ((v1, v2, v3, v4) : CST.preproc_def) =
  let v1 = token env v1 (* pattern #[ 	]*define *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    match v3 with
    | Some tok ->
        let x = token env tok (* preproc_arg *) in
        (* TODO: we should parse this x! can be DefineExpr, etc. *)
        DefineTodo ("MacroBody", x)
    | None -> DefineEmpty
  in
  let _v4 = token env v4 (* "\n" *) in
  Define (v1, v2, DefineVar, v3)

let map_preproc_defined (env : env) (x : CST.preproc_defined) =
  match x with
  | `Defi_LPAR_id_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "defined" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = str env v3 (* pattern [a-zA-Z_]\w* *) in
      let v4 = token env v4 (* ")" *) in
      Call (IdSpecial (Defined, v1), (v2, [ Arg (expr_of_id v3) ], v4))
  | `Defi_id (v1, v2) ->
      let v1 = token env v1 (* "defined" *) in
      let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
      Call (IdSpecial (Defined, v1), fb [ Arg (expr_of_id v2) ])

let map_variadic_declarator (env : env) ((v1, v2) : CST.variadic_declarator) =
  let v1 = token env v1 (* "..." *) in
  let v2 =
    match v2 with
    | Some tok -> Some (str env tok) (* pattern [a-zA-Z_]\w* *)
    | None -> None
  in
  (v1, v2)

let map_field_designator (env : env) ((v1, v2) : CST.field_designator) :
    designator =
  let v1 = token env v1 (* "." *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  DesignatorField (v1, v2)

let map_variadic_type_parameter_declaration (env : env)
    ((v1, v2, v3) : CST.variadic_type_parameter_declaration) =
  let v1 = map_anon_choice_type_a2fe5d4 env v1 in
  let v2 = token env v2 (* "..." *) in
  let v3 =
    match v3 with
    | Some tok -> Some (str env tok) (* pattern [a-zA-Z_]\w* *)
    | None -> None
  in
  TPVariadic (v1, v2, v3)

let map_type_parameter_declaration (env : env)
    ((v1, v2) : CST.type_parameter_declaration) : template_parameter =
  let v1 = map_anon_choice_type_a2fe5d4 env v1 in
  let v2 =
    match v2 with
    | Some tok -> Some (str env tok) (* pattern [a-zA-Z_]\w* *)
    | None -> None
  in
  TPClass (v1, v2, None)

let map_ms_declspec_modifier (env : env)
    ((v1, v2, v3, v4) : CST.ms_declspec_modifier) =
  let v1 = token env v1 (* "__declspec" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = str env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 = token env v4 (* ")" *) in
  DeclSpec (v1, (v2, v3, v4))

let map_anon_choice_stmt_id_d3c4b5f (env : env)
    (x : CST.anon_choice_stmt_id_d3c4b5f) =
  match x with
  | `Id tok -> str env tok (* pattern [a-zA-Z_]\w* *)
  (* TODO: should return an either? *)
  | `DOTDOTDOT tok -> str env tok

(* "..." *)

let map_sized_type_specifier (env : env) ((v1, v2) : CST.sized_type_specifier) :
    type_ =
  let v1 =
    List.map
      (fun x ->
        match x with
        | `Signed tok -> token env tok (* "signed" *)
        | `Unsi tok -> token env tok (* "unsigned" *)
        | `Long tok -> token env tok (* "long" *)
        | `Short tok -> token env tok
        (* "short" *))
      v1
  in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
        | `Prim_type tok -> token env tok (* primitive_type *))
    | None -> complicated env ()
  in
  complicated env (v1, v2)

let map_destructor_name (env : env) ((v1, v2) : CST.destructor_name) =
  let v1 = token env v1 (* "~" *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  IdDestructor (v1, v2)

let map_anon_choice_raw_str_lit_28125b5 (env : env)
    (x : CST.anon_choice_raw_str_lit_28125b5) =
  match x with
  | `Raw_str_lit tok ->
      let x = str env tok (* raw_string_literal *) in
      x
  | `Str_lit x -> map_string_literal env x

let rec map_preproc_argument_list (env : env)
    ((v1, v2, v3) : CST.preproc_argument_list) : expr list paren =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_preproc_expression env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_preproc_expression env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and map_preproc_binary_expression (env : env)
    (x : CST.preproc_binary_expression) =
  match x with
  | `Prep_exp_PLUS_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Plus, v2), v3)
  | `Prep_exp_DASH_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Minus, v2), v3)
  | `Prep_exp_STAR_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Mul, v2), v3)
  | `Prep_exp_SLASH_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Div, v2), v3)
  | `Prep_exp_PERC_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Mod, v2), v3)
  | `Prep_exp_BARBAR_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical OrLog, v2), v3)
  | `Prep_exp_AMPAMP_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical AndLog, v2), v3)
  | `Prep_exp_BAR_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Or, v2), v3)
  | `Prep_exp_HAT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith Xor, v2), v3)
  | `Prep_exp_AMP_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith And, v2), v3)
  | `Prep_exp_EQEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical Eq, v2), v3)
  | `Prep_exp_BANGEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical NotEq, v2), v3)
  | `Prep_exp_GT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical Sup, v2), v3)
  | `Prep_exp_GTEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical SupEq, v2), v3)
  | `Prep_exp_LTEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical InfEq, v2), v3)
  | `Prep_exp_LT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Logical Inf, v2), v3)
  | `Prep_exp_LTLT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith DecLeft, v2), v3)
  | `Prep_exp_GTGT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = map_preproc_expression env v3 in
      Binary (v1, (Arith DecRight, v2), v3)

and map_preproc_call_expression (env : env)
    ((v1, v2) : CST.preproc_call_expression) =
  let v1 = str env v1 (* pattern [a-zA-Z_]\w* *) in
  let v2 = map_preproc_argument_list env v2 in
  (v1, v2)

and map_preproc_expression (env : env) (x : CST.preproc_expression) : expr =
  match x with
  | `Id tok ->
      let x = str env tok (* pattern [a-zA-Z_]\w* *) in
      expr_of_id x
  | `Prep_call_exp x ->
      let id, (l, xs, r) = map_preproc_call_expression env x in
      let args = xs |> List.map expr_to_arg in
      Call (expr_of_id id, (l, args, r))
  | `Num_lit tok ->
      let x = str env tok (* number_literal *) in
      parse_number_literal x
  | `Char_lit x ->
      let c = map_char_literal env x in
      C c
  | `Prep_defi x ->
      let x = map_preproc_defined env x in
      x
  | `Prep_un_exp (v1, v2) ->
      let op = map_anon_choice_BANG_67174d6 env v1 in
      let v2 = map_preproc_expression env v2 in
      Unary (op, v2)
  | `Prep_bin_exp x -> map_preproc_binary_expression env x
  | `Prep_paren_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_preproc_expression env v2 in
      let v3 = token env v3 (* ")" *) in
      ParenExpr (v1, v2, v3)

let map_variadic_reference_declarator (env : env)
    ((v1, v2) : CST.variadic_reference_declarator) =
  let v1 =
    match v1 with
    | `AMPAMP tok -> token env tok (* "&&" *)
    | `AMP tok -> token env tok
    (* "&" *)
  in
  let v2 = map_variadic_declarator env v2 in
  (v1, v2)

let map_preproc_params (env : env) ((v1, v2, v3) : CST.preproc_params) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_stmt_id_d3c4b5f env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_stmt_id_d3c4b5f env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

let map_anon_choice_stmt_id_efddc5b (env : env)
    (x : CST.anon_choice_stmt_id_efddc5b) : ident_or_op =
  match x with
  | `Id tok -> IdIdent (str env tok) (* pattern [a-zA-Z_]\w* *)
  | `Op_name tok ->
      let op = str env tok (* operator_name *) in
      IdOperator (parse_operator env op)
  | `Dest_name x ->
      let x = map_destructor_name env x in
      x

let map_concatenated_string (env : env) ((v1, v2) : CST.concatenated_string) =
  let v1 = map_anon_choice_raw_str_lit_28125b5 env v1 in
  let v2 = List.map (map_anon_choice_raw_str_lit_28125b5 env) v2 in
  (* TODO: grab it from v1? *)
  let isWchar = IsChar in
  match v2 with [] -> String (v1, isWchar) | _ -> MultiString (v1 :: v2)

let map_preproc_include (env : env) ((v1, v2, v3) : CST.preproc_include) =
  let v1 = token env v1 (* pattern #[ 	]*include *) in
  let v2 =
    match v2 with
    (* todo: in pfff the string in the string wrap does not contain
     * the enclosing chars *)
    | `Str_lit x ->
        let x = map_string_literal env x in
        IncLocal x
    | `System_lib_str tok ->
        let x = str env tok (* system_lib_string *) in
        IncSystem x
    | `Id tok ->
        let id = str env tok (* pattern [a-zA-Z_]\w* *) in
        IncOther (expr_of_id id)
    | `Prep_call_exp x ->
        let id, (l, xs, r) = map_preproc_call_expression env x in
        let args = xs |> List.map expr_to_arg in
        let x = Call (expr_of_id id, (l, args, r)) in
        IncOther x
  in
  let _v3 = token env v3 (* "\n" *) in
  Include (v1, v2)

let map_preproc_function_def (env : env)
    ((v1, v2, v3, v4, v5) : CST.preproc_function_def) =
  let v1 = token env v1 (* pattern #[ 	]*define *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = map_preproc_params env v3 in
  let v4 =
    match v4 with
    (* TODO: we should parse this x! can be DefineExpr, etc. *)
    | Some tok ->
        let x = token env tok (* preproc_arg *) in
        DefineTodo ("MacroBody", x)
    | None -> DefineEmpty
  in
  let _v5 = token env v5 (* "\n" *) in
  Define (v1, v2, DefineMacro v3, v4)

let rec map_abstract_array_declarator (env : env)
    ((v1, v2, v3, v4, v5) : CST.abstract_array_declarator) : abstract_declarator
    =
  let v1 =
    match v1 with Some x -> map_abstract_declarator env x | None -> id
  in
  let v2 = token env v2 (* "[" *) in
  let v3 = List.map (map_type_qualifier env) v3 in
  let v4 =
    match v4 with
    | Some x -> Some (map_anon_choice_exp_508611b env x)
    | None -> None
  in
  let v5 = token env v5 (* "]" *) in
  fun x -> v1 (v3, TArray ((v2, v4, v5), x))

and map_abstract_declarator (env : env) (x : CST.abstract_declarator) :
    abstract_declarator =
  match x with
  | `Choice_abst_poin_decl x -> (
      match x with
      | `Abst_poin_decl x -> map_abstract_pointer_declarator env x
      | `Abst_func_decl x -> map_abstract_function_declarator env x
      | `Abst_array_decl x -> map_abstract_array_declarator env x
      | `Abst_paren_decl x -> map_abstract_parenthesized_declarator env x)
  | `Abst_ref_decl (v1, v2) ->
      let v1 = map_anon_choice_AMP_c92c117 env v1 in
      let v2 =
        match v2 with Some x -> map_abstract_declarator env x | None -> id
      in
      fun x -> x |> v2 |> v1

and map_abstract_function_declarator (env : env)
    ((v1, v2, v3, v4) : CST.abstract_function_declarator) : abstract_declarator
    =
  let v1 =
    match v1 with Some x -> map_abstract_declarator env x | None -> id
  in
  let v2 = map_parameter_list env v2 in
  let v3 =
    List.map
      (fun x ->
        match x with
        | `Type_qual x ->
            let x = map_type_qualifier env x in
            Left (TQ x)
        | `Noex x ->
            let x = map_noexcept env x in
            Right x
        | `Throw_spec x ->
            let x = map_throw_specifier env x in
            Right x)
      v3
  in
  let v4 =
    match v4 with
    | Some x -> Some (map_trailing_return_type env x)
    | None -> None
  in
  let ft_specs, ft_throw = Common.partition_either id v3 in
  fun x ->
    v1
      ( nQ,
        TFunction
          {
            (* override ft_ret with trailing_return_type as x can actually
             * be a fake type sometimes (e.g., in lambdas)
             *)
            ft_ret = (match v4 with None -> x | Some t -> t);
            ft_params = v2;
            ft_specs;
            ft_const = None (* TODO *);
            ft_throw;
          } )

and map_abstract_parenthesized_declarator (env : env)
    ((v1, v2, v3) : CST.abstract_parenthesized_declarator) : abstract_declarator
    =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_abstract_declarator env v2 in
  let v3 = token env v3 (* ")" *) in
  fun x -> (nQ, ParenType (v1, v2 x, v3))

and map_abstract_pointer_declarator (env : env)
    ((v1, v2, v3) : CST.abstract_pointer_declarator) : abstract_declarator =
  let v1 = token env v1 (* "*" *) in
  let v2 = List.map (map_type_qualifier env) v2 in
  let f1 x = (v2, TPointer (v1, x, [])) in
  let v3 =
    match v3 with Some x -> map_abstract_declarator env x | None -> id
  in
  (* TODO: bug? seems different order than in declarator *)
  fun x -> x |> v3 |> f1

and map_alias_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.alias_declaration) : using =
  let v1 = token env v1 (* "using" *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = token env v3 (* "=" *) in
  let v4 = map_type_descriptor env v4 in
  let v5 = token env v5 (* ";" *) in
  (v1, UsingAlias (v2, v3, v4), v5)

(* for New and ObjInit *)
and map_anon_choice_arg_list_e4b6f8f (env : env)
    (x : CST.anon_choice_arg_list_e4b6f8f) : obj_init =
  match x with
  | `Arg_list x ->
      let x = map_argument_list env x in
      Args x
  | `Init_list x ->
      let x = map_initializer_list env x in
      Inits x

(* after a struct/union/class keyword *)
and map_anon_choice_class_name_d6703e6 (env : env)
    (x : CST.anon_choice_class_name_d6703e6) : class_key wrap -> type_ =
  match x with
  | `Class_name x ->
      let x = map_class_name env x in
      fun ckey -> (nQ, ClassName (ckey, x))
  | `Opt_class_name_opt_virt_spec_opt_base_class_clause_field_decl_list
      (v1, v2, v3, v4) ->
      let v1 =
        match v1 with Some x -> Some (map_class_name env x) | None -> None
      in
      let v2 =
        match v2 with
        | Some x -> Some (map_virtual_specifier env x)
        | None -> None
      in
      let v3 =
        match v3 with Some x -> map_base_class_clause env x | None -> []
      in
      let v4 = map_field_declaration_list env v4 in
      fun ckey ->
        let cdef =
          ( v1,
            {
              c_kind = ckey;
              c_inherit =
                v3 |> List.map (fun clause -> { clause with i_virtual = v2 });
              c_members = v4;
            } )
        in
        (nQ, ClassDef cdef)

and map_anon_choice_comp_stmt_be91723 (env : env)
    (x : CST.anon_choice_comp_stmt_be91723) : function_body =
  match x with
  | `Comp_stmt x ->
      let x = map_compound_statement env x in
      FBDef x
  | `Defa_meth_clause (v1, v2, v3) ->
      let v1 = token env v1 (* "=" *) in
      let v2 = token env v2 (* "default" *) in
      let v3 = token env v3 (* ";" *) in
      FBDefault (v1, v2, v3)
  | `Delete_meth_clause (v1, v2, v3) ->
      let v1 = token env v1 (* "=" *) in
      let v2 = token env v2 (* "delete" *) in
      let v3 = token env v3 (* ";" *) in
      FBDelete (v1, v2, v3)

(* for map_declaration *)
and map_anon_choice_decl_f8b0ff3 (env : env) (x : CST.anon_choice_decl_f8b0ff3)
    =
  match x with
  | `Decl x ->
      let x = map_declarator env x in
      fun attrs t specs ->
        {
          v_namei = Some (x.dn, None);
          v_type = x.dt t;
          v_storage = NoSto;
          v_specs = List.map (fun x -> A x) attrs @ specs;
        }
  | `Init_decl x ->
      let x, init = map_init_declarator env x in
      fun attrs t specs ->
        {
          v_namei = Some (x.dn, Some init);
          v_type = x.dt t;
          v_storage = NoSto;
          v_specs = List.map (fun x -> A x) attrs @ specs;
        }

and map_anon_choice_exp_3078596 (env : env) (x : CST.anon_choice_exp_3078596) :
    initialiser =
  match x with
  | `Exp x ->
      let x = map_expression env x in
      InitExpr x
  | `Init_list x ->
      let x = map_initializer_list env x in
      InitList x

and map_anon_choice_exp_508611b (env : env) (x : CST.anon_choice_exp_508611b) =
  match x with
  | `Exp x ->
      let x = map_expression env x in
      x
  | `STAR tok ->
      let t = token env tok in
      ExprTodo (("StarTArray", t), [])

(* "*" *)
and map_anon_choice_exp_55b4dba (env : env) (x : CST.anon_choice_exp_55b4dba) :
    expr =
  match x with
  | `Exp x -> map_expression env x
  | `Comma_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "," *) in
      let v3 = map_anon_choice_exp_55b4dba env v3 in
      Sequence (v1, v2, v3)

and map_anon_choice_init_pair_1a6981e (env : env)
    (x : CST.anon_choice_init_pair_1a6981e) : initialiser =
  match x with
  | `Init_pair (v1, v2, v3) ->
      let v1 =
        List.map
          (fun x ->
            match x with
            | `Subs_desi x -> map_subscript_designator env x
            | `Field_desi x -> map_field_designator env x)
          v1
      in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_anon_choice_exp_3078596 env v3 in
      InitDesignators (v1, v2, v3)
  | `Exp x ->
      let x = map_expression env x in
      InitExpr x
  | `Init_list x ->
      let x = map_initializer_list env x in
      InitList x

(* inside template parameters *)
and map_anon_choice_param_decl_13b5913 (env : env)
    (x : CST.anon_choice_param_decl_13b5913) : template_parameter =
  match x with
  | `Param_decl x ->
      let x = map_parameter_declaration env x in
      TP (P x)
  | `Opt_param_decl x ->
      let x = map_optional_parameter_declaration env x in
      TP (P x)
  | `Type_param_decl x ->
      let x = map_type_parameter_declaration env x in
      x
  | `Vari_param_decl x ->
      let x = map_variadic_parameter_declaration env x in
      TP x
  | `Vari_type_param_decl x ->
      let x = map_variadic_type_parameter_declaration env x in
      x
  | `Opt_type_param_decl x ->
      let x = map_optional_type_parameter_declaration env x in
      x
  | `Temp_temp_param_decl (v1, v2, v3) ->
      let v1 = token env v1 (* "template" *) in
      let v2 = map_template_parameter_list env v2 in
      let v3 =
        match v3 with
        | `Type_param_decl x -> map_type_parameter_declaration env x
        | `Vari_type_param_decl x ->
            map_variadic_type_parameter_declaration env x
        | `Opt_type_param_decl x ->
            map_optional_type_parameter_declaration env x
      in
      TPNested (v1, v2, v3)

and map_anon_choice_param_decl_d9083af (env : env)
    (x : CST.anon_choice_param_decl_d9083af) : parameter =
  match x with
  | `Param_decl x ->
      let x = map_parameter_declaration env x in
      P x
  | `Opt_param_decl x ->
      let x = map_optional_parameter_declaration env x in
      P x
  | `Vari_param_decl x ->
      let x = map_variadic_parameter_declaration env x in
      x
  | `DOTDOTDOT tok ->
      let x = token env tok in
      ParamDots x

(* "..." *)
and map_anon_choice_prep_else_8b52b0f (env : env)
    (x : CST.anon_choice_prep_else_8b52b0f) : toplevel list =
  match x with
  | `Prep_else (v1, v2) ->
      let v1 = token env v1 (* pattern #[ 	]*else *) in
      let v2 = map_translation_unit env v2 in
      let dir = CppIfdef (IfdefElse v1) in
      dir :: v2
  | `Prep_elif (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* pattern #[ 	]*elif *) in
      let _v2 = map_preproc_expression env v2 in
      let _v3 = token env v3 (* "\n" *) in
      let v4 = map_translation_unit env v4 in
      let v5 =
        match v5 with
        | Some x -> map_anon_choice_prep_else_8b52b0f env x
        | None -> []
      in
      let dir = CppIfdef (IfdefElseif v1) in
      (dir :: v4) @ v5

and map_anon_choice_prep_else_in_field_decl_list_97ea65e (env : env)
    (x : CST.anon_choice_prep_else_in_field_decl_list_97ea65e) :
    class_member sequencable list =
  match x with
  | `Prep_else_in_field_decl_list (v1, v2) ->
      let v1 = token env v1 (* pattern #[ 	]*else *) in
      let v2 = List.map (map_field_declaration_list_item env) v2 in
      let dir = CppIfdef (IfdefElse v1) in
      dir :: List.flatten v2
  | `Prep_elif_in_field_decl_list (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* pattern #[ 	]*elif *) in
      let _v2 = map_preproc_expression env v2 in
      let _v3 = token env v3 (* "\n" *) in
      let v4 = List.map (map_field_declaration_list_item env) v4 in
      let v5 =
        match v5 with
        | Some x -> map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
        | None -> []
      in
      let dir = CppIfdef (IfdefElseif v1) in
      (dir :: List.flatten v4) @ v5

(* used for field initializer in ctor/dtor *)
and map_anon_choice_stmt_id_ae28a26 (env : env)
    (x : CST.anon_choice_stmt_id_ae28a26) : name =
  match x with
  | `Id tok ->
      let x = str env tok (* pattern [a-zA-Z_]\w* *) in
      name_of_id x
  | `Scoped_field_id (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some x -> Some (map_anon_choice_stmt_id_ec78ce4 env x)
        | None -> None
      in
      let v2 = token env v2 (* "::" *) in
      let v3 = map_anon_choice_stmt_id_efddc5b env v3 in
      name_scoped v1 v2 v3

and map_anon_choice_stmt_id_ec78ce4 (env : env)
    (x : CST.anon_choice_stmt_id_ec78ce4) =
  match x with
  | `Id tok ->
      let x = str env tok (* pattern [a-zA-Z_]\w* *) in
      name_of_id x
  | `Temp_type x ->
      let x = map_template_type env x in
      x
  | `Scoped_name_id x ->
      let x = map_scoped_namespace_identifier env x in
      x

and map_anon_choice_stmt_id_f1f5a37 (env : env)
    (x : CST.anon_choice_stmt_id_f1f5a37) : name =
  match x with
  | `Id tok ->
      let x = str env tok (* pattern [a-zA-Z_]\w* *) in
      name_of_id x
  | `Scoped_id x ->
      let x = map_scoped_identifier env x in
      x

and map_anon_choice_stor_class_spec_5764fed (env : env)
    (x : CST.anon_choice_stor_class_spec_5764fed) : specifier =
  match x with
  | `Stor_class_spec x ->
      let x = map_storage_class_specifier env x in
      ST x
  | `Type_qual x ->
      let x = map_type_qualifier env x in
      TQ x
  | `Attr_spec x ->
      let x = map_attribute_specifier env x in
      A x
  | `Ms_decl_modi x ->
      let x = map_ms_declspec_modifier env x in
      A x

and map_anon_choice_type_desc_4d9cafa (env : env)
    (x : CST.anon_choice_type_desc_4d9cafa) : template_argument =
  match x with
  | `Type_desc x ->
      let x = map_type_descriptor env x in
      Left x
  | `Type_param_pack_expa (v1, v2) ->
      let v1 = map_type_descriptor env v1 in
      let v2 = token env v2 (* "..." *) in
      let t = (nQ, TypeTodo (("TypeDots", v2), [ v1 ])) in
      Left t
  | `Exp x ->
      let x = map_expression env x in
      Right x

and map_anon_choice_type_qual_01506e0 (env : env)
    (x : CST.anon_choice_type_qual_01506e0) :
    (specifier, exn_spec) Common.either =
  match x with
  | `Type_qual x ->
      let x = map_type_qualifier env x in
      Left (TQ x)
  | `Virt_spec x ->
      let x = map_virtual_specifier env x in
      Left (M x)
  | `Noex x ->
      let x = map_noexcept env x in
      Right x
  | `Throw_spec x ->
      let x = map_throw_specifier env x in
      Right x
  | `Trai_ret_type x ->
      let x = map_trailing_return_type env x in
      complicated env x

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) :
    argument list paren =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_exp_3078596 env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_exp_3078596 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  ( v1,
    v2
    |> List.map (function
         | InitExpr e -> Arg e
         | InitList (l, xs, r) -> ArgInits (l, xs, r)
         | _ -> raise Impossible (* see map_anon_choice_exp_3078596 *)),
    v3 )

and map_array_declarator (env : env)
    ((v1, v2, v3, v4, v5) : CST.array_declarator) : declarator =
  let v1 = map_declarator env v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = List.map (map_type_qualifier env) v3 in
  let v4 =
    match v4 with
    | Some x -> Some (map_anon_choice_exp_508611b env x)
    | None -> None
  in
  let v5 = token env v5 (* "]" *) in
  { dn = v1.dn; dt = (fun x -> v1.dt (v3, TArray ((v2, v4, v5), x))) }

and map_array_field_declarator (env : env)
    ((v1, v2, v3, v4, v5) : CST.array_field_declarator) =
  let v1 = map_field_declarator env v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = List.map (map_type_qualifier env) v3 in
  let v4 =
    match v4 with
    | Some x -> Some (map_anon_choice_exp_508611b env x)
    | None -> None
  in
  let v5 = token env v5 (* "]" *) in
  { dn = v1.dn; dt = (fun x -> v1.dt (v3, TArray ((v2, v4, v5), x))) }

and map_assignment_expression (env : env)
    ((v1, v2, v3) : CST.assignment_expression) : expr =
  let v1 = map_assignment_left_expression env v1 in
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
  let v3 = map_expression env v3 in
  Assign (v1, v2, v3)

and map_assignment_left_expression (env : env)
    (x : CST.assignment_left_expression) : a_lhs =
  match x with
  | `Choice_id x -> (
      match x with
      | `Id tok ->
          let x = str env tok (* pattern [a-zA-Z_]\w* *) in
          expr_of_id x
      | `Call_exp x -> map_call_expression env x
      | `Field_exp x -> map_field_expression env x
      | `Poin_exp x -> map_pointer_expression env x
      | `Subs_exp x -> map_subscript_expression env x
      | `Paren_exp x ->
          let l, e, r = map_parenthesized_expression env x in
          ParenExpr (l, e, r))
  | `Scoped_name_id x ->
      let x = map_scoped_namespace_identifier env x in
      N (x, noIdInfo ())

and map_attribute (env : env) ((v1, v2, v3, v4) : CST.attribute) : attribute =
  let v1 = token env v1 (* "[[" *) in
  let v2 = map_expression env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = map_expression env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* "]]" *) in
  BracketsAttr (v1, v2 :: v3, v4)

and map_attribute_specifier (env : env)
    ((v1, v2, v3, v4) : CST.attribute_specifier) : attribute =
  let v1 = token env v1 (* "__attribute__" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = map_argument_list env v3 in
  let v4 = token env v4 (* ")" *) in
  UnderscoresAttr (v1, (v2, v3, v4))

and map_base_class_clause (env : env)
    ((v1, v2, v3, v4, v5) : CST.base_class_clause) : base_clause list =
  let v1 = token env v1 (* ":" *) in
  let v2 =
    match v2 with
    | Some x -> Some (map_anon_choice_public_c9638d9 env x)
    | None -> None
  in
  let v3 = map_class_name env v3 in
  let _v4TODO =
    match v4 with Some tok -> Some (token env tok) (* "..." *) | None -> None
  in
  let base1 = { i_name = v3; i_access = v2; i_virtual = None } in
  let v5 =
    List.map
      (fun (v1, v2, v3, v4) ->
        let _v1 = token env v1 (* "," *) in
        let v2 =
          match v2 with
          | Some x -> Some (map_anon_choice_public_c9638d9 env x)
          | None -> None
        in
        let v3 = map_class_name env v3 in
        let _v4 =
          match v4 with
          | Some tok -> Some (token env tok) (* "..." *)
          | None -> None
        in
        { i_name = v3; i_access = v2; i_virtual = None })
      v5
  in
  base1 :: v5

and map_binary_expression (env : env) (x : CST.binary_expression) : expr =
  match x with
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Arith Plus, v2), v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Arith Minus, v2), v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Arith Mul, v2), v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Arith Div, v2), v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Arith Mod, v2), v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Logical OrLog, v2), v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Logical AndLog, v2), v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Arith Or, v2), v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Arith Xor, v2), v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Arith And, v2), v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Logical Eq, v2), v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Logical NotEq, v2), v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Logical Sup, v2), v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Logical SupEq, v2), v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Logical InfEq, v2), v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Logical Inf, v2), v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Arith Plus, v2), v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = map_expression env v3 in
      Binary (v1, (Arith Plus, v2), v3)

and map_bitfield_clause (env : env) ((v1, v2) : CST.bitfield_clause) =
  let v1 = token env v1 (* ":" *) in
  let v2 = map_expression env v2 in
  (v1, v2)

and map_call_expression (env : env) (x : CST.call_expression) : expr =
  match x with
  | `Exp_arg_list (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_argument_list env v2 in
      Call (v1, v2)
  | `Prim_type_arg_list (v1, v2) ->
      let v1 = str env v1 (* primitive_type *) in
      let t = parse_primitive_type v1 in
      let v2 = map_argument_list env v2 in
      ConstructedObject (t, Args v2)

and map_case_statement (env : env) ((v1, v2, v3) : CST.case_statement) : stmt =
  let v1 =
    match v1 with
    | `Case_exp (v1, v2) ->
        let v1 = token env v1 (* "case" *) in
        let v2 = map_expression env v2 in
        fun t st -> Case (v1, v2, t, st)
    | `Defa tok ->
        let v1 = token env tok in
        (* "default" *)
        fun t st -> Default (v1, t, st)
  in
  let v2 = token env v2 (* ":" *) in
  let v3 =
    List.map
      (fun x ->
        match x with
        | `Choice_labe_stmt x ->
            let x = map_non_case_statement env x in
            S x
        | `Decl x ->
            let x = map_declaration env x in
            D (DeclList x)
        | `Type_defi x ->
            let x = map_type_definition env x in
            D (DeclList x))
      v3
  in
  v1 v2 v3

and map_cast_expression (env : env) ((v1, v2, v3, v4) : CST.cast_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_type_descriptor env v2 in
  let v3 = token env v3 (* ")" *) in
  let v4 = map_expression env v4 in
  Cast ((v1, v2, v3), v4)

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) : handler =
  let v1 = token env v1 (* "catch" *) in
  let l, v2, r = map_parameter_list env v2 in
  let v3 = map_compound_statement env v3 in
  let params = v2 |> List.map (fun p -> ExnDecl p) in
  (v1, (l, params, r), v3)

and map_class_name (env : env) (x : CST.class_name) : a_class_name =
  match x with
  | `Id tok ->
      let x = str env tok (* pattern [a-zA-Z_]\w* *) in
      name_of_id x
  | `Scoped_type_id x ->
      let x = map_scoped_type_identifier env x in
      x
  | `Temp_type x ->
      let x = map_template_type env x in
      x

and map_compound_literal_expression (env : env)
    (x : CST.compound_literal_expression) =
  match x with
  | `LPAR_type_desc_RPAR_init_list (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_type_descriptor env v2 in
      let v3 = token env v3 (* ")" *) in
      let v4 = map_initializer_list env v4 in
      GccConstructor ((v1, v2, v3), v4)
  | `Choice_id_init_list (v1, v2) ->
      let v1 =
        match v1 with
        | `Id tok ->
            let x = str env tok (* pattern [a-zA-Z_]\w* *) in
            name_of_id x
        | `Temp_type x ->
            let x = map_template_type env x in
            x
        | `Scoped_type_id x ->
            let x = map_scoped_type_identifier env x in
            x
      in
      let t = (nQ, TypeName v1) in
      let l, xs, r = map_initializer_list env v2 in
      ConstructedObject (t, Inits (l, xs, r))

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) :
    compound =
  let v1 = token env v1 (* "{" *) in
  let v2 = map_translation_unit env v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and map_condition_clause (env : env) ((v1, v2, v3) : CST.condition_clause) :
    condition_clause paren =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | `Opt_choice_decl_choice_exp (v1, v2) -> (
        let v1 =
          match v1 with
          | Some x -> (
              match x with
              | `Decl x -> Common2.Left3 (map_declaration env x)
              | `Exp_stmt x ->
                  let eopt, sc = map_expression_statement env x in
                  Common2.Right3 (eopt, sc))
          | None -> Common2.Middle3 ()
        in
        let v2 = map_anon_choice_exp_55b4dba env v2 in
        match v1 with
        | Common2.Middle3 () -> CondClassic v2
        | Common2.Left3 d -> CondDecl (d, v2)
        | Common2.Right3 e -> CondStmt (e, v2))
    | `Cond_decl x -> map_condition_declaration env x
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and map_condition_declaration (env : env)
    ((v1, v2, v3) : CST.condition_declaration) =
  let t, specs = map_declaration_specifiers env v1 in
  let { dn; dt } = map_declarator env v2 in
  let v3 =
    match v3 with
    | `EQ_exp (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        EqInit (v1, InitExpr v2)
    | `Init_list x -> ObjInit (Inits (map_initializer_list env x))
  in
  let one =
    {
      v_namei = Some (dn, Some v3);
      v_type = dt t;
      v_storage = NoSto;
      v_specs = specs;
    }
  in
  CondOneDecl one

and map_conditional_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.conditional_expression) =
  let v1 = map_expression env v1 in
  let v2 = token env v2 (* "?" *) in
  let v3 = map_expression env v3 in
  let v4 = token env v4 (* ":" *) in
  let v5 = map_expression env v5 in
  CondExpr (v1, v2, Some v3, v4, v5)

and map_constructor_or_destructor_declaration (env : env)
    ((v1, v2, v3) : CST.constructor_or_destructor_declaration) =
  let v1 =
    match v1 with Some x -> map_constructor_specifiers env x | None -> []
  in
  let { dn; dt } = map_function_declarator env v2 in
  let v3 = token env v3 (* ";" *) in

  let t = dt (nQ, TBase (Void (ii_of_name dn))) in
  let ent, def = H2.fixFunc ((dn, t, NoSto), FBDecl v3) in
  ({ ent with specs = v1 @ ent.specs }, def)

and map_constructor_or_destructor_definition (env : env)
    ((v1, v2, v3, v4) : CST.constructor_or_destructor_definition) =
  let v1 =
    match v1 with Some x -> map_constructor_specifiers env x | None -> []
  in
  let { dn; dt } = map_function_declarator env v2 in
  let _v3TODO =
    match v3 with Some x -> map_field_initializer_list env x | None -> []
  in
  let t = dt (nQ, TBase (Void (ii_of_name dn))) in
  let v4 = map_anon_choice_comp_stmt_be91723 env v4 in
  let ent, def = H2.fixFunc ((dn, t, NoSto), v4) in
  ({ ent with specs = v1 @ ent.specs }, def)

and map_constructor_specifiers (env : env) (xs : CST.constructor_specifiers) :
    specifier list =
  List.map
    (fun x ->
      match x with
      | `Stor_class_spec x ->
          let x = map_storage_class_specifier env x in
          ST x
      | `Type_qual x ->
          let x = map_type_qualifier env x in
          TQ x
      | `Attr_spec x ->
          let x = map_attribute_specifier env x in
          A x
      | `Virt_func_spec x ->
          let x = map_virtual_function_specifier env x in
          M x
      | `Expl_func_spec x ->
          let x = map_explicit_function_specifier env x in
          M x)
    xs

and map_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.declaration) :
    vars_decl =
  let v1 = List.map (map_attribute env) v1 in
  let t, specs = map_declaration_specifiers env v2 in
  let v3 = map_anon_choice_decl_f8b0ff3 env v3 in
  let v4 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = map_anon_choice_decl_f8b0ff3 env v2 in
        v2)
      v4
  in
  let v5 = token env v5 (* ";" *) in
  let xs = v3 :: v4 |> List.map (fun f -> f v1 t specs) in
  (xs, v5)

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) :
    declarations =
  let v1 = token env v1 (* "{" *) in
  let v2 = map_translation_unit env v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and map_declaration_specifiers (env : env)
    ((v1, v2, v3) : CST.declaration_specifiers) : type_ * specifier list =
  let v1 = List.map (map_anon_choice_stor_class_spec_5764fed env) v1 in
  let v2 = map_type_specifier env v2 in
  let v3 = List.map (map_anon_choice_stor_class_spec_5764fed env) v3 in
  (v2, v1 @ v3)

and map_declarator (env : env) (x : CST.declarator) : declarator =
  match x with
  | `Choice_poin_decl x -> (
      match x with
      | `Poin_decl x -> map_pointer_declarator env x
      | `Func_decl x -> map_function_declarator env x
      | `Array_decl x -> map_array_declarator env x
      | `Paren_decl x -> map_parenthesized_declarator env x
      | `Id tok ->
          let x = str env tok (* pattern [a-zA-Z_]\w* *) in
          { dn = name_of_id x; dt = id })
  | `Ref_decl (v1, v2) ->
      let v1 = map_anon_choice_AMP_c92c117 env v1 in
      let v2 = map_declarator env v2 in
      { dn = v2.dn; dt = (fun x -> x |> v1 |> v2.dt) }
  | `Scoped_id x ->
      let x = map_scoped_identifier env x in
      { dn = x; dt = id }
  | `Temp_func x ->
      let x = map_template_function env x in
      { dn = x; dt = id }
  | `Op_name tok ->
      let op = str env tok (* operator_name *) in
      let dn = (None, noQscope, IdOperator (parse_operator env op)) in
      { dn; dt = id }
  | `Dest_name x ->
      let x = map_destructor_name env x in
      let dn = (None, noQscope, x) in
      { dn; dt = id }
  | `Stru_bind_decl (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
            v2)
          v3
      in
      let v4 = token env v4 (* "]" *) in
      complicated env (v1, v2, v3, v4)

and map_empty_declaration (env : env) ((v1, v2) : CST.empty_declaration) : decl
    =
  let v1 = map_type_specifier env v1 in
  let v2 = token env v2 (* ";" *) in
  let one = { v_namei = None; v_type = v1; v_storage = NoSto; v_specs = [] } in
  DeclList ([ one ], v2)

and map_enum_base_clause (env : env) ((v1, v2) : CST.enum_base_clause) =
  let v1 = token env v1 (* ":" *) in
  let v2 =
    match v2 with
    | `Scoped_type_id x ->
        let x = map_scoped_type_identifier env x in
        (nQ, TypeName x)
    | `Id tok ->
        let x = str env tok (* pattern [a-zA-Z_]\w* *) in
        (nQ, TypeName (name_of_id x))
    | `Sized_type_spec x ->
        let x = map_sized_type_specifier env x in
        x
  in
  v2

and map_enumerator (env : env) ((v1, v2) : CST.enumerator) : enum_elem =
  let v1 = str env v1 (* pattern [a-zA-Z_]\w* *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        Some (v1, v2)
    | None -> None
  in
  { e_name = v1; e_val = v2 }

and map_enumerator_list (env : env) ((v1, v2, v3, v4) : CST.enumerator_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_enumerator env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_enumerator env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = trailing_comma env v3 in
  let v4 = token env v4 (* "}" *) in
  (v1, v2, v4)

and map_explicit_function_specifier (env : env)
    (x : CST.explicit_function_specifier) : modifier =
  match x with
  | `Expl tok -> Explicit (token env tok, (* "explicit" *) None)
  | `Expl_LPAR_exp_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "explicit" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* ")" *) in
      Explicit (v1, Some (v2, v3, v4))

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Choice_cond_exp x -> (
      match x with
      | `Cond_exp x -> map_conditional_expression env x
      | `Assign_exp x -> map_assignment_expression env x
      | `Bin_exp x -> map_binary_expression env x
      | `Un_exp x -> map_unary_expression env x
      | `Update_exp x -> map_update_expression env x
      | `Cast_exp x -> map_cast_expression env x
      | `Poin_exp x -> map_pointer_expression env x
      | `Sizeof_exp x -> map_sizeof_expression env x
      | `Subs_exp x -> map_subscript_expression env x
      | `Call_exp x -> map_call_expression env x
      | `Field_exp x -> map_field_expression env x
      | `Comp_lit_exp x -> map_compound_literal_expression env x
      | `Id tok ->
          let x = str env tok (* pattern [a-zA-Z_]\w* *) in
          expr_of_id x
      | `Num_lit tok ->
          let x = str env tok (* number_literal *) in
          parse_number_literal x
      | `Str_lit x ->
          let x = map_string_literal env x in
          C (String (x, IsChar))
      | `True tok ->
          let x = token env tok (* true *) in
          C (Bool (true, x))
      | `False tok ->
          let x = token env tok (* false *) in
          C (Bool (false, x))
      | `Null tok ->
          let x = token env tok (* "NULL" *) in
          C (Nullptr x)
      | `Conc_str x ->
          let x = map_concatenated_string env x in
          C x
      | `Char_lit x ->
          let x = map_char_literal env x in
          C x
      | `Paren_exp x ->
          let l, e, r = map_parenthesized_expression env x in
          ParenExpr (l, e, r))
  | `Temp_func x ->
      let x = map_template_function env x in
      N (x, noIdInfo ())
  | `Scoped_id x ->
      let x = map_scoped_identifier env x in
      N (x, noIdInfo ())
  | `New_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        match v1 with
        | Some tok -> Some (token env tok) (* "::" *)
        | None -> None
      in
      let v2 = token env v2 (* "new" *) in
      let v3 =
        match v3 with Some x -> Some (map_argument_list env x) | None -> None
      in
      let v4 = map_type_specifier env v4 in
      let _v5 =
        match v5 with Some x -> Some (map_new_declarator env x) | None -> None
      in
      let v6 =
        match v6 with
        | Some x -> Some (map_anon_choice_arg_list_e4b6f8f env x)
        | None -> None
      in
      New (v1, v2, v3, v4, v6)
  | `Delete_exp (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some tok -> Some (token env tok) (* "::" *)
        | None -> None
      in
      let v2 = token env v2 (* "delete" *) in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "[" *) in
            let v2 = token env v2 (* "]" *) in
            fun tcolcol tkwd e -> Delete (tcolcol, tkwd, Some (v1, (), v2), e)
        | None -> fun tcolcol tkwd e -> Delete (tcolcol, tkwd, None, e)
      in
      let v4 = map_expression env v4 in
      v3 v1 v2 v4
  | `Lambda_exp (v1, v2, v3) ->
      let l, xs, r = map_lambda_capture_specifier env v1 in
      let v2 =
        match v2 with
        | Some x -> Some (map_abstract_function_declarator env x)
        | None -> None
      in
      let v3 = map_compound_statement env v3 in
      let ft_ret = (nQ, TAuto l) in
      let f_type =
        match v2 with
        | Some ft -> (
            let t = ft ft_ret in
            match t with _, TFunction ft -> ft | _ -> raise Impossible)
        | None ->
            {
              ft_ret;
              ft_params = (l, [], r);
              ft_specs = [];
              ft_const = None;
              ft_throw = [];
            }
      in
      let fdef =
        { f_type; f_storage = NoSto; f_body = FBDef v3; f_specs = [] }
      in
      Lambda ((l, xs, r), fdef)
  | `Param_pack_expa (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "..." *) in
      ParamPackExpansion (v1, v2)
  | `Null tok ->
      let x = token env tok (* "nullptr" *) in
      C (Nullptr x)
  | `This tok ->
      let x = token env tok (* "this" *) in
      IdSpecial (This, x)
  | `Raw_str_lit tok ->
      let x = str env tok in
      C (String (x, IsChar))

(* raw_string_literal *)
and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) :
    expr option * sc =
  let v1 =
    match v1 with
    | Some x -> Some (map_anon_choice_exp_55b4dba env x)
    | None -> None
  in
  let v2 = token env v2 (* ";" *) in
  (v1, v2)

and map_field_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.field_declaration) : class_member =
  let v1 = List.map (map_attribute env) v1 in
  let v2 =
    match v2 with
    | Some x -> [ M (map_virtual_function_specifier env x) ]
    | None -> []
  in
  let t, specs = map_declaration_specifiers env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let v1 = map_field_declarator env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_field_declarator env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v5 =
    match v5 with
    | Some x -> (
        match x with
        | `Bitf_clause x ->
            let x = map_bitfield_clause env x in
            todo env x
        | `Init_list x ->
            let x = map_initializer_list env x in
            todo env x
        | `EQ_choice_exp (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_anon_choice_exp_3078596 env v2 in
            todo env (v1, v2))
    | None -> todo env ()
  in
  let v6 = token env v6 (* ";" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_field_declaration_list (env : env)
    ((v1, v2, v3) : CST.field_declaration_list) :
    class_member sequencable list brace =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (map_field_declaration_list_item env) v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, List.flatten v2, v3)

and map_field_declaration_list_item (env : env)
    (x : CST.field_declaration_list_item) : class_member sequencable list =
  match x with
  | `Choice_field_decl x -> (
      match x with
      | `Field_decl x ->
          let x = map_field_declaration env x in
          [ X x ]
      | `Prep_def x ->
          let x = map_preproc_def env x in
          [ CppDirective x ]
      | `Prep_func_def x ->
          let x = map_preproc_function_def env x in
          [ CppDirective x ]
      | `Prep_call x ->
          let x = map_preproc_call env x in
          [ CppDirective x ]
      | `Prep_if_in_field_decl_list x ->
          let x = map_preproc_if_in_field_declaration_list env x in
          x
      | `Prep_ifdef_in_field_decl_list x ->
          let x = map_preproc_ifdef_in_field_declaration_list env x in
          x)
  | `Temp_decl x ->
      let x = map_template_declaration env x in
      [ X (MemberDecl x) ]
  | `Inline_meth_defi (v1, v2, v3, v4, v5) ->
      let v1 = List.map (map_attribute env) v1 in
      let v2 =
        match v2 with
        | Some x -> [ M (map_virtual_function_specifier env x) ]
        | None -> []
      in
      let t, specs = map_declaration_specifiers env v3 in
      let { dn; dt } = map_field_declarator env v4 in
      let v5 = map_anon_choice_comp_stmt_be91723 env v5 in

      let t = dt t in
      let ent, def = H2.fixFunc ((dn, t, NoSto), v5) in
      let fdef =
        ( {
            ent with
            specs = ent.specs @ List.map (fun x -> A x) v1 @ v2 @ specs;
          },
          def )
      in
      [ X (MemberDecl (Func fdef)) ]
  | `Cons_or_dest_defi x ->
      let x = map_constructor_or_destructor_definition env x in
      [ X (MemberDecl (Func x)) ]
  | `Cons_or_dest_decl x ->
      let x = map_constructor_or_destructor_declaration env x in
      [ X (MemberDecl (Func x)) ]
  | `Op_cast_defi x ->
      let x = map_operator_cast_definition env x in
      [ X (MemberDecl (Func x)) ]
  | `Op_cast_decl x ->
      let x = map_operator_cast_declaration env x in
      [ X (MemberDecl (Func x)) ]
  | `Friend_decl (v1, v2) ->
      let v1 = token env v1 (* "friend" *) in
      let v2 =
        match v2 with
        | `Decl x ->
            let x = map_declaration env x in
            DeclList x
        | `Func_defi x ->
            let x = map_function_definition env x in
            Func x
        | `Opt_choice_class_class_name_SEMI (v1, v2, v3) ->
            let v1 =
              match v1 with
              | Some x -> (
                  match x with
                  | `Class tok -> Some (Class, token env tok) (* "class" *)
                  | `Struct tok -> Some (Struct, token env tok) (* "struct" *)
                  | `Union tok -> Some (Union, token env tok) (* "union" *))
              | None -> None
            in
            let v2 = map_class_name env v2 in
            let v3 = token env v3 (* ";" *) in
            let t =
              match v1 with
              | None -> (nQ, TypeName v2)
              | Some class_key -> (nQ, ClassName (class_key, v2))
            in
            let one =
              { v_namei = None; v_type = t; v_storage = NoSto; v_specs = [] }
            in
            DeclList ([ one ], v3)
      in
      [ X (Friend (v1, v2)) ]
  | `Access_spec (v1, v2) ->
      let v1 = map_anon_choice_public_c9638d9 env v1 in
      let v2 = token env v2 (* ":" *) in
      [ X (Access (v1, v2)) ]
  | `Alias_decl x ->
      let x = map_alias_declaration env x in
      [ X (MemberDecl (UsingDecl x)) ]
  | `Using_decl x ->
      let x = map_using_declaration env x in
      [ X (MemberDecl (UsingDecl x)) ]
  | `Type_defi x ->
      let x = map_type_definition env x in
      [ X (MemberDecl (DeclList x)) ]
  | `Static_assert_decl x ->
      let x = map_static_assert_declaration env x in
      [ X (MemberDecl x) ]

and map_field_declarator (env : env) (x : CST.field_declarator) : declarator =
  match x with
  | `Choice_poin_field_decl x -> (
      match x with
      | `Poin_field_decl x -> map_pointer_field_declarator env x
      | `Func_field_decl x -> map_function_field_declarator env x
      | `Array_field_decl x -> map_array_field_declarator env x
      | `Paren_field_decl x -> map_parenthesized_field_declarator env x
      | `Id tok ->
          let x = str env tok (* pattern [a-zA-Z_]\w* *) in
          { dn = name_of_id x; dt = id })
  | `Ref_field_decl (v1, v2) ->
      let v1 = map_anon_choice_AMP_c92c117 env v1 in
      let v2 = map_field_declarator env v2 in
      todo env (v1, v2)
  | `Temp_meth x ->
      let x = map_template_method env x in
      { dn = x; dt = id }
  | `Op_name tok ->
      let x = str env tok (* operator_name *) in
      { dn = (None, [], IdOperator (parse_operator env x)); dt = id }

and map_field_expression (env : env) (x : CST.field_expression) : expr =
  match x with
  | `Exp_choice_DOT_id (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_DOT_2ad1dab env v2 in
      let v3 = str env v3 (* pattern [a-zA-Z_]\w* *) in
      DotAccess (v1, v2, name_of_id v3)
  | `Exp_choice_DOT_choice_dest_name (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_DOT_2ad1dab env v2 in
      let v3 =
        match v3 with
        | `Dest_name x ->
            let x = map_destructor_name env x in
            (None, [], x)
        | `Temp_meth x ->
            let x = map_template_method env x in
            x
      in
      DotAccess (v1, v2, v3)

and map_field_initializer (env : env) ((v1, v2, v3) : CST.field_initializer) =
  let v1 = map_anon_choice_stmt_id_ae28a26 env v1 in
  let v2 =
    match v2 with
    | `Init_list x ->
        let x = map_initializer_list env x in
        Inits x
    | `Arg_list x ->
        let x = map_argument_list env x in
        Args x
  in
  let _v3TODO =
    match v3 with Some tok -> Some (token env tok) (* "..." *) | None -> None
  in
  (v1, v2)

and map_field_initializer_list (env : env)
    ((v1, v2, v3) : CST.field_initializer_list) =
  let _v1 = token env v1 (* ":" *) in
  let v2 = map_field_initializer env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = map_field_initializer env v2 in
        v2)
      v3
  in
  v2 :: v3

and map_function_declarator (env : env)
    ((v1, v2, v3, v4) : CST.function_declarator) : declarator =
  let v1 = map_declarator env v1 in
  let v2 = map_parameter_list env v2 in
  let _v3 = List.map (map_attribute_specifier env) v3 in
  let ft_specs, ft_throw =
    Common.partition_either (map_anon_choice_type_qual_01506e0 env) v4
  in
  {
    dn = v1.dn;
    dt =
      (fun x ->
        v1.dt
          ( nQ,
            TFunction
              {
                ft_ret = x;
                ft_params = v2;
                ft_specs;
                ft_const = None;
                ft_throw;
              } ));
  }

and map_function_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.function_definition) : func_definition =
  let v1 = List.map (map_attribute env) v1 in
  let v2 =
    match v2 with
    | Some x ->
        let s, t = map_ms_call_modifier env x in
        [ M (MsCall (s, t)) ]
    | None -> []
  in
  let t, specs = map_declaration_specifiers env v3 in
  let { dn; dt } = map_declarator env v4 in
  let v5 = map_compound_statement env v5 in
  let t = dt t in
  let ent, def = H2.fixFunc ((dn, t, NoSto), FBDef v5) in
  ({ ent with specs = ent.specs @ List.map (fun x -> A x) v1 @ v2 }, def)

and map_function_field_declarator (env : env)
    ((v1, v2, v3) : CST.function_field_declarator) =
  let v1 = map_field_declarator env v1 in
  let v2 = map_parameter_list env v2 in
  let ft_specs, ft_throw =
    Common.partition_either (map_anon_choice_type_qual_01506e0 env) v3
  in

  {
    dn = v1.dn;
    dt =
      (fun x ->
        v1.dt
          ( nQ,
            TFunction
              {
                ft_ret = x;
                ft_params = v2;
                ft_specs;
                ft_const = None;
                ft_throw;
              } ));
  }

and map_init_declarator (env : env) (x : CST.init_declarator) =
  match x with
  | `Decl_EQ_choice_init_list (v1, v2, v3) ->
      let v1 = map_declarator env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 =
        match v3 with
        | `Init_list x ->
            let x = map_initializer_list env x in
            InitList x
        | `Exp x ->
            let x = map_expression env x in
            InitExpr x
      in
      (v1, EqInit (v2, v3))
  | `Decl_choice_arg_list (v1, v2) ->
      let v1 = map_declarator env v1 in
      let v2 = map_anon_choice_arg_list_e4b6f8f env v2 in
      (v1, ObjInit v2)

and map_initializer_list (env : env) ((v1, v2, v3, v4) : CST.initializer_list) :
    initialiser list brace =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_init_pair_1a6981e env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_init_pair_1a6981e env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = trailing_comma env v3 in
  let v4 = token env v4 (* "}" *) in
  (v1, v2, v4)

and map_lambda_capture_specifier (env : env)
    ((v1, v2, v3) : CST.lambda_capture_specifier) : lambda_capture list bracket
    =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | `Lambda_defa_capt x ->
        let x = map_lambda_default_capture env x in
        [ x ]
    | `Opt_exp_rep_COMMA_exp opt -> (
        match opt with
        | Some (v1, v2) ->
            let v1 = map_expression env v1 in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = map_expression env v2 in
                  v2)
                v2
            in
            v1 :: v2 |> List.map (fun e -> CaptureOther e)
        | None -> [])
    | `Lambda_defa_capt_COMMA_exp_rep_COMMA_exp (v1, v2, v3, v4) ->
        let v1 = map_lambda_default_capture env v1 in
        let _v2 = token env v2 (* "," *) in
        let v3 = map_expression env v3 in
        let v4 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_expression env v2 in
              v2)
            v4
        in
        v1 :: (v3 :: v4 |> List.map (fun e -> CaptureOther e))
  in
  let v3 = token env v3 (* "]" *) in
  (v1, v2, v3)

and map_linkage_specification (env : env)
    ((v1, v2, v3) : CST.linkage_specification) =
  let v1 = token env v1 (* "extern" *) in
  let v2 = map_string_literal env v2 in
  let v3 =
    match v3 with
    | `Func_defi x ->
        let x = map_function_definition env x in
        ExternDecl (v1, v2, Func x)
    | `Decl x ->
        let x = map_declaration env x in
        ExternDecl (v1, v2, DeclList x)
    | `Decl_list x ->
        let x = map_declaration_list env x in
        ExternList (v1, v2, x)
  in
  v3

and map_ms_based_modifier (env : env) ((v1, v2) : CST.ms_based_modifier) =
  let v1 = token env v1 (* "__based" *) in
  let v2 = map_argument_list env v2 in
  Based (v1, v2)

and map_new_declarator (env : env) (x : CST.new_declarator) =
  match x with
  | `Rectype (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "]" *) in
      let v4 =
        match v4 with Some x -> Some (map_new_declarator env x) | None -> None
      in
      todo env (v1, v2, v3, v4)

and map_noexcept (env : env) ((v1, v2) : CST.noexcept) : exn_spec =
  let v1 = token env v1 (* "noexcept" *) in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* "(" *) in
        let v2 =
          match v2 with Some x -> Some (map_expression env x) | None -> None
        in
        let v3 = token env v3 (* ")" *) in
        Some (v1, v2, v3)
    | None -> None
  in
  Noexcept (v1, v2)

and map_non_case_statement (env : env) (x : CST.non_case_statement) : stmt =
  match x with
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = str env v1 (* pattern [a-zA-Z_]\w* *) in
      let v2 = token env v2 (* ":" *) in
      let v3 = map_statement env v3 in
      Label (v1, v2, v3)
  | `Comp_stmt x ->
      let l, xs, r = map_compound_statement env x in
      Compound (l, xs, r)
  | `Exp_stmt x ->
      let eopt, sc = map_expression_statement env x in
      ExprStmt (eopt, sc)
  | `If_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "if" *) in
      let v2 =
        match v2 with
        | Some tok -> Some (token env tok) (* "constexpr" *)
        | None -> None
      in
      let v3 = map_condition_clause env v3 in
      let v4 = map_statement env v4 in
      let v5 =
        match v5 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "else" *) in
            let v2 = map_statement env v2 in
            Some (v1, v2)
        | None -> None
      in
      If (v1, v2, v3, v4, v5)
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = map_condition_clause env v2 in
      let l, xs, r = map_compound_statement env v3 in
      Switch (v1, v2, Compound (l, xs, r))
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = map_statement env v2 in
      let v3 = token env v3 (* "while" *) in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = token env v5 (* ";" *) in
      DoWhile (v1, v2, v3, v4, v5)
  | `While_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = map_condition_clause env v2 in
      let v3 = map_statement env v3 in
      While (v1, v2, v3)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        match v3 with
        | `Decl x ->
            let x = map_declaration env x in
            Right x
        | `Opt_choice_exp_SEMI x ->
            let eopt, sc = map_expression_statement env x in
            Left (eopt, sc)
      in
      let v4 =
        match v4 with Some x -> Some (map_expression env x) | None -> None
      in
      let _v5 = token env v5 (* ";" *) in
      let v6 =
        match v6 with
        | Some x -> Some (map_anon_choice_exp_55b4dba env x)
        | None -> None
      in
      let v7 = token env v7 (* ")" *) in
      let v8 = map_statement env v8 in
      For (v1, (v2, ForClassic (v3, v4, v6), v7), v8)
  | `Ret_stmt x -> map_return_statement env x
  | `Brk_stmt (v1, v2) ->
      let v1 = token env v1 (* "break" *) in
      let v2 = token env v2 (* ";" *) in
      Jump (Break v1, v2)
  | `Cont_stmt (v1, v2) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 = token env v2 (* ";" *) in
      Jump (Continue v1, v2)
  | `Goto_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "goto" *) in
      let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
      let v3 = token env v3 (* ";" *) in
      Jump (Goto (v1, v2), v3)

and map_operator_cast (env : env) ((v1, v2, v3, v4) : CST.operator_cast) =
  let v1 =
    match v1 with
    | Some (v1, v2) ->
        let v1 =
          match v1 with
          | Some x -> map_anon_choice_stmt_id_ec78ce4 env x
          | None -> todo env ()
        in
        let v2 = token env v2 (* "::" *) in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v2 = token env v2 (* "operator" *) in
  let t, specs = map_declaration_specifiers env v3 in
  let v4 = map_abstract_declarator env v4 in
  todo env (v1, v2, v3, v4)

and map_operator_cast_declaration (env : env)
    ((v1, v2, v3, v4) : CST.operator_cast_declaration) =
  let v1 =
    match v1 with Some x -> map_constructor_specifiers env x | None -> []
  in
  let v2 = map_operator_cast env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v4 = token env v4 (* ";" *) in
  todo env (v1, v2, v3, v4)

and map_operator_cast_definition (env : env)
    ((v1, v2, v3) : CST.operator_cast_definition) =
  let v1 =
    match v1 with Some x -> map_constructor_specifiers env x | None -> []
  in
  let v2 = map_operator_cast env v2 in
  let v3 = map_anon_choice_comp_stmt_be91723 env v3 in
  todo env (v1, v2, v3)

and map_optional_parameter_declaration (env : env)
    ((v1, v2, v3, v4) : CST.optional_parameter_declaration) : parameter_classic
    =
  let t, specs = map_declaration_specifiers env v1 in
  let v3 = token env v3 (* "=" *) in
  let v4 = map_expression env v4 in
  let v2 =
    match v2 with
    | Some x ->
        let { dn; dt } = map_declarator env x in
        let id = name_for_param dn in
        { (basic_param id (dt t) specs) with p_val = Some (v3, v4) }
    | None ->
        {
          p_name = None;
          p_type = t;
          p_register = None;
          p_specs = specs;
          p_val = Some (v3, v4);
        }
  in
  v2

and map_optional_type_parameter_declaration (env : env)
    ((v1, v2, v3, v4) : CST.optional_type_parameter_declaration) =
  let v1 = map_anon_choice_type_a2fe5d4 env v1 in
  let v2 =
    match v2 with
    | Some tok -> Some (str env tok) (* pattern [a-zA-Z_]\w* *)
    | None -> None
  in
  let _v3 = token env v3 (* "=" *) in
  let v4 = map_type_specifier env v4 in
  TPClass (v1, v2, Some v4)

and map_parameter_declaration (env : env)
    ((v1, v2, v3) : CST.parameter_declaration) : parameter_classic =
  let v1 = List.map (map_attribute env) v1 in
  let t, specs = map_declaration_specifiers env v2 in
  let specs = (v1 |> List.map (fun x -> A x)) @ specs in
  let v3 =
    match v3 with
    | Some x -> (
        match x with
        | `Decl x ->
            let { dn; dt } = map_declarator env x in
            let id = name_for_param dn in
            basic_param id (dt t) specs
        | `Abst_decl x ->
            let dt = map_abstract_declarator env x in
            {
              p_name = None;
              p_type = dt t;
              p_specs = specs;
              p_register = None;
              p_val = None;
            })
    | None ->
        {
          p_name = None;
          p_type = t;
          p_specs = specs;
          p_register = None;
          p_val = None;
        }
  in
  v3

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) :
    parameter list paren =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_param_decl_d9083af env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_param_decl_d9083af env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and map_parenthesized_declarator (env : env)
    ((v1, v2, v3) : CST.parenthesized_declarator) : declarator =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_declarator env v2 in
  let v3 = token env v3 (* ")" *) in
  { dn = v2.dn; dt = (fun x -> (nQ, ParenType (v1, v2.dt x, v3))) }

and map_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : expr paren =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_anon_choice_exp_55b4dba env v2 in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and map_parenthesized_field_declarator (env : env)
    ((v1, v2, v3) : CST.parenthesized_field_declarator) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_field_declarator env v2 in
  let v3 = token env v3 (* ")" *) in
  { dn = v2.dn; dt = (fun x -> (nQ, ParenType (v1, v2.dt x, v3))) }

and map_pointer_declarator (env : env)
    ((v1, v2, v3, v4, v5) : CST.pointer_declarator) : declarator =
  let v1 =
    match v1 with Some x -> [ map_ms_based_modifier env x ] | None -> []
  in
  let v2 = token env v2 (* "*" *) in
  let v3 = List.map (map_ms_pointer_modifier env) v3 in
  let v4 = List.map (map_type_qualifier env) v4 in
  let f1 x = (v4, TPointer (v2, x, v1 @ v3)) in
  let v5 = map_declarator env v5 in
  { dn = v5.dn; dt = (fun x -> x |> f1 |> v5.dt) }

and map_pointer_expression (env : env) ((v1, v2) : CST.pointer_expression) :
    expr =
  let v1 =
    match v1 with
    | `STAR tok -> (DeRef, token env tok) (* "*" *)
    | `AMP tok -> (GetRef, token env tok)
    (* "&" *)
  in
  let v2 = map_expression env v2 in
  Unary (v1, v2)

and map_pointer_field_declarator (env : env)
    ((v1, v2, v3, v4, v5) : CST.pointer_field_declarator) : declarator =
  let v1 =
    match v1 with Some x -> [ map_ms_based_modifier env x ] | None -> []
  in
  let v2 = token env v2 (* "*" *) in
  let v3 = List.map (map_ms_pointer_modifier env) v3 in
  let v4 = List.map (map_type_qualifier env) v4 in
  let f1 x = (v4, TPointer (v2, x, v1 @ v3)) in
  let v5 = map_field_declarator env v5 in
  { dn = v5.dn; dt = (fun x -> x |> f1 |> v5.dt) }

and map_preproc_if (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if) :
    toplevel list =
  (* TODO: IfIf *)
  let v1 = token env v1 (* pattern #[ 	]*if *) in
  let _v2TODO = map_preproc_expression env v2 in
  let v3 = token env v3 (* "\n" *) in
  let dir1 = Ifdef v1 in
  let v4 = map_translation_unit env v4 in
  let v5 =
    match v5 with
    | Some x -> map_anon_choice_prep_else_8b52b0f env x
    | None -> []
  in
  let v6 = token env v6 (* pattern #[ 	]*endif *) in
  let dir2 = IfdefEndif v6 in
  (CppIfdef dir1 :: v4) @ v5 @ [ CppIfdef dir2 ]

and map_preproc_if_in_field_declaration_list (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_field_declaration_list) :
    class_member sequencable list =
  (* TODO: IfIf *)
  let v1 = token env v1 (* pattern #[ 	]*if *) in
  let _v2TODO = map_preproc_expression env v2 in
  let v3 = token env v3 (* "\n" *) in
  let dir1 = Ifdef v1 in
  let v4 = List.map (map_field_declaration_list_item env) v4 in
  let v5 =
    match v5 with
    | Some x -> map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
    | None -> []
  in
  let v6 = token env v6 (* pattern #[ 	]*endif *) in
  let dir2 = IfdefEndif v6 in
  (CppIfdef dir1 :: List.flatten v4) @ v5 @ [ CppIfdef dir2 ]

and map_preproc_ifdef (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef) :
    toplevel list =
  let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
  let dir1 = v1 in
  let _v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = map_translation_unit env v3 in
  let v4 =
    match v4 with
    | Some x -> map_anon_choice_prep_else_8b52b0f env x
    | None -> []
  in
  let v5 = token env v5 (* pattern #[ 	]*endif *) in
  let dir2 = IfdefEndif v5 in
  (CppIfdef dir1 :: v3) @ v4 @ [ CppIfdef dir2 ]

and map_preproc_ifdef_in_field_declaration_list (env : env)
    ((v1, v2, v3, v4, v5) : CST.preproc_ifdef_in_field_declaration_list) :
    class_member sequencable list =
  let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
  let dir1 = v1 in
  let _v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = List.map (map_field_declaration_list_item env) v3 in
  let v4 =
    match v4 with
    | Some x -> map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
    | None -> []
  in
  let v5 = token env v5 (* pattern #[ 	]*endif *) in
  let dir2 = IfdefEndif v5 in
  (CppIfdef dir1 :: List.flatten v3) @ v4 @ [ CppIfdef dir2 ]

and map_return_statement (env : env) (x : CST.return_statement) : stmt =
  match x with
  | `Ret_opt_choice_exp_SEMI (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        match v2 with
        | Some x -> Some (Arg (map_anon_choice_exp_55b4dba env x))
        | None -> None
      in
      let v3 = token env v3 (* ";" *) in
      Jump (Return (v1, v2), v3)
  | `Ret_init_list_SEMI (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 = map_initializer_list env v2 in
      let v3 = token env v3 (* ";" *) in
      Jump (Return (v1, Some (ArgInits v2)), v3)

and map_scoped_identifier (env : env) ((v1, v2, v3) : CST.scoped_identifier) :
    name =
  let v1 =
    match v1 with
    | Some x -> Some (map_anon_choice_stmt_id_ec78ce4 env x)
    | None -> None
  in
  let v2 = token env v2 (* "::" *) in
  let v3 = map_anon_choice_stmt_id_efddc5b env v3 in
  name_scoped v1 v2 v3

and map_scoped_namespace_identifier (env : env)
    ((v1, v2, v3) : CST.scoped_namespace_identifier) =
  let v1 =
    match v1 with
    | Some x -> Some (map_anon_choice_stmt_id_ec78ce4 env x)
    | None -> None
  in
  let v2 = token env v2 (* "::" *) in
  let v3 = str env v3 (* pattern [a-zA-Z_]\w* *) in
  name_scoped v1 v2 (IdIdent v3)

and map_scoped_type_identifier (env : env)
    ((v1, v2, v3) : CST.scoped_type_identifier) : name =
  let v1 =
    match v1 with
    | Some x -> Some (map_anon_choice_stmt_id_ec78ce4 env x)
    | None -> None
  in
  let v2 = token env v2 (* "::" *) in
  let v3 = str env v3 (* pattern [a-zA-Z_]\w* *) in
  name_scoped v1 v2 (IdIdent v3)

and map_sizeof_expression (env : env) (x : CST.sizeof_expression) : expr =
  match x with
  | `Sizeof_choice_exp (v1, v2) ->
      let v1top = token env v1 (* "sizeof" *) in
      let v2 =
        match v2 with
        | `Exp x ->
            let x = map_expression env x in
            SizeOf (v1top, Left x)
        | `LPAR_type_desc_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 = map_type_descriptor env v2 in
            let v3 = token env v3 (* ")" *) in
            SizeOf (v1top, Right (v1, v2, v3))
      in
      v2
  | `Sizeof_DOTDOTDOT_LPAR_id_RPAR (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "sizeof" *) in
      let v2 = token env v2 (* "..." *) in
      let v3 = token env v3 (* "(" *) in
      let v4 = str env v4 (* pattern [a-zA-Z_]\w* *) in
      let v5 = token env v5 (* ")" *) in
      ExprTodo (("SizeofDots", v1), [])

and map_statement (env : env) (x : CST.statement) : stmt =
  match x with
  | `Choice_case_stmt x -> (
      match x with
      | `Case_stmt x ->
          let x = map_case_statement env x in
          x
      | `Choice_labe_stmt x ->
          let x = map_non_case_statement env x in
          x)
  | `For_range_loop (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "(" *) in
      let t, specs = map_declaration_specifiers env v3 in
      let v4 = map_declarator env v4 in
      let v5 = token env v5 (* ":" *) in
      let v6 = map_anon_choice_exp_3078596 env v6 in
      let v7 = token env v7 (* ")" *) in
      let v8 = map_statement env v8 in
      let ent = { name = v4.dn; specs } in
      let var = { v__type = v4.dt t } in
      let for_header = ForRange ((ent, var), v5, v6) in
      For (v1, (v2, for_header, v7), v8)
  | `Try_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = map_compound_statement env v2 in
      let v3 = List.map (map_catch_clause env) v3 in
      Try (v1, v2, v3)
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 =
        match v2 with Some x -> Some (map_expression env x) | None -> None
      in
      let v3 = token env v3 (* ";" *) in
      ExprStmt (Some (Throw (v1, v2)), v3)

and map_static_assert_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.static_assert_declaration) =
  let v1 = token env v1 (* "static_assert" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = map_expression env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 =
          match v2 with
          | `Str_lit x ->
              let x = map_string_literal env x in
              String (x, IsChar)
          | `Raw_str_lit tok ->
              let x = str env tok (* raw_string_literal *) in
              String (x, IsChar)
          | `Conc_str x ->
              let x = map_concatenated_string env x in
              x
        in
        [ Arg (C v2) ]
    | None -> []
  in
  let v5 = token env v5 (* ")" *) in
  let v6 = token env v6 (* ";" *) in
  let args = (v2, Arg v3 :: v4, v5) in
  StaticAssert (v1, args)

and map_subscript_designator (env : env)
    ((v1, v2, v3) : CST.subscript_designator) : designator =
  let v1 = token env v1 (* "[" *) in
  let v2 = map_expression env v2 in
  let v3 = token env v3 (* "]" *) in
  DesignatorIndex (v1, v2, v3)

and map_subscript_expression (env : env)
    ((v1, v2, v3, v4) : CST.subscript_expression) : expr =
  let v1 = map_expression env v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = map_expression env v3 in
  let v4 = token env v4 (* "]" *) in
  ArrayAccess (v1, (v2, v3, v4))

and map_template_argument_list (env : env)
    ((v1, v2, v3) : CST.template_argument_list) : template_arguments =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_type_desc_4d9cafa env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_type_desc_4d9cafa env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* tok_GT *) in
  (v1, v2, v3)

and map_template_declaration (env : env)
    ((v1, v2, v3) : CST.template_declaration) : decl =
  let v1 = token env v1 (* "template" *) in
  let v2 = map_template_parameter_list env v2 in
  let v3 =
    match v3 with
    | `Empty_decl x ->
        let x = map_empty_declaration env x in
        x
    | `Alias_decl x ->
        let x = map_alias_declaration env x in
        UsingDecl x
    | `Decl x ->
        let x = map_declaration env x in
        DeclList x
    | `Temp_decl x ->
        let x = map_template_declaration env x in
        x
    | `Func_defi x ->
        let x = map_function_definition env x in
        Func x
    | `Cons_or_dest_decl x ->
        let x = map_constructor_or_destructor_declaration env x in
        Func x
    | `Cons_or_dest_defi x ->
        let x = map_constructor_or_destructor_definition env x in
        Func x
    | `Op_cast_decl x ->
        let x = map_operator_cast_declaration env x in
        Func x
    | `Op_cast_defi x ->
        let x = map_operator_cast_definition env x in
        Func x
  in
  TemplateDecl (v1, v2, v3)

and map_template_function (env : env) ((v1, v2) : CST.template_function) : name
    =
  let v1 = map_anon_choice_stmt_id_f1f5a37 env v1 in
  let v2 = map_template_argument_list env v2 in
  name_add_template_args v1 v2

and map_template_method (env : env) ((v1, v2) : CST.template_method) : name =
  let v1 = map_anon_choice_stmt_id_ae28a26 env v1 in
  let v2 = map_template_argument_list env v2 in
  name_add_template_args v1 v2

and map_template_parameter_list (env : env)
    ((v1, v2, v3) : CST.template_parameter_list) : template_parameters =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_param_decl_13b5913 env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_anon_choice_param_decl_13b5913 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* tok_GT *) in
  (v1, v2, v3)

and map_template_type (env : env) ((v1, v2) : CST.template_type) : name =
  let v1 =
    match v1 with
    | `Id tok ->
        let x = str env tok (* pattern [a-zA-Z_]\w* *) in
        name_of_id x
    | `Scoped_type_id x ->
        let x = map_scoped_type_identifier env x in
        x
  in
  let v2 = map_template_argument_list env v2 in
  name_add_template_args v1 v2

and map_throw_specifier (env : env) ((v1, v2, v3, v4) : CST.throw_specifier) :
    exn_spec =
  let v1 = token env v1 (* "throw" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = map_type_descriptor env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = map_type_descriptor env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v4 = token env v4 (* ")" *) in
  ThrowSpec (v1, (v2, v3, v4))

and map_top_level_item (env : env) (x : CST.top_level_item) : toplevel list =
  match x with
  | `Choice_func_defi x -> (
      match x with
      | `Func_defi x ->
          let x = map_function_definition env x in
          [ X (D (Func x)) ]
      | `Link_spec x ->
          let x = map_linkage_specification env x in
          [ X (D x) ]
      | `Decl x ->
          let x = map_declaration env x in
          [ X (D (DeclList x)) ]
      | `Choice_choice_case_stmt x ->
          let x = map_statement env x in
          [ X (S x) ]
      | `Type_defi x ->
          let x = map_type_definition env x in
          [ X (D (DeclList x)) ]
      | `Empty_decl x ->
          let x = map_empty_declaration env x in
          [ X (D x) ]
      | `Prep_if x ->
          let x = map_preproc_if env x in
          x
      | `Prep_ifdef x ->
          let x = map_preproc_ifdef env x in
          x
      | `Prep_incl x ->
          let x = map_preproc_include env x in
          [ CppDirective x ]
      | `Prep_def x ->
          let x = map_preproc_def env x in
          [ CppDirective x ]
      | `Prep_func_def x ->
          let x = map_preproc_function_def env x in
          [ CppDirective x ]
      | `Prep_call x ->
          let x = map_preproc_call env x in
          [ CppDirective x ])
  | `Name_defi (v1, v2, v3) ->
      let v1 = token env v1 (* "namespace" *) in
      let v2 =
        match v2 with
        | Some tok -> Some (str env tok) (* pattern [a-zA-Z_]\w* *)
        | None -> None
      in
      let v3 = map_declaration_list env v3 in
      [ X (D (NameSpace (v1, v2, v3))) ]
  | `Using_decl x ->
      let x = map_using_declaration env x in
      [ X (D (UsingDecl x)) ]
  | `Alias_decl x ->
      let x = map_alias_declaration env x in
      [ X (D (UsingDecl x)) ]
  | `Static_assert_decl x ->
      let x = map_static_assert_declaration env x in
      [ X (D x) ]
  | `Temp_decl x ->
      let x = map_template_declaration env x in
      [ X (D x) ]
  | `Temp_inst (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "template" *) in
      let v2 =
        match v2 with
        | Some x ->
            let t, specs = map_declaration_specifiers env x in
            todo env ()
        | None -> todo env ()
      in
      let v3 = map_declarator env v3 in
      let v4 = token env v4 (* ";" *) in
      todo env (v1, v2, v3, v4)
  | `Cons_or_dest_defi x ->
      let x = map_constructor_or_destructor_definition env x in
      [ X (D (Func x)) ]
  | `Op_cast_defi x ->
      let x = map_operator_cast_definition env x in
      [ X (D (Func x)) ]
  | `Op_cast_decl x ->
      let x = map_operator_cast_declaration env x in
      [ X (D (Func x)) ]

and map_trailing_return_type (env : env)
    ((v1, v2, v3, v4) : CST.trailing_return_type) : type_ =
  let v1 = token env v1 (* "->" *) in
  let _v2TODO =
    match v2 with Some x -> [ map_type_qualifier env x ] | None -> []
  in
  let v3 = map_type_specifier env v3 in
  let v4 =
    match v4 with Some x -> map_abstract_declarator env x | None -> id
  in
  v4 v3

and map_translation_unit (env : env) (xs : CST.translation_unit) : program =
  List.map (map_top_level_item env) xs |> List.flatten

(* actually dn is an ident here, never a complex name *)
and map_type_declarator (env : env) (x : CST.type_declarator) : declarator =
  match x with
  | `Poin_type_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with Some x -> [ map_ms_based_modifier env x ] | None -> []
      in
      let v2 = token env v2 (* "*" *) in
      let v3 = List.map (map_ms_pointer_modifier env) v3 in
      let v4 = List.map (map_type_qualifier env) v4 in
      let f1 x = (v4, TPointer (v2, x, v1 @ v3)) in
      let v5 = map_type_declarator env v5 in
      { dn = v5.dn; dt = (fun x -> x |> f1 |> v5.dt) }
  | `Func_type_decl (v1, v2) ->
      let v1 = map_type_declarator env v1 in
      let v2 = map_parameter_list env v2 in
      {
        dn = v1.dn;
        dt =
          (fun x ->
            v1.dt
              ( nQ,
                TFunction
                  {
                    ft_ret = x;
                    ft_params = v2;
                    ft_specs = [];
                    ft_const = None;
                    ft_throw = [];
                  } ));
      }
  | `Array_type_decl (v1, v2, v3, v4, v5) ->
      let v1 = map_type_declarator env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = List.map (map_type_qualifier env) v3 in
      let v4 =
        match v4 with
        | Some x -> Some (map_anon_choice_exp_508611b env x)
        | None -> None
      in
      let v5 = token env v5 (* "]" *) in
      { dn = v1.dn; dt = (fun x -> v1.dt (v3, TArray ((v2, v4, v5), x))) }
  | `Paren_type_decl (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_type_declarator env v2 in
      let v3 = token env v3 (* ")" *) in
      { dn = v2.dn; dt = (fun x -> (nQ, ParenType (v1, v2.dt x, v3))) }
  | `Id tok ->
      let x = str env tok in
      { dn = name_of_id x; dt = id }

(* pattern [a-zA-Z_]\w* *)
and map_type_definition (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.type_definition) : vars_decl =
  let v1 = token env v1 (* "typedef" *) in
  let _v2TODO = List.map (map_type_qualifier env) v2 in
  let v3 = map_type_specifier env v3 in
  let v4 = map_type_declarator env v4 in
  let v5 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = map_type_declarator env v2 in
        v2)
      v5
  in
  let v6 = token env v6 (* ";" *) in

  let xs =
    v4 :: v5
    |> List.map (fun { dn; dt } ->
           let id = name_for_typedef dn in
           {
             v_namei = Some (name_of_id id, None);
             v_type = dt v3;
             v_storage = StoTypedef v1;
             v_specs = [];
           })
  in
  (xs, v6)

and map_type_descriptor (env : env) ((v1, v2, v3, v4) : CST.type_descriptor) :
    type_ =
  let v1 = List.map (map_type_qualifier env) v1 in
  let qs, tc = map_type_specifier env v2 in
  let v3 = List.map (map_type_qualifier env) v3 in
  let t = (v1 @ qs @ v3, tc) in
  let v4 =
    match v4 with Some x -> map_abstract_declarator env x | None -> id
  in
  v4 t

and map_type_specifier (env : env) (x : CST.type_specifier) : type_ =
  match x with
  | `Struct_spec (v1, v2, v3) ->
      let v1 = token env v1 (* "struct" *) in
      let _v2 =
        match v2 with
        | Some x -> [ map_ms_declspec_modifier env x ]
        | None -> []
      in
      let v3 = map_anon_choice_class_name_d6703e6 env v3 in
      v3 (Struct, v1)
  | `Union_spec (v1, v2, v3) ->
      let v1 = token env v1 (* "union" *) in
      let _v2 =
        match v2 with
        | Some x -> [ map_ms_declspec_modifier env x ]
        | None -> []
      in
      let v3 = map_anon_choice_class_name_d6703e6 env v3 in
      v3 (Union, v1)
  | `Enum_spec (v1, v2, v3) ->
      let v1 = token env v1 (* "enum" *) in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `Class tok -> token env tok (* "class" *)
            | `Struct tok -> token env tok (* "struct" *))
        | None -> todo env ()
      in
      let v3 =
        match v3 with
        | `Class_name_opt_enum_base_clause_opt_enum_list (v1, v2, v3) ->
            let v1 = map_class_name env v1 in
            let v2 =
              match v2 with
              | Some x -> map_enum_base_clause env x
              | None -> todo env ()
            in
            let v3 =
              match v3 with
              | Some x -> map_enumerator_list env x
              | None -> todo env ()
            in
            todo env (v1, v2, v3)
        | `Enum_list x -> map_enumerator_list env x
      in
      todo env (v1, v2, v3)
  | `Class_spec (v1, v2, v3) ->
      let v1 = token env v1 (* "class" *) in
      let _v2 =
        match v2 with
        | Some x -> [ map_ms_declspec_modifier env x ]
        | None -> []
      in
      let v3 = map_anon_choice_class_name_d6703e6 env v3 in
      v3 (Class, v1)
  | `Sized_type_spec x ->
      let x = map_sized_type_specifier env x in
      complicated env x
  | `Prim_type tok ->
      let x = str env tok (* primitive_type *) in
      parse_primitive_type x
  | `Temp_type x ->
      let x = map_template_type env x in
      (nQ, TypeName x)
  | `Auto tok ->
      let x = token env tok (* "auto" *) in
      (nQ, TAuto x)
  | `Depe_type (v1, v2) ->
      let v1 = token env v1 (* "typename" *) in
      let v2 = map_type_specifier env v2 in
      (nQ, TypenameKwd (v1, v2))
  | `Decl (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "decltype" *) in
      let _v2 = token env v2 (* "(" *) in
      let _v3 = map_expression env v3 in
      let _v4 = token env v4 (* ")" *) in
      (nQ, TypeTodo (("decltype", v1), []))
  | `Choice_scoped_type_id x ->
      let name =
        match x with
        | `Scoped_type_id x -> map_scoped_type_identifier env x
        | `Id tok ->
            let x = str env tok (* pattern [a-zA-Z_]\w* *) in
            name_of_id x
      in
      (nQ, TypeName name)

and map_unary_expression (env : env) ((v1, v2) : CST.unary_expression) =
  let v1 = map_anon_choice_BANG_67174d6 env v1 in
  let v2 = map_expression env v2 in
  Unary (v1, v2)

and map_update_expression (env : env) (x : CST.update_expression) : expr =
  match x with
  | `Choice_DASHDASH_exp (v1, v2) ->
      let v1 = map_anon_choice_DASHDASH_d11def2 env v1 in
      let v2 = map_expression env v2 in
      Prefix (v1, v2)
  | `Exp_choice_DASHDASH (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_DASHDASH_d11def2 env v2 in
      Postfix (v1, v2)

and map_using_declaration (env : env) ((v1, v2, v3, v4) : CST.using_declaration)
    : using =
  let v1 = token env v1 (* "using" *) in
  let v3 = map_anon_choice_stmt_id_f1f5a37 env v3 in
  let v4 = token env v4 (* ";" *) in
  let v2 =
    match v2 with
    | Some tok ->
        let n = token env tok (* "namespace" *) in
        (v1, UsingNamespace (n, v3), v4)
    | None -> (v1, UsingName v3, v4)
  in
  v2

and map_variadic_parameter_declaration (env : env)
    ((v1, v2) : CST.variadic_parameter_declaration) =
  let t, specs = map_declaration_specifiers env v1 in
  let v2 =
    match v2 with
    | `Vari_decl x ->
        let tdots, idopt = map_variadic_declarator env x in
        let p =
          {
            p_name = idopt;
            p_type = t;
            p_specs = specs;
            p_val = None;
            p_register = None;
          }
        in
        ParamVariadic (None, tdots, p)
    | `Vari_ref_decl x ->
        let ampersand, (tdots, idopt) =
          map_variadic_reference_declarator env x
        in
        let p =
          {
            p_name = idopt;
            p_type = t;
            p_specs = specs;
            p_val = None;
            p_register = None;
          }
        in
        ParamVariadic (Some ampersand, tdots, p)
  in
  v2

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_cpp.Parse.file file ())
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      try map_translation_unit env cst
      with Failure "not implemented" as exn ->
        H.debug_sexp_cst_after_error (CST.sexp_of_translation_unit cst);
        raise exn)

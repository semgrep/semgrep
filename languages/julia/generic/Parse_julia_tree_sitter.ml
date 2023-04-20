(* Brandon Wu
 *
 * Copyright (c) 2022 R2C
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
module CST = Tree_sitter_julia.CST
module H = Parse_tree_sitter_helpers
module G = AST_generic
open AST_generic
module H2 = AST_generic_helpers
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Julia parser using tree-sitter-lang/semgrep-julia and converting
 * directly to AST_generic.ml
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type context = Program | Pattern
type env = context H.env

let token = H.token
let str = H.str
let fb = PI.unsafe_fake_bracket
let sc tok = PI.sc tok

let in_pattern env =
  match env.H.extra with
  | Program -> false
  | Pattern -> true

(* In Jane Street Core, this exists as Option.all, reeee
 *)
let option_all xs =
  List.fold_right
    (fun x acc ->
      match (x, acc) with
      | _, None -> None
      | None, _ -> None
      | Some x, Some xs -> Some (x :: xs))
    xs (Some [])

let todo (_env : env) _ = failwith "not implemented lol"
let _invalid (_env : env) _ = failwith "invalid program"

let map_trailing_comma env v =
  match v with
  | Some tok -> Some ((* "," *) token env tok)
  | None -> None

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

let map_identifier (env : env) (tok : CST.identifier) = str env tok

let map_identifier_exp (env : env) (tok : CST.identifier) : expr =
  N (H2.name_of_id (map_identifier env tok)) |> G.e

let map_tok_abst_pat_3d340f6_type (env : env)
    (tok : CST.tok_abst_pat_3d340f6_type) =
  (* tok_abst_pat_3d340f6_type *) token env tok

let map_assign_operator (env : env) (tok : CST.assign_operator) =
  (* assign_operator *)
  let tok = token env tok in
  let doit op v1 v3 = AssignOp (v1, (op, tok), v3) |> G.e in
  match PI.str_of_info tok with
  | "+=" -> Plus |> doit
  | "-=" -> Minus |> doit
  | "*=" -> Mult |> doit
  | "/=" -> Div |> doit
  | "^=" -> Pow |> doit
  | "÷=" -> FloorDiv |> doit
  | "%=" -> Mod |> doit
  | "<<=" -> LSL |> doit
  | ">>>=" -> LSR |> doit
  | ">>=" -> ASR |> doit
  | "|=" -> BitOr |> doit
  | "&=" -> BitAnd |> doit
  | "⊻=" -> BitXor |> doit
  (*| "≔"
    | "⩴"
    | "//="
    | "\\="
    | "≕"
    | "~"
    | "$="
    | ":=" as s
  *)
  | s ->
      fun v1 v3 ->
        Call (N (H2.name_of_id (s, tok)) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e

let map_tok_choice_dot_choice_plus (env : env)
    (tok : CST.tok_choice_dot_choice_plus) =
  (* tok_choice_dot_choice_plus *) token env tok

let map_tok_choice_dot_choice_ampamp (env : env)
    (tok : CST.tok_choice_dot_choice_ampamp) =
  (* tok_choice_dot_choice_ampamp *) token env tok

let map_imm_tok_colon (env : env) (tok : CST.imm_tok_colon) =
  (* ":" *) token env tok

let map_imm_tok_choice_bare (env : env) (tok : CST.imm_tok_choice_bare) =
  (* imm_tok_choice_bare *) str env tok

let map_tok_choice_dot_choice_barbar (env : env)
    (tok : CST.tok_choice_dot_choice_barbar) =
  (* tok_choice_dot_choice_barbar *) token env tok

let map_pat_4aee1e1 (env : env) (tok : CST.pat_4aee1e1) =
  (* pattern ;+ *) token env tok

let map_imm_tok_dot (env : env) (tok : CST.imm_tok_dot) =
  (* "." *) token env tok

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  match x with
  | `True tok -> (* "true" *) L (Bool (true, token env tok)) |> G.e
  | `False tok -> (* "false" *) L (Bool (false, token env tok)) |> G.e

let map_imm_tok_choice_tok_choice_dot_choice_plus (env : env)
    (tok : CST.imm_tok_choice_tok_choice_dot_choice_plus) =
  (* imm_tok_choice_tok_choice_dot_choice_plus *) str env tok

let map_terminator (env : env) (x : CST.terminator) =
  match x with
  | `LF tok -> (* "\n" *) token env tok
  | `Pat_4aee1e1 x -> map_pat_4aee1e1 env x

let map_terminator_opt (env : env) (x : CST.terminator option) =
  match x with
  | None -> None
  | Some x -> Some (map_terminator env x)

let map_newline_opt (env : env) x =
  match x with
  | None -> None
  | Some tok -> Some (token env tok)

let map_anon_choice_str_content_no_interp_24ac4f9 (env : env)
    (x : CST.anon_choice_str_content_no_interp_24ac4f9) =
  match x with
  | `Str_content_no_interp tok -> (* string_content_no_interp *) str env tok
  | `Esc_seq tok -> (* escape_sequence *) str env tok

let map_operator (env : env) (x : CST.operator) =
  match x with
  | `Pair_op tok -> (* pair_operator *) str env tok
  | `Arrow_op tok -> (* arrow_operator *) str env tok
  | `Comp_op tok -> (* comparison_operator *) str env tok
  | `Pipe_left_op tok -> (* pipe_left_operator *) str env tok
  | `Pipe_right_op tok -> (* pipe_right_operator *) str env tok
  | `Ellips_op tok -> (* ellipsis_operator *) str env tok
  | `Plus_op tok -> (* plus_operator *) str env tok
  | `Times_op tok -> (* times_operator *) str env tok
  | `Rati_op tok -> (* rational_operator *) str env tok
  | `Bits_op tok -> (* bitshift_operator *) str env tok
  | `Power_op tok -> (* power_operator *) str env tok
  | `Un_op tok -> (* unary_operator *) str env tok

let map_operator_exp (env : env) (x : CST.operator) : expr =
  let id = map_operator env x in
  N (H2.name_of_id id) |> G.e

let map_integer_literal (env : env) (x : CST.integer_literal) =
  let s, tok =
    match x with
    | `Tok_0b_pat_1c3450e x -> (* binary *) str env x
    | `Tok_0o_pat_c83427c x -> (* octal *) str env x
    | `Tok_0x_pat_50ed65e x -> (* hexadecimal *) str env x
    | `Pat_a25c544 x -> (* numeral *) str env x
  in
  L (Int (int_of_string_opt s, tok)) |> G.e

let map_float_literal (env : env) (x : CST.float_literal) : expr =
  (* Note that decimal exponents in Julia allow the use of "e" to separate
     the base from the exponent, but also the character "f".
     OCaml's `float_of_string` only allows "e", so we need to play around
     that in order to convert it properly.
  *)
  (* This indiscriminate mapping is safe to do, because we are guaranteed to not
     contain any other instance of "f" in a float literal which has a decimal exponent.
     This is because hex literals, which may contain A-F characters, use a different symbol
     to separate the exponent.
     So we need to just call this function in the decimal cases.
  *)
  let fix_string s =
    String.map
      (function
        | 'f' -> 'e'
        | other -> other)
      s
  in
  let s, tok =
    match x with
    | `Tok_dot_pat_a25c544_choice_pat_55159f5 x ->
        (* Leading period dec + exponent *)
        let s, tok = str env x in
        (fix_string s, tok)
    | `Pat_a25c544_imm_tok_dot_choice_pat_a25c544_choice_pat_55159f5 (v1, v2) ->
        (* Decimal base *)
        let s1, t1 = str env v1 in
        (* . and then more decimal, or exponent *)
        let s2, t2 = str env v2 in
        (fix_string (s1 ^ s2), Parse_info.combine_infos t1 [ t2 ])
    | `Tok_pat_a25c544_pat_55159f5 x ->
        (* Decimal and exponent *)
        let s, tok = str env x in
        (fix_string s, tok)
    | `Tok_choice_0x_pat_50ed65e_choice_dot_choice_pat_50ed65e_pat_dd04cb4 x ->
        (* 0x, with optional dot, and hex exponent *)
        (* This case is not safe to call `fix_string`! *)
        str env x
  in
  L (Float (float_of_string_opt s, tok)) |> G.e

let map_prefixed_command_literal (env : env)
    ((v1, v2, v3, v4, v5) : CST.prefixed_command_literal) : expr =
  (* This is some special thing. I can only reproduce it in the REPL, and I can't find any
     docs on it.
     It's something like if you write "x`foo`", and it tries to find a "@x_cmd".
  *)
  let v1 = map_identifier_exp env v1 in
  let ((s2, t2) as v2) = (* immediate_command_start *) str env v2 in
  let v3 = Common.map (map_anon_choice_str_content_no_interp_24ac4f9 env) v3 in
  let s4, t4 = (* command_end *) str env v4 in
  let cmd_id =
    ( s2 ^ String.concat "" (Common.map fst v3) ^ s4,
      Parse_info.combine_infos t2 (Common.map snd v3 @ [ t4 ]) )
  in
  let v5 =
    match v5 with
    | Some tok -> [ G.E (map_identifier_exp env tok) ]
    | None -> []
  in
  OtherExpr (v2, [ G.E v1; G.E (N (H2.name_of_id cmd_id) |> G.e) ] @ v5) |> G.e

let map_prefixed_string_literal (env : env)
    ((v1, v2, v3, v4, v5) : CST.prefixed_string_literal) =
  let v1 = map_identifier env v1 in
  let s2, t2 = (* immediate_string_start *) str env v2 in
  let v3 = Common.map (map_anon_choice_str_content_no_interp_24ac4f9 env) v3 in
  let s4, t4 = (* string_end *) str env v4 in
  let tok = Parse_info.combine_infos t2 (Common.map snd v3 @ [ t4 ]) in
  let s = s2 ^ String.concat "" (Common.map fst v3) ^ s4 in
  let v5 =
    match v5 with
    | Some tok -> [ G.I (map_identifier env tok) ]
    | None -> []
  in
  OtherExpr (("prefixed", fake "prefixed"), [ G.I v1; G.I (s, tok) ] @ v5)
  |> G.e

let map_anon_choice_id_267a5f7 (env : env) (x : CST.anon_choice_id_267a5f7) =
  match x with
  | `Id tok -> map_identifier env tok
  | `Op x -> map_operator env x

let rec map_adjoint_expression (env : env) ((v1, v2) : CST.adjoint_expression) =
  let v1 = map_expression env v1 in
  let id = str env v2 in
  Call (N (H2.name_of_id id) |> G.e, fb [ Arg v1 ]) |> G.e

and map_multi_assign ?(attrs = []) (env : env) x =
  (* THINK: EDynamic, versus Assign? Unclear.
   *)
  match x with
  | `Assign x ->
      let l_exp, _, r_exp = map_assignment env x in
      DefStmt
        ( { name = EDynamic l_exp; attrs; tparams = [] },
          VarDef { vinit = Some r_exp; vtype = None } )
      |> G.s
  | `Id tok ->
      let id = map_identifier env tok in
      DefStmt (basic_entity ~attrs id, VarDef { vinit = None; vtype = None })
      |> G.s
  | `Typed_exp x ->
      let l_exp, _, ty = map_typed_expression env x in
      DefStmt
        ( { name = EDynamic l_exp; attrs; tparams = [] },
          VarDef { vinit = None; vtype = Some ty } )
      |> G.s
  | `Bare_tuple x ->
      let e = map_bare_tuple_exp env x in
      DefStmt
        ( { name = EDynamic e; attrs; tparams = [] },
          VarDef { vinit = None; vtype = None } )
      |> G.s

and map_anon_choice_decl_f2ab0d0 (env : env) (x : CST.anon_choice_decl_f2ab0d0)
    : expr =
  match x with
  | `Decl x -> StmtExpr (map_declaration env x) |> G.e
  | `Exp x -> map_expression env x
  | `Assign x ->
      let l, t, r = map_assignment env x in
      Assign (l, t, r) |> G.e
  | `Short_func_defi x -> StmtExpr (map_short_function_definition env x) |> G.e

and map_type_parameter (env : env) (x : CST.anon_choice_exp_0381022) :
    G.type_parameter =
  match x with
  | `Exp x -> (
      (* For semantic purposes, we should do something better in this case in the future.
         But for now, let's just inject it into OtherTypeParam.
         There's not guaranteed to be any ID, is the issue, so...
      *)
      let exp = map_expression env x in
      match exp.e with
      | N (Id (id, _)) -> tparam_of_id id
      | __else__ -> OtherTypeParam (("tparam", fake "tparam"), [ G.E exp ]))
  | `Type_clause x ->
      (* This doesn't actually make sense to me as a standalone type parameter.
         This seems to imply you can have stuff like
         { T, <: Any }
         as opposed to
         { T, T2 <: Any }
         the standalone chevrons don't seem to mean anything...
      *)
      OtherTypeParam
        (("tparam", fake "tparam"), [ G.At (map_type_clause env x) ])
  | `Named_field x -> map_named_field_type_parameter env x

and map_anon_choice_exp_3c18676 (env : env) (x : CST.anon_choice_exp_3c18676) =
  match x with
  | `Exp x -> map_expression env x
  | `Assign x -> map_assignment_exp env x
  | `Bare_tuple x -> map_bare_tuple_exp env x

and map_anon_choice_exp_91c2553 (env : env) (x : CST.anon_choice_exp_91c2553) :
    argument =
  match x with
  | `Exp x -> Arg (map_expression env x)
  | `Named_field x -> (
      let either, _, exp = map_named_field env x in
      match either with
      | Left id -> ArgKwd (id, exp)
      | Right e1 -> OtherArg (("namedarg", fake "namedarg"), [ G.E e1; G.E exp ])
      )

and map_anon_choice_exp_91c2553_exp (env : env)
    (x : CST.anon_choice_exp_91c2553) : expr =
  match x with
  | `Exp x -> map_expression env x
  | `Named_field x ->
      let either, eq, exp = map_named_field env x in
      let e =
        match either with
        | Left id -> G.N (H2.name_of_id id) |> G.e
        | Right e -> e
      in
      Assign (e, eq, exp) |> G.e

and map_anon_choice_exp_b833738 (env : env) (x : CST.anon_choice_exp_b833738) =
  match x with
  | `Exp x -> map_expression env x
  | `Assign x -> map_assignment_exp env x

and map_anon_choice_exp_c681153 (env : env) (x : CST.anon_choice_exp_c681153) :
    stmt =
  match x with
  | `Exp x -> ExprStmt (map_expression env x, G.sc) |> G.s
  | `Decl x -> map_declaration env x
  | `Assign x ->
      (* TODO: Might be good to translate this to a `DefStmt` in the future.
         Python just lets it be an `Assign`, though, so we will too.
      *)
      ExprStmt (map_assignment_exp env x, G.sc) |> G.s
  | `Bare_tuple x -> ExprStmt (map_bare_tuple_exp env x, G.sc) |> G.s
  | `Short_func_defi x -> map_short_function_definition env x

and map_anon_choice_exp_c681153_arg (env : env)
    (x : CST.anon_choice_exp_c681153) : argument list =
  match x with
  | `Exp x -> [ Arg (map_expression env x) ]
  | `Decl x ->
      [ OtherArg (("decl", fake "decl"), [ G.S (map_declaration env x) ]) ]
  | `Assign x -> (
      let e1, tok, e2 = map_assignment env x in
      match e1.e with
      | N (Id (id, _)) -> [ ArgKwd (id, e2) ]
      | __else__ ->
          [
            OtherArg
              (("assign", fake "assign"), [ G.E (Assign (e1, tok, e2) |> G.e) ]);
          ])
  | `Bare_tuple x -> map_bare_tuple env x |> Common.map (fun x -> Arg x)
  | `Short_func_defi x ->
      (* who on earth is passing a short function def as an argument...
       *)
      let exp = map_short_function_definition env x in
      [
        OtherArg (("shortfunc", fake "shortfunc"), [ G.E (StmtExpr exp |> G.e) ]);
      ]

and map_anon_choice_exp_rep_COMMA_choice_exp_7e6cb67 (env : env)
    ((v1, v2) : CST.anon_choice_exp_rep_COMMA_choice_exp_7e6cb67) :
    argument list =
  let v1 = map_anon_choice_exp_91c2553 env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_exp_91c2553 env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_anon_choice_exp_rep_COMMA_choice_exp_7e6cb67_exp (env : env)
    ((v1, v2) : CST.anon_choice_exp_rep_COMMA_choice_exp_7e6cb67) : expr list =
  let v1 = map_anon_choice_exp_91c2553_exp env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_exp_91c2553_exp env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_anon_choice_for_clause_4e31839 (env : env)
    (x : CST.anon_choice_for_clause_4e31839) =
  match x with
  | `For_clause x -> map_for_clause env x
  | `If_clause (v1, v2) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = map_expression env v2 in
      [ CompIf (v1, v2) ]

and map_anon_choice_id_00cc266 (env : env) (x : CST.anon_choice_id_00cc266) =
  match x with
  | `Id tok -> Left (map_identifier env tok)
  | `Interp_exp x -> map_interpolation_expression_either env x

and map_anon_choice_id_00cc266_ent ?(attrs = []) ?(tparams = []) (env : env)
    (x : CST.anon_choice_id_00cc266) =
  match x with
  | `Id tok ->
      let id = map_identifier env tok in
      basic_entity ~attrs ~tparams id
  | `Interp_exp x -> (
      match map_interpolation_expression_either env x with
      | Left id -> { name = EN (H2.name_of_id id); attrs; tparams }
      | Right exp -> { name = EDynamic exp; attrs; tparams })

and map_anon_choice_id_a8b5d0d (env : env) (x : CST.anon_choice_id_a8b5d0d) :
    any =
  match x with
  | `Id tok -> G.I (map_identifier env tok)
  | `Macro_id x -> G.E (map_macro_identifier env x)
  | `Op x -> G.I (map_operator env x)
  | `LPAR_choice_id_RPAR (v1, v2, v3) ->
      let _v1 = (* "(" *) token env v1 in
      let v2 = map_anon_choice_id_267a5f7 env v2 in
      let _v3 = (* ")" *) token env v3 in
      G.I v2
  | `Interp_exp x -> G.E (map_interpolation_expression env x)

and map_anon_choice_id_6965274 (env : env) (x : CST.anon_choice_id_6965274) =
  match x with
  | `Id tok -> map_id_parameter env tok
  | `Slurp_param x -> map_slurp_parameter env x
  | `Typed_param x -> map_typed_parameter env x
  | `LPAR_choice_id_RPAR x -> map_paren_parameter env x
  | `Tuple_exp x -> ParamPattern (map_tuple_pat env x)

and map_id_parameter env (tok : CST.identifier) =
  let id = map_identifier env tok in
  Param (param_of_id id)

and map_paren_parameter env (l, param, r) =
  let _v1 = (* "(" *) token env l in
  let v2 = map_anon_choice_id_6314bc3 env param in
  let _v3 = (* ")" *) token env r in
  v2

and map_tuple_parameter env x = ParamPattern (map_tuple_pat env x)

and map_call_parameter env x =
  (* It is unclear to me why a parameter to a function ought to be a function call.
      According to the tree-sitter parser, this is for "Gen.jl".
  *)
  let exp = map_call_expression env x in
  OtherParam (("pattern", fake "pattern"), [ G.E exp ])

and map_closed_macro_parameter env x =
  let exp = map_closed_macrocall_expression env x in
  OtherParam (("pattern", fake "pattern"), [ G.E exp ])

and map_anon_choice_id_6314bc3 (env : env) (x : CST.anon_choice_id_6314bc3) =
  match x with
  | `Id tok -> map_id_parameter env tok
  | `Slurp_param x -> map_slurp_parameter env x
  | `Typed_param x -> map_typed_parameter env x

and map_anon_choice_id_687d935 (env : env) (x : CST.anon_choice_id_687d935) =
  match x with
  | `Id tok -> map_id_parameter env tok
  | `Slurp_param x -> map_slurp_parameter env x
  | `Opt_param x -> map_optional_parameter env x
  | `Typed_param x -> map_typed_parameter env x
  | `Interp_exp x -> map_interpolation_parameter env x
  | `Closed_macr_exp x ->
      let exp = map_closed_macrocall_expression env x in
      OtherParam (("pattern", fake "pattern"), [ G.E exp ])

and map_anon_choice_id_150150 (env : env) x =
  match x with
  | `Id tok -> map_id_parameter env tok
  | `Typed_param x -> map_typed_parameter env x
  | `Tuple_exp x -> ParamPattern (map_tuple_pat env x)

and map_anon_choice_id_c087cf9 (env : env) (x : CST.anon_choice_id_c087cf9) =
  match x with
  | `Id tok -> map_id_parameter env tok
  | `Slurp_param x -> map_slurp_parameter env x
  | `Opt_param x -> map_optional_parameter env x
  | `Typed_param x -> map_typed_parameter env x
  | `Tuple_exp x -> map_tuple_parameter env x
  | `Interp_exp x -> map_interpolation_parameter env x
  | `Closed_macr_exp x -> map_closed_macro_parameter env x
  | `Call_exp x -> map_call_parameter env x

and map_anon_choice_id_c313bb1 (env : env) (x : CST.anon_choice_id_c313bb1) =
  match x with
  | `Id tok ->
      let id = map_identifier env tok in
      let ent = basic_entity id in
      DefStmt (ent, VarDef { vinit = None; vtype = None }) |> G.s
  | `Named_field x ->
      let either, _tok, exp = map_named_field env x in
      let ent =
        match either with
        | Left id -> basic_entity id
        | Right e -> { name = EDynamic e; attrs = []; tparams = [] }
      in
      DefStmt (ent, VarDef { vinit = Some exp; vtype = None }) |> G.s

and map_anon_choice_id_f1f5a37 (env : env) (x : CST.anon_choice_id_f1f5a37) =
  match x with
  | `Id tok -> ([], Left (map_identifier env tok))
  | `Scoped_id x -> map_scoped_identifier_rev env x

and map_scoped_identifier_closed (env : env) (x : CST.scoped_identifier) =
  (* This is for the purposes of an import.
     While imported paths allow metaprogramming, paths like
     a.b.$foo
     this doesn't actually really make sense when fitting the generic AST.

     So we'll just refuse to parse such things, for now, and return an option.

     The acc is in the form:

     xn x(n-1) ... x2

     and the base is x1
  *)
  let acc, base = map_scoped_identifier_rev env x in
  match base with
  | Left id ->
      let ( >>| ) x y = Option.map y x in
      (* xn x(n-1) ... x2 *)
      acc
      |> Common.map (fun (_, x) ->
             match x with
             | Left x -> Some x
             | __else__ -> None)
      |> option_all
      (* xn x(n-1) ... x2 but now it's only idents*)
      >>| List.rev
      (* x2 x3 ... xn *)
      (* then add on the x1 *)
      >>|
      fun l -> id :: l
  | Right _ -> None

and map_scoped_identifier_exp (env : env) ((v1, v2, v3) : CST.scoped_identifier)
    =
  let acc, base = map_scoped_identifier_rev env (v1, v2, v3) in
  let base =
    match base with
    | Left id -> N (H2.name_of_id id) |> G.e
    | Right exp -> exp
  in
  (* given xn xn-1 ... x2
     we do a right fold starting with x1, adding everything to the end
  *)
  List.fold_right
    (fun (tok, field) acc ->
      match field with
      | Left id -> DotAccess (acc, tok, FN (H2.name_of_id id)) |> G.e
      | Right exp -> DotAccess (acc, tok, FDynamic exp) |> G.e)
    acc base

and map_anon_choice_id_f1f5a37_closed (env : env)
    (x : CST.anon_choice_id_f1f5a37) =
  (* This is exactly the same code as above.
     All these functions being mutually recursive means that the above function can't
     polymorphically generalize, so we have to duplicate the code...
  *)
  let acc, base = map_anon_choice_id_f1f5a37 env x in
  match base with
  | Left id ->
      let ( >>| ) x y = Option.map y x in
      (* xn x(n-1) ... x2 *)
      acc
      |> Common.map (fun (_, x) ->
             match x with
             | Left x -> Some x
             | __else__ -> None)
      |> option_all
      (* xn x(n-1) ... x2 but now it's only idents*)
      >>| List.rev
      (* x2 x3 ... xn *)
      (* then add on the x1 *)
      >>|
      fun l -> id :: l
  | Right _ -> None

and map_import_subject (env : env) (x : CST.anon_choice_impo_3d80307) :
    (ident * alias option) option =
  (* import x.y.z: *
     Notably, our `ImportFrom` construct only allows aliases which are a single
     identifier. So we will reject the first case, here.
  *)
  match x with
  | `Impo x -> (
      (* We cannot import as a scoped identifier. *)
      match map_importable env x with
      | Some [ id ] -> Some (id, None)
      | __else__ -> None)
  | `Import_alias x -> map_import_alias env x
  | `Macro_id _x -> None
  | `Op x -> Some (map_operator env x, None)

and map_anon_choice_impo_a542259 ~import_tok (env : env)
    (x : CST.anon_choice_impo_a542259) =
  match x with
  | `Impo x ->
      let* dotted = map_importable env x in
      Some (ImportAs (import_tok, DottedName dotted, None))
  | `Import_alias x ->
      let* id, alias_opt = map_import_alias env x in
      Some (ImportAs (import_tok, DottedName [ id ], alias_opt))

and map_anon_choice_str_content_838a78d (env : env)
    (x : CST.anon_choice_str_content_838a78d) =
  match x with
  | `Str_content tok -> (* string_content *) Left3 (str env tok)
  | `Str_interp (v1, v2) ->
      let dollar_s, dollar_t = (* "$" *) str env v1 in
      let v2 =
        match v2 with
        | `Id tok -> (
            let s, t = map_identifier env tok in
            match env.extra with
            | Pattern ->
                let mvar_id = (dollar_s ^ s, PI.combine_infos dollar_t [ t ]) in
                Left3 mvar_id
            | Program -> Middle3 (N (H2.name_of_id (s, t)) |> G.e))
        | `Imme_paren_LPAR_choice_exp_RPAR (v1, v2, v3, v4) ->
            let _v1 = (* immediate_paren *) token env v1 in
            let v2 = (* "(" *) token env v2 in
            let v3 = map_anon_choice_exp_91c2553_exp env v3 in
            let v4 = (* ")" *) token env v4 in
            Right3 (v2, Some v3, v4)
      in
      v2
  | `Esc_seq tok -> (* escape_sequence *) Left3 (str env tok)

and mk_comprehension (v1, v2) = Comprehension (List, fb (v1, v2)) |> G.e

and map_argument_list (env : env) ((v1, v2, v3, v4, v5) : CST.argument_list) :
    argument list bracket =
  let l = (* "(" *) token env v1 in
  let args =
    match v2 with
    | Some x -> (
        match x with
        | `Choice_exp_rep_COMMA_choice_exp_opt_COMMA_opt_exp_comp_clause
            (v1, v2, v3) ->
            let v1 = map_anon_choice_exp_91c2553 env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_anon_choice_exp_91c2553 env v2 in
                  v2)
                v2
            in
            let v3 =
              match v3 with
              | Some (v1, v2) -> (
                  let _v1 = (* "," *) token env v1 in
                  match v2 with
                  | Some (v1, v2) ->
                      let v1 = map_expression env v1 in
                      let v2 = map_comprehension_clause env v2 in
                      [ Arg (mk_comprehension (v1, v2)) ]
                  | None -> [])
              | None -> []
            in
            (v1 :: v2) @ v3
        | `Exp_comp_clause (v1, v2) ->
            let v1 = map_expression env v1 in
            let v2 = map_comprehension_clause env v2 in
            [ Arg (Comprehension (List, fb (v1, v2)) |> G.e) ])
    | None -> []
  in
  let kwdargs =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = (* ";" *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> map_anon_choice_exp_rep_COMMA_choice_exp_7e6cb67 env x
          | None -> []
        in
        v2
    | None -> []
  in
  let _v4 = map_trailing_comma env v4 in
  let r = (* ")" *) token env v5 in
  (l, args @ kwdargs, r)

and map_assignment (env : env) ((v1, v2, v3) : CST.assignment) :
    expr * tok * expr =
  let v1 =
    match v1 with
    | `Quot x -> map_quotable env x
    | `Field_exp x -> map_field_expression env x
    | `Index_exp x -> map_index_expression env x
    | `Para_type_exp x -> map_parametrized_type_expression env x
    | `Interp_exp x -> map_interpolation_expression env x
    | `Quote_exp x -> map_quote_expression env x
    | `Typed_exp x -> map_typed_expression_exp env x
    | `Op x -> map_operator_exp env x
    | `Pref_cmd_lit x -> map_prefixed_command_literal env x
    | `Pref_str_lit x -> map_prefixed_string_literal env x
    | `Bin_exp x -> map_binary_expression env x
    | `Un_exp x -> map_unary_expression env x
    | `Bare_tuple x -> map_bare_tuple_exp env x
  in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_anon_choice_exp_3c18676 env v3 in
  (v1, v2, v3)

and map_assignment_exp (env : env) ((v1, v2, v3) : CST.assignment) : expr =
  let l_exp, tok, r_exp = map_assignment env (v1, v2, v3) in
  Assign (l_exp, tok, r_exp) |> G.e

and map_bare_tuple (env : env) ((v1, v2) : CST.bare_tuple) : expr list =
  let v1 = map_expression env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_bare_tuple_exp (env : env) ((v1, v2) : CST.bare_tuple) : expr =
  let xs = map_bare_tuple env (v1, v2) in
  Container (Tuple, fb xs) |> G.e

and map_binary_expression (env : env) (x : CST.binary_expression) : expr =
  let mk_binary id arg1 arg2 =
    Call (N (H2.name_of_id id) |> G.e, fb [ Arg arg1; Arg arg2 ]) |> G.e
  in
  match x with
  | `Exp_power_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* power_operator *) token env v2 in
      let v3 = map_expression env v3 in
      opcall (Pow, v2) [ v1; v3 ]
  | `Exp_rati_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* rational_operator *) str env v2 in
      let v3 = map_expression env v3 in
      mk_binary v2 v1 v3
  | `Exp_bits_op_exp (v1, v2, v3) -> (
      let v1 = map_expression env v1 in
      let v2 = (* bitshift_operator *) token env v2 in
      let v3 = map_expression env v3 in
      match PI.str_of_info v2 with
      | "<<" -> opcall (LSL, v2) [ v1; v3 ]
      | ">>" -> opcall (ASR, v2) [ v1; v3 ]
      | ">>>" -> opcall (LSR, v2) [ v1; v3 ]
      | s ->
          Call (N (H2.name_of_id (s, v2)) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e)
  | `Exp_times_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* times_operator *) token env v2 in
      let v3 = map_expression env v3 in
      opcall (Mult, v2) [ v1; v3 ]
  | `Exp_choice_tok_choice_dot_choice_plus_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `Tok_choice_dot_choice_plus x -> map_tok_choice_dot_choice_plus env x
        | `Plus_op tok -> (* plus_operator *) token env tok
      in
      let v3 = map_expression env v3 in
      opcall (Plus, v2) [ v1; v3 ]
  | `Exp_ellips_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ellipsis_operator *) str env v2 in
      let v3 = map_expression env v3 in
      (* This is not strictly a part of the Julia language, but it is used in some
         packages.
         It's just a part of the grammar, so we need to deal with it.
      *)
      mk_binary v2 v1 v3
  | `Exp_arrow_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* arrow_operator *) str env v2 in
      let v3 = map_expression env v3 in
      mk_binary v2 v1 v3
  | `Exp_pipe_left_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* pipe_left_operator *) str env v2 in
      let v3 = map_expression env v3 in
      mk_binary v2 v1 v3
  | `Exp_pipe_right_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* pipe_right_operator *) str env v2 in
      let v3 = map_expression env v3 in
      mk_binary v2 v1 v3
  | `Exp_choice_in_exp (v1, v2, v3) -> (
      let v1 = map_expression env v1 in
      let v3 = map_expression env v3 in
      match v2 with
      | `In tok ->
          (* "in" *)
          opcall (In, token env tok) [ v1; v3 ]
      | `Isa tok ->
          (* "isa" *)
          opcall (Is, token env tok) [ v1; v3 ]
      | `Comp_op tok -> (
          (* comparison_operator *)
          let s, tok = str env tok in
          (* There are actually like 3 billion Julia comparison operators and I don't have time to
              support all of them.
              Here's some of them.
          *)
          match s with
          | ">" -> opcall (Gt, tok) [ v1; v3 ]
          | "<" -> opcall (Lt, tok) [ v1; v3 ]
          | ">="
          | "≥" ->
              opcall (LtE, tok) [ v1; v3 ]
          | "<="
          | "≤" ->
              opcall (LtE, tok) [ v1; v3 ]
          | "==" -> opcall (Eq, tok) [ v1; v3 ]
          | "===" -> opcall (PhysEq, tok) [ v1; v3 ]
          | "!=" -> opcall (NotEq, tok) [ v1; v3 ]
          | "!==" -> opcall (NotPhysEq, tok) [ v1; v3 ]
          | "∈" -> opcall (In, tok) [ v1; v3 ]
          | "∉" -> opcall (NotIn, tok) [ v1; v3 ]
          | _ ->
              Call (N (H2.name_of_id (s, tok)) |> G.e, fb [ Arg v1; Arg v3 ])
              |> G.e))
  | `Exp_tok_choice_dot_choice_barbar_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_tok_choice_dot_choice_barbar env v2 in
      let v3 = map_expression env v3 in
      opcall (Or, v2) [ v1; v3 ]
  | `Exp_tok_choice_dot_choice_ampamp_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_tok_choice_dot_choice_ampamp env v2 in
      let v3 = map_expression env v3 in
      opcall (And, v2) [ v1; v3 ]
  | `Exp_pair_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* pair_operator *) str env v2 in
      let v3 = map_expression env v3 in
      OtherExpr (v2, [ E v1; E v3 ]) |> G.e

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = map_anon_choice_exp_c681153 env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = map_terminator env v1 in
        let v2 = map_anon_choice_exp_c681153 env v2 in
        v2)
      v2
  in
  let _v3 = map_terminator_opt env v3 in
  v1 :: v2

and map_call_expression (env : env) ((v1, v2, v3, v4) : CST.call_expression) :
    expr =
  let v1 =
    match v1 with
    | `Prim_exp x -> map_primary_expression env x
    | `Op x ->
        let op = map_operator env x in
        N (H2.name_of_id op) |> G.e
  in
  let _v2 = (* immediate_paren *) token env v2 in
  let l, args, r = map_argument_list env v3 in
  (* A "do" adds an implicit first parameter to the call.
     See https://docs.julialang.org/en/v1/manual/functions/#Do-Block-Syntax-for-Function-Arguments-1
  *)
  let args =
    match v4 with
    | Some x -> Arg (map_do_clause env x) :: args
    | None -> args
  in
  Call (v1, (l, args, r)) |> G.e

and map_catch_clause (env : env) ((v1, v2, v3, v4) : CST.catch_clause) : catch =
  let v1 = (* "catch" *) token env v1 in
  let catch =
    match v2 with
    | Some tok ->
        let id = map_identifier env tok in
        CatchPattern (PatId (id, empty_id_info ()))
    | None -> CatchPattern (PatUnderscore (fake "underscore"))
  in
  let _v3 = map_terminator_opt env v3 in
  let v4 = map_source_file_stmt env v4 in
  (v1, catch, v4)

and map_closed_macrocall_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.closed_macrocall_expression) : expr =
  let v2 = map_macro_identifier env v2 in
  let _v3 = (* immediate_paren *) token env v3 in
  let l, args, r = map_argument_list env v4 in
  let args =
    match v5 with
    | Some x -> Arg (map_do_clause env x) :: args
    | None -> args
  in
  let base = Call (v2, (l, args, r)) |> G.e in
  match v1 with
  | Some (v1, v2) ->
      (* It's unclear to me whether this has anything to do with macros.
         I can't seem to find anything about it in the Julia docs.
         It seems more likely to me that this was intended to appease the parser,
         and that this is really just a special case of "scoped_identifier" that couldn't fit
         for shift-reduce reasons.
         So I choose to interpret it as the `DotAccesss`, where the field name happens to be a
         macro argument.
      *)
      let v1 = map_primary_expression env v1 in
      let v2 = map_imm_tok_dot env v2 in
      DotAccess (v1, v2, FDynamic base) |> G.e
  | None -> base

and map_command_literal (env : env) ((v1, v2, v3) : CST.command_literal) =
  let s, v1 = (* command_start *) str env v1 in
  let v2 = Common.map (map_anon_choice_str_content_838a78d env) v2 in
  let v3 = (* command_end *) token env v3 in
  OtherExpr ((s, v1), [ G.E (G.interpolated (v1, v2, v3)) ]) |> G.e

and map_comprehension_clause (env : env)
    ((v1, v2, v3, v4) : CST.comprehension_clause) =
  let _v1 = map_for_clause env v1 in
  let _v2 = map_newline_opt env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_for_clause_4e31839 env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = map_newline_opt env v1 in
              let v2 = map_anon_choice_for_clause_4e31839 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v4 = map_newline_opt env v4 in
  Common.flatten v3

and map_comprehension_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.comprehension_expression) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_anon_choice_exp_b833738 env v2 in
  let _v3 = map_terminator_opt env v3 in
  let v4 = map_comprehension_clause env v4 in
  let v5 = (* "]" *) token env v5 in
  Comprehension (Array, (v1, (v2, v4), v5)) |> G.e

and map_type_parameter_list (env : env)
    ((v1, v2, v3, v4) : CST.curly_expression) : type_parameter list =
  let _v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_type_parameter env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_type_parameter env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 =
    match v3 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  let _v4 = (* "}" *) token env v4 in
  v2

and map_declaration (env : env) (x : CST.declaration) : stmt =
  match x with
  | `Const_decl (v1, v2) -> (
      let v1 = (* "const" *) token env v1 in
      let attrs = [ KeywordAttr (Const, v1) ] in
      match v2 with
      | `Assign x ->
          let l_exp, _, r_exp = map_assignment env x in
          DefStmt
            ( { name = EDynamic l_exp; attrs; tparams = [] },
              VarDef { vinit = Some r_exp; vtype = None } )
          |> G.s
      | `Id tok ->
          let id = map_identifier env tok in
          DefStmt (basic_entity ~attrs id, VarDef { vinit = None; vtype = None })
          |> G.s
      | `Typed_exp x ->
          let l_exp, _, ty = map_typed_expression env x in
          DefStmt
            ( { name = EDynamic l_exp; attrs; tparams = [] },
              VarDef { vinit = None; vtype = Some ty } )
          |> G.s)
  | `Local_decl (v1, v2) ->
      let v1 = (* "local" *) str env v1 in
      map_multi_assign ~attrs:[ OtherAttribute (v1, []) ] env v2
  | `Global_decl (v1, v2) ->
      let v1 = (* "global" *) str env v1 in
      map_multi_assign ~attrs:[ OtherAttribute (v1, []) ] env v2

and map_definition (env : env) (x : CST.definition) : stmt =
  match x with
  | `Module_defi (v1, v2, v3, v4, v5) ->
      let _v1 =
        match v1 with
        | `Module tok -> (* "module" *) token env tok
        | `Bare tok -> (* "baremodule" *) token env tok
      in
      let ent = map_anon_choice_id_00cc266_ent env v2 in
      let _v3 = map_terminator_opt env v3 in
      let v4 = map_source_file env v4 in
      let _v5 = (* "end" *) token env v5 in
      DefStmt (ent, ModuleDef { mbody = ModuleStruct (None, v4) }) |> G.s
  | `Abst_defi (v1, v2, v3, v4, v5) ->
      (* abstract type *)
      let v1 = map_tok_abst_pat_3d340f6_type env v1 in
      let tparams =
        match v3 with
        | None -> []
        | Some (_, x) -> map_type_parameter_list env x
      in
      let attrs = map_type_clause_opt env v4 in
      let ent = map_anon_choice_id_00cc266_ent ~attrs ~tparams env v2 in
      let _v5 = (* "end" *) token env v5 in
      DefStmt (ent, TypeDef { tbody = AbstractType v1 }) |> G.s
  | `Prim_defi (v1, v2, v3, v4, v5, v6) ->
      (* primitive type *)
      let tparams =
        match v3 with
        | None -> []
        | Some (_, x) -> map_type_parameter_list env x
      in
      let attrs = map_type_clause_opt env v4 in
      let ent = map_anon_choice_id_00cc266_ent ~attrs ~tparams env v2 in
      let i = map_integer_literal env v5 in
      let _v6 = (* "end" *) token env v6 in
      DefStmt (ent, TypeDef { tbody = OtherTypeKind (str env v1, [ G.E i ]) })
      |> G.s
  | `Struct_defi (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        match v1 with
        | Some tok -> (* "mutable" *) [ KeywordAttr (Mutable, token env tok) ]
        | None -> []
      in
      let v2 = (* "struct" *) token env v2 in
      let tparams =
        match v4 with
        | None -> []
        | Some (_, x) -> map_type_parameter_list env x
      in
      let attrs = v1 @ map_type_clause_opt env v5 in
      let _v6 = map_terminator_opt env v6 in
      let v7 = map_source_file env v7 in
      let v8 = (* "end" *) token env v8 in
      let ent = map_anon_choice_id_00cc266_ent ~attrs ~tparams env v3 in
      DefStmt
        (ent, TypeDef { tbody = AndType (v2, Common.map (fun x -> F x) v7, v8) })
      |> G.s
  | `Func_defi (v1, v2, v3) -> (
      let func_tok = (* "function" *) token env v1 in
      let _v3 = (* "end" *) token env v3 in
      match v2 with
      | `Choice_func_sign_opt_choice_LF_opt_blk (v1, v2, v3) -> (
          let _v2 = map_terminator_opt env v2 in
          let v3 = map_source_file_stmt env v3 in
          match v1 with
          | `Func_sign x ->
              let ent, fun_def =
                map_function_signature ~body:(FBStmt v3)
                  ~func_tok:(Some func_tok) env x
              in
              DefStmt (ent, FuncDef fun_def) |> G.s
          | `Param_list_rep_where_clause (v1, v2) ->
              let fparams = map_parameter_list env v1 in
              let v2 = Common.map (map_where_clause env) v2 in
              (* I don't really have anywhere else to put this "where" in a lambda, so let's just
                  add it as a return type (which otherwise is not specified).
                  Note that this doesn't actually make sense. The "where" is a restriction on
                  the types which may appear in the parameters, for instance:
                  function (x :: T, y :: T2) where T <: Int64 where T2 <: String return 1 end
              *)
              let frettype =
                match v2 with
                | [] -> None
                | __else__ ->
                    Some
                      (OtherType
                         ( ("where", fake "where"),
                           v2 |> Common.map (fun x -> G.At x) )
                      |> G.t)
              in
              ExprStmt
                ( Lambda
                    {
                      fkind = (LambdaKind, func_tok);
                      fparams;
                      frettype;
                      fbody = FBStmt v3;
                    }
                  |> G.e,
                  G.sc )
              |> G.s)
      | `Choice_id x ->
          (* I have no idea what this means.
             This allows things like:

             function f
             end

             Kinda useless.
          *)
          let id = map_anon_choice_id_267a5f7 env x in
          let ent = basic_entity id in
          DefStmt
            ( ent,
              FuncDef
                {
                  fkind = (Function, func_tok);
                  fparams = fb [];
                  frettype = None;
                  fbody = FBNothing;
                } )
          |> G.s)
  | `Macro_defi (v1, v2, v3, v4, v5, v6, v7) -> (
      let _v1 = (* "macro" *) token env v1 in
      let ent =
        match v2 with
        | `Id tok -> basic_entity (map_identifier env tok)
        | `Op x -> basic_entity (map_operator env x)
        | `Interp_exp x -> (
            match map_interpolation_expression_either env x with
            | Left id -> basic_entity id
            | Right exp ->
                (* What kind of a sick, twisted psychopath would make the name of a macro
                   the result of a run-time value????
                *)
                { name = EDynamic exp; attrs = []; tparams = [] })
      in
      let _v3 = (* immediate_paren *) token env v3 in
      let _, v4, _ = map_parameter_list env v4 in
      let macroparams =
        Common.map
          (function
            | Param { pname = Some id; _ } -> Some id
            (* TODO: Accommodate this, macros currently just can't take in arguments which are not idents...
         *)
            | _ -> None)
          v4
      in
      let _v5 = map_terminator_opt env v5 in
      let v6 = map_source_file env v6 in
      let _v7 = (* "end" *) token env v7 in
      match option_all macroparams with
      | None ->
          (* In this case, just don't inject it into MacroDef.
           *)
          DefStmt
            ( ent,
              OtherDef
                ( ("macro", fake "macro"),
                  Common.map (fun x -> G.Pa x) v4 @ [ G.Ss v6 ] ) )
          |> G.s
      | Some macroparams ->
          DefStmt
            ( ent,
              MacroDef
                { macroparams; macrobody = Common.map (fun x -> G.S x) v6 } )
          |> G.s)

and map_do_clause (env : env) ((v1, v2, v3, v4) : CST.do_clause) =
  let v1 = (* "do" *) token env v1 in
  let fparams = map_do_parameter_list env v2 in
  let body = map_source_file_stmt env v3 in
  let _v4 = (* "end" *) token env v4 in
  Lambda
    {
      fkind = (LambdaKind, v1);
      fparams = fb fparams;
      frettype = None;
      fbody = FBStmt body;
    }
  |> G.e

and map_do_parameter_list (env : env) ((v1, v2) : CST.do_parameter_list) :
    parameter list =
  let v1 =
    match v1 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_id_6965274 env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_anon_choice_id_6965274 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v2 = map_terminator env v2 in
  v1

and map_else_clause (env : env) ((v1, v2, v3) : CST.else_clause) =
  let _v1 = (* "else" *) token env v1 in
  let _v2 = map_terminator_opt env v2 in
  let v3 = map_source_file_stmt env v3 in
  v3

and map_elseif_clause (env : env) ((v1, v2, v3, v4) : CST.elseif_clause) =
  let v1 = (* "elseif" *) token env v1 in
  let v2 = map_expression env v2 in
  let _v3 = map_terminator_opt env v3 in
  let v4 = map_source_file_stmt env v4 in
  fun stmt_opt -> If (v1, Cond v2, v4, stmt_opt) |> G.s

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Semg_ellips tok -> Ellipsis (token env tok) |> G.e
  | `Choice_choice_module_defi x -> (
      match x with
      | `Choice_module_defi x -> (
          let stmt = map_definition env x in
          match stmt.s with
          | ExprStmt (expr, _) -> expr
          | __else__ -> StmtExpr stmt |> G.e)
      | `Choice_choice_comp_stmt x -> (
          let stmt = map_statement env x in
          match stmt with
          | [ { s = ExprStmt (expr, _); _ } ] -> expr
          | __else__ -> StmtExpr (Block (fb stmt) |> G.s) |> G.e)
      | `Lit x -> map_literal env x
      | `Prim_exp x -> map_primary_expression env x
      | `Macr_exp (v1, v2, v3) -> (
          (* As mentioned elsewhere, I believe this to be a parser shenanigan and not
             actually indicating that this is part of the macro.
             I believe that this just indicates that the macro is the field to a DotAccess.
          *)
          let v2 = map_macro_identifier env v2 in
          let args =
            match v3 with
            | Some x -> map_macro_argument_list env x
            | None -> []
          in
          let base = Call (v2, fb args) |> G.e in
          match v1 with
          | Some (v1, v2) ->
              let v1 = map_primary_expression env v1 in
              let v2 = map_imm_tok_dot env v2 in
              (* TODO: simplify the `FDynamic` in the case of an Id? *)
              DotAccess (v1, v2, FDynamic base) |> G.e
          | None -> base)
      | `Adjo_exp x -> map_adjoint_expression env x
      | `Un_exp x -> map_unary_expression env x
      | `Bin_exp x -> map_binary_expression env x
      | `Range_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = map_imm_tok_colon env v2 in
          let v3 = map_expression env v3 in
          opcall (Range, v2) [ v1; v3 ]
      | `Splat_exp (v1, v2) ->
          let v1 = map_expression env v1 in
          let tok = (* "..." *) token env v2 in
          special (Spread, tok) [ v1 ]
      | `Tern_exp (v1, v2, v3, v4, v5) ->
          let v1 = map_expression env v1 in
          let _v2 = (* "?" *) token env v2 in
          let v3 = map_anon_choice_exp_b833738 env v3 in
          let _v4 = (* ":" *) token env v4 in
          let v5 = map_anon_choice_exp_b833738 env v5 in
          Conditional (v1, v3, v5) |> G.e
      | `Typed_exp x -> map_typed_expression_exp env x
      | `Func_exp (v1, v2, v3) ->
          let fparams =
            match v1 with
            | `Id tok -> fb [ map_id_parameter env tok ]
            | `Param_list x -> map_parameter_list env x
            | `Typed_exp x ->
                let exp, _tok, ty = map_typed_expression env x in
                fb [ OtherParam (("typed", fake "typed"), [ G.E exp; G.T ty ]) ]
          in
          let _v2 = (* "->" *) token env v2 in
          let v3 = map_anon_choice_exp_3c18676 env v3 in
          Lambda
            {
              fkind = (LambdaKind, fake "lambda");
              fparams;
              frettype = None;
              fbody = FBExpr v3;
            }
          |> G.e
      | `Juxt_exp (v1, v2) ->
          (* As far as I can tell, this allows literals like "2x" to be syntactic sugar for
             2 * x.
          *)
          let v1 =
            match v1 with
            | `Int_lit x -> map_integer_literal env x
            | `Float_lit x -> map_float_literal env x
            | `Adjo_exp x -> map_adjoint_expression env x
          in
          let v2 = map_primary_expression env v2 in
          opcall (Mult, fake "") [ v1; v2 ]
      | `Comp_assign_exp (v1, v2, v3) ->
          let v1 = map_primary_expression env v1 in
          let v3 = map_expression env v3 in
          map_assign_operator env v2 v1 v3
      | `Where_exp (v1, v2, v3) ->
          let v1 = map_expression env v1 in
          let v2 = (* "where" *) str env v2 in
          let v3 = map_expression env v3 in
          OtherExpr (v2, [ G.E v1; G.E v3 ]) |> G.e
      | `Op x -> map_operator_exp env x
      | `COLON tok ->
          (* For creating "symbols" for metaprogramming. *)
          let t = (* ":" *) str env tok in
          OtherExpr (t, []) |> G.e
      | `Begin tok ->
          (* "begin" *)
          (* what even is this I don't understand why why why *)
          OtherExpr (str env tok, []) |> G.e)

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) =
  let v1 = map_primary_expression env v1 in
  let v2 = map_imm_tok_dot env v2 in
  let field =
    match v3 with
    | `Id tok -> FN (H2.name_of_id (map_identifier env tok))
    | `Interp_exp x -> (
        match map_interpolation_expression_either env x with
        | Left id -> FN (H2.name_of_id id)
        | Right exp -> FDynamic exp)
    | `Quote_exp x -> FDynamic (map_quote_expression env x)
    | `Cmd_lit x -> FDynamic (map_command_literal env x)
    | `Str_lit x -> FDynamic (map_string_literal env x)
    | `Pref_cmd_lit x -> FDynamic (map_prefixed_command_literal env x)
    | `Pref_str_lit x -> FDynamic (map_prefixed_string_literal env x)
  in
  DotAccess (v1, v2, field) |> G.e

and map_finally_clause (env : env) ((v1, v2, v3) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let _v2 = map_terminator_opt env v2 in
  let v3 = map_source_file env v3 in
  (v1, Block (fb v3) |> G.s)

and map_for_binding (env : env) ((v1, v2, v3) : CST.for_binding) =
  let v1 =
    match v1 with
    | `Id tok ->
        let id = map_identifier env tok in
        PatId (id, empty_id_info ())
    (* TODO: Make these proper patterns.
       Perhaps there should be an `exp_to_pat`, so this doesn't have to be duplicated...
    *)
    | `Tuple_exp x -> map_tuple_pat env x
    | `Typed_param x ->
        let param = map_typed_parameter env x in
        OtherPat (("pat", fake "pat"), [ G.Pa param ])
    | `Interp_exp x -> (
        match map_interpolation_expression_either env x with
        | Left id -> PatId (id, empty_id_info ())
        | Right exp -> OtherPat (("pat", fake "pat"), [ G.E exp ]))
  in
  let v2 =
    match v2 with
    | `In tok -> (* "in" *) token env tok
    | `EQ tok -> (* "=" *) token env tok
    | `UNKUNKUNK tok -> (* "\226\136\136" *) token env tok
  in
  let v3 = map_expression env v3 in
  (v1, v2, v3)

and map_for_clause (env : env) ((v1, v2, v3) : CST.for_clause) =
  let v1 = (* "for" *) token env v1 in
  let v2 = map_for_binding env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_for_binding env v2 in
        v2)
      v3
  in
  Common.map (fun (x, y, z) -> CompFor (v1, x, y, z)) (v2 :: v3)

and map_function_signature ~body ~func_tok (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.function_signature) =
  let ent =
    let attrs = Common.map (map_where_clause env) v6 in
    let tparams =
      match v2 with
      | Some (v1, v2) ->
          let _v1 = (* immediate_brace *) token env v1 in
          let v2 = map_type_parameter_list env v2 in
          v2
      | None -> []
    in
    match v1 with
    | `Id tok ->
        let id = map_identifier env tok in
        basic_entity ~tparams ~attrs id
    | `Op x ->
        let id = map_operator env x in
        basic_entity ~tparams ~attrs id
    | `LPAR_choice_id_RPAR (v1, v2, v3) ->
        let _v1 = (* "(" *) token env v1 in
        let id = map_anon_choice_id_267a5f7 env v2 in
        let _v3 = (* ")" *) token env v3 in
        basic_entity ~tparams ~attrs id
    | `Field_exp x ->
        { name = EDynamic (map_field_expression env x); attrs; tparams }
    | `LPAR_typed_param_RPAR (v1, v2, v3) ->
        (* This is really weird and probably like, a lambda.
           As far as I can tell functions that fit this case should look like:

           function (f :: String)(x :: Int64) return 1 end

           Which is really an anonymous function taking in `f`, with two arguments.

           What you can do with this is attach a method to a particular type, as in this example:
           struct Name
             x
           end
           (D::Name)(x) = 2)

           then

           Name(5)(3) will return 2
        *)
        let _v1 = (* "(" *) token env v1 in
        let param = map_typed_parameter env v2 in
        let _v3 = (* ")" *) token env v3 in
        {
          name = OtherEntity (("anonymous", fake "anonymous"), [ G.Pa param ]);
          attrs;
          tparams;
        }
    | `Interp_exp x -> (
        match map_interpolation_expression_either env x with
        | Left id -> basic_entity ~attrs ~tparams id
        | Right exp -> { name = EDynamic exp; attrs; tparams })
  in
  let func_tok =
    match func_tok with
    | None -> fake "function"
    | Some tok -> tok
  in
  let _v3 = (* immediate_paren *) token env v3 in
  let fparams = map_parameter_list env v4 in
  let frettype =
    match v5 with
    | Some (v1, v2) ->
        let _v1 = (* "::" *) token env v1 in
        let v2 = map_type env v2 in
        Some v2
    | None -> None
  in
  (ent, { fkind = (Function, func_tok); fparams; frettype; fbody = body })

and map_import_alias (env : env) ((v1, v2, v3) : CST.import_alias) :
    (ident * alias option) option =
  let* v1 = map_importable env v1 in
  let _v2 = (* "as" *) token env v2 in
  let v3 = map_identifier env v3 in
  match v1 with
  | [ id ] -> Some (id, Some (v3, empty_id_info ()))
  | __else__ -> None

and map_import_list ~import_tok (env : env) ((v1, v2) : CST.import_list) =
  let v1 = map_anon_choice_impo_a542259 ~import_tok env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_impo_a542259 ~import_tok env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_importable (env : env) (x : CST.importable) : ident list option =
  match x with
  | `Id tok -> Some [ map_identifier env tok ]
  | `Scoped_id x ->
      let* dotted = map_scoped_identifier_closed env x in
      Some dotted
  | `Rela_qual (v1, v2) ->
      let v1 = Common.map (str env (* "." *)) v1 in
      let* v2 = map_anon_choice_id_f1f5a37_closed env v2 in
      Some (v1 @ v2)
  | `LPAR_choice_id_RPAR (v1, v2, v3) ->
      let _v1 = (* "(" *) token env v1 in
      let v2 = map_anon_choice_id_267a5f7 env v2 in
      let _v3 = (* ")" *) token env v3 in
      Some [ v2 ]
  | `Interp_exp _x ->
      (* TODO: AST_generic can't fit an arbitrary expression in an import right now
         We will just discard the entire import in that case, but continue.
      *)
      None

and map_index_expression (env : env) ((v1, v2, v3) : CST.index_expression) =
  let v1 = map_primary_expression env v1 in
  let v2 = (* immediate_bracket *) token env v2 in
  let v3 =
    match v3 with
    | `Comp_exp x -> map_comprehension_expression env x
    | `Matrix_exp x -> map_matrix_expression env x
    | `Vec_exp x -> map_vector_expression env x
  in
  (* TODO: Should unsugar the more basic array accesses later, but for now this is OK
     This means that something like x[5] will be considered an ArrayAccess of an array.
  *)
  ArrayAccess (v1, (v2, v3, v2)) |> G.e

(* We need this here because, quite often, the enclosing function calling the interpolation
   function will want to have different behavior depending on if the interpolation succeeds
   with a metavariable.
   For instance, we might want to use a proper entity instead of an EDynamic, or a proper
   parameter instead of an OtherParam.
*)
and map_interpolation_expression_either (env : env)
    ((v1, v2) : CST.interpolation_expression) =
  let ((s1, t1) as v1) = (* "$" *) str env v1 in
  let v2 =
    match v2 with
    | `Lit x -> map_literal env x
    | `Quot x -> map_quotable env x
  in
  match v2.e with
  (* Actually, we might want to inject into Left even if it's not a pattern...
   *)
  | G.N (Id ((s, tok), _)) when in_pattern env ->
      let id = (s1 ^ s, Parse_info.combine_infos t1 [ tok ]) in
      Left id
  | __else__ -> Right (OtherExpr (v1, [ G.E v2 ]) |> G.e)

and map_interpolation_expression (env : env)
    ((v1, v2) : CST.interpolation_expression) : expr =
  match map_interpolation_expression_either env (v1, v2) with
  | Left id -> G.N (H2.name_of_id id) |> G.e
  | Right exp -> exp

and map_interpolation_parameter (env : env)
    ((v1, v2) : CST.interpolation_expression) : parameter =
  let ((s1, t1) as v1) = (* "$" *) str env v1 in
  let v2 =
    match v2 with
    | `Lit x -> map_literal env x
    | `Quot x -> map_quotable env x
  in
  match v2.e with
  (* When this is a pattern, this parameter is not an interpolation, but a metavariable.
   *)
  | G.N (Id ((s, tok), _)) when in_pattern env ->
      let id = (s1 ^ s, Parse_info.combine_infos t1 [ tok ]) in
      Param (param_of_id id)
  | __else__ -> OtherParam (v1, [ G.E v2 ])

and map_keyword_parameters (env : env)
    ((v1, v2, v3, v4) : CST.keyword_parameters) =
  let _v1 = (* ";" *) token env v1 in
  let v2 = map_anon_choice_id_687d935 env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_id_687d935 env v2 in
        v2)
      v3
  in
  let _v4 =
    match v4 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  v2 :: v3

and map_literal (env : env) (x : CST.literal) =
  match x with
  | `Bool_lit x -> map_boolean_literal env x
  | `Int_lit x -> map_integer_literal env x
  | `Float_lit x -> map_float_literal env x
  | `Char_lit tok ->
      (* character_literal *)
      let s, tok = str env tok in
      L (Char (s, tok)) |> G.e
  | `Str_lit (v1, v2, v3) ->
      let l = (* string_start *) token env v1 in
      let xs = Common.map (map_anon_choice_str_content_838a78d env) v2 in
      let r = (* string_end *) token env v3 in
      G.interpolated (l, xs, r)
  | `Cmd_lit (v1, v2, v3) ->
      (* Command literals look like strings, but they aren't.
         See https://docs.julialang.org/en/v1/manual/running-external-programs/
      *)
      let s, v1 = (* command_start *) str env v1 in
      let v2 = Common.map (map_anon_choice_str_content_838a78d env) v2 in
      let v3 = (* command_end *) token env v3 in
      OtherExpr ((s, v1), [ G.E (G.interpolated (v1, v2, v3)) ]) |> G.e

and map_macro_argument_list (env : env) (xs : CST.macro_argument_list) =
  List.concat_map (map_anon_choice_exp_c681153_arg env) xs

and map_macro_identifier (env : env) ((v1, v2) : CST.macro_identifier) : expr =
  let v1 = (* "@" *) str env v1 in
  let anys =
    match v2 with
    | `Id tok -> [ G.I (map_identifier env tok) ]
    | `Op x -> [ G.I (map_operator env x) ]
    | `Scoped_id x -> [ G.E (map_scoped_identifier_exp env x) ]
    | `Imm_tok_dot x -> [ G.I (str env x) ]
  in
  OtherExpr (v1, anys) |> G.e

and map_matrix_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.matrix_expression) =
  let start_tok = (* "[" *) token env v1 in
  let inner =
    match v2 with
    | `Matrix_row_choice_LF_opt_LF (v1, v2, v3) ->
        let v1 = map_matrix_row env v1 in
        let _v2 = map_terminator env v2 in
        let _v3 = map_newline_opt env v3 in
        [ v1 ]
    | `Matrix_row_rep_choice_LF_opt_LF_matrix_row (v1, v2) ->
        let v1 = map_matrix_row env v1 in
        let v2 =
          Common.map
            (fun (v1, v2, v3) ->
              let _v1 = map_terminator env v1 in
              let _v2 = map_newline_opt env v2 in
              let v3 = map_matrix_row env v3 in
              v3)
            v2
        in
        v1 :: v2
  in
  let _v3 = map_terminator_opt env v3 in
  let _v4 = map_newline_opt env v4 in
  let end_tok = (* "]" *) token env v5 in
  Container (Array, (start_tok, inner, end_tok)) |> G.e

and map_matrix_row (env : env) (xs : CST.matrix_row) =
  Container (Array, fb (Common.map (map_anon_choice_exp_91c2553_exp env) xs))
  |> G.e

and map_named_field_type_parameter (env : env) ((v1, v2, v3) : CST.named_field)
    =
  let v1 =
    match v1 with
    | `Id id -> map_identifier env id
    | `Interp_exp v1 -> todo env v1
  in
  let _v2 = (* "=" *) token env v2 in
  let v3 = map_anon_choice_exp_91c2553_exp env v3 in
  TP
    {
      tp_id = v1;
      tp_attrs = [];
      tp_bounds = [];
      tp_default = Some (TyExpr v3 |> G.t);
      tp_variance = None;
    }

and map_named_field (env : env) ((v1, v2, v3) : CST.named_field) =
  let v1 = map_anon_choice_id_00cc266 env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 =
    match v3 with
    | `Exp exp -> map_expression env exp
    | `Named_field x -> (
        let either, tok, exp = map_named_field env x in
        match either with
        | Left id -> Assign (G.N (H2.name_of_id id) |> G.e, tok, exp) |> G.e
        | Right e -> Assign (e, tok, exp) |> G.e)
    (* Who on earth is going to set a named field equal to a named field?? *)
  in
  (v1, v2, v3)

and map_optional_parameter (env : env) ((v1, v2, v3) : CST.optional_parameter) =
  let v1 = map_anon_choice_id_150150 env v1 in
  let _v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  match v1 with
  | Param ({ pdefault = None; _ } as p) -> Param { p with pdefault = Some v3 }
  | __else__ -> OtherParam (("optional", fake "optional"), [ G.Pa v1; G.E v3 ])

and map_parameter_list (env : env) ((v1, v2, v3, v4, v5) : CST.parameter_list) :
    parameters =
  let l = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_id_c087cf9 env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_anon_choice_id_c087cf9 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 =
    match v3 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  let v4 =
    match v4 with
    | Some x -> map_keyword_parameters env x
    | None -> []
  in
  let r = (* ")" *) token env v5 in
  (l, v2 @ v4, r)

and map_parametrized_type_expression (env : env)
    ((v1, v2, v3) : CST.parametrized_type_expression) =
  let v1 = map_primary_expression env v1 in
  let _v2 = (* immediate_brace *) token env v2 in
  let v3 = map_type_parameter_list env v3 in
  OtherExpr
    ( ("type_parametrized", fake "type_parametrized"),
      [ G.E v1; G.Anys (Common.map (fun x -> G.Tp x) v3) ] )
  |> G.e

and map_primary_expression (env : env) (x : CST.primary_expression) : expr =
  match x with
  | `Quot x -> map_quotable env x
  | `Broa_call_exp (v1, v2, v3, v4, v5) ->
      (* These are broadcasted vectorized functions.
         https://docs.julialang.org/en/v1/manual/functions/#man-vectorized
      *)
      let v1 = map_primary_expression env v1 in
      let v2 = (* "." *) str env v2 in
      let _v3 = (* immediate_paren *) token env v3 in
      let l, args, r = map_argument_list env v4 in
      let args =
        match v5 with
        | Some x -> Arg (map_do_clause env x) :: args
        | None -> args
      in
      Call (OtherExpr (v2, [ G.E v1 ]) |> G.e, (l, args, r)) |> G.e
  | `Call_exp x -> map_call_expression env x
  | `Closed_macr_exp x -> map_closed_macrocall_expression env x
  | `Para_type_exp x -> map_parametrized_type_expression env x
  | `Field_exp x -> map_field_expression env x
  | `Index_exp x -> map_index_expression env x
  | `Interp_exp x -> map_interpolation_expression env x
  | `Quote_exp x -> map_quote_expression env x
  | `Pref_cmd_lit x -> map_prefixed_command_literal env x
  | `Pref_str_lit x -> map_prefixed_string_literal env x

and map_type (env : env) (x : CST.primary_expression) : type_ =
  H2.expr_to_type (map_primary_expression env x)

and map_quotable (env : env) (x : CST.quotable) : expr =
  match x with
  | `Id tok ->
      let s, tok = map_identifier env tok in
      G.N (H2.name_of_id (s, tok)) |> G.e
  | `Curl_exp x ->
      (* This can be called while looking for the subject of a where clause.
       *)
      let tparams = map_type_parameter_list env x in
      OtherExpr
        ( ("curly", fake "curly"),
          [ G.Anys (Common.map (fun x -> G.Tp x) tparams) ] )
      |> G.e
  | `Comp_exp x -> map_comprehension_expression env x
  | `Matrix_exp x -> map_matrix_expression env x
  | `Vec_exp x -> map_vector_expression env x
  | `Paren_exp (v1, v2, v3, v4, v5, v6) -> (
      let v1 = (* "(" *) token env v1 in
      let v2 = map_anon_choice_decl_f2ab0d0 env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* ";" *) token env v1 in
            let v2 = map_anon_choice_decl_f2ab0d0 env v2 in
            v2)
          v3
      in
      let base = Seq (v2 :: v3) |> G.e in
      let _v5 =
        match v5 with
        | Some tok -> (* ";" *) Some (token env tok)
        | None -> None
      in
      let v6 = (* ")" *) token env v6 in
      match v4 with
      | Some x ->
          let comp = map_comprehension_clause env x in
          Comprehension (List, (v1, (base, comp), v6)) |> G.e
      | None -> base)
  | `Tuple_exp x ->
      let l, xs, r = map_tuple_expression env x in
      Container (Tuple, (l, xs, r)) |> G.e

and map_quote_expression (env : env) ((v1, v2) : CST.quote_expression) : expr =
  let v1 = (* ":" *) str env v1 in
  let v2 =
    match v2 with
    | `Lit x -> map_literal env x
    | `Quot x -> map_quotable env x
    | `Op x -> map_operator_exp env x
    | `Imm_tok_choice_tok_choice_dot_choice_plus x ->
        (* This is a bunch of random operators, like plus, lazy and, some assign ops, etc *)
        let id = map_imm_tok_choice_tok_choice_dot_choice_plus env x in
        N (H2.name_of_id id) |> G.e
    | `Imm_tok_choice_bare x ->
        (* Keywords, apparently *)
        let id = map_imm_tok_choice_bare env x in
        N (H2.name_of_id id) |> G.e
  in
  OtherExpr (v1, [ G.E v2 ]) |> G.e

and map_scoped_identifier_rev (env : env) ((v1, v2, v3) : CST.scoped_identifier)
    : (tok * (ident, expr) either) list * (ident, expr) either =
  let acc, base = map_anon_choice_id_f1f5a37 env v1 in
  let v2 = map_imm_tok_dot env v2 in
  let field =
    match v3 with
    | `Id tok -> Left (map_identifier env tok)
    | `Interp_exp x -> map_interpolation_expression_either env x
    | `Quote_exp x -> Right (map_quote_expression env x)
  in
  ((v2, field) :: acc, base)

and map_selected_import ~import_tok (env : env)
    ((v1, v2, v3, v4) : CST.selected_import) : directive_kind option =
  let* v1 = map_importable env v1 in
  let _v2 = map_imm_tok_colon env v2 in
  let* v3 = map_import_subject env v3 in
  let v4 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_import_subject env v2 in
        v2)
      v4
  in
  let* import_subjects = Some v3 :: v4 |> option_all in
  Some (ImportFrom (import_tok, DottedName v1, import_subjects))

and map_short_function_definition (env : env)
    ((v1, v2, v3) : CST.short_function_definition) =
  let _v2 = (* "=" *) token env v2 in
  let v3 = map_anon_choice_exp_3c18676 env v3 in
  let ent, fun_def =
    map_function_signature ~body:(FBExpr v3) ~func_tok:None env v1
  in
  DefStmt (ent, FuncDef fun_def) |> G.s

and map_slurp_parameter (env : env) ((v1, v2) : CST.slurp_parameter) =
  let v1 =
    match v1 with
    | `Id tok -> param_of_id (map_identifier env tok)
    | `Typed_param x -> map_typed_parameter_classic env x
  in
  let v2 = (* "..." *) token env v2 in
  ParamRest (v2, v1)

and map_source_file (env : env) (opt : CST.source_file) =
  match opt with
  | Some x -> map_block env x
  | None -> []

and map_source_file_stmt (env : env) (opt : CST.source_file) =
  match opt with
  | None -> Block (fb []) |> G.s
  | Some x -> Block (map_block env x |> fb) |> G.s

and map_statement (env : env) (x : CST.statement) : stmt list =
  match x with
  | `Semg_ellips tok ->
      [ ExprStmt (Ellipsis (token env tok) |> G.e, G.sc) |> G.s ]
  | `Choice_comp_stmt x -> (
      match x with
      | `Comp_stmt (v1, v2, v3, v4) ->
          let v1 = (* "begin" *) token env v1 in
          let _v2 = map_terminator_opt env v2 in
          let v3 = map_source_file env v3 in
          let v4 = (* "end" *) token env v4 in
          [ Block (v1, v3, v4) |> G.s ]
      | `Quote_stmt (v1, v2, v3, v4) ->
          (* This is a Expr object, created using a quote block.
             Not the same as a `begin` block.
          *)
          let _v1 = (* "quote" *) str env v1 in
          let _v2 = map_terminator_opt env v2 in
          let v3 = map_source_file_stmt env v3 in
          let _v4 = (* "end" *) token env v4 in
          [ OtherStmt (OS_Todo, [ G.S v3 ]) |> G.s ]
      | `Let_stmt (v1, v2, v3, v4, v5) ->
          let v1 = (* "let" *) token env v1 in
          let defs =
            match v2 with
            | Some (v1, v2) ->
                let v1 = map_anon_choice_id_c313bb1 env v1 in
                let v2 =
                  Common.map
                    (fun (v1, v2) ->
                      let _v1 = (* "," *) token env v1 in
                      let v2 = map_anon_choice_id_c313bb1 env v2 in
                      v2)
                    v2
                in
                v1 :: v2
            | None -> []
          in
          let _v3 = map_terminator env v3 in
          let v4 = map_source_file env v4 in
          let v5 = (* "end" *) token env v5 in
          [ Block (v1, defs @ v4, v5) |> G.s ]
      | `If_stmt (v1, v2, v3, v4, v5, v6, v7) ->
          let v1 = (* "if" *) token env v1 in
          let v2 = map_expression env v2 in
          let _v3 = map_terminator_opt env v3 in
          let v4 = map_source_file_stmt env v4 in
          let elses =
            let v5 = Common.map (map_elseif_clause env) v5 in
            let v6 =
              match v6 with
              | Some x -> Some (map_else_clause env x)
              | None -> None
            in
            let _v7 = (* "end" *) token env v7 in
            List.fold_right (fun k acc -> Some (k acc)) v5 v6
          in
          [ If (v1, Cond v2, v4, elses) |> G.s ]
      | `Try_stmt (v1, v2, v3, v4, v5, v6) ->
          let v1 = (* "try" *) token env v1 in
          let _v2 = map_terminator_opt env v2 in
          let v3 = map_source_file_stmt env v3 in
          let v4 =
            match v4 with
            | Some x -> [ map_catch_clause env x ]
            | None -> []
          in
          let v5 = Option.map (map_finally_clause env) v5 in
          let _v6 = (* "end" *) token env v6 in
          [ Try (v1, v3, v4, v5) |> G.s ]
      | `For_stmt (v1, v2, v3, v4, v5, v6) ->
          let v1 = (* "for" *) token env v1 in
          let header =
            let v2 = map_for_binding env v2 in
            let v3 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_for_binding env v2 in
                  v2)
                v3
            in
            match v3 with
            | [] -> ForEach v2
            | _ -> MultiForEach (Common.map (fun x -> FE x) (v2 :: v3))
          in
          let _v4 = map_terminator_opt env v4 in
          let v5 = map_source_file env v5 in
          let _v6 = (* "end" *) token env v6 in
          [ For (v1, header, Block (fb v5) |> G.s) |> G.s ]
      | `While_stmt (v1, v2, v3, v4, v5) ->
          let v1 = (* "while" *) token env v1 in
          let v2 = map_expression env v2 in
          let _v3 = map_terminator_opt env v3 in
          let v4 = map_source_file_stmt env v4 in
          let _v5 = (* "end" *) token env v5 in
          [ While (v1, Cond v2, v4) |> G.s ]
      | `Brk_stmt tok ->
          let tbreak = (* "break" *) token env tok in
          [ Break (tbreak, LNone, sc tbreak) |> G.s ]
      | `Cont_stmt tok ->
          let tcont = (* "continue" *) token env tok in
          [ Continue (tcont, LNone, sc tcont) |> G.s ]
      | `Ret_stmt (v1, v2) ->
          let v1 = (* "return" *) token env v1 in
          let v2 =
            match v2 with
            | Some x -> Some (map_anon_choice_exp_3c18676 env x)
            | None -> None
          in
          [ Return (v1, v2, sc v1) |> G.s ]
      | `Export_stmt (v1, v2, v3) ->
          let v1 = (* "export" *) str env v1 in
          let v2 = map_anon_choice_id_a8b5d0d env v2 in
          let v3 =
            Common.map
              (fun (v1, v2) ->
                let _v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_id_a8b5d0d env v2 in
                v2)
              v3
          in
          [ DirectiveStmt (OtherDirective (v1, v2 :: v3) |> G.d) |> G.s ]
      | `Import_stmt (v1, v2) -> (
          let v1 =
            match v1 with
            | `Import tok -> (* "import" *) token env tok
            | `Using tok -> (* "using" *) token env tok
          in
          match v2 with
          | `Import_list x ->
              map_import_list ~import_tok:v1 env x
              (* Filter map here, as unrelated imports need not interfere with each other. *)
              |> List.filter_map Fun.id
              |> Common.map (fun dk -> DirectiveStmt (dk |> G.d) |> G.s)
          | `Sele_import x -> (
              match map_selected_import ~import_tok:v1 env x with
              | None -> []
              | Some directive_kind ->
                  [ DirectiveStmt (directive_kind |> G.d) |> G.s ])))

and map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) : expr =
  let v1 = (* string_start *) token env v1 in
  let v2 = Common.map (map_anon_choice_str_content_838a78d env) v2 in
  let v3 = (* string_end *) token env v3 in
  G.interpolated (v1, v2, v3)

and map_tuple_expression (env : env) ((v1, v2, v3) : CST.tuple_expression) :
    expr list bracket =
  let v1 = (* "(" *) token env v1 in
  let v3 = (* ")" *) token env v3 in
  let xs =
    match v2 with
    | Some x -> (
        match x with
        | `Choice_exp_COMMA (v1, v2) ->
            let v1 = map_anon_choice_exp_91c2553_exp env v1 in
            let _v2 = (* "," *) token env v2 in
            [ v1 ]
        | `Choice_exp_rep1_COMMA_choice_exp_opt_choice_comp_clause (v1, v2, v3)
          -> (
            let contents =
              let v1 = map_anon_choice_exp_91c2553_exp env v1 in
              let v2 =
                Common.map
                  (fun (v1, v2) ->
                    let _v1 = (* "," *) token env v1 in
                    let v2 = map_anon_choice_exp_91c2553_exp env v2 in
                    v2)
                  v2
              in
              v1 :: v2
            in
            match List.rev contents with
            | [] -> raise Impossible
            | x :: xs ->
                let last_elem =
                  match v3 with
                  | Some c -> (
                      match c with
                      | `Comp_clause c ->
                          let comp = map_comprehension_clause env c in
                          Comprehension (List, fb (x, comp)) |> G.e
                      | `COMMA _tok -> (* "," *) x)
                  | None -> x
                in
                List.rev (last_elem :: xs))
        | `SEMI _tok ->
            (* ";" *)
            []
        | `SEMI_choice_exp_rep_COMMA_choice_exp_opt_COMMA (v1, v2, v3, v4) ->
            let _v1 = (* ";" *) token env v1 in
            let v2 = map_anon_choice_exp_91c2553_exp env v2 in
            let v3 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_anon_choice_exp_91c2553_exp env v2 in
                  v2)
                v3
            in
            let _v4 =
              match v4 with
              | Some tok -> (* "," *) Some (token env tok)
              | None -> None
            in
            v2 :: v3)
    | None -> []
  in
  (v1, xs, v3)

and map_tuple_pat (env : env) ((v1, v2, v3) : CST.tuple_expression) : pattern =
  (* TODO: this should be a proper PatTuple
     but Julia permits so many things to exist here that it's a big pain to do
     so I'd rather just leave it as a TODO for now
  *)
  let _, xs, _ = map_tuple_expression env (v1, v2, v3) in
  OtherPat
    (("pattern", fake "pattern"), [ G.E (Container (Tuple, fb xs) |> G.e) ])

and map_type_clause (env : env) ((v1, v2) : CST.type_clause) =
  let v1 =
    match v1 with
    | `LTCOLON tok -> (* "<:" *) str env tok
    | `GTCOLON tok -> (* ">:" *) str env tok
  in
  let v2 = map_primary_expression env v2 in
  OtherAttribute (v1, [ G.E v2 ])

and map_type_clause_opt (env : env) (v1 : CST.type_clause option) =
  match v1 with
  | None -> []
  | Some x -> [ map_type_clause env x ]

and map_typed_expression (env : env) ((v1, v2, v3) : CST.typed_expression) =
  let v1 = map_expression env v1 in
  let tok = (* "::" *) token env v2 in
  let v3 =
    match v3 with
    | `Prim_exp x -> map_type env x
  in
  (v1, tok, v3)

and map_typed_expression_exp (env : env) ((v1, v2, v3) : CST.typed_expression) :
    expr =
  let v1 = map_expression env v1 in
  let tok = (* "::" *) token env v2 in
  let v3 =
    match v3 with
    | `Prim_exp x -> map_primary_expression env x
  in
  Cast (TyExpr v3 |> G.t, tok, v1) |> G.e

and map_typed_parameter (env : env) (v1 : CST.typed_parameter) : parameter =
  match v1 with
  | `Semg_ellips tok -> ParamEllipsis (token env tok)
  | `Opt_choice_id_COLONCOLON_prim_exp_opt_where_clause (v1, v2, v3, v4) -> (
      let _v2 = (* "::" *) token env v2 in
      let v3 = map_type env v3 in
      let pattrs =
        match v4 with
        | Some x -> [ map_where_clause env x ]
        | None -> []
      in
      match v1 with
      | Some x -> (
          match x with
          | `Id tok ->
              Param
                (param_of_id ~pattrs ~ptype:(Some v3) (map_identifier env tok))
          | `Tuple_exp x ->
              let pat = map_tuple_pat env x in
              OtherParam (("param", fake "param"), [ G.P pat ])
          | `Interp_exp x -> (
              match map_interpolation_expression_either env x with
              | Left id -> Param (param_of_id id)
              | Right exp -> OtherParam (("param", fake "param"), [ G.E exp ])))
      | None ->
          (* For some absolutely absurd reason, you can elide the pattern, but still enforce a type
              on it, meaning that the parameter here might not actually exist.
              Absurd.

              function f(:: Int64) return 1 end
          *)
          Param
            {
              pname = None;
              ptype = Some v3;
              pdefault = None;
              pattrs;
              pinfo = empty_id_info ();
            })

and map_typed_parameter_classic (env : env) (v1 : CST.typed_parameter) :
    parameter_classic =
  match map_typed_parameter env v1 with
  | Param classic -> classic
  | __else__ -> failwith "not classic"

and map_unary_expression (env : env) ((v1, v2) : CST.unary_expression) =
  let ((s, v1) as id) = (* unary_operator *) str env v1 in
  let v2 = map_expression env v2 in
  match s with
  | "+" -> opcall (Plus, v1) [ v2 ]
  | "-" -> opcall (Minus, v1) [ v2 ]
  | "!" -> opcall (Not, v1) [ v2 ]
  | "~" -> opcall (BitNot, v1) [ v2 ]
  (* Julia says this doesn't exist, the negate I mean. *)
  | "¬"
  | "√"
  | "∛"
  | "∜"
  | _ ->
      Call (N (H2.name_of_id id) |> G.e, fb [ Arg v2 ]) |> G.e

and map_vector_expression (env : env) ((v1, v2, v3, v4) : CST.vector_expression)
    =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_anon_choice_exp_rep_COMMA_choice_exp_7e6cb67_exp env x
    | None -> []
  in
  let _ =
    match v3 with
    | Some _tok -> (* "," *) ()
    | None -> ()
  in
  let v4 = (* "]" *) token env v4 in
  Container (Array, (v1, v2, v4)) |> G.e

and map_where_clause (env : env) ((v1, v2, v3) : CST.where_clause) =
  let v1 = (* "where" *) str env v1 in
  let v2 = map_primary_expression env v2 in
  match v3 with
  | Some x ->
      let v3 = map_type_clause env x in
      OtherAttribute (v1, [ G.E v2; G.At v3 ])
  | None -> OtherAttribute (v1, [ G.E v2 ])

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_julia.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Program } in
      map_source_file env cst)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_julia.Parse.string str)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = Pattern } in
      match map_source_file env cst with
      | [ s ] -> (
          match s.G.s with
          | ExprStmt (e, _) -> G.E e
          | _ -> G.S s)
      | stmts -> G.Ss stmts)

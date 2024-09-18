(* Yoann Padioleau
 *
 * Copyright (C) 2020-2022 r2c
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
open Either_
open Fpath_.Operators
module AST = Ast_go
module CST = Tree_sitter_go.CST
open Ast_go
module G = AST_generic
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Go parser using tree-sitter-lang/semgrep-go and converting
 * to ../ast/ast_go.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * go_to_generic.ml
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str

(* for Ast_go.mk_vars_or_consts *)
let rev = false

let trailing_comma env v =
  match v with
  | Some tok -> Some (token env tok) (* "," *)
  | None -> None

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-go/.../Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the go CST
   to another type of tree.
*)

(* TODO: Update grammar so that the leading and trailing backticks are
 * tokenized separately, the way interpreted string literals are:
 * https://github.com/tree-sitter/tree-sitter-go/blob/0fa917a7022d1cd2e9b779a6a8fc5dc7fad69c75/grammar.js#L839-L843
 * *)
let raw_string_literal env tok =
  let _, s = tok in
  (* Remove leading and trailing backticks. The grammar guarantees that raw
   * string literals will always have leading and trailing backticks, so this
   * String.sub call should be safe. Let's check just to be sure. *)
  if
    not
      (String.length s >= 2
      && String.get s 0 =$= '`'
      && String.get s (String.length s - 1) =$= '`')
  then
    failwith @@ "Found unexpected raw string literal without delimiters: " ^ s;
  let s = String.sub s 1 (String.length s - 2) in
  (s, token env tok)

let expr1 xs =
  match xs with
  | [] -> raise Impossible
  | [ x ] -> x
  | _ -> failwith "expr1: was expecting just one expression"

let expr_to_call_expr = function
  | Call x -> x
  | _ -> failwith "expr_to_call_expr: was expecting a Call"

let identifier (env : env) (tok : CST.identifier) = str env tok

(* identifier *)

let anon_choice_new_0342769 (env : env) (x : CST.anon_choice_new_0342769) =
  match x with
  | `New tok -> str env tok (* "new" *)
  | `Make tok -> str env tok

(* "make" *)

let imaginary_literal (env : env) (tok : CST.imaginary_literal) = str env tok

(* imaginary_literal *)

let float_literal (env : env) (tok : CST.float_literal) =
  let s, t = str env tok (* float_literal *) in
  (float_of_string_opt s, t)

let int_literal (env : env) (tok : CST.int_literal) =
  let s, t = str env tok (* int_literal *) in
  Parsed_int.parse_c_octal (s, t)

let rune_literal (env : env) (tok : CST.rune_literal) = str env tok

(* rune_literal *)

let escape_sequence (env : env) (tok : CST.escape_sequence) = str env tok

(* escape_sequence *)

let anon_choice_EQ_4ccabd6 (env : env) (x : CST.anon_choice_EQ_4ccabd6) =
  match x with
  | `EQ tok -> (Left (), token env tok) (* "=" *)
  | `COLONEQ tok -> (Right (), token env tok)
(* ":=" *)

let anon_choice_LF_249c99f (env : env) (x : CST.anon_choice_LF_249c99f) =
  match x with
  | `LF tok -> token env tok (* "\n" *)
  | `SEMI tok -> token env tok (* ";" *)

let trailing_terminator env v =
  match v with
  | Some x -> Some (anon_choice_LF_249c99f env x)
  | None -> None

let qualified_type (env : env) ((v1, v2, v3) : CST.qualified_type) =
  let v1 = identifier env v1 (* identifier *) in
  let _v2 = token env v2 (* "." *) in
  let v3 = identifier env v3 (* identifier *) in
  [ v1; v3 ]

let empty_labeled_statement (env : env) ((v1, v2) : CST.empty_labeled_statement)
    : stmt =
  let v1 = identifier env v1 (* identifier *) in
  let _v2 = token env v2 (* ":" *) in
  Label (v1, Empty)

let field_name_list (env : env) ((v1, v2) : CST.field_name_list) : ident list =
  let v1 = identifier env v1 (* identifier *) in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = identifier env v2 (* identifier *) in
        v2)
      v2
  in
  v1 :: v2

let string_literal (env : env) (x : CST.string_literal) =
  match x with
  | `Raw_str_lit tok -> raw_string_literal env tok (* raw_string_literal *)
  | `Inte_str_lit (v1, v2, v3) ->
      let v1 = token env v1 (* "\"" *) in
      let v2 =
        List_.map
          (fun x ->
            match x with
            | `Inte_str_lit_basic_content tok ->
                str env tok (* pattern "[^\"\\n\\\\]+" *)
            | `Esc_seq tok -> escape_sequence env tok
            (* escape_sequence *))
          v2
      in
      let v3 = token env v3 (* "\"" *) in
      let str = v2 |> List_.map fst |> String.concat "" in
      let toks = (v2 |> List_.map snd) @ [ v3 ] in
      (str, Tok.combine_toks v1 toks)

let import_spec (env : env) ((v1, v2) : CST.import_spec) =
  let v1 =
    match v1 with
    | Some x -> (
        match x with
        | `Dot tok -> ImportDot (token env tok) (* "." *)
        | `Blank_id tok -> ImportNamed (identifier env tok) (* "_" *)
        | `Id tok -> ImportNamed (identifier env tok) (* identifier *))
    | None -> ImportOrig
  in
  let v2 = string_literal env v2 in
  (v1, v2)

let rec type_case (env : env) ((v1, v2, v3, v4, v5) : CST.type_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 = type_ env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_ env v2 in
        v2)
      v3
  in
  let v4 = token env v4 (* ":" *) in
  let v5 =
    match v5 with
    | Some x -> statement_list env x
    | None -> []
  in
  let xs = v2 :: v3 in
  (CaseExprs (v1, xs |> List_.map (fun x -> Right x)), stmt1 v4 v5)

and simple_statement (env : env) (x : CST.simple_statement) : simple =
  match x with
  | `Exp x -> ExprStmt (expression env x)
  | `Send_stmt x -> ExprStmt (send_statement env x)
  | `Inc_stmt (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "++" *) in
      IncDec (v1, (G.Incr, v2), G.Postfix)
  | `Dec_stmt (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "--" *) in
      IncDec (v1, (G.Decr, v2), G.Postfix)
  | `Assign_stmt (v1, v2, v3) -> (
      let v1 = expression_list env v1 in
      let v2 =
        match v2 with
        | `STAREQ tok -> (G.Mult, token env tok) (* "*=" *)
        | `SLASHEQ tok -> (G.Div, token env tok) (* "/=" *)
        | `PERCEQ tok -> (G.Mod, token env tok) (* "%=" *)
        | `LTLTEQ tok -> (G.LSL, token env tok) (* "<<=" *)
        | `GTGTEQ tok -> (G.LSR, token env tok) (* ">>=" *)
        | `AMPEQ tok -> (G.BitAnd, token env tok) (* "&=" *)
        | `AMPHATEQ tok -> (G.BitClear, token env tok) (* "&^=" *)
        | `PLUSEQ tok -> (G.Plus, token env tok) (* "+=" *)
        | `DASHEQ tok -> (G.Minus, token env tok) (* "-=" *)
        | `BAREQ tok -> (G.BitOr, token env tok) (* "|=" *)
        | `HATEQ tok -> (G.BitXor, token env tok) (* "^=" *)
        | `EQ tok -> (G.Eq, token env tok)
        (* "=" *)
      in
      let v3 = expression_list env v3 in
      match v2 with
      | G.Eq, t -> Assign (v1, t, v3)
      | _ -> AssignOp (expr1 v1, v2, expr1 v3))
  | `Short_var_decl (v1, v2, v3) ->
      let v1 = expression_list env v1 in
      let v2 = token env v2 (* ":=" *) in
      let v3 = expression_list env v3 in
      DShortVars (v1, v2, v3)

and anon_choice_exp_047b57a (env : env) (x : CST.anon_choice_exp_047b57a) =
  match x with
  | `Exp x -> Arg (expression env x)
  | `Vari_arg (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "..." *) in
      ArgDots (v1, v2)

and binary_expression (env : env) (x : CST.binary_expression) =
  match x with
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        match v2 with
        | `STAR tok -> (G.Mult, token env tok) (* "*" *)
        | `SLASH tok -> (G.Div, token env tok) (* "/" *)
        | `PERC tok -> (G.Mod, token env tok) (* "%" *)
        | `LTLT tok -> (G.LSL, token env tok) (* "<<" *)
        | `GTGT tok -> (G.LSR, token env tok) (* ">>" *)
        | `AMP tok -> (G.BitAnd, token env tok) (* "&" *)
        | `AMPHAT tok -> (G.BitClear, token env tok)
        (* "&^" *)
      in
      let v3 = expression env v3 in
      Binary (v1, v2, v3)
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (G.Plus, token env tok) (* "+" *)
        | `DASH tok -> (G.Minus, token env tok) (* "-" *)
        | `BAR tok -> (G.BitOr, token env tok) (* "|" *)
        | `HAT tok -> (G.BitXor, token env tok)
        (* "^" *)
      in
      let v3 = expression env v3 in
      Binary (v1, v2, v3)
  | `Exp_choice_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        match v2 with
        | `EQEQ tok -> (G.Eq, token env tok) (* "==" *)
        | `BANGEQ tok -> (G.NotEq, token env tok) (* "!=" *)
        | `LT tok -> (G.Lt, token env tok) (* "<" *)
        | `LTEQ tok -> (G.LtE, token env tok) (* "<=" *)
        | `GT tok -> (G.Gt, token env tok) (* ">" *)
        | `GTEQ tok -> (G.GtE, token env tok)
        (* ">=" *)
      in
      let v3 = expression env v3 in
      Binary (v1, v2, v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      Binary (v1, (G.And, v2), v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = expression env v3 in
      Binary (v1, (G.Or, v2), v3)

and interface_body (env : env) (x : CST.interface_body) : interface_field =
  match x with
  | `Meth_spec (v1, v2, v3) ->
      let id = (* identifier *) identifier env v1 in
      let fparams = parameter_list env v2 in
      let fresults =
        match v3 with
        | Some x -> anon_choice_param_list_29faba4 env x
        | None -> []
      in
      Method (id, { ftok = snd id; fparams; fresults })
  | `Inte_type_name x ->
      let name = interface_type_name env x in
      EmbeddedInterface name
  | `Cons_elem (v1, v2) ->
      let v1 = constraint_term env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "|" *) token env v1 in
            let v2 = constraint_term env v2 in
            v2)
          v2
      in
      let xs = v1 :: v2 in
      Constraints xs
  | `Struct_elem (v1, v2) ->
      let v1 = struct_term env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "|" *) token env v1 in
            let v2 = struct_term env v2 in
            v2)
          v2
      in
      let xs = v1 :: v2 in
      Constraints xs

and struct_term (env : env) ((v1, v2) : CST.struct_term) : constraint_ =
  let tilde_opt, fty =
    match v1 with
    | Some x -> (
        match x with
        | `TILDE tok ->
            let t = (* "~" *) token env tok in
            (Some t, fun ty -> ty)
        | `STAR tok ->
            let t = (* "*" *) token env tok in
            (None, fun ty -> TPtr (t, ty)))
    | None -> (None, fun ty -> ty)
  in
  let ty = struct_type env v2 in
  (tilde_opt, fty ty)

and constraint_term (env : env) ((v1, v2) : CST.constraint_term) : constraint_ =
  let tilde_opt =
    match v1 with
    | Some tok -> Some ((* "~" *) token env tok)
    | None -> None
  in
  let id = (* identifier *) identifier env v2 in
  let ty = TName [ id ] in
  (tilde_opt, ty)

and interface_type_name (env : env) (x : CST.interface_type_name) :
    qualified_ident =
  match x with
  | `Id tok -> [ (* identifier *) identifier env tok ]
  | `Qual_type x -> qualified_type env x

and block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some x -> statement_list env x
    | None -> []
  in
  let v3 = token env v3 (* "}" *) in
  Block (v1, v2, v3)

and receive_statement (env : env) ((v1, v2) : CST.receive_statement) =
  let v1 =
    match v1 with
    | Some (v1, v2) ->
        let v1 = expression_list env v1 in
        let v2 = anon_choice_EQ_4ccabd6 env v2 in
        Some (v1, v2)
    | None -> None
  in
  let v2 = expression env v2 in
  (v1, v2)

and field_declaration (env : env) ((v1, v2) : CST.field_declaration) :
    struct_field list =
  let v1 =
    match v1 with
    | `Id_rep_COMMA_id_choice_simple_type (v1, v2, v3) ->
        let v1 = identifier env v1 (* identifier *) in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = identifier env v2 (* identifier *) in
              v2)
            v2
        in
        let v3 = type_ env v3 in
        let xs = v1 :: v2 in
        xs |> List_.map (fun id -> Field (id, v3))
    | `Opt_STAR_choice_id (v1, v2) ->
        let v1 =
          match v1 with
          | Some tok -> Some (token env tok) (* "*" *)
          | None -> None
        in
        let v2 =
          match v2 with
          | `Id tok -> [ identifier env tok ] (* identifier *)
          | `Qual_type x -> qualified_type env x
        in
        [ EmbeddedField (v1, v2) ]
  in
  let v2 =
    match v2 with
    | Some x -> Some (string_literal env x)
    | None -> None
  in
  v1 |> List_.map (fun x -> (x, v2))

and special_argument_list (env : env)
    ((v1, v2, v3, v4, v5) : CST.special_argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 = type_ env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = expression env v2 in
        Arg v2)
      v3
  in
  let _v4 = trailing_comma env v4 in
  let v5 = token env v5 (* ")" *) in
  let args = ArgType v2 :: v3 in
  (v1, args, v5)

and for_clause (env : env) ((v1, v2, v3, v4, v5) : CST.for_clause) =
  let v1 =
    match v1 with
    | Some x -> Some (simple_statement env x)
    | None -> None
  in
  let _v2 = token env v2 (* ";" *) in
  let v3 =
    match v3 with
    | Some x -> Some (expression env x)
    | None -> None
  in
  let _v4 = token env v4 (* ";" *) in
  let v5 =
    match v5 with
    | Some x -> Some (simple_statement env x)
    | None -> None
  in
  ForClassic (v1, v3, v5)

and parameter_declaration env (v1, v2) =
  let v2 = type_ env v2 in
  match v1 with
  | Some x ->
      field_name_list env x
      |> List_.map (fun id ->
             ParamClassic { pname = Some id; ptype = v2; pdots = None })
  | None -> [ ParamClassic { pname = None; ptype = v2; pdots = None } ]

and anon_choice_param_decl_18823e5 (env : env)
    (x : CST.anon_choice_param_decl_18823e5) =
  match x with
  | `Param_decl x -> parameter_declaration env x
  | `Vari_param_decl (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some tok -> Some (identifier env tok) (* identifier *)
        | None -> None
      in
      let v2 = token env v2 (* "..." *) in
      let v3 = type_ env v3 in
      [ ParamClassic { pname = v1; ptype = v3; pdots = Some v2 } ]

and array_type (env : env) ((v1, v2, v3, v4) : CST.array_type) =
  let v1 = token env v1 (* "[" *) in
  let v2 = expression env v2 in
  let v3 = token env v3 (* "]" *) in
  let v4 = type_ env v4 in
  TArray ((v1, Some v2, v3), v4)

and struct_type (env : env) ((v1, v2) : CST.struct_type) =
  let v1 = token env v1 (* "struct" *) in
  let v2 = field_declaration_list env v2 in
  TStruct (v1, v2)

and anon_choice_param_list_29faba4 (env : env)
    (x : CST.anon_choice_param_list_29faba4) : parameter_binding list =
  match x with
  | `Param_list x ->
      let _l, xs, _r = parameter_list env x in
      xs
  | `Simple_type x ->
      let x = simple_type env x in
      [ ParamClassic { pname = None; ptype = x; pdots = None } ]

and simple_type (env : env) (x : CST.simple_type) : type_ =
  match x with
  | `Id tok -> TName [ identifier env tok ] (* identifier *)
  | `Gene_type x -> generic_type env x
  | `Qual_type x -> TName (qualified_type env x)
  | `Poin_type (v1, v2) ->
      let v1 = token env v1 (* "*" *) in
      let v2 = type_ env v2 in
      TPtr (v1, v2)
  | `Struct_type x -> struct_type env x
  | `Inte_type (v1, v2, v3, v4) ->
      let tinterface = (* "interface" *) token env v1 in
      let lbra = (* "{" *) token env v2 in
      let fields =
        match v3 with
        | Some (v1, v2, v3) ->
            let v1 = interface_body env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = anon_choice_LF_249c99f env v1 in
                  let v2 = interface_body env v2 in
                  v2)
                v2
            in
            let _v3 = trailing_terminator env v3 in
            v1 :: v2
        | None -> []
      in
      let rbra = (* "}" *) token env v4 in
      TInterface (tinterface, (lbra, fields, rbra))
  | `Array_type x -> array_type env x
  | `Slice_type x -> slice_type env x
  | `Map_type x -> map_type env x
  | `Chan_type x -> channel_type env x
  | `Func_type (v1, v2, v3) ->
      let ftok = token env v1 (* "func" *) in
      let v2 = parameter_list env v2 in
      let v3 =
        match v3 with
        | Some x -> anon_choice_param_list_29faba4 env x
        | None -> []
      in
      TFunc { ftok; fparams = v2; fresults = v3 }

and generic_type (env : env) ((v1, v2) : CST.generic_type) : type_ =
  let name = interface_type_name env v1 in
  let targs = type_arguments env v2 in
  TGeneric (name, targs)

and type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) =
  let lbra = (* "[" *) token env v1 in
  let t = type_ env v2 in
  let ts =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = type_ env v2 in
        v2)
      v3
  in
  let _v4 = trailing_comma env v4 in
  let rbra = (* "]" *) token env v5 in
  (lbra, t :: ts, rbra)

and call_expression (env : env) (x : CST.call_expression) =
  match x with
  | `Choice_new_spec_arg_list (v1, v2) ->
      let v1 = anon_choice_new_0342769 env v1 in
      let v2 = special_argument_list env v2 in
      Call (mk_Id v1, None, v2)
  | `Exp_opt_type_args_arg_list (v1, v2, v3) ->
      let e = expression env v1 in
      let targs_opt =
        match v2 with
        | Some x -> Some (type_arguments env x)
        | None -> None
      in
      let args = argument_list env v3 in
      Call (e, targs_opt, args)

and default_case (env : env) ((v1, v2, v3) : CST.default_case) =
  let v1 = token env v1 (* "default" *) in
  let v2 = token env v2 (* ":" *) in
  let v3 =
    match v3 with
    | Some x -> statement_list env x
    | None -> []
  in
  (CaseDefault v1, stmt1 v2 v3)

and slice_type (env : env) ((v1, v2, v3) : CST.slice_type) =
  let v1 = token env v1 (* "[" *) in
  let v2 = token env v2 (* "]" *) in
  let v3 = type_ env v3 in
  TArray ((v1, None, v2), v3)

and expression_list (env : env) ((v1, v2) : CST.expression_list) =
  let v1 = expression env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = expression env v2 in
        v2)
      v2
  in
  v1 :: v2

and expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Un_exp (v1, v2) -> (
      let v2 = expression env v2 in

      match v1 with
      | `PLUS tok -> Unary ((G.Plus, token env tok (* "+" *)), v2)
      | `DASH tok -> Unary ((G.Minus, token env tok) (* "-" *), v2)
      | `BANG tok -> Unary ((G.Not, token env tok) (* "!" *), v2)
      | `HAT tok -> Unary ((G.Xor, token env tok (* "^" *)), v2)
      | `STAR tok -> Deref (token env tok (* "*" *), v2)
      | `AMP tok -> Ref (token env tok (* "&" *), v2)
      | `LTDASH tok -> Receive (token env tok (* "<-" *), v2))
  | `Bin_exp x -> binary_expression env x
  | `Sele_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = identifier env v3 (* identifier *) in
      Selector (v1, v2, v3)
  | `Index_exp (v1, v2, v3, v4) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = expression env v3 in
      let v4 = token env v4 (* "]" *) in
      Index (v1, (v2, v3, v4))
  | `Slice_exp (v1, v2, v3, v4) -> (
      let v1top = expression env v1 in
      let t1 = token env v2 (* "[" *) in

      let t2 = token env v4 (* "]" *) in

      match v3 with
      | `Opt_exp_COLON_opt_exp (v1, v2, v3) ->
          let v1 =
            match v1 with
            | Some x -> Some (expression env x)
            | None -> None
          in
          let _v2 = token env v2 (* ":" *) in
          let v3 =
            match v3 with
            | Some x -> Some (expression env x)
            | None -> None
          in
          Slice (v1top, (t1, (v1, v3, None), t2))
      | `Opt_exp_COLON_exp_COLON_exp (v1, v2, v3, v4, v5) ->
          let v1 =
            match v1 with
            | Some x -> Some (expression env x)
            | None -> None
          in
          let _v2 = token env v2 (* ":" *) in
          let v3 = expression env v3 in
          let _v4 = token env v4 (* ":" *) in
          let v5 = expression env v5 in
          Slice (v1top, (t1, (v1, Some v3, Some v5), t2)))
  | `Call_exp x -> call_expression env x
  | `Type_asse_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* "(" *) in
      let v4 = type_ env v4 in
      let v5 = token env v5 (* ")" *) in
      TypeAssert (v1, (v3, v4, v5))
  | `Type_conv_exp (v1, v2, v3, v4, v5) ->
      let v1 = type_ env v1 in
      let v2 = token env v2 (* "(" *) in
      let v3 = expression env v3 in
      let _v4 = trailing_comma env v4 in
      let v5 = token env v5 (* ")" *) in
      Cast (v1, (v2, v3, v5))
  | `Id tok -> mk_Id (identifier env tok) (* identifier *)
  | `Choice_new x -> mk_Id (anon_choice_new_0342769 env x)
  | `Comp_lit (v1, v2) ->
      let v1 =
        match v1 with
        | `Map_type x -> map_type env x
        | `Slice_type x -> slice_type env x
        | `Array_type x -> array_type env x
        | `Impl_len_array_type x -> implicit_length_array_type env x
        | `Struct_type x -> struct_type env x
        | `Id tok -> TName [ identifier env tok ] (* identifier *)
        | `Qual_type x -> TName (qualified_type env x)
        | `Gene_type x -> generic_type env x
      in
      let v2 = map_literal_value env v2 in
      CompositeLit (v1, v2)
  | `Func_lit (v1, v2, v3, v4) ->
      let ftok = token env v1 (* "func" *) in
      let v2 = parameter_list env v2 in
      let v3 =
        match v3 with
        | Some x -> anon_choice_param_list_29faba4 env x
        | None -> []
      in
      let v4 = block env v4 in
      FuncLit ({ ftok; fparams = v2; fresults = v3 }, v4)
  | `Choice_raw_str_lit x -> BasicLit (String (string_literal env x))
  | `Int_lit tok -> BasicLit (Int (int_literal env tok)) (* int_literal *)
  | `Float_lit tok ->
      BasicLit (Float (float_literal env tok)) (* float_literal *)
  | `Imag_lit tok ->
      BasicLit (Imag (imaginary_literal env tok)) (* imaginary_literal *)
  | `Rune_lit tok -> BasicLit (Rune (rune_literal env tok)) (* rune_literal *)
  | `Nil tok -> mk_Id (identifier env tok) (* "nil" *)
  | `True tok -> mk_Id (identifier env tok) (* "true" *)
  | `False tok -> mk_Id (identifier env tok) (* "false" *)
  | `Iota tok -> mk_Id (identifier env tok) (* iota *)
  | `Paren_exp (v1, v2, v3) ->
      let _v1 = token env v1 (* "(" *) in
      let v2 = expression env v2 in
      let _v3 = token env v3 (* ")" *) in
      v2

and type_switch_header (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.type_switch_header) =
  let v1 =
    match v1 with
    | Some (v1, v2) ->
        let v1 = simple_statement env v1 in
        let _v2 = token env v2 (* ";" *) in
        Some v1
    | None -> None
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = expression_list env v1 in
        let v2 = token env v2 (* ":=" *) in
        fun x -> DShortVars (v1, v2, [ x ])
    | None -> fun x -> ExprStmt x
  in
  let v3 = expression env v3 in
  let _v4 = token env v4 (* "." *) in
  let _v5 = token env v5 (* "(" *) in
  let v6 = token env v6 (* "type" *) in
  let _v7 = token env v7 (* ")" *) in
  let e = TypeSwitchExpr (v3, v6) in
  (v1, v2 e)

and type_alias (env : env) ((v1, v2, v3) : CST.type_alias) =
  let v1 = identifier env v1 (* identifier *) in
  let v2 = token env v2 (* "=" *) in
  let v3 = type_ env v3 in
  DTypeAlias (v1, v2, v3)

and if_statement (env : env) ((v1, v2, v3, v4, v5) : CST.if_statement) : stmt =
  let v1 = token env v1 (* "if" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = simple_statement env v1 in
        let _v2 = token env v2 (* ";" *) in
        Some v1
    | None -> None
  in
  let v3 = expression env v3 in
  let v4 = block env v4 in
  let v5 =
    match v5 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* "else" *) in
        let v2 =
          match v2 with
          | `Blk x -> block env x
          | `If_stmt x -> if_statement env x
        in
        Some v2
    | None -> None
  in
  If (v1, v2, v3, v4, v5)

and statement (env : env) (x : CST.statement) : stmt =
  match x with
  | `Decl x -> DeclStmts (declaration env x)
  | `Simple_stmt x -> SimpleStmt (simple_statement env x)
  | `Ret_stmt (v1, v2) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        match v2 with
        | Some x -> Some (expression_list env x)
        | None -> None
      in
      Return (v1, v2)
  | `Go_stmt (v1, v2) ->
      let v1 = token env v1 (* "go" *) in
      let v2 = expression env v2 in
      Go (v1, expr_to_call_expr v2)
  | `Defer_stmt (v1, v2) ->
      let v1 = token env v1 (* "defer" *) in
      let v2 = expression env v2 in
      Defer (v1, expr_to_call_expr v2)
  | `If_stmt x -> if_statement env x
  | `For_stmt (v1, v2, v3) -> (
      let v1 = token env v1 (* "for" *) in

      let v3 = block env v3 in

      match v2 with
      | Some x -> (
          match x with
          | `Exp x ->
              For (v1, ForClassic (None, Some (expression env x), None), v3)
          | `For_clause x -> For (v1, for_clause env x, v3)
          | `Range_clause x ->
              let a, b, c = range_clause env x in
              For (v1, ForRange (a, b, c), v3))
      | None -> For (v1, ForClassic (None, None, None), v3))
  | `Exp_switch_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = simple_statement env v1 in
            let _v2 = token env v2 (* ";" *) in
            Some v1
        | None -> None
      in
      let v3 =
        match v3 with
        | Some x -> Some (ExprStmt (expression env x))
        | None -> None
      in
      let _v4 = token env v4 (* "{" *) in
      let v5 =
        List_.map
          (fun x ->
            match x with
            | `Exp_case x -> CaseClause (expression_case env x)
            | `Defa_case x -> CaseClause (default_case env x))
          v5
      in
      let _v6 = token env v6 (* "}" *) in
      Switch (v1, v2, v3, v5)
  | `Type_switch_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = type_switch_header env v2 in
      let _v3 = token env v3 (* "{" *) in
      let v4 =
        List_.map
          (fun x ->
            match x with
            | `Type_case x -> type_case env x
            | `Defa_case x -> default_case env x)
          v4
        |> List_.map (fun x -> CaseClause x)
      in
      let a, b = v2 in
      let _v5 = token env v5 (* "}" *) in
      Switch (v1, a, Some b, v4)
  | `Select_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "select" *) in
      let _v2 = token env v2 (* "{" *) in
      let v3 =
        List_.map
          (fun x ->
            match x with
            | `Comm_case x -> communication_case env x
            | `Defa_case x -> default_case env x)
          v3
        |> List_.map (fun x -> CaseClause x)
      in
      let _v4 = token env v4 (* "}" *) in
      Select (v1, v3)
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = identifier env v1 (* identifier *) in
      let _v2 = token env v2 (* ":" *) in
      let v3 = statement env v3 in
      Label (v1, v3)
  | `Fall_stmt tok -> Fallthrough (token env tok) (* "fallthrough" *)
  | `Brk_stmt (v1, v2) ->
      let v1 = token env v1 (* "break" *) in
      let v2 =
        match v2 with
        | Some tok -> Some (identifier env tok) (* identifier *)
        | None -> None
      in
      Break (v1, v2)
  | `Cont_stmt (v1, v2) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 =
        match v2 with
        | Some tok -> Some (identifier env tok) (* identifier *)
        | None -> None
      in
      Continue (v1, v2)
  | `Goto_stmt (v1, v2) ->
      let v1 = token env v1 (* "goto" *) in
      let v2 = identifier env v2 (* identifier *) in
      Goto (v1, v2)
  | `Blk x -> block env x
  | `Empty_stmt tok ->
      let _t = token env tok (* ";" *) in
      Empty

and range_clause (env : env) ((v1, v2, v3) : CST.range_clause) =
  let v1 =
    match v1 with
    | Some (v1, v2) ->
        let v1 = expression_list env v1 in
        let v2 = anon_choice_EQ_4ccabd6 env v2 in
        Some (v1, snd v2)
    | None -> None
  in
  let v2 = token env v2 (* "range" *) in
  let v3 = expression env v3 in
  (v1, v2, v3)

and send_statement (env : env) ((v1, v2, v3) : CST.send_statement) =
  let v1 = expression env v1 in
  let v2 = token env v2 (* "<-" *) in
  let v3 = expression env v3 in
  Send (v1, v2, v3)

and field_declaration_list (env : env)
    ((v1, v2, v3) : CST.field_declaration_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = field_declaration env v1 in
        let v2 =
          List.concat_map
            (fun (v1, v2) ->
              let _v1 = anon_choice_LF_249c99f env v1 in
              let v2 = field_declaration env v2 in
              v2)
            v2
        in
        let _v3 = trailing_terminator env v3 in
        v1 @ v2
    | None -> []
  in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and map_type (env : env) ((v1, v2, v3, v4, v5) : CST.map_type) =
  let v1 = token env v1 (* "map" *) in
  let v2 = token env v2 (* "[" *) in
  let v3 = type_ env v3 in
  let v4 = token env v4 (* "]" *) in
  let v5 = type_ env v5 in
  TMap (v1, (v2, v3, v4), v5)

and implicit_length_array_type (env : env)
    ((v1, v2, v3, v4) : CST.implicit_length_array_type) =
  let v1 = token env v1 (* "[" *) in
  let v2 = token env v2 (* "..." *) in
  let v3 = token env v3 (* "]" *) in
  let v4 = type_ env v4 in
  TArrayEllipsis ((v1, v2, v3), v4)

and expression_case (env : env) ((v1, v2, v3, v4) : CST.expression_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 = expression_list env v2 in
  let v3 = token env v3 (* ":" *) in
  let v4 =
    match v4 with
    | Some x -> statement_list env x
    | None -> []
  in
  (CaseExprs (v1, v2 |> List_.map (fun x -> Left x)), stmt1 v3 v4)

and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = anon_choice_exp_047b57a env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = anon_choice_exp_047b57a env v2 in
              v2)
            v2
        in
        let _v3 = trailing_comma env v3 in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and type_ (env : env) (x : CST.type_) =
  match x with
  | `Simple_type x -> simple_type env x
  | `Paren_type (v1, v2, v3) ->
      let _v1 = token env v1 (* "(" *) in
      let v2 = type_ env v2 in
      let _v3 = token env v3 (* ")" *) in
      v2

and const_spec (env : env) ((v1, v2, v3) : CST.const_spec) =
  let v1 = identifier env v1 (* identifier *) in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = identifier env v2 (* identifier *) in
        v2)
      v2
  in
  let xs = v1 :: v2 in
  match v3 with
  | Some (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some x -> Some (type_ env x)
        | None -> None
      in
      let _v2 = token env v2 (* "=" *) in
      let v3 = expression_list env v3 in
      mk_consts ~rev xs v1 (Some v3)
  | None -> mk_consts ~rev xs None None

and map_anon_choice_lit_elem_0952f3f (env : env)
    (x : CST.anon_choice_lit_elem_0952f3f) : init =
  match x with
  | `Lit_elem x -> literal_element env x
  (* from the grammar:
     // In T{k: v}, the key k may be:
     // - any expression (when T is a map, slice or array),
     // - a field identifier (when T is a struct), or
     // - a literal_element (when T is an array).
     // The first two cases cannot be distinguished without type information.
  *)
  | `Keyed_elem (v1, v2, v3) ->
      let e1 = literal_element env v1 in
      let tcolon = token env v2 in
      let e2 = literal_element env v3 in
      InitKeyValue (e1, tcolon, e2)

and type_spec (env : env) ((v1, v2, v3) : CST.type_spec) =
  let v1 = identifier env v1 (* identifier *) in
  let tparams_opt =
    match v2 with
    | Some x -> Some (type_parameter_list env x)
    | None -> None
  in
  let v3 = type_ env v3 in
  DTypeDef (v1, tparams_opt, v3)

and type_parameter_list (env : env)
    ((v1, v2, v3, v4, v5) : CST.type_parameter_list) =
  let lbra = (* "[" *) token env v1 in
  let params = parameter_declaration env v2 in
  let paramss =
    List.concat_map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = parameter_declaration env v2 in
        v2)
      v3
  in
  let _v4 = trailing_comma env v4 in
  let rbra = (* "]" *) token env v5 in
  (lbra, params @ paramss, rbra)

and channel_type (env : env) (x : CST.channel_type) =
  match x with
  | `Chan_choice_simple_type (v1, v2) ->
      let v1 = token env v1 (* "chan" *) in
      let v2 = type_ env v2 in
      TChan (v1, TBidirectional, v2)
  | `Chan_LTDASH_choice_simple_type (v1, v2, v3) ->
      let v1 = token env v1 (* "chan" *) in
      let _v2 = token env v2 (* "<-" *) in
      let v3 = type_ env v3 in
      TChan (v1, TRecv, v3)
  | `LTDASH_chan_choice_simple_type (v1, v2, v3) ->
      let _v1 = token env v1 (* "<-" *) in
      let v2 = token env v2 (* "chan" *) in
      let v3 = type_ env v3 in
      TChan (v2, TSend, v3)

and parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) :
    parameter_binding list bracket =
  let lp = token env v1 (* "(" *) in
  let xs =
    match v2 with
    | Some (v1, v2) ->
        let v1 =
          match v1 with
          | Some (v1, v2) ->
              let v1 = anon_choice_param_decl_18823e5 env v1 in
              let v2 =
                List.concat_map
                  (fun (v1, v2) ->
                    let _v1 = token env v1 (* "," *) in
                    let v2 = anon_choice_param_decl_18823e5 env v2 in
                    v2)
                  v2
              in
              v1 @ v2
          | None -> []
        in
        let _v2 = trailing_comma env v2 in
        v1
    | None -> []
  in
  let rp = token env v3 (* ")" *) in
  (lp, xs, rp)

and literal_element (env : env) (x : CST.literal_element) : init =
  match x with
  | `Exp x -> InitExpr (expression env x)
  | `Lit_value x -> InitBraces (map_literal_value env x)

and var_spec (env : env) ((v1, v2, v3) : CST.var_spec) =
  let v1 = identifier env v1 (* identifier *) in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = identifier env v2 (* identifier *) in
        v2)
      v2
  in
  let xs = v1 :: v2 in
  match v3 with
  | `Choice_simple_type_opt_EQ_exp_list (v1, v2) ->
      let v1 = type_ env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* "=" *) in
            let v2 = expression_list env v2 in
            Some v2
        | None -> None
      in
      mk_vars ~rev xs (Some v1) v2
  | `EQ_exp_list (v1, v2) ->
      let _v1 = token env v1 (* "=" *) in
      let v2 = expression_list env v2 in
      mk_vars ~rev xs None (Some v2)

and declaration (env : env) (x : CST.declaration) =
  match x with
  | `Const_decl (v1, v2) ->
      let _v1 = token env v1 (* "const" *) in
      let v2 =
        match v2 with
        | `Const_spec x -> const_spec env x
        | `LPAR_rep_const_spec_choice_LF_RPAR (v1, v2, v3) ->
            let _v1 = token env v1 (* "(" *) in
            let v2 =
              List.concat_map
                (fun (v1, v2) ->
                  let v1 = const_spec env v1 in
                  let _v2 = anon_choice_LF_249c99f env v2 in
                  v1)
                v2
            in
            let _v3 = token env v3 (* ")" *) in
            v2
      in
      v2
  | `Type_decl (v1, v2) ->
      let _v1 = token env v1 (* "type" *) in
      let v2 =
        match v2 with
        | `Type_spec x -> [ type_spec env x ]
        | `Type_alias x -> [ type_alias env x ]
        | `LPAR_rep_choice_type_spec_choice_LF_RPAR (v1, v2, v3) ->
            let _v1 = token env v1 (* "(" *) in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let v1 =
                    match v1 with
                    | `Type_spec x -> type_spec env x
                    | `Type_alias x -> type_alias env x
                  in
                  let _v2 = anon_choice_LF_249c99f env v2 in
                  v1)
                v2
            in
            let _v3 = token env v3 (* ")" *) in
            v2
      in
      v2
  | `Var_decl (v1, v2) ->
      let _v1 = token env v1 (* "var" *) in
      let v2 =
        match v2 with
        | `Var_spec x -> var_spec env x
        | `LPAR_rep_var_spec_choice_LF_RPAR (v1, v2, v3) ->
            let _v1 = token env v1 (* "(" *) in
            let v2 =
              List.concat_map
                (fun (v1, v2) ->
                  let v1 = var_spec env v1 in
                  let _v2 = anon_choice_LF_249c99f env v2 in
                  v1)
                v2
            in
            let _v3 = token env v3 (* ")" *) in
            v2
      in
      v2

and statement_list (env : env) (x : CST.statement_list) : stmt list =
  match x with
  | `Stmt_rep_choice_LF_stmt_opt_choice_LF_opt_empty_labe_stmt (v1, v2, v3) ->
      let v1 = statement env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = anon_choice_LF_249c99f env v1 in
            let v2 = statement env v2 in
            v2)
          v2
      in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let _v1 = anon_choice_LF_249c99f env v1 in
            let v2 =
              match v2 with
              | Some x -> [ empty_labeled_statement env x ]
              | None -> []
            in
            v2
        | None -> []
      in
      v1 :: (v2 @ v3)
  | `Empty_labe_stmt x -> [ empty_labeled_statement env x ]

and communication_case (env : env) ((v1, v2, v3, v4) : CST.communication_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 =
    match v2 with
    | `Send_stmt x ->
        let e = send_statement env x in
        CaseExprs (v1, [ Left e ])
    | `Rece_stmt x -> (
        let opt, e = receive_statement env x in
        match opt with
        | None -> CaseExprs (v1, [ Left e ])
        | Some (xs, (_lr, tk)) ->
            CaseAssign (v1, xs |> List_.map (fun e -> Left e), tk, e))
  in
  let v3 = token env v3 (* ":" *) in
  let v4 =
    match v4 with
    | Some x -> statement_list env x
    | None -> []
  in
  (v2, stmt1 v3 v4)

and map_literal_value (env : env) ((v1, v2, v3) : CST.literal_value) :
    init list bracket =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 =
          match v1 with
          | Some (v1, v2) ->
              let v1 = map_anon_choice_lit_elem_0952f3f env v1 in
              let v2 =
                List_.map
                  (fun (v1, v2) ->
                    let _v1 = (* "," *) token env v1 in
                    let v2 = map_anon_choice_lit_elem_0952f3f env v2 in
                    v2)
                  v2
              in
              v1 :: v2
          | None -> []
        in
        let _v2 = trailing_comma env v2 in
        v1
    | None -> []
  in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

let import_spec_list (env : env) ((v1, v2, v3) : CST.import_spec_list) =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let v1 = import_spec env v1 in
        let _v2 = anon_choice_LF_249c99f env v2 in
        v1)
      v2
  in
  let _v3 = token env v3 (* ")" *) in
  v2

let top_level_declaration (env : env) (x : CST.top_level_declaration) :
    top_decl list =
  match x with
  | `Pack_clause (v1, v2) ->
      let v1 = token env v1 (* "package" *) in
      let v2 = identifier env v2 (* identifier *) in
      [ Package (v1, v2) ]
  | `Func_decl (v1, v2, v3, v4, v5, v6) ->
      let tfunc = token env v1 (* "func" *) in
      let id = identifier env v2 (* identifier *) in
      let tparams =
        match v3 with
        | Some x -> Some (type_parameter_list env x)
        | None -> None
      in
      let fparams = parameter_list env v4 in
      let fresults =
        match v5 with
        | Some x -> anon_choice_param_list_29faba4 env x
        | None -> []
      in
      let body =
        match v6 with
        | Some x -> block env x
        | None -> Empty
      in
      [ DFunc (id, tparams, ({ ftok = tfunc; fparams; fresults }, body)) ]
  | `Meth_decl (v1, v2, v3, v4, v5, v6) ->
      let ftok = token env v1 (* "func" *) in
      let _l, v2, _r = parameter_list env v2 in
      let v3 = identifier env v3 (* identifier *) in
      let v4 = parameter_list env v4 in
      let v5 =
        match v5 with
        | Some x -> anon_choice_param_list_29faba4 env x
        | None -> []
      in
      let v6 =
        match v6 with
        | Some x -> block env x
        | None -> Empty
      in
      let receiver =
        match v2 with
        | [] -> raise Impossible
        | [ ParamClassic x ] -> x
        | _ -> failwith "expected one receiver"
      in
      [ DMethod (v3, receiver, ({ ftok; fparams = v4; fresults = v5 }, v6)) ]
  | `Import_decl (v1, v2) ->
      let v1 = token env v1 (* "import" *) in
      let v2 =
        match v2 with
        | `Import_spec x -> [ import_spec env x ]
        | `Import_spec_list x -> import_spec_list env x
      in
      v2
      |> List_.map (fun (a, b) -> Import { i_tok = v1; i_path = b; i_kind = a })

let source_file (env : env) (xs : CST.source_file) : program =
  List.concat_map
    (fun x ->
      match x with
      | `Stmt_choice_LF (v1, v2) ->
          let v1 = statement env v1 in
          let _v2 = anon_choice_LF_249c99f env v2 in
          [ STop v1 ]
      | `Choice_pack_clause_opt_choice_LF (v1, v2) ->
          let v1 = top_level_declaration env v1 in
          let _v2 = trailing_terminator env v2 in
          v1)
    xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_go.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      let x = source_file env cst in
      x)

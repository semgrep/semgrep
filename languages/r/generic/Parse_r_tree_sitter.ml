(* Yoann Padioleau
 *
 * Copyright (c) 2022 R2C
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
open Fpath_.Operators
module CST = Tree_sitter_r.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

let combine_str_and_infos l xs r =
  let s = xs |> List_.map fst |> String.concat "" in
  let t = Tok.combine_toks l (List_.map snd xs @ [ r ]) in
  (s, t)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-r/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the r CST
   to another type of tree.
*)

let map_pat_de5d470 (env : env) (tok : CST.pat_de5d470) =
  (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) str env tok

let map_pat_4ad362e (env : env) (tok : CST.pat_4ad362e) =
  (* pattern [^`\\\n]+|\\\r?\n *) str env tok

let map_na (env : env) (x : CST.na) =
  match x with
  | `NA tok -> (* "NA" *) str env tok
  | `NA_char_ tok -> (* "NA_character_" *) str env tok
  | `NA_comp_ tok -> (* "NA_complex_" *) str env tok
  | `NA_int_ tok -> (* "NA_integer_" *) str env tok
  | `NA_real_ tok -> (* "NA_real_" *) str env tok

let map_pat_3e41275 (env : env) (tok : CST.pat_3e41275) =
  (* pattern [.\p{XID_Start}][._\p{XID_Continue}]* *) str env tok

let map_pat_5e7ac5f (env : env) (tok : CST.pat_5e7ac5f) =
  (* pattern [^%\\\n]+|\\\r?\n *) str env tok

let map_pat_3e57880 (env : env) (tok : CST.pat_3e57880) =
  (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) str env tok

let map_special (env : env) ((v1, v2, v3) : CST.special) : G.ident =
  let v1 = (* "%" *) token env v1 in
  let v2 =
    List_.map
      (fun x ->
        match x with
        | `Pat_5e7ac5f x -> map_pat_5e7ac5f env x
        | `Esc_seq tok -> (* escape_sequence *) str env tok)
      v2
  in
  let v3 = (* "%" *) token env v3 in
  combine_str_and_infos v1 v2 v3

let map_identifier (env : env) (x : CST.identifier) : G.ident =
  match x with
  | `Choice_pat_3e41275 x -> (
      match x with
      | `Pat_3e41275 x -> map_pat_3e41275 env x
      | `BQUOT_rep_choice_pat_4ad362e_BQUOT (v1, v2, v3) ->
          let l = (* "`" *) token env v1 in
          let xs =
            List_.map
              (fun x ->
                match x with
                | `Pat_4ad362e x -> map_pat_4ad362e env x
                | `Esc_seq tok -> (* escape_sequence *) str env tok)
              v2
          in
          let r = (* "`" *) token env v3 in
          combine_str_and_infos l xs r)
  | `Semg_meta tok -> (* semgrep_metavariable *) str env tok

let map_string_ (env : env) (x : CST.string_) : string G.wrap G.bracket =
  match x with
  (* TODO: need to remove the enclosing quotes *)
  | `Raw_str_lit tok -> (* raw_string_literal *) fb (str env tok)
  | `DQUOT_rep_choice_pat_de5d470_DQUOT (v1, v2, v3) ->
      let l = (* "\"" *) token env v1 in
      let xs =
        List_.map
          (fun x ->
            match x with
            | `Pat_de5d470 x -> map_pat_de5d470 env x
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let r = (* "\"" *) token env v3 in
      G.string_ (l, xs, r)
  | `SQUOT_rep_choice_pat_3e57880_SQUOT (v1, v2, v3) ->
      let l = (* "'" *) token env v1 in
      let xs =
        List_.map
          (fun x ->
            match x with
            | `Pat_3e57880 x -> map_pat_3e57880 env x
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let r = (* "'" *) token env v3 in
      G.string_ (l, xs, r)

let rec map_argument (env : env) (x : CST.argument) : G.argument =
  match x with
  | `Exp x ->
      let e = map_expression env x in
      G.Arg e
  | `Defa_arg x -> map_default_argument env x

and map_arguments (env : env) (xs : CST.arguments) : G.argument list =
  (* TODO ,, has a meaning? *)
  List_.filter_map
    (fun x ->
      match x with
      | `Arg x -> Some (map_argument env x)
      | `COMMA tok ->
          let _ = (* "," *) token env tok in
          None)
    xs

and map_assignment (env : env) (x : CST.assignment) : G.expr =
  match x with
  | `Equals_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      (* Eq or Assign? *)
      G.opcall (Eq, v2) [ v1; v3 ]
  | `Left_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<-" *) token env v2 in
      let v3 = map_expression env v3 in
      Assign (v1, v2, v3) |> G.e
  | `Left_assign2 (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ":=" *) token env v2 in
      let v3 = map_expression env v3 in
      OtherExpr ((":=", v2), [ E v1; E v3 ]) |> G.e
  | `Right_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_expression env v3 in
      OtherExpr (("->", v2), [ E v1; E v3 ]) |> G.e
  | `Super_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<<-" *) token env v2 in
      let v3 = map_expression env v3 in
      OtherExpr (("<<=", v2), [ E v1; E v3 ]) |> G.e
  | `Super_right_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "->>" *) token env v2 in
      let v3 = map_expression env v3 in
      OtherExpr (("->>", v2), [ E v1; E v3 ]) |> G.e

and map_binary (env : env) (x : CST.binary) : G.expr =
  match x with
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (Plus, (* "+" *) token env tok)
        | `DASH tok -> (Minus, (* "-" *) token env tok)
      in
      let v3 = map_expression env v3 in
      G.opcall v2 [ v1; v3 ]
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `STAR tok -> (Mult, (* "*" *) token env tok)
        | `SLASH tok -> (Div, (* "/" *) token env tok)
      in
      let v3 = map_expression env v3 in
      G.opcall v2 [ v1; v3 ]
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (Pow, v2) [ v1; v3 ]
  | `Exp_choice_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `LT tok -> (* "<" *) (Lt, token env tok)
        | `GT tok -> (* ">" *) (Gt, token env tok)
        | `LTEQ tok -> (* "<=" *) (LtE, token env tok)
        | `GTEQ tok -> (* ">=" *) (GtE, token env tok)
        | `EQEQ tok -> (* "==" *) (PhysEq, token env tok)
        | `BANGEQ tok -> (* "!=" *) (NotEq, token env tok)
      in
      let v3 = map_expression env v3 in
      G.opcall v2 [ v1; v3 ]
  | `Exp_choice_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `BARBAR tok -> (Or, (* "||" *) token env tok)
        | `BAR tok -> (BitOr, (* "|" *) token env tok)
      in
      let v3 = map_expression env v3 in
      G.opcall v2 [ v1; v3 ]
  | `Exp_choice_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `AMPAMP tok -> (And, (* "&&" *) token env tok)
        | `AMP tok -> (BitAnd, (* "&" *) token env tok)
      in
      let v3 = map_expression env v3 in
      G.opcall v2 [ v1; v3 ]
  | `Exp_spec_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_special env v2 in
      let v3 = map_expression env v3 in
      Call (N (H2.name_of_id v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_COLON_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_expression env v3 in
      OtherExpr ((":", v2), [ E v1; E v3 ]) |> G.e
  | `Exp_TILDE_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "~" *) token env v2 in
      let v3 = map_expression env v3 in
      OtherExpr (("~", v2), [ E v1; E v3 ]) |> G.e

and map_default_argument (env : env) ((v1, v2, v3) : CST.default_argument) :
    G.argument =
  let id =
    match v1 with
    | `Id x -> map_identifier env x
    | `Str x ->
        let _, x, _ = map_string_ env x in
        x
    (* TODO ?? not semgrep! *)
    | `Dots tok ->
        let x = (* "..." *) str env tok in
        x
  in
  let teq = (* "=" *) token env v2 in
  match v3 with
  | Some x ->
      let e = map_expression env x in
      ArgKwd (id, e)
  | None -> OtherArg (("Empty =", teq), [ I id ])

and map_expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Id x ->
      let id = map_identifier env x in
      N (H2.name_of_id id) |> G.e
  | `Int tok ->
      let s, t = (* integer *) str env tok in
      L (Int (Parsed_int.parse_c_octal (s, t))) |> G.e
  | `Float tok ->
      let s, t = (* float *) str env tok in
      L (Float (Common2.float_of_string_opt s, t)) |> G.e
  | `Comp (v1, v2) ->
      let s, t = (* float *) str env v1 in
      let v2 = (* "i" *) token env v2 in
      let finalt = Tok.combine_toks t [ v2 ] in
      L (Imag (s, finalt)) |> G.e
  | `Str x ->
      let x = map_string_ env x in
      L (String x) |> G.e
  | `Call (v1, v2, v3, v4) ->
      let e = map_expression env v1 in
      let l = (* "(" *) token env v2 in
      let xs =
        match v3 with
        | Some x -> map_arguments env x
        | None -> []
      in
      let r = (* ")" *) token env v4 in
      Call (e, (l, xs, r)) |> G.e
  | `Func_defi x ->
      let func = map_function_definition env x in
      Lambda func |> G.e
  | `Lambda_func x ->
      let func = map_lambda_function env x in
      Lambda func |> G.e
  | `Assign x -> map_assignment env x
  | `Paren_list (v1, v2, v3) ->
      let l = (* "(" *) token env v1 in
      let xs = List_.map (map_expression env) v2 in
      let r = (* ")" *) token env v3 in
      Container (Tuple, (l, xs, r)) |> G.e
  | `Bin x -> map_binary env x
  | `Un x -> map_unary env x
  | `Pipe (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|>" *) token env v2 in
      let v3 = map_pipe_rhs env v3 in
      OtherExpr (("|>", v2), [ E v1; E v3 ]) |> G.e
  | `Subset (v1, v2, v3, v4) -> (
      let e = map_expression env v1 in
      let l = (* "[" *) token env v2 in
      let xs =
        match v3 with
        | Some x -> map_arguments env x
        | None -> []
      in
      let r = (* "]" *) token env v4 in
      match xs with
      | [ Arg e ] -> ArrayAccess (e, (l, e, r)) |> G.e
      (* TODO? SliceAccess? *)
      | xs -> OtherExpr (("ArrayAccess[xs]", l), [ E e; Args xs ]) |> G.e)
  | `Subset2 (v1, v2, v3, v4) ->
      let e = map_expression env v1 in
      let l = (* "[[" *) token env v2 in
      let xs =
        match v3 with
        | Some x -> map_arguments env x
        | None -> []
      in
      let _r = (* "]]" *) token env v4 in
      OtherExpr (("ArrayAccess[[xs]]", l), [ E e; Args xs ]) |> G.e
  | `Dollar (v1, v2, v3) ->
      let e = map_expression env v1 in
      let t = (* "$" *) token env v2 in
      let id =
        match v3 with
        | `Id x -> map_identifier env x
        | `Str x -> map_string_ env x |> Tok.unbracket
      in
      DotAccess (e, t, FN (H2.name_of_id id)) |> G.e
  | `Slot (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "@" *) token env v2 in
      let v3 = map_identifier env v3 in
      OtherExpr (("@", v2), [ E v1; I v3 ]) |> G.e
  | `Name_get (v1, v2, v3) ->
      let v1 = map_identifier env v1 in
      let _v2 = (* "::" *) token env v2 in
      let v3 = map_identifier env v3 in
      N (H2.name_of_ids [ v1; v3 ]) |> G.e
  | `Name_get_inte (v1, v2, v3) ->
      let v1 = map_identifier env v1 in
      let v2 = (* ":::" *) token env v2 in
      let v3 = map_identifier env v3 in
      OtherExpr ((":::", v2), [ I v1; I v3 ]) |> G.e
  | `Brace_list _
  | `If _
  | `For _
  | `While _
  | `Repeat _
  | `Switch _
  | `Brk _
  | `Next _ ->
      let st = map_statement env x in
      G.stmt_to_expr st
  | `True tok ->
      let x = (* "TRUE" *) token env tok in
      L (Bool (true, x)) |> G.e
  | `False tok ->
      let x = (* "FALSE" *) token env tok in
      L (Bool (false, x)) |> G.e
  | `Null tok ->
      let x = (* "NULL" *) token env tok in
      L (Null x) |> G.e
  | `Inf tok ->
      let x = (* "Inf" *) str env tok in
      N (H2.name_of_id x) |> G.e
  | `Nan tok ->
      let x = (* "NaN" *) str env tok in
      N (H2.name_of_id x) |> G.e
  | `Na x ->
      let x = map_na env x in
      N (H2.name_of_id x) |> G.e
  (* part of original language! *)
  | `Dots tok ->
      let x = (* "..." *) token env tok in
      Ellipsis x |> G.e

and map_statement (env : env) (x : CST.expression) : G.stmt =
  match x with
  | `Brace_list (v1, v2, v3) ->
      let l = (* "{" *) token env v1 in
      let xs = map_program env v2 in
      let r = (* "}" *) token env v3 in
      Block (l, xs, r) |> G.s
  | `If (v1, v2, v3, v4, v5, v6) ->
      let tif = (* "if" *) token env v1 in
      let _l = (* "(" *) token env v2 in
      let cond = map_expression env v3 in
      let _r = (* ")" *) token env v4 in
      let then_ = map_statement env v5 in
      let elseopt =
        match v6 with
        | Some (v1, v2) ->
            let _telse = (* "else" *) token env v1 in
            let else_ = map_statement env v2 in
            Some else_
        | None -> None
      in
      If (tif, Cond cond, then_, elseopt) |> G.s
  | `For (v1, v2, v3, v4, v5, v6, v7) ->
      let tfor = (* "for" *) token env v1 in
      let _l = (* "(" *) token env v2 in
      let id = map_identifier env v3 in
      let tin = (* "in" *) token env v4 in
      let e = map_expression env v5 in
      let _r = (* ")" *) token env v6 in
      let body = map_statement env v7 in
      let for_header = ForEach (PatId (id, G.empty_id_info ()), tin, e) in
      For (tfor, for_header, body) |> G.s
  | `While (v1, v2, v3, v4, v5) ->
      let twhile = (* "while" *) token env v1 in
      let _l = (* "(" *) token env v2 in
      let cond = map_expression env v3 in
      let _r = (* ")" *) token env v4 in
      let body = map_statement env v5 in
      While (twhile, Cond cond, body) |> G.s
  | `Repeat (v1, v2) ->
      let trepeat = (* "repeat" *) token env v1 in
      let body = map_statement env v2 in
      While (trepeat, OtherCond (("Repeat", trepeat), []), body) |> G.s
  | `Switch (v1, v2, v3, v4, v5, v6) ->
      let _tswitch = (* "switch" *) token env v1 in
      let _l = (* "(" *) token env v2 in
      let cond = map_expression env v3 in
      let _comma = (* "," *) token env v4 in
      let cases = map_arguments env v5 in
      let _r = (* ")" *) token env v6 in
      (* TODO: transform in Switch? map_arguments?? *)
      OtherStmt (OS_Todo, [ E cond; Args cases ]) |> G.s
  | `Brk tok ->
      let x = (* "break" *) token env tok in
      Break (x, LNone, G.sc) |> G.s
  | `Next tok ->
      let x = (* "next" *) token env tok in
      Continue (x, LNone, G.sc) |> G.s
  | e ->
      let e = map_expression env e in
      G.exprstmt e

and map_formal_parameter (env : env) (x : CST.formal_parameter) : G.parameter =
  match x with
  | `Id x ->
      let id = map_identifier env x in
      G.Param (G.param_of_id id)
  | `Defa_param (v1, v2, v3) ->
      let id = map_identifier env v1 in
      let _teq = (* "=" *) token env v2 in
      let e = map_expression env v3 in
      G.Param (G.param_of_id id ~pdefault:e)
  | `Dots tok ->
      (* not semgrep-ext: either, part of the original language *)
      let t = (* "..." *) token env tok in
      G.ParamEllipsis t

and map_formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) :
    G.parameters =
  let l = (* "(" *) token env v1 in
  let xs =
    match v2 with
    | Some (v1, v2, v3) ->
        let e = map_formal_parameter env v1 in
        let xs =
          List_.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_formal_parameter env v2 in
              v2)
            v2
        in
        let _v3 =
          match v3 with
          | Some tok -> Some ((* "," *) token env tok)
          | None -> None
        in
        e :: xs
    | None -> []
  in
  let r = (* ")" *) token env v3 in
  (l, xs, r)

and map_function_definition (env : env) ((v1, v2, v3) : CST.function_definition)
    : G.function_definition =
  let tk = (* "function" *) token env v1 in
  let fparams = map_formal_parameters env v2 in
  let body = map_expression env v3 in
  { G.fkind = (LambdaKind, tk); fparams; frettype = None; fbody = FBExpr body }

and map_lambda_function (env : env) ((v1, v2, v3) : CST.lambda_function) :
    G.function_definition =
  let tk = (* "\\" *) token env v1 in
  let params = map_formal_parameters env v2 in
  let body = map_expression env v3 in
  {
    G.fkind = (LambdaKind, tk);
    fparams = params;
    frettype = None;
    fbody = FBExpr body;
  }

and map_pipe_rhs (env : env) ((v1, v2, v3, v4) : CST.pipe_rhs) : G.expr =
  let e = map_expression env v1 in
  let l = (* "(" *) token env v2 in
  let xs =
    match v3 with
    | Some x -> map_pipe_rhs_arguments env x
    | None -> []
  in
  let r = (* ")" *) token env v4 in
  Call (e, (l, xs, r)) |> G.e

and map_pipe_rhs_argument (env : env) (x : CST.pipe_rhs_argument) : G.argument =
  match x with
  | `Exp x -> Arg (map_expression env x)
  | `Defa_arg x -> map_default_argument env x
  | `Pipe_plac_arg (v1, v2, v3) ->
      let id = map_identifier env v1 in
      let _teq = (* "=" *) token env v2 in
      let tunderscore = (* "_" *) token env v3 in
      OtherArg (("Id_", tunderscore), [ I id ])

and map_pipe_rhs_arguments (env : env) (xs : CST.pipe_rhs_arguments) :
    G.argument list =
  List_.filter_map
    (fun x ->
      match x with
      | `Pipe_rhs_arg x -> Some (map_pipe_rhs_argument env x)
      | `COMMA tok ->
          let _c = (* "," *) token env tok in
          None)
    xs

and map_program (env : env) (xs : CST.program) : G.program =
  List_.map
    (fun (v1, v2) ->
      let v1 = map_statement env v1 in
      let _v2 =
        match v2 with
        | Some x -> (
            match x with
            | `LF tok -> (* "\n" *) Some (token env tok)
            | `SEMI tok -> (* ";" *) Some (token env tok))
        | None -> None
      in
      v1)
    xs

and map_unary (env : env) (x : CST.unary) : G.expr =
  match x with
  | `Choice_DASH_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `DASH tok -> (Minus, (* "-" *) token env tok)
        | `PLUS tok -> (Plus, (* "+" *) token env tok)
      in
      let v2 = map_expression env v2 in
      G.opcall v1 [ v2 ]
  | `BANG_exp (v1, v2) ->
      let v1 = (* "!" *) token env v1 in
      let v2 = map_expression env v2 in
      G.opcall (Not, v1) [ v2 ]
  | `TILDE_exp (v1, v2) ->
      let v1 = (* "~" *) token env v1 in
      let v2 = map_expression env v2 in
      G.opcall (BitNot, v1) [ v2 ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_r.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      map_program env cst)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_r.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      G.Ss (map_program env cst))

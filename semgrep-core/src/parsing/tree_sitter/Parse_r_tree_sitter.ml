(* Yoann Padioleau
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
module CST = Tree_sitter_r.CST
module H = Parse_tree_sitter_helpers

(*open AST_generic*)
module G = AST_generic

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let todo _env _x = failwith "TODO"

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-r/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the r CST
   to another type of tree.
*)

let map_integer (env : env) (tok : CST.integer) = (* integer *) token env tok

let map_pat_de5d470 (env : env) (tok : CST.pat_de5d470) =
  (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_pat_4ad362e (env : env) (tok : CST.pat_4ad362e) =
  (* pattern [^`\\\n]+|\\\r?\n *) token env tok

let map_raw_string_literal (env : env) (tok : CST.raw_string_literal) =
  (* raw_string_literal *) token env tok

let map_float_ (env : env) (tok : CST.float_) = (* float *) token env tok

let map_na (env : env) (x : CST.na) =
  match x with
  | `NA tok -> (* "NA" *) token env tok
  | `NA_char_ tok -> (* "NA_character_" *) token env tok
  | `NA_comp_ tok -> (* "NA_complex_" *) token env tok
  | `NA_int_ tok -> (* "NA_integer_" *) token env tok
  | `NA_real_ tok -> (* "NA_real_" *) token env tok

let map_pat_3e41275 (env : env) (tok : CST.pat_3e41275) =
  (* pattern [.\p{XID_Start}][._\p{XID_Continue}]* *) token env tok

let map_pat_5e7ac5f (env : env) (tok : CST.pat_5e7ac5f) =
  (* pattern [^%\\\n]+|\\\r?\n *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_pat_3e57880 (env : env) (tok : CST.pat_3e57880) =
  (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_special (env : env) ((v1, v2, v3) : CST.special) =
  let v1 = (* "%" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Pat_5e7ac5f x -> map_pat_5e7ac5f env x
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "%" *) token env v3 in
  todo env (v1, v2, v3)

let map_identifier (env : env) (x : CST.identifier) =
  match x with
  | `Pat_3e41275 x -> map_pat_3e41275 env x
  | `BQUOT_rep_choice_pat_4ad362e_BQUOT (v1, v2, v3) ->
      let v1 = (* "`" *) token env v1 in
      let v2 =
        Common.map
          (fun x ->
            match x with
            | `Pat_4ad362e x -> map_pat_4ad362e env x
            | `Esc_seq tok -> (* escape_sequence *) token env tok)
          v2
      in
      let v3 = (* "`" *) token env v3 in
      todo env (v1, v2, v3)

let map_string_ (env : env) (x : CST.string_) =
  match x with
  | `Raw_str_lit tok -> (* raw_string_literal *) token env tok
  | `DQUOT_rep_choice_pat_de5d470_DQUOT (v1, v2, v3) ->
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        Common.map
          (fun x ->
            match x with
            | `Pat_de5d470 x -> map_pat_de5d470 env x
            | `Esc_seq tok -> (* escape_sequence *) token env tok)
          v2
      in
      let v3 = (* "\"" *) token env v3 in
      todo env (v1, v2, v3)
  | `SQUOT_rep_choice_pat_3e57880_SQUOT (v1, v2, v3) ->
      let v1 = (* "'" *) token env v1 in
      let v2 =
        Common.map
          (fun x ->
            match x with
            | `Pat_3e57880 x -> map_pat_3e57880 env x
            | `Esc_seq tok -> (* escape_sequence *) token env tok)
          v2
      in
      let v3 = (* "'" *) token env v3 in
      todo env (v1, v2, v3)

let rec map_argument (env : env) (x : CST.argument) : G.argument =
  match x with
  | `Exp x ->
      let e = map_expression env x in
      G.Arg e
  | `Defa_arg x -> map_default_argument env x

and map_arguments (env : env) (xs : CST.arguments) =
  Common.map
    (fun x ->
      match x with
      | `Arg x -> map_argument env x
      | `COMMA tok ->
          let _ = (* "," *) token env tok in
          todo env ())
    xs

and map_assignment (env : env) (x : CST.assignment) =
  match x with
  | `Equals_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Left_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<-" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Left_assign2 (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ":=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Right_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Super_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<<-" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Super_right_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "->>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)

and map_binary (env : env) (x : CST.binary) : G.expr =
  match x with
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (* "+" *) token env tok
        | `DASH tok -> (* "-" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `STAR tok -> (* "*" *) token env tok
        | `SLASH tok -> (* "/" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `LT tok -> (* "<" *) token env tok
        | `GT tok -> (* ">" *) token env tok
        | `LTEQ tok -> (* "<=" *) token env tok
        | `GTEQ tok -> (* ">=" *) token env tok
        | `EQEQ tok -> (* "==" *) token env tok
        | `BANGEQ tok -> (* "!=" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `BARBAR tok -> (* "||" *) token env tok
        | `BAR tok -> (* "|" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `AMPAMP tok -> (* "&&" *) token env tok
        | `AMP tok -> (* "&" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_spec_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_special env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_COLON_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_TILDE_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "~" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)

and map_default_argument (env : env) ((v1, v2, v3) : CST.default_argument) =
  let v1 =
    match v1 with
    | `Id x -> map_identifier env x
    | `Str x -> map_string_ env x
    | `Dots tok -> (* "..." *) token env tok
  in
  let v2 = (* "=" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_expression env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Id x ->
      let id = map_identifier env x in
      todo env id
  | `Int tok ->
      let x = (* integer *) token env tok in
      todo env x
  | `Float tok ->
      let x = (* float *) token env tok in
      todo env x
  | `Comp (v1, v2) ->
      let v1 = (* float *) token env v1 in
      let v2 = (* "i" *) token env v2 in
      todo env (v1, v2)
  | `Str x ->
      let x = map_string_ env x in
      todo env x
  | `Call (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_arguments env x
        | None -> todo env ()
      in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Func_defi x ->
      let func = map_function_definition env x in
      todo env func
  | `Lambda_func x ->
      let func = map_lambda_function env x in
      todo env func
  | `Assign x -> map_assignment env x
  | `Brace_list (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = map_program env v2 in
      let v3 = (* "}" *) token env v3 in
      todo env (v1, v2, v3)
  | `Paren_list (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = Common.map (map_expression env) v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `Bin x -> map_binary env x
  | `Un x -> map_unary env x
  | `Pipe (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|>" *) token env v2 in
      let v3 = map_pipe_rhs env v3 in
      todo env (v1, v2, v3)
  | `Subset (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_arguments env x
        | None -> todo env ()
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Subset2 (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "[[" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_arguments env x
        | None -> todo env ()
      in
      let v4 = (* "]]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Dollar (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "$" *) token env v2 in
      let v3 =
        match v3 with
        | `Id x -> map_identifier env x
        | `Str x -> map_string_ env x
      in
      todo env (v1, v2, v3)
  | `Slot (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "@" *) token env v2 in
      let v3 = map_identifier env v3 in
      todo env (v1, v2, v3)
  | `Name_get (v1, v2, v3) ->
      let v1 = map_identifier env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_identifier env v3 in
      todo env (v1, v2, v3)
  | `Name_get_inte (v1, v2, v3) ->
      let v1 = map_identifier env v1 in
      let v2 = (* ":::" *) token env v2 in
      let v3 = map_identifier env v3 in
      todo env (v1, v2, v3)
  | `If (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 =
        match v6 with
        | Some (v1, v2) ->
            let v1 = (* "else" *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `For (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_identifier env v3 in
      let v4 = (* "in" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* ")" *) token env v6 in
      let v7 = map_expression env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `While (v1, v2, v3, v4, v5) ->
      let v1 = (* "while" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Repeat (v1, v2) ->
      let v1 = (* "repeat" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Switch (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "switch" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "," *) token env v4 in
      let v5 = map_arguments env v5 in
      let v6 = (* ")" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Brk tok ->
      let x = (* "break" *) token env tok in
      todo env x
  | `Next tok ->
      let x = (* "next" *) token env tok in
      todo env x
  | `True tok ->
      let x = (* "TRUE" *) token env tok in
      todo env x
  | `False tok ->
      let x = (* "FALSE" *) token env tok in
      todo env x
  | `Null tok ->
      let x = (* "NULL" *) token env tok in
      todo env x
  | `Inf tok ->
      let x = (* "Inf" *) token env tok in
      todo env x
  | `Nan tok ->
      let x = (* "NaN" *) token env tok in
      todo env x
  | `Na x ->
      let x = map_na env x in
      todo env x
  | `Dots tok ->
      let x = (* "..." *) token env tok in
      todo env x

and map_statement (env : env) (x : CST.expression) : G.stmt =
  match x with
  | _ -> todo env ()

and map_formal_parameter (env : env) (x : CST.formal_parameter) : G.parameter =
  match x with
  | `Id x ->
      let id = map_identifier env x in
      todo env id
  | `Defa_param (v1, v2, v3) ->
      let v1 = map_identifier env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Dots tok ->
      let t = (* "..." *) token env tok in
      todo env t

and map_formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_formal_parameter env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_formal_parameter env v2 in
              todo env (v1, v2))
            v2
        in
        let v3 =
          match v3 with
          | Some tok -> (* "," *) token env tok
          | None -> todo env ()
        in
        todo env (v1, v2, v3)
    | None -> todo env ()
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_function_definition (env : env) ((v1, v2, v3) : CST.function_definition)
    : G.function_definition =
  let v1 = (* "function" *) token env v1 in
  let v2 = map_formal_parameters env v2 in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_lambda_function (env : env) ((v1, v2, v3) : CST.lambda_function) :
    G.function_definition =
  let v1 = (* "\\" *) token env v1 in
  let v2 = map_formal_parameters env v2 in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_pipe_rhs (env : env) ((v1, v2, v3, v4) : CST.pipe_rhs) =
  let v1 = map_expression env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_pipe_rhs_arguments env x
    | None -> todo env ()
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_pipe_rhs_argument (env : env) (x : CST.pipe_rhs_argument) =
  match x with
  | `Exp x -> map_expression env x
  | `Defa_arg x ->
      let x = map_default_argument env x in
      todo env x
  | `Pipe_plac_arg (v1, v2, v3) ->
      let v1 = map_identifier env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = (* "_" *) token env v3 in
      todo env (v1, v2, v3)

and map_pipe_rhs_arguments (env : env) (xs : CST.pipe_rhs_arguments) =
  Common.map
    (fun x ->
      match x with
      | `Pipe_rhs_arg x -> map_pipe_rhs_argument env x
      | `COMMA tok ->
          let _c = (* "," *) token env tok in
          todo env ())
    xs

and map_program (env : env) (xs : CST.program) : G.program =
  Common.map
    (fun (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `LF tok -> (* "\n" *) token env tok
            | `SEMI tok -> (* ";" *) token env tok)
        | None -> todo env ()
      in
      todo env (v1, v2))
    xs

and map_unary (env : env) (x : CST.unary) : G.expr =
  match x with
  | `Choice_DASH_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `DASH tok -> (* "-" *) token env tok
        | `PLUS tok -> (* "+" *) token env tok
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `BANG_exp (v1, v2) ->
      let v1 = (* "!" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `TILDE_exp (v1, v2) ->
      let v1 = (* "~" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_r.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      try map_program env cst with
      | Failure "not implemented" as exn ->
          H.debug_sexp_cst_after_error (CST.sexp_of_program cst);
          raise exn)

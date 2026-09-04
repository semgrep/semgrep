(**
   Boilerplate to be used as a template when mapping the lua CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_number (env : env) (tok : CST.number) =
  (* number *) token env tok

let map_global_variable (env : env) (x : CST.global_variable) =
  (match x with
  | `X__G tok -> R.Case ("X__G",
      (* "_G" *) token env tok
    )
  | `X__VERSION tok -> R.Case ("X__VERSION",
      (* "_VERSION" *) token env tok
    )
  )

let map_field_sep (env : env) (x : CST.field_sep) =
  (match x with
  | `COMMA tok -> R.Case ("COMMA",
      (* "," *) token env tok
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env tok

let map_string_ (env : env) (tok : CST.string_) =
  (* string *) token env tok

let map_local_variable_declarator (env : env) ((v1, v2) : CST.local_variable_declarator) =
  let v1 =
    (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 =
          (match v1 with
          | `Self tok -> R.Case ("Self",
              (* "self" *) token env tok
            )
          | `Spread tok -> R.Case ("Spread",
              (* "..." *) token env tok
            )
          | `Id tok -> R.Case ("Id",
              (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env tok
            )
          )
        in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
            in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* "," *) token env v1 in
              let v2 = (* "..." *) token env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_function_name_field (env : env) ((v1, v2) : CST.function_name_field) =
  let v1 =
    (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_function_name (env : env) ((v1, v2) : CST.function_name) =
  let v1 =
    (match v1 with
    | `Id tok -> R.Case ("Id",
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env tok
      )
    | `Func_name_field x -> R.Case ("Func_name_field",
        map_function_name_field env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 =
          (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let rec map_anon_exp_rep_COMMA_exp_0bb260c (env : env) ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_arguments (env : env) (x : CST.arguments) =
  (match x with
  | `LPAR_opt_exp_rep_COMMA_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_exp_rep_COMMA_exp_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_exp_rep_COMMA_exp_0bb260c env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Table x -> R.Case ("Table",
      map_table env x
    )
  | `Str tok -> R.Case ("Str",
      (* string *) token env tok
    )
  )

and map_binary_operation (env : env) (x : CST.binary_operation) =
  (match x with
  | `Exp_or_exp (v1, v2, v3) -> R.Case ("Exp_or_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "or" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_and_exp (v1, v2, v3) -> R.Case ("Exp_and_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_TILDEEQ_exp (v1, v2, v3) -> R.Case ("Exp_TILDEEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "~=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_TILDE_exp (v1, v2, v3) -> R.Case ("Exp_TILDE_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "~" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASH_exp (v1, v2, v3) -> R.Case ("Exp_DASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAR_exp (v1, v2, v3) -> R.Case ("Exp_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASHSLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASHSLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "//" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DOTDOT_exp (v1, v2, v3) -> R.Case ("Exp_DOTDOT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ".." *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_else_ (env : env) ((v1, v2, v3) : CST.else_) =
  let v1 = (* "else" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_return_statement env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_elseif (env : env) ((v1, v2, v3, v4, v5) : CST.elseif) =
  let v1 = (* "elseif" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "then" *) token env v3 in
  let v4 = R.List (List.map (map_statement env) v4) in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_return_statement env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Spread tok -> R.Case ("Spread",
      (* "..." *) token env tok
    )
  | `Prefix x -> R.Case ("Prefix",
      map_prefix env x
    )
  | `Next tok -> R.Case ("Next",
      (* "next" *) token env tok
    )
  | `Func_defi (v1, v2) -> R.Case ("Func_defi",
      let v1 = (* "function" *) token env v1 in
      let v2 = map_function_body env v2 in
      R.Tuple [v1; v2]
    )
  | `Table x -> R.Case ("Table",
      map_table env x
    )
  | `Bin_oper x -> R.Case ("Bin_oper",
      map_binary_operation env x
    )
  | `Un_oper (v1, v2) -> R.Case ("Un_oper",
      let v1 =
        (match v1 with
        | `Not tok -> R.Case ("Not",
            (* "not" *) token env tok
          )
        | `HASH tok -> R.Case ("HASH",
            (* "#" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `TILDE tok -> R.Case ("TILDE",
            (* "~" *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Str tok -> R.Case ("Str",
      (* string *) token env tok
    )
  | `Num tok -> R.Case ("Num",
      (* number *) token env tok
    )
  | `Nil tok -> R.Case ("Nil",
      (* "nil" *) token env tok
    )
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env tok
    )
  )

and map_field (env : env) (x : CST.field) =
  (match x with
  | `LBRACK_exp_RBRACK_EQ_exp (v1, v2, v3, v4, v5) -> R.Case ("LBRACK_exp_RBRACK_EQ_exp",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "]" *) token env v3 in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Id_EQ_exp (v1, v2, v3) -> R.Case ("Id_EQ_exp",
      let v1 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_field_sequence (env : env) ((v1, v2, v3) : CST.field_sequence) =
  let v1 = map_field env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_field_sep env v1 in
      let v2 = map_field env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_field_sep env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_function_body (env : env) ((v1, v2, v3, v4) : CST.function_body) =
  let v1 = map_parameters env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_return_statement env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "end" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_function_call_statement (env : env) (x : CST.function_call_statement) =
  (match x with
  | `Prefix_args (v1, v2) -> R.Case ("Prefix_args",
      let v1 = map_prefix env v1 in
      let v2 = map_arguments env v2 in
      R.Tuple [v1; v2]
    )
  | `Prefix_COLON_id_args (v1, v2, v3, v4) -> R.Case ("Prefix_COLON_id_args",
      let v1 = map_prefix env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v3
      in
      let v4 = map_arguments env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_in_loop_expression (env : env) ((v1, v2, v3, v4, v5) : CST.in_loop_expression) =
  let v1 =
    (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = (* "in" *) token env v3 in
  let v4 = map_expression env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_loop_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.loop_expression) =
  let v1 =
    (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v1
  in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "," *) token env v4 in
  let v5 = map_expression env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_prefix (env : env) (x : CST.prefix) =
  (match x with
  | `Self tok -> R.Case ("Self",
      (* "self" *) token env tok
    )
  | `Global_var x -> R.Case ("Global_var",
      map_global_variable env x
    )
  | `Var_decl x -> R.Case ("Var_decl",
      map_variable_declarator env x
    )
  | `Func_call_stmt x -> R.Case ("Func_call_stmt",
      map_function_call_statement env x
    )
  | `LPAR_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_exp_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_return_statement (env : env) ((v1, v2, v3) : CST.return_statement) =
  let v1 = (* "return" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_exp_rep_COMMA_exp_0bb260c env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Var_decl (v1, v2, v3, v4, v5) -> R.Case ("Var_decl",
      let v1 = map_variable_declarator env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_variable_declarator env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_expression env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Local_var_decl (v1, v2, v3) -> R.Case ("Local_var_decl",
      let v1 = (* "local" *) token env v1 in
      let v2 = map_local_variable_declarator env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            let v3 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_expression env v2 in
                R.Tuple [v1; v2]
              ) v3)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Do_stmt (v1, v2, v3, v4) -> R.Case ("Do_stmt",
      let v1 = (* "do" *) token env v1 in
      let v2 = R.List (List.map (map_statement env) v2) in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_return_statement env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "end" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `If_stmt (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("If_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "then" *) token env v3 in
      let v4 = R.List (List.map (map_statement env) v4) in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_return_statement env x
          ))
        | None -> R.Option None)
      in
      let v6 = R.List (List.map (map_elseif env) v6) in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            map_else_ env x
          ))
        | None -> R.Option None)
      in
      let v8 = (* "end" *) token env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `While_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "do" *) token env v3 in
      let v4 = R.List (List.map (map_statement env) v4) in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_return_statement env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* "end" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Repeat_stmt (v1, v2, v3, v4, v5) -> R.Case ("Repeat_stmt",
      let v1 = (* "repeat" *) token env v1 in
      let v2 = R.List (List.map (map_statement env) v2) in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_return_statement env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "until" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `For_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("For_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = map_loop_expression env v2 in
      let v3 = (* "do" *) token env v3 in
      let v4 = R.List (List.map (map_statement env) v4) in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_return_statement env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* "end" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `For_in_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("For_in_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = map_in_loop_expression env v2 in
      let v3 = (* "do" *) token env v3 in
      let v4 = R.List (List.map (map_statement env) v4) in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_return_statement env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* "end" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Goto_stmt (v1, v2) -> R.Case ("Goto_stmt",
      let v1 = (* "goto" *) token env v1 in
      let v2 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
      in
      R.Tuple [v1; v2]
    )
  | `Brk_stmt tok -> R.Case ("Brk_stmt",
      (* "break" *) token env tok
    )
  | `Label_stmt (v1, v2, v3) -> R.Case ("Label_stmt",
      let v1 = (* "::" *) token env v1 in
      let v2 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
      in
      let v3 = (* "::" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Empty_stmt tok -> R.Case ("Empty_stmt",
      (* ";" *) token env tok
    )
  | `Func_stmt (v1, v2, v3) -> R.Case ("Func_stmt",
      let v1 = (* "function" *) token env v1 in
      let v2 = map_function_name env v2 in
      let v3 = map_function_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Local_func_stmt (v1, v2, v3, v4) -> R.Case ("Local_func_stmt",
      let v1 = (* "local" *) token env v1 in
      let v2 = (* "function" *) token env v2 in
      let v3 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v3
      in
      let v4 = map_function_body env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Func_call_stmt x -> R.Case ("Func_call_stmt",
      map_function_call_statement env x
    )
  )

and map_table (env : env) ((v1, v2, v3) : CST.table) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_field_sequence env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_variable_declarator (env : env) (x : CST.variable_declarator) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env tok
    )
  | `Prefix_LBRACK_exp_RBRACK (v1, v2, v3, v4) -> R.Case ("Prefix_LBRACK_exp_RBRACK",
      let v1 = map_prefix env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Field_exp (v1, v2, v3) -> R.Case ("Field_exp",
      let v1 = map_prefix env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 = R.List (List.map (map_statement env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_return_statement env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_program () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras

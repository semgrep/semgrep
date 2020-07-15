module CST = Tree_sitter_go.CST
[@@@warning "-32"]

let parse _file =
  raise Common.Todo

(* This was started by copying ocaml-tree-sitter-lang/go/.../Boilerplate.ml *)
(**
   Boilerplate to be used as a template when mapping the go CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]


type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_int_literal (env : env) (tok : CST.int_literal) =
  token env tok (* int_literal *)

let map_identifier (env : env) (tok : CST.identifier) =
  token env tok (* identifier *)

let map_raw_string_literal (env : env) (tok : CST.raw_string_literal) =
  token env tok (* raw_string_literal *)

let map_rune_literal (env : env) (tok : CST.rune_literal) =
  token env tok (* rune_literal *)

let map_float_literal (env : env) (tok : CST.float_literal) =
  token env tok (* float_literal *)

let map_imaginary_literal (env : env) (tok : CST.imaginary_literal) =
  token env tok (* imaginary_literal *)

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  token env tok (* escape_sequence *)

let map_qualified_type (env : env) ((v1, v2, v3) : CST.qualified_type) =
  let v1 = token env v1 (* identifier *) in
  let v2 = token env v2 (* "." *) in
  let v3 = token env v3 (* identifier *) in
  todo env (v1, v2, v3)

let map_package_clause (env : env) ((v1, v2) : CST.package_clause) =
  let v1 = token env v1 (* "package" *) in
  let v2 = token env v2 (* identifier *) in
  todo env (v1, v2)

let map_empty_labeled_statement (env : env) ((v1, v2) : CST.empty_labeled_statement) =
  let v1 = token env v1 (* identifier *) in
  let v2 = token env v2 (* ":" *) in
  todo env (v1, v2)

let map_interpreted_string_literal (env : env) ((v1, v2, v3) : CST.interpreted_string_literal) =
  let v1 = token env v1 (* "\"" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Blank () -> todo env ()
      | `Esc_seq tok -> token env tok (* escape_sequence *)
      )
    ) v2
  in
  let v3 = token env v3 (* "\"" *) in
  todo env (v1, v2, v3)

let map_import_spec (env : env) ((v1, v2) : CST.import_spec) =
  let v1 =
    (match v1 with
    | Some x ->
        (match x with
        | `Dot tok -> token env tok (* "." *)
        | `Blank_id tok -> token env tok (* "_" *)
        | `Id tok -> token env tok (* identifier *)
        )
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Raw_str_lit tok -> token env tok (* raw_string_literal *)
    | `Inte_str_lit x -> map_interpreted_string_literal env x
    )
  in
  todo env (v1, v2)

let rec map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Decl_const_decl (v1, v2) ->
      let v1 = token env v1 (* "const" *) in
      let v2 =
        (match v2 with
        | `Const_spec x -> map_const_spec env x
        | `LPAR_rep_const_spec_choice_LF_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = map_const_spec env v1 in
                let v2 =
                  (match v2 with
                  | `LF tok -> token env tok (* "\n" *)
                  | `SEMI tok -> token env tok (* ";" *)
                  )
                in
                todo env (v1, v2)
              ) v2
            in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2)
  | `Decl_type_decl (v1, v2) ->
      let v1 = token env v1 (* "type" *) in
      let v2 =
        (match v2 with
        | `Type_spec x -> map_type_spec env x
        | `Type_alias x -> map_type_alias env x
        | `LPAR_rep_choice_type_spec_choice_LF_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 =
                  (match v1 with
                  | `Type_spec x -> map_type_spec env x
                  | `Type_alias x -> map_type_alias env x
                  )
                in
                let v2 =
                  (match v2 with
                  | `LF tok -> token env tok (* "\n" *)
                  | `SEMI tok -> token env tok (* ";" *)
                  )
                in
                todo env (v1, v2)
              ) v2
            in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2)
  | `Decl_var_decl (v1, v2) ->
      let v1 = token env v1 (* "var" *) in
      let v2 =
        (match v2 with
        | `Var_spec x -> map_var_spec env x
        | `LPAR_rep_var_spec_choice_LF_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = map_var_spec env v1 in
                let v2 =
                  (match v2 with
                  | `LF tok -> token env tok (* "\n" *)
                  | `SEMI tok -> token env tok (* ";" *)
                  )
                in
                todo env (v1, v2)
              ) v2
            in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2)
  )


and map_const_spec (env : env) ((v1, v2, v3) : CST.const_spec) =
  let v1 = token env v1 (* identifier *) in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = token env v2 (* identifier *) in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | Some x ->
              (match x with
              | `Simple_type x -> map_simple_type env x
              | `Paren_type x -> map_parenthesized_type env x
              )
          | None -> todo env ())
        in
        let v2 = token env v2 (* "=" *) in
        let v3 = map_expression_list env v3 in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)


and map_var_spec (env : env) ((v1, v2, v3) : CST.var_spec) =
  let v1 = token env v1 (* identifier *) in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = token env v2 (* identifier *) in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | `Choice_simple_type_opt_EQ_exp_list (v1, v2) ->
        let v1 =
          (match v1 with
          | `Simple_type x -> map_simple_type env x
          | `Paren_type x -> map_parenthesized_type env x
          )
        in
        let v2 =
          (match v2 with
          | Some (v1, v2) ->
              let v1 = token env v1 (* "=" *) in
              let v2 = map_expression_list env v2 in
              todo env (v1, v2)
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `EQ_exp_list (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression_list env v2 in
        todo env (v1, v2)
    )
  in
  todo env (v1, v2, v3)


and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some (v1, v2) ->
              let v1 =
                (match v1 with
                | `Param_decl x -> map_parameter_declaration env x
                | `Vari_param_decl x ->
                    map_variadic_parameter_declaration env x
                )
              in
              let v2 =
                List.map (fun (v1, v2) ->
                  let v1 = token env v1 (* "," *) in
                  let v2 =
                    (match v2 with
                    | `Param_decl x -> map_parameter_declaration env x
                    | `Vari_param_decl x ->
                        map_variadic_parameter_declaration env x
                    )
                  in
                  todo env (v1, v2)
                ) v2
              in
              todo env (v1, v2)
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | Some tok -> token env tok (* "," *)
          | None -> todo env ())
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)


and map_parameter_declaration (env : env) ((v1, v2) : CST.parameter_declaration) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* identifier *) in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = token env v2 (* identifier *) in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Simple_type x -> map_simple_type env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  todo env (v1, v2)


and map_variadic_parameter_declaration (env : env) ((v1, v2, v3) : CST.variadic_parameter_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* identifier *)
    | None -> todo env ())
  in
  let v2 = token env v2 (* "..." *) in
  let v3 =
    (match v3 with
    | `Simple_type x -> map_simple_type env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  todo env (v1, v2, v3)


and map_type_alias (env : env) ((v1, v2, v3) : CST.type_alias) =
  let v1 = token env v1 (* identifier *) in
  let v2 = token env v2 (* "=" *) in
  let v3 =
    (match v3 with
    | `Simple_type x -> map_simple_type env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  todo env (v1, v2, v3)


and map_type_spec (env : env) ((v1, v2) : CST.type_spec) =
  let v1 = token env v1 (* identifier *) in
  let v2 =
    (match v2 with
    | `Simple_type x -> map_simple_type env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  todo env (v1, v2)


and map_expression_list (env : env) ((v1, v2) : CST.expression_list) =
  let v1 = map_expression env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)


and map_parenthesized_type (env : env) ((v1, v2, v3) : CST.parenthesized_type) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | `Simple_type x -> map_simple_type env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)


and map_simple_type (env : env) (x : CST.simple_type) =
  (match x with
  | `Simple_type_id tok -> token env tok (* identifier *)
  | `Simple_type_qual_type x -> map_qualified_type env x
  | `Simple_type_poin_type (v1, v2) ->
      let v1 = token env v1 (* "*" *) in
      let v2 =
        (match v2 with
        | `Simple_type x -> map_simple_type env x
        | `Paren_type x -> map_parenthesized_type env x
        )
      in
      todo env (v1, v2)
  | `Simple_type_stru_type x -> map_struct_type env x
  | `Simple_type_inte_type (v1, v2) ->
      let v1 = token env v1 (* "interface" *) in
      let v2 = map_method_spec_list env v2 in
      todo env (v1, v2)
  | `Simple_type_array_type x -> map_array_type env x
  | `Simple_type_slice_type x -> map_slice_type env x
  | `Simple_type_map_type x -> map_map_type env x
  | `Simple_type_chan_type x -> map_channel_type env x
  | `Simple_type_func_type (v1, v2, v3) ->
      let v1 = token env v1 (* "func" *) in
      let v2 = map_parameter_list env v2 in
      let v3 =
        (match v3 with
        | Some x ->
            (match x with
            | `Param_list x -> map_parameter_list env x
            | `Simple_type x -> map_simple_type env x
            )
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  )


and map_array_type (env : env) ((v1, v2, v3, v4) : CST.array_type) =
  let v1 = token env v1 (* "[" *) in
  let v2 = map_expression env v2 in
  let v3 = token env v3 (* "]" *) in
  let v4 =
    (match v4 with
    | `Simple_type x -> map_simple_type env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  todo env (v1, v2, v3, v4)


and map_implicit_length_array_type (env : env) ((v1, v2, v3, v4) : CST.implicit_length_array_type) =
  let v1 = token env v1 (* "[" *) in
  let v2 = token env v2 (* "..." *) in
  let v3 = token env v3 (* "]" *) in
  let v4 =
    (match v4 with
    | `Simple_type x -> map_simple_type env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  todo env (v1, v2, v3, v4)


and map_slice_type (env : env) ((v1, v2, v3) : CST.slice_type) =
  let v1 = token env v1 (* "[" *) in
  let v2 = token env v2 (* "]" *) in
  let v3 =
    (match v3 with
    | `Simple_type x -> map_simple_type env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  todo env (v1, v2, v3)


and map_struct_type (env : env) ((v1, v2) : CST.struct_type) =
  let v1 = token env v1 (* "struct" *) in
  let v2 = map_field_declaration_list env v2 in
  todo env (v1, v2)


and map_field_declaration_list (env : env) ((v1, v2, v3) : CST.field_declaration_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_field_declaration env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | `LF tok -> token env tok (* "\n" *)
              | `SEMI tok -> token env tok (* ";" *)
              )
            in
            let v2 = map_field_declaration env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some x ->
              (match x with
              | `LF tok -> token env tok (* "\n" *)
              | `SEMI tok -> token env tok (* ";" *)
              )
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and map_field_declaration (env : env) ((v1, v2) : CST.field_declaration) =
  let v1 =
    (match v1 with
    | `Id_rep_COMMA_id_choice_simple_type (v1, v2, v3) ->
        let v1 = token env v1 (* identifier *) in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = token env v2 (* identifier *) in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | `Simple_type x -> map_simple_type env x
          | `Paren_type x -> map_parenthesized_type env x
          )
        in
        todo env (v1, v2, v3)
    | `Opt_STAR_choice_id (v1, v2) ->
        let v1 =
          (match v1 with
          | Some tok -> token env tok (* "*" *)
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | `Id tok -> token env tok (* identifier *)
          | `Qual_type x -> map_qualified_type env x
          )
        in
        todo env (v1, v2)
    )
  in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Raw_str_lit tok -> token env tok (* raw_string_literal *)
        | `Inte_str_lit x -> map_interpreted_string_literal env x
        )
    | None -> todo env ())
  in
  todo env (v1, v2)


and map_method_spec_list (env : env) ((v1, v2, v3) : CST.method_spec_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | `Id tok -> token env tok (* identifier *)
          | `Qual_type x -> map_qualified_type env x
          | `Meth_spec x -> map_method_spec env x
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | `LF tok -> token env tok (* "\n" *)
              | `SEMI tok -> token env tok (* ";" *)
              )
            in
            let v2 =
              (match v2 with
              | `Id tok -> token env tok (* identifier *)
              | `Qual_type x -> map_qualified_type env x
              | `Meth_spec x -> map_method_spec env x
              )
            in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some x ->
              (match x with
              | `LF tok -> token env tok (* "\n" *)
              | `SEMI tok -> token env tok (* ";" *)
              )
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and map_method_spec (env : env) ((v1, v2, v3) : CST.method_spec) =
  let v1 = token env v1 (* identifier *) in
  let v2 = map_parameter_list env v2 in
  let v3 =
    (match v3 with
    | Some x ->
        (match x with
        | `Param_list x -> map_parameter_list env x
        | `Simple_type x -> map_simple_type env x
        )
    | None -> todo env ())
  in
  todo env (v1, v2, v3)


and map_map_type (env : env) ((v1, v2, v3, v4, v5) : CST.map_type) =
  let v1 = token env v1 (* "map" *) in
  let v2 = token env v2 (* "[" *) in
  let v3 =
    (match v3 with
    | `Simple_type x -> map_simple_type env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  let v4 = token env v4 (* "]" *) in
  let v5 =
    (match v5 with
    | `Simple_type x -> map_simple_type env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  todo env (v1, v2, v3, v4, v5)


and map_channel_type (env : env) (x : CST.channel_type) =
  (match x with
  | `Chan_type_chan_choice_simple_type (v1, v2) ->
      let v1 = token env v1 (* "chan" *) in
      let v2 =
        (match v2 with
        | `Simple_type x -> map_simple_type env x
        | `Paren_type x -> map_parenthesized_type env x
        )
      in
      todo env (v1, v2)
  | `Chan_type_chan_LTDASH_choice_simple_type (v1, v2, v3) ->
      let v1 = token env v1 (* "chan" *) in
      let v2 = token env v2 (* "<-" *) in
      let v3 =
        (match v3 with
        | `Simple_type x -> map_simple_type env x
        | `Paren_type x -> map_parenthesized_type env x
        )
      in
      todo env (v1, v2, v3)
  | `Chan_type_LTDASH_chan_choice_simple_type (v1, v2, v3) ->
      let v1 = token env v1 (* "<-" *) in
      let v2 = token env v2 (* "chan" *) in
      let v3 =
        (match v3 with
        | `Simple_type x -> map_simple_type env x
        | `Paren_type x -> map_parenthesized_type env x
        )
      in
      todo env (v1, v2, v3)
  )


and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> map_statement_list env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and map_statement_list (env : env) (x : CST.statement_list) =
  (match x with
  | `Stmt_list_stmt_rep_choice_LF_stmt_opt_choice_LF_opt_empty_labe_stmt (v1, v2, v3) ->
      let v1 = map_statement env v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let v1 =
            (match v1 with
            | `LF tok -> token env tok (* "\n" *)
            | `SEMI tok -> token env tok (* ";" *)
            )
          in
          let v2 = map_statement env v2 in
          todo env (v1, v2)
        ) v2
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 =
              (match v1 with
              | `LF tok -> token env tok (* "\n" *)
              | `SEMI tok -> token env tok (* ";" *)
              )
            in
            let v2 =
              (match v2 with
              | Some x -> map_empty_labeled_statement env x
              | None -> todo env ())
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Stmt_list_empty_labe_stmt x ->
      map_empty_labeled_statement env x
  )


and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Stmt_decl x -> map_declaration env x
  | `Stmt_simple_stmt x -> map_simple_statement env x
  | `Stmt_ret_stmt (v1, v2) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        (match v2 with
        | Some x -> map_expression_list env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Stmt_go_stmt (v1, v2) ->
      let v1 = token env v1 (* "go" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Stmt_defer_stmt (v1, v2) ->
      let v1 = token env v1 (* "defer" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Stmt_if_stmt x -> map_if_statement env x
  | `Stmt_for_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "for" *) in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Exp x -> map_expression env x
            | `For_clau x -> map_for_clause env x
            | `Range_clau x -> map_range_clause env x
            )
        | None -> todo env ())
      in
      let v3 = map_block env v3 in
      todo env (v1, v2, v3)
  | `Stmt_exp_swit_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = map_simple_statement env v1 in
            let v2 = token env v2 (* ";" *) in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "{" *) in
      let v5 =
        List.map (fun x ->
          (match x with
          | `Exp_case x -> map_expression_case env x
          | `Defa_case x -> map_default_case env x
          )
        ) v5
      in
      let v6 = token env v6 (* "}" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Stmt_type_swit_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = map_type_switch_header env v2 in
      let v3 = token env v3 (* "{" *) in
      let v4 =
        List.map (fun x ->
          (match x with
          | `Type_case x -> map_type_case env x
          | `Defa_case x -> map_default_case env x
          )
        ) v4
      in
      let v5 = token env v5 (* "}" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Stmt_sele_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "select" *) in
      let v2 = token env v2 (* "{" *) in
      let v3 =
        List.map (fun x ->
          (match x with
          | `Comm_case x -> map_communication_case env x
          | `Defa_case x -> map_default_case env x
          )
        ) v3
      in
      let v4 = token env v4 (* "}" *) in
      todo env (v1, v2, v3, v4)
  | `Stmt_labe_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* identifier *) in
      let v2 = token env v2 (* ":" *) in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `Stmt_fall_stmt tok -> token env tok (* "fallthrough" *)
  | `Stmt_brk_stmt (v1, v2) ->
      let v1 = token env v1 (* "break" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* identifier *)
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Stmt_cont_stmt (v1, v2) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* identifier *)
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Stmt_goto_stmt (v1, v2) ->
      let v1 = token env v1 (* "goto" *) in
      let v2 = token env v2 (* identifier *) in
      todo env (v1, v2)
  | `Stmt_blk x -> map_block env x
  | `Stmt_empty_stmt tok -> token env tok (* ";" *)
  )


and map_simple_statement (env : env) (x : CST.simple_statement) =
  (match x with
  | `Simple_stmt_exp x -> map_expression env x
  | `Simple_stmt_send_stmt x -> map_send_statement env x
  | `Simple_stmt_inc_stmt (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "++" *) in
      todo env (v1, v2)
  | `Simple_stmt_dec_stmt (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "--" *) in
      todo env (v1, v2)
  | `Simple_stmt_assign_stmt (v1, v2, v3) ->
      let v1 = map_expression_list env v1 in
      let v2 =
        (match v2 with
        | `STAREQ tok -> token env tok (* "*=" *)
        | `SLASHEQ tok -> token env tok (* "/=" *)
        | `PERCEQ tok -> token env tok (* "%=" *)
        | `LTLTEQ tok -> token env tok (* "<<=" *)
        | `GTGTEQ tok -> token env tok (* ">>=" *)
        | `AMPEQ tok -> token env tok (* "&=" *)
        | `AMPHATEQ tok -> token env tok (* "&^=" *)
        | `PLUSEQ tok -> token env tok (* "+=" *)
        | `DASHEQ tok -> token env tok (* "-=" *)
        | `BAREQ tok -> token env tok (* "|=" *)
        | `HATEQ tok -> token env tok (* "^=" *)
        | `EQ tok -> token env tok (* "=" *)
        )
      in
      let v3 = map_expression_list env v3 in
      todo env (v1, v2, v3)
  | `Simple_stmt_short_var_decl (v1, v2, v3) ->
      let v1 = map_expression_list env v1 in
      let v2 = token env v2 (* ":=" *) in
      let v3 = map_expression_list env v3 in
      todo env (v1, v2, v3)
  )


and map_send_statement (env : env) ((v1, v2, v3) : CST.send_statement) =
  let v1 = map_expression env v1 in
  let v2 = token env v2 (* "<-" *) in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)


and map_receive_statement (env : env) ((v1, v2) : CST.receive_statement) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = map_expression_list env v1 in
        let v2 =
          (match v2 with
          | `EQ tok -> token env tok (* "=" *)
          | `COLONEQ tok -> token env tok (* ":=" *)
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = map_expression env v2 in
  todo env (v1, v2)


and map_if_statement (env : env) ((v1, v2, v3, v4, v5) : CST.if_statement) =
  let v1 = token env v1 (* "if" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_simple_statement env v1 in
        let v2 = token env v2 (* ";" *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = map_expression env v3 in
  let v4 = map_block env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "else" *) in
        let v2 =
          (match v2 with
          | `Blk x -> map_block env x
          | `If_stmt x -> map_if_statement env x
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)


and map_for_clause (env : env) ((v1, v2, v3, v4, v5) : CST.for_clause) =
  let v1 =
    (match v1 with
    | Some x -> map_simple_statement env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* ";" *) in
  let v3 =
    (match v3 with
    | Some x -> map_expression env x
    | None -> todo env ())
  in
  let v4 = token env v4 (* ";" *) in
  let v5 =
    (match v5 with
    | Some x -> map_simple_statement env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)


and map_range_clause (env : env) ((v1, v2, v3) : CST.range_clause) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = map_expression_list env v1 in
        let v2 =
          (match v2 with
          | `EQ tok -> token env tok (* "=" *)
          | `COLONEQ tok -> token env tok (* ":=" *)
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = token env v2 (* "range" *) in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)


and map_expression_case (env : env) ((v1, v2, v3, v4) : CST.expression_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 = map_expression_list env v2 in
  let v3 = token env v3 (* ":" *) in
  let v4 =
    (match v4 with
    | Some x -> map_statement_list env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)


and map_default_case (env : env) ((v1, v2, v3) : CST.default_case) =
  let v1 = token env v1 (* "default" *) in
  let v2 = token env v2 (* ":" *) in
  let v3 =
    (match v3 with
    | Some x -> map_statement_list env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)


and map_type_switch_header (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.type_switch_header) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = map_simple_statement env v1 in
        let v2 = token env v2 (* ";" *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_expression_list env v1 in
        let v2 = token env v2 (* ":=" *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = map_expression env v3 in
  let v4 = token env v4 (* "." *) in
  let v5 = token env v5 (* "(" *) in
  let v6 = token env v6 (* "type" *) in
  let v7 = token env v7 (* ")" *) in
  todo env (v1, v2, v3, v4, v5, v6, v7)


and map_type_case (env : env) ((v1, v2, v3, v4, v5) : CST.type_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 =
    (match v2 with
    | `Simple_type x -> map_simple_type env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 =
        (match v2 with
        | `Simple_type x -> map_simple_type env x
        | `Paren_type x -> map_parenthesized_type env x
        )
      in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* ":" *) in
  let v5 =
    (match v5 with
    | Some x -> map_statement_list env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)


and map_communication_case (env : env) ((v1, v2, v3, v4) : CST.communication_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 =
    (match v2 with
    | `Send_stmt x -> map_send_statement env x
    | `Rece_stmt x -> map_receive_statement env x
    )
  in
  let v3 = token env v3 (* ":" *) in
  let v4 =
    (match v4 with
    | Some x -> map_statement_list env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)


and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Exp_un_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `PLUS tok -> token env tok (* "+" *)
        | `DASH tok -> token env tok (* "-" *)
        | `BANG tok -> token env tok (* "!" *)
        | `HAT tok -> token env tok (* "^" *)
        | `STAR tok -> token env tok (* "*" *)
        | `AMP tok -> token env tok (* "&" *)
        | `LTDASH tok -> token env tok (* "<-" *)
        )
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Exp_bin_exp x -> map_binary_expression env x
  | `Exp_sele_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* identifier *) in
      todo env (v1, v2, v3)
  | `Exp_index_exp (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Exp_slice_exp (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 =
        (match v3 with
        | `Opt_exp_COLON_opt_exp (v1, v2, v3) ->
            let v1 =
              (match v1 with
              | Some x -> map_expression env x
              | None -> todo env ())
            in
            let v2 = token env v2 (* ":" *) in
            let v3 =
              (match v3 with
              | Some x -> map_expression env x
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | `Opt_exp_COLON_exp_COLON_exp (v1, v2, v3, v4, v5) ->
            let v1 =
              (match v1 with
              | Some x -> map_expression env x
              | None -> todo env ())
            in
            let v2 = token env v2 (* ":" *) in
            let v3 = map_expression env v3 in
            let v4 = token env v4 (* ":" *) in
            let v5 = map_expression env v5 in
            todo env (v1, v2, v3, v4, v5)
        )
      in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Exp_call_exp x -> map_call_expression env x
  | `Exp_type_asse_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* "(" *) in
      let v4 =
        (match v4 with
        | `Simple_type x -> map_simple_type env x
        | `Paren_type x -> map_parenthesized_type env x
        )
      in
      let v5 = token env v5 (* ")" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Exp_type_conv_exp (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | `Simple_type x -> map_simple_type env x
        | `Paren_type x -> map_parenthesized_type env x
        )
      in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_expression env v3 in
      let v4 =
        (match v4 with
        | Some tok -> token env tok (* "," *)
        | None -> todo env ())
      in
      let v5 = token env v5 (* ")" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Exp_id tok -> token env tok (* identifier *)
  | `Exp_choice_new x ->
      (match x with
      | `New tok -> token env tok (* "new" *)
      | `Make tok -> token env tok (* "make" *)
      )
  | `Exp_comp_lit (v1, v2) ->
      let v1 =
        (match v1 with
        | `Map_type x -> map_map_type env x
        | `Slice_type x -> map_slice_type env x
        | `Array_type x -> map_array_type env x
        | `Impl_len_array_type x ->
            map_implicit_length_array_type env x
        | `Stru_type x -> map_struct_type env x
        | `Id tok -> token env tok (* identifier *)
        | `Qual_type x -> map_qualified_type env x
        )
      in
      let v2 = map_literal_value env v2 in
      todo env (v1, v2)
  | `Exp_func_lit (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "func" *) in
      let v2 = map_parameter_list env v2 in
      let v3 =
        (match v3 with
        | Some x ->
            (match x with
            | `Param_list x -> map_parameter_list env x
            | `Simple_type x -> map_simple_type env x
            )
        | None -> todo env ())
      in
      let v4 = map_block env v4 in
      todo env (v1, v2, v3, v4)
  | `Exp_choice_raw_str_lit x ->
      (match x with
      | `Raw_str_lit tok -> token env tok (* raw_string_literal *)
      | `Inte_str_lit x -> map_interpreted_string_literal env x
      )
  | `Exp_int_lit tok -> token env tok (* int_literal *)
  | `Exp_float_lit tok -> token env tok (* float_literal *)
  | `Exp_imag_lit tok -> token env tok (* imaginary_literal *)
  | `Exp_rune_lit tok -> token env tok (* rune_literal *)
  | `Exp_nil tok -> token env tok (* "nil" *)
  | `Exp_true tok -> token env tok (* "true" *)
  | `Exp_false tok -> token env tok (* "false" *)
  | `Exp_paren_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  )


and map_call_expression (env : env) (x : CST.call_expression) =
  (match x with
  | `Choice_new_spec_arg_list (v1, v2) ->
      let v1 =
        (match v1 with
        | `New tok -> token env tok (* "new" *)
        | `Make tok -> token env tok (* "make" *)
        )
      in
      let v2 = map_special_argument_list env v2 in
      todo env (v1, v2)
  | `Exp_arg_list (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_argument_list env v2 in
      todo env (v1, v2)
  )


and map_variadic_argument (env : env) ((v1, v2) : CST.variadic_argument) =
  let v1 = map_expression env v1 in
  let v2 = token env v2 (* "..." *) in
  todo env (v1, v2)


and map_special_argument_list (env : env) ((v1, v2, v3, v4, v5) : CST.special_argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | `Simple_type x -> map_simple_type env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v5 = token env v5 (* ")" *) in
  todo env (v1, v2, v3, v4, v5)


and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | `Exp x -> map_expression env x
          | `Vari_arg x -> map_variadic_argument env x
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | `Exp x -> map_expression env x
              | `Vari_arg x -> map_variadic_argument env x
              )
            in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> token env tok (* "," *)
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)


and map_literal_value (env : env) ((v1, v2, v3) : CST.literal_value) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | `Elem x -> map_element env x
          | `Keyed_elem x -> map_keyed_element env x
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | `Elem x -> map_element env x
              | `Keyed_elem x -> map_keyed_element env x
              )
            in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> token env tok (* "," *)
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and map_keyed_element (env : env) ((v1, v2) : CST.keyed_element) =
  let v1 =
    (match v1 with
    | `Exp_COLON (v1, v2) ->
        let v1 = map_expression env v1 in
        let v2 = token env v2 (* ":" *) in
        todo env (v1, v2)
    | `Lit_value_COLON (v1, v2) ->
        let v1 = map_literal_value env v1 in
        let v2 = token env v2 (* ":" *) in
        todo env (v1, v2)
    | `Id_COLON (v1, v2) ->
        let v1 = token env v1 (* identifier *) in
        let v2 = token env v2 (* ":" *) in
        todo env (v1, v2)
    )
  in
  let v2 =
    (match v2 with
    | `Exp x -> map_expression env x
    | `Lit_value x -> map_literal_value env x
    )
  in
  todo env (v1, v2)


and map_element (env : env) (x : CST.element) =
  (match x with
  | `Elem_exp x -> map_expression env x
  | `Elem_lit_value x -> map_literal_value env x
  )


and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Bin_exp_exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `STAR tok -> token env tok (* "*" *)
        | `SLASH tok -> token env tok (* "/" *)
        | `PERC tok -> token env tok (* "%" *)
        | `LTLT tok -> token env tok (* "<<" *)
        | `GTGT tok -> token env tok (* ">>" *)
        | `AMP tok -> token env tok (* "&" *)
        | `AMPHAT tok -> token env tok (* "&^" *)
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> token env tok (* "+" *)
        | `DASH tok -> token env tok (* "-" *)
        | `BAR tok -> token env tok (* "|" *)
        | `HAT tok -> token env tok (* "^" *)
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_choice_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> token env tok (* "==" *)
        | `BANGEQ tok -> token env tok (* "!=" *)
        | `LT tok -> token env tok (* "<" *)
        | `LTEQ tok -> token env tok (* "<=" *)
        | `GT tok -> token env tok (* ">" *)
        | `GTEQ tok -> token env tok (* ">=" *)
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

let map_import_spec_list (env : env) ((v1, v2, v3) : CST.import_spec_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = map_import_spec env v1 in
      let v2 =
        (match v2 with
        | `LF tok -> token env tok (* "\n" *)
        | `SEMI tok -> token env tok (* ";" *)
        )
      in
      todo env (v1, v2)
    ) v2
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

let map_method_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.method_declaration) =
  let v1 = token env v1 (* "func" *) in
  let v2 = map_parameter_list env v2 in
  let v3 = token env v3 (* identifier *) in
  let v4 = map_parameter_list env v4 in
  let v5 =
    (match v5 with
    | Some x ->
        (match x with
        | `Param_list x -> map_parameter_list env x
        | `Simple_type x -> map_simple_type env x
        )
    | None -> todo env ())
  in
  let v6 =
    (match v6 with
    | Some x -> map_block env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6)

let map_function_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.function_declaration) =
  let v1 = token env v1 (* "func" *) in
  let v2 = token env v2 (* identifier *) in
  let v3 = map_parameter_list env v3 in
  let v4 =
    (match v4 with
    | Some x ->
        (match x with
        | `Param_list x -> map_parameter_list env x
        | `Simple_type x -> map_simple_type env x
        )
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_block env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

let map_import_declaration (env : env) ((v1, v2) : CST.import_declaration) =
  let v1 = token env v1 (* "import" *) in
  let v2 =
    (match v2 with
    | `Impo_spec x -> map_import_spec env x
    | `Impo_spec_list x -> map_import_spec_list env x
    )
  in
  todo env (v1, v2)

let map_source_file (env : env) (xs : CST.source_file) =
  List.map (fun x ->
    (match x with
    | `Stmt_choice_LF (v1, v2) ->
        let v1 = map_statement env v1 in
        let v2 =
          (match v2 with
          | `LF tok -> token env tok (* "\n" *)
          | `SEMI tok -> token env tok (* ";" *)
          )
        in
        todo env (v1, v2)
    | `Choice_pack_clau_opt_choice_LF (v1, v2) ->
        let v1 =
          (match v1 with
          | `Pack_clau x -> map_package_clause env x
          | `Func_decl x -> map_function_declaration env x
          | `Meth_decl x -> map_method_declaration env x
          | `Impo_decl x -> map_import_declaration env x
          )
        in
        let v2 =
          (match v2 with
          | Some x ->
              (match x with
              | `LF tok -> token env tok (* "\n" *)
              | `SEMI tok -> token env tok (* ";" *)
              )
          | None -> todo env ())
        in
        todo env (v1, v2)
    )
  ) xs

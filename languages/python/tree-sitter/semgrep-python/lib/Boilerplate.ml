(**
   Boilerplate to be used as a template when mapping the python CST
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

let map_import_prefix (env : env) (xs : CST.import_prefix) =
  R.List (List.map (token env (* "." *)) xs)

let map_type_conversion (env : env) (tok : CST.type_conversion) =
  (* pattern ![a-z] *) token env tok

let map_dedent (env : env) (tok : CST.dedent) =
  (* dedent *) token env tok

let map_string_content_ (env : env) (tok : CST.string_content_) =
  (* string_content_ *) token env tok

let map_float_ (env : env) (tok : CST.float_) =
  (* float *) token env tok

let map_string_end (env : env) (tok : CST.string_end) =
  (* string_end *) token env tok

let map_escape_interpolation (env : env) (tok : CST.escape_interpolation) =
  (* escape_interpolation *) token env tok

let map_indent (env : env) (tok : CST.indent) =
  (* indent *) token env tok

let map_tok_prec_p1_pat_a2d1fce (env : env) (tok : CST.tok_prec_p1_pat_a2d1fce) =
  (* tok_prec_p1_pat_a2d1fce *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok

let map_string_start (env : env) (tok : CST.string_start) =
  (* string_start *) token env tok

let map_is_not (env : env) ((v1, v2) : CST.is_not) =
  let v1 = (* "is" *) token env v1 in
  let v2 = (* "not" *) token env v2 in
  R.Tuple [v1; v2]

let map_integer (env : env) (tok : CST.integer) =
  (* integer *) token env tok

let map_newline (env : env) (tok : CST.newline) =
  (* newline *) token env tok

let map_keyword_identifier (env : env) (x : CST.keyword_identifier) =
  (match x with
  | `Choice_print x -> R.Case ("Choice_print",
      (match x with
      | `Print tok -> R.Case ("Print",
          (* "print" *) token env tok
        )
      | `Exec tok -> R.Case ("Exec",
          (* "exec" *) token env tok
        )
      | `Async tok -> R.Case ("Async",
          (* "async" *) token env tok
        )
      | `Await tok -> R.Case ("Await",
          (* "await" *) token env tok
        )
      )
    )
  | `Choice_type x -> R.Case ("Choice_type",
      (match x with
      | `Type tok -> R.Case ("Type",
          (* "type" *) token env tok
        )
      | `Match tok -> R.Case ("Match",
          (* "match" *) token env tok
        )
      )
    )
  )

let map_not_in (env : env) ((v1, v2) : CST.not_in) =
  let v1 = (* "not" *) token env v1 in
  let v2 = (* "in" *) token env v2 in
  R.Tuple [v1; v2]

let map_anon_choice_STAR_f834b26 (env : env) (x : CST.anon_choice_STAR_f834b26) =
  (match x with
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `STARSTAR tok -> R.Case ("STARSTAR",
      (* "**" *) token env tok
    )
  )

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_dotted_name (env : env) ((v1, v2) : CST.dotted_name) =
  let v1 =
    (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_anon_choice_int_e7b97da (env : env) (x : CST.anon_choice_int_e7b97da) =
  (match x with
  | `Int tok -> R.Case ("Int",
      (* integer *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* float *) token env tok
    )
  )

let map_named_expression_lhs (env : env) (x : CST.named_expression_lhs) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
    )
  | `Choice_choice_print x -> R.Case ("Choice_choice_print",
      map_keyword_identifier env x
    )
  )

let map_splat_pattern (env : env) ((v1, v2) : CST.splat_pattern) =
  let v1 = map_anon_choice_STAR_f834b26 env v1 in
  let v2 =
    (match v2 with
    | `Id tok -> R.Case ("Id",
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
      )
    | `X__ tok -> R.Case ("X__",
        (* "_" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

let map_string_content (env : env) (xs : CST.string_content) =
  R.List (List.map (fun x ->
    (match x with
    | `Esc_interp tok -> R.Case ("Esc_interp",
        (* escape_interpolation *) token env tok
      )
    | `Esc_seq tok -> R.Case ("Esc_seq",
        (* escape_sequence *) token env tok
      )
    | `Not_esc_seq tok -> R.Case ("Not_esc_seq",
        (* "\\" *) token env tok
      )
    | `Str_content_ tok -> R.Case ("Str_content_",
        (* string_content_ *) token env tok
      )
    )
  ) xs)

let map_relative_import (env : env) ((v1, v2) : CST.relative_import) =
  let v1 = map_import_prefix env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_dotted_name env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let rec map_anon_choice_exp_03d361f (env : env) (x : CST.anon_choice_exp_03d361f) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Yield x -> R.Case ("Yield",
      map_yield env x
    )
  | `List_splat x -> R.Case ("List_splat",
      map_list_splat env x
    )
  | `Paren_list_splat x -> R.Case ("Paren_list_splat",
      map_parenthesized_list_splat env x
    )
  )

and map_anon_choice_exp_a577897 (env : env) (x : CST.anon_choice_exp_a577897) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Slice (v1, v2, v3, v4) -> R.Case ("Slice",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* ":" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_expression env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_anon_choice_exp_aad5b2d (env : env) (x : CST.anon_choice_exp_aad5b2d) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `List_splat x -> R.Case ("List_splat",
      map_list_splat env x
    )
  | `Dict_splat x -> R.Case ("Dict_splat",
      map_dictionary_splat env x
    )
  | `Paren_list_splat x -> R.Case ("Paren_list_splat",
      map_parenthesized_list_splat env x
    )
  | `Kw_arg (v1, v2, v3) -> R.Case ("Kw_arg",
      let v1 = map_named_expression_lhs env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_id_9e93682 (env : env) (x : CST.anon_choice_id_9e93682) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
    )
  | `Choice_choice_print x -> R.Case ("Choice_choice_print",
      map_keyword_identifier env x
    )
  | `Subs x -> R.Case ("Subs",
      map_subscript env x
    )
  | `Attr x -> R.Case ("Attr",
      map_attribute env x
    )
  )

and map_anon_choice_pair_002ffed (env : env) (x : CST.anon_choice_pair_002ffed) =
  (match x with
  | `Pair x -> R.Case ("Pair",
      map_pair env x
    )
  | `Dict_splat x -> R.Case ("Dict_splat",
      map_dictionary_splat env x
    )
  )

and map_argument_list (env : env) ((v1, v2, v3, v4) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_exp_aad5b2d env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_exp_aad5b2d env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_attribute (env : env) ((v1, v2, v3) : CST.attribute) =
  let v1 = map_primary_expression env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 =
    (match v3 with
    | `Id tok -> R.Case ("Id",
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
      )
    | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
        (* "..." *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_binary_operator (env : env) (x : CST.binary_operator) =
  (match x with
  | `Prim_exp_PLUS_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_PLUS_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prim_exp_DASH_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_DASH_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prim_exp_STAR_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_STAR_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prim_exp_AT_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_AT_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "@" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prim_exp_SLASH_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_SLASH_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prim_exp_PERC_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_PERC_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prim_exp_SLASHSLASH_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_SLASHSLASH_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "//" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prim_exp_STARSTAR_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_STARSTAR_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prim_exp_BAR_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_BAR_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prim_exp_AMP_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_AMP_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prim_exp_HAT_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_HAT_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prim_exp_LTLT_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_LTLT_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prim_exp_GTGT_prim_exp (v1, v2, v3) -> R.Case ("Prim_exp_GTGT_prim_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_primary_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_boolean_operator (env : env) (x : CST.boolean_operator) =
  (match x with
  | `Exp_and_exp (v1, v2, v3) -> R.Case ("Exp_and_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_or_exp (v1, v2, v3) -> R.Case ("Exp_or_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "or" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_collection_elements (env : env) ((v1, v2, v3) : CST.collection_elements) =
  let v1 = map_anon_choice_exp_03d361f env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_exp_03d361f env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_comprehension_clauses (env : env) ((v1, v2) : CST.comprehension_clauses) =
  let v1 = map_for_in_clause env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `For_in_clause x -> R.Case ("For_in_clause",
          map_for_in_clause env x
        )
      | `If_clause x -> R.Case ("If_clause",
          map_if_clause env x
        )
      )
    ) v2)
  in
  R.Tuple [v1; v2]

and map_concatenated_string (env : env) ((v1, v2) : CST.concatenated_string) =
  let v1 = map_string_ env v1 in
  let v2 = R.List (List.map (map_string_ env) v2) in
  R.Tuple [v1; v2]

and map_dictionary_splat (env : env) ((v1, v2) : CST.dictionary_splat) =
  let v1 = (* "**" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_dictionary_splat_pattern (env : env) ((v1, v2) : CST.dictionary_splat_pattern) =
  let v1 = (* "**" *) token env v1 in
  let v2 = map_anon_choice_id_9e93682 env v2 in
  R.Tuple [v1; v2]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Comp_op (v1, v2) -> R.Case ("Comp_op",
      let v1 = map_primary_expression env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 =
            (match v1 with
            | `LT tok -> R.Case ("LT",
                (* "<" *) token env tok
              )
            | `LTEQ tok -> R.Case ("LTEQ",
                (* "<=" *) token env tok
              )
            | `EQEQ tok -> R.Case ("EQEQ",
                (* "==" *) token env tok
              )
            | `BANGEQ tok -> R.Case ("BANGEQ",
                (* "!=" *) token env tok
              )
            | `GTEQ tok -> R.Case ("GTEQ",
                (* ">=" *) token env tok
              )
            | `GT tok -> R.Case ("GT",
                (* ">" *) token env tok
              )
            | `LTGT tok -> R.Case ("LTGT",
                (* "<>" *) token env tok
              )
            | `In tok -> R.Case ("In",
                (* "in" *) token env tok
              )
            | `Not_in x -> R.Case ("Not_in",
                map_not_in env x
              )
            | `Is tok -> R.Case ("Is",
                (* "is" *) token env tok
              )
            | `Is_not x -> R.Case ("Is_not",
                map_is_not env x
              )
            )
          in
          let v2 = map_primary_expression env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Not_op (v1, v2) -> R.Case ("Not_op",
      let v1 = (* "not" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Bool_op x -> R.Case ("Bool_op",
      map_boolean_operator env x
    )
  | `Lambda (v1, v2, v3, v4) -> R.Case ("Lambda",
      let v1 = (* "lambda" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_lambda_parameters env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Prim_exp x -> R.Case ("Prim_exp",
      map_primary_expression env x
    )
  | `Cond_exp (v1, v2, v3, v4, v5) -> R.Case ("Cond_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "if" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "else" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Named_exp (v1, v2, v3) -> R.Case ("Named_exp",
      let v1 = map_named_expression_lhs env v1 in
      let v2 = (* ":=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `As_pat_ (v1, v2, v3) -> R.Case ("As_pat_",
      let v1 = map_expression env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_expression_list (env : env) ((v1, v2) : CST.expression_list) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `COMMA tok -> R.Case ("COMMA",
        (* "," *) token env tok
      )
    | `Rep1_COMMA_exp_opt_COMMA (v1, v2) -> R.Case ("Rep1_COMMA_exp_opt_COMMA",
        let v1 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ) v1)
        in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2]

and map_expression_within_for_in_clause (env : env) (x : CST.expression_within_for_in_clause) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Lambda_within_for_in_clause (v1, v2, v3, v4) -> R.Case ("Lambda_within_for_in_clause",
      let v1 = (* "lambda" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_lambda_parameters env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_expression_within_for_in_clause env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_expressions (env : env) (x : CST.expressions) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Exp_list x -> R.Case ("Exp_list",
      map_expression_list env x
    )
  )

and map_f_expression (env : env) (x : CST.f_expression) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Exp_list x -> R.Case ("Exp_list",
      map_expression_list env x
    )
  | `Pat_list x -> R.Case ("Pat_list",
      map_pattern_list env x
    )
  | `Yield x -> R.Case ("Yield",
      map_yield env x
    )
  )

and map_for_in_clause (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.for_in_clause) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "for" *) token env v2 in
  let v3 = map_left_hand_side env v3 in
  let v4 = (* "in" *) token env v4 in
  let v5 = map_expression_within_for_in_clause env v5 in
  let v6 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression_within_for_in_clause env v2 in
      R.Tuple [v1; v2]
    ) v6)
  in
  let v7 =
    (match v7 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_format_specifier (env : env) ((v1, v2) : CST.format_specifier) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Tok_prec_p1_pat_a2d1fce x -> R.Case ("Tok_prec_p1_pat_a2d1fce",
          map_tok_prec_p1_pat_a2d1fce env x
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      )
    ) v2)
  in
  R.Tuple [v1; v2]

and map_generator_expression (env : env) ((v1, v2, v3, v4) : CST.generator_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = map_comprehension_clauses env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_if_clause (env : env) ((v1, v2) : CST.if_clause) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_interpolation (env : env) ((v1, v2, v3, v4, v5, v6) : CST.interpolation) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_f_expression env v2 in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "=" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* pattern ![a-z] *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_format_specifier env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* "}" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_lambda_parameters (env : env) (x : CST.lambda_parameters) =
  map_parameters_ env x

and map_left_hand_side (env : env) (x : CST.left_hand_side) =
  (match x with
  | `Pat x -> R.Case ("Pat",
      map_pattern env x
    )
  | `Pat_list x -> R.Case ("Pat_list",
      map_pattern_list env x
    )
  )

and map_list_splat (env : env) ((v1, v2) : CST.list_splat) =
  let v1 = (* "*" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_list_splat_pattern (env : env) ((v1, v2) : CST.list_splat_pattern) =
  let v1 = (* "*" *) token env v1 in
  let v2 = map_anon_choice_id_9e93682 env v2 in
  R.Tuple [v1; v2]

and map_pair (env : env) ((v1, v2, v3) : CST.pair) =
  let v1 = map_expression env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_parameter (env : env) (x : CST.parameter) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
    )
  | `Typed_param (v1, v2, v3) -> R.Case ("Typed_param",
      let v1 =
        (match v1 with
        | `Id tok -> R.Case ("Id",
            (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
          )
        | `List_splat_pat x -> R.Case ("List_splat_pat",
            map_list_splat_pattern env x
          )
        | `Dict_splat_pat x -> R.Case ("Dict_splat_pat",
            map_dictionary_splat_pattern env x
          )
        )
      in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Defa_param (v1, v2, v3) -> R.Case ("Defa_param",
      let v1 =
        (match v1 with
        | `Id tok -> R.Case ("Id",
            (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
          )
        | `Tuple_pat_ x -> R.Case ("Tuple_pat_",
            map_tuple_pattern_ env x
          )
        )
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Typed_defa_param (v1, v2, v3, v4, v5) -> R.Case ("Typed_defa_param",
      let v1 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v1
      in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_type_ env v3 in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `List_splat_pat x -> R.Case ("List_splat_pat",
      map_list_splat_pattern env x
    )
  | `Tuple_pat_ x -> R.Case ("Tuple_pat_",
      map_tuple_pattern_ env x
    )
  | `Kw_sepa tok -> R.Case ("Kw_sepa",
      (* "*" *) token env tok
    )
  | `Posi_sepa tok -> R.Case ("Posi_sepa",
      (* "/" *) token env tok
    )
  | `Dict_splat_pat x -> R.Case ("Dict_splat_pat",
      map_dictionary_splat_pattern env x
    )
  )

and map_parameters_ (env : env) ((v1, v2, v3) : CST.parameters_) =
  let v1 = map_parameter env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_parameter env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_parenthesized_list_splat (env : env) ((v1, v2, v3) : CST.parenthesized_list_splat) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Paren_list_splat x -> R.Case ("Paren_list_splat",
        map_parenthesized_list_splat env x
      )
    | `List_splat x -> R.Case ("List_splat",
        map_list_splat env x
      )
    )
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
    )
  | `Choice_choice_print x -> R.Case ("Choice_choice_print",
      map_keyword_identifier env x
    )
  | `Subs x -> R.Case ("Subs",
      map_subscript env x
    )
  | `Attr x -> R.Case ("Attr",
      map_attribute env x
    )
  | `List_splat_pat x -> R.Case ("List_splat_pat",
      map_list_splat_pattern env x
    )
  | `Tuple_pat_ x -> R.Case ("Tuple_pat_",
      map_tuple_pattern_ env x
    )
  | `List_pat_ (v1, v2, v3) -> R.Case ("List_pat_",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_patterns env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_pattern_list (env : env) ((v1, v2) : CST.pattern_list) =
  let v1 = map_pattern env v1 in
  let v2 =
    (match v2 with
    | `COMMA tok -> R.Case ("COMMA",
        (* "," *) token env tok
      )
    | `Rep1_COMMA_pat_opt_COMMA (v1, v2) -> R.Case ("Rep1_COMMA_pat_opt_COMMA",
        let v1 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_pattern env v2 in
            R.Tuple [v1; v2]
          ) v1)
        in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2]

and map_patterns (env : env) ((v1, v2, v3) : CST.patterns) =
  let v1 = map_pattern env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Await (v1, v2) -> R.Case ("Await",
      let v1 = (* "await" *) token env v1 in
      let v2 = map_primary_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Bin_op x -> R.Case ("Bin_op",
      map_binary_operator env x
    )
  | `Id tok -> R.Case ("Id",
      (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
    )
  | `Choice_choice_print x -> R.Case ("Choice_choice_print",
      map_keyword_identifier env x
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Conc_str x -> R.Case ("Conc_str",
      map_concatenated_string env x
    )
  | `Int tok -> R.Case ("Int",
      (* integer *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* float *) token env tok
    )
  | `True tok -> R.Case ("True",
      (* "True" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "False" *) token env tok
    )
  | `None tok -> R.Case ("None",
      (* "None" *) token env tok
    )
  | `Un_op (v1, v2) -> R.Case ("Un_op",
      let v1 =
        (match v1 with
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `TILDE tok -> R.Case ("TILDE",
            (* "~" *) token env tok
          )
        )
      in
      let v2 = map_primary_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Attr x -> R.Case ("Attr",
      map_attribute env x
    )
  | `Subs x -> R.Case ("Subs",
      map_subscript env x
    )
  | `Call (v1, v2) -> R.Case ("Call",
      let v1 = map_primary_expression env v1 in
      let v2 =
        (match v2 with
        | `Gene_exp x -> R.Case ("Gene_exp",
            map_generator_expression env x
          )
        | `Arg_list x -> R.Case ("Arg_list",
            map_argument_list env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `List (v1, v2, v3) -> R.Case ("List",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_collection_elements env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `List_comp (v1, v2, v3, v4) -> R.Case ("List_comp",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_comprehension_clauses env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Dict (v1, v2, v3, v4) -> R.Case ("Dict",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_anon_choice_pair_002ffed env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_pair_002ffed env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Dict_comp (v1, v2, v3, v4) -> R.Case ("Dict_comp",
      let v1 = (* "{" *) token env v1 in
      let v2 = map_pair env v2 in
      let v3 = map_comprehension_clauses env v3 in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Set (v1, v2, v3) -> R.Case ("Set",
      let v1 = (* "{" *) token env v1 in
      let v2 = map_collection_elements env v2 in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Set_comp (v1, v2, v3, v4) -> R.Case ("Set_comp",
      let v1 = (* "{" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_comprehension_clauses env v3 in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Tuple (v1, v2, v3) -> R.Case ("Tuple",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_collection_elements env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Paren_exp (v1, v2, v3) -> R.Case ("Paren_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Yield x -> R.Case ("Yield",
            map_yield env x
          )
        )
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Gene_exp x -> R.Case ("Gene_exp",
      map_generator_expression env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `List_splat_pat x -> R.Case ("List_splat_pat",
      map_list_splat_pattern env x
    )
  )

and map_string_ (env : env) ((v1, v2, v3) : CST.string_) =
  let v1 = (* string_start *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Str_content x -> R.Case ("Str_content",
          map_string_content env x
        )
      )
    ) v2)
  in
  let v3 = (* string_end *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_subscript (env : env) ((v1, v2, v3, v4, v5, v6) : CST.subscript) =
  let v1 = map_primary_expression env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_anon_choice_exp_a577897 env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_exp_a577897 env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* "]" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_tuple_pattern_ (env : env) ((v1, v2, v3) : CST.tuple_pattern_) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_patterns env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Splat_type (v1, v2) -> R.Case ("Splat_type",
      let v1 = map_anon_choice_STAR_f834b26 env v1 in
      let v2 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v2
      in
      R.Tuple [v1; v2]
    )
  | `Gene_type (v1, v2) -> R.Case ("Gene_type",
      let v1 =
        (match v1 with
        | `Id tok -> R.Case ("Id",
            (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
          )
        | `Type tok -> R.Case ("Type",
            (* "type" *) token env tok
          )
        )
      in
      let v2 = map_type_parameter env v2 in
      R.Tuple [v1; v2]
    )
  | `Union_type (v1, v2, v3) -> R.Case ("Union_type",
      let v1 = map_type_ env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cons_type (v1, v2, v3) -> R.Case ("Cons_type",
      let v1 = map_type_ env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Member_type (v1, v2, v3) -> R.Case ("Member_type",
      let v1 = map_type_ env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_type_parameter (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameter) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_yield (env : env) ((v1, v2) : CST.yield) =
  let v1 = (* "yield" *) token env v1 in
  let v2 =
    (match v2 with
    | `From_exp (v1, v2) -> R.Case ("From_exp",
        let v1 = (* "from" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      )
    | `Opt_choice_exp opt -> R.Case ("Opt_choice_exp",
        (match opt with
        | Some x -> R.Option (Some (
            map_expressions env x
          ))
        | None -> R.Option None)
      )
    )
  in
  R.Tuple [v1; v2]

let map_anon_choice_dotted_name_c5c573a (env : env) (x : CST.anon_choice_dotted_name_c5c573a) =
  (match x with
  | `Dotted_name x -> R.Case ("Dotted_name",
      map_dotted_name env x
    )
  | `Alia_import (v1, v2, v3) -> R.Case ("Alia_import",
      let v1 = map_dotted_name env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_decorator (env : env) ((v1, v2, v3) : CST.decorator) =
  let v1 = (* "@" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* newline *) token env v3 in
  R.Tuple [v1; v2; v3]

let rec map_anon_case_pat_rep_COMMA_case_pat_opt_COMMA_0f1ba58 (env : env) ((v1, v2, v3) : CST.anon_case_pat_rep_COMMA_case_pat_opt_COMMA_0f1ba58) =
  let v1 = map_case_pattern env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_case_pattern env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_anon_choice_key_value_pat_9cde426 (env : env) (x : CST.anon_choice_key_value_pat_9cde426) =
  (match x with
  | `Key_value_pat (v1, v2, v3) -> R.Case ("Key_value_pat",
      let v1 = map_simple_pattern env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_case_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Splat_pat x -> R.Case ("Splat_pat",
      map_splat_pattern env x
    )
  )

and map_case_pattern (env : env) (x : CST.case_pattern) =
  (match x with
  | `As_pat (v1, v2, v3) -> R.Case ("As_pat",
      let v1 = map_case_pattern env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  | `Kw_pat (v1, v2, v3) -> R.Case ("Kw_pat",
      let v1 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_simple_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Simple_pat x -> R.Case ("Simple_pat",
      map_simple_pattern env x
    )
  )

and map_simple_pattern (env : env) (x : CST.simple_pattern) =
  (match x with
  | `Class_pat (v1, v2, v3, v4) -> R.Case ("Class_pat",
      let v1 = map_dotted_name env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_case_pat_rep_COMMA_case_pat_opt_COMMA_0f1ba58 env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Splat_pat x -> R.Case ("Splat_pat",
      map_splat_pattern env x
    )
  | `Union_pat (v1, v2) -> R.Case ("Union_pat",
      let v1 = map_simple_pattern env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "|" *) token env v1 in
          let v2 = map_simple_pattern env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `List_pat (v1, v2, v3) -> R.Case ("List_pat",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_case_pat_rep_COMMA_case_pat_opt_COMMA_0f1ba58 env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Tuple_pat (v1, v2, v3) -> R.Case ("Tuple_pat",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_case_pat_rep_COMMA_case_pat_opt_COMMA_0f1ba58 env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Dict_pat (v1, v2, v3) -> R.Case ("Dict_pat",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_anon_choice_key_value_pat_9cde426 env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_key_value_pat_9cde426 env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Conc_str x -> R.Case ("Conc_str",
      map_concatenated_string env x
    )
  | `True tok -> R.Case ("True",
      (* "True" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "False" *) token env tok
    )
  | `None tok -> R.Case ("None",
      (* "None" *) token env tok
    )
  | `Opt_DASH_choice_int (v1, v2) -> R.Case ("Opt_DASH_choice_int",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "-" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_anon_choice_int_e7b97da env v2 in
      R.Tuple [v1; v2]
    )
  | `Comp_pat (v1, v2, v3, v4) -> R.Case ("Comp_pat",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "-" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_anon_choice_int_e7b97da env v2 in
      let v3 =
        (match v3 with
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        )
      in
      let v4 = map_anon_choice_int_e7b97da env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Dotted_name x -> R.Case ("Dotted_name",
      map_dotted_name env x
    )
  | `X__ tok -> R.Case ("X__",
      (* "_" *) token env tok
    )
  )

let map_with_item (env : env) (v1 : CST.with_item) =
  map_expression env v1

let map_parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_lambda_parameters env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_chevron (env : env) ((v1, v2) : CST.chevron) =
  let v1 = (* ">>" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

let rec map_assignment (env : env) ((v1, v2) : CST.assignment) =
  let v1 = map_left_hand_side env v1 in
  let v2 =
    (match v2 with
    | `EQ_right_hand_side (v1, v2) -> R.Case ("EQ_right_hand_side",
        let v1 = (* "=" *) token env v1 in
        let v2 = map_right_hand_side env v2 in
        R.Tuple [v1; v2]
      )
    | `COLON_type (v1, v2) -> R.Case ("COLON_type",
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      )
    | `COLON_type_EQ_right_hand_side (v1, v2, v3, v4) -> R.Case ("COLON_type_EQ_right_hand_side",
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        let v3 = (* "=" *) token env v3 in
        let v4 = map_right_hand_side env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  R.Tuple [v1; v2]

and map_augmented_assignment (env : env) ((v1, v2, v3) : CST.augmented_assignment) =
  let v1 = map_left_hand_side env v1 in
  let v2 =
    (match v2 with
    | `PLUSEQ tok -> R.Case ("PLUSEQ",
        (* "+=" *) token env tok
      )
    | `DASHEQ tok -> R.Case ("DASHEQ",
        (* "-=" *) token env tok
      )
    | `STAREQ tok -> R.Case ("STAREQ",
        (* "*=" *) token env tok
      )
    | `SLASHEQ tok -> R.Case ("SLASHEQ",
        (* "/=" *) token env tok
      )
    | `ATEQ tok -> R.Case ("ATEQ",
        (* "@=" *) token env tok
      )
    | `SLASHSLASHEQ tok -> R.Case ("SLASHSLASHEQ",
        (* "//=" *) token env tok
      )
    | `PERCEQ tok -> R.Case ("PERCEQ",
        (* "%=" *) token env tok
      )
    | `STARSTAREQ tok -> R.Case ("STARSTAREQ",
        (* "**=" *) token env tok
      )
    | `GTGTEQ tok -> R.Case ("GTGTEQ",
        (* ">>=" *) token env tok
      )
    | `LTLTEQ tok -> R.Case ("LTLTEQ",
        (* "<<=" *) token env tok
      )
    | `AMPEQ tok -> R.Case ("AMPEQ",
        (* "&=" *) token env tok
      )
    | `HATEQ tok -> R.Case ("HATEQ",
        (* "^=" *) token env tok
      )
    | `BAREQ tok -> R.Case ("BAREQ",
        (* "|=" *) token env tok
      )
    )
  in
  let v3 = map_right_hand_side env v3 in
  R.Tuple [v1; v2; v3]

and map_right_hand_side (env : env) (x : CST.right_hand_side) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Exp_list x -> R.Case ("Exp_list",
      map_expression_list env x
    )
  | `Assign x -> R.Case ("Assign",
      map_assignment env x
    )
  | `Augm_assign x -> R.Case ("Augm_assign",
      map_augmented_assignment env x
    )
  | `Pat_list x -> R.Case ("Pat_list",
      map_pattern_list env x
    )
  | `Yield x -> R.Case ("Yield",
      map_yield env x
    )
  )

let map_import_list (env : env) ((v1, v2, v3) : CST.import_list) =
  let v1 = map_anon_choice_dotted_name_c5c573a env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_dotted_name_c5c573a env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_with_clause (env : env) (x : CST.with_clause) =
  (match x with
  | `With_item_rep_COMMA_with_item_opt_COMMA (v1, v2, v3) -> R.Case ("With_item_rep_COMMA_with_item_opt_COMMA",
      let v1 = map_with_item env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_with_item env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `LPAR_with_item_rep_COMMA_with_item_opt_COMMA_RPAR (v1, v2, v3, v4, v5) -> R.Case ("LPAR_with_item_rep_COMMA_with_item_opt_COMMA_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_with_item env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_with_item env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

let map_print_statement (env : env) (x : CST.print_statement) =
  (match x with
  | `Print_chev_rep_COMMA_exp_opt_COMMA (v1, v2, v3, v4) -> R.Case ("Print_chev_rep_COMMA_exp_opt_COMMA",
      let v1 = (* "print" *) token env v1 in
      let v2 = map_chevron env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Print_exp_rep_COMMA_exp_opt_COMMA (v1, v2, v3, v4) -> R.Case ("Print_exp_rep_COMMA_exp_opt_COMMA",
      let v1 = (* "print" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_expression_statement (env : env) (x : CST.expression_statement) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Exp_rep_COMMA_exp_opt_COMMA (v1, v2, v3) -> R.Case ("Exp_rep_COMMA_exp_opt_COMMA",
      let v1 = map_expression env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Assign x -> R.Case ("Assign",
      map_assignment env x
    )
  | `Augm_assign x -> R.Case ("Augm_assign",
      map_augmented_assignment env x
    )
  | `Yield x -> R.Case ("Yield",
      map_yield env x
    )
  )

let map_simple_statement (env : env) (x : CST.simple_statement) =
  (match x with
  | `Future_import_stmt (v1, v2, v3, v4) -> R.Case ("Future_import_stmt",
      let v1 = (* "from" *) token env v1 in
      let v2 = (* "__future__" *) token env v2 in
      let v3 = (* "import" *) token env v3 in
      let v4 =
        (match v4 with
        | `Import_list x -> R.Case ("Import_list",
            map_import_list env x
          )
        | `LPAR_import_list_RPAR (v1, v2, v3) -> R.Case ("LPAR_import_list_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = map_import_list env v2 in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Import_stmt (v1, v2) -> R.Case ("Import_stmt",
      let v1 = (* "import" *) token env v1 in
      let v2 = map_import_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Import_from_stmt (v1, v2, v3, v4) -> R.Case ("Import_from_stmt",
      let v1 = (* "from" *) token env v1 in
      let v2 =
        (match v2 with
        | `Rela_import x -> R.Case ("Rela_import",
            map_relative_import env x
          )
        | `Dotted_name x -> R.Case ("Dotted_name",
            map_dotted_name env x
          )
        )
      in
      let v3 = (* "import" *) token env v3 in
      let v4 =
        (match v4 with
        | `Wild_import tok -> R.Case ("Wild_import",
            (* "*" *) token env tok
          )
        | `Import_list x -> R.Case ("Import_list",
            map_import_list env x
          )
        | `LPAR_import_list_RPAR (v1, v2, v3) -> R.Case ("LPAR_import_list_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = map_import_list env v2 in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Print_stmt x -> R.Case ("Print_stmt",
      map_print_statement env x
    )
  | `Assert_stmt (v1, v2, v3) -> R.Case ("Assert_stmt",
      let v1 = (* "assert" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_expression_statement env x
    )
  | `Ret_stmt (v1, v2) -> R.Case ("Ret_stmt",
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expressions env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Delete_stmt (v1, v2) -> R.Case ("Delete_stmt",
      let v1 = (* "del" *) token env v1 in
      let v2 = map_expressions env v2 in
      R.Tuple [v1; v2]
    )
  | `Raise_stmt (v1, v2, v3) -> R.Case ("Raise_stmt",
      let v1 = (* "raise" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expressions env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "from" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Pass_stmt tok -> R.Case ("Pass_stmt",
      (* "pass" *) token env tok
    )
  | `Brk_stmt tok -> R.Case ("Brk_stmt",
      (* "break" *) token env tok
    )
  | `Cont_stmt tok -> R.Case ("Cont_stmt",
      (* "continue" *) token env tok
    )
  | `Global_stmt (v1, v2, v3) -> R.Case ("Global_stmt",
      let v1 = (* "global" *) token env v1 in
      let v2 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v2
      in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 =
            (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v2
          in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Nonl_stmt (v1, v2, v3) -> R.Case ("Nonl_stmt",
      let v1 = (* "nonlocal" *) token env v1 in
      let v2 =
        (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v2
      in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 =
            (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v2
          in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Exec_stmt (v1, v2, v3) -> R.Case ("Exec_stmt",
      let v1 = (* "exec" *) token env v1 in
      let v2 =
        (match v2 with
        | `Str x -> R.Case ("Str",
            map_string_ env x
          )
        | `Id tok -> R.Case ("Id",
            (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env tok
          )
        )
      in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = (* "in" *) token env v1 in
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
  | `Type_alias_stmt (v1, v2, v3, v4) -> R.Case ("Type_alias_stmt",
      let v1 = (* "type" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_type_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_simple_statements (env : env) ((v1, v2, v3, v4) : CST.simple_statements) =
  let v1 = map_simple_statement env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* ";" *) token env v1 in
      let v2 = map_simple_statement env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* newline *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let rec map_block (env : env) ((v1, v2) : CST.block) =
  let v1 = map_module_ env v1 in
  let v2 = (* dedent *) token env v2 in
  R.Tuple [v1; v2]

and map_case_clause (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.case_clause) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_case_pattern env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_case_pattern env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_if_clause env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* ":" *) token env v6 in
  let v7 = map_suite env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_class_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_definition) =
  let v1 = (* "class" *) token env v1 in
  let v2 =
    (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v2
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_parameter env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* ":" *) token env v5 in
  let v6 = map_suite env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_compound_statement (env : env) (x : CST.compound_statement) =
  (match x with
  | `If_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("If_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_suite env v4 in
      let v5 = R.List (List.map (map_elif_clause env) v5) in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_else_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("For_stmt",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "async" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "for" *) token env v2 in
      let v3 = map_left_hand_side env v3 in
      let v4 = (* "in" *) token env v4 in
      let v5 = map_expressions env v5 in
      let v6 = (* ":" *) token env v6 in
      let v7 = map_suite env v7 in
      let v8 =
        (match v8 with
        | Some x -> R.Option (Some (
            map_else_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `While_stmt (v1, v2, v3, v4, v5) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_suite env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_else_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Try_stmt (v1, v2, v3, v4) -> R.Case ("Try_stmt",
      let v1 = (* "try" *) token env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_suite env v3 in
      let v4 =
        (match v4 with
        | `Rep1_except_clause_opt_else_clause_opt_fina_clause (v1, v2, v3) -> R.Case ("Rep1_except_clause_opt_else_clause_opt_fina_clause",
            let v1 = R.List (List.map (map_except_clause env) v1) in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_else_clause env x
                ))
              | None -> R.Option None)
            in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_finally_clause env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        | `Rep1_except_group_clause_opt_else_clause_opt_fina_clause (v1, v2, v3) -> R.Case ("Rep1_except_group_clause_opt_else_clause_opt_fina_clause",
            let v1 =
              R.List (List.map (map_except_group_clause env) v1)
            in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_else_clause env x
                ))
              | None -> R.Option None)
            in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_finally_clause env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        | `Fina_clause x -> R.Case ("Fina_clause",
            map_finally_clause env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `With_stmt (v1, v2, v3, v4, v5) -> R.Case ("With_stmt",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "async" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "with" *) token env v2 in
      let v3 = map_with_clause env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_suite env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Func_defi x -> R.Case ("Func_defi",
      map_function_definition env x
    )
  | `Class_defi x -> R.Case ("Class_defi",
      map_class_definition env x
    )
  | `Deco_defi (v1, v2) -> R.Case ("Deco_defi",
      let v1 = R.List (List.map (map_decorator env) v1) in
      let v2 =
        (match v2 with
        | `Class_defi x -> R.Case ("Class_defi",
            map_class_definition env x
          )
        | `Func_defi x -> R.Case ("Func_defi",
            map_function_definition env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Match_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("Match_stmt",
      let v1 = (* "match" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = (* ":" *) token env v5 in
      let v6 = map_match_block env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_elif_clause (env : env) ((v1, v2, v3, v4) : CST.elif_clause) =
  let v1 = (* "elif" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_suite env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_else_clause (env : env) ((v1, v2, v3) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_suite env v3 in
  R.Tuple [v1; v2; v3]

and map_except_clause (env : env) ((v1, v2, v3, v4) : CST.except_clause) =
  let v1 = (* "except" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_expression env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 =
                (match v1 with
                | `As tok -> R.Case ("As",
                    (* "as" *) token env tok
                  )
                | `COMMA tok -> R.Case ("COMMA",
                    (* "," *) token env tok
                  )
                )
              in
              let v2 = map_expression env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_suite env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_except_group_clause (env : env) ((v1, v2, v3, v4, v5) : CST.except_group_clause) =
  let v1 = (* "except*" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "as" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_suite env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_finally_clause (env : env) ((v1, v2, v3) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_suite env v3 in
  R.Tuple [v1; v2; v3]

and map_function_definition (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.function_definition) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "def" *) token env v2 in
  let v3 =
    (* pattern \$?[_\p{XID_Start}][_\p{XID_Continue}]* *) token env v3
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameter env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_parameters env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "->" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v7 = (* ":" *) token env v7 in
  let v8 = map_suite env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_match_block (env : env) (x : CST.match_block) =
  (match x with
  | `Indent_rep_case_clause_dedent (v1, v2, v3) -> R.Case ("Indent_rep_case_clause_dedent",
      let v1 = (* indent *) token env v1 in
      let v2 = R.List (List.map (map_case_clause env) v2) in
      let v3 = (* dedent *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Nl tok -> R.Case ("Nl",
      (* newline *) token env tok
    )
  )

and map_module_ (env : env) (xs : CST.module_) =
  R.List (List.map (map_statement env) xs)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Simple_stmts x -> R.Case ("Simple_stmts",
      map_simple_statements env x
    )
  | `Choice_if_stmt x -> R.Case ("Choice_if_stmt",
      map_compound_statement env x
    )
  )

and map_suite (env : env) (x : CST.suite) =
  (match x with
  | `Simple_stmts x -> R.Case ("Simple_stmts",
      map_simple_statements env x
    )
  | `Indent_blk (v1, v2) -> R.Case ("Indent_blk",
      let v1 = (* indent *) token env v1 in
      let v2 = map_block env v2 in
      R.Tuple [v1; v2]
    )
  | `Nl tok -> R.Case ("Nl",
      (* newline *) token env tok
    )
  )

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_line_continuation (env : env) (tok : CST.line_continuation) =
  (* line_continuation *) token env tok

let dump_tree root =
  map_module_ () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Line_continuation (_loc, x) -> ("line_continuation", "line_continuation", map_line_continuation env x)

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

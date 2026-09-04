(**
   Boilerplate to be used as a template when mapping the cairo CST
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

let map_number_suffix (env : env) (tok : CST.number_suffix) =
  (* pattern _[a-z][a-z0-9]+ *) token env tok

let map_unit_ (env : env) (tok : CST.unit_) =
  (* unit *) token env tok

let map_binary (env : env) (tok : CST.binary) =
  (* pattern 0b[01]+ *) token env tok

let map_integer (env : env) (tok : CST.integer) =
  (* pattern [0-9]+ *) token env tok

let map_octal (env : env) (tok : CST.octal) =
  (* pattern 0o[0-7]+ *) token env tok

let map_hex (env : env) (tok : CST.hex) =
  (* pattern 0x[0-9a-f]+ *) token env tok

let map_unit_type (env : env) (tok : CST.unit_type) =
  (* unit_type *) token env tok

let map_semgrep_var (env : env) (tok : CST.semgrep_var) =
  (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok

let map_string_ (env : env) (tok : CST.string_) =
  (* string *) token env tok

let map_pat_7fdeb71 (env : env) (tok : CST.pat_7fdeb71) =
  (* pattern [a-zA-Z][a-zA-Z0-9_]* *) token env tok

let map_modifier (env : env) (x : CST.modifier) =
  (match x with
  | `Modi_ref tok -> R.Case ("Modi_ref",
      (* "ref" *) token env tok
    )
  | `Modi_mut tok -> R.Case ("Modi_mut",
      (* "mut" *) token env tok
    )
  )

let map_name (env : env) (x : CST.name) =
  (match x with
  | `Pat_7fdeb71 x -> R.Case ("Pat_7fdeb71",
      map_pat_7fdeb71 env x
    )
  | `Semg_var tok -> R.Case ("Semg_var",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  )

let map_literal_expression (env : env) (x : CST.literal_expression) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  | `Num (v1, v2) -> R.Case ("Num",
      let v1 =
        (match v1 with
        | `Int tok -> R.Case ("Int",
            (* pattern [0-9]+ *) token env tok
          )
        | `Hex tok -> R.Case ("Hex",
            (* pattern 0x[0-9a-f]+ *) token env tok
          )
        | `Octal tok -> R.Case ("Octal",
            (* pattern 0o[0-7]+ *) token env tok
          )
        | `Bin tok -> R.Case ("Bin",
            (* pattern 0b[01]+ *) token env tok
          )
        )
      in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* pattern _[a-z][a-z0-9]+ *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Str tok -> R.Case ("Str",
      (* string *) token env tok
    )
  | `Unit tok -> R.Case ("Unit",
      (* unit *) token env tok
    )
  )

let map_path (env : env) ((v1, v2, v3) : CST.path) =
  let v1 = map_name env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "::" *) token env v1 in
      let v2 = map_name env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "::" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_pattern_var (env : env) (x : CST.pattern_var) =
  (match x with
  | `Wild tok -> R.Case ("Wild",
      (* "_" *) token env tok
    )
  | `Choice_pat_7fdeb71 x -> R.Case ("Choice_pat_7fdeb71",
      map_name env x
    )
  )

let map_type_argument_list (env : env) ((v1, v2, v3, v4, v5) : CST.type_argument_list) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_literal_expression env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_literal_expression env v2 in
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
  let v5 = (* ">" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let rec map_attribute_argument (env : env) (x : CST.attribute_argument) =
  (match x with
  | `Choice_true x -> R.Case ("Choice_true",
      map_literal_expression env x
    )
  | `Choice_pat_7fdeb71 x -> R.Case ("Choice_pat_7fdeb71",
      map_name env x
    )
  | `Path_COLON_choice_true (v1, v2, v3) -> R.Case ("Path_COLON_choice_true",
      let v1 = map_path env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_literal_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Path_COLON_attr_arg_list (v1, v2, v3) -> R.Case ("Path_COLON_attr_arg_list",
      let v1 = map_path env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_attribute_argument_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_attribute_argument_list (env : env) ((v1, v2, v3, v4, v5) : CST.attribute_argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_attribute_argument env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_attribute_argument env v2 in
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

let map_qualified_name_segment (env : env) ((v1, v2) : CST.qualified_name_segment) =
  let v1 = map_name env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = map_path env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_qualified_name (env : env) ((v1, v2, v3) : CST.qualified_name) =
  let v1 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_qualified_name_segment env v1 in
      let v2 = (* "::" *) token env v2 in
      R.Tuple [v1; v2]
    ) v1)
  in
  let v2 = map_name env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "::" *) token env v1 in
        let v2 = map_type_argument_list env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_attribute_list (env : env) ((v1, v2, v3, v4) : CST.attribute_list) =
  let v1 = (* "#" *) token env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = R.List (List.map (map_attribute env) v3) in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let rec map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Choice_wild x -> R.Case ("Choice_wild",
      map_pattern_var env x
    )
  | `Pat_struct (v1, v2, v3, v4, v5, v6) -> R.Case ("Pat_struct",
      let v1 = map_qualified_name env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 = map_pattern_struct_binding env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_pattern_struct_binding env v2 in
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
      let v6 = (* "}" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Pat_enum (v1, v2, v3, v4, v5, v6) -> R.Case ("Pat_enum",
      let v1 = map_qualified_name env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_pattern env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_pattern env v2 in
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
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Pat_tuple (v1, v2, v3, v4, v5) -> R.Case ("Pat_tuple",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_pattern env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_pattern env v2 in
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

and map_pattern_struct_binding (env : env) (x : CST.pattern_struct_binding) =
  (match x with
  | `Choice_pat_7fdeb71 x -> R.Case ("Choice_pat_7fdeb71",
      map_name env x
    )
  | `Choice_pat_7fdeb71_COLON_choice_choice_wild (v1, v2, v3) -> R.Case ("Choice_pat_7fdeb71_COLON_choice_choice_wild",
      let v1 = map_name env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let rec map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Type_tuple (v1, v2, v3, v4) -> R.Case ("Type_tuple",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_type_ env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Unit_type tok -> R.Case ("Unit_type",
      (* unit_type *) token env tok
    )
  | `Type_id (v1, v2) -> R.Case ("Type_id",
      let v1 = map_qualified_name env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_ env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_match_case_pattern (env : env) (x : CST.match_case_pattern) =
  (match x with
  | `Choice_choice_wild x -> R.Case ("Choice_choice_wild",
      map_pattern env x
    )
  | `Choice_true x -> R.Case ("Choice_true",
      map_literal_expression env x
    )
  )

let map_member_declaration (env : env) (x : CST.member_declaration) =
  (match x with
  | `Rep_attr_list_choice_pat_7fdeb71_COLON_choice_type_tuple (v1, v2, v3, v4) -> R.Case ("Rep_attr_list_choice_pat_7fdeb71_COLON_choice_type_tuple",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = map_name env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_parameter_declaration (env : env) (x : CST.parameter_declaration) =
  (match x with
  | `Rep_choice_modi_ref_choice_pat_7fdeb71_COLON_choice_type_tuple (v1, v2, v3, v4) -> R.Case ("Rep_choice_modi_ref_choice_pat_7fdeb71_COLON_choice_type_tuple",
      let v1 = R.List (List.map (map_modifier env) v1) in
      let v2 = map_name env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let rec map_argument_list (env : env) ((v1, v2, v3, v4) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Choice_tuple_exp_choice_STAR_choice_tuple_exp (v1, v2, v3) -> R.Case ("Choice_tuple_exp_choice_STAR_choice_tuple_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `STAR tok -> R.Case ("STAR",
            (* "*" *) token env tok
          )
        | `SLASH tok -> R.Case ("SLASH",
            (* "/" *) token env tok
          )
        | `PERC tok -> R.Case ("PERC",
            (* "%" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_tuple_exp_choice_PLUS_choice_tuple_exp (v1, v2, v3) -> R.Case ("Choice_tuple_exp_choice_PLUS_choice_tuple_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_tuple_exp_choice_EQEQ_choice_tuple_exp (v1, v2, v3) -> R.Case ("Choice_tuple_exp_choice_EQEQ_choice_tuple_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> R.Case ("EQEQ",
            (* "==" *) token env tok
          )
        | `BANGEQ tok -> R.Case ("BANGEQ",
            (* "!=" *) token env tok
          )
        | `LT tok -> R.Case ("LT",
            (* "<" *) token env tok
          )
        | `LTEQ tok -> R.Case ("LTEQ",
            (* "<=" *) token env tok
          )
        | `GT tok -> R.Case ("GT",
            (* ">" *) token env tok
          )
        | `GTEQ tok -> R.Case ("GTEQ",
            (* ">=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_tuple_exp_AMPAMP_choice_tuple_exp (v1, v2, v3) -> R.Case ("Choice_tuple_exp_AMPAMP_choice_tuple_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_tuple_exp_BARBAR_choice_tuple_exp (v1, v2, v3) -> R.Case ("Choice_tuple_exp_BARBAR_choice_tuple_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) ((v1, v2, v3, v4) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun v1 ->
      map_statement env v1
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_call_expression (env : env) ((v1, v2) : CST.call_expression) =
  let v1 = map_expression env v1 in
  let v2 = map_argument_list env v2 in
  R.Tuple [v1; v2]

and map_deep_ellipsis (env : env) ((v1, v2, v3) : CST.deep_ellipsis) =
  let v1 = (* "<..." *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "...>" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Tuple_exp x -> R.Case ("Tuple_exp",
      map_tuple_expression env x
    )
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `Un_exp x -> R.Case ("Un_exp",
      map_unary_expression env x
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `If_exp x -> R.Case ("If_exp",
      map_if_expression env x
    )
  | `Loop_exp x -> R.Case ("Loop_exp",
      map_loop_expression env x
    )
  | `Match_exp x -> R.Case ("Match_exp",
      map_match_expression env x
    )
  | `Sele_exp x -> R.Case ("Sele_exp",
      map_selector_expression env x
    )
  | `Call_exp x -> R.Case ("Call_exp",
      map_call_expression env x
    )
  | `Qual_name x -> R.Case ("Qual_name",
      map_qualified_name env x
    )
  | `Choice_true x -> R.Case ("Choice_true",
      map_literal_expression env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips x -> R.Case ("Deep_ellips",
      map_deep_ellipsis env x
    )
  )

and map_if_expression (env : env) ((v1, v2, v3, v4) : CST.if_expression) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_simple_expression env v2 in
  let v3 = map_block env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "else" *) token env v1 in
        let v2 =
          (match v2 with
          | `If_exp x -> R.Case ("If_exp",
              map_if_expression env x
            )
          | `Blk x -> R.Case ("Blk",
              map_block env x
            )
          )
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_loop_expression (env : env) ((v1, v2) : CST.loop_expression) =
  let v1 = (* "loop" *) token env v1 in
  let v2 = map_block env v2 in
  R.Tuple [v1; v2]

and map_match_case (env : env) ((v1, v2, v3) : CST.match_case) =
  let v1 = map_match_case_pattern env v1 in
  let v2 = (* "=>" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_match_cases (env : env) ((v1, v2, v3, v4) : CST.match_cases) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_match_case env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_match_case env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_match_expression (env : env) ((v1, v2, v3) : CST.match_expression) =
  let v1 = (* "match" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = map_match_cases env v3 in
  R.Tuple [v1; v2; v3]

and map_selector_expression (env : env) (x : CST.selector_expression) =
  (match x with
  | `Choice_tuple_exp_DOT_choice_pat_7fdeb71 (v1, v2, v3) -> R.Case ("Choice_tuple_exp_DOT_choice_pat_7fdeb71",
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_tuple_exp_DOT_ellips (v1, v2, v3) -> R.Case ("Choice_tuple_exp_DOT_ellips",
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "..." *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_simple_expression (env : env) (x : CST.simple_expression) =
  (match x with
  | `Tuple_exp x -> R.Case ("Tuple_exp",
      map_tuple_expression env x
    )
  | `Un_exp x -> R.Case ("Un_exp",
      map_unary_expression env x
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `If_exp x -> R.Case ("If_exp",
      map_if_expression env x
    )
  | `Loop_exp x -> R.Case ("Loop_exp",
      map_loop_expression env x
    )
  | `Match_exp x -> R.Case ("Match_exp",
      map_match_expression env x
    )
  | `Sele_exp x -> R.Case ("Sele_exp",
      map_selector_expression env x
    )
  | `Call_exp x -> R.Case ("Call_exp",
      map_call_expression env x
    )
  | `Qual_name x -> R.Case ("Qual_name",
      map_qualified_name env x
    )
  | `Choice_true x -> R.Case ("Choice_true",
      map_literal_expression env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips x -> R.Case ("Deep_ellips",
      map_deep_ellipsis env x
    )
  )

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Let_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Let_stmt",
      let v1 = (* "let" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "mut" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_pattern env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_type_ env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = (* "=" *) token env v5 in
      let v6 = map_expression env v6 in
      let v7 = (* ";" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Assign_stmt (v1, v2, v3, v4) -> R.Case ("Assign_stmt",
      let v1 =
        (match v1 with
        | `Choice_pat_7fdeb71 x -> R.Case ("Choice_pat_7fdeb71",
            map_name env x
          )
        | `Wild tok -> R.Case ("Wild",
            (* "_" *) token env tok
          )
        )
      in
      let v2 =
        (match v2 with
        | `STAREQ tok -> R.Case ("STAREQ",
            (* "*=" *) token env tok
          )
        | `SLASHEQ tok -> R.Case ("SLASHEQ",
            (* "/=" *) token env tok
          )
        | `PERCEQ tok -> R.Case ("PERCEQ",
            (* "%=" *) token env tok
          )
        | `PLUSEQ tok -> R.Case ("PLUSEQ",
            (* "+=" *) token env tok
          )
        | `DASHEQ tok -> R.Case ("DASHEQ",
            (* "-=" *) token env tok
          )
        | `EQ tok -> R.Case ("EQ",
            (* "=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Ret_stmt (v1, v2, v3) -> R.Case ("Ret_stmt",
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Brk_stmt (v1, v2, v3) -> R.Case ("Brk_stmt",
      let v1 = (* "break" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `If_exp x -> R.Case ("If_exp",
      map_if_expression env x
    )
  | `Loop_exp x -> R.Case ("Loop_exp",
      map_loop_expression env x
    )
  | `Match_exp x -> R.Case ("Match_exp",
      map_match_expression env x
    )
  | `Choice_tuple_exp_SEMI (v1, v2) -> R.Case ("Choice_tuple_exp_SEMI",
      let v1 = map_expression env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips x -> R.Case ("Deep_ellips",
      map_deep_ellipsis env x
    )
  )

and map_tuple_expression (env : env) ((v1, v2, v3, v4, v5) : CST.tuple_expression) =
  let v1 = (* "(" *) token env v1 in
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
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_unary_expression (env : env) ((v1, v2) : CST.unary_expression) =
  let v1 =
    (match v1 with
    | `BANG tok -> R.Case ("BANG",
        (* "!" *) token env tok
      )
    | `STAR tok -> R.Case ("STAR",
        (* "*" *) token env tok
      )
    | `DASH tok -> R.Case ("DASH",
        (* "-" *) token env tok
      )
    )
  in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

let map_member_declaration_list (env : env) ((v1, v2, v3, v4) : CST.member_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_member_declaration env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_member_declaration env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_parameter_list (env : env) ((v1, v2, v3, v4) : CST.parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_parameter_declaration env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_parameter_declaration env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_type_parameter_declaration (env : env) (x : CST.type_parameter_declaration) =
  (match x with
  | `Choice_pat_7fdeb71 x -> R.Case ("Choice_pat_7fdeb71",
      map_name env x
    )
  | `Type_const_decl (v1, v2, v3, v4) -> R.Case ("Type_const_decl",
      let v1 = (* "const" *) token env v1 in
      let v2 = map_name env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Type_impl_decl (v1, v2, v3, v4) -> R.Case ("Type_impl_decl",
      let v1 = (* "impl" *) token env v1 in
      let v2 = map_name env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_type_parameter_list (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameter_list) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_parameter_declaration env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter_declaration env v2 in
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
  let v5 = (* ">" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_function_signature (env : env) ((v1, v2, v3, v4, v5, v6) : CST.function_signature) =
  let v1 = R.List (List.map (map_attribute_list env) v1) in
  let v2 = (* "fn" *) token env v2 in
  let v3 = map_name env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_parameter_list env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "->" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_trait_function (env : env) ((v1, v2) : CST.trait_function) =
  let v1 = map_function_signature env v1 in
  let v2 =
    (match v2 with
    | `Blk x -> R.Case ("Blk",
        map_block env x
      )
    | `SEMI tok -> R.Case ("SEMI",
        (* ";" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

let map_trait_body (env : env) ((v1, v2, v3) : CST.trait_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_trait_function env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let rec map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Import_decl (v1, v2, v3, v4) -> R.Case ("Import_decl",
      let v1 = (* "use" *) token env v1 in
      let v2 = map_path env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "as" *) token env v1 in
            let v2 = map_name env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Module_decl (v1, v2, v3, v4) -> R.Case ("Module_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = (* "mod" *) token env v2 in
      let v3 = map_name env v3 in
      let v4 =
        (match v4 with
        | `SEMI tok -> R.Case ("SEMI",
            (* ";" *) token env tok
          )
        | `Module_body x -> R.Case ("Module_body",
            map_module_body env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Typeas_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Typeas_decl",
      let v1 = (* "type" *) token env v1 in
      let v2 = map_name env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_parameter_list env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_type_ env v5 in
      let v6 = (* ";" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Const_decl (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Const_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = (* "const" *) token env v2 in
      let v3 = map_name env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_type_ env v5 in
      let v6 = (* "=" *) token env v6 in
      let v7 = map_expression env v7 in
      let v8 = (* ";" *) token env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Trait_decl (v1, v2, v3, v4, v5) -> R.Case ("Trait_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = (* "trait" *) token env v2 in
      let v3 = map_name env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_parameter_list env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_trait_body env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Struct_decl (v1, v2, v3, v4, v5) -> R.Case ("Struct_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = (* "struct" *) token env v2 in
      let v3 = map_name env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_parameter_list env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | `Member_decl_list x -> R.Case ("Member_decl_list",
            map_member_declaration_list env x
          )
        | `SEMI tok -> R.Case ("SEMI",
            (* ";" *) token env tok
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Enum_decl (v1, v2, v3, v4, v5) -> R.Case ("Enum_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = (* "enum" *) token env v2 in
      let v3 = map_name env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_parameter_list env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_member_declaration_list env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Choice_impl_base x -> R.Case ("Choice_impl_base",
      map_impl_declaration env x
    )
  | `Func_decl (v1, v2) -> R.Case ("Func_decl",
      let v1 = map_function_signature env v1 in
      let v2 = map_block env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_impl_body (env : env) ((v1, v2, v3) : CST.impl_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_declaration env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_impl_declaration (env : env) (x : CST.impl_declaration) =
  (match x with
  | `Impl_base (v1, v2, v3, v4, v5) -> R.Case ("Impl_base",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = (* "impl" *) token env v2 in
      let v3 = map_name env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_parameter_list env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_impl_body env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Impl_trait (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Impl_trait",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = (* "impl" *) token env v2 in
      let v3 = map_name env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_parameter_list env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "of" *) token env v5 in
      let v6 = map_qualified_name env v6 in
      let v7 = map_impl_body env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  )

and map_module_body (env : env) ((v1, v2, v3) : CST.module_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_declaration env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_source_file (env : env) (x : CST.source_file) =
  (match x with
  | `Rep_choice_import_decl xs -> R.Case ("Rep_choice_import_decl",
      R.List (List.map (map_declaration env) xs)
    )
  | `Semg_exp (v1, v2) -> R.Case ("Semg_exp",
      let v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Semg_stmt (v1, v2) -> R.Case ("Semg_stmt",
      let v1 = (* "__SEMGREP_STATEMENT" *) token env v1 in
      let v2 = R.List (List.map (map_statement env) v2) in
      R.Tuple [v1; v2]
    )
  )

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_source_file () root
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

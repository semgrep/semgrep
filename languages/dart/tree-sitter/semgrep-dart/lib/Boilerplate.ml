(**
   Boilerplate to be used as a template when mapping the dart CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) = R.Token tok
let blank (env : env) () = R.Tuple []

let map_break_builtin (env : env) (tok : CST.break_builtin) =
  (* break_builtin *) token env tok

let map_anon_choice_async_725f72f (env : env)
    (x : CST.anon_choice_async_725f72f) =
  match x with
  | `Async tok -> R.Case ("Async", (* "async" *) token env tok)
  | `Asyn tok -> R.Case ("Asyn", (* "async*" *) token env tok)
  | `Sync tok -> R.Case ("Sync", (* "sync*" *) token env tok)

let map_bitwise_operator (env : env) (x : CST.bitwise_operator) =
  match x with
  | `AMP tok -> R.Case ("AMP", (* "&" *) token env tok)
  | `HAT tok -> R.Case ("HAT", (* "^" *) token env tok)
  | `BAR tok -> R.Case ("BAR", (* "|" *) token env tok)

let map_increment_operator (env : env) (tok : CST.increment_operator) =
  (* increment_operator *) token env tok

let map_unused_escape_sequence (env : env) (tok : CST.unused_escape_sequence) =
  (* unused_escape_sequence *) token env tok

let map_template_chars_single (env : env) (tok : CST.template_chars_single) =
  (* template_chars_single *) token env tok

let map_pat_05bf793 (env : env) (tok : CST.pat_05bf793) =
  (* pattern [^*]*\*+([^/*][^*]*\*+)* *) token env tok

let map_as_operator (env : env) (tok : CST.as_operator) =
  (* as_operator *) token env tok

let map_documentation_block_comment (env : env)
    (tok : CST.documentation_block_comment) =
  (* documentation_block_comment *) token env tok

let map_pat_d6c261f (env : env) (tok : CST.pat_d6c261f) =
  (* pattern ([^/\n].*\
     )? *)
  token env tok

let map_block_comment (env : env) (tok : CST.block_comment) =
  (* block_comment *) token env tok

let map_pat_4fd4a56 (env : env) (tok : CST.pat_4fd4a56) =
  (* pattern .* *) token env tok

let map_assert_builtin (env : env) (tok : CST.assert_builtin) =
  (* assert_builtin *) token env tok

let map_semgrep_named_ellipsis (env : env) (tok : CST.semgrep_named_ellipsis) =
  (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok

let map_const_builtin (env : env) (tok : CST.const_builtin) =
  (* const_builtin *) token env tok

let map_final_builtin (env : env) (tok : CST.final_builtin) =
  (* final_builtin *) token env tok

let map_multiplicative_operator_ (env : env) (x : CST.multiplicative_operator_)
    =
  match x with
  | `STAR tok -> R.Case ("STAR", (* "*" *) token env tok)
  | `SLASH tok -> R.Case ("SLASH", (* "/" *) token env tok)
  | `PERC tok -> R.Case ("PERC", (* "%" *) token env tok)
  | `TILDESLASH tok -> R.Case ("TILDESLASH", (* "~/" *) token env tok)

let map_template_chars_single_single (env : env)
    (tok : CST.template_chars_single_single) =
  (* template_chars_single_single *) token env tok

let map_template_chars_double (env : env) (tok : CST.template_chars_double) =
  (* template_chars_double *) token env tok

let map_semicolon (env : env) (v1 : CST.semicolon) = (* ";" *) token env v1

let map_relational_operator (env : env) (x : CST.relational_operator) =
  match x with
  | `LT tok -> R.Case ("LT", (* "<" *) token env tok)
  | `GT tok -> R.Case ("GT", (* ">" *) token env tok)
  | `LTEQ tok -> R.Case ("LTEQ", (* "<=" *) token env tok)
  | `GTEQ tok -> R.Case ("GTEQ", (* ">=" *) token env tok)

let map_decimal_floating_point_literal (env : env)
    (tok : CST.decimal_floating_point_literal) =
  (* decimal_floating_point_literal *) token env tok

let map_tok_is (env : env) (tok : CST.tok_is) = (* tok_is *) token env tok

let map_void_type (env : env) (tok : CST.void_type) =
  (* void_type *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z_$][\w$]* *) token env tok

let map_identifier_dollar_escaped (env : env)
    (tok : CST.identifier_dollar_escaped) =
  (* pattern ([a-zA-Z_]|(\\\$))([\w]|(\\\$))* *) token env tok

let map_pat_a3d33dc (env : env) (tok : CST.pat_a3d33dc) =
  (* pattern [^a-zA-Z_{] *) token env tok

let map_equality_operator (env : env) (tok : CST.equality_operator) =
  (* equality_operator *) token env tok

let map_shift_operator_ (env : env) (x : CST.shift_operator_) =
  match x with
  | `LTLT tok -> R.Case ("LTLT", (* "<<" *) token env tok)
  | `GTGT tok -> R.Case ("GTGT", (* ">>" *) token env tok)
  | `GTGTGT tok -> R.Case ("GTGTGT", (* ">>>" *) token env tok)

let map_hex_integer_literal (env : env) (tok : CST.hex_integer_literal) =
  (* hex_integer_literal *) token env tok

let map_pat_0017fb0 (env : env) (tok : CST.pat_0017fb0) =
  (* pattern .+ *) token env tok

let map_template_chars_double_single (env : env)
    (tok : CST.template_chars_double_single) =
  (* template_chars_double_single *) token env tok

let map_template_chars_raw_slash (env : env)
    (tok : CST.template_chars_raw_slash) =
  (* template_chars_raw_slash *) token env tok

let map_assignment_operator (env : env) (x : CST.assignment_operator) =
  match x with
  | `EQ tok -> R.Case ("EQ", (* "=" *) token env tok)
  | `PLUSEQ tok -> R.Case ("PLUSEQ", (* "+=" *) token env tok)
  | `DASHEQ tok -> R.Case ("DASHEQ", (* "-=" *) token env tok)
  | `STAREQ tok -> R.Case ("STAREQ", (* "*=" *) token env tok)
  | `SLASHEQ tok -> R.Case ("SLASHEQ", (* "/=" *) token env tok)
  | `PERCEQ tok -> R.Case ("PERCEQ", (* "%=" *) token env tok)
  | `TILDESLASHEQ tok -> R.Case ("TILDESLASHEQ", (* "~/=" *) token env tok)
  | `LTLTEQ tok -> R.Case ("LTLTEQ", (* "<<=" *) token env tok)
  | `GTGTEQ tok -> R.Case ("GTGTEQ", (* ">>=" *) token env tok)
  | `GTGTGTEQ tok -> R.Case ("GTGTGTEQ", (* ">>>=" *) token env tok)
  | `AMPEQ tok -> R.Case ("AMPEQ", (* "&=" *) token env tok)
  | `HATEQ tok -> R.Case ("HATEQ", (* "^=" *) token env tok)
  | `BAREQ tok -> R.Case ("BAREQ", (* "|=" *) token env tok)
  | `QMARKQMARKEQ tok -> R.Case ("QMARKQMARKEQ", (* "??=" *) token env tok)

let map_decimal_integer_literal (env : env) (tok : CST.decimal_integer_literal)
    =
  (* decimal_integer_literal *) token env tok

let map_additive_operator_ (env : env) (tok : CST.additive_operator_) =
  (* additive_operator_ *) token env tok

let map_case_builtin (env : env) (tok : CST.case_builtin) =
  (* case_builtin *) token env tok

let map_bitwise_operator_ (env : env) (x : CST.bitwise_operator_) =
  map_bitwise_operator env x

let map_final_or_const (env : env) (x : CST.final_or_const) =
  match x with
  | `Final_buil tok -> R.Case ("Final_buil", (* final_builtin *) token env tok)
  | `Const_buil tok -> R.Case ("Const_buil", (* const_builtin *) token env tok)

let map_multiplicative_operator (env : env) (x : CST.multiplicative_operator) =
  map_multiplicative_operator_ env x

let rec map_module_name (env : env) (x : CST.module_name) =
  match x with
  | `Id tok -> R.Case ("Id", (* pattern [a-zA-Z_$][\w$]* *) token env tok)
  | `Module_name_DOT_id (v1, v2, v3) ->
      R.Case
        ( "Module_name_DOT_id",
          let v1 = map_module_name env v1 in
          let v2 = (* "." *) token env v2 in
          let v3 = (* pattern [a-zA-Z_$][\w$]* *) token env v3 in
          R.Tuple [ v1; v2; v3 ] )

let map_identifier_list_ (env : env) ((v1, v2) : CST.identifier_list_) =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

let rec map_ambiguous_name (env : env) (x : CST.ambiguous_name) =
  match x with
  | `Id tok -> R.Case ("Id", (* pattern [a-zA-Z_$][\w$]* *) token env tok)
  | `Scoped_id (v1, v2, v3) ->
      R.Case
        ( "Scoped_id",
          let v1 = map_ambiguous_name env v1 in
          let v2 = (* "." *) token env v2 in
          let v3 = (* pattern [a-zA-Z_$][\w$]* *) token env v3 in
          R.Tuple [ v1; v2; v3 ] )

let map_catch_clause (env : env) ((v1, v2, v3, v4, v5) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* pattern [a-zA-Z_$][\w$]* *) token env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = (* "," *) token env v1 in
              let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_identifier_list (env : env) ((v1, v2) : CST.identifier_list) =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

let map_type_dot_identifier (env : env) ((v1, v2) : CST.type_dot_identifier) =
  let v1 = (* "." *) token env v1 in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
  R.Tuple [ v1; v2 ]

let map_dot_identifier (env : env) ((v1, v2) : CST.dot_identifier) =
  let v1 = (* "." *) token env v1 in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
  R.Tuple [ v1; v2 ]

let map_label (env : env) ((v1, v2) : CST.label) =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  R.Tuple [ v1; v2 ]

let map_sub_string_test (env : env) ((v1, v2) : CST.sub_string_test) =
  let v1 = (* "$" *) token env v1 in
  let v2 = map_pat_a3d33dc env v2 in
  R.Tuple [ v1; v2 ]

let map_shift_operator (env : env) (x : CST.shift_operator) =
  map_shift_operator_ env x

let map_script_tag (env : env) ((v1, v2, v3) : CST.script_tag) =
  let v1 = (* "#!" *) token env v1 in
  let v2 = map_pat_0017fb0 env v2 in
  let v3 = (* "\n" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_external_and_static (env : env) ((v1, v2) : CST.external_and_static) =
  let v1 = (* "external" *) token env v1 in
  let v2 =
    match v2 with
    | Some tok -> R.Option (Some ((* "static" *) token env tok))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

let map_is_operator (env : env) ((v1, v2) : CST.is_operator) =
  let v1 = map_tok_is env v1 in
  let v2 =
    match v2 with
    | Some tok -> R.Option (Some ((* "!" *) token env tok))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

let map_prefix_operator (env : env) (x : CST.prefix_operator) =
  match x with
  | `Minus_op tok -> R.Case ("Minus_op", (* "-" *) token env tok)
  | `Nega_op tok -> R.Case ("Nega_op", (* "!" *) token env tok)
  | `Tilde_op tok -> R.Case ("Tilde_op", (* "~" *) token env tok)

let map_combinator (env : env) (x : CST.combinator) =
  match x with
  | `Show_id_list (v1, v2) ->
      R.Case
        ( "Show_id_list",
          let v1 = (* "show" *) token env v1 in
          let v2 = map_identifier_list env v2 in
          R.Tuple [ v1; v2 ] )
  | `Hide_id_list (v1, v2) ->
      R.Case
        ( "Hide_id_list",
          let v1 = (* "hide" *) token env v1 in
          let v2 = map_identifier_list env v2 in
          R.Tuple [ v1; v2 ] )

let map_type_name (env : env) ((v1, v2) : CST.type_name) =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> R.Option (Some (map_type_dot_identifier env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

let map_qualified (env : env) ((v1, v2) : CST.qualified) =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> R.Option (Some (map_dot_identifier env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

let map_dotted_identifier_list (env : env)
    ((v1, v2) : CST.dotted_identifier_list) =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) token env v1 in
  let v2 = R.List (List.map (map_dot_identifier env) v2) in
  R.Tuple [ v1; v2 ]

let map_raw_string_literal_double_quotes (env : env)
    ((v1, v2, v3) : CST.raw_string_literal_double_quotes) =
  let v1 = (* "r\"" *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun x ->
           match x with
           | `Temp_chars_double_single tok ->
               R.Case
                 ( "Temp_chars_double_single",
                   (* template_chars_double_single *) token env tok )
           | `SQUOT tok -> R.Case ("SQUOT", (* "'" *) token env tok)
           | `Temp_chars_raw_slash tok ->
               R.Case
                 ( "Temp_chars_raw_slash",
                   (* template_chars_raw_slash *) token env tok )
           | `Unused_esc_seq tok ->
               R.Case
                 ("Unused_esc_seq", (* unused_escape_sequence *) token env tok)
           | `Sub_str_test x ->
               R.Case ("Sub_str_test", map_sub_string_test env x)
           | `DOLLAR tok -> R.Case ("DOLLAR", (* "$" *) token env tok))
         v2)
  in
  let v3 = (* "\"" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_raw_string_literal_single_quotes_multiple (env : env)
    ((v1, v2, v3) : CST.raw_string_literal_single_quotes_multiple) =
  let v1 = (* "r'''" *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun x ->
           match x with
           | `Temp_chars_single tok ->
               R.Case
                 ("Temp_chars_single", (* template_chars_single *) token env tok)
           | `DQUOT tok -> R.Case ("DQUOT", (* "\"" *) token env tok)
           | `SQUOT tok -> R.Case ("SQUOT", (* "'" *) token env tok)
           | `Temp_chars_raw_slash tok ->
               R.Case
                 ( "Temp_chars_raw_slash",
                   (* template_chars_raw_slash *) token env tok )
           | `Unused_esc_seq tok ->
               R.Case
                 ("Unused_esc_seq", (* unused_escape_sequence *) token env tok)
           | `Sub_str_test x ->
               R.Case ("Sub_str_test", map_sub_string_test env x)
           | `DOLLAR tok -> R.Case ("DOLLAR", (* "$" *) token env tok))
         v2)
  in
  let v3 = (* "'''" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_raw_string_literal_double_quotes_multiple (env : env)
    ((v1, v2, v3) : CST.raw_string_literal_double_quotes_multiple) =
  let v1 = (* "r\"\"\"" *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun x ->
           match x with
           | `Temp_chars_double tok ->
               R.Case
                 ("Temp_chars_double", (* template_chars_double *) token env tok)
           | `SQUOT tok -> R.Case ("SQUOT", (* "'" *) token env tok)
           | `Temp_chars_raw_slash tok ->
               R.Case
                 ( "Temp_chars_raw_slash",
                   (* template_chars_raw_slash *) token env tok )
           | `DQUOT tok -> R.Case ("DQUOT", (* "\"" *) token env tok)
           | `Unused_esc_seq tok ->
               R.Case
                 ("Unused_esc_seq", (* unused_escape_sequence *) token env tok)
           | `Sub_str_test x ->
               R.Case ("Sub_str_test", map_sub_string_test env x)
           | `DOLLAR tok -> R.Case ("DOLLAR", (* "$" *) token env tok))
         v2)
  in
  let v3 = (* "\"\"\"" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_raw_string_literal_single_quotes (env : env)
    ((v1, v2, v3) : CST.raw_string_literal_single_quotes) =
  let v1 = (* "r'" *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun x ->
           match x with
           | `Temp_chars_single_single tok ->
               R.Case
                 ( "Temp_chars_single_single",
                   (* template_chars_single_single *) token env tok )
           | `DQUOT tok -> R.Case ("DQUOT", (* "\"" *) token env tok)
           | `Temp_chars_raw_slash tok ->
               R.Case
                 ( "Temp_chars_raw_slash",
                   (* template_chars_raw_slash *) token env tok )
           | `Unused_esc_seq tok ->
               R.Case
                 ("Unused_esc_seq", (* unused_escape_sequence *) token env tok)
           | `Sub_str_test x ->
               R.Case ("Sub_str_test", map_sub_string_test env x)
           | `DOLLAR tok -> R.Case ("DOLLAR", (* "$" *) token env tok))
         v2)
  in
  let v3 = (* "'" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_binary_operator (env : env) (x : CST.binary_operator) =
  match x with
  | `Mult_op x -> R.Case ("Mult_op", map_multiplicative_operator env x)
  | `Addi_op tok -> R.Case ("Addi_op", (* additive_operator_ *) token env tok)
  | `Shift_op x -> R.Case ("Shift_op", map_shift_operator env x)
  | `Rela_op x -> R.Case ("Rela_op", map_relational_operator env x)
  | `EQEQ tok -> R.Case ("EQEQ", (* "==" *) token env tok)
  | `Bitw_op_ x -> R.Case ("Bitw_op_", map_bitwise_operator_ env x)

let rec map_additive_expression (env : env) (x : CST.additive_expression) =
  match x with
  | `Real_exp_rep1_addi_op_real_exp (v1, v2) ->
      R.Case
        ( "Real_exp_rep1_addi_op_real_exp",
          let v1 = map_real_expression env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* additive_operator_ *) token env v1 in
                   let v2 = map_real_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )
  | `Super_rep1_addi_op_real_exp (v1, v2) ->
      R.Case
        ( "Super_rep1_addi_op_real_exp",
          let v1 = (* "super" *) token env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* additive_operator_ *) token env v1 in
                   let v2 = map_real_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )

and map_annotation_ (env : env) (x : CST.annotation_) =
  match x with
  | `Marker_anno (v1, v2) ->
      R.Case
        ( "Marker_anno",
          let v1 = (* "@" *) token env v1 in
          let v2 = map_ambiguous_name env v2 in
          R.Tuple [ v1; v2 ] )
  | `Anno (v1, v2, v3) ->
      R.Case
        ( "Anno",
          let v1 = (* "@" *) token env v1 in
          let v2 = map_ambiguous_name env v2 in
          let v3 = map_arguments env v3 in
          R.Tuple [ v1; v2; v3 ] )

and map_anon_arg_rep_COMMA_arg_eb223b2 (env : env)
    ((v1, v2) : CST.anon_arg_rep_COMMA_arg_eb223b2) =
  let v1 = map_argument env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_argument env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

and map_anon_elem_rep_COMMA_elem_opt_COMMA_4ec364f (env : env)
    ((v1, v2, v3) : CST.anon_elem_rep_COMMA_elem_opt_COMMA_4ec364f) =
  let v1 = map_element env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_element env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  let v3 =
    match v3 with
    | Some tok -> R.Option (Some ((* "," *) token env tok))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3 ]

and map_argument (env : env) (x : CST.argument) = map_expression env x

and map_argument_list (env : env) (x : CST.argument_list) =
  match x with
  | `Named_arg_rep_COMMA_named_arg (v1, v2) ->
      R.Case
        ( "Named_arg_rep_COMMA_named_arg",
          let v1 = map_named_argument env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "," *) token env v1 in
                   let v2 = map_named_argument env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )
  | `Arg_rep_COMMA_arg_rep_COMMA_named_arg_rep_COMMA_named_arg (v1, v2, v3) ->
      R.Case
        ( "Arg_rep_COMMA_arg_rep_COMMA_named_arg_rep_COMMA_named_arg",
          let v1 = map_argument env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "," *) token env v1 in
                   let v2 = map_argument env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          let v3 =
            R.List
              (List.map
                 (fun (v1, v2, v3) ->
                   let v1 = (* "," *) token env v1 in
                   let v2 = map_named_argument env v2 in
                   let v3 =
                     R.List
                       (List.map
                          (fun (v1, v2) ->
                            let v1 = (* "," *) token env v1 in
                            let v2 = map_named_argument env v2 in
                            R.Tuple [ v1; v2 ])
                          v3)
                   in
                   R.Tuple [ v1; v2; v3 ])
                 v3)
          in
          R.Tuple [ v1; v2; v3 ] )

and map_argument_part (env : env) ((v1, v2) : CST.argument_part) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_type_arguments env x))
    | None -> R.Option None
  in
  let v2 = map_arguments env v2 in
  R.Tuple [ v1; v2 ]

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = map_argument_list env v1 in
              let v2 =
                match v2 with
                | Some tok -> R.Option (Some ((* "," *) token env tok))
                | None -> R.Option None
              in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_assertion (env : env) ((v1, v2, v3, v4, v5, v6) : CST.assertion) =
  let v1 = (* assert_builtin *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_argument env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = (* "," *) token env v1 in
              let v2 = map_argument env v2 in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  let v5 =
    match v5 with
    | Some tok -> R.Option (Some ((* "," *) token env tok))
    | None -> R.Option None
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [ v1; v2; v3; v4; v5; v6 ]

and map_assignable_expression (env : env) (x : CST.assignable_expression) =
  match x with
  | `Prim_assi_sele_part (v1, v2) ->
      R.Case
        ( "Prim_assi_sele_part",
          let v1 = map_primary env v1 in
          let v2 = map_assignable_selector_part env v2 in
          R.Tuple [ v1; v2 ] )
  | `Super_unco_assi_sele (v1, v2) ->
      R.Case
        ( "Super_unco_assi_sele",
          let v1 = (* "super" *) token env v1 in
          let v2 = map_unconditional_assignable_selector env v2 in
          R.Tuple [ v1; v2 ] )
  | `Cons_invo_assi_sele_part (v1, v2) ->
      R.Case
        ( "Cons_invo_assi_sele_part",
          let v1 = map_constructor_invocation env v1 in
          let v2 = map_assignable_selector_part env v2 in
          R.Tuple [ v1; v2 ] )
  | `Id tok -> R.Case ("Id", (* pattern [a-zA-Z_$][\w$]* *) token env tok)

and map_assignable_selector (env : env) (x : CST.assignable_selector) =
  match x with
  | `Unco_assi_sele x ->
      R.Case ("Unco_assi_sele", map_unconditional_assignable_selector env x)
  | `Cond_assi_sele (v1, v2) ->
      R.Case
        ( "Cond_assi_sele",
          let v1 = (* "?." *) token env v1 in
          let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
          R.Tuple [ v1; v2 ] )

and map_assignable_selector_part (env : env)
    ((v1, v2) : CST.assignable_selector_part) =
  let v1 = R.List (List.map (map_selector env) v1) in
  let v2 = map_assignable_selector env v2 in
  R.Tuple [ v1; v2 ]

and map_assignment_expression (env : env)
    ((v1, v2, v3) : CST.assignment_expression) =
  let v1 = map_assignable_expression env v1 in
  let v2 = map_assignment_operator env v2 in
  let v3 = map_argument env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_bitwise_and_expression (env : env) (x : CST.bitwise_and_expression) =
  match x with
  | `Real_exp_rep1_AMP_real_exp (v1, v2) ->
      R.Case
        ( "Real_exp_rep1_AMP_real_exp",
          let v1 = map_real_expression env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "&" *) token env v1 in
                   let v2 = map_real_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )
  | `Super_rep1_AMP_real_exp (v1, v2) ->
      R.Case
        ( "Super_rep1_AMP_real_exp",
          let v1 = (* "super" *) token env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "&" *) token env v1 in
                   let v2 = map_real_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )

and map_bitwise_or_expression (env : env) (x : CST.bitwise_or_expression) =
  match x with
  | `Real_exp_rep1_BAR_real_exp (v1, v2) ->
      R.Case
        ( "Real_exp_rep1_BAR_real_exp",
          let v1 = map_real_expression env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "|" *) token env v1 in
                   let v2 = map_real_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )
  | `Super_rep1_BAR_real_exp (v1, v2) ->
      R.Case
        ( "Super_rep1_BAR_real_exp",
          let v1 = (* "super" *) token env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "|" *) token env v1 in
                   let v2 = map_real_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )

and map_bitwise_xor_expression (env : env) (x : CST.bitwise_xor_expression) =
  match x with
  | `Real_exp_rep1_HAT_real_exp (v1, v2) ->
      R.Case
        ( "Real_exp_rep1_HAT_real_exp",
          let v1 = map_real_expression env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "^" *) token env v1 in
                   let v2 = map_real_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )
  | `Super_rep1_HAT_real_exp (v1, v2) ->
      R.Case
        ( "Super_rep1_HAT_real_exp",
          let v1 = (* "super" *) token env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "^" *) token env v1 in
                   let v2 = map_real_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_cascade_assignment_section (env : env)
    ((v1, v2) : CST.cascade_assignment_section) =
  let v1 = map_assignment_operator env v1 in
  let v2 = map_expression_without_cascade env v2 in
  R.Tuple [ v1; v2 ]

and map_cascade_section (env : env) ((v1, v2, v3, v4, v5) : CST.cascade_section)
    =
  let v1 =
    match v1 with
    | `DOTDOT tok -> R.Case ("DOTDOT", (* ".." *) token env tok)
    | `QMARKDOTDOT tok -> R.Case ("QMARKDOTDOT", (* "?.." *) token env tok)
  in
  let v2 = map_cascade_selector env v2 in
  let v3 = R.List (List.map (map_argument_part env) v3) in
  let v4 = R.List (List.map (map_cascade_subsection env) v4) in
  let v5 =
    match v5 with
    | Some x -> R.Option (Some (map_cascade_assignment_section env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4; v5 ]

and map_cascade_selector (env : env) (x : CST.cascade_selector) =
  match x with
  | `Opt_null_type_LBRACK_exp_RBRACK (v1, v2, v3, v4) ->
      R.Case
        ( "Opt_null_type_LBRACK_exp_RBRACK",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "?" *) token env tok))
            | None -> R.Option None
          in
          let v2 = (* "[" *) token env v2 in
          let v3 = map_argument env v3 in
          let v4 = (* "]" *) token env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `Id tok -> R.Case ("Id", (* pattern [a-zA-Z_$][\w$]* *) token env tok)

and map_cascade_subsection (env : env) ((v1, v2) : CST.cascade_subsection) =
  let v1 = map_assignable_selector env v1 in
  let v2 = R.List (List.map (map_argument_part env) v2) in
  R.Tuple [ v1; v2 ]

and map_constructor_invocation (env : env)
    ((v1, v2, v3, v4, v5) : CST.constructor_invocation) =
  let v1 = map_type_name env v1 in
  let v2 = map_type_arguments env v2 in
  let v3 = (* "." *) token env v3 in
  let v4 = (* pattern [a-zA-Z_$][\w$]* *) token env v4 in
  let v5 = map_arguments env v5 in
  R.Tuple [ v1; v2; v3; v4; v5 ]

and map_constructor_param (env : env)
    ((v1, v2, v3, v4, v5) : CST.constructor_param) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_final_const_var_or_type env x))
    | None -> R.Option None
  in
  let v2 = (* "this" *) token env v2 in
  let v3 = (* "." *) token env v3 in
  let v4 = (* pattern [a-zA-Z_$][\w$]* *) token env v4 in
  let v5 =
    match v5 with
    | Some x -> R.Option (Some (map_formal_parameter_part env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4; v5 ]

and map_declared_identifier (env : env)
    ((v1, v2, v3, v4) : CST.declared_identifier) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_metadata env x))
    | None -> R.Option None
  in
  let v2 =
    match v2 with
    | Some tok -> R.Option (Some ((* "covariant" *) token env tok))
    | None -> R.Option None
  in
  let v3 = map_final_const_var_or_type env v3 in
  let v4 = (* pattern [a-zA-Z_$][\w$]* *) token env v4 in
  R.Tuple [ v1; v2; v3; v4 ]

and map_default_formal_parameter (env : env)
    ((v1, v2) : CST.default_formal_parameter) =
  let v1 = map_formal_parameter env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = (* "=" *) token env v1 in
              let v2 = map_argument env v2 in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

and map_default_named_parameter (env : env) (x : CST.default_named_parameter) =
  match x with
  | `Opt_requ_formal_param_opt_EQ_exp (v1, v2, v3) ->
      R.Case
        ( "Opt_requ_formal_param_opt_EQ_exp",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "required" *) token env tok))
            | None -> R.Option None
          in
          let v2 = map_formal_parameter env v2 in
          let v3 =
            match v3 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = (* "=" *) token env v1 in
                      let v2 = map_argument env v2 in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2; v3 ] )
  | `Opt_requ_formal_param_opt_COLON_exp (v1, v2, v3) ->
      R.Case
        ( "Opt_requ_formal_param_opt_COLON_exp",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "required" *) token env tok))
            | None -> R.Option None
          in
          let v2 = map_formal_parameter env v2 in
          let v3 =
            match v3 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = (* ":" *) token env v1 in
                      let v2 = map_argument env v2 in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2; v3 ] )

and map_element (env : env) (x : CST.element) =
  match x with
  | `Exp x -> R.Case ("Exp", map_argument env x)
  | `Pair (v1, v2, v3) ->
      R.Case
        ( "Pair",
          let v1 = map_argument env v1 in
          let v2 = (* ":" *) token env v2 in
          let v3 = map_argument env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Spread_elem (v1, v2, v3) ->
      R.Case
        ( "Spread_elem",
          let v1 = (* "..." *) token env v1 in
          let v2 =
            match v2 with
            | Some tok -> R.Option (Some ((* "?" *) token env tok))
            | None -> R.Option None
          in
          let v3 = map_argument env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `If_elem (v1, v2, v3, v4) ->
      R.Case
        ( "If_elem",
          let v1 = (* "if" *) token env v1 in
          let v2 = map_parenthesized_expression env v2 in
          let v3 = map_element env v3 in
          let v4 =
            match v4 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = (* "else" *) token env v1 in
                      let v2 = map_element env v2 in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `For_elem (v1, v2, v3, v4, v5, v6) ->
      R.Case
        ( "For_elem",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "await" *) token env tok))
            | None -> R.Option None
          in
          let v2 = (* "for" *) token env v2 in
          let v3 = (* "(" *) token env v3 in
          let v4 = map_for_loop_parts env v4 in
          let v5 = (* ")" *) token env v5 in
          let v6 = map_element env v6 in
          R.Tuple [ v1; v2; v3; v4; v5; v6 ] )

and map_equality_expression (env : env) (x : CST.equality_expression) =
  match x with
  | `Real_exp_equa_op_real_exp (v1, v2, v3) ->
      R.Case
        ( "Real_exp_equa_op_real_exp",
          let v1 = map_real_expression env v1 in
          let v2 = (* equality_operator *) token env v2 in
          let v3 = map_real_expression env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Super_equa_op_real_exp (v1, v2, v3) ->
      R.Case
        ( "Super_equa_op_real_exp",
          let v1 = (* "super" *) token env v1 in
          let v2 = (* equality_operator *) token env v2 in
          let v3 = map_real_expression env v3 in
          R.Tuple [ v1; v2; v3 ] )

and map_expression (env : env) (x : CST.expression) =
  match x with
  | `Choice_assign_exp x ->
      R.Case
        ( "Choice_assign_exp",
          match x with
          | `Assign_exp x ->
              R.Case ("Assign_exp", map_assignment_expression env x)
          | `Throw_exp x -> R.Case ("Throw_exp", map_throw_expression env x)
          | `Real_exp_rep_casc_sect (v1, v2) ->
              R.Case
                ( "Real_exp_rep_casc_sect",
                  let v1 = map_real_expression env v1 in
                  let v2 = R.List (List.map (map_cascade_section env) v2) in
                  R.Tuple [ v1; v2 ] ) )
  | `Semg_ellips tok -> R.Case ("Semg_ellips", (* "..." *) token env tok)
  | `Semg_named_ellips tok ->
      R.Case
        ( "Semg_named_ellips",
          (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok )
  | `Deep_ellips (v1, v2, v3) ->
      R.Case
        ( "Deep_ellips",
          let v1 = (* "<..." *) token env v1 in
          let v2 = map_argument env v2 in
          let v3 = (* "...>" *) token env v3 in
          R.Tuple [ v1; v2; v3 ] )

and map_expression_statement (env : env) (x : CST.expression_statement) =
  match x with
  | `Exp_semi (v1, v2) ->
      R.Case
        ( "Exp_semi",
          let v1 = map_argument env v1 in
          let v2 = map_semicolon env v2 in
          R.Tuple [ v1; v2 ] )
  | `Semg_ellips tok -> R.Case ("Semg_ellips", (* "..." *) token env tok)

and map_expression_without_cascade (env : env)
    (x : CST.expression_without_cascade) =
  match x with
  | `Assign_exp_with_casc (v1, v2, v3) ->
      R.Case
        ( "Assign_exp_with_casc",
          let v1 = map_assignable_expression env v1 in
          let v2 = map_assignment_operator env v2 in
          let v3 = map_expression_without_cascade env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Real_exp x -> R.Case ("Real_exp", map_real_expression env x)
  | `Throw_exp_with_casc (v1, v2) ->
      R.Case
        ( "Throw_exp_with_casc",
          let v1 = (* "throw" *) token env v1 in
          let v2 = map_expression_without_cascade env v2 in
          R.Tuple [ v1; v2 ] )

and map_final_const_var_or_type (env : env) (x : CST.final_const_var_or_type) =
  match x with
  | `Opt_late_buil_final_buil_opt_type (v1, v2, v3) ->
      R.Case
        ( "Opt_late_buil_final_buil_opt_type",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "late" *) token env tok))
            | None -> R.Option None
          in
          let v2 = (* final_builtin *) token env v2 in
          let v3 =
            match v3 with
            | Some x -> R.Option (Some (map_type_ env x))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2; v3 ] )
  | `Const_buil_opt_type (v1, v2) ->
      R.Case
        ( "Const_buil_opt_type",
          let v1 = (* const_builtin *) token env v1 in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_type_ env x))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2 ] )
  | `Opt_late_buil_var_or_type (v1, v2) ->
      R.Case
        ( "Opt_late_buil_var_or_type",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "late" *) token env tok))
            | None -> R.Option None
          in
          let v2 = map_var_or_type env v2 in
          R.Tuple [ v1; v2 ] )

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_block env v2 in
  R.Tuple [ v1; v2 ]

and map_for_loop_parts (env : env) (x : CST.for_loop_parts) =
  match x with
  | `Choice_decl_id_in_exp (v1, v2, v3) ->
      R.Case
        ( "Choice_decl_id_in_exp",
          let v1 =
            match v1 with
            | `Decl_id x -> R.Case ("Decl_id", map_declared_identifier env x)
            | `Id tok ->
                R.Case ("Id", (* pattern [a-zA-Z_$][\w$]* *) token env tok)
          in
          let v2 = (* "in" *) token env v2 in
          let v3 = map_argument env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Opt_choice_local_var_decl_opt_exp_semi_opt_exp_rep_COMMA_exp
      (v1, v2, v3, v4) ->
      R.Case
        ( "Opt_choice_local_var_decl_opt_exp_semi_opt_exp_rep_COMMA_exp",
          let v1 =
            match v1 with
            | Some x ->
                R.Option
                  (Some
                     (match x with
                     | `Local_var_decl x ->
                         R.Case
                           ( "Local_var_decl",
                             map_local_variable_declaration env x )
                     | `Opt_exp_rep_COMMA_exp_semi (v1, v2) ->
                         R.Case
                           ( "Opt_exp_rep_COMMA_exp_semi",
                             let v1 =
                               match v1 with
                               | Some x ->
                                   R.Option
                                     (Some
                                        (map_anon_arg_rep_COMMA_arg_eb223b2 env
                                           x))
                               | None -> R.Option None
                             in
                             let v2 = map_semicolon env v2 in
                             R.Tuple [ v1; v2 ] )))
            | None -> R.Option None
          in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_argument env x))
            | None -> R.Option None
          in
          let v3 = map_semicolon env v3 in
          let v4 =
            match v4 with
            | Some x ->
                R.Option (Some (map_anon_arg_rep_COMMA_arg_eb223b2 env x))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2; v3; v4 ] )

and map_formal_parameter (env : env) (x : CST.formal_parameter) =
  match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips", (* "..." *) token env tok)
  | `Normal_formal_param (v1, v2) ->
      R.Case
        ( "Normal_formal_param",
          let v1 =
            match v1 with
            | Some x -> R.Option (Some (map_metadata env x))
            | None -> R.Option None
          in
          let v2 =
            match v2 with
            | `Func_formal_param x ->
                R.Case ("Func_formal_param", map_function_formal_parameter env x)
            | `Simple_formal_param x ->
                R.Case ("Simple_formal_param", map_simple_formal_parameter env x)
            | `Cons_param x -> R.Case ("Cons_param", map_constructor_param env x)
            | `Super_formal_param x ->
                R.Case ("Super_formal_param", map_super_formal_parameter env x)
          in
          R.Tuple [ v1; v2 ] )

and map_formal_parameter_list (env : env) (x : CST.formal_parameter_list) =
  map_strict_formal_parameter_list env x

and map_formal_parameter_part (env : env) ((v1, v2) : CST.formal_parameter_part)
    =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_type_parameters env x))
    | None -> R.Option None
  in
  let v2 = map_formal_parameter_list env v2 in
  R.Tuple [ v1; v2 ]

and map_function_body (env : env) (x : CST.function_body) =
  match x with
  | `Opt_async_EQGT_exp_semi (v1, v2, v3, v4) ->
      R.Case
        ( "Opt_async_EQGT_exp_semi",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "async" *) token env tok))
            | None -> R.Option None
          in
          let v2 = (* "=>" *) token env v2 in
          let v3 = map_argument env v3 in
          let v4 = map_semicolon env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `Opt_choice_async_blk (v1, v2) ->
      R.Case
        ( "Opt_choice_async_blk",
          let v1 =
            match v1 with
            | Some x -> R.Option (Some (map_anon_choice_async_725f72f env x))
            | None -> R.Option None
          in
          let v2 = map_block env v2 in
          R.Tuple [ v1; v2 ] )

and map_function_expression_body (env : env) (x : CST.function_expression_body)
    =
  match x with
  | `Opt_async_EQGT_exp (v1, v2, v3) ->
      R.Case
        ( "Opt_async_EQGT_exp",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "async" *) token env tok))
            | None -> R.Option None
          in
          let v2 = (* "=>" *) token env v2 in
          let v3 = map_argument env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Opt_choice_async_blk (v1, v2) ->
      R.Case
        ( "Opt_choice_async_blk",
          let v1 =
            match v1 with
            | Some x -> R.Option (Some (map_anon_choice_async_725f72f env x))
            | None -> R.Option None
          in
          let v2 = map_block env v2 in
          R.Tuple [ v1; v2 ] )

and map_function_formal_parameter (env : env)
    ((v1, v2, v3, v4, v5) : CST.function_formal_parameter) =
  let v1 =
    match v1 with
    | Some tok -> R.Option (Some ((* "covariant" *) token env tok))
    | None -> R.Option None
  in
  let v2 =
    match v2 with
    | Some x -> R.Option (Some (map_type_ env x))
    | None -> R.Option None
  in
  let v3 = (* pattern [a-zA-Z_$][\w$]* *) token env v3 in
  let v4 = map_formal_parameter_part env v4 in
  let v5 =
    match v5 with
    | Some tok -> R.Option (Some ((* "?" *) token env tok))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4; v5 ]

and map_function_signature (env : env)
    ((v1, v2, v3, v4) : CST.function_signature) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_type_ env x))
    | None -> R.Option None
  in
  let v2 =
    match v2 with
    | `Get tok -> R.Case ("Get", (* "get" *) token env tok)
    | `Set tok -> R.Case ("Set", (* "set" *) token env tok)
    | `Id tok -> R.Case ("Id", (* pattern [a-zA-Z_$][\w$]* *) token env tok)
  in
  let v3 = map_formal_parameter_part env v3 in
  let v4 =
    match v4 with
    | Some x -> R.Option (Some (map_native env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4 ]

and map_function_type (env : env) (x : CST.function_type) =
  match x with
  | `Func_type_tails x ->
      R.Case ("Func_type_tails", map_function_type_tails env x)
  | `Type_not_func_func_type_tails (v1, v2) ->
      R.Case
        ( "Type_not_func_func_type_tails",
          let v1 = map_type_not_function env v1 in
          let v2 = map_function_type_tails env v2 in
          R.Tuple [ v1; v2 ] )

and map_function_type_tail (env : env)
    ((v1, v2, v3, v4, v5) : CST.function_type_tail) =
  let v1 = (* "Function" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> R.Option (Some (map_type_parameters env x))
    | None -> R.Option None
  in
  let v3 =
    match v3 with
    | Some tok -> R.Option (Some ((* "?" *) token env tok))
    | None -> R.Option None
  in
  let v4 =
    match v4 with
    | Some x -> R.Option (Some (map_parameter_type_list env x))
    | None -> R.Option None
  in
  let v5 =
    match v5 with
    | Some tok -> R.Option (Some ((* "?" *) token env tok))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4; v5 ]

and map_function_type_tails (env : env) (xs : CST.function_type_tails) =
  R.List (List.map (map_function_type_tail env) xs)

and map_if_null_expression_ (env : env) (xs : CST.if_null_expression_) =
  R.List
    (List.map
       (fun (v1, v2) ->
         let v1 = (* "??" *) token env v1 in
         let v2 = map_real_expression env v2 in
         R.Tuple [ v1; v2 ])
       xs)

and map_initialized_identifier (env : env)
    ((v1, v2) : CST.initialized_identifier) =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = (* "=" *) token env v1 in
              let v2 = map_argument env v2 in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

and map_initialized_variable_definition (env : env)
    ((v1, v2, v3) : CST.initialized_variable_definition) =
  let v1 = map_declared_identifier env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = (* "=" *) token env v1 in
              let v2 = map_argument env v2 in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  let v3 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_initialized_identifier env v2 in
           R.Tuple [ v1; v2 ])
         v3)
  in
  R.Tuple [ v1; v2; v3 ]

and map_interface_type_list (env : env) ((v1, v2) : CST.interface_type_list) =
  let v1 = map_type_ env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_type_ env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

and map_lambda_expression (env : env) ((v1, v2) : CST.lambda_expression) =
  let v1 = map_function_signature env v1 in
  let v2 = map_function_body env v2 in
  R.Tuple [ v1; v2 ]

and map_literal (env : env) (x : CST.literal) =
  match x with
  | `Deci_int_lit tok ->
      R.Case ("Deci_int_lit", (* decimal_integer_literal *) token env tok)
  | `Hex_int_lit tok ->
      R.Case ("Hex_int_lit", (* hex_integer_literal *) token env tok)
  | `Deci_floa_point_lit tok ->
      R.Case
        ( "Deci_floa_point_lit",
          (* decimal_floating_point_literal *) token env tok )
  | `True tok -> R.Case ("True", (* "true" *) token env tok)
  | `False tok -> R.Case ("False", (* "false" *) token env tok)
  | `Str_lit x -> R.Case ("Str_lit", map_uri env x)
  | `Null_lit tok -> R.Case ("Null_lit", (* "null" *) token env tok)
  | `Symb_lit (v1, v2) ->
      R.Case
        ( "Symb_lit",
          let v1 = (* "#" *) token env v1 in
          let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
          R.Tuple [ v1; v2 ] )
  | `List_lit (v1, v2, v3, v4, v5) ->
      R.Case
        ( "List_lit",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* const_builtin *) token env tok))
            | None -> R.Option None
          in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_type_arguments env x))
            | None -> R.Option None
          in
          let v3 = (* "[" *) token env v3 in
          let v4 =
            match v4 with
            | Some x ->
                R.Option
                  (Some (map_anon_elem_rep_COMMA_elem_opt_COMMA_4ec364f env x))
            | None -> R.Option None
          in
          let v5 = (* "]" *) token env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )
  | `Set_or_map_lit (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Set_or_map_lit",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* const_builtin *) token env tok))
            | None -> R.Option None
          in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_type_arguments env x))
            | None -> R.Option None
          in
          let v3 = (* "{" *) token env v3 in
          let v4 =
            match v4 with
            | Some x ->
                R.Option
                  (Some (map_anon_elem_rep_COMMA_elem_opt_COMMA_4ec364f env x))
            | None -> R.Option None
          in
          let v5 = (* "}" *) token env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )

and map_local_variable_declaration (env : env)
    ((v1, v2) : CST.local_variable_declaration) =
  let v1 = map_initialized_variable_definition env v1 in
  let v2 = map_semicolon env v2 in
  R.Tuple [ v1; v2 ]

and map_metadata (env : env) (xs : CST.metadata) =
  R.List (List.map (map_annotation_ env) xs)

and map_multiplicative_expression (env : env)
    (x : CST.multiplicative_expression) =
  match x with
  | `Un_exp_rep1_mult_op_un_exp (v1, v2) ->
      R.Case
        ( "Un_exp_rep1_mult_op_un_exp",
          let v1 = map_unary_expression env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = map_multiplicative_operator env v1 in
                   let v2 = map_unary_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )
  | `Super_rep1_mult_op_un_exp (v1, v2) ->
      R.Case
        ( "Super_rep1_mult_op_un_exp",
          let v1 = (* "super" *) token env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = map_multiplicative_operator env v1 in
                   let v2 = map_unary_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )

and map_named_argument (env : env) ((v1, v2) : CST.named_argument) =
  let v1 = map_label env v1 in
  let v2 = map_argument env v2 in
  R.Tuple [ v1; v2 ]

and map_named_parameter_type (env : env)
    ((v1, v2, v3) : CST.named_parameter_type) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_metadata env x))
    | None -> R.Option None
  in
  let v2 =
    match v2 with
    | Some tok -> R.Option (Some ((* "required" *) token env tok))
    | None -> R.Option None
  in
  let v3 = map_typed_identifier env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_native (env : env) ((v1, v2) : CST.native) =
  let v1 = (* "native" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> R.Option (Some (map_uri env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

and map_normal_formal_parameters (env : env)
    ((v1, v2) : CST.normal_formal_parameters) =
  let v1 = map_formal_parameter env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_formal_parameter env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

and map_normal_parameter_type (env : env) ((v1, v2) : CST.normal_parameter_type)
    =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_metadata env x))
    | None -> R.Option None
  in
  let v2 =
    match v2 with
    | `Typed_id x -> R.Case ("Typed_id", map_typed_identifier env x)
    | `Type x -> R.Case ("Type", map_type_ env x)
  in
  R.Tuple [ v1; v2 ]

and map_on_part (env : env) (x : CST.on_part) =
  match x with
  | `Catch_clause_blk (v1, v2) ->
      R.Case
        ( "Catch_clause_blk",
          let v1 = map_catch_clause env v1 in
          let v2 = map_block env v2 in
          R.Tuple [ v1; v2 ] )
  | `On_type_not_void_opt_catch_clause_blk (v1, v2, v3, v4) ->
      R.Case
        ( "On_type_not_void_opt_catch_clause_blk",
          let v1 = (* "on" *) token env v1 in
          let v2 = map_type_not_void env v2 in
          let v3 =
            match v3 with
            | Some x -> R.Option (Some (map_catch_clause env x))
            | None -> R.Option None
          in
          let v4 = map_block env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )

and map_optional_formal_parameters (env : env)
    (x : CST.optional_formal_parameters) =
  match x with
  | `Opt_post_formal_params (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Opt_post_formal_params",
          let v1 = (* "[" *) token env v1 in
          let v2 = map_default_formal_parameter env v2 in
          let v3 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "," *) token env v1 in
                   let v2 = map_default_formal_parameter env v2 in
                   R.Tuple [ v1; v2 ])
                 v3)
          in
          let v4 =
            match v4 with
            | Some tok -> R.Option (Some ((* "," *) token env tok))
            | None -> R.Option None
          in
          let v5 = (* "]" *) token env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )
  | `Named_formal_params (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Named_formal_params",
          let v1 = (* "{" *) token env v1 in
          let v2 = map_default_named_parameter env v2 in
          let v3 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "," *) token env v1 in
                   let v2 = map_default_named_parameter env v2 in
                   R.Tuple [ v1; v2 ])
                 v3)
          in
          let v4 =
            match v4 with
            | Some tok -> R.Option (Some ((* "," *) token env tok))
            | None -> R.Option None
          in
          let v5 = (* "}" *) token env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )

and map_optional_parameter_types (env : env) (x : CST.optional_parameter_types)
    =
  match x with
  | `Opt_posi_param_types (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Opt_posi_param_types",
          let v1 = (* "[" *) token env v1 in
          let v2 = map_normal_parameter_type env v2 in
          let v3 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "," *) token env v1 in
                   let v2 = map_normal_parameter_type env v2 in
                   R.Tuple [ v1; v2 ])
                 v3)
          in
          let v4 =
            match v4 with
            | Some tok -> R.Option (Some ((* "," *) token env tok))
            | None -> R.Option None
          in
          let v5 = (* "]" *) token env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )
  | `Named_param_types (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Named_param_types",
          let v1 = (* "{" *) token env v1 in
          let v2 = map_named_parameter_type env v2 in
          let v3 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "," *) token env v1 in
                   let v2 = map_named_parameter_type env v2 in
                   R.Tuple [ v1; v2 ])
                 v3)
          in
          let v4 =
            match v4 with
            | Some tok -> R.Option (Some ((* "," *) token env tok))
            | None -> R.Option None
          in
          let v5 = (* "}" *) token env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )

and map_parameter_type_list (env : env) ((v1, v2, v3) : CST.parameter_type_list)
    =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x ->
        R.Option
          (Some
             (match x with
             | `Normal_param_type_rep_COMMA_normal_param_type_opt_COMMA
                 (v1, v2, v3) ->
                 R.Case
                   ( "Normal_param_type_rep_COMMA_normal_param_type_opt_COMMA",
                     let v1 = map_normal_parameter_type env v1 in
                     let v2 =
                       R.List
                         (List.map
                            (fun (v1, v2) ->
                              let v1 = (* "," *) token env v1 in
                              let v2 = map_normal_parameter_type env v2 in
                              R.Tuple [ v1; v2 ])
                            v2)
                     in
                     let v3 =
                       match v3 with
                       | Some tok -> R.Option (Some ((* "," *) token env tok))
                       | None -> R.Option None
                     in
                     R.Tuple [ v1; v2; v3 ] )
             | `Normal_param_type_rep_COMMA_normal_param_type_COMMA_opt_param_types
                 (v1, v2, v3, v4) ->
                 R.Case
                   ( "Normal_param_type_rep_COMMA_normal_param_type_COMMA_opt_param_types",
                     let v1 = map_normal_parameter_type env v1 in
                     let v2 =
                       R.List
                         (List.map
                            (fun (v1, v2) ->
                              let v1 = (* "," *) token env v1 in
                              let v2 = map_normal_parameter_type env v2 in
                              R.Tuple [ v1; v2 ])
                            v2)
                     in
                     let v3 = (* "," *) token env v3 in
                     let v4 = map_optional_parameter_types env v4 in
                     R.Tuple [ v1; v2; v3; v4 ] )
             | `Opt_param_types x ->
                 R.Case ("Opt_param_types", map_optional_parameter_types env x)))
    | None -> R.Option None
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_argument env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_postfix_expression (env : env) (x : CST.postfix_expression) =
  match x with
  | `Prim_rep_sele (v1, v2) ->
      R.Case
        ( "Prim_rep_sele",
          let v1 = map_primary env v1 in
          let v2 = R.List (List.map (map_selector env) v2) in
          R.Tuple [ v1; v2 ] )
  | `Post_exp_ x -> R.Case ("Post_exp_", map_postfix_expression_ env x)

and map_postfix_expression_ (env : env) (x : CST.postfix_expression_) =
  match x with
  | `Assi_exp_post_op (v1, v2) ->
      R.Case
        ( "Assi_exp_post_op",
          let v1 = map_assignable_expression env v1 in
          let v2 = (* increment_operator *) token env v2 in
          R.Tuple [ v1; v2 ] )
  | `Cons_invo_rep_sele (v1, v2) ->
      R.Case
        ( "Cons_invo_rep_sele",
          let v1 = map_constructor_invocation env v1 in
          let v2 = R.List (List.map (map_selector env) v2) in
          R.Tuple [ v1; v2 ] )

and map_primary (env : env) (x : CST.primary) =
  match x with
  | `Lit x -> R.Case ("Lit", map_literal env x)
  | `Func_exp (v1, v2) ->
      R.Case
        ( "Func_exp",
          let v1 = map_formal_parameter_part env v1 in
          let v2 = map_function_expression_body env v2 in
          R.Tuple [ v1; v2 ] )
  | `Id tok -> R.Case ("Id", (* pattern [a-zA-Z_$][\w$]* *) token env tok)
  | `New_exp (v1, v2, v3, v4) ->
      R.Case
        ( "New_exp",
          let v1 = (* "new" *) token env v1 in
          let v2 = map_type_not_void env v2 in
          let v3 =
            match v3 with
            | Some x -> R.Option (Some (map_dot_identifier env x))
            | None -> R.Option None
          in
          let v4 = map_arguments env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `Const_obj_exp (v1, v2, v3, v4) ->
      R.Case
        ( "Const_obj_exp",
          let v1 = (* const_builtin *) token env v1 in
          let v2 = map_type_not_void env v2 in
          let v3 =
            match v3 with
            | Some x -> R.Option (Some (map_dot_identifier env x))
            | None -> R.Option None
          in
          let v4 = map_arguments env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `LPAR_exp_RPAR x ->
      R.Case ("LPAR_exp_RPAR", map_parenthesized_expression env x)
  | `This tok -> R.Case ("This", (* "this" *) token env tok)
  | `Super_unco_assi_sele (v1, v2) ->
      R.Case
        ( "Super_unco_assi_sele",
          let v1 = (* "super" *) token env v1 in
          let v2 = map_unconditional_assignable_selector env v2 in
          R.Tuple [ v1; v2 ] )

and map_real_expression (env : env) (x : CST.real_expression) =
  match x with
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Cond_exp",
          let v1 = map_real_expression env v1 in
          let v2 = (* "?" *) token env v2 in
          let v3 = map_expression_without_cascade env v3 in
          let v4 = (* ":" *) token env v4 in
          let v5 = map_expression_without_cascade env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )
  | `Logi_or_exp (v1, v2) ->
      R.Case
        ( "Logi_or_exp",
          let v1 = map_real_expression env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "||" *) token env v1 in
                   let v2 = map_real_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )
  | `If_null_exp (v1, v2) ->
      R.Case
        ( "If_null_exp",
          let v1 = map_real_expression env v1 in
          let v2 = map_if_null_expression_ env v2 in
          R.Tuple [ v1; v2 ] )
  | `Addi_exp x -> R.Case ("Addi_exp", map_additive_expression env x)
  | `Mult_exp x -> R.Case ("Mult_exp", map_multiplicative_expression env x)
  | `Rela_exp x -> R.Case ("Rela_exp", map_relational_expression env x)
  | `Equa_exp x -> R.Case ("Equa_exp", map_equality_expression env x)
  | `Logi_and_exp (v1, v2) ->
      R.Case
        ( "Logi_and_exp",
          let v1 = map_real_expression env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = (* "&&" *) token env v1 in
                   let v2 = map_real_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )
  | `Bitw_and_exp x -> R.Case ("Bitw_and_exp", map_bitwise_and_expression env x)
  | `Bitw_or_exp x -> R.Case ("Bitw_or_exp", map_bitwise_or_expression env x)
  | `Bitw_xor_exp x -> R.Case ("Bitw_xor_exp", map_bitwise_xor_expression env x)
  | `Shift_exp x -> R.Case ("Shift_exp", map_shift_expression env x)
  | `Type_cast_exp (v1, v2) ->
      R.Case
        ( "Type_cast_exp",
          let v1 = map_real_expression env v1 in
          let v2 = map_type_cast env v2 in
          R.Tuple [ v1; v2 ] )
  | `Type_test_exp (v1, v2) ->
      R.Case
        ( "Type_test_exp",
          let v1 = map_real_expression env v1 in
          let v2 = map_type_test env v2 in
          R.Tuple [ v1; v2 ] )
  | `Un_exp x -> R.Case ("Un_exp", map_unary_expression env x)

and map_relational_expression (env : env) (x : CST.relational_expression) =
  match x with
  | `Real_exp_rela_op_real_exp (v1, v2, v3) ->
      R.Case
        ( "Real_exp_rela_op_real_exp",
          let v1 = map_real_expression env v1 in
          let v2 = map_relational_operator env v2 in
          let v3 = map_real_expression env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Super_rela_op_real_exp (v1, v2, v3) ->
      R.Case
        ( "Super_rela_op_real_exp",
          let v1 = (* "super" *) token env v1 in
          let v2 = map_relational_operator env v2 in
          let v3 = map_real_expression env v3 in
          R.Tuple [ v1; v2; v3 ] )

and map_selector (env : env) (x : CST.selector) =
  match x with
  | `Excl_op tok -> R.Case ("Excl_op", (* "!" *) token env tok)
  | `Assi_sele x -> R.Case ("Assi_sele", map_assignable_selector env x)
  | `Arg_part x -> R.Case ("Arg_part", map_argument_part env x)

and map_shift_expression (env : env) (x : CST.shift_expression) =
  match x with
  | `Real_exp_rep1_shift_op_real_exp (v1, v2) ->
      R.Case
        ( "Real_exp_rep1_shift_op_real_exp",
          let v1 = map_real_expression env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = map_shift_operator env v1 in
                   let v2 = map_real_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )
  | `Super_rep1_shift_op_real_exp (v1, v2) ->
      R.Case
        ( "Super_rep1_shift_op_real_exp",
          let v1 = (* "super" *) token env v1 in
          let v2 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 = map_shift_operator env v1 in
                   let v2 = map_real_expression env v2 in
                   R.Tuple [ v1; v2 ])
                 v2)
          in
          R.Tuple [ v1; v2 ] )

and map_simple_formal_parameter (env : env) (x : CST.simple_formal_parameter) =
  match x with
  | `Decl_id x -> R.Case ("Decl_id", map_declared_identifier env x)
  | `Opt_cova_id (v1, v2) ->
      R.Case
        ( "Opt_cova_id",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "covariant" *) token env tok))
            | None -> R.Option None
          in
          let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
          R.Tuple [ v1; v2 ] )

and map_statement (env : env) (x : CST.statement) =
  match x with
  | `Blk x -> R.Case ("Blk", map_block env x)
  | `Local_func_decl (v1, v2) ->
      R.Case
        ( "Local_func_decl",
          let v1 =
            match v1 with
            | Some x -> R.Option (Some (map_metadata env x))
            | None -> R.Option None
          in
          let v2 = map_lambda_expression env v2 in
          R.Tuple [ v1; v2 ] )
  | `Local_var_decl x ->
      R.Case ("Local_var_decl", map_local_variable_declaration env x)
  | `For_stmt (v1, v2, v3, v4, v5, v6) ->
      R.Case
        ( "For_stmt",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "await" *) token env tok))
            | None -> R.Option None
          in
          let v2 = (* "for" *) token env v2 in
          let v3 = (* "(" *) token env v3 in
          let v4 = map_for_loop_parts env v4 in
          let v5 = (* ")" *) token env v5 in
          let v6 = map_statement env v6 in
          R.Tuple [ v1; v2; v3; v4; v5; v6 ] )
  | `While_stmt (v1, v2, v3) ->
      R.Case
        ( "While_stmt",
          let v1 = (* "while" *) token env v1 in
          let v2 = map_parenthesized_expression env v2 in
          let v3 = map_statement env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Do_stmt",
          let v1 = (* "do" *) token env v1 in
          let v2 = map_statement env v2 in
          let v3 = (* "while" *) token env v3 in
          let v4 = map_parenthesized_expression env v4 in
          let v5 = map_semicolon env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )
  | `Switch_stmt (v1, v2, v3) ->
      R.Case
        ( "Switch_stmt",
          let v1 = (* "switch" *) token env v1 in
          let v2 = map_parenthesized_expression env v2 in
          let v3 = map_switch_block env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `If_stmt (v1, v2, v3, v4) ->
      R.Case
        ( "If_stmt",
          let v1 = (* "if" *) token env v1 in
          let v2 = map_parenthesized_expression env v2 in
          let v3 = map_statement env v3 in
          let v4 =
            match v4 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = (* "else" *) token env v1 in
                      let v2 = map_statement env v2 in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `Try_stmt (v1, v2) ->
      R.Case
        ( "Try_stmt",
          let v1 = map_try_head env v1 in
          let v2 =
            match v2 with
            | `Fina_clause x -> R.Case ("Fina_clause", map_finally_clause env x)
            | `Rep1_on_part_opt_fina_clause (v1, v2) ->
                R.Case
                  ( "Rep1_on_part_opt_fina_clause",
                    let v1 = R.List (List.map (map_on_part env) v1) in
                    let v2 =
                      match v2 with
                      | Some x -> R.Option (Some (map_finally_clause env x))
                      | None -> R.Option None
                    in
                    R.Tuple [ v1; v2 ] )
          in
          R.Tuple [ v1; v2 ] )
  | `Brk_stmt (v1, v2, v3) ->
      R.Case
        ( "Brk_stmt",
          let v1 = (* break_builtin *) token env v1 in
          let v2 =
            match v2 with
            | Some tok ->
                R.Option (Some ((* pattern [a-zA-Z_$][\w$]* *) token env tok))
            | None -> R.Option None
          in
          let v3 = map_semicolon env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Cont_stmt (v1, v2, v3) ->
      R.Case
        ( "Cont_stmt",
          let v1 = (* "continue" *) token env v1 in
          let v2 =
            match v2 with
            | Some tok ->
                R.Option (Some ((* pattern [a-zA-Z_$][\w$]* *) token env tok))
            | None -> R.Option None
          in
          let v3 = map_semicolon env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Ret_stmt (v1, v2, v3) ->
      R.Case
        ( "Ret_stmt",
          let v1 = (* "return" *) token env v1 in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_argument env x))
            | None -> R.Option None
          in
          let v3 = map_semicolon env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Yield_stmt (v1, v2, v3) ->
      R.Case
        ( "Yield_stmt",
          let v1 = (* "yield" *) token env v1 in
          let v2 = map_argument env v2 in
          let v3 = map_semicolon env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Yield_each_stmt (v1, v2, v3, v4) ->
      R.Case
        ( "Yield_each_stmt",
          let v1 = (* "yield" *) token env v1 in
          let v2 = (* "*" *) token env v2 in
          let v3 = map_argument env v3 in
          let v4 = map_semicolon env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `Exp_stmt x -> R.Case ("Exp_stmt", map_expression_statement env x)
  | `Assert_stmt (v1, v2) ->
      R.Case
        ( "Assert_stmt",
          let v1 = map_assertion env v1 in
          let v2 = (* ";" *) token env v2 in
          R.Tuple [ v1; v2 ] )

and map_strict_formal_parameter_list (env : env)
    (x : CST.strict_formal_parameter_list) =
  match x with
  | `LPAR_RPAR (v1, v2) ->
      R.Case
        ( "LPAR_RPAR",
          let v1 = (* "(" *) token env v1 in
          let v2 = (* ")" *) token env v2 in
          R.Tuple [ v1; v2 ] )
  | `LPAR_normal_formal_params_opt_COMMA_RPAR (v1, v2, v3, v4) ->
      R.Case
        ( "LPAR_normal_formal_params_opt_COMMA_RPAR",
          let v1 = (* "(" *) token env v1 in
          let v2 = map_normal_formal_parameters env v2 in
          let v3 =
            match v3 with
            | Some tok -> R.Option (Some ((* "," *) token env tok))
            | None -> R.Option None
          in
          let v4 = (* ")" *) token env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `LPAR_normal_formal_params_COMMA_opt_formal_params_RPAR (v1, v2, v3, v4, v5)
    ->
      R.Case
        ( "LPAR_normal_formal_params_COMMA_opt_formal_params_RPAR",
          let v1 = (* "(" *) token env v1 in
          let v2 = map_normal_formal_parameters env v2 in
          let v3 = (* "," *) token env v3 in
          let v4 = map_optional_formal_parameters env v4 in
          let v5 = (* ")" *) token env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )
  | `LPAR_opt_formal_params_RPAR (v1, v2, v3) ->
      R.Case
        ( "LPAR_opt_formal_params_RPAR",
          let v1 = (* "(" *) token env v1 in
          let v2 = map_optional_formal_parameters env v2 in
          let v3 = (* ")" *) token env v3 in
          R.Tuple [ v1; v2; v3 ] )

and map_string_literal (env : env) (xs : CST.string_literal) =
  R.List
    (List.map
       (fun x ->
         match x with
         | `Str_lit_double_quotes x ->
             R.Case
               ("Str_lit_double_quotes", map_string_literal_double_quotes env x)
         | `Str_lit_single_quotes x ->
             R.Case
               ("Str_lit_single_quotes", map_string_literal_single_quotes env x)
         | `Str_lit_double_quotes_mult x ->
             R.Case
               ( "Str_lit_double_quotes_mult",
                 map_string_literal_double_quotes_multiple env x )
         | `Str_lit_single_quotes_mult x ->
             R.Case
               ( "Str_lit_single_quotes_mult",
                 map_string_literal_single_quotes_multiple env x )
         | `Raw_str_lit_double_quotes x ->
             R.Case
               ( "Raw_str_lit_double_quotes",
                 map_raw_string_literal_double_quotes env x )
         | `Raw_str_lit_single_quotes x ->
             R.Case
               ( "Raw_str_lit_single_quotes",
                 map_raw_string_literal_single_quotes env x )
         | `Raw_str_lit_double_quotes_mult x ->
             R.Case
               ( "Raw_str_lit_double_quotes_mult",
                 map_raw_string_literal_double_quotes_multiple env x )
         | `Raw_str_lit_single_quotes_mult x ->
             R.Case
               ( "Raw_str_lit_single_quotes_mult",
                 map_raw_string_literal_single_quotes_multiple env x ))
       xs)

and map_string_literal_double_quotes (env : env)
    ((v1, v2, v3) : CST.string_literal_double_quotes) =
  let v1 = (* "\"" *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun x ->
           match x with
           | `Temp_chars_double_single tok ->
               R.Case
                 ( "Temp_chars_double_single",
                   (* template_chars_double_single *) token env tok )
           | `SQUOT tok -> R.Case ("SQUOT", (* "'" *) token env tok)
           | `Esc_seq tok ->
               R.Case ("Esc_seq", (* unused_escape_sequence *) token env tok)
           | `Sub_str_test x ->
               R.Case ("Sub_str_test", map_sub_string_test env x)
           | `Temp_subs x ->
               R.Case ("Temp_subs", map_template_substitution env x))
         v2)
  in
  let v3 = (* "\"" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_string_literal_double_quotes_multiple (env : env)
    ((v1, v2, v3) : CST.string_literal_double_quotes_multiple) =
  let v1 = (* "\"\"\"" *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun x ->
           match x with
           | `Temp_chars_double tok ->
               R.Case
                 ("Temp_chars_double", (* template_chars_double *) token env tok)
           | `SQUOT tok -> R.Case ("SQUOT", (* "'" *) token env tok)
           | `DQUOT tok -> R.Case ("DQUOT", (* "\"" *) token env tok)
           | `Esc_seq tok ->
               R.Case ("Esc_seq", (* unused_escape_sequence *) token env tok)
           | `Sub_str_test x ->
               R.Case ("Sub_str_test", map_sub_string_test env x)
           | `Temp_subs x ->
               R.Case ("Temp_subs", map_template_substitution env x))
         v2)
  in
  let v3 = (* "\"\"\"" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_string_literal_single_quotes (env : env)
    ((v1, v2, v3) : CST.string_literal_single_quotes) =
  let v1 = (* "'" *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun x ->
           match x with
           | `Temp_chars_single_single tok ->
               R.Case
                 ( "Temp_chars_single_single",
                   (* template_chars_single_single *) token env tok )
           | `DQUOT tok -> R.Case ("DQUOT", (* "\"" *) token env tok)
           | `Esc_seq tok ->
               R.Case ("Esc_seq", (* unused_escape_sequence *) token env tok)
           | `Sub_str_test x ->
               R.Case ("Sub_str_test", map_sub_string_test env x)
           | `Temp_subs x ->
               R.Case ("Temp_subs", map_template_substitution env x))
         v2)
  in
  let v3 = (* "'" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_string_literal_single_quotes_multiple (env : env)
    ((v1, v2, v3) : CST.string_literal_single_quotes_multiple) =
  let v1 = (* "'''" *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun x ->
           match x with
           | `Temp_chars_single tok ->
               R.Case
                 ("Temp_chars_single", (* template_chars_single *) token env tok)
           | `DQUOT tok -> R.Case ("DQUOT", (* "\"" *) token env tok)
           | `SQUOT tok -> R.Case ("SQUOT", (* "'" *) token env tok)
           | `Esc_seq tok ->
               R.Case ("Esc_seq", (* unused_escape_sequence *) token env tok)
           | `Sub_str_test x ->
               R.Case ("Sub_str_test", map_sub_string_test env x)
           | `Temp_subs x ->
               R.Case ("Temp_subs", map_template_substitution env x))
         v2)
  in
  let v3 = (* "'''" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_super_formal_parameter (env : env)
    ((v1, v2, v3, v4, v5) : CST.super_formal_parameter) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_final_const_var_or_type env x))
    | None -> R.Option None
  in
  let v2 = (* "super" *) token env v2 in
  let v3 = (* "." *) token env v3 in
  let v4 = (* pattern [a-zA-Z_$][\w$]* *) token env v4 in
  let v5 =
    match v5 with
    | Some x -> R.Option (Some (map_formal_parameter_part env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4; v5 ]

and map_switch_block (env : env) ((v1, v2, v3) : CST.switch_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun x ->
           match x with
           | `Switch_label x -> R.Case ("Switch_label", map_switch_label env x)
           | `Stmt x -> R.Case ("Stmt", map_statement env x))
         v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_switch_label (env : env) ((v1, v2) : CST.switch_label) =
  let v1 = R.List (List.map (map_label env) v1) in
  let v2 =
    match v2 with
    | `Case_buil_exp_COLON (v1, v2, v3) ->
        R.Case
          ( "Case_buil_exp_COLON",
            let v1 = (* case_builtin *) token env v1 in
            let v2 = map_argument env v2 in
            let v3 = (* ":" *) token env v3 in
            R.Tuple [ v1; v2; v3 ] )
    | `Defa_COLON (v1, v2) ->
        R.Case
          ( "Defa_COLON",
            let v1 = (* "default" *) token env v1 in
            let v2 = (* ":" *) token env v2 in
            R.Tuple [ v1; v2 ] )
  in
  R.Tuple [ v1; v2 ]

and map_template_substitution (env : env) ((v1, v2) : CST.template_substitution)
    =
  let v1 = (* "$" *) token env v1 in
  let v2 =
    match v2 with
    | `LCURL_exp_RCURL (v1, v2, v3) ->
        R.Case
          ( "LCURL_exp_RCURL",
            let v1 = (* "{" *) token env v1 in
            let v2 = map_argument env v2 in
            let v3 = (* "}" *) token env v3 in
            R.Tuple [ v1; v2; v3 ] )
    | `Id_dollar_esca tok ->
        R.Case
          ( "Id_dollar_esca",
            (* pattern ([a-zA-Z_]|(\\\$))([\w]|(\\\$))* *) token env tok )
  in
  R.Tuple [ v1; v2 ]

and map_throw_expression (env : env) ((v1, v2) : CST.throw_expression) =
  let v1 = (* "throw" *) token env v1 in
  let v2 = map_argument env v2 in
  R.Tuple [ v1; v2 ]

and map_try_head (env : env) ((v1, v2) : CST.try_head) =
  let v1 = (* "try" *) token env v1 in
  let v2 = map_block env v2 in
  R.Tuple [ v1; v2 ]

and map_type_ (env : env) (x : CST.type_) =
  match x with
  | `Func_type_opt_null_type (v1, v2) ->
      R.Case
        ( "Func_type_opt_null_type",
          let v1 = map_function_type env v1 in
          let v2 =
            match v2 with
            | Some tok -> R.Option (Some ((* "?" *) token env tok))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2 ] )
  | `Type_not_func x -> R.Case ("Type_not_func", map_type_not_function env x)

and map_type_arguments (env : env) (x : CST.type_arguments) =
  match x with
  | `LT_opt_type_rep_COMMA_type_GT (v1, v2, v3) ->
      R.Case
        ( "LT_opt_type_rep_COMMA_type_GT",
          let v1 = (* "<" *) token env v1 in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_interface_type_list env x))
            | None -> R.Option None
          in
          let v3 = (* ">" *) token env v3 in
          R.Tuple [ v1; v2; v3 ] )

and map_type_bound (env : env) ((v1, v2) : CST.type_bound) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = map_type_not_void env v2 in
  R.Tuple [ v1; v2 ]

and map_type_cast (env : env) ((v1, v2) : CST.type_cast) =
  let v1 = (* as_operator *) token env v1 in
  let v2 = map_type_not_void env v2 in
  R.Tuple [ v1; v2 ]

and map_type_not_function (env : env) (x : CST.type_not_function) =
  match x with
  | `Type_not_void_not_func x ->
      R.Case ("Type_not_void_not_func", map_type_not_void_not_function env x)
  | `Void_type tok -> R.Case ("Void_type", (* void_type *) token env tok)

and map_type_not_void (env : env) (x : CST.type_not_void) =
  match x with
  | `Func_type_opt_null_type (v1, v2) ->
      R.Case
        ( "Func_type_opt_null_type",
          let v1 = map_function_type env v1 in
          let v2 =
            match v2 with
            | Some tok -> R.Option (Some ((* "?" *) token env tok))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2 ] )
  | `Type_not_void_not_func x ->
      R.Case ("Type_not_void_not_func", map_type_not_void_not_function env x)

and map_type_not_void_not_function (env : env)
    (x : CST.type_not_void_not_function) =
  match x with
  | `Type_name_opt_type_args_opt_null_type (v1, v2, v3) ->
      R.Case
        ( "Type_name_opt_type_args_opt_null_type",
          let v1 = map_type_name env v1 in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_type_arguments env x))
            | None -> R.Option None
          in
          let v3 =
            match v3 with
            | Some tok -> R.Option (Some ((* "?" *) token env tok))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2; v3 ] )
  | `Func_buil_id_opt_null_type (v1, v2) ->
      R.Case
        ( "Func_buil_id_opt_null_type",
          let v1 = (* "Function" *) token env v1 in
          let v2 =
            match v2 with
            | Some tok -> R.Option (Some ((* "?" *) token env tok))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2 ] )

and map_type_parameter (env : env) ((v1, v2, v3, v4) : CST.type_parameter) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_metadata env x))
    | None -> R.Option None
  in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
  let v3 =
    match v3 with
    | Some tok -> R.Option (Some ((* "?" *) token env tok))
    | None -> R.Option None
  in
  let v4 =
    match v4 with
    | Some x -> R.Option (Some (map_type_bound env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4 ]

and map_type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_type_parameter env v2 in
           R.Tuple [ v1; v2 ])
         v3)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [ v1; v2; v3; v4 ]

and map_type_test (env : env) ((v1, v2) : CST.type_test) =
  let v1 = map_is_operator env v1 in
  let v2 = map_type_not_void env v2 in
  R.Tuple [ v1; v2 ]

and map_typed_identifier (env : env) ((v1, v2) : CST.typed_identifier) =
  let v1 = map_type_ env v1 in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
  R.Tuple [ v1; v2 ]

and map_unary_expression (env : env) (x : CST.unary_expression) =
  match x with
  | `Post_exp x -> R.Case ("Post_exp", map_postfix_expression env x)
  | `Un_exp_ x -> R.Case ("Un_exp_", map_unary_expression_ env x)

and map_unary_expression_ (env : env) (x : CST.unary_expression_) =
  match x with
  | `Prefix_op_un_exp (v1, v2) ->
      R.Case
        ( "Prefix_op_un_exp",
          let v1 = map_prefix_operator env v1 in
          let v2 = map_unary_expression env v2 in
          R.Tuple [ v1; v2 ] )
  | `Await_exp (v1, v2) ->
      R.Case
        ( "Await_exp",
          let v1 = (* "await" *) token env v1 in
          let v2 = map_unary_expression env v2 in
          R.Tuple [ v1; v2 ] )
  | `Choice_minus_op_super (v1, v2) ->
      R.Case
        ( "Choice_minus_op_super",
          let v1 =
            match v1 with
            | `Minus_op tok -> R.Case ("Minus_op", (* "-" *) token env tok)
            | `Tilde_op tok -> R.Case ("Tilde_op", (* "~" *) token env tok)
          in
          let v2 = (* "super" *) token env v2 in
          R.Tuple [ v1; v2 ] )
  | `Incr_op_assi_exp (v1, v2) ->
      R.Case
        ( "Incr_op_assi_exp",
          let v1 = (* increment_operator *) token env v1 in
          let v2 = map_assignable_expression env v2 in
          R.Tuple [ v1; v2 ] )

and map_unconditional_assignable_selector (env : env)
    (x : CST.unconditional_assignable_selector) =
  match x with
  | `Opt_null_type_LBRACK_exp_RBRACK (v1, v2, v3, v4) ->
      R.Case
        ( "Opt_null_type_LBRACK_exp_RBRACK",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "?" *) token env tok))
            | None -> R.Option None
          in
          let v2 = (* "[" *) token env v2 in
          let v3 = map_argument env v3 in
          let v4 = (* "]" *) token env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `DOT_id x -> R.Case ("DOT_id", map_dot_identifier env x)

and map_uri (env : env) (x : CST.uri) = map_string_literal env x

and map_var_or_type (env : env) (x : CST.var_or_type) =
  match x with
  | `Type x -> R.Case ("Type", map_type_ env x)
  | `Infe_type_opt_type (v1, v2) ->
      R.Case
        ( "Infe_type_opt_type",
          let v1 = (* "var" *) token env v1 in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_type_ env x))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2 ] )

let map_wildcard_bounds (env : env) (x : CST.wildcard_bounds) =
  match x with
  | `Extends_type (v1, v2) ->
      R.Case
        ( "Extends_type",
          let v1 = (* "extends" *) token env v1 in
          let v2 = map_type_ env v2 in
          R.Tuple [ v1; v2 ] )
  | `Super_type (v1, v2) ->
      R.Case
        ( "Super_type",
          let v1 = (* "super" *) token env v1 in
          let v2 = map_type_ env v2 in
          R.Tuple [ v1; v2 ] )

let map_explicit_constructor_invocation (env : env)
    ((v1, v2, v3) : CST.explicit_constructor_invocation) =
  let v1 =
    match v1 with
    | `Opt_type_args_choice_this (v1, v2) ->
        R.Case
          ( "Opt_type_args_choice_this",
            let v1 =
              match v1 with
              | Some x -> R.Option (Some (map_type_arguments env x))
              | None -> R.Option None
            in
            let v2 =
              match v2 with
              | `This tok -> R.Case ("This", (* "this" *) token env tok)
              | `Super tok -> R.Case ("Super", (* "super" *) token env tok)
            in
            R.Tuple [ v1; v2 ] )
    | `Choice_choice_id_DOT_opt_type_args_super (v1, v2, v3, v4) ->
        R.Case
          ( "Choice_choice_id_DOT_opt_type_args_super",
            let v1 =
              match v1 with
              | `Choice_id x -> R.Case ("Choice_id", map_ambiguous_name env x)
              | `Prim x -> R.Case ("Prim", map_primary env x)
            in
            let v2 = (* "." *) token env v2 in
            let v3 =
              match v3 with
              | Some x -> R.Option (Some (map_type_arguments env x))
              | None -> R.Option None
            in
            let v4 = (* "super" *) token env v4 in
            R.Tuple [ v1; v2; v3; v4 ] )
  in
  let v2 = map_arguments env v2 in
  let v3 = map_semicolon env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_throws (env : env) ((v1, v2, v3) : CST.throws) =
  let v1 = (* "throws" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_type_ env v2 in
           R.Tuple [ v1; v2 ])
         v3)
  in
  R.Tuple [ v1; v2; v3 ]

let map_part_directive (env : env) ((v1, v2, v3, v4) : CST.part_directive) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_metadata env x))
    | None -> R.Option None
  in
  let v2 = (* "part" *) token env v2 in
  let v3 = map_uri env v3 in
  let v4 = map_semicolon env v4 in
  R.Tuple [ v1; v2; v3; v4 ]

let map_uri_test (env : env) ((v1, v2) : CST.uri_test) =
  let v1 = map_dotted_identifier_list env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = (* "==" *) token env v1 in
              let v2 = map_uri env v2 in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

let map_initialized_identifier_list (env : env)
    ((v1, v2) : CST.initialized_identifier_list) =
  let v1 = map_initialized_identifier env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_initialized_identifier env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

let map_enum_constant (env : env) ((v1, v2) : CST.enum_constant) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_metadata env x))
    | None -> R.Option None
  in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
  R.Tuple [ v1; v2 ]

let map_setter_signature (env : env)
    ((v1, v2, v3, v4, v5) : CST.setter_signature) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_type_ env x))
    | None -> R.Option None
  in
  let v2 = (* "set" *) token env v2 in
  let v3 = (* pattern [a-zA-Z_$][\w$]* *) token env v3 in
  let v4 = map_formal_parameter_part env v4 in
  let v5 =
    match v5 with
    | Some x -> R.Option (Some (map_native env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_anon_choice_type_be0da33 (env : env) (x : CST.anon_choice_type_be0da33)
    =
  match x with
  | `Type x -> R.Case ("Type", map_type_ env x)
  | `Infe_type tok -> R.Case ("Infe_type", (* "var" *) token env tok)

let map_operator_signature (env : env)
    ((v1, v2, v3, v4, v5) : CST.operator_signature) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_type_ env x))
    | None -> R.Option None
  in
  let v2 = (* "operator" *) token env v2 in
  let v3 =
    match v3 with
    | `TILDE tok -> R.Case ("TILDE", (* "~" *) token env tok)
    | `Bin_op x -> R.Case ("Bin_op", map_binary_operator env x)
    | `LBRACKRBRACK tok -> R.Case ("LBRACKRBRACK", (* "[]" *) token env tok)
    | `LBRACKRBRACKEQ tok -> R.Case ("LBRACKRBRACKEQ", (* "[]=" *) token env tok)
  in
  let v4 = map_formal_parameter_list env v4 in
  let v5 =
    match v5 with
    | Some x -> R.Option (Some (map_native env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_static_final_declaration (env : env)
    ((v1, v2, v3) : CST.static_final_declaration) =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_argument env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_type_not_void_list (env : env) ((v1, v2) : CST.type_not_void_list) =
  let v1 = map_type_not_void env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_type_not_void env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

let map_dimensions (env : env) (xs : CST.dimensions) =
  R.List
    (List.map
       (fun (v1, v2, v3) ->
         let v1 =
           match v1 with
           | Some x -> R.Option (Some (map_metadata env x))
           | None -> R.Option None
         in
         let v2 = (* "[" *) token env v2 in
         let v3 = (* "]" *) token env v3 in
         R.Tuple [ v1; v2; v3 ])
       xs)

let map_library_name (env : env) ((v1, v2, v3, v4) : CST.library_name) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_metadata env x))
    | None -> R.Option None
  in
  let v2 = (* "library" *) token env v2 in
  let v3 = map_dotted_identifier_list env v3 in
  let v4 = map_semicolon env v4 in
  R.Tuple [ v1; v2; v3; v4 ]

let map_getter_signature (env : env) ((v1, v2, v3, v4) : CST.getter_signature) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_type_ env x))
    | None -> R.Option None
  in
  let v2 = (* "get" *) token env v2 in
  let v3 = (* pattern [a-zA-Z_$][\w$]* *) token env v3 in
  let v4 =
    match v4 with
    | Some x -> R.Option (Some (map_native env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4 ]

let map_constant_constructor_signature (env : env)
    ((v1, v2, v3) : CST.constant_constructor_signature) =
  let v1 = (* const_builtin *) token env v1 in
  let v2 = map_qualified env v2 in
  let v3 = map_formal_parameter_list env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_factory_constructor_signature (env : env)
    ((v1, v2, v3, v4) : CST.factory_constructor_signature) =
  let v1 = (* "factory" *) token env v1 in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
  let v3 = R.List (List.map (map_dot_identifier env) v3) in
  let v4 = map_formal_parameter_list env v4 in
  R.Tuple [ v1; v2; v3; v4 ]

let map_part_of_directive (env : env)
    ((v1, v2, v3, v4, v5) : CST.part_of_directive) =
  let v1 =
    match v1 with
    | Some x -> R.Option (Some (map_metadata env x))
    | None -> R.Option None
  in
  let v2 = (* "part" *) token env v2 in
  let v3 = (* "of" *) token env v3 in
  let v4 =
    match v4 with
    | `Dotted_id_list x ->
        R.Case ("Dotted_id_list", map_dotted_identifier_list env x)
    | `Uri x -> R.Case ("Uri", map_uri env x)
  in
  let v5 = map_semicolon env v5 in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_type_alias (env : env) (x : CST.type_alias) =
  match x with
  | `Type_type_name_opt_type_params_EQ_func_type_SEMI (v1, v2, v3, v4, v5, v6)
    ->
      R.Case
        ( "Type_type_name_opt_type_params_EQ_func_type_SEMI",
          let v1 = (* "typedef" *) token env v1 in
          let v2 = map_type_name env v2 in
          let v3 =
            match v3 with
            | Some x -> R.Option (Some (map_type_parameters env x))
            | None -> R.Option None
          in
          let v4 = (* "=" *) token env v4 in
          let v5 = map_function_type env v5 in
          let v6 = (* ";" *) token env v6 in
          R.Tuple [ v1; v2; v3; v4; v5; v6 ] )
  | `Type_opt_type_type_name_formal_param_part_SEMI (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Type_opt_type_type_name_formal_param_part_SEMI",
          let v1 = (* "typedef" *) token env v1 in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_type_ env x))
            | None -> R.Option None
          in
          let v3 = map_type_name env v3 in
          let v4 = map_formal_parameter_part env v4 in
          let v5 = (* ";" *) token env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )

let map_constructor_signature (env : env)
    ((v1, v2, v3) : CST.constructor_signature) =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> R.Option (Some (map_dot_identifier env x))
    | None -> R.Option None
  in
  let v3 = map_formal_parameter_list env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_configuration_uri (env : env)
    ((v1, v2, v3, v4, v5) : CST.configuration_uri) =
  let v1 = (* "if" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_uri_test env v3 in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_uri env v5 in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_enum_body (env : env) ((v1, v2, v3, v4, v5) : CST.enum_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_enum_constant env v2 in
  let v3 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_enum_constant env v2 in
           R.Tuple [ v1; v2 ])
         v3)
  in
  let v4 =
    match v4 with
    | Some tok -> R.Option (Some ((* "," *) token env tok))
    | None -> R.Option None
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_static_final_declaration_list (env : env)
    ((v1, v2) : CST.static_final_declaration_list) =
  let v1 = map_static_final_declaration env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_static_final_declaration env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

let map_interfaces (env : env) ((v1, v2) : CST.interfaces) =
  let v1 = (* "implements" *) token env v1 in
  let v2 = map_type_not_void_list env v2 in
  R.Tuple [ v1; v2 ]

let map_mixins (env : env) ((v1, v2) : CST.mixins) =
  let v1 = (* "with" *) token env v1 in
  let v2 = map_type_not_void_list env v2 in
  R.Tuple [ v1; v2 ]

let map_method_declarator (env : env) ((v1, v2, v3) : CST.method_declarator) =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) token env v1 in
  let v2 = map_formal_parameter_list env v2 in
  let v3 =
    match v3 with
    | Some x -> R.Option (Some (map_dimensions env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3 ]

let map_initializer_list_entry (env : env) (x : CST.initializer_list_entry) =
  match x with
  | `Super_opt_DOT_qual_args (v1, v2, v3) ->
      R.Case
        ( "Super_opt_DOT_qual_args",
          let v1 = (* "super" *) token env v1 in
          let v2 =
            match v2 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = (* "." *) token env v1 in
                      let v2 = map_qualified env v2 in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          let v3 = map_arguments env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Field_init (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Field_init",
          let v1 =
            match v1 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = (* "this" *) token env v1 in
                      let v2 = (* "." *) token env v2 in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
          let v3 = (* "=" *) token env v3 in
          let v4 = map_real_expression env v4 in
          let v5 = R.List (List.map (map_cascade_section env) v5) in
          R.Tuple [ v1; v2; v3; v4; v5 ] )
  | `Asse x -> R.Case ("Asse", map_assertion env x)

let map_configurable_uri (env : env) ((v1, v2) : CST.configurable_uri) =
  let v1 = map_uri env v1 in
  let v2 = R.List (List.map (map_configuration_uri env) v2) in
  R.Tuple [ v1; v2 ]

let map_enum_declaration (env : env) ((v1, v2, v3) : CST.enum_declaration) =
  let v1 = (* "enum" *) token env v1 in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
  let v3 = map_enum_body env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_mixin_application (env : env) ((v1, v2, v3) : CST.mixin_application) =
  let v1 = map_type_not_void env v1 in
  let v2 = map_mixins env v2 in
  let v3 =
    match v3 with
    | Some x -> R.Option (Some (map_interfaces env x))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3 ]

let map_superclass (env : env) (x : CST.superclass) =
  match x with
  | `Extends_type_not_void_opt_mixins (v1, v2, v3) ->
      R.Case
        ( "Extends_type_not_void_opt_mixins",
          let v1 = (* "extends" *) token env v1 in
          let v2 = map_type_not_void env v2 in
          let v3 =
            match v3 with
            | Some x -> R.Option (Some (map_mixins env x))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2; v3 ] )
  | `Mixins x -> R.Case ("Mixins", map_mixins env x)

let map_initializers (env : env) ((v1, v2, v3) : CST.initializers) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_initializer_list_entry env v2 in
  let v3 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_initializer_list_entry env v2 in
           R.Tuple [ v1; v2 ])
         v3)
  in
  R.Tuple [ v1; v2; v3 ]

let map_import_specification (env : env) (x : CST.import_specification) =
  match x with
  | `Import_conf_uri_opt_as_id_rep_comb_semi (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Import_conf_uri_opt_as_id_rep_comb_semi",
          let v1 = (* "import" *) token env v1 in
          let v2 = map_configurable_uri env v2 in
          let v3 =
            match v3 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = (* "as" *) token env v1 in
                      let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          let v4 = R.List (List.map (map_combinator env) v4) in
          let v5 = map_semicolon env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )
  | `Import_uri_defe_as_id_rep_comb_semi (v1, v2, v3, v4, v5, v6, v7) ->
      R.Case
        ( "Import_uri_defe_as_id_rep_comb_semi",
          let v1 = (* "import" *) token env v1 in
          let v2 = map_uri env v2 in
          let v3 = (* "deferred" *) token env v3 in
          let v4 = (* "as" *) token env v4 in
          let v5 = (* pattern [a-zA-Z_$][\w$]* *) token env v5 in
          let v6 = R.List (List.map (map_combinator env) v6) in
          let v7 = map_semicolon env v7 in
          R.Tuple [ v1; v2; v3; v4; v5; v6; v7 ] )

let map_mixin_application_class (env : env)
    ((v1, v2, v3, v4, v5) : CST.mixin_application_class) =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> R.Option (Some (map_type_parameters env x))
    | None -> R.Option None
  in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_mixin_application env v4 in
  let v5 = map_semicolon env v5 in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_method_signature (env : env) (x : CST.method_signature) =
  match x with
  | `Cons_sign_opt_initis (v1, v2) ->
      R.Case
        ( "Cons_sign_opt_initis",
          let v1 = map_constructor_signature env v1 in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_initializers env x))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2 ] )
  | `Fact_cons_sign x ->
      R.Case ("Fact_cons_sign", map_factory_constructor_signature env x)
  | `Opt_static_choice_func_sign (v1, v2) ->
      R.Case
        ( "Opt_static_choice_func_sign",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "static" *) token env tok))
            | None -> R.Option None
          in
          let v2 =
            match v2 with
            | `Func_sign x -> R.Case ("Func_sign", map_function_signature env x)
            | `Getter_sign x ->
                R.Case ("Getter_sign", map_getter_signature env x)
            | `Setter_sign x ->
                R.Case ("Setter_sign", map_setter_signature env x)
          in
          R.Tuple [ v1; v2 ] )
  | `Op_sign x -> R.Case ("Op_sign", map_operator_signature env x)

let map_anon_choice_redi_3f8cf96 (env : env) (x : CST.anon_choice_redi_3f8cf96)
    =
  match x with
  | `Redi (v1, v2, v3, v4) ->
      R.Case
        ( "Redi",
          let v1 = (* ":" *) token env v1 in
          let v2 = (* "this" *) token env v2 in
          let v3 =
            match v3 with
            | Some x -> R.Option (Some (map_dot_identifier env x))
            | None -> R.Option None
          in
          let v4 = map_arguments env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `Initis x -> R.Case ("Initis", map_initializers env x)

let map_declaration_ (env : env) (x : CST.declaration_) =
  match x with
  | `Cst_cons_sign_opt_choice_redi (v1, v2) ->
      R.Case
        ( "Cst_cons_sign_opt_choice_redi",
          let v1 = map_constant_constructor_signature env v1 in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_anon_choice_redi_3f8cf96 env x))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2 ] )
  | `Cons_sign_opt_choice_redi (v1, v2) ->
      R.Case
        ( "Cons_sign_opt_choice_redi",
          let v1 = map_constructor_signature env v1 in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_anon_choice_redi_3f8cf96 env x))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2 ] )
  | `Exte_opt_const_buil_fact_cons_sign (v1, v2, v3) ->
      R.Case
        ( "Exte_opt_const_buil_fact_cons_sign",
          let v1 = (* "external" *) token env v1 in
          let v2 =
            match v2 with
            | Some tok -> R.Option (Some ((* const_builtin *) token env tok))
            | None -> R.Option None
          in
          let v3 = map_factory_constructor_signature env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Opt_const_buil_fact_cons_sign_native (v1, v2, v3) ->
      R.Case
        ( "Opt_const_buil_fact_cons_sign_native",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* const_builtin *) token env tok))
            | None -> R.Option None
          in
          let v2 = map_factory_constructor_signature env v2 in
          let v3 = map_native env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Exte_cst_cons_sign (v1, v2) ->
      R.Case
        ( "Exte_cst_cons_sign",
          let v1 = (* "external" *) token env v1 in
          let v2 = map_constant_constructor_signature env v2 in
          R.Tuple [ v1; v2 ] )
  | `Redi_fact_cons_sign (v1, v2, v3, v4, v5, v6, v7, v8) ->
      R.Case
        ( "Redi_fact_cons_sign",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* const_builtin *) token env tok))
            | None -> R.Option None
          in
          let v2 = (* "factory" *) token env v2 in
          let v3 = (* pattern [a-zA-Z_$][\w$]* *) token env v3 in
          let v4 = R.List (List.map (map_dot_identifier env) v4) in
          let v5 = map_formal_parameter_list env v5 in
          let v6 = (* "=" *) token env v6 in
          let v7 = map_type_not_void env v7 in
          let v8 =
            match v8 with
            | Some x -> R.Option (Some (map_dot_identifier env x))
            | None -> R.Option None
          in
          R.Tuple [ v1; v2; v3; v4; v5; v6; v7; v8 ] )
  | `Exte_cons_sign (v1, v2) ->
      R.Case
        ( "Exte_cons_sign",
          let v1 = (* "external" *) token env v1 in
          let v2 = map_constructor_signature env v2 in
          R.Tuple [ v1; v2 ] )
  | `Opt_exte_buil_opt_static_getter_sign (v1, v2, v3) ->
      R.Case
        ( "Opt_exte_buil_opt_static_getter_sign",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "external" *) token env tok))
            | None -> R.Option None
          in
          let v2 =
            match v2 with
            | Some tok -> R.Option (Some ((* "static" *) token env tok))
            | None -> R.Option None
          in
          let v3 = map_getter_signature env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Opt_exte_and_static_setter_sign (v1, v2) ->
      R.Case
        ( "Opt_exte_and_static_setter_sign",
          let v1 =
            match v1 with
            | Some x -> R.Option (Some (map_external_and_static env x))
            | None -> R.Option None
          in
          let v2 = map_setter_signature env v2 in
          R.Tuple [ v1; v2 ] )
  | `Opt_exte_op_sign (v1, v2) ->
      R.Case
        ( "Opt_exte_op_sign",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "external" *) token env tok))
            | None -> R.Option None
          in
          let v2 = map_operator_signature env v2 in
          R.Tuple [ v1; v2 ] )
  | `Opt_exte_and_static_func_sign (v1, v2) ->
      R.Case
        ( "Opt_exte_and_static_func_sign",
          let v1 =
            match v1 with
            | Some x -> R.Option (Some (map_external_and_static env x))
            | None -> R.Option None
          in
          let v2 = map_function_signature env v2 in
          R.Tuple [ v1; v2 ] )
  | `Static_func_sign (v1, v2) ->
      R.Case
        ( "Static_func_sign",
          let v1 = (* "static" *) token env v1 in
          let v2 = map_function_signature env v2 in
          R.Tuple [ v1; v2 ] )
  | `Static_choice_final_or_const_opt_type_static_final_decl_list (v1, v2) ->
      R.Case
        ( "Static_choice_final_or_const_opt_type_static_final_decl_list",
          let v1 = (* "static" *) token env v1 in
          let v2 =
            match v2 with
            | `Final_or_const_opt_type_static_final_decl_list (v1, v2, v3) ->
                R.Case
                  ( "Final_or_const_opt_type_static_final_decl_list",
                    let v1 = map_final_or_const env v1 in
                    let v2 =
                      match v2 with
                      | Some x -> R.Option (Some (map_type_ env x))
                      | None -> R.Option None
                    in
                    let v3 = map_static_final_declaration_list env v3 in
                    R.Tuple [ v1; v2; v3 ] )
            | `Late_buil_choice_final_buil_opt_type_init_id_list (v1, v2) ->
                R.Case
                  ( "Late_buil_choice_final_buil_opt_type_init_id_list",
                    let v1 = (* "late" *) token env v1 in
                    let v2 =
                      match v2 with
                      | `Final_buil_opt_type_init_id_list (v1, v2, v3) ->
                          R.Case
                            ( "Final_buil_opt_type_init_id_list",
                              let v1 = (* final_builtin *) token env v1 in
                              let v2 =
                                match v2 with
                                | Some x -> R.Option (Some (map_type_ env x))
                                | None -> R.Option None
                              in
                              let v3 = map_initialized_identifier_list env v3 in
                              R.Tuple [ v1; v2; v3 ] )
                      | `Choice_type_init_id_list (v1, v2) ->
                          R.Case
                            ( "Choice_type_init_id_list",
                              let v1 = map_anon_choice_type_be0da33 env v1 in
                              let v2 = map_initialized_identifier_list env v2 in
                              R.Tuple [ v1; v2 ] )
                    in
                    R.Tuple [ v1; v2 ] )
            | `Choice_type_init_id_list (v1, v2) ->
                R.Case
                  ( "Choice_type_init_id_list",
                    let v1 = map_anon_choice_type_be0da33 env v1 in
                    let v2 = map_initialized_identifier_list env v2 in
                    R.Tuple [ v1; v2 ] )
          in
          R.Tuple [ v1; v2 ] )
  | `Cova_choice_late_buil_choice_final_buil_opt_type_id_list_ (v1, v2) ->
      R.Case
        ( "Cova_choice_late_buil_choice_final_buil_opt_type_id_list_",
          let v1 = (* "covariant" *) token env v1 in
          let v2 =
            match v2 with
            | `Late_buil_choice_final_buil_opt_type_id_list_ (v1, v2) ->
                R.Case
                  ( "Late_buil_choice_final_buil_opt_type_id_list_",
                    let v1 = (* "late" *) token env v1 in
                    let v2 =
                      match v2 with
                      | `Final_buil_opt_type_id_list_ (v1, v2, v3) ->
                          R.Case
                            ( "Final_buil_opt_type_id_list_",
                              let v1 = (* final_builtin *) token env v1 in
                              let v2 =
                                match v2 with
                                | Some x -> R.Option (Some (map_type_ env x))
                                | None -> R.Option None
                              in
                              let v3 = map_identifier_list_ env v3 in
                              R.Tuple [ v1; v2; v3 ] )
                      | `Choice_type_init_id_list (v1, v2) ->
                          R.Case
                            ( "Choice_type_init_id_list",
                              let v1 = map_anon_choice_type_be0da33 env v1 in
                              let v2 = map_initialized_identifier_list env v2 in
                              R.Tuple [ v1; v2 ] )
                    in
                    R.Tuple [ v1; v2 ] )
            | `Choice_type_init_id_list (v1, v2) ->
                R.Case
                  ( "Choice_type_init_id_list",
                    let v1 = map_anon_choice_type_be0da33 env v1 in
                    let v2 = map_initialized_identifier_list env v2 in
                    R.Tuple [ v1; v2 ] )
          in
          R.Tuple [ v1; v2 ] )
  | `Opt_late_buil_final_buil_opt_type_init_id_list (v1, v2, v3, v4) ->
      R.Case
        ( "Opt_late_buil_final_buil_opt_type_init_id_list",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "late" *) token env tok))
            | None -> R.Option None
          in
          let v2 = (* final_builtin *) token env v2 in
          let v3 =
            match v3 with
            | Some x -> R.Option (Some (map_type_ env x))
            | None -> R.Option None
          in
          let v4 = map_initialized_identifier_list env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `Opt_late_buil_var_or_type_init_id_list (v1, v2, v3) ->
      R.Case
        ( "Opt_late_buil_var_or_type_init_id_list",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "late" *) token env tok))
            | None -> R.Option None
          in
          let v2 = map_var_or_type env v2 in
          let v3 = map_initialized_identifier_list env v3 in
          R.Tuple [ v1; v2; v3 ] )

let map_import_or_export (env : env) (x : CST.import_or_export) =
  match x with
  | `Libr_import (v1, v2) ->
      R.Case
        ( "Libr_import",
          let v1 =
            match v1 with
            | Some x -> R.Option (Some (map_metadata env x))
            | None -> R.Option None
          in
          let v2 = map_import_specification env v2 in
          R.Tuple [ v1; v2 ] )
  | `Libr_export (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Libr_export",
          let v1 =
            match v1 with
            | Some x -> R.Option (Some (map_metadata env x))
            | None -> R.Option None
          in
          let v2 = (* "export" *) token env v2 in
          let v3 = map_configurable_uri env v3 in
          let v4 = R.List (List.map (map_combinator env) v4) in
          let v5 = map_semicolon env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )

let map_class_member_definition (env : env) (x : CST.class_member_definition) =
  match x with
  | `Decl__semi (v1, v2) ->
      R.Case
        ( "Decl__semi",
          let v1 = map_declaration_ env v1 in
          let v2 = map_semicolon env v2 in
          R.Tuple [ v1; v2 ] )
  | `Meth_sign_func_body (v1, v2) ->
      R.Case
        ( "Meth_sign_func_body",
          let v1 = map_method_signature env v1 in
          let v2 = map_function_body env v2 in
          R.Tuple [ v1; v2 ] )

let map_extension_body (env : env) ((v1, v2, v3) : CST.extension_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun x ->
           match x with
           | `Opt_meta_decl__semi (v1, v2, v3) ->
               R.Case
                 ( "Opt_meta_decl__semi",
                   let v1 =
                     match v1 with
                     | Some x -> R.Option (Some (map_metadata env x))
                     | None -> R.Option None
                   in
                   let v2 = map_declaration_ env v2 in
                   let v3 = map_semicolon env v3 in
                   R.Tuple [ v1; v2; v3 ] )
           | `Opt_meta_meth_sign_func_body (v1, v2, v3) ->
               R.Case
                 ( "Opt_meta_meth_sign_func_body",
                   let v1 =
                     match v1 with
                     | Some x -> R.Option (Some (map_metadata env x))
                     | None -> R.Option None
                   in
                   let v2 = map_method_signature env v2 in
                   let v3 = map_function_body env v3 in
                   R.Tuple [ v1; v2; v3 ] ))
         v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List
      (List.map
         (fun (v1, v2) ->
           let v1 =
             match v1 with
             | Some x -> R.Option (Some (map_metadata env x))
             | None -> R.Option None
           in
           let v2 = map_class_member_definition env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_extension_declaration (env : env) (x : CST.extension_declaration) =
  match x with
  | `Exte_opt_id_opt_type_params_on_type_exte_body (v1, v2, v3, v4, v5, v6) ->
      R.Case
        ( "Exte_opt_id_opt_type_params_on_type_exte_body",
          let v1 = (* "extension" *) token env v1 in
          let v2 =
            match v2 with
            | Some tok ->
                R.Option (Some ((* pattern [a-zA-Z_$][\w$]* *) token env tok))
            | None -> R.Option None
          in
          let v3 =
            match v3 with
            | Some x -> R.Option (Some (map_type_parameters env x))
            | None -> R.Option None
          in
          let v4 = (* "on" *) token env v4 in
          let v5 = map_type_ env v5 in
          let v6 = map_extension_body env v6 in
          R.Tuple [ v1; v2; v3; v4; v5; v6 ] )

let map_class_definition (env : env) (x : CST.class_definition) =
  match x with
  | `Opt_abst_class_id_opt_type_params_opt_supe_opt_inters_class_body
      (v1, v2, v3, v4, v5, v6, v7) ->
      R.Case
        ( "Opt_abst_class_id_opt_type_params_opt_supe_opt_inters_class_body",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "abstract" *) token env tok))
            | None -> R.Option None
          in
          let v2 = (* "class" *) token env v2 in
          let v3 = (* pattern [a-zA-Z_$][\w$]* *) token env v3 in
          let v4 =
            match v4 with
            | Some x -> R.Option (Some (map_type_parameters env x))
            | None -> R.Option None
          in
          let v5 =
            match v5 with
            | Some x -> R.Option (Some (map_superclass env x))
            | None -> R.Option None
          in
          let v6 =
            match v6 with
            | Some x -> R.Option (Some (map_interfaces env x))
            | None -> R.Option None
          in
          let v7 = map_class_body env v7 in
          R.Tuple [ v1; v2; v3; v4; v5; v6; v7 ] )
  | `Opt_meta_opt_abst_class_mixin_app_class (v1, v2, v3, v4) ->
      R.Case
        ( "Opt_meta_opt_abst_class_mixin_app_class",
          let v1 =
            match v1 with
            | Some x -> R.Option (Some (map_metadata env x))
            | None -> R.Option None
          in
          let v2 =
            match v2 with
            | Some tok -> R.Option (Some ((* "abstract" *) token env tok))
            | None -> R.Option None
          in
          let v3 = (* "class" *) token env v3 in
          let v4 = map_mixin_application_class env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )

let map_top_level_definition (env : env) (x : CST.top_level_definition) =
  match x with
  | `Class_defi x -> R.Case ("Class_defi", map_class_definition env x)
  | `Enum_decl x -> R.Case ("Enum_decl", map_enum_declaration env x)
  | `Exte_decl x -> R.Case ("Exte_decl", map_extension_declaration env x)
  | `Mixin_decl (v1, v2, v3, v4, v5, v6) ->
      R.Case
        ( "Mixin_decl",
          let v1 = (* "mixin" *) token env v1 in
          let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
          let v3 =
            match v3 with
            | Some x -> R.Option (Some (map_type_parameters env x))
            | None -> R.Option None
          in
          let v4 =
            match v4 with
            | Some (v1, v2) ->
                R.Option
                  (Some
                     (let v1 = (* "on" *) token env v1 in
                      let v2 = map_type_not_void_list env v2 in
                      R.Tuple [ v1; v2 ]))
            | None -> R.Option None
          in
          let v5 =
            match v5 with
            | Some x -> R.Option (Some (map_interfaces env x))
            | None -> R.Option None
          in
          let v6 = map_class_body env v6 in
          R.Tuple [ v1; v2; v3; v4; v5; v6 ] )
  | `Type_alias x -> R.Case ("Type_alias", map_type_alias env x)
  | `Opt_exte_buil_func_sign_semi (v1, v2, v3) ->
      R.Case
        ( "Opt_exte_buil_func_sign_semi",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "external" *) token env tok))
            | None -> R.Option None
          in
          let v2 = map_function_signature env v2 in
          let v3 = map_semicolon env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Opt_exte_buil_getter_sign_semi (v1, v2, v3) ->
      R.Case
        ( "Opt_exte_buil_getter_sign_semi",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "external" *) token env tok))
            | None -> R.Option None
          in
          let v2 = map_getter_signature env v2 in
          let v3 = map_semicolon env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Opt_exte_buil_setter_sign_semi (v1, v2, v3) ->
      R.Case
        ( "Opt_exte_buil_setter_sign_semi",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "external" *) token env tok))
            | None -> R.Option None
          in
          let v2 = map_setter_signature env v2 in
          let v3 = map_semicolon env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `Func_sign_func_body x ->
      R.Case ("Func_sign_func_body", map_lambda_expression env x)
  | `Getter_sign_func_body (v1, v2) ->
      R.Case
        ( "Getter_sign_func_body",
          let v1 = map_getter_signature env v1 in
          let v2 = map_function_body env v2 in
          R.Tuple [ v1; v2 ] )
  | `Setter_sign_func_body (v1, v2) ->
      R.Case
        ( "Setter_sign_func_body",
          let v1 = map_setter_signature env v1 in
          let v2 = map_function_body env v2 in
          R.Tuple [ v1; v2 ] )
  | `Choice_final_buil_opt_type_static_final_decl_list_semi (v1, v2, v3, v4) ->
      R.Case
        ( "Choice_final_buil_opt_type_static_final_decl_list_semi",
          let v1 = map_final_or_const env v1 in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_type_ env x))
            | None -> R.Option None
          in
          let v3 = map_static_final_declaration_list env v3 in
          let v4 = map_semicolon env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )
  | `Late_buil_final_buil_opt_type_init_id_list_semi (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Late_buil_final_buil_opt_type_init_id_list_semi",
          let v1 = (* "late" *) token env v1 in
          let v2 = (* final_builtin *) token env v2 in
          let v3 =
            match v3 with
            | Some x -> R.Option (Some (map_type_ env x))
            | None -> R.Option None
          in
          let v4 = map_initialized_identifier_list env v4 in
          let v5 = map_semicolon env v5 in
          R.Tuple [ v1; v2; v3; v4; v5 ] )
  | `Opt_late_buil_choice_type_init_id_list_semi (v1, v2, v3, v4) ->
      R.Case
        ( "Opt_late_buil_choice_type_init_id_list_semi",
          let v1 =
            match v1 with
            | Some tok -> R.Option (Some ((* "late" *) token env tok))
            | None -> R.Option None
          in
          let v2 = map_var_or_type env v2 in
          let v3 = map_initialized_identifier_list env v3 in
          let v4 = map_semicolon env v4 in
          R.Tuple [ v1; v2; v3; v4 ] )

let map_program (env : env) (x : CST.program) =
  match x with
  | `Opt_script_tag_opt_libr_name_rep_import_or_export_rep_part_dire_rep_part_of_dire_rep_opt_meta_top_level_defi_rep_stmt
      (v1, v2, v3, v4, v5, v6, v7) ->
      R.Case
        ( "Opt_script_tag_opt_libr_name_rep_import_or_export_rep_part_dire_rep_part_of_dire_rep_opt_meta_top_level_defi_rep_stmt",
          let v1 =
            match v1 with
            | Some x -> R.Option (Some (map_script_tag env x))
            | None -> R.Option None
          in
          let v2 =
            match v2 with
            | Some x -> R.Option (Some (map_library_name env x))
            | None -> R.Option None
          in
          let v3 = R.List (List.map (map_import_or_export env) v3) in
          let v4 = R.List (List.map (map_part_directive env) v4) in
          let v5 = R.List (List.map (map_part_of_directive env) v5) in
          let v6 =
            R.List
              (List.map
                 (fun (v1, v2) ->
                   let v1 =
                     match v1 with
                     | Some x -> R.Option (Some (map_metadata env x))
                     | None -> R.Option None
                   in
                   let v2 = map_top_level_definition env v2 in
                   R.Tuple [ v1; v2 ])
                 v6)
          in
          let v7 = R.List (List.map (map_statement env) v7) in
          R.Tuple [ v1; v2; v3; v4; v5; v6; v7 ] )
  | `Semg_exp (v1, v2) ->
      R.Case
        ( "Semg_exp",
          let v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
          let v2 = map_argument env v2 in
          R.Tuple [ v1; v2 ] )

let dump_tree root =
  map_program () root |> Tree_sitter_run.Raw_tree.to_string |> print_string

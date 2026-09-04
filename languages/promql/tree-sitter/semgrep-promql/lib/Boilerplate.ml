(**
   Boilerplate to be used as a template when mapping the promql CST
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

let map_pat_atan2 (env : env) (tok : CST.pat_atan2) =
  (* pattern [aA][tT][aA][nN]2 *) token env tok

let map_backtick_quoted_string (env : env) (tok : CST.backtick_quoted_string) =
  (* backtick_quoted_string *) token env tok

let map_pat_group_left (env : env) (tok : CST.pat_group_left) =
  (* pattern [gG][rR][oO][uU][pP]_[lL][eE][fF][tT] *) token env tok

let map_pat_group_right (env : env) (tok : CST.pat_group_right) =
  (* pattern [gG][rR][oO][uU][pP]_[rR][iI][gG][hH][tT] *) token env tok

let map_double_quoted_string (env : env) (tok : CST.double_quoted_string) =
  (* double_quoted_string *) token env tok

let map_pat_780550e (env : env) (tok : CST.pat_780550e) =
  (* pattern [0-9]+ *) token env tok

let map_pat_and (env : env) (tok : CST.pat_and) =
  (* pattern [aA][nN][dD] *) token env tok

let map_pat_with (env : env) (tok : CST.pat_with) =
  (* pattern [wW][iI][tT][hH][oO][uU][tT] *) token env tok

let map_pat_bool (env : env) (tok : CST.pat_bool) =
  (* pattern [bB][oO][oO][lL] *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z_:][a-zA-Z0-9_:]* *) token env tok

let map_pat_igno (env : env) (tok : CST.pat_igno) =
  (* pattern [iI][gG][nN][oO][rR][iI][nN][gG] *) token env tok

let map_pat_by (env : env) (tok : CST.pat_by) =
  (* pattern [bB][yY] *) token env tok

let map_pat_or (env : env) (tok : CST.pat_or) =
  (* pattern [oO][rR] *) token env tok

let map_pat_db4e4e9 (env : env) (tok : CST.pat_db4e4e9) =
  (* pattern [-+]?([0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?|0[xX][0-9a-fA-F]+|[nN][aA][nN]|[iI][nN][fF]) *) token env tok

let map_single_quoted_string (env : env) (tok : CST.single_quoted_string) =
  (* single_quoted_string *) token env tok

let map_pat_on (env : env) (tok : CST.pat_on) =
  (* pattern [oO][nN] *) token env tok

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* semgrep_metavariable *) token env tok

let map_pat_unless (env : env) (tok : CST.pat_unless) =
  (* pattern [uU][nN][lL][eE][sS][sS] *) token env tok

let map_pat_dcab316 (env : env) (tok : CST.pat_dcab316) =
  (* pattern [1-9][0-9]* *) token env tok

let map_quoted_string (env : env) (x : CST.quoted_string) =
  (match x with
  | `Single_quoted_str tok -> R.Case ("Single_quoted_str",
      (* single_quoted_string *) token env tok
    )
  | `Double_quoted_str tok -> R.Case ("Double_quoted_str",
      (* double_quoted_string *) token env tok
    )
  | `Back_quoted_str tok -> R.Case ("Back_quoted_str",
      (* backtick_quoted_string *) token env tok
    )
  )

let map_label_name (env : env) (x : CST.label_name) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* pattern [a-zA-Z_:][a-zA-Z0-9_:]* *) token env tok
    )
  )

let map_function_name (env : env) (x : CST.function_name) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* pattern [a-zA-Z_:][a-zA-Z0-9_:]* *) token env tok
    )
  )

let map_duration (env : env) (x : CST.duration) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Rep1_pat_780550e_choice_ms xs -> R.Case ("Rep1_pat_780550e_choice_ms",
      R.List (List.map (fun (v1, v2) ->
        let v1 = map_pat_780550e env v1 in
        let v2 =
          (match v2 with
          | `Ms tok -> R.Case ("Ms",
              (* "ms" *) token env tok
            )
          | `S tok -> R.Case ("S",
              (* "s" *) token env tok
            )
          | `M tok -> R.Case ("M",
              (* "m" *) token env tok
            )
          | `H tok -> R.Case ("H",
              (* "h" *) token env tok
            )
          | `D tok -> R.Case ("D",
              (* "d" *) token env tok
            )
          | `W tok -> R.Case ("W",
              (* "w" *) token env tok
            )
          | `Y tok -> R.Case ("Y",
              (* "y" *) token env tok
            )
          )
        in
        R.Tuple [v1; v2]
      ) xs)
    )
  )

let map_float_literal (env : env) (x : CST.float_literal) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Pat_db4e4e9 x -> R.Case ("Pat_db4e4e9",
      map_pat_db4e4e9 env x
    )
  )

let map_metric_name (env : env) (x : CST.metric_name) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* pattern [a-zA-Z_:][a-zA-Z0-9_:]* *) token env tok
    )
  )

let map_at (env : env) ((v1, v2) : CST.at) =
  let v1 = (* "@" *) token env v1 in
  let v2 =
    (match v2 with
    | `Star tok -> R.Case ("Star",
        (* "start()" *) token env tok
      )
    | `EndL tok -> R.Case ("EndL",
        (* "end()" *) token env tok
      )
    | `Pat_dcab316 x -> R.Case ("Pat_dcab316",
        map_pat_dcab316 env x
      )
    )
  in
  R.Tuple [v1; v2]

let map_label_value (env : env) (x : CST.label_value) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Quoted_str x -> R.Case ("Quoted_str",
      map_quoted_string env x
    )
  )

let map_string_literal (env : env) (x : CST.string_literal) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Quoted_str x -> R.Case ("Quoted_str",
      map_quoted_string env x
    )
  )

let map_anon_choice_semg_ellips_d768110 (env : env) (x : CST.anon_choice_semg_ellips_d768110) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Label_name x -> R.Case ("Label_name",
      map_label_name env x
    )
  )

let map_anon_label_name_rep_COMMA_label_name_opt_COMMA_84ead0c (env : env) ((v1, v2, v3) : CST.anon_label_name_rep_COMMA_label_name_opt_COMMA_84ead0c) =
  let v1 = map_label_name env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_label_name env v2 in
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

let map_range_selection (env : env) ((v1, v2, v3) : CST.range_selection) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_duration env v2 in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_offset (env : env) ((v1, v2, v3) : CST.offset) =
  let v1 = (* "offset" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "-" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_duration env v3 in
  R.Tuple [v1; v2; v3]

let map_subquery_range_selection (env : env) ((v1, v2, v3, v4, v5) : CST.subquery_range_selection) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_duration env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_duration env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_literal_expression (env : env) (x : CST.literal_expression) =
  (match x with
  | `Float_lit x -> R.Case ("Float_lit",
      map_float_literal env x
    )
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  )

let map_grouping (env : env) ((v1, v2, v3, v4) : CST.grouping) =
  let v1 =
    (match v1 with
    | `Pat_by x -> R.Case ("Pat_by",
        map_pat_by env x
      )
    | `Pat_with x -> R.Case ("Pat_with",
        map_pat_with env x
      )
    )
  in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_anon_choice_semg_ellips_d768110 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_semg_ellips_d768110 env v2 in
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
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_binary_grouping (env : env) ((v1, v2, v3, v4, v5) : CST.binary_grouping) =
  let v1 =
    (match v1 with
    | `Pat_on x -> R.Case ("Pat_on",
        map_pat_on env x
      )
    | `Pat_igno x -> R.Case ("Pat_igno",
        map_pat_igno env x
      )
    )
  in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_label_name_rep_COMMA_label_name_opt_COMMA_84ead0c env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | `Pat_group_left x -> R.Case ("Pat_group_left",
              map_pat_group_left env x
            )
          | `Pat_group_right x -> R.Case ("Pat_group_right",
              map_pat_group_right env x
            )
          )
        in
        let v2 =
          (match v2 with
          | Some (v1, v2, v3) -> R.Option (Some (
              let v1 = (* "(" *) token env v1 in
              let v2 =
                (match v2 with
                | Some x -> R.Option (Some (
                    map_anon_label_name_rep_COMMA_label_name_opt_COMMA_84ead0c env x
                  ))
                | None -> R.Option None)
              in
              let v3 = (* ")" *) token env v3 in
              R.Tuple [v1; v2; v3]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

let map_modifier (env : env) (x : CST.modifier) =
  (match x with
  | `Offset_opt_at (v1, v2) -> R.Case ("Offset_opt_at",
      let v1 = map_offset env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_at env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `At_opt_offset (v1, v2) -> R.Case ("At_opt_offset",
      let v1 = map_at env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_offset env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

let map_anon_choice_semg_ellips_c826cc7 (env : env) (x : CST.anon_choice_semg_ellips_c826cc7) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Label_matc (v1, v2, v3) -> R.Case ("Label_matc",
      let v1 = map_label_name env v1 in
      let v2 =
        (match v2 with
        | `EQ tok -> R.Case ("EQ",
            (* "=" *) token env tok
          )
        | `BANGEQ tok -> R.Case ("BANGEQ",
            (* "!=" *) token env tok
          )
        | `EQTILDE tok -> R.Case ("EQTILDE",
            (* "=~" *) token env tok
          )
        | `BANGTILDE tok -> R.Case ("BANGTILDE",
            (* "!~" *) token env tok
          )
        )
      in
      let v3 = map_label_value env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_label_selectors (env : env) ((v1, v2, v3) : CST.label_selectors) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_anon_choice_semg_ellips_c826cc7 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_semg_ellips_c826cc7 env v2 in
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

let map_series_matcher (env : env) (x : CST.series_matcher) =
  (match x with
  | `Metric_name x -> R.Case ("Metric_name",
      map_metric_name env x
    )
  | `Label_selecs x -> R.Case ("Label_selecs",
      map_label_selectors env x
    )
  | `Metric_name_label_selecs (v1, v2) -> R.Case ("Metric_name_label_selecs",
      let v1 = map_metric_name env v1 in
      let v2 = map_label_selectors env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_selector_expression (env : env) (x : CST.selector_expression) =
  (match x with
  | `Inst_vec_sele (v1, v2) -> R.Case ("Inst_vec_sele",
      let v1 = map_series_matcher env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_modifier env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Range_vec_sele (v1, v2, v3) -> R.Case ("Range_vec_sele",
      let v1 = map_series_matcher env v1 in
      let v2 = map_range_selection env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_modifier env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

let rec map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Query_choice_HAT_opt_bin_grou_query (v1, v2, v3, v4) -> R.Case ("Query_choice_HAT_opt_bin_grou_query",
      let v1 = map_query_ env v1 in
      let v2 =
        (match v2 with
        | `HAT tok -> R.Case ("HAT",
            (* "^" *) token env tok
          )
        )
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_binary_grouping env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_query_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Query_choice_STAR_opt_bin_grou_query (v1, v2, v3, v4) -> R.Case ("Query_choice_STAR_opt_bin_grou_query",
      let v1 = map_query_ env v1 in
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
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_binary_grouping env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_query_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Query_choice_PLUS_opt_bin_grou_query (v1, v2, v3, v4) -> R.Case ("Query_choice_PLUS_opt_bin_grou_query",
      let v1 = map_query_ env v1 in
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
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_binary_grouping env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_query_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Query_choice_EQEQ_opt_pat_bool_opt_bin_grou_query (v1, v2, v3, v4, v5) -> R.Case ("Query_choice_EQEQ_opt_pat_bool_opt_bin_grou_query",
      let v1 = map_query_ env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> R.Case ("EQEQ",
            (* "==" *) token env tok
          )
        | `BANGEQ tok -> R.Case ("BANGEQ",
            (* "!=" *) token env tok
          )
        | `GT tok -> R.Case ("GT",
            (* ">" *) token env tok
          )
        | `GTEQ tok -> R.Case ("GTEQ",
            (* ">=" *) token env tok
          )
        | `LT tok -> R.Case ("LT",
            (* "<" *) token env tok
          )
        | `LTEQ tok -> R.Case ("LTEQ",
            (* "<=" *) token env tok
          )
        )
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_pat_bool env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_binary_grouping env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_query_ env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Query_choice_pat_and_opt_bin_grou_query (v1, v2, v3, v4) -> R.Case ("Query_choice_pat_and_opt_bin_grou_query",
      let v1 = map_query_ env v1 in
      let v2 =
        (match v2 with
        | `Pat_and x -> R.Case ("Pat_and",
            map_pat_and env x
          )
        | `Pat_or x -> R.Case ("Pat_or",
            map_pat_or env x
          )
        | `Pat_unless x -> R.Case ("Pat_unless",
            map_pat_unless env x
          )
        )
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_binary_grouping env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_query_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Query_choice_pat_atan2_opt_bin_grou_query (v1, v2, v3, v4) -> R.Case ("Query_choice_pat_atan2_opt_bin_grou_query",
      let v1 = map_query_ env v1 in
      let v2 =
        (match v2 with
        | `Pat_atan2 x -> R.Case ("Pat_atan2",
            map_pat_atan2 env x
          )
        )
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_binary_grouping env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_query_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_call_expression (env : env) (x : CST.call_expression) =
  map_function_call env x

and map_function_args (env : env) ((v1, v2, v3) : CST.function_args) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_query_ env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_query_ env v2 in
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
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_function_call (env : env) (x : CST.function_call) =
  (match x with
  | `Func_name_func_args (v1, v2) -> R.Case ("Func_name_func_args",
      let v1 = map_function_name env v1 in
      let v2 = map_function_args env v2 in
      R.Tuple [v1; v2]
    )
  | `Func_name_grou_func_args (v1, v2, v3) -> R.Case ("Func_name_grou_func_args",
      let v1 = map_function_name env v1 in
      let v2 = map_grouping env v2 in
      let v3 = map_function_args env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Func_name_func_args_grou (v1, v2, v3) -> R.Case ("Func_name_func_args_grou",
      let v1 = map_function_name env v1 in
      let v2 = map_function_args env v2 in
      let v3 = map_grouping env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_operator_expression (env : env) (x : CST.operator_expression) =
  map_binary_expression env x

and map_query (env : env) (x : CST.query) =
  (match x with
  | `Query_exp x -> R.Case ("Query_exp",
      map_query_expression env x
    )
  | `LPAR_query_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_query_exp_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_query_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_query_ (env : env) (x : CST.query_) =
  map_query env x

and map_query_expression (env : env) (x : CST.query_expression) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Choice_lit_exp x -> R.Case ("Choice_lit_exp",
      (match x with
      | `Lit_exp x -> R.Case ("Lit_exp",
          map_literal_expression env x
        )
      | `Sele_exp x -> R.Case ("Sele_exp",
          map_selector_expression env x
        )
      | `Call_exp x -> R.Case ("Call_exp",
          map_call_expression env x
        )
      | `Op_exp x -> R.Case ("Op_exp",
          map_operator_expression env x
        )
      | `Subq_exp x -> R.Case ("Subq_exp",
          map_subquery_expression env x
        )
      )
    )
  )

and map_subquery (env : env) ((v1, v2, v3) : CST.subquery) =
  let v1 = map_query_ env v1 in
  let v2 = map_subquery_range_selection env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_modifier env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_subquery_expression (env : env) (x : CST.subquery_expression) =
  map_subquery env x

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_query_ () root
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

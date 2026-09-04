(**
   Boilerplate to be used as a template when mapping the circom CST
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

let map_signal_visability (env : env) (x : CST.signal_visability) =
  (match x with
  | `Input tok -> R.Case ("Input",
      (* "input" *) token env tok
    )
  | `Output tok -> R.Case ("Output",
      (* "output" *) token env tok
    )
  )

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_int_ (env : env) (tok : CST.int_) =
  (* pattern \d+ *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok

let map_circom_version (env : env) (tok : CST.circom_version) =
  (* pattern "\"?\\.? ?(\\d|\\*\
  )+(\\. ?(\\d|\\*\
  )+ ?(\\.(\\d|\\*\
  )+)?)?\"?" *) token env tok

let map_string_immediate_elt_inside_double_quote (env : env) (tok : CST.string_immediate_elt_inside_double_quote) =
  (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_string_immediate_elt_inside_quote (env : env) (tok : CST.string_immediate_elt_inside_quote) =
  (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_parameter (env : env) (x : CST.parameter) =
  (match x with
  | `Id v1 -> R.Case ("Id",
      (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v1
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_signal_tags (env : env) ((v1, v2, v3, v4) : CST.signal_tags) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_circom_pragma_token (env : env) (x : CST.circom_pragma_token) =
  (match x with
  | `Circom_circom_vers (v1, v2) -> R.Case ("Circom_circom_vers",
      let v1 = (* "circom" *) token env v1 in
      let v2 =
        (* pattern "\"?\\.? ?(\\d|\\*\
  )+(\\. ?(\\d|\\*\
  )+ ?(\\.(\\d|\\*\
  )+)?)?\"?" *) token env v2
      in
      R.Tuple [v1; v2]
    )
  | `Circom_id (v1, v2) -> R.Case ("Circom_id",
      let v1 = (* "circom" *) token env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      R.Tuple [v1; v2]
    )
  )

let map_template_type (env : env) (x : CST.template_type) =
  (match x with
  | `Custom tok -> R.Case ("Custom",
      (* "custom" *) token env tok
    )
  | `Para tok -> R.Case ("Para",
      (* "parallel" *) token env tok
    )
  )

let rec map_anon_choice_id_3723479 (env : env) (x : CST.anon_choice_id_3723479) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
    )
  | `Member_exp x -> R.Case ("Member_exp",
      map_member_expression env x
    )
  | `Array_access_exp x -> R.Case ("Array_access_exp",
      map_array_access_expression env x
    )
  )

and map_anonymous_inputs (env : env) ((v1, v2, v3) : CST.anonymous_inputs) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_argument_list (env : env) ((v1, v2) : CST.argument_list) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_array_ (env : env) ((v1, v2, v3, v4, v5) : CST.array_) =
  let v1 = (* "[" *) token env v1 in
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
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_array_access_expression (env : env) ((v1, v2, v3, v4) : CST.array_access_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_assignment_expression (env : env) ((v1, v2, v3) : CST.assignment_expression) =
  let v1 =
    (match v1 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    )
  in
  let v2 =
    (match v2 with
    | `LTEQEQ tok -> R.Case ("LTEQEQ",
        (* "<==" *) token env tok
      )
    | `EQEQGT tok -> R.Case ("EQEQGT",
        (* "==>" *) token env tok
      )
    | `LTDASHDASH tok -> R.Case ("LTDASHDASH",
        (* "<--" *) token env tok
      )
    | `DASHDASHGT tok -> R.Case ("DASHDASHGT",
        (* "-->" *) token env tok
      )
    | `AMPEQ tok -> R.Case ("AMPEQ",
        (* "&=" *) token env tok
      )
    | `PLUSEQ tok -> R.Case ("PLUSEQ",
        (* "+=" *) token env tok
      )
    | `DASHEQ tok -> R.Case ("DASHEQ",
        (* "-=" *) token env tok
      )
    | `STAREQ tok -> R.Case ("STAREQ",
        (* "*=" *) token env tok
      )
    | `STARSTAREQ tok -> R.Case ("STARSTAREQ",
        (* "**=" *) token env tok
      )
    | `SLASHEQ tok -> R.Case ("SLASHEQ",
        (* "/=" *) token env tok
      )
    | `BSLASHEQ tok -> R.Case ("BSLASHEQ",
        (* "\\=" *) token env tok
      )
    | `PERCEQ tok -> R.Case ("PERCEQ",
        (* "%=" *) token env tok
      )
    | `BAREQ tok -> R.Case ("BAREQ",
        (* "|=" *) token env tok
      )
    | `HATEQ tok -> R.Case ("HATEQ",
        (* "^=" *) token env tok
      )
    | `GTGTEQ tok -> R.Case ("GTGTEQ",
        (* ">>=" *) token env tok
      )
    | `LTLTEQ tok -> R.Case ("LTLTEQ",
        (* "<<=" *) token env tok
      )
    | `EQEQEQ tok -> R.Case ("EQEQEQ",
        (* "===" *) token env tok
      )
    | `EQ tok -> R.Case ("EQ",
        (* "=" *) token env tok
      )
    )
  in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
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
  | `Exp_BSLASH_exp (v1, v2, v3) -> R.Case ("Exp_BSLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "\\" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STARSTAR_exp (v1, v2, v3) -> R.Case ("Exp_STARSTAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) token env v2 in
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
  | `Exp_BANGEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
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
  )

and map_call_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.call_expression) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "parallel" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
  in
  let v3 = (* "(" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_anonymous_inputs env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_decrement_expression (env : env) ((v1, v2) : CST.decrement_expression) =
  let v1 = map_anon_choice_id_3723479 env v1 in
  let v2 = (* "--" *) token env v2 in
  R.Tuple [v1; v2]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_int x -> R.Case ("Choice_int",
      (match x with
      | `Int tok -> R.Case ("Int",
          (* pattern \d+ *) token env tok
        )
      | `Id tok -> R.Case ("Id",
          (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
        )
      | `Array x -> R.Case ("Array",
          map_array_ env x
        )
      | `Tuple x -> R.Case ("Tuple",
          map_tuple env x
        )
      | `Un_exp x -> R.Case ("Un_exp",
          map_unary_expression env x
        )
      | `Bin_exp x -> R.Case ("Bin_exp",
          map_binary_expression env x
        )
      | `Tern_exp x -> R.Case ("Tern_exp",
          map_ternary_expression env x
        )
      | `Paren_exp x -> R.Case ("Paren_exp",
          map_parenthesized_expression env x
        )
      | `Call_exp x -> R.Case ("Call_exp",
          map_call_expression env x
        )
      | `Incr_exp x -> R.Case ("Incr_exp",
          map_increment_expression env x
        )
      | `Decr_exp x -> R.Case ("Decr_exp",
          map_decrement_expression env x
        )
      | `Member_exp x -> R.Case ("Member_exp",
          map_member_expression env x
        )
      | `Array_access_exp x -> R.Case ("Array_access_exp",
          map_array_access_expression env x
        )
      | `Assign_exp x -> R.Case ("Assign_exp",
          map_assignment_expression env x
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips (v1, v2, v3) -> R.Case ("Deep_ellips",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_increment_expression (env : env) ((v1, v2) : CST.increment_expression) =
  let v1 = map_anon_choice_id_3723479 env v1 in
  let v2 = (* "++" *) token env v2 in
  R.Tuple [v1; v2]

and map_member_expression (env : env) ((v1, v2, v3) : CST.member_expression) =
  let v1 =
    (match v1 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Id tok -> R.Case ("Id",
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env tok
      )
    )
  in
  let v2 = (* "." *) token env v2 in
  let v3 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v3
  in
  R.Tuple [v1; v2; v3]

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_ternary_expression (env : env) ((v1, v2, v3, v4, v5) : CST.ternary_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "?" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_expression env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_tuple (env : env) ((v1, v2, v3, v4, v5) : CST.tuple) =
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

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `BANG_exp (v1, v2) -> R.Case ("BANG_exp",
      let v1 = (* "!" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `TILDE_exp (v1, v2) -> R.Case ("TILDE_exp",
      let v1 = (* "~" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `DASH_exp (v1, v2) -> R.Case ("DASH_exp",
      let v1 = (* "-" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `PLUS_exp (v1, v2) -> R.Case ("PLUS_exp",
      let v1 = (* "+" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `DQUOT_rep_choice_str_imme_elt_inside_double_quote_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_rep_choice_str_imme_elt_inside_double_quote_DQUOT",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Str_imme_elt_inside_double_quote tok -> R.Case ("Str_imme_elt_inside_double_quote",
              (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) token env tok
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `SQUOT_rep_choice_str_imme_elt_inside_quote_SQUOT (v1, v2, v3) -> R.Case ("SQUOT_rep_choice_str_imme_elt_inside_quote_SQUOT",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Str_imme_elt_inside_quote tok -> R.Case ("Str_imme_elt_inside_quote",
              (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) token env tok
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
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
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_main_component_public_signals (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.main_component_public_signals) =
  let v1 = (* "{" *) token env v1 in
  let v2 = (* "public" *) token env v2 in
  let v3 = (* "[" *) token env v3 in
  let v4 = map_parameter env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_parameter env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  let v6 =
    (match v6 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v7 = (* "]" *) token env v7 in
  let v8 = (* "}" *) token env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

let map_expression_statement (env : env) (x : CST.expression_statement) =
  (match x with
  | `Exp_semi (v1, v2) -> R.Case ("Exp_semi",
      let v1 = map_expression env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips_SEMI (v1, v2) -> R.Case ("Ellips_SEMI",
      let v1 = (* "..." *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_array_definition (env : env) (xs : CST.array_definition) =
  R.List (List.map (fun (v1, v2, v3) ->
    let v1 = (* "[" *) token env v1 in
    let v2 = map_expression env v2 in
    let v3 = (* "]" *) token env v3 in
    R.Tuple [v1; v2; v3]
  ) xs)

let map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Signal (v1, v2, v3) -> R.Case ("Signal",
      let v1 = (* "signal" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_signal_visability env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_signal_tags env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Var tok -> R.Case ("Var",
      (* "var" *) token env tok
    )
  | `Comp tok -> R.Case ("Comp",
      (* "component" *) token env tok
    )
  )

let map_variable_initialization (env : env) ((v1, v2, v3, v4, v5) : CST.variable_initialization) =
  let v1 =
    (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
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
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_array_definition env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | `EQ tok -> R.Case ("EQ",
              (* "=" *) token env tok
            )
          | `LTEQEQ tok -> R.Case ("LTEQEQ",
              (* "<==" *) token env tok
            )
          | `EQEQGT tok -> R.Case ("EQEQGT",
              (* "==>" *) token env tok
            )
          | `LTDASHDASH tok -> R.Case ("LTDASHDASH",
              (* "<--" *) token env tok
            )
          | `DASHDASHGT tok -> R.Case ("DASHDASHGT",
              (* "-->" *) token env tok
            )
          )
        in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

let map_directive (env : env) (x : CST.directive) =
  (match x with
  | `Pragma_dire (v1, v2, v3) -> R.Case ("Pragma_dire",
      let v1 = (* "pragma" *) token env v1 in
      let v2 =
        (match v2 with
        | `Circom_pragma_tok x -> R.Case ("Circom_pragma_tok",
            map_circom_pragma_token env x
          )
        | `Circom_custom_templs_tok tok -> R.Case ("Circom_custom_templs_tok",
            (* "custom_templates" *) token env tok
          )
        )
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Incl_dire (v1, v2, v3) -> R.Case ("Incl_dire",
      let v1 = (* "include" *) token env v1 in
      let v2 = map_string_ env v2 in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_variable_declaration_statement (env : env) ((v1, v2, v3, v4, v5) : CST.variable_declaration_statement) =
  let v1 = map_type_ env v1 in
  let v2 = map_variable_initialization env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable_initialization env v2 in
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
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let rec map_for_statement (env : env) (x : CST.for_statement) =
  (match x with
  | `For_LPAR_choice_var_decl_stmt_choice_exp_stmt_opt_exp_RPAR_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("For_LPAR_choice_var_decl_stmt_choice_exp_stmt_opt_exp_RPAR_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | `Var_decl_stmt x -> R.Case ("Var_decl_stmt",
            map_variable_declaration_statement env x
          )
        | `Exp_stmt x -> R.Case ("Exp_stmt",
            map_expression_statement env x
          )
        | `Semi tok -> R.Case ("Semi",
            (* ";" *) token env tok
          )
        )
      in
      let v4 =
        (match v4 with
        | `Exp_stmt x -> R.Case ("Exp_stmt",
            map_expression_statement env x
          )
        | `Semi tok -> R.Case ("Semi",
            (* ";" *) token env tok
          )
        )
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* ")" *) token env v6 in
      let v7 = map_statement env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `For_LPAR_ellips_RPAR_stmt (v1, v2, v3, v4, v5) -> R.Case ("For_LPAR_ellips_RPAR_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = (* "..." *) token env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Ret_stmt (v1, v2, v3) -> R.Case ("Ret_stmt",
      let v1 = (* "return" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Blk_stmt (v1, v2, v3) -> R.Case ("Blk_stmt",
      let v1 = (* "{" *) token env v1 in
      let v2 = R.List (List.map (map_statement env) v2) in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `If_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("If_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_statement env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "else" *) token env v1 in
            let v2 = map_statement env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `For_stmt x -> R.Case ("For_stmt",
      map_for_statement env x
    )
  | `While_stmt (v1, v2, v3, v4, v5) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Var_decl_stmt x -> R.Case ("Var_decl_stmt",
      map_variable_declaration_statement env x
    )
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_expression_statement env x
    )
  )

let map_function_body (env : env) ((v1, v2, v3) : CST.function_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_template_body (env : env) ((v1, v2, v3) : CST.template_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_definition (env : env) (x : CST.definition) =
  (match x with
  | `Func_defi (v1, v2, v3, v4) -> R.Case ("Func_defi",
      let v1 = (* "function" *) token env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in
      let v3 = map_parameter_list env v3 in
      let v4 = map_function_body env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Temp_defi (v1, v2, v3, v4, v5) -> R.Case ("Temp_defi",
      let v1 = (* "template" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_template_type env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v3
      in
      let v4 = map_parameter_list env v4 in
      let v5 = map_template_body env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Main_comp_defi (v1, v2, v3, v4, v5, v6) -> R.Case ("Main_comp_defi",
      let v1 = (* "component" *) token env v1 in
      let v2 = (* "main" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_main_component_public_signals env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_call_expression env v5 in
      let v6 = (* ";" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

let map_source_unit (env : env) (x : CST.source_unit) =
  (match x with
  | `Dire x -> R.Case ("Dire",
      map_directive env x
    )
  | `Defi x -> R.Case ("Defi",
      map_definition env x
    )
  )

let map_source_file (env : env) (x : CST.source_file) =
  (match x with
  | `Rep_source_unit v1 -> R.Case ("Rep_source_unit",
      R.List (List.map (map_source_unit env) v1)
    )
  | `Rep1_stmt xs -> R.Case ("Rep1_stmt",
      R.List (List.map (map_statement env) xs)
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
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

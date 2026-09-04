(**
   Boilerplate to be used as a template when mapping the r CST
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

let map_integer (env : env) (tok : CST.integer) =
  (* integer *) token env tok

let map_pat_de5d470 (env : env) (tok : CST.pat_de5d470) =
  (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_pat_4ad362e (env : env) (tok : CST.pat_4ad362e) =
  (* pattern [^`\\\n]+|\\\r?\n *) token env tok

let map_raw_string_literal (env : env) (tok : CST.raw_string_literal) =
  (* raw_string_literal *) token env tok

let map_float_ (env : env) (tok : CST.float_) =
  (* float *) token env tok

let map_na (env : env) (x : CST.na) =
  (match x with
  | `NA tok -> R.Case ("NA",
      (* "NA" *) token env tok
    )
  | `NA_char_ tok -> R.Case ("NA_char_",
      (* "NA_character_" *) token env tok
    )
  | `NA_comp_ tok -> R.Case ("NA_comp_",
      (* "NA_complex_" *) token env tok
    )
  | `NA_int_ tok -> R.Case ("NA_int_",
      (* "NA_integer_" *) token env tok
    )
  | `NA_real_ tok -> R.Case ("NA_real_",
      (* "NA_real_" *) token env tok
    )
  )

let map_pat_3e41275 (env : env) (tok : CST.pat_3e41275) =
  (* pattern [.\p{XID_Start}][._\p{XID_Continue}]* *) token env tok

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* semgrep_metavariable *) token env tok

let map_pat_5e7ac5f (env : env) (tok : CST.pat_5e7ac5f) =
  (* pattern [^%\\\n]+|\\\r?\n *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_pat_3e57880 (env : env) (tok : CST.pat_3e57880) =
  (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) token env tok

let map_special (env : env) ((v1, v2, v3) : CST.special) =
  let v1 = (* "%" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Pat_5e7ac5f x -> R.Case ("Pat_5e7ac5f",
          map_pat_5e7ac5f env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "%" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_identifier (env : env) (x : CST.identifier) =
  (match x with
  | `Choice_pat_3e41275 x -> R.Case ("Choice_pat_3e41275",
      (match x with
      | `Pat_3e41275 x -> R.Case ("Pat_3e41275",
          map_pat_3e41275 env x
        )
      | `BQUOT_rep_choice_pat_4ad362e_BQUOT (v1, v2, v3) -> R.Case ("BQUOT_rep_choice_pat_4ad362e_BQUOT",
          let v1 = (* "`" *) token env v1 in
          let v2 =
            R.List (List.map (fun x ->
              (match x with
              | `Pat_4ad362e x -> R.Case ("Pat_4ad362e",
                  map_pat_4ad362e env x
                )
              | `Esc_seq tok -> R.Case ("Esc_seq",
                  (* escape_sequence *) token env tok
                )
              )
            ) v2)
          in
          let v3 = (* "`" *) token env v3 in
          R.Tuple [v1; v2; v3]
        )
      )
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  )

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `Raw_str_lit tok -> R.Case ("Raw_str_lit",
      (* raw_string_literal *) token env tok
    )
  | `DQUOT_rep_choice_pat_de5d470_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_rep_choice_pat_de5d470_DQUOT",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Pat_de5d470 x -> R.Case ("Pat_de5d470",
              map_pat_de5d470 env x
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
  | `SQUOT_rep_choice_pat_3e57880_SQUOT (v1, v2, v3) -> R.Case ("SQUOT_rep_choice_pat_3e57880_SQUOT",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Pat_3e57880 x -> R.Case ("Pat_3e57880",
              map_pat_3e57880 env x
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

let rec map_argument (env : env) (x : CST.argument) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Defa_arg x -> R.Case ("Defa_arg",
      map_default_argument env x
    )
  )

and map_arguments (env : env) (xs : CST.arguments) =
  R.List (List.map (fun x ->
    (match x with
    | `Arg x -> R.Case ("Arg",
        map_argument env x
      )
    | `COMMA tok -> R.Case ("COMMA",
        (* "," *) token env tok
      )
    )
  ) xs)

and map_assignment (env : env) (x : CST.assignment) =
  (match x with
  | `Equals_assign (v1, v2, v3) -> R.Case ("Equals_assign",
      let v1 = map_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Left_assign (v1, v2, v3) -> R.Case ("Left_assign",
      let v1 = map_expression env v1 in
      let v2 = (* "<-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Left_assign2 (v1, v2, v3) -> R.Case ("Left_assign2",
      let v1 = map_expression env v1 in
      let v2 = (* ":=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Right_assign (v1, v2, v3) -> R.Case ("Right_assign",
      let v1 = map_expression env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Super_assign (v1, v2, v3) -> R.Case ("Super_assign",
      let v1 = map_expression env v1 in
      let v2 = (* "<<-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Super_right_assign (v1, v2, v3) -> R.Case ("Super_right_assign",
      let v1 = map_expression env v1 in
      let v2 = (* "->>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_binary (env : env) (x : CST.binary) =
  (match x with
  | `Exp_choice_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_choice_PLUS_exp",
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
  | `Exp_choice_STAR_exp (v1, v2, v3) -> R.Case ("Exp_choice_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `STAR tok -> R.Case ("STAR",
            (* "*" *) token env tok
          )
        | `SLASH tok -> R.Case ("SLASH",
            (* "/" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_LT_exp (v1, v2, v3) -> R.Case ("Exp_choice_LT_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `LT tok -> R.Case ("LT",
            (* "<" *) token env tok
          )
        | `GT tok -> R.Case ("GT",
            (* ">" *) token env tok
          )
        | `LTEQ tok -> R.Case ("LTEQ",
            (* "<=" *) token env tok
          )
        | `GTEQ tok -> R.Case ("GTEQ",
            (* ">=" *) token env tok
          )
        | `EQEQ tok -> R.Case ("EQEQ",
            (* "==" *) token env tok
          )
        | `BANGEQ tok -> R.Case ("BANGEQ",
            (* "!=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_choice_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `BARBAR tok -> R.Case ("BARBAR",
            (* "||" *) token env tok
          )
        | `BAR tok -> R.Case ("BAR",
            (* "|" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_choice_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `AMPAMP tok -> R.Case ("AMPAMP",
            (* "&&" *) token env tok
          )
        | `AMP tok -> R.Case ("AMP",
            (* "&" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_spec_exp (v1, v2, v3) -> R.Case ("Exp_spec_exp",
      let v1 = map_expression env v1 in
      let v2 = map_special env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_COLON_exp (v1, v2, v3) -> R.Case ("Exp_COLON_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_TILDE_exp (v1, v2, v3) -> R.Case ("Exp_TILDE_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "~" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_default_argument (env : env) ((v1, v2, v3) : CST.default_argument) =
  let v1 =
    (match v1 with
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    | `Str x -> R.Case ("Str",
        map_string_ env x
      )
    | `Dots tok -> R.Case ("Dots",
        (* "..." *) token env tok
      )
    )
  in
  let v2 = (* "=" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Int tok -> R.Case ("Int",
      (* integer *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* float *) token env tok
    )
  | `Comp (v1, v2) -> R.Case ("Comp",
      let v1 = (* float *) token env v1 in
      let v2 = (* "i" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Call (v1, v2, v3, v4) -> R.Case ("Call",
      let v1 = map_expression env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_arguments env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Func_defi x -> R.Case ("Func_defi",
      map_function_definition env x
    )
  | `Lambda_func x -> R.Case ("Lambda_func",
      map_lambda_function env x
    )
  | `Assign x -> R.Case ("Assign",
      map_assignment env x
    )
  | `Brace_list (v1, v2, v3) -> R.Case ("Brace_list",
      let v1 = (* "{" *) token env v1 in
      let v2 = map_program env v2 in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Paren_list (v1, v2, v3) -> R.Case ("Paren_list",
      let v1 = (* "(" *) token env v1 in
      let v2 = R.List (List.map (map_expression env) v2) in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin x -> R.Case ("Bin",
      map_binary env x
    )
  | `Un x -> R.Case ("Un",
      map_unary env x
    )
  | `Pipe (v1, v2, v3) -> R.Case ("Pipe",
      let v1 = map_expression env v1 in
      let v2 = (* "|>" *) token env v2 in
      let v3 = map_pipe_rhs env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Subset (v1, v2, v3, v4) -> R.Case ("Subset",
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_arguments env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Subset2 (v1, v2, v3, v4) -> R.Case ("Subset2",
      let v1 = map_expression env v1 in
      let v2 = (* "[[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_arguments env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Dollar (v1, v2, v3) -> R.Case ("Dollar",
      let v1 = map_expression env v1 in
      let v2 = (* "$" *) token env v2 in
      let v3 =
        (match v3 with
        | `Id x -> R.Case ("Id",
            map_identifier env x
          )
        | `Str x -> R.Case ("Str",
            map_string_ env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Slot (v1, v2, v3) -> R.Case ("Slot",
      let v1 = map_expression env v1 in
      let v2 = (* "@" *) token env v2 in
      let v3 = map_identifier env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Name_get (v1, v2, v3) -> R.Case ("Name_get",
      let v1 = map_identifier env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_identifier env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Name_get_inte (v1, v2, v3) -> R.Case ("Name_get_inte",
      let v1 = map_identifier env v1 in
      let v2 = (* ":::" *) token env v2 in
      let v3 = map_identifier env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `If (v1, v2, v3, v4, v5, v6) -> R.Case ("If",
      let v1 = (* "if" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "else" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `For (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("For",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_identifier env v3 in
      let v4 = (* "in" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* ")" *) token env v6 in
      let v7 = map_expression env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `While (v1, v2, v3, v4, v5) -> R.Case ("While",
      let v1 = (* "while" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Repeat (v1, v2) -> R.Case ("Repeat",
      let v1 = (* "repeat" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Switch (v1, v2, v3, v4, v5, v6) -> R.Case ("Switch",
      let v1 = (* "switch" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "," *) token env v4 in
      let v5 = map_arguments env v5 in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Brk tok -> R.Case ("Brk",
      (* "break" *) token env tok
    )
  | `Next tok -> R.Case ("Next",
      (* "next" *) token env tok
    )
  | `True tok -> R.Case ("True",
      (* "TRUE" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "FALSE" *) token env tok
    )
  | `Null tok -> R.Case ("Null",
      (* "NULL" *) token env tok
    )
  | `Inf tok -> R.Case ("Inf",
      (* "Inf" *) token env tok
    )
  | `Nan tok -> R.Case ("Nan",
      (* "NaN" *) token env tok
    )
  | `Na x -> R.Case ("Na",
      map_na env x
    )
  | `Dots tok -> R.Case ("Dots",
      (* "..." *) token env tok
    )
  )

and map_formal_parameter (env : env) (x : CST.formal_parameter) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Defa_param (v1, v2, v3) -> R.Case ("Defa_param",
      let v1 = map_identifier env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Dots tok -> R.Case ("Dots",
      (* "..." *) token env tok
    )
  )

and map_formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_formal_parameter env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_formal_parameter env v2 in
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

and map_function_definition (env : env) ((v1, v2, v3) : CST.function_definition) =
  let v1 = (* "function" *) token env v1 in
  let v2 = map_formal_parameters env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_lambda_function (env : env) ((v1, v2, v3) : CST.lambda_function) =
  let v1 = (* "\\" *) token env v1 in
  let v2 = map_formal_parameters env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_pipe_rhs (env : env) ((v1, v2, v3, v4) : CST.pipe_rhs) =
  let v1 = map_expression env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_pipe_rhs_arguments env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_pipe_rhs_argument (env : env) (x : CST.pipe_rhs_argument) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Defa_arg x -> R.Case ("Defa_arg",
      map_default_argument env x
    )
  | `Pipe_plac_arg (v1, v2, v3) -> R.Case ("Pipe_plac_arg",
      let v1 = map_identifier env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = (* "_" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_pipe_rhs_arguments (env : env) (xs : CST.pipe_rhs_arguments) =
  R.List (List.map (fun x ->
    (match x with
    | `Pipe_rhs_arg x -> R.Case ("Pipe_rhs_arg",
        map_pipe_rhs_argument env x
      )
    | `COMMA tok -> R.Case ("COMMA",
        (* "," *) token env tok
      )
    )
  ) xs)

and map_program (env : env) (xs : CST.program) =
  R.List (List.map (fun (v1, v2) ->
    let v1 = map_expression env v1 in
    let v2 =
      (match v2 with
      | Some x -> R.Option (Some (
          (match x with
          | `LF tok -> R.Case ("LF",
              (* "\n" *) token env tok
            )
          | `SEMI tok -> R.Case ("SEMI",
              (* ";" *) token env tok
            )
          )
        ))
      | None -> R.Option None)
    in
    R.Tuple [v1; v2]
  ) xs)

and map_unary (env : env) (x : CST.unary) =
  (match x with
  | `Choice_DASH_exp (v1, v2) -> R.Case ("Choice_DASH_exp",
      let v1 =
        (match v1 with
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
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
  )

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

(**
   Boilerplate to be used as a template when mapping the ruby CST
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

let map_element_reference_bracket (env : env) (tok : CST.element_reference_bracket) =
  (* element_reference_bracket *) token env tok

let map_imm_tok_ri (env : env) (tok : CST.imm_tok_ri) =
  (* "ri" *) token env tok

let map_string_content (env : env) (tok : CST.string_content) =
  (* string_content *) token env tok

let map_imm_tok_r (env : env) (tok : CST.imm_tok_r) =
  (* "r" *) token env tok

let map_hash_key_symbol (env : env) (tok : CST.hash_key_symbol) =
  (* hash_key_symbol *) token env tok

let map_pat_3d340f6 (env : env) (tok : CST.pat_3d340f6) =
  (* pattern \s+ *) token env tok

let map_string_start (env : env) (tok : CST.string_start) =
  (* string_start *) token env tok

let map_tok_pat_562b724_pat_f7bc484 (env : env) (tok : CST.tok_pat_562b724_pat_f7bc484) =
  (* tok_pat_562b724_pat_f7bc484 *) token env tok

let map_line_break (env : env) (tok : CST.line_break) =
  (* line_break *) token env tok

let map_string_array_start (env : env) (tok : CST.string_array_start) =
  (* string_array_start *) token env tok

let map_class_variable (env : env) (tok : CST.class_variable) =
  (* class_variable *) token env tok

let map_block_ampersand (env : env) (tok : CST.block_ampersand) =
  (* block_ampersand *) token env tok

let map_uninterpreted (env : env) (tok : CST.uninterpreted) =
  (* pattern (.|\s)* *) token env tok

let map_imm_tok_eq (env : env) (tok : CST.imm_tok_eq) =
  (* "=" *) token env tok

let map_splat_star (env : env) (tok : CST.splat_star) =
  (* splat_star *) token env tok

let map_anon_choice_BANG_b88b9c5 (env : env) (x : CST.anon_choice_BANG_b88b9c5) =
  (match x with
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  )

let map_string_end (env : env) (tok : CST.string_end) =
  (* string_end *) token env tok

let map_semgrep_ellipsis_followed_by_newline (env : env) (tok : CST.semgrep_ellipsis_followed_by_newline) =
  (* semgrep_ellipsis_followed_by_newline *) token env tok

let map_anon_choice_PLUSEQ_6a24756 (env : env) (x : CST.anon_choice_PLUSEQ_6a24756) =
  (match x with
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
  | `BARBAREQ tok -> R.Case ("BARBAREQ",
      (* "||=" *) token env tok
    )
  | `BAREQ tok -> R.Case ("BAREQ",
      (* "|=" *) token env tok
    )
  | `AMPAMPEQ tok -> R.Case ("AMPAMPEQ",
      (* "&&=" *) token env tok
    )
  | `AMPEQ tok -> R.Case ("AMPEQ",
      (* "&=" *) token env tok
    )
  | `PERCEQ tok -> R.Case ("PERCEQ",
      (* "%=" *) token env tok
    )
  | `GTGTEQ tok -> R.Case ("GTGTEQ",
      (* ">>=" *) token env tok
    )
  | `LTLTEQ tok -> R.Case ("LTLTEQ",
      (* "<<=" *) token env tok
    )
  | `HATEQ tok -> R.Case ("HATEQ",
      (* "^=" *) token env tok
    )
  )

let map_float_ (env : env) (tok : CST.float_) =
  (* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
  )? *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_unary_minus (env : env) (tok : CST.unary_minus) =
  (* unary_minus *) token env tok

let map_singleton_class_left_angle_left_langle (env : env) (tok : CST.singleton_class_left_angle_left_langle) =
  (* singleton_class_left_angle_left_langle *) token env tok

let map_symbol_array_start (env : env) (tok : CST.symbol_array_start) =
  (* symbol_array_start *) token env tok

let map_character (env : env) (tok : CST.character) =
  (* pattern \?(\\\S(\{[0-9A-Fa-f]*\}|[0-9A-Fa-f]*|-\S([MC]-\S)?)?|\S) *) token env tok

let map_pat___end__ (env : env) (tok : CST.pat___end__) =
  (* pattern __END__ *) token env tok

let map_instance_variable (env : env) (tok : CST.instance_variable) =
  (* instance_variable *) token env tok

let map_hash_splat_nil (env : env) ((v1, v2) : CST.hash_splat_nil) =
  let v1 = (* "**" *) token env v1 in
  let v2 = (* "nil" *) token env v2 in
  R.Tuple [v1; v2]

let map_constant_suffix_ (env : env) (tok : CST.constant_suffix_) =
  (* constant_suffix_ *) token env tok

let map_tok_pat_562b724_pat_f7bc484_pat_38b534e (env : env) (tok : CST.tok_pat_562b724_pat_f7bc484_pat_38b534e) =
  (* tok_pat_562b724_pat_f7bc484_pat_38b534e *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_regex_start (env : env) (tok : CST.regex_start) =
  (* regex_start *) token env tok

let map_binary_star (env : env) (tok : CST.binary_star) =
  (* binary_star *) token env tok

let map_heredoc_content (env : env) (tok : CST.heredoc_content) =
  (* heredoc_content *) token env tok

let map_operator (env : env) (x : CST.operator) =
  (match x with
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  | `BAR tok -> R.Case ("BAR",
      (* "|" *) token env tok
    )
  | `HAT tok -> R.Case ("HAT",
      (* "^" *) token env tok
    )
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  | `LTEQGT tok -> R.Case ("LTEQGT",
      (* "<=>" *) token env tok
    )
  | `EQEQ tok -> R.Case ("EQEQ",
      (* "==" *) token env tok
    )
  | `EQEQEQ tok -> R.Case ("EQEQEQ",
      (* "===" *) token env tok
    )
  | `EQTILDE tok -> R.Case ("EQTILDE",
      (* "=~" *) token env tok
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
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `BANGEQ tok -> R.Case ("BANGEQ",
      (* "!=" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `SLASH tok -> R.Case ("SLASH",
      (* "/" *) token env tok
    )
  | `PERC tok -> R.Case ("PERC",
      (* "%" *) token env tok
    )
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  | `BANGTILDE tok -> R.Case ("BANGTILDE",
      (* "!~" *) token env tok
    )
  | `STARSTAR tok -> R.Case ("STARSTAR",
      (* "**" *) token env tok
    )
  | `LTLT tok -> R.Case ("LTLT",
      (* "<<" *) token env tok
    )
  | `GTGT tok -> R.Case ("GTGT",
      (* ">>" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  | `PLUSAT tok -> R.Case ("PLUSAT",
      (* "+@" *) token env tok
    )
  | `DASHAT tok -> R.Case ("DASHAT",
      (* "-@" *) token env tok
    )
  | `TILDEAT tok -> R.Case ("TILDEAT",
      (* "~@" *) token env tok
    )
  | `LBRACKRBRACK tok -> R.Case ("LBRACKRBRACK",
      (* "[]" *) token env tok
    )
  | `LBRACKRBRACKEQ tok -> R.Case ("LBRACKRBRACKEQ",
      (* "[]=" *) token env tok
    )
  | `BQUOT tok -> R.Case ("BQUOT",
      (* "`" *) token env tok
    )
  )

let map_no_line_break (env : env) (tok : CST.no_line_break) =
  (* no_line_break *) token env tok

let map_hash_splat_star_star (env : env) (tok : CST.hash_splat_star_star) =
  (* hash_splat_star_star *) token env tok

let map_simple_symbol (env : env) (tok : CST.simple_symbol) =
  (* simple_symbol *) token env tok

let map_imm_tok_lpar (env : env) (tok : CST.imm_tok_lpar) =
  (* "(" *) token env tok

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* semgrep_metavariable *) token env tok

let map_integer (env : env) (tok : CST.integer) =
  (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0[xX][0-9a-fA-F](_?[0-9a-fA-F])* *) token env tok

let map_imm_tok_lbrack (env : env) (tok : CST.imm_tok_lbrack) =
  (* "[" *) token env tok

let map_short_interpolation (env : env) (tok : CST.short_interpolation) =
  (* short_interpolation *) token env tok

let map_imm_tok_coloncolon (env : env) (tok : CST.imm_tok_coloncolon) =
  (* "::" *) token env tok

let map_binary_minus (env : env) (tok : CST.binary_minus) =
  (* binary_minus *) token env tok

let map_tok_prec_p1000_dotdotdot_comma (env : env) (tok : CST.tok_prec_p1000_dotdotdot_comma) =
  (* tok_prec_p1000_dotdotdot_comma *) token env tok

let map_heredoc_body_start (env : env) (tok : CST.heredoc_body_start) =
  (* heredoc_body_start *) token env tok

let map_unary_minus_num (env : env) (tok : CST.unary_minus_num) =
  (* unary_minus_num *) token env tok

let map_identifier_suffix_ (env : env) (tok : CST.identifier_suffix_) =
  (* identifier_suffix_ *) token env tok

let map_imm_tok_i (env : env) (tok : CST.imm_tok_i) =
  (* "i" *) token env tok

let map_heredoc_end (env : env) (tok : CST.heredoc_end) =
  (* heredoc_end *) token env tok

let map_subshell_start (env : env) (tok : CST.subshell_start) =
  (* subshell_start *) token env tok

let map_binary_star_star (env : env) (tok : CST.binary_star_star) =
  (* binary_star_star *) token env tok

let map_tok_pat_3fee85b_pat_f7bc484_pat_38b534e (env : env) (tok : CST.tok_pat_3fee85b_pat_f7bc484_pat_38b534e) =
  (* tok_pat_3fee85b_pat_f7bc484_pat_38b534e *) token env tok

let map_heredoc_beginning (env : env) (tok : CST.heredoc_beginning) =
  (* heredoc_beginning *) token env tok

let map_global_variable (env : env) (tok : CST.global_variable) =
  (* pattern "\\$(-[a-zA-Z0-9_]|[!@&`'+~=/\\\\,;.<>*$?:\"]|[0-9]+|[a-zA-Z_][a-zA-Z0-9_]*\
  )" *) token env tok

let map_imm_tok_colon (env : env) (tok : CST.imm_tok_colon) =
  (* ":" *) token env tok

let map_symbol_start (env : env) (tok : CST.symbol_start) =
  (* symbol_start *) token env tok

let map_anon_choice_DOTDOT_ed078ec (env : env) (x : CST.anon_choice_DOTDOT_ed078ec) =
  (match x with
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  )

let map_terminator (env : env) (x : CST.terminator) =
  (match x with
  | `Line_brk tok -> R.Case ("Line_brk",
      (* line_break *) token env tok
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

let map_constant_suffix (env : env) (x : CST.constant_suffix) =
  (match x with
  | `Tok_pat_562b724_pat_f7bc484_pat_38b534e x -> R.Case ("Tok_pat_562b724_pat_f7bc484_pat_38b534e",
      map_tok_pat_562b724_pat_f7bc484_pat_38b534e env x
    )
  | `Cst_suffix_ tok -> R.Case ("Cst_suffix_",
      (* constant_suffix_ *) token env tok
    )
  )

let map_splat_parameter (env : env) ((v1, v2) : CST.splat_parameter) =
  let v1 = (* "*" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* identifier *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_hash_splat_parameter (env : env) ((v1, v2) : CST.hash_splat_parameter) =
  let v1 = (* "**" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* identifier *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_constant (env : env) (x : CST.constant) =
  (match x with
  | `Tok_pat_562b724_pat_f7bc484 x -> R.Case ("Tok_pat_562b724_pat_f7bc484",
      map_tok_pat_562b724_pat_f7bc484 env x
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  )

let map_int_or_float (env : env) (x : CST.int_or_float) =
  (match x with
  | `Int tok -> R.Case ("Int",
      (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0[xX][0-9a-fA-F](_?[0-9a-fA-F])* *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
  )? *) token env tok
    )
  )

let map_call_operator (env : env) (x : CST.call_operator) =
  (match x with
  | `DOT tok -> R.Case ("DOT",
      (* "." *) token env tok
    )
  | `AMPDOT tok -> R.Case ("AMPDOT",
      (* "&." *) token env tok
    )
  | `Imm_tok_colo x -> R.Case ("Imm_tok_colo",
      map_imm_tok_coloncolon env x
    )
  )

let map_identifier_suffix (env : env) (x : CST.identifier_suffix) =
  (match x with
  | `Tok_pat_3fee85b_pat_f7bc484_pat_38b534e x -> R.Case ("Tok_pat_3fee85b_pat_f7bc484_pat_38b534e",
      map_tok_pat_3fee85b_pat_f7bc484_pat_38b534e env x
    )
  | `Id_suffix_ tok -> R.Case ("Id_suffix_",
      (* identifier_suffix_ *) token env tok
    )
  )

let map_nonlocal_variable (env : env) (x : CST.nonlocal_variable) =
  (match x with
  | `Inst_var tok -> R.Case ("Inst_var",
      (* instance_variable *) token env tok
    )
  | `Class_var tok -> R.Case ("Class_var",
      (* class_variable *) token env tok
    )
  | `Global_var tok -> R.Case ("Global_var",
      (* pattern "\\$(-[a-zA-Z0-9_]|[!@&`'+~=/\\\\,;.<>*$?:\"]|[0-9]+|[a-zA-Z_][a-zA-Z0-9_]*\
  )" *) token env tok
    )
  )

let map_keyword_variable (env : env) (x : CST.keyword_variable) =
  (match x with
  | `Nil tok -> R.Case ("Nil",
      (* "nil" *) token env tok
    )
  | `Self tok -> R.Case ("Self",
      (* "self" *) token env tok
    )
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  | `Line tok -> R.Case ("Line",
      (* "__LINE__" *) token env tok
    )
  | `File tok -> R.Case ("File",
      (* "__FILE__" *) token env tok
    )
  | `Enco tok -> R.Case ("Enco",
      (* "__ENCODING__" *) token env tok
    )
  )

let map_hash_pattern_any_rest (env : env) (x : CST.hash_pattern_any_rest) =
  (match x with
  | `Hash_splat_param x -> R.Case ("Hash_splat_param",
      map_hash_splat_parameter env x
    )
  | `Hash_splat_nil x -> R.Case ("Hash_splat_nil",
      map_hash_splat_nil env x
    )
  )

let rec map_pattern_constant (env : env) (x : CST.pattern_constant) =
  (match x with
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  | `Pat_cst_resol (v1, v2, v3) -> R.Case ("Pat_cst_resol",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_pattern_constant env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_constant env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_complex (env : env) (x : CST.complex) =
  (match x with
  | `Int_or_float_imm_tok_i (v1, v2) -> R.Case ("Int_or_float_imm_tok_i",
      let v1 = map_int_or_float env v1 in
      let v2 = map_imm_tok_i env v2 in
      R.Tuple [v1; v2]
    )
  | `Int_or_float_imm_tok_ri (v1, v2) -> R.Case ("Int_or_float_imm_tok_ri",
      let v1 = map_int_or_float env v1 in
      let v2 = map_imm_tok_ri env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_function_identifier (env : env) (x : CST.function_identifier) =
  (match x with
  | `Id_suffix x -> R.Case ("Id_suffix",
      map_identifier_suffix env x
    )
  | `Cst_suffix x -> R.Case ("Cst_suffix",
      map_constant_suffix env x
    )
  )

let map_variable (env : env) (x : CST.variable) =
  (match x with
  | `Self tok -> R.Case ("Self",
      (* "self" *) token env tok
    )
  | `Super tok -> R.Case ("Super",
      (* "super" *) token env tok
    )
  | `Nonl_var x -> R.Case ("Nonl_var",
      map_nonlocal_variable env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  )

let map_simple_numeric (env : env) (x : CST.simple_numeric) =
  (match x with
  | `Int tok -> R.Case ("Int",
      (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0[xX][0-9a-fA-F](_?[0-9a-fA-F])* *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
  )? *) token env tok
    )
  | `Comp x -> R.Case ("Comp",
      map_complex env x
    )
  | `Rati (v1, v2) -> R.Case ("Rati",
      let v1 = map_int_or_float env v1 in
      let v2 = map_imm_tok_r env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_function_identifier_call (env : env) (x : CST.function_identifier_call) =
  map_function_identifier env x

let map_anon_choice_var_2a392d7 (env : env) (x : CST.anon_choice_var_2a392d7) =
  (match x with
  | `Var x -> R.Case ("Var",
      map_variable env x
    )
  | `Func_id x -> R.Case ("Func_id",
      map_function_identifier_call env x
    )
  )

let map_numeric (env : env) (x : CST.numeric) =
  (match x with
  | `Simple_nume x -> R.Case ("Simple_nume",
      map_simple_numeric env x
    )
  | `Un_lit (v1, v2) -> R.Case ("Un_lit",
      let v1 =
        (match v1 with
        | `Un_minus_num tok -> R.Case ("Un_minus_num",
            (* unary_minus_num *) token env tok
          )
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        )
      in
      let v2 = map_simple_numeric env v2 in
      R.Tuple [v1; v2]
    )
  )

let rec map_anon_choice_blk_ce9ba27 (env : env) (x : CST.anon_choice_blk_ce9ba27) =
  (match x with
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `Do_blk x -> R.Case ("Do_blk",
      map_do_block env x
    )
  )

and map_anon_choice_call__23b9492 (env : env) (x : CST.anon_choice_call__23b9492) =
  (match x with
  | `Call_ x -> R.Case ("Call_",
      map_call_ env x
    )
  | `Choice_var x -> R.Case ("Choice_var",
      map_anon_choice_var_2a392d7 env x
    )
  )

and map_anon_choice_choice_call__cfb94af (env : env) (x : CST.anon_choice_choice_call__cfb94af) =
  (match x with
  | `Choice_call_ x -> R.Case ("Choice_call_",
      map_anon_choice_call__23b9492 env x
    )
  | `Prim_choice_DOT (v1, v2) -> R.Case ("Prim_choice_DOT",
      let v1 = map_primary env v1 in
      let v2 = map_call_operator env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_anon_choice_cst_c1a97cb (env : env) (x : CST.anon_choice_cst_c1a97cb) =
  (match x with
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  | `Scope_resol x -> R.Case ("Scope_resol",
      map_scope_resolution env x
    )
  )

and map_anon_choice_else_4cfa13b (env : env) (x : CST.anon_choice_else_4cfa13b) =
  (match x with
  | `Else x -> R.Case ("Else",
      map_else_ env x
    )
  | `Elsif (v1, v2, v3, v4) -> R.Case ("Elsif",
      let v1 = (* "elsif" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = map_anon_choice_term_b9e1843 env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_else_4cfa13b env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_anon_choice_exp_dae8cc5 (env : env) (x : CST.anon_choice_exp_dae8cc5) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Rescue_modi_exp (v1, v2, v3) -> R.Case ("Rescue_modi_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "rescue" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_lhs_3a98eae (env : env) (x : CST.anon_choice_lhs_3a98eae) =
  (match x with
  | `Lhs x -> R.Case ("Lhs",
      map_lhs env x
    )
  | `Rest_assign (v1, v2) -> R.Case ("Rest_assign",
      let v1 = (* "*" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_lhs env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Dest_left_assign (v1, v2, v3) -> R.Case ("Dest_left_assign",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_left_assignment_list env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_lhs_6f12f8f (env : env) (x : CST.anon_choice_lhs_6f12f8f) =
  (match x with
  | `Lhs x -> R.Case ("Lhs",
      map_lhs env x
    )
  | `Left_assign_list x -> R.Case ("Left_assign_list",
      map_left_assignment_list env x
    )
  )

and map_anon_choice_pair_a4f33e2 (env : env) (x : CST.anon_choice_pair_a4f33e2) =
  (match x with
  | `Pair x -> R.Case ("Pair",
      map_pair env x
    )
  | `Hash_splat_arg x -> R.Case ("Hash_splat_arg",
      map_hash_splat_argument env x
    )
  )

and map_anon_choice_rescue_d627f1b (env : env) (x : CST.anon_choice_rescue_d627f1b) =
  (match x with
  | `Rescue (v1, v2, v3, v4) -> R.Case ("Rescue",
      let v1 = (* "rescue" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_exceptions env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_exception_variable env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_anon_choice_term_b9e1843 env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Else x -> R.Case ("Else",
      map_else_ env x
    )
  | `Ensure (v1, v2) -> R.Case ("Ensure",
      let v1 = (* "ensure" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_block_body env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_anon_choice_term_b9e1843 (env : env) (x : CST.anon_choice_term_b9e1843) =
  (match x with
  | `Term x -> R.Case ("Term",
      map_terminator env x
    )
  | `Then x -> R.Case ("Then",
      map_then_ env x
    )
  )

and map_anon_formal_param_rep_COMMA_formal_param_fcb57c2 (env : env) ((v1, v2) : CST.anon_formal_param_rep_COMMA_formal_param_fcb57c2) =
  let v1 = map_formal_parameter env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_formal_parameter env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e (env : env) ((v1, v2) : CST.anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e) =
  let v1 = map_literal_contents env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_pat_3d340f6 env v1 in
      let v2 = map_literal_contents env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_arg (env : env) (x : CST.arg) =
  (match x with
  | `Un_minus_pow (v1, v2) -> R.Case ("Un_minus_pow",
      let v1 = (* unary_minus_num *) token env v1 in
      let v2 = map_pow env v2 in
      R.Tuple [v1; v2]
    )
  | `Prim x -> R.Case ("Prim",
      map_primary env x
    )
  | `Assign x -> R.Case ("Assign",
      map_assignment env x
    )
  | `Op_assign (v1, v2, v3) -> R.Case ("Op_assign",
      let v1 = map_lhs env v1 in
      let v2 = map_anon_choice_PLUSEQ_6a24756 env v2 in
      let v3 = map_arg_rhs env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cond (v1, v2, v3, v4, v5) -> R.Case ("Cond",
      let v1 = map_arg env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_arg env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_arg env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Range x -> R.Case ("Range",
      map_range env x
    )
  | `Bin x -> R.Case ("Bin",
      map_binary env x
    )
  | `Un x -> R.Case ("Un",
      map_unary env x
    )
  )

and map_arg_rhs (env : env) (x : CST.arg_rhs) =
  (match x with
  | `Arg x -> R.Case ("Arg",
      map_arg env x
    )
  | `Rescue_modi_arg (v1, v2, v3) -> R.Case ("Rescue_modi_arg",
      let v1 = map_arg env v1 in
      let v2 = (* "rescue" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_argument (env : env) (x : CST.argument) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Splat_arg x -> R.Case ("Splat_arg",
      map_splat_argument env x
    )
  | `Hash_splat_arg x -> R.Case ("Hash_splat_arg",
      map_hash_splat_argument env x
    )
  | `Forw_arg tok -> R.Case ("Forw_arg",
      (* "..." *) token env tok
    )
  | `Blk_arg (v1, v2) -> R.Case ("Blk_arg",
      let v1 = (* block_ampersand *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_arg env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Pair x -> R.Case ("Pair",
      map_pair env x
    )
  )

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = map_imm_tok_lpar env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument_list_with_trailing_comma env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_argument_list_with_trailing_comma (env : env) ((v1, v2, v3) : CST.argument_list_with_trailing_comma) =
  let v1 = map_argument env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_argument env v2 in
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

and map_array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument_list_with_trailing_comma env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_array_pattern (env : env) (x : CST.array_pattern) =
  (match x with
  | `LBRACK_opt_array_pat_body_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_opt_array_pat_body_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_array_pattern_body env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_cst_imm_tok_lbrack_opt_array_pat_body_RBRACK (v1, v2, v3, v4) -> R.Case ("Pat_cst_imm_tok_lbrack_opt_array_pat_body_RBRACK",
      let v1 = map_pattern_constant env v1 in
      let v2 = map_imm_tok_lbrack env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_array_pattern_body env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Pat_cst_imm_tok_lpar_opt_array_pat_body_RPAR (v1, v2, v3, v4) -> R.Case ("Pat_cst_imm_tok_lpar_opt_array_pat_body_RPAR",
      let v1 = map_pattern_constant env v1 in
      let v2 = map_imm_tok_lpar env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_array_pattern_body env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_array_pattern_body (env : env) (x : CST.array_pattern_body) =
  (match x with
  | `Pat_expr x -> R.Case ("Pat_expr",
      map_pattern_expr env x
    )
  | `Array_pat_n x -> R.Case ("Array_pat_n",
      map_array_pattern_n env x
    )
  )

and map_array_pattern_n (env : env) (x : CST.array_pattern_n) =
  (match x with
  | `Pat_expr_COMMA (v1, v2) -> R.Case ("Pat_expr_COMMA",
      let v1 = map_pattern_expr env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_expr_COMMA_choice_pat_expr (v1, v2, v3) -> R.Case ("Pat_expr_COMMA_choice_pat_expr",
      let v1 = map_pattern_expr env v1 in
      let v2 = (* "," *) token env v2 in
      let v3 = map_array_pattern_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Splat_param_rep_COMMA_pat_expr (v1, v2) -> R.Case ("Splat_param_rep_COMMA_pat_expr",
      let v1 = map_splat_parameter env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_pattern_expr env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  )

and map_assignment (env : env) (x : CST.assignment) =
  (match x with
  | `Choice_lhs_EQ_choice_choice_arg (v1, v2, v3) -> R.Case ("Choice_lhs_EQ_choice_choice_arg",
      let v1 = map_anon_choice_lhs_6f12f8f env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (match v3 with
        | `Choice_arg x -> R.Case ("Choice_arg",
            map_arg_rhs env x
          )
        | `Splat_arg x -> R.Case ("Splat_arg",
            map_splat_argument env x
          )
        | `Right_assign_list x -> R.Case ("Right_assign_list",
            map_right_assignment_list env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_bare_parameters (env : env) ((v1, v2) : CST.bare_parameters) =
  let v1 = map_simple_formal_parameter env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_formal_parameter env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_begin_ (env : env) ((v1, v2, v3, v4) : CST.begin_) =
  let v1 = (* "begin" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_body_statement env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "end" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_binary (env : env) (x : CST.binary) =
  (match x with
  | `Arg_and_arg (v1, v2, v3) -> R.Case ("Arg_and_arg",
      let v1 = map_arg env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_or_arg (v1, v2, v3) -> R.Case ("Arg_or_arg",
      let v1 = map_arg env v1 in
      let v2 = (* "or" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_BARBAR_arg (v1, v2, v3) -> R.Case ("Arg_BARBAR_arg",
      let v1 = map_arg env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_AMPAMP_arg (v1, v2, v3) -> R.Case ("Arg_AMPAMP_arg",
      let v1 = map_arg env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_choice_LTLT_arg (v1, v2, v3) -> R.Case ("Arg_choice_LTLT_arg",
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `LTLT tok -> R.Case ("LTLT",
            (* "<<" *) token env tok
          )
        | `GTGT tok -> R.Case ("GTGT",
            (* ">>" *) token env tok
          )
        )
      in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_choice_LT_arg (v1, v2, v3) -> R.Case ("Arg_choice_LT_arg",
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
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
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_AMP_arg (v1, v2, v3) -> R.Case ("Arg_AMP_arg",
      let v1 = map_arg env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_choice_HAT_arg (v1, v2, v3) -> R.Case ("Arg_choice_HAT_arg",
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `HAT tok -> R.Case ("HAT",
            (* "^" *) token env tok
          )
        | `BAR tok -> R.Case ("BAR",
            (* "|" *) token env tok
          )
        )
      in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_choice_PLUS_arg (v1, v2, v3) -> R.Case ("Arg_choice_PLUS_arg",
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `Bin_minus tok -> R.Case ("Bin_minus",
            (* binary_minus *) token env tok
          )
        )
      in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_choice_SLASH_arg (v1, v2, v3) -> R.Case ("Arg_choice_SLASH_arg",
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `SLASH tok -> R.Case ("SLASH",
            (* "/" *) token env tok
          )
        | `PERC tok -> R.Case ("PERC",
            (* "%" *) token env tok
          )
        | `Bin_star tok -> R.Case ("Bin_star",
            (* binary_star *) token env tok
          )
        )
      in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_choice_EQEQ_arg (v1, v2, v3) -> R.Case ("Arg_choice_EQEQ_arg",
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> R.Case ("EQEQ",
            (* "==" *) token env tok
          )
        | `BANGEQ tok -> R.Case ("BANGEQ",
            (* "!=" *) token env tok
          )
        | `EQEQEQ tok -> R.Case ("EQEQEQ",
            (* "===" *) token env tok
          )
        | `LTEQGT tok -> R.Case ("LTEQGT",
            (* "<=>" *) token env tok
          )
        | `EQTILDE tok -> R.Case ("EQTILDE",
            (* "=~" *) token env tok
          )
        | `BANGTILDE tok -> R.Case ("BANGTILDE",
            (* "!~" *) token env tok
          )
        )
      in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_bin_star_star_arg (v1, v2, v3) -> R.Case ("Arg_bin_star_star_arg",
      let v1 = map_arg env v1 in
      let v2 = (* binary_star_star *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) ((v1, v2, v3, v4) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_block_parameters env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_block_body env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_block_body (env : env) (x : CST.block_body) =
  map_statements env x

and map_block_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.block_parameters) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_formal_param_rep_COMMA_formal_param_fcb57c2 env x
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
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* ";" *) token env v1 in
        let v2 = (* identifier *) token env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = (* identifier *) token env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v5 = (* "|" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_body_expr (env : env) ((v1, v2) : CST.body_expr) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_arg_rhs env v2 in
  R.Tuple [v1; v2]

and map_body_statement (env : env) (x : CST.body_statement) =
  map_body_statement_ env x

and map_body_statement_ (env : env) (x : CST.body_statement_) =
  (match x with
  | `Stmts_rep_choice_rescue (v1, v2) -> R.Case ("Stmts_rep_choice_rescue",
      let v1 = map_block_body env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_rescue_d627f1b env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Opt_stmts_rep1_choice_rescue (v1, v2) -> R.Case ("Opt_stmts_rep1_choice_rescue",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_block_body env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        R.List (List.map (map_anon_choice_rescue_d627f1b env) v2)
      in
      R.Tuple [v1; v2]
    )
  )

and map_break (env : env) ((v1, v2) : CST.break) =
  let v1 = (* "break" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_call (env : env) (x : CST.call) =
  (match x with
  | `Choice_choice_call__arg_list (v1, v2) -> R.Case ("Choice_choice_call__arg_list",
      let v1 = map_anon_choice_choice_call__cfb94af env v1 in
      let v2 = map_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_choice_call__arg_list_blk (v1, v2, v3) -> R.Case ("Choice_choice_call__arg_list_blk",
      let v1 = map_anon_choice_choice_call__cfb94af env v1 in
      let v2 = map_argument_list env v2 in
      let v3 = map_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_choice_call__arg_list_do_blk (v1, v2, v3) -> R.Case ("Choice_choice_call__arg_list_do_blk",
      let v1 = map_anon_choice_choice_call__cfb94af env v1 in
      let v2 = map_argument_list env v2 in
      let v3 = map_do_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_call__blk (v1, v2) -> R.Case ("Choice_call__blk",
      let v1 = map_anon_choice_call__23b9492 env v1 in
      let v2 = map_block env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_call__do_blk (v1, v2) -> R.Case ("Choice_call__do_blk",
      let v1 = map_anon_choice_call__23b9492 env v1 in
      let v2 = map_do_block env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_call_ (env : env) ((v1, v2, v3) : CST.call_) =
  let v1 = map_primary env v1 in
  let v2 = map_call_operator env v2 in
  let v3 =
    (match v3 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Op x -> R.Case ("Op",
        map_operator env x
      )
    | `Cst x -> R.Case ("Cst",
        map_constant env x
      )
    | `Func_id x -> R.Case ("Func_id",
        map_function_identifier_call env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_case (env : env) ((v1, v2, v3, v4, v5, v6) : CST.case) =
  let v1 = (* "case" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* line_break *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 = map_statement env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v4 = R.List (List.map (map_when_ env) v4) in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_else_ env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* "end" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_case_match (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.case_match) =
  let v1 = (* "case" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* line_break *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_statement env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v5 = R.List (List.map (map_in_clause env) v5) in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_else_ env x
      ))
    | None -> R.Option None)
  in
  let v7 = (* "end" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_chained_command_call (env : env) ((v1, v2, v3) : CST.chained_command_call) =
  let v1 = map_command_call_with_block env v1 in
  let v2 = map_call_operator env v2 in
  let v3 =
    (match v3 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Func_id x -> R.Case ("Func_id",
        map_function_identifier_call env x
      )
    | `Op x -> R.Case ("Op",
        map_operator env x
      )
    | `Cst x -> R.Case ("Cst",
        map_constant env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_chained_string (env : env) ((v1, v2) : CST.chained_string) =
  let v1 = map_string_ env v1 in
  let v2 = R.List (List.map (map_string_ env) v2) in
  R.Tuple [v1; v2]

and map_class_ (env : env) ((v1, v2, v3, v4, v5) : CST.class_) =
  let v1 = (* "class" *) token env v1 in
  let v2 = map_anon_choice_cst_c1a97cb env v2 in
  let v3 =
    (match v3 with
    | `Supe_term (v1, v2) -> R.Case ("Supe_term",
        let v1 = map_superclass env v1 in
        let v2 = map_terminator env v2 in
        R.Tuple [v1; v2]
      )
    | `Opt_term opt -> R.Case ("Opt_term",
        (match opt with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      )
    )
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_body_statement env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_command_argument_list (env : env) (x : CST.command_argument_list) =
  (match x with
  | `Tok_prec_p1000_dotd_comma_arg_rep_COMMA_arg (v1, v2, v3) -> R.Case ("Tok_prec_p1000_dotd_comma_arg_rep_COMMA_arg",
      let v1 = map_tok_prec_p1000_dotdotdot_comma env v1 in
      let v2 = map_argument env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_argument env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_rep_COMMA_arg (v1, v2) -> R.Case ("Arg_rep_COMMA_arg",
      let v1 = map_argument env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_argument env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  )

and map_command_call_with_block (env : env) (x : CST.command_call_with_block) =
  (match x with
  | `Choice_choice_call__cmd_arg_list_blk x -> R.Case ("Choice_choice_call__cmd_arg_list_blk",
      (match x with
      | `Choice_call__cmd_arg_list_blk (v1, v2, v3) -> R.Case ("Choice_call__cmd_arg_list_blk",
          let v1 = map_anon_choice_call__23b9492 env v1 in
          let v2 = map_command_argument_list env v2 in
          let v3 = map_block env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Choice_call__cmd_arg_list_do_blk (v1, v2, v3) -> R.Case ("Choice_call__cmd_arg_list_do_blk",
          let v1 = map_anon_choice_call__23b9492 env v1 in
          let v2 = map_command_argument_list env v2 in
          let v3 = map_do_block env v3 in
          R.Tuple [v1; v2; v3]
        )
      )
    )
  | `Arg_DOTDOTDOT_do_blk (v1, v2, v3) -> R.Case ("Arg_DOTDOTDOT_do_blk",
      let v1 = map_arg env v1 in
      let v2 = (* "..." *) token env v2 in
      let v3 = map_do_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_DOTDOTDOT_blk (v1, v2, v3) -> R.Case ("Arg_DOTDOTDOT_blk",
      let v1 = map_arg env v1 in
      let v2 = (* "..." *) token env v2 in
      let v3 = map_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_command_unary (env : env) (x : CST.command_unary) =
  (match x with
  | `Defi_exp (v1, v2) -> R.Case ("Defi_exp",
      let v1 = (* "defined?" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Not_exp (v1, v2) -> R.Case ("Not_exp",
      let v1 = (* "not" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_un_minus_exp (v1, v2) -> R.Case ("Choice_un_minus_exp",
      let v1 =
        (match v1 with
        | `Un_minus tok -> R.Case ("Un_minus",
            (* unary_minus *) token env tok
          )
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_BANG_exp (v1, v2) -> R.Case ("Choice_BANG_exp",
      let v1 = map_anon_choice_BANG_b88b9c5 env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_delimited_symbol (env : env) ((v1, v2, v3) : CST.delimited_symbol) =
  let v1 = (* symbol_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_literal_contents env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* string_end *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_do_ (env : env) ((v1, v2, v3) : CST.do_) =
  let v1 =
    (match v1 with
    | `Do tok -> R.Case ("Do",
        (* "do" *) token env tok
      )
    | `Term x -> R.Case ("Term",
        map_terminator env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_block_body env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "end" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_do_block (env : env) ((v1, v2, v3, v4, v5) : CST.do_block) =
  let v1 = (* "do" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_block_parameters env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_terminator env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_body_statement env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_else_ (env : env) ((v1, v2, v3) : CST.else_) =
  let v1 = (* "else" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_block_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_exception_variable (env : env) ((v1, v2) : CST.exception_variable) =
  let v1 = (* "=>" *) token env v1 in
  let v2 = map_lhs env v2 in
  R.Tuple [v1; v2]

and map_exceptions (env : env) ((v1, v2) : CST.exceptions) =
  let v1 = map_pattern env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Cmd_bin (v1, v2, v3) -> R.Case ("Cmd_bin",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `Or tok -> R.Case ("Or",
            (* "or" *) token env tok
          )
        | `And tok -> R.Case ("And",
            (* "and" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cmd_un x -> R.Case ("Cmd_un",
      map_command_unary env x
    )
  | `Cmd_assign (v1, v2, v3) -> R.Case ("Cmd_assign",
      let v1 = map_anon_choice_lhs_6f12f8f env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_anon_choice_exp_dae8cc5 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cmd_op_assign (v1, v2, v3) -> R.Case ("Cmd_op_assign",
      let v1 = map_lhs env v1 in
      let v2 = map_anon_choice_PLUSEQ_6a24756 env v2 in
      let v3 = map_anon_choice_exp_dae8cc5 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cmd_call (v1, v2) -> R.Case ("Cmd_call",
      let v1 =
        (match v1 with
        | `Call_ x -> R.Case ("Call_",
            map_call_ env x
          )
        | `Chai_cmd_call x -> R.Case ("Chai_cmd_call",
            map_chained_command_call env x
          )
        | `Choice_var x -> R.Case ("Choice_var",
            map_anon_choice_var_2a392d7 env x
          )
        )
      in
      let v2 = map_command_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Cmd_call_with_blk x -> R.Case ("Cmd_call_with_blk",
      map_command_call_with_block env x
    )
  | `Chai_cmd_call x -> R.Case ("Chai_cmd_call",
      map_chained_command_call env x
    )
  | `Ret_cmd (v1, v2) -> R.Case ("Ret_cmd",
      let v1 = (* "return" *) token env v1 in
      let v2 = map_command_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Yield_cmd (v1, v2) -> R.Case ("Yield_cmd",
      let v1 = (* "yield" *) token env v1 in
      let v2 = map_command_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Brk_cmd (v1, v2) -> R.Case ("Brk_cmd",
      let v1 = (* "break" *) token env v1 in
      let v2 = map_command_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Next_cmd (v1, v2) -> R.Case ("Next_cmd",
      let v1 = (* "next" *) token env v1 in
      let v2 = map_command_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Match_pat (v1, v2, v3) -> R.Case ("Match_pat",
      let v1 = map_arg env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_pattern_top_expr_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Test_pat (v1, v2, v3) -> R.Case ("Test_pat",
      let v1 = map_arg env v1 in
      let v2 = (* "in" *) token env v2 in
      let v3 = map_pattern_top_expr_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg x -> R.Case ("Arg",
      map_arg env x
    )
  )

and map_find_pattern (env : env) (x : CST.find_pattern) =
  (match x with
  | `LBRACK_find_pat_body_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_find_pat_body_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_find_pattern_body env v2 in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_cst_imm_tok_lbrack_find_pat_body_RBRACK (v1, v2, v3, v4) -> R.Case ("Pat_cst_imm_tok_lbrack_find_pat_body_RBRACK",
      let v1 = map_pattern_constant env v1 in
      let v2 = map_imm_tok_lbrack env v2 in
      let v3 = map_find_pattern_body env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Pat_cst_imm_tok_lpar_find_pat_body_RPAR (v1, v2, v3, v4) -> R.Case ("Pat_cst_imm_tok_lpar_find_pat_body_RPAR",
      let v1 = map_pattern_constant env v1 in
      let v2 = map_imm_tok_lpar env v2 in
      let v3 = map_find_pattern_body env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_find_pattern_body (env : env) ((v1, v2, v3, v4) : CST.find_pattern_body) =
  let v1 = map_splat_parameter env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pattern_expr env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = (* "," *) token env v3 in
  let v4 = map_splat_parameter env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_for_ (env : env) ((v1, v2, v3, v4) : CST.for_) =
  let v1 = (* "for" *) token env v1 in
  let v2 = map_anon_choice_lhs_6f12f8f env v2 in
  let v3 = map_in_ env v3 in
  let v4 = map_do_ env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_formal_parameter (env : env) (x : CST.formal_parameter) =
  (match x with
  | `Simple_formal_param x -> R.Case ("Simple_formal_param",
      map_simple_formal_parameter env x
    )
  | `Params x -> R.Case ("Params",
      map_parameters env x
    )
  )

and map_guard (env : env) (x : CST.guard) =
  (match x with
  | `If_guard (v1, v2) -> R.Case ("If_guard",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Unless_guard (v1, v2) -> R.Case ("Unless_guard",
      let v1 = (* "unless" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_hash (env : env) ((v1, v2, v3) : CST.hash) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_anon_choice_pair_a4f33e2 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_pair_a4f33e2 env v2 in
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

and map_hash_pattern (env : env) (x : CST.hash_pattern) =
  (match x with
  | `LCURL_opt_hash_pat_body_RCURL (v1, v2, v3) -> R.Case ("LCURL_opt_hash_pat_body_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_hash_pattern_body env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_cst_imm_tok_lbrack_hash_pat_body_RBRACK (v1, v2, v3, v4) -> R.Case ("Pat_cst_imm_tok_lbrack_hash_pat_body_RBRACK",
      let v1 = map_pattern_constant env v1 in
      let v2 = map_imm_tok_lbrack env v2 in
      let v3 = map_hash_pattern_body env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Pat_cst_imm_tok_lpar_hash_pat_body_RPAR (v1, v2, v3, v4) -> R.Case ("Pat_cst_imm_tok_lpar_hash_pat_body_RPAR",
      let v1 = map_pattern_constant env v1 in
      let v2 = map_imm_tok_lpar env v2 in
      let v3 = map_hash_pattern_body env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_hash_pattern_body (env : env) (x : CST.hash_pattern_body) =
  (match x with
  | `Kw_pat_rep_COMMA_kw_pat_opt_COMMA (v1, v2, v3) -> R.Case ("Kw_pat_rep_COMMA_kw_pat_opt_COMMA",
      let v1 = map_keyword_pattern env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_keyword_pattern env v2 in
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
  | `Kw_pat_rep_COMMA_kw_pat_COMMA_hash_pat_any_rest (v1, v2, v3, v4) -> R.Case ("Kw_pat_rep_COMMA_kw_pat_COMMA_hash_pat_any_rest",
      let v1 = map_keyword_pattern env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_keyword_pattern env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 = (* "," *) token env v3 in
      let v4 = map_hash_pattern_any_rest env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Hash_pat_any_rest x -> R.Case ("Hash_pat_any_rest",
      map_hash_pattern_any_rest env x
    )
  )

and map_hash_splat_argument (env : env) ((v1, v2) : CST.hash_splat_argument) =
  let v1 = (* hash_splat_star_star *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_arg env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_if_ (env : env) ((v1, v2, v3, v4, v5) : CST.if_) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_statement env v2 in
  let v3 = map_anon_choice_term_b9e1843 env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_else_4cfa13b env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_in_ (env : env) ((v1, v2) : CST.in_) =
  let v1 = (* "in" *) token env v1 in
  let v2 = map_arg env v2 in
  R.Tuple [v1; v2]

and map_in_clause (env : env) ((v1, v2, v3, v4) : CST.in_clause) =
  let v1 = (* "in" *) token env v1 in
  let v2 = map_pattern_top_expr_body env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_guard env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_anon_choice_term_b9e1843 env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_interpolation (env : env) (x : CST.interpolation) =
  (match x with
  | `HASHLCURL_opt_stmts_RCURL (v1, v2, v3) -> R.Case ("HASHLCURL_opt_stmts_RCURL",
      let v1 = (* "#{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_block_body env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Short_interp_nonl_var (v1, v2) -> R.Case ("Short_interp_nonl_var",
      let v1 = (* short_interpolation *) token env v1 in
      let v2 = map_nonlocal_variable env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_keyword_pattern (env : env) (x : CST.keyword_pattern) =
  (match x with
  | `Choice_id_imm_tok_colon_opt_pat_expr (v1, v2, v3) -> R.Case ("Choice_id_imm_tok_colon_opt_pat_expr",
      let v1 =
        (match v1 with
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Cst x -> R.Case ("Cst",
            map_constant env x
          )
        | `Id_suffix x -> R.Case ("Id_suffix",
            map_identifier_suffix env x
          )
        | `Cst_suffix x -> R.Case ("Cst_suffix",
            map_constant_suffix env x
          )
        | `Str x -> R.Case ("Str",
            map_string_ env x
          )
        )
      in
      let v2 = map_imm_tok_colon env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_pattern_expr env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_lambda (env : env) ((v1, v2, v3) : CST.lambda) =
  let v1 = (* "->" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Params x -> R.Case ("Params",
            map_parameters env x
          )
        | `Bare_params x -> R.Case ("Bare_params",
            map_bare_parameters env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = map_anon_choice_blk_ce9ba27 env v3 in
  R.Tuple [v1; v2; v3]

and map_left_assignment_list (env : env) (x : CST.left_assignment_list) =
  map_mlhs env x

and map_lhs (env : env) (x : CST.lhs) =
  (match x with
  | `Var x -> R.Case ("Var",
      map_variable env x
    )
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  | `Nil tok -> R.Case ("Nil",
      (* "nil" *) token env tok
    )
  | `Scope_resol x -> R.Case ("Scope_resol",
      map_scope_resolution env x
    )
  | `Elem_ref (v1, v2, v3, v4, v5) -> R.Case ("Elem_ref",
      let v1 = map_primary env v1 in
      let v2 = (* element_reference_bracket *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_argument_list_with_trailing_comma env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_blk_ce9ba27 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Call_ x -> R.Case ("Call_",
      map_call_ env x
    )
  )

and map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Simple_symb tok -> R.Case ("Simple_symb",
      (* simple_symbol *) token env tok
    )
  | `Deli_symb x -> R.Case ("Deli_symb",
      map_delimited_symbol env x
    )
  | `Nume x -> R.Case ("Nume",
      map_numeric env x
    )
  )

and map_literal_contents (env : env) (xs : CST.literal_contents) =
  R.List (List.map (fun x ->
    (match x with
    | `Str_content tok -> R.Case ("Str_content",
        (* string_content *) token env tok
      )
    | `Interp x -> R.Case ("Interp",
        map_interpolation env x
      )
    | `Esc_seq tok -> R.Case ("Esc_seq",
        (* escape_sequence *) token env tok
      )
    )
  ) xs)

and map_method_ (env : env) ((v1, v2) : CST.method_) =
  let v1 = (* "def" *) token env v1 in
  let v2 = map_method_rest env v2 in
  R.Tuple [v1; v2]

and map_method_name (env : env) (x : CST.method_name) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Func_id x -> R.Case ("Func_id",
      map_function_identifier_call env x
    )
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  | `Setter (v1, v2) -> R.Case ("Setter",
      let v1 = (* identifier *) token env v1 in
      let v2 = map_imm_tok_eq env v2 in
      R.Tuple [v1; v2]
    )
  | `Simple_symb tok -> R.Case ("Simple_symb",
      (* simple_symbol *) token env tok
    )
  | `Deli_symb x -> R.Case ("Deli_symb",
      map_delimited_symbol env x
    )
  | `Op x -> R.Case ("Op",
      map_operator env x
    )
  | `Nonl_var x -> R.Case ("Nonl_var",
      map_nonlocal_variable env x
    )
  )

and map_method_rest (env : env) ((v1, v2) : CST.method_rest) =
  let v1 = map_method_name env v1 in
  let v2 =
    (match v2 with
    | `Body_expr x -> R.Case ("Body_expr",
        map_body_expr env x
      )
    | `Params_choice_opt_term_opt_body_stmt_end (v1, v2) -> R.Case ("Params_choice_opt_term_opt_body_stmt_end",
        let v1 = map_parameters env v1 in
        let v2 =
          (match v2 with
          | `Opt_term_opt_body_stmt_end (v1, v2, v3) -> R.Case ("Opt_term_opt_body_stmt_end",
              let v1 =
                (match v1 with
                | Some x -> R.Option (Some (
                    map_terminator env x
                  ))
                | None -> R.Option None)
              in
              let v2 =
                (match v2 with
                | Some x -> R.Option (Some (
                    map_body_statement env x
                  ))
                | None -> R.Option None)
              in
              let v3 = (* "end" *) token env v3 in
              R.Tuple [v1; v2; v3]
            )
          | `Body_expr x -> R.Case ("Body_expr",
              map_body_expr env x
            )
          )
        in
        R.Tuple [v1; v2]
      )
    | `Opt_bare_params_term_opt_body_stmt_end (v1, v2, v3, v4) -> R.Case ("Opt_bare_params_term_opt_body_stmt_end",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_bare_parameters env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_terminator env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_body_statement env x
            ))
          | None -> R.Option None)
        in
        let v4 = (* "end" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  R.Tuple [v1; v2]

and map_mlhs (env : env) ((v1, v2, v3) : CST.mlhs) =
  let v1 = map_anon_choice_lhs_3a98eae env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_lhs_3a98eae env v2 in
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

and map_module_ (env : env) ((v1, v2, v3, v4, v5) : CST.module_) =
  let v1 = (* "module" *) token env v1 in
  let v2 = map_anon_choice_cst_c1a97cb env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_body_statement env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_next (env : env) ((v1, v2) : CST.next) =
  let v1 = (* "next" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_pair (env : env) (x : CST.pair) =
  (match x with
  | `Choice_arg_EQGT_arg x -> R.Case ("Choice_arg_EQGT_arg",
      (match x with
      | `Arg_EQGT_arg (v1, v2, v3) -> R.Case ("Arg_EQGT_arg",
          let v1 = map_arg env v1 in
          let v2 = (* "=>" *) token env v2 in
          let v3 = map_arg env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Choice_str_imm_tok_colon_arg (v1, v2, v3) -> R.Case ("Choice_str_imm_tok_colon_arg",
          let v1 =
            (match v1 with
            | `Str x -> R.Case ("Str",
                map_string_ env x
              )
            )
          in
          let v2 = map_imm_tok_colon env v2 in
          let v3 = map_arg env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Choice_hash_key_symb_imm_tok_colon_choice_opt_arg (v1, v2, v3) -> R.Case ("Choice_hash_key_symb_imm_tok_colon_choice_opt_arg",
          let v1 =
            (match v1 with
            | `Hash_key_symb tok -> R.Case ("Hash_key_symb",
                (* hash_key_symbol *) token env tok
              )
            | `Id tok -> R.Case ("Id",
                (* identifier *) token env tok
              )
            | `Cst x -> R.Case ("Cst",
                map_constant env x
              )
            | `Id_suffix x -> R.Case ("Id_suffix",
                map_identifier_suffix env x
              )
            | `Cst_suffix x -> R.Case ("Cst_suffix",
                map_constant_suffix env x
              )
            )
          in
          let v2 = map_imm_tok_colon env v2 in
          let v3 =
            (match v3 with
            | `Opt_arg opt -> R.Case ("Opt_arg",
                (match opt with
                | Some x -> R.Option (Some (
                    map_arg env x
                  ))
                | None -> R.Option None)
              )
            | `No_line_brk tok -> R.Case ("No_line_brk",
                (* no_line_break *) token env tok
              )
            )
          in
          R.Tuple [v1; v2; v3]
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_formal_param_rep_COMMA_formal_param_fcb57c2 env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_statements (env : env) ((v1, v2, v3) : CST.parenthesized_statements) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_block_body env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_unary (env : env) ((v1, v2) : CST.parenthesized_unary) =
  let v1 =
    (match v1 with
    | `Defi tok -> R.Case ("Defi",
        (* "defined?" *) token env tok
      )
    | `Not tok -> R.Case ("Not",
        (* "not" *) token env tok
      )
    )
  in
  let v2 = map_parenthesized_statements env v2 in
  R.Tuple [v1; v2]

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Arg x -> R.Case ("Arg",
      map_arg env x
    )
  | `Splat_arg x -> R.Case ("Splat_arg",
      map_splat_argument env x
    )
  )

and map_pattern_expr (env : env) (x : CST.pattern_expr) =
  (match x with
  | `As_pat (v1, v2, v3) -> R.Case ("As_pat",
      let v1 = map_pattern_expr env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_expr_alt x -> R.Case ("Pat_expr_alt",
      map_pattern_expr_alt env x
    )
  )

and map_pattern_expr_alt (env : env) (x : CST.pattern_expr_alt) =
  (match x with
  | `Alt_pat (v1, v2) -> R.Case ("Alt_pat",
      let v1 = map_pattern_expr_basic env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "|" *) token env v1 in
          let v2 = map_pattern_expr_basic env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Pat_expr_basic x -> R.Case ("Pat_expr_basic",
      map_pattern_expr_basic env x
    )
  )

and map_pattern_expr_basic (env : env) (x : CST.pattern_expr_basic) =
  (match x with
  | `Pat_value x -> R.Case ("Pat_value",
      map_pattern_value env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Array_pat x -> R.Case ("Array_pat",
      map_array_pattern env x
    )
  | `Find_pat x -> R.Case ("Find_pat",
      map_find_pattern env x
    )
  | `Hash_pat x -> R.Case ("Hash_pat",
      map_hash_pattern env x
    )
  | `Paren_pat (v1, v2, v3) -> R.Case ("Paren_pat",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_pattern_expr env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_pattern_lambda (env : env) (x : CST.pattern_lambda) =
  map_lambda env x

and map_pattern_literal (env : env) (x : CST.pattern_literal) =
  (match x with
  | `Lit x -> R.Case ("Lit",
      map_literal env x
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Subs x -> R.Case ("Subs",
      map_subshell env x
    )
  | `Here_begin tok -> R.Case ("Here_begin",
      (* heredoc_beginning *) token env tok
    )
  | `Regex x -> R.Case ("Regex",
      map_regex env x
    )
  | `Str_array x -> R.Case ("Str_array",
      map_string_array env x
    )
  | `Symb_array x -> R.Case ("Symb_array",
      map_symbol_array env x
    )
  | `Kw_var x -> R.Case ("Kw_var",
      map_keyword_variable env x
    )
  )

and map_pattern_primitive (env : env) (x : CST.pattern_primitive) =
  (match x with
  | `Pat_lit x -> R.Case ("Pat_lit",
      map_pattern_literal env x
    )
  | `Pat_lambda x -> R.Case ("Pat_lambda",
      map_pattern_lambda env x
    )
  )

and map_pattern_range (env : env) (x : CST.pattern_range) =
  (match x with
  | `Pat_prim_choice_DOTDOT_pat_prim (v1, v2, v3) -> R.Case ("Pat_prim_choice_DOTDOT_pat_prim",
      let v1 = map_pattern_primitive env v1 in
      let v2 = map_anon_choice_DOTDOT_ed078ec env v2 in
      let v3 = map_pattern_primitive env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_DOTDOT_pat_prim (v1, v2) -> R.Case ("Choice_DOTDOT_pat_prim",
      let v1 = map_anon_choice_DOTDOT_ed078ec env v1 in
      let v2 = map_pattern_primitive env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_prim_choice_DOTDOT (v1, v2) -> R.Case ("Pat_prim_choice_DOTDOT",
      let v1 = map_pattern_primitive env v1 in
      let v2 = map_anon_choice_DOTDOT_ed078ec env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_pattern_top_expr_body (env : env) (x : CST.pattern_top_expr_body) =
  (match x with
  | `Pat_expr x -> R.Case ("Pat_expr",
      map_pattern_expr env x
    )
  | `Array_pat_n x -> R.Case ("Array_pat_n",
      map_array_pattern_n env x
    )
  | `Find_pat_body x -> R.Case ("Find_pat_body",
      map_find_pattern_body env x
    )
  | `Hash_pat_body x -> R.Case ("Hash_pat_body",
      map_hash_pattern_body env x
    )
  )

and map_pattern_value (env : env) (x : CST.pattern_value) =
  (match x with
  | `Pat_prim x -> R.Case ("Pat_prim",
      map_pattern_primitive env x
    )
  | `Pat_range x -> R.Case ("Pat_range",
      map_pattern_range env x
    )
  | `Var_ref_pat (v1, v2) -> R.Case ("Var_ref_pat",
      let v1 = (* "^" *) token env v1 in
      let v2 =
        (match v2 with
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Nonl_var x -> R.Case ("Nonl_var",
            map_nonlocal_variable env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Exp_ref_pat (v1, v2, v3, v4) -> R.Case ("Exp_ref_pat",
      let v1 = (* "^" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Pat_cst x -> R.Case ("Pat_cst",
      map_pattern_constant env x
    )
  )

and map_pow (env : env) ((v1, v2, v3) : CST.pow) =
  let v1 = map_simple_numeric env v1 in
  let v2 = (* binary_star_star *) token env v2 in
  let v3 = map_arg env v3 in
  R.Tuple [v1; v2; v3]

and map_primary (env : env) (x : CST.primary) =
  (match x with
  | `Choice_paren_stmts x -> R.Case ("Choice_paren_stmts",
      (match x with
      | `Paren_stmts x -> R.Case ("Paren_stmts",
          map_parenthesized_statements env x
        )
      | `Lhs x -> R.Case ("Lhs",
          map_lhs env x
        )
      | `Func_id_call x -> R.Case ("Func_id_call",
          map_function_identifier_call env x
        )
      | `Call x -> R.Case ("Call",
          map_call env x
        )
      | `Array x -> R.Case ("Array",
          map_array_ env x
        )
      | `Str_array x -> R.Case ("Str_array",
          map_string_array env x
        )
      | `Symb_array x -> R.Case ("Symb_array",
          map_symbol_array env x
        )
      | `Hash x -> R.Case ("Hash",
          map_hash env x
        )
      | `Subs x -> R.Case ("Subs",
          map_subshell env x
        )
      | `Lit x -> R.Case ("Lit",
          map_literal env x
        )
      | `Str x -> R.Case ("Str",
          map_string_ env x
        )
      | `Char tok -> R.Case ("Char",
          (* pattern \?(\\\S(\{[0-9A-Fa-f]*\}|[0-9A-Fa-f]*|-\S([MC]-\S)?)?|\S) *) token env tok
        )
      | `Chai_str x -> R.Case ("Chai_str",
          map_chained_string env x
        )
      | `Regex x -> R.Case ("Regex",
          map_regex env x
        )
      | `Lambda x -> R.Case ("Lambda",
          map_pattern_lambda env x
        )
      | `Meth x -> R.Case ("Meth",
          map_method_ env x
        )
      | `Sing_meth x -> R.Case ("Sing_meth",
          map_singleton_method env x
        )
      | `Class x -> R.Case ("Class",
          map_class_ env x
        )
      | `Sing_class x -> R.Case ("Sing_class",
          map_singleton_class env x
        )
      | `Module x -> R.Case ("Module",
          map_module_ env x
        )
      | `Begin x -> R.Case ("Begin",
          map_begin_ env x
        )
      | `While x -> R.Case ("While",
          map_while_ env x
        )
      | `Until x -> R.Case ("Until",
          map_until env x
        )
      | `If x -> R.Case ("If",
          map_if_ env x
        )
      | `Unless x -> R.Case ("Unless",
          map_unless env x
        )
      | `For x -> R.Case ("For",
          map_for_ env x
        )
      | `Case x -> R.Case ("Case",
          map_case env x
        )
      | `Case_match x -> R.Case ("Case_match",
          map_case_match env x
        )
      | `Ret x -> R.Case ("Ret",
          map_return env x
        )
      | `Yield x -> R.Case ("Yield",
          map_yield env x
        )
      | `Brk x -> R.Case ("Brk",
          map_break env x
        )
      | `Next x -> R.Case ("Next",
          map_next env x
        )
      | `Redo x -> R.Case ("Redo",
          map_redo env x
        )
      | `Retry x -> R.Case ("Retry",
          map_retry env x
        )
      | `Paren_un x -> R.Case ("Paren_un",
          map_parenthesized_unary env x
        )
      | `Here_begin tok -> R.Case ("Here_begin",
          (* heredoc_beginning *) token env tok
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Semg_ellips_foll_by_nl tok -> R.Case ("Semg_ellips_foll_by_nl",
      (* semgrep_ellipsis_followed_by_newline *) token env tok
    )
  | `Deep_ellips (v1, v2, v3) -> R.Case ("Deep_ellips",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_range (env : env) (x : CST.range) =
  (match x with
  | `Arg_choice_DOTDOT_arg (v1, v2, v3) -> R.Case ("Arg_choice_DOTDOT_arg",
      let v1 = map_arg env v1 in
      let v2 = map_anon_choice_DOTDOT_ed078ec env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_DOTDOT_arg (v1, v2) -> R.Case ("Choice_DOTDOT_arg",
      let v1 = map_anon_choice_DOTDOT_ed078ec env v1 in
      let v2 = map_arg env v2 in
      R.Tuple [v1; v2]
    )
  | `Arg_choice_DOTDOT (v1, v2) -> R.Case ("Arg_choice_DOTDOT",
      let v1 = map_arg env v1 in
      let v2 = map_anon_choice_DOTDOT_ed078ec env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_redo (env : env) ((v1, v2) : CST.redo) =
  let v1 = (* "redo" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_regex (env : env) ((v1, v2, v3) : CST.regex) =
  let v1 = (* regex_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_literal_contents env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* string_end *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_retry (env : env) ((v1, v2) : CST.retry) =
  let v1 = (* "retry" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_return (env : env) ((v1, v2) : CST.return) =
  let v1 = (* "return" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_right_assignment_list (env : env) ((v1, v2) : CST.right_assignment_list) =
  let v1 = map_pattern env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_scope_resolution (env : env) ((v1, v2) : CST.scope_resolution) =
  let v1 =
    (match v1 with
    | `COLONCOLON tok -> R.Case ("COLONCOLON",
        (* "::" *) token env tok
      )
    | `Prim_imm_tok_colo (v1, v2) -> R.Case ("Prim_imm_tok_colo",
        let v1 = map_primary env v1 in
        let v2 = map_imm_tok_coloncolon env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v2 = map_constant env v2 in
  R.Tuple [v1; v2]

and map_simple_formal_parameter (env : env) (x : CST.simple_formal_parameter) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Splat_param x -> R.Case ("Splat_param",
      map_splat_parameter env x
    )
  | `Hash_splat_param x -> R.Case ("Hash_splat_param",
      map_hash_splat_parameter env x
    )
  | `Hash_splat_nil x -> R.Case ("Hash_splat_nil",
      map_hash_splat_nil env x
    )
  | `Forw_param tok -> R.Case ("Forw_param",
      (* "..." *) token env tok
    )
  | `Blk_param (v1, v2) -> R.Case ("Blk_param",
      let v1 = (* "&" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* identifier *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Kw_param (v1, v2, v3) -> R.Case ("Kw_param",
      let v1 = (* identifier *) token env v1 in
      let v2 = map_imm_tok_colon env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_arg env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_param (v1, v2, v3) -> R.Case ("Opt_param",
      let v1 = (* identifier *) token env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_singleton_class (env : env) ((v1, v2, v3, v4, v5, v6) : CST.singleton_class) =
  let v1 = (* "class" *) token env v1 in
  let v2 =
    (* singleton_class_left_angle_left_langle *) token env v2
  in
  let v3 = map_arg env v3 in
  let v4 = map_terminator env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_body_statement env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* "end" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_singleton_method (env : env) ((v1, v2, v3, v4) : CST.singleton_method) =
  let v1 = (* "def" *) token env v1 in
  let v2 =
    (match v2 with
    | `Var x -> R.Case ("Var",
        map_variable env x
      )
    | `LPAR_arg_RPAR (v1, v2, v3) -> R.Case ("LPAR_arg_RPAR",
        let v1 = (* "(" *) token env v1 in
        let v2 = map_arg env v2 in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v3 =
    (match v3 with
    | `DOT tok -> R.Case ("DOT",
        (* "." *) token env tok
      )
    | `COLONCOLON tok -> R.Case ("COLONCOLON",
        (* "::" *) token env tok
      )
    )
  in
  let v4 = map_method_rest env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_splat_argument (env : env) ((v1, v2) : CST.splat_argument) =
  let v1 = (* splat_star *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_arg env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Undef (v1, v2, v3) -> R.Case ("Undef",
      let v1 = (* "undef" *) token env v1 in
      let v2 = map_method_name env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_method_name env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Alias (v1, v2, v3) -> R.Case ("Alias",
      let v1 = (* "alias" *) token env v1 in
      let v2 = map_method_name env v2 in
      let v3 = map_method_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `If_modi (v1, v2, v3) -> R.Case ("If_modi",
      let v1 = map_statement env v1 in
      let v2 = (* "if" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Unless_modi (v1, v2, v3) -> R.Case ("Unless_modi",
      let v1 = map_statement env v1 in
      let v2 = (* "unless" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `While_modi (v1, v2, v3) -> R.Case ("While_modi",
      let v1 = map_statement env v1 in
      let v2 = (* "while" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Until_modi (v1, v2, v3) -> R.Case ("Until_modi",
      let v1 = map_statement env v1 in
      let v2 = (* "until" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Rescue_modi (v1, v2, v3) -> R.Case ("Rescue_modi",
      let v1 = map_statement env v1 in
      let v2 = (* "rescue" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Begin_blk (v1, v2, v3, v4) -> R.Case ("Begin_blk",
      let v1 = (* "BEGIN" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_block_body env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `End_blk (v1, v2, v3, v4) -> R.Case ("End_blk",
      let v1 = (* "END" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_block_body env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_statements (env : env) (x : CST.statements) =
  (match x with
  | `Rep1_choice_stmt_term_opt_stmt (v1, v2) -> R.Case ("Rep1_choice_stmt_term_opt_stmt",
      let v1 =
        R.List (List.map (fun x ->
          (match x with
          | `Stmt_term (v1, v2) -> R.Case ("Stmt_term",
              let v1 = map_statement env v1 in
              let v2 = map_terminator env v2 in
              R.Tuple [v1; v2]
            )
          | `Empty_stmt tok -> R.Case ("Empty_stmt",
              (* ";" *) token env tok
            )
          )
        ) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_statement env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Stmt x -> R.Case ("Stmt",
      map_statement env x
    )
  )

and map_string_ (env : env) ((v1, v2, v3) : CST.string_) =
  let v1 = (* string_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_literal_contents env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* string_end *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_string_array (env : env) ((v1, v2, v3, v4, v5) : CST.string_array) =
  let v1 = (* string_array_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_pat_3d340f6 env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_pat_3d340f6 env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* string_end *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_subshell (env : env) ((v1, v2, v3) : CST.subshell) =
  let v1 = (* subshell_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_literal_contents env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* string_end *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_superclass (env : env) ((v1, v2) : CST.superclass) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_symbol_array (env : env) ((v1, v2, v3, v4, v5) : CST.symbol_array) =
  let v1 = (* symbol_array_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_pat_3d340f6 env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_pat_3d340f6 env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* string_end *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_then_ (env : env) (x : CST.then_) =
  (match x with
  | `Term_stmts (v1, v2) -> R.Case ("Term_stmts",
      let v1 = map_terminator env v1 in
      let v2 = map_block_body env v2 in
      R.Tuple [v1; v2]
    )
  | `Opt_term_then_opt_stmts (v1, v2, v3) -> R.Case ("Opt_term_then_opt_stmts",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "then" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_block_body env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_unary (env : env) (x : CST.unary) =
  (match x with
  | `Defi_arg (v1, v2) -> R.Case ("Defi_arg",
      let v1 = (* "defined?" *) token env v1 in
      let v2 = map_arg env v2 in
      R.Tuple [v1; v2]
    )
  | `Not_arg (v1, v2) -> R.Case ("Not_arg",
      let v1 = (* "not" *) token env v1 in
      let v2 = map_arg env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_un_minus_arg (v1, v2) -> R.Case ("Choice_un_minus_arg",
      let v1 =
        (match v1 with
        | `Un_minus tok -> R.Case ("Un_minus",
            (* unary_minus *) token env tok
          )
        | `Bin_minus tok -> R.Case ("Bin_minus",
            (* binary_minus *) token env tok
          )
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        )
      in
      let v2 = map_arg env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_BANG_arg (v1, v2) -> R.Case ("Choice_BANG_arg",
      let v1 = map_anon_choice_BANG_b88b9c5 env v1 in
      let v2 = map_arg env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_unless (env : env) ((v1, v2, v3, v4, v5) : CST.unless) =
  let v1 = (* "unless" *) token env v1 in
  let v2 = map_statement env v2 in
  let v3 = map_anon_choice_term_b9e1843 env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_else_4cfa13b env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_until (env : env) ((v1, v2, v3) : CST.until) =
  let v1 = (* "until" *) token env v1 in
  let v2 = map_statement env v2 in
  let v3 = map_do_ env v3 in
  R.Tuple [v1; v2; v3]

and map_when_ (env : env) ((v1, v2, v3, v4) : CST.when_) =
  let v1 = (* "when" *) token env v1 in
  let v2 = map_pattern env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = map_anon_choice_term_b9e1843 env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_while_ (env : env) ((v1, v2, v3) : CST.while_) =
  let v1 = (* "while" *) token env v1 in
  let v2 = map_statement env v2 in
  let v3 = map_do_ env v3 in
  R.Tuple [v1; v2; v3]

and map_yield (env : env) ((v1, v2) : CST.yield) =
  let v1 = (* "yield" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_block_body env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Pat___end___unin (v1, v2) -> R.Case ("Pat___end___unin",
            let v1 = map_pat___end__ env v1 in
            let v2 = (* pattern (.|\s)* *) token env v2 in
            R.Tuple [v1; v2]
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_heredoc_body (env : env) ((v1, v2, v3) : CST.heredoc_body) =
  let v1 = (* heredoc_body_start *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Here_content tok -> R.Case ("Here_content",
          (* heredoc_content *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* heredoc_end *) token env v3 in
  R.Tuple [v1; v2; v3]

let dump_tree root =
  map_program () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Heredoc_body (_loc, x) -> ("heredoc_body", "heredoc_body", map_heredoc_body env x)

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

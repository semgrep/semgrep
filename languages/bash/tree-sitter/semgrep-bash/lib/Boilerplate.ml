(**
   Boilerplate to be used as a template when mapping the bash CST
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

let map_imm_tok_e (env : env) (tok : CST.imm_tok_e) =
  (* "E" *) token env tok

let map_semgrep_metavar_pluseq (env : env) (tok : CST.semgrep_metavar_pluseq) =
  (* pattern \$[A-Z_][A-Z_0-9]*\+= *) token env tok

let map_empty_value (env : env) (tok : CST.empty_value) =
  (* empty_value *) token env tok

let map_imm_tok_dotdot (env : env) (tok : CST.imm_tok_dotdot) =
  (* ".." *) token env tok

let map_imm_tok_hash (env : env) (tok : CST.imm_tok_hash) =
  (* "#" *) token env tok

let map_anon_choice_LTLT_5d21964 (env : env) (x : CST.anon_choice_LTLT_5d21964) =
  (match x with
  | `LTLT tok -> R.Case ("LTLT",
      (* "<<" *) token env tok
    )
  | `GTGT tok -> R.Case ("GTGT",
      (* ">>" *) token env tok
    )
  )

let map_imm_tok_star (env : env) (tok : CST.imm_tok_star) =
  (* "*" *) token env tok

let map_ansi_c_string (env : env) (tok : CST.ansi_c_string) =
  (* pattern "\\$'([^']|\\\\')*'" *) token env tok

let map_bare_dollar (env : env) (tok : CST.bare_dollar) =
  (* bare_dollar *) token env tok

let map_semgrep_named_ellipsis (env : env) (tok : CST.semgrep_named_ellipsis) =
  (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok

let map_string_content (env : env) (tok : CST.string_content) =
  (* string_content *) token env tok

let map_imm_tok_p (env : env) (tok : CST.imm_tok_p) =
  (* "P" *) token env tok

let map_semgrep_metavar_eq (env : env) (tok : CST.semgrep_metavar_eq) =
  (* pattern \$[A-Z_][A-Z_0-9]*= *) token env tok

let map_external_expansion_sym_bang (env : env) (tok : CST.external_expansion_sym_bang) =
  (* external_expansion_sym_bang *) token env tok

let map_word (env : env) (tok : CST.word) =
  (* word *) token env tok

let map_pat_2b6adbc (env : env) (tok : CST.pat_2b6adbc) =
  (* pattern [a-zA-Z_][a-zA-Z0-9_]* *) token env tok

let map_anon_choice_PLUS_da42005 (env : env) (x : CST.anon_choice_PLUS_da42005) =
  (match x with
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  )

let map_tok_prec_p1_dashdash (env : env) (tok : CST.tok_prec_p1_dashdash) =
  (* tok_prec_p1_dashdash *) token env tok

let map_imm_tok_coloneq (env : env) (tok : CST.imm_tok_coloneq) =
  (* ":=" *) token env tok

let map_imm_tok_k_ (env : env) (tok : CST.imm_tok_k_) =
  (* "k" *) token env tok

let map_regex_no_space (env : env) (tok : CST.regex_no_space) =
  (* regex_no_space *) token env tok

let map_anon_choice_BAR_9062646 (env : env) (x : CST.anon_choice_BAR_9062646) =
  (match x with
  | `BAR tok -> R.Case ("BAR",
      (* "|" *) token env tok
    )
  | `BARAMP tok -> R.Case ("BARAMP",
      (* "|&" *) token env tok
    )
  )

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok

let map_regex_no_slash (env : env) (tok : CST.regex_no_slash) =
  (* regex_no_slash *) token env tok

let map_pat_3d340f6 (env : env) (tok : CST.pat_3d340f6) =
  (* pattern \s+ *) token env tok

let map_anon_choice_EQ_2bc102b (env : env) (x : CST.anon_choice_EQ_2bc102b) =
  (match x with
  | `EQ tok -> R.Case ("EQ",
      (* "=" *) token env tok
    )
  | `EQTILDE tok -> R.Case ("EQTILDE",
      (* "=~" *) token env tok
    )
  )

let map_comment_word (env : env) (tok : CST.comment_word) =
  (* comment_word *) token env tok

let map_tok_prec_n1_rcurl (env : env) (tok : CST.tok_prec_n1_rcurl) =
  (* tok_prec_n1_rcurl *) token env tok

let map_heredoc_body_beginning (env : env) (tok : CST.heredoc_body_beginning) =
  (* heredoc_body_beginning *) token env tok

let map_regex (env : env) (tok : CST.regex) =
  (* regex *) token env tok

let map_imm_tok_pat_217c202 (env : env) (tok : CST.imm_tok_pat_217c202) =
  (* pattern \d+ *) token env tok

let map_pat_1d78758 (env : env) (tok : CST.pat_1d78758) =
  (* pattern \n *) token env tok

let map_simple_heredoc_body_ (env : env) (tok : CST.simple_heredoc_body_) =
  (* simple_heredoc_body_ *) token env tok

let map_variable_name (env : env) (tok : CST.variable_name) =
  (* variable_name *) token env tok

let map_tok_prec_p1_plusplus (env : env) (tok : CST.tok_prec_p1_plusplus) =
  (* tok_prec_p1_plusplus *) token env tok

let map_anon_choice_STAR_6fc9218 (env : env) (x : CST.anon_choice_STAR_6fc9218) =
  (match x with
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

let map_imm_tok_u (env : env) (tok : CST.imm_tok_u) =
  (* "U" *) token env tok

let map_imm_tok_eq (env : env) (tok : CST.imm_tok_eq) =
  (* "=" *) token env tok

let map_external_expansion_sym_equal (env : env) (tok : CST.external_expansion_sym_equal) =
  (* external_expansion_sym_equal *) token env tok

let map_brace_start (env : env) (tok : CST.brace_start) =
  (* brace_start *) token env tok

let map_imm_tok_a (env : env) (tok : CST.imm_tok_a) =
  (* "A" *) token env tok

let map_imm_tok_l (env : env) (tok : CST.imm_tok_l) =
  (* "L" *) token env tok

let map_imm_tok_qmark (env : env) (tok : CST.imm_tok_qmark) =
  (* "?" *) token env tok

let map_imm_tok_q (env : env) (tok : CST.imm_tok_q) =
  (* "Q" *) token env tok

let map_anon_choice_PLUSEQ_110f722 (env : env) (x : CST.anon_choice_PLUSEQ_110f722) =
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
  | `SLASHEQ tok -> R.Case ("SLASHEQ",
      (* "/=" *) token env tok
    )
  | `PERCEQ tok -> R.Case ("PERCEQ",
      (* "%=" *) token env tok
    )
  | `STARSTAREQ tok -> R.Case ("STARSTAREQ",
      (* "**=" *) token env tok
    )
  | `LTLTEQ tok -> R.Case ("LTLTEQ",
      (* "<<=" *) token env tok
    )
  | `GTGTEQ tok -> R.Case ("GTGTEQ",
      (* ">>=" *) token env tok
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

let map_heredoc_end (env : env) (tok : CST.heredoc_end) =
  (* heredoc_end *) token env tok

let map_imm_tok_rcurl (env : env) (tok : CST.imm_tok_rcurl) =
  (* "}" *) token env tok

let map_special_character (env : env) (tok : CST.special_character) =
  (* special_character *) token env tok

let map_imm_tok_plus (env : env) (tok : CST.imm_tok_plus) =
  (* "+" *) token env tok

let map_extglob_pattern (env : env) (tok : CST.extglob_pattern) =
  (* extglob_pattern *) token env tok

let map_pat_b978cc7 (env : env) (tok : CST.pat_b978cc7) =
  (* pattern -?(0x)?[0-9]+# *) token env tok

let map_raw_string (env : env) (tok : CST.raw_string) =
  (* pattern "'[^']*'" *) token env tok

let map_imm_tok_dash (env : env) (tok : CST.imm_tok_dash) =
  (* "-" *) token env tok

let map_pat_421f39f (env : env) (tok : CST.pat_421f39f) =
  (* pattern -?(0x)?[0-9]+(#[0-9A-Za-z@_]+)? *) token env tok

let map_concat (env : env) (tok : CST.concat) =
  (* concat *) token env tok

let map_imm_tok_k (env : env) (tok : CST.imm_tok_k) =
  (* "K" *) token env tok

let map_immediate_double_hash (env : env) (tok : CST.immediate_double_hash) =
  (* immediate_double_hash *) token env tok

let map_imm_tok_colonqmark (env : env) (tok : CST.imm_tok_colonqmark) =
  (* ":?" *) token env tok

let map_pat_a883a20 (env : env) (tok : CST.pat_a883a20) =
  (* pattern `\s*` *) token env tok

let map_imm_tok_colondash (env : env) (tok : CST.imm_tok_colondash) =
  (* ":-" *) token env tok

let map_anon_choice_LT_995091f (env : env) (x : CST.anon_choice_LT_995091f) =
  (match x with
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
  )

let map_tok_prec_p1_plus (env : env) (tok : CST.tok_prec_p1_plus) =
  (* tok_prec_p1_plus *) token env tok

let map_pat_42e353e (env : env) (tok : CST.pat_42e353e) =
  (* pattern \w+ *) token env tok

let map_tok_prec_p1_tilde (env : env) (tok : CST.tok_prec_p1_tilde) =
  (* tok_prec_p1_tilde *) token env tok

let map_external_expansion_sym_hash (env : env) (tok : CST.external_expansion_sym_hash) =
  (* external_expansion_sym_hash *) token env tok

let map_heredoc_content (env : env) (tok : CST.heredoc_content) =
  (* heredoc_content *) token env tok

let map_imm_tok_u_ (env : env) (tok : CST.imm_tok_u_) =
  (* "u" *) token env tok

let map_tok_prec_n1_pat_ac9ca5c (env : env) (tok : CST.tok_prec_n1_pat_ac9ca5c) =
  (* tok_prec_n1_pat_ac9ca5c *) token env tok

let map_imm_tok_a_ (env : env) (tok : CST.imm_tok_a_) =
  (* "a" *) token env tok

let map_tok_prec_p1_dash (env : env) (tok : CST.tok_prec_p1_dash) =
  (* tok_prec_p1_dash *) token env tok

let map_test_operator (env : env) (tok : CST.test_operator) =
  (* test_operator *) token env tok

let map_imm_tok_at (env : env) (tok : CST.imm_tok_at) =
  (* "@" *) token env tok

let map_imm_tok_colonplus (env : env) (tok : CST.imm_tok_colonplus) =
  (* ":+" *) token env tok

let map_heredoc_start (env : env) (tok : CST.heredoc_start) =
  (* heredoc_start *) token env tok

let map_anon_choice_PLUSPLUS_e498e28 (env : env) (x : CST.anon_choice_PLUSPLUS_e498e28) =
  (match x with
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  )

let map_special_variable_name (env : env) (x : CST.special_variable_name) =
  (match x with
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `AT tok -> R.Case ("AT",
      (* "@" *) token env tok
    )
  | `QMARK tok -> R.Case ("QMARK",
      (* "?" *) token env tok
    )
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  | `HASH tok -> R.Case ("HASH",
      (* "#" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `DOLLAR tok -> R.Case ("DOLLAR",
      (* "$" *) token env tok
    )
  | `X__ tok -> R.Case ("X__",
      (* "_" *) token env tok
    )
  )

let map_file_descriptor (env : env) (tok : CST.file_descriptor) =
  (* file_descriptor *) token env tok

let map_expansion_word (env : env) (tok : CST.expansion_word) =
  (* expansion_word *) token env tok

let map_imm_tok_bang (env : env) (tok : CST.imm_tok_bang) =
  (* "!" *) token env tok

let map_anon_choice_EQEQ_8fff75a (env : env) (x : CST.anon_choice_EQEQ_8fff75a) =
  (match x with
  | `EQEQ tok -> R.Case ("EQEQ",
      (* "==" *) token env tok
    )
  | `BANGEQ tok -> R.Case ("BANGEQ",
      (* "!=" *) token env tok
    )
  )

let map_c_word (env : env) (x : CST.c_word) =
  map_pat_2b6adbc env x

let map_extended_word (env : env) (x : CST.extended_word) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  | `Word tok -> R.Case ("Word",
      (* word *) token env tok
    )
  )

let map_expansion_regex_removal (env : env) ((v1, v2) : CST.expansion_regex_removal) =
  let v1 =
    (match v1 with
    | `COMMA tok -> R.Case ("COMMA",
        (* "," *) token env tok
      )
    | `COMMACOMMA tok -> R.Case ("COMMACOMMA",
        (* ",," *) token env tok
      )
    | `HAT tok -> R.Case ("HAT",
        (* "^" *) token env tok
      )
    | `HATHAT tok -> R.Case ("HATHAT",
        (* "^^" *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* regex *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_c_terminator (env : env) (x : CST.c_terminator) =
  (match x with
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  | `Pat_1d78758 x -> R.Case ("Pat_1d78758",
      map_pat_1d78758 env x
    )
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  )

let map_terminator (env : env) (x : CST.terminator) =
  (match x with
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  | `SEMISEMI tok -> R.Case ("SEMISEMI",
      (* ";;" *) token env tok
    )
  | `Pat_1d78758 x -> R.Case ("Pat_1d78758",
      map_pat_1d78758 env x
    )
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  )

let map_anon_choice_tok_prec_p1_plusps_0918a88 (env : env) (x : CST.anon_choice_tok_prec_p1_plusps_0918a88) =
  (match x with
  | `Tok_prec_p1_plusps x -> R.Case ("Tok_prec_p1_plusps",
      map_tok_prec_p1_plusplus env x
    )
  | `Tok_prec_p1_dash x -> R.Case ("Tok_prec_p1_dash",
      map_tok_prec_p1_dashdash env x
    )
  )

let map_simple_heredoc_body (env : env) ((v1, v2) : CST.simple_heredoc_body) =
  let v1 = (* simple_heredoc_body_ *) token env v1 in
  let v2 = (* heredoc_end *) token env v2 in
  R.Tuple [v1; v2]

let map_brace_expression (env : env) ((v1, v2, v3, v4, v5) : CST.brace_expression) =
  let v1 = (* brace_start *) token env v1 in
  let v2 = map_imm_tok_pat_217c202 env v2 in
  let v3 = map_imm_tok_dotdot env v3 in
  let v4 = map_imm_tok_pat_217c202 env v4 in
  let v5 = map_imm_tok_rcurl env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_anon_choice_concat_28900c6 (env : env) (x : CST.anon_choice_concat_28900c6) =
  (match x with
  | `Concat tok -> R.Case ("Concat",
      (* concat *) token env tok
    )
  | `Pat_a883a20 x -> R.Case ("Pat_a883a20",
      map_pat_a883a20 env x
    )
  )

let map_orig_simple_variable_name (env : env) (x : CST.orig_simple_variable_name) =
  map_pat_42e353e env x

let map_anon_choice_exte_expa_sym_hash_a1d74dc (env : env) (x : CST.anon_choice_exte_expa_sym_hash_a1d74dc) =
  (match x with
  | `Exte_expa_sym_hash tok -> R.Case ("Exte_expa_sym_hash",
      (* external_expansion_sym_hash *) token env tok
    )
  | `Exte_expa_sym_bang tok -> R.Case ("Exte_expa_sym_bang",
      (* external_expansion_sym_bang *) token env tok
    )
  | `Exte_expa_sym_equal tok -> R.Case ("Exte_expa_sym_equal",
      (* external_expansion_sym_equal *) token env tok
    )
  )

let map_anon_choice_tok_prec_p1_dash_a8e1cc3 (env : env) (x : CST.anon_choice_tok_prec_p1_dash_a8e1cc3) =
  (match x with
  | `Tok_prec_p1_dash x -> R.Case ("Tok_prec_p1_dash",
      map_tok_prec_p1_dash env x
    )
  | `Tok_prec_p1_plus x -> R.Case ("Tok_prec_p1_plus",
      map_tok_prec_p1_plus env x
    )
  | `Tok_prec_p1_tilde x -> R.Case ("Tok_prec_p1_tilde",
      map_tok_prec_p1_tilde env x
    )
  )

let map_expansion_operator (env : env) ((v1, v2) : CST.expansion_operator) =
  let v1 = map_imm_tok_at env v1 in
  let v2 =
    (match v2 with
    | `Imm_tok_u x -> R.Case ("Imm_tok_u",
        map_imm_tok_u env x
      )
    | `Imm_tok_u_ x -> R.Case ("Imm_tok_u_",
        map_imm_tok_u_ env x
      )
    | `Imm_tok_l x -> R.Case ("Imm_tok_l",
        map_imm_tok_l env x
      )
    | `Imm_tok_q x -> R.Case ("Imm_tok_q",
        map_imm_tok_q env x
      )
    | `Imm_tok_e x -> R.Case ("Imm_tok_e",
        map_imm_tok_e env x
      )
    | `Imm_tok_p x -> R.Case ("Imm_tok_p",
        map_imm_tok_p env x
      )
    | `Imm_tok_a x -> R.Case ("Imm_tok_a",
        map_imm_tok_a env x
      )
    | `Imm_tok_k x -> R.Case ("Imm_tok_k",
        map_imm_tok_k env x
      )
    | `Imm_tok_a_ x -> R.Case ("Imm_tok_a_",
        map_imm_tok_a_ env x
      )
    | `Imm_tok_k_ x -> R.Case ("Imm_tok_k_",
        map_imm_tok_k_ env x
      )
    )
  in
  R.Tuple [v1; v2]

let map_simple_expansion (env : env) (x : CST.simple_expansion) =
  (match x with
  | `DOLLAR_choice_orig_simple_var_name (v1, v2) -> R.Case ("DOLLAR_choice_orig_simple_var_name",
      let v1 = (* "$" *) token env v1 in
      let v2 =
        (match v2 with
        | `Orig_simple_var_name x -> R.Case ("Orig_simple_var_name",
            map_orig_simple_variable_name env x
          )
        | `Choice_STAR x -> R.Case ("Choice_STAR",
            map_special_variable_name env x
          )
        | `BANG tok -> R.Case ("BANG",
            (* "!" *) token env tok
          )
        | `HASH tok -> R.Case ("HASH",
            (* "#" *) token env tok
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Semg_named_ellips tok -> R.Case ("Semg_named_ellips",
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok
    )
  )

let map_simple_variable_name (env : env) (x : CST.simple_variable_name) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  | `Pat_42e353e x -> R.Case ("Pat_42e353e",
      map_orig_simple_variable_name env x
    )
  )

let rec map_anon_c_exp_rep_COMMA_c_exp_f9a26b6 (env : env) ((v1, v2) : CST.anon_c_exp_rep_COMMA_c_exp_f9a26b6) =
  let v1 = map_c_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_c_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anon_choice_lit_a931c8d (env : env) (x : CST.anon_choice_lit_a931c8d) =
  (match x with
  | `Choice_conc x -> R.Case ("Choice_conc",
      map_literal env x
    )
  | `Array x -> R.Case ("Array",
      map_array_ env x
    )
  | `Empty_value tok -> R.Case ("Empty_value",
      (* empty_value *) token env tok
    )
  | `Comm_word tok -> R.Case ("Comm_word",
      (* comment_word *) token env tok
    )
  )

and map_anon_choice_lit_af2a0ae (env : env) (x : CST.anon_choice_lit_af2a0ae) =
  (match x with
  | `Choice_conc x -> R.Case ("Choice_conc",
      map_literal env x
    )
  | `Extg_blob x -> R.Case ("Extg_blob",
      map_extglob_blob env x
    )
  )

and map_anon_choice_simple_var_name_b9e31ae (env : env) (x : CST.anon_choice_simple_var_name_b9e31ae) =
  (match x with
  | `Choice_semg_meta x -> R.Case ("Choice_semg_meta",
      map_simple_variable_name env x
    )
  | `Num x -> R.Case ("Num",
      map_number env x
    )
  | `Arit_expa x -> R.Case ("Arit_expa",
      map_arithmetic_expansion env x
    )
  | `Expa x -> R.Case ("Expa",
      map_expansion env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Cmd_subs x -> R.Case ("Cmd_subs",
      map_command_substitution env x
    )
  | `Expa_max_len_bin_exp x -> R.Case ("Expa_max_len_bin_exp",
      map_expansion_max_length_binary_expression env x
    )
  | `Pat_1d78758 x -> R.Case ("Pat_1d78758",
      map_pat_1d78758 env x
    )
  )

and map_anon_choice_word_54526ae (env : env) (x : CST.anon_choice_word_54526ae) =
  (match x with
  | `Word tok -> R.Case ("Word",
      (* word *) token env tok
    )
  | `Var_name tok -> R.Case ("Var_name",
      (* variable_name *) token env tok
    )
  | `Simple_expa x -> R.Case ("Simple_expa",
      map_simple_expansion env x
    )
  | `Expa x -> R.Case ("Expa",
      map_expansion env x
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Raw_str tok -> R.Case ("Raw_str",
      (* pattern "'[^']*'" *) token env tok
    )
  | `Ansi_c_str tok -> R.Case ("Ansi_c_str",
      (* pattern "\\$'([^']|\\\\')*'" *) token env tok
    )
  | `Cmd_subs x -> R.Case ("Cmd_subs",
      map_command_substitution env x
    )
  | `Expa_word tok -> R.Case ("Expa_word",
      (* expansion_word *) token env tok
    )
  | `Array x -> R.Case ("Array",
      map_array_ env x
    )
  | `Proc_subs x -> R.Case ("Proc_subs",
      map_process_substitution env x
    )
  )

and map_arithmetic_binary_expression (env : env) (x : CST.arithmetic_binary_expression) =
  (match x with
  | `Arit_exp_choice_PLUSEQ_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_choice_PLUSEQ_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = map_anon_choice_PLUSEQ_110f722 env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arit_exp_choice_EQ_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_choice_EQ_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = map_anon_choice_EQ_2bc102b env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arit_exp_BARBAR_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_BARBAR_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arit_exp_AMPAMP_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_AMPAMP_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arit_exp_BAR_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_BAR_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arit_exp_HAT_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_HAT_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arit_exp_AMP_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_AMP_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arit_exp_choice_EQEQ_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_choice_EQEQ_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = map_anon_choice_EQEQ_8fff75a env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arit_exp_choice_LT_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_choice_LT_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = map_anon_choice_LT_995091f env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arit_exp_choice_LTLT_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_choice_LTLT_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = map_anon_choice_LTLT_5d21964 env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arit_exp_choice_PLUS_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_choice_PLUS_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = map_anon_choice_PLUS_da42005 env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arit_exp_choice_STAR_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_choice_STAR_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = map_anon_choice_STAR_6fc9218 env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arit_exp_STARSTAR_arit_exp (v1, v2, v3) -> R.Case ("Arit_exp_STARSTAR_arit_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_arithmetic_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_arithmetic_expansion (env : env) (x : CST.arithmetic_expansion) =
  (match x with
  | `DOLLARLPARLPAR_arit_exp_rep_COMMA_arit_exp_RPARRPAR (v1, v2, v3, v4) -> R.Case ("DOLLARLPARLPAR_arit_exp_rep_COMMA_arit_exp_RPARRPAR",
      let v1 = (* "$((" *) token env v1 in
      let v2 = map_arithmetic_expression env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_arithmetic_expression env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* "))" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `DOLLARLBRACK_arit_exp_RBRACK (v1, v2, v3) -> R.Case ("DOLLARLBRACK_arit_exp_RBRACK",
      let v1 = (* "$[" *) token env v1 in
      let v2 = map_arithmetic_expression env v2 in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_arithmetic_expression (env : env) (x : CST.arithmetic_expression) =
  (match x with
  | `Arit_lit x -> R.Case ("Arit_lit",
      map_arithmetic_literal env x
    )
  | `Arit_un_exp x -> R.Case ("Arit_un_exp",
      map_arithmetic_unary_expression env x
    )
  | `Arit_tern_exp (v1, v2, v3, v4, v5) -> R.Case ("Arit_tern_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_arithmetic_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_arithmetic_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Arit_bin_exp x -> R.Case ("Arit_bin_exp",
      map_arithmetic_binary_expression env x
    )
  | `Arit_post_exp (v1, v2) -> R.Case ("Arit_post_exp",
      let v1 = map_arithmetic_expression env v1 in
      let v2 = map_anon_choice_PLUSPLUS_e498e28 env v2 in
      R.Tuple [v1; v2]
    )
  | `Arit_paren_exp (v1, v2, v3) -> R.Case ("Arit_paren_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_arithmetic_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cmd_subs x -> R.Case ("Cmd_subs",
      map_command_substitution env x
    )
  )

and map_arithmetic_literal (env : env) (x : CST.arithmetic_literal) =
  (match x with
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  | `Semg_deep_exp x -> R.Case ("Semg_deep_exp",
      map_semgrep_deep_expression env x
    )
  | `Num x -> R.Case ("Num",
      map_number env x
    )
  | `Subs x -> R.Case ("Subs",
      map_subscript env x
    )
  | `Simple_expa x -> R.Case ("Simple_expa",
      map_simple_expansion env x
    )
  | `Expa x -> R.Case ("Expa",
      map_expansion env x
    )
  | `Orig_simple_var_name x -> R.Case ("Orig_simple_var_name",
      map_orig_simple_variable_name env x
    )
  | `Var_name tok -> R.Case ("Var_name",
      (* variable_name *) token env tok
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Raw_str tok -> R.Case ("Raw_str",
      (* pattern "'[^']*'" *) token env tok
    )
  )

and map_arithmetic_unary_expression (env : env) (x : CST.arithmetic_unary_expression) =
  (match x with
  | `Choice_tok_prec_p1_plusps_arit_exp (v1, v2) -> R.Case ("Choice_tok_prec_p1_plusps_arit_exp",
      let v1 =
        map_anon_choice_tok_prec_p1_plusps_0918a88 env v1
      in
      let v2 = map_arithmetic_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_tok_prec_p1_dash_arit_exp (v1, v2) -> R.Case ("Choice_tok_prec_p1_dash_arit_exp",
      let v1 = map_anon_choice_tok_prec_p1_dash_a8e1cc3 env v1 in
      let v2 = map_arithmetic_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `BANG_arit_exp (v1, v2) -> R.Case ("BANG_arit_exp",
      let v1 = (* "!" *) token env v1 in
      let v2 = map_arithmetic_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let v1 = (* "(" *) token env v1 in
  let v2 = R.List (List.map (map_literal env) v2) in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Choice_exp_choice_PLUSEQ_exp x -> R.Case ("Choice_exp_choice_PLUSEQ_exp",
      (match x with
      | `Exp_choice_PLUSEQ_exp (v1, v2, v3) -> R.Case ("Exp_choice_PLUSEQ_exp",
          let v1 = map_expression env v1 in
          let v2 = map_anon_choice_PLUSEQ_110f722 env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_choice_EQ_exp (v1, v2, v3) -> R.Case ("Exp_choice_EQ_exp",
          let v1 = map_expression env v1 in
          let v2 = map_anon_choice_EQ_2bc102b env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
          let v1 = map_expression env v1 in
          let v2 = (* "||" *) token env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
          let v1 = map_expression env v1 in
          let v2 = (* "&&" *) token env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
          let v1 = map_expression env v1 in
          let v2 = (* "|" *) token env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
          let v1 = map_expression env v1 in
          let v2 = (* "^" *) token env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
          let v1 = map_expression env v1 in
          let v2 = (* "&" *) token env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_choice_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_choice_EQEQ_exp",
          let v1 = map_expression env v1 in
          let v2 = map_anon_choice_EQEQ_8fff75a env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_choice_LT_exp (v1, v2, v3) -> R.Case ("Exp_choice_LT_exp",
          let v1 = map_expression env v1 in
          let v2 = map_anon_choice_LT_995091f env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_test_op_exp (v1, v2, v3) -> R.Case ("Exp_test_op_exp",
          let v1 = map_expression env v1 in
          let v2 = (* test_operator *) token env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_choice_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_choice_LTLT_exp",
          let v1 = map_expression env v1 in
          let v2 = map_anon_choice_LTLT_5d21964 env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_choice_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_choice_PLUS_exp",
          let v1 = map_expression env v1 in
          let v2 = map_anon_choice_PLUS_da42005 env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_choice_STAR_exp (v1, v2, v3) -> R.Case ("Exp_choice_STAR_exp",
          let v1 = map_expression env v1 in
          let v2 = map_anon_choice_STAR_6fc9218 env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Exp_STARSTAR_exp (v1, v2, v3) -> R.Case ("Exp_STARSTAR_exp",
          let v1 = map_expression env v1 in
          let v2 = (* "**" *) token env v2 in
          let v3 = map_expression env v3 in
          R.Tuple [v1; v2; v3]
        )
      )
    )
  | `Exp_EQTILDE_regex_no_sp (v1, v2, v3) -> R.Case ("Exp_EQTILDE_regex_no_sp",
      let v1 = map_expression env v1 in
      let v2 = (* "=~" *) token env v2 in
      let v3 = (* regex_no_space *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_EQEQ_extg_blob (v1, v2, v3) -> R.Case ("Exp_choice_EQEQ_extg_blob",
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_EQEQ_8fff75a env v2 in
      let v3 = map_extglob_blob env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_c_binary_expression (env : env) (x : CST.c_binary_expression) =
  (match x with
  | `C_exp_not_assign_choice_PLUSEQ_c_exp_not_assign (v1, v2, v3) -> R.Case ("C_exp_not_assign_choice_PLUSEQ_c_exp_not_assign",
      let v1 = map_c_expression_not_assignment env v1 in
      let v2 = map_anon_choice_PLUSEQ_110f722 env v2 in
      let v3 = map_c_expression_not_assignment env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `C_exp_not_assign_choice_BARBAR_c_exp_not_assign (v1, v2, v3) -> R.Case ("C_exp_not_assign_choice_BARBAR_c_exp_not_assign",
      let v1 = map_c_expression_not_assignment env v1 in
      let v2 =
        (match v2 with
        | `BARBAR tok -> R.Case ("BARBAR",
            (* "||" *) token env tok
          )
        | `DASHo tok -> R.Case ("DASHo",
            (* "-o" *) token env tok
          )
        )
      in
      let v3 = map_c_expression_not_assignment env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `C_exp_not_assign_choice_AMPAMP_c_exp_not_assign (v1, v2, v3) -> R.Case ("C_exp_not_assign_choice_AMPAMP_c_exp_not_assign",
      let v1 = map_c_expression_not_assignment env v1 in
      let v2 =
        (match v2 with
        | `AMPAMP tok -> R.Case ("AMPAMP",
            (* "&&" *) token env tok
          )
        | `DASHa tok -> R.Case ("DASHa",
            (* "-a" *) token env tok
          )
        )
      in
      let v3 = map_c_expression_not_assignment env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `C_exp_not_assign_BAR_c_exp_not_assign (v1, v2, v3) -> R.Case ("C_exp_not_assign_BAR_c_exp_not_assign",
      let v1 = map_c_expression_not_assignment env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_c_expression_not_assignment env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `C_exp_not_assign_HAT_c_exp_not_assign (v1, v2, v3) -> R.Case ("C_exp_not_assign_HAT_c_exp_not_assign",
      let v1 = map_c_expression_not_assignment env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_c_expression_not_assignment env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `C_exp_not_assign_AMP_c_exp_not_assign (v1, v2, v3) -> R.Case ("C_exp_not_assign_AMP_c_exp_not_assign",
      let v1 = map_c_expression_not_assignment env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_c_expression_not_assignment env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `C_exp_not_assign_choice_EQEQ_c_exp_not_assign (v1, v2, v3) -> R.Case ("C_exp_not_assign_choice_EQEQ_c_exp_not_assign",
      let v1 = map_c_expression_not_assignment env v1 in
      let v2 = map_anon_choice_EQEQ_8fff75a env v2 in
      let v3 = map_c_expression_not_assignment env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `C_exp_not_assign_choice_LT_c_exp_not_assign (v1, v2, v3) -> R.Case ("C_exp_not_assign_choice_LT_c_exp_not_assign",
      let v1 = map_c_expression_not_assignment env v1 in
      let v2 = map_anon_choice_LT_995091f env v2 in
      let v3 = map_c_expression_not_assignment env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `C_exp_not_assign_choice_LTLT_c_exp_not_assign (v1, v2, v3) -> R.Case ("C_exp_not_assign_choice_LTLT_c_exp_not_assign",
      let v1 = map_c_expression_not_assignment env v1 in
      let v2 = map_anon_choice_LTLT_5d21964 env v2 in
      let v3 = map_c_expression_not_assignment env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `C_exp_not_assign_choice_PLUS_c_exp_not_assign (v1, v2, v3) -> R.Case ("C_exp_not_assign_choice_PLUS_c_exp_not_assign",
      let v1 = map_c_expression_not_assignment env v1 in
      let v2 = map_anon_choice_PLUS_da42005 env v2 in
      let v3 = map_c_expression_not_assignment env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `C_exp_not_assign_choice_STAR_c_exp_not_assign (v1, v2, v3) -> R.Case ("C_exp_not_assign_choice_STAR_c_exp_not_assign",
      let v1 = map_c_expression_not_assignment env v1 in
      let v2 = map_anon_choice_STAR_6fc9218 env v2 in
      let v3 = map_c_expression_not_assignment env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `C_exp_not_assign_STARSTAR_c_exp_not_assign (v1, v2, v3) -> R.Case ("C_exp_not_assign_STARSTAR_c_exp_not_assign",
      let v1 = map_c_expression_not_assignment env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_c_expression_not_assignment env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_c_expression (env : env) (x : CST.c_expression) =
  (match x with
  | `C_exp_not_assign x -> R.Case ("C_exp_not_assign",
      map_c_expression_not_assignment env x
    )
  | `C_var_assign x -> R.Case ("C_var_assign",
      map_c_variable_assignment env x
    )
  )

and map_c_expression_not_assignment (env : env) (x : CST.c_expression_not_assignment) =
  (match x with
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  | `Choice_pat_2b6adbc x -> R.Case ("Choice_pat_2b6adbc",
      (match x with
      | `Pat_2b6adbc x -> R.Case ("Pat_2b6adbc",
          map_c_word env x
        )
      | `Simple_expa x -> R.Case ("Simple_expa",
          map_simple_expansion env x
        )
      | `Expa x -> R.Case ("Expa",
          map_expansion env x
        )
      | `Num x -> R.Case ("Num",
          map_number env x
        )
      | `Str x -> R.Case ("Str",
          map_string_ env x
        )
      | `C_un_exp x -> R.Case ("C_un_exp",
          map_c_unary_expression env x
        )
      | `C_bin_exp x -> R.Case ("C_bin_exp",
          map_c_binary_expression env x
        )
      | `C_post_exp x -> R.Case ("C_post_exp",
          map_c_postfix_expression env x
        )
      | `C_paren_exp x -> R.Case ("C_paren_exp",
          map_c_parenthesized_expression env x
        )
      | `Cmd_subs x -> R.Case ("Cmd_subs",
          map_command_substitution env x
        )
      )
    )
  )

and map_c_parenthesized_expression (env : env) ((v1, v2, v3, v4) : CST.c_parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_c_expression env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_c_expression env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_c_postfix_expression (env : env) ((v1, v2) : CST.c_postfix_expression) =
  let v1 = map_c_expression_not_assignment env v1 in
  let v2 = map_anon_choice_PLUSPLUS_e498e28 env v2 in
  R.Tuple [v1; v2]

and map_c_style_for_statement (env : env) ((v1, v2, v3, v4, v5, v6) : CST.c_style_for_statement) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* "((" *) token env v2 in
  let v3 =
    (match v3 with
    | `For_body x -> R.Case ("For_body",
        map_for_body env x
      )
    )
  in
  let v4 = (* "))" *) token env v4 in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | `Do_group x -> R.Case ("Do_group",
        map_do_group env x
      )
    | `Comp_stmt x -> R.Case ("Comp_stmt",
        map_compound_statement env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_c_unary_expression (env : env) ((v1, v2) : CST.c_unary_expression) =
  let v1 = map_anon_choice_PLUSPLUS_e498e28 env v1 in
  let v2 = map_c_expression_not_assignment env v2 in
  R.Tuple [v1; v2]

and map_c_variable_assignment (env : env) (x : CST.c_variable_assignment) =
  (match x with
  | `Semg_meta_eq_c_exp (v1, v2) -> R.Case ("Semg_meta_eq_c_exp",
      let v1 = (* pattern \$[A-Z_][A-Z_0-9]*= *) token env v1 in
      let v2 = map_c_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_2b6adbc_EQ_c_exp (v1, v2, v3) -> R.Case ("Pat_2b6adbc_EQ_c_exp",
      let v1 = map_c_word env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_c_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_case_item (env : env) ((v1, v2, v3) : CST.case_item) =
  let v1 =
    (match v1 with
    | `Opt_LPAR_choice_choice_conc_rep_BAR_choice_choice_conc_RPAR (v1, v2, v3, v4) -> R.Case ("Opt_LPAR_choice_choice_conc_rep_BAR_choice_choice_conc_RPAR",
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* "(" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 = map_anon_choice_lit_af2a0ae env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "|" *) token env v1 in
            let v2 = map_anon_choice_lit_af2a0ae env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 = (* ")" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  let v2 = map_program env v2 in
  let v3 =
    (match v3 with
    | `SEMISEMI tok -> R.Case ("SEMISEMI",
        (* ";;" *) token env tok
      )
    | `Choice_SEMIAMP x -> R.Case ("Choice_SEMIAMP",
        (match x with
        | `SEMIAMP tok -> R.Case ("SEMIAMP",
            (* ";&" *) token env tok
          )
        | `SEMISEMIAMP tok -> R.Case ("SEMISEMIAMP",
            (* ";;&" *) token env tok
          )
        )
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_case_statement (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.case_statement) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_literal env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "in" *) token env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = R.List (List.map (map_case_item env) v1) in
        let v2 = map_last_case_item env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v7 = (* "esac" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_command (env : env) ((v1, v2, v3) : CST.command) =
  let v1 =
    R.List (List.map (fun x ->
      (match x with
      | `Var_assign x -> R.Case ("Var_assign",
          map_variable_assignment env x
        )
      | `Choice_file_redi x -> R.Case ("Choice_file_redi",
          map_redirect env x
        )
      )
    ) v1)
  in
  let v2 = map_command_name env v2 in
  let v3 =
    (match v3 with
    | `Rep_choice_choice_conc xs -> R.Case ("Rep_choice_choice_conc",
        R.List (List.map (fun x ->
          (match x with
          | `Choice_conc x -> R.Case ("Choice_conc",
              map_literal env x
            )
          | `Bare_dollar tok -> R.Case ("Bare_dollar",
              (* bare_dollar *) token env tok
            )
          | `Choice_EQTILDE_choice_choice_conc (v1, v2) -> R.Case ("Choice_EQTILDE_choice_choice_conc",
              let v1 =
                (match v1 with
                | `EQTILDE tok -> R.Case ("EQTILDE",
                    (* "=~" *) token env tok
                  )
                | `EQEQ tok -> R.Case ("EQEQ",
                    (* "==" *) token env tok
                  )
                )
              in
              let v2 =
                (match v2 with
                | `Choice_conc x -> R.Case ("Choice_conc",
                    map_literal env x
                  )
                | `Regex tok -> R.Case ("Regex",
                    (* regex *) token env tok
                  )
                )
              in
              R.Tuple [v1; v2]
            )
          | `Here_redi x -> R.Case ("Here_redi",
              map_herestring_redirect env x
            )
          )
        ) xs)
      )
    | `Subs x -> R.Case ("Subs",
        map_subshell env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_command_name (env : env) (x : CST.command_name) =
  (match x with
  | `Conc x -> R.Case ("Conc",
      map_concatenation env x
    )
  | `Choice_semg_deep_exp x -> R.Case ("Choice_semg_deep_exp",
      map_primary_expression env x
    )
  | `Rep1_spec_char xs -> R.Case ("Rep1_spec_char",
      R.List (List.map (token env (* special_character *)) xs)
    )
  )

and map_command_substitution (env : env) (x : CST.command_substitution) =
  (match x with
  | `DOLLARLPAR_stmts_RPAR (v1, v2, v3) -> R.Case ("DOLLARLPAR_stmts_RPAR",
      let v1 = (* "$(" *) token env v1 in
      let v2 = map_statements env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `DOLLARLPAR_file_redi_RPAR (v1, v2, v3) -> R.Case ("DOLLARLPAR_file_redi_RPAR",
      let v1 = (* "$(" *) token env v1 in
      let v2 = map_file_redirect env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `BQUOT_stmts_BQUOT (v1, v2, v3) -> R.Case ("BQUOT_stmts_BQUOT",
      let v1 = (* "`" *) token env v1 in
      let v2 = map_statements env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `DOLLARBQUOT_stmts_BQUOT (v1, v2, v3) -> R.Case ("DOLLARBQUOT_stmts_BQUOT",
      let v1 = (* "$`" *) token env v1 in
      let v2 = map_statements env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_compound_statement (env : env) (x : CST.compound_statement) =
  (match x with
  | `LCURL_opt_choice_rep1_choice_choice_redi_stmt_choice_SEMI_tok_prec_n1_rcurl (v1, v2, v3) -> R.Case ("LCURL_opt_choice_rep1_choice_choice_redi_stmt_choice_SEMI_tok_prec_n1_rcurl",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_terminated_statement env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_tok_prec_n1_rcurl env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LPARLPAR_rep_arit_exp_COMMA_arit_exp_RPARRPAR (v1, v2, v3, v4) -> R.Case ("LPARLPAR_rep_arit_exp_COMMA_arit_exp_RPARRPAR",
      let v1 = (* "((" *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_arithmetic_expression env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 = map_arithmetic_expression env v3 in
      let v4 = (* "))" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_concatenation (env : env) ((v1, v2, v3) : CST.concatenation) =
  let v1 =
    (match v1 with
    | `Choice_semg_deep_exp x -> R.Case ("Choice_semg_deep_exp",
        map_primary_expression env x
      )
    | `Spec_char tok -> R.Case ("Spec_char",
        (* special_character *) token env tok
      )
    )
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_anon_choice_concat_28900c6 env v1 in
      let v2 =
        (match v2 with
        | `Choice_semg_deep_exp x -> R.Case ("Choice_semg_deep_exp",
            map_primary_expression env x
          )
        | `Spec_char tok -> R.Case ("Spec_char",
            (* special_character *) token env tok
          )
        | `Comm_word tok -> R.Case ("Comm_word",
            (* comment_word *) token env tok
          )
        | `Bare_dollar tok -> R.Case ("Bare_dollar",
            (* bare_dollar *) token env tok
          )
        )
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* concat *) token env v1 in
        let v2 = (* "$" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_concatenation_in_expansion (env : env) ((v1, v2) : CST.concatenation_in_expansion) =
  let v1 = map_anon_choice_word_54526ae env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_anon_choice_concat_28900c6 env v1 in
      let v2 = map_anon_choice_word_54526ae env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_declaration_command (env : env) ((v1, v2) : CST.declaration_command) =
  let v1 =
    (match v1 with
    | `Decl tok -> R.Case ("Decl",
        (* "declare" *) token env tok
      )
    | `Type tok -> R.Case ("Type",
        (* "typeset" *) token env tok
      )
    | `Export tok -> R.Case ("Export",
        (* "export" *) token env tok
      )
    | `Read tok -> R.Case ("Read",
        (* "readonly" *) token env tok
      )
    | `Local tok -> R.Case ("Local",
        (* "local" *) token env tok
      )
    )
  in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Choice_conc x -> R.Case ("Choice_conc",
          map_literal env x
        )
      | `Choice_semg_meta x -> R.Case ("Choice_semg_meta",
          map_simple_variable_name env x
        )
      | `Var_assign x -> R.Case ("Var_assign",
          map_variable_assignment env x
        )
      )
    ) v2)
  in
  R.Tuple [v1; v2]

and map_do_group (env : env) ((v1, v2, v3) : CST.do_group) =
  let v1 = (* "do" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminated_statement env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "done" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_elif_clause (env : env) ((v1, v2, v3, v4) : CST.elif_clause) =
  let v1 = (* "elif" *) token env v1 in
  let v2 = map_terminated_statement env v2 in
  let v3 = (* "then" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_terminated_statement env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminated_statement env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_expansion (env : env) ((v1, v2, v3) : CST.expansion) =
  let v1 = (* "${" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_expansion_body env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_expansion_body (env : env) (x : CST.expansion_body) =
  (match x with
  | `Rep1_choice_exte_expa_sym_hash xs -> R.Case ("Rep1_choice_exte_expa_sym_hash",
      R.List (List.map (map_anon_choice_exte_expa_sym_hash_a1d74dc env) xs)
    )
  | `Opt_imm_tok_bang_choice_var_name_choice_expa_exp (v1, v2, v3) -> R.Case ("Opt_imm_tok_bang_choice_var_name_choice_expa_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_imm_tok_bang env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Var_name tok -> R.Case ("Var_name",
            (* variable_name *) token env tok
          )
        | `Choice_semg_meta x -> R.Case ("Choice_semg_meta",
            map_simple_variable_name env x
          )
        | `Choice_STAR x -> R.Case ("Choice_STAR",
            map_special_variable_name env x
          )
        | `Subs x -> R.Case ("Subs",
            map_subscript env x
          )
        )
      in
      let v3 =
        (match v3 with
        | `Expa_exp x -> R.Case ("Expa_exp",
            map_expansion_expression env x
          )
        | `Expa_regex x -> R.Case ("Expa_regex",
            map_expansion_regex env x
          )
        | `Expa_regex_repl x -> R.Case ("Expa_regex_repl",
            map_expansion_regex_replacement env x
          )
        | `Expa_regex_remo x -> R.Case ("Expa_regex_remo",
            map_expansion_regex_removal env x
          )
        | `Expa_max_len x -> R.Case ("Expa_max_len",
            map_expansion_max_length env x
          )
        | `Expa_op x -> R.Case ("Expa_op",
            map_expansion_operator env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Imm_tok_bang_choice_choice_semg_meta_opt_choice_imm_tok_at (v1, v2, v3) -> R.Case ("Imm_tok_bang_choice_choice_semg_meta_opt_choice_imm_tok_at",
      let v1 = map_imm_tok_bang env v1 in
      let v2 =
        (match v2 with
        | `Choice_semg_meta x -> R.Case ("Choice_semg_meta",
            map_simple_variable_name env x
          )
        | `Var_name tok -> R.Case ("Var_name",
            (* variable_name *) token env tok
          )
        )
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            (match x with
            | `Imm_tok_at x -> R.Case ("Imm_tok_at",
                map_imm_tok_at env x
              )
            | `Imm_tok_star x -> R.Case ("Imm_tok_star",
                map_imm_tok_star env x
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_choice_imm_tok_hash_choice_subs_rep_choice_exte_expa_sym_hash (v1, v2, v3) -> R.Case ("Opt_choice_imm_tok_hash_choice_subs_rep_choice_exte_expa_sym_hash",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            (match x with
            | `Imm_tok_hash x -> R.Case ("Imm_tok_hash",
                map_imm_tok_hash env x
              )
            | `Imm_tok_bang x -> R.Case ("Imm_tok_bang",
                map_imm_tok_bang env x
              )
            | `Imm_tok_eq x -> R.Case ("Imm_tok_eq",
                map_imm_tok_eq env x
              )
            )
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Subs x -> R.Case ("Subs",
            map_subscript env x
          )
        | `Choice_semg_meta x -> R.Case ("Choice_semg_meta",
            map_simple_variable_name env x
          )
        | `Choice_STAR x -> R.Case ("Choice_STAR",
            map_special_variable_name env x
          )
        | `Cmd_subs x -> R.Case ("Cmd_subs",
            map_command_substitution env x
          )
        )
      in
      let v3 =
        R.List (List.map (map_anon_choice_exte_expa_sym_hash_a1d74dc env) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_expansion_expression (env : env) ((v1, v2) : CST.expansion_expression) =
  let v1 =
    (match v1 with
    | `Imm_tok_eq x -> R.Case ("Imm_tok_eq",
        map_imm_tok_eq env x
      )
    | `Imm_tok_colo_9c804d8 x -> R.Case ("Imm_tok_colo_9c804d8",
        map_imm_tok_coloneq env x
      )
    | `Imm_tok_dash x -> R.Case ("Imm_tok_dash",
        map_imm_tok_dash env x
      )
    | `Imm_tok_colo_a6ba89c x -> R.Case ("Imm_tok_colo_a6ba89c",
        map_imm_tok_colondash env x
      )
    | `Imm_tok_plus x -> R.Case ("Imm_tok_plus",
        map_imm_tok_plus env x
      )
    | `Imm_tok_colons x -> R.Case ("Imm_tok_colons",
        map_imm_tok_colonplus env x
      )
    | `Imm_tok_qmark x -> R.Case ("Imm_tok_qmark",
        map_imm_tok_qmark env x
      )
    | `Imm_tok_colo_109b622 x -> R.Case ("Imm_tok_colo_109b622",
        map_imm_tok_colonqmark env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some v1 -> R.Option (Some (
        (match v1 with
        | `Conc_in_expa x -> R.Case ("Conc_in_expa",
            map_concatenation_in_expansion env x
          )
        | `Cmd_subs x -> R.Case ("Cmd_subs",
            map_command_substitution env x
          )
        | `Word tok -> R.Case ("Word",
            (* word *) token env tok
          )
        | `Expa x -> R.Case ("Expa",
            map_expansion env x
          )
        | `Simple_expa x -> R.Case ("Simple_expa",
            map_simple_expansion env x
          )
        | `Array x -> R.Case ("Array",
            map_array_ env x
          )
        | `Str x -> R.Case ("Str",
            map_string_ env x
          )
        | `Raw_str tok -> R.Case ("Raw_str",
            (* pattern "'[^']*'" *) token env tok
          )
        | `Ansi_c_str tok -> R.Case ("Ansi_c_str",
            (* pattern "\\$'([^']|\\\\')*'" *) token env tok
          )
        | `Expa_word tok -> R.Case ("Expa_word",
            (* expansion_word *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_expansion_max_length (env : env) ((v1, v2, v3) : CST.expansion_max_length) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_simple_var_name_b9e31ae env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_simple_expansion env x
            ))
          | None -> R.Option None)
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_anon_choice_simple_var_name_b9e31ae env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_expansion_max_length_binary_expression (env : env) (x : CST.expansion_max_length_binary_expression) =
  (match x with
  | `Expa_max_len_exp_choice_PLUS_expa_max_len_exp (v1, v2, v3) -> R.Case ("Expa_max_len_exp_choice_PLUS_expa_max_len_exp",
      let v1 = map_expansion_max_length_expression env v1 in
      let v2 = map_anon_choice_PLUS_da42005 env v2 in
      let v3 = map_expansion_max_length_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expa_max_len_exp_choice_STAR_expa_max_len_exp (v1, v2, v3) -> R.Case ("Expa_max_len_exp_choice_STAR_expa_max_len_exp",
      let v1 = map_expansion_max_length_expression env v1 in
      let v2 = map_anon_choice_STAR_6fc9218 env v2 in
      let v3 = map_expansion_max_length_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_expansion_max_length_expression (env : env) (x : CST.expansion_max_length_expression) =
  (match x with
  | `Choice_semg_meta x -> R.Case ("Choice_semg_meta",
      map_simple_variable_name env x
    )
  | `Num x -> R.Case ("Num",
      map_number env x
    )
  | `Expa x -> R.Case ("Expa",
      map_expansion env x
    )
  | `Expa_max_len_bin_exp x -> R.Case ("Expa_max_len_bin_exp",
      map_expansion_max_length_binary_expression env x
    )
  )

and map_expansion_regex (env : env) ((v1, v2) : CST.expansion_regex) =
  let v1 =
    (match v1 with
    | `HASH tok -> R.Case ("HASH",
        (* "#" *) token env tok
      )
    | `Imme_double_hash tok -> R.Case ("Imme_double_hash",
        (* immediate_double_hash *) token env tok
      )
    | `PERC tok -> R.Case ("PERC",
        (* "%" *) token env tok
      )
    | `PERCPERC tok -> R.Case ("PERCPERC",
        (* "%%" *) token env tok
      )
    )
  in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Regex tok -> R.Case ("Regex",
          (* regex *) token env tok
        )
      | `RPAR tok -> R.Case ("RPAR",
          (* ")" *) token env tok
        )
      | `Str x -> R.Case ("Str",
          map_string_ env x
        )
      | `Raw_str tok -> R.Case ("Raw_str",
          (* pattern "'[^']*'" *) token env tok
        )
      | `Pat_3d340f6 x -> R.Case ("Pat_3d340f6",
          map_pat_3d340f6 env x
        )
      )
    ) v2)
  in
  R.Tuple [v1; v2]

and map_expansion_regex_replacement (env : env) ((v1, v2, v3) : CST.expansion_regex_replacement) =
  let v1 =
    (match v1 with
    | `SLASH tok -> R.Case ("SLASH",
        (* "/" *) token env tok
      )
    | `SLASHSLASH tok -> R.Case ("SLASHSLASH",
        (* "//" *) token env tok
      )
    | `SLASHHASH tok -> R.Case ("SLASHHASH",
        (* "/#" *) token env tok
      )
    | `SLASHPERC tok -> R.Case ("SLASHPERC",
        (* "/%" *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Regex_no_slash tok -> R.Case ("Regex_no_slash",
            (* regex_no_slash *) token env tok
          )
        | `Str x -> R.Case ("Str",
            map_string_ env x
          )
        | `Cmd_subs x -> R.Case ("Cmd_subs",
            map_command_substitution env x
          )
        | `Str_regex_no_slash (v1, v2) -> R.Case ("Str_regex_no_slash",
            let v1 = map_string_ env v1 in
            let v2 = (* regex_no_slash *) token env v2 in
            R.Tuple [v1; v2]
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "/" *) token env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 =
                (match v1 with
                | `Choice_semg_deep_exp x -> R.Case ("Choice_semg_deep_exp",
                    map_primary_expression env x
                  )
                | `Rep1_spec_char xs -> R.Case ("Rep1_spec_char",
                    R.List (List.map (token env (* special_character *)) xs)
                  )
                | `Cmd_subs_expa_word (v1, v2) -> R.Case ("Cmd_subs_expa_word",
                    let v1 = map_command_substitution env v1 in
                    let v2 = (* expansion_word *) token env v2 in
                    R.Tuple [v1; v2]
                  )
                | `Expa_word tok -> R.Case ("Expa_word",
                    (* expansion_word *) token env tok
                  )
                | `Conc_in_expa x -> R.Case ("Conc_in_expa",
                    map_concatenation_in_expansion env x
                  )
                | `Array x -> R.Case ("Array",
                    map_array_ env x
                  )
                )
              in
              let v2 =
                (match v2 with
                | Some tok -> R.Option (Some (
                    (* "/" *) token env tok
                  ))
                | None -> R.Option None)
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_conc x -> R.Case ("Choice_conc",
      map_literal env x
    )
  | `Un_exp x -> R.Case ("Un_exp",
      map_unary_expression env x
    )
  | `Tern_exp (v1, v2, v3, v4, v5) -> R.Case ("Tern_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Post_exp (v1, v2) -> R.Case ("Post_exp",
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_PLUSPLUS_e498e28 env v2 in
      R.Tuple [v1; v2]
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  )

and map_extglob_blob (env : env) (x : CST.extglob_blob) =
  (match x with
  | `Extg_pat tok -> R.Case ("Extg_pat",
      (* extglob_pattern *) token env tok
    )
  | `Extg_pat_choice_str_opt_extg_pat (v1, v2, v3) -> R.Case ("Extg_pat_choice_str_opt_extg_pat",
      let v1 = (* extglob_pattern *) token env v1 in
      let v2 =
        (match v2 with
        | `Str x -> R.Case ("Str",
            map_string_ env x
          )
        | `Expa x -> R.Case ("Expa",
            map_expansion env x
          )
        | `Cmd_subs x -> R.Case ("Cmd_subs",
            map_command_substitution env x
          )
        )
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* extglob_pattern *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_file_redirect (env : env) ((v1, v2) : CST.file_redirect) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* file_descriptor *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Choice_LT_rep1_choice_conc (v1, v2) -> R.Case ("Choice_LT_rep1_choice_conc",
        let v1 =
          (match v1 with
          | `LT tok -> R.Case ("LT",
              (* "<" *) token env tok
            )
          | `GT tok -> R.Case ("GT",
              (* ">" *) token env tok
            )
          | `GTGT tok -> R.Case ("GTGT",
              (* ">>" *) token env tok
            )
          | `AMPGT tok -> R.Case ("AMPGT",
              (* "&>" *) token env tok
            )
          | `AMPGTGT tok -> R.Case ("AMPGTGT",
              (* "&>>" *) token env tok
            )
          | `LTAMP tok -> R.Case ("LTAMP",
              (* "<&" *) token env tok
            )
          | `GTAMP tok -> R.Case ("GTAMP",
              (* ">&" *) token env tok
            )
          | `GTBAR tok -> R.Case ("GTBAR",
              (* ">|" *) token env tok
            )
          )
        in
        let v2 = map_heredoc_command env v2 in
        R.Tuple [v1; v2]
      )
    | `Choice_LTAMPDASH_opt_choice_conc (v1, v2) -> R.Case ("Choice_LTAMPDASH_opt_choice_conc",
        let v1 =
          (match v1 with
          | `LTAMPDASH tok -> R.Case ("LTAMPDASH",
              (* "<&-" *) token env tok
            )
          | `GTAMPDASH tok -> R.Case ("GTAMPDASH",
              (* ">&-" *) token env tok
            )
          )
        in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_literal env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2]

and map_for_body (env : env) ((v1, v2, v3, v4, v5) : CST.for_body) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_anon_c_exp_rep_COMMA_c_exp_f9a26b6 env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_c_terminator env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_c_exp_rep_COMMA_c_exp_f9a26b6 env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_c_terminator env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_c_exp_rep_COMMA_c_exp_f9a26b6 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_for_statement (env : env) ((v1, v2, v3, v4, v5) : CST.for_statement) =
  let v1 =
    (match v1 with
    | `For tok -> R.Case ("For",
        (* "for" *) token env tok
      )
    | `Select tok -> R.Case ("Select",
        (* "select" *) token env tok
      )
    )
  in
  let v2 = map_simple_variable_name env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "in" *) token env v1 in
        let v2 = map_heredoc_command env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = map_terminator env v4 in
  let v5 = map_do_group env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_function_definition (env : env) ((v1, v2, v3) : CST.function_definition) =
  let v1 =
    (match v1 with
    | `Func_exte_word_opt_LPAR_RPAR (v1, v2, v3) -> R.Case ("Func_exte_word_opt_LPAR_RPAR",
        let v1 = (* "function" *) token env v1 in
        let v2 = map_extended_word env v2 in
        let v3 =
          (match v3 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* "(" *) token env v1 in
              let v2 = (* ")" *) token env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Choice_word_LPAR_RPAR (v1, v2, v3) -> R.Case ("Choice_word_LPAR_RPAR",
        let v1 =
          (match v1 with
          | `Word tok -> R.Case ("Word",
              (* word *) token env tok
            )
          | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
              (* "..." *) token env tok
            )
          )
        in
        let v2 = (* "(" *) token env v2 in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v2 =
    (match v2 with
    | `Comp_stmt x -> R.Case ("Comp_stmt",
        map_compound_statement env x
      )
    | `Subs x -> R.Case ("Subs",
        map_subshell env x
      )
    | `Test_cmd x -> R.Case ("Test_cmd",
        map_test_command env x
      )
    | `If_stmt x -> R.Case ("If_stmt",
        map_if_statement env x
      )
    )
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_redirect env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_heredoc_body (env : env) ((v1, v2) : CST.heredoc_body) =
  let v1 = map_heredoc_body_ env v1 in
  let v2 = (* heredoc_end *) token env v2 in
  R.Tuple [v1; v2]

and map_heredoc_body_ (env : env) ((v1, v2) : CST.heredoc_body_) =
  let v1 = (* heredoc_body_beginning *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Expa x -> R.Case ("Expa",
          map_expansion env x
        )
      | `Simple_expa x -> R.Case ("Simple_expa",
          map_simple_expansion env x
        )
      | `Cmd_subs x -> R.Case ("Cmd_subs",
          map_command_substitution env x
        )
      | `Here_content tok -> R.Case ("Here_content",
          (* heredoc_content *) token env tok
        )
      )
    ) v2)
  in
  R.Tuple [v1; v2]

and map_heredoc_command (env : env) (xs : CST.heredoc_command) =
  R.List (List.map (map_literal env) xs)

and map_heredoc_expression (env : env) ((v1, v2) : CST.heredoc_expression) =
  let v1 =
    (match v1 with
    | `BARBAR tok -> R.Case ("BARBAR",
        (* "||" *) token env tok
      )
    | `AMPAMP tok -> R.Case ("AMPAMP",
        (* "&&" *) token env tok
      )
    )
  in
  let v2 = map_statement env v2 in
  R.Tuple [v1; v2]

and map_heredoc_pipeline (env : env) ((v1, v2) : CST.heredoc_pipeline) =
  let v1 = map_anon_choice_BAR_9062646 env v1 in
  let v2 = map_statement env v2 in
  R.Tuple [v1; v2]

and map_heredoc_redirect (env : env) ((v1, v2, v3, v4, v5, v6) : CST.heredoc_redirect) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* file_descriptor *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `LTLT tok -> R.Case ("LTLT",
        (* "<<" *) token env tok
      )
    | `LTLTDASH tok -> R.Case ("LTLTDASH",
        (* "<<-" *) token env tok
      )
    )
  in
  let v3 = (* heredoc_start *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        (match x with
        | `Here_pipe x -> R.Case ("Here_pipe",
            map_heredoc_pipeline env x
          )
        | `Rep1_choice_file_redi_opt_here_exp (v1, v2) -> R.Case ("Rep1_choice_file_redi_opt_here_exp",
            let v1 = R.List (List.map (map_redirect env) v1) in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_heredoc_expression env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Here_exp x -> R.Case ("Here_exp",
            map_heredoc_expression env x
          )
        | `Here_cmd x -> R.Case ("Here_cmd",
            map_heredoc_command env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v5 = map_pat_1d78758 env v5 in
  let v6 =
    (match v6 with
    | `Here_body x -> R.Case ("Here_body",
        map_heredoc_body env x
      )
    | `Simple_here_body x -> R.Case ("Simple_here_body",
        map_simple_heredoc_body env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_herestring_redirect (env : env) ((v1, v2, v3) : CST.herestring_redirect) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* file_descriptor *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "<<<" *) token env v2 in
  let v3 = map_literal env v3 in
  R.Tuple [v1; v2; v3]

and map_if_statement (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.if_statement) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_terminated_statement env v2 in
  let v3 = (* "then" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_terminated_statement env x
      ))
    | None -> R.Option None)
  in
  let v5 = R.List (List.map (map_elif_clause env) v5) in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_else_clause env x
      ))
    | None -> R.Option None)
  in
  let v7 = (* "fi" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_last_case_item (env : env) ((v1, v2, v3, v4, v5, v6) : CST.last_case_item) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "(" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_anon_choice_lit_af2a0ae env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_anon_choice_lit_af2a0ae env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_program env v5 in
  let v6 =
    (match v6 with
    | Some tok -> R.Option (Some (
        (* ";;" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_list_ (env : env) ((v1, v2, v3) : CST.list_) =
  let v1 = map_statement env v1 in
  let v2 =
    (match v2 with
    | `AMPAMP tok -> R.Case ("AMPAMP",
        (* "&&" *) token env tok
      )
    | `BARBAR tok -> R.Case ("BARBAR",
        (* "||" *) token env tok
      )
    )
  in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Conc x -> R.Case ("Conc",
      map_concatenation env x
    )
  | `Choice_semg_deep_exp x -> R.Case ("Choice_semg_deep_exp",
      map_primary_expression env x
    )
  | `Rep1_spec_char xs -> R.Case ("Rep1_spec_char",
      R.List (List.map (token env (* special_character *)) xs)
    )
  )

and map_negated_command (env : env) ((v1, v2) : CST.negated_command) =
  let v1 = (* "!" *) token env v1 in
  let v2 =
    (match v2 with
    | `Cmd x -> R.Case ("Cmd",
        map_command env x
      )
    | `Var_assign x -> R.Case ("Var_assign",
        map_variable_assignment env x
      )
    | `Test_cmd x -> R.Case ("Test_cmd",
        map_test_command env x
      )
    | `Subs x -> R.Case ("Subs",
        map_subshell env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_number (env : env) (x : CST.number) =
  (match x with
  | `Pat_421f39f x -> R.Case ("Pat_421f39f",
      map_pat_421f39f env x
    )
  | `Pat_b978cc7_choice_expa (v1, v2) -> R.Case ("Pat_b978cc7_choice_expa",
      let v1 = map_pat_b978cc7 env v1 in
      let v2 =
        (match v2 with
        | `Expa x -> R.Case ("Expa",
            map_expansion env x
          )
        | `Cmd_subs x -> R.Case ("Cmd_subs",
            map_command_substitution env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Semg_deep_exp x -> R.Case ("Semg_deep_exp",
      map_semgrep_deep_expression env x
    )
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  | `Choice_word x -> R.Case ("Choice_word",
      (match x with
      | `Word tok -> R.Case ("Word",
          (* word *) token env tok
        )
      | `Test_op tok -> R.Case ("Test_op",
          (* test_operator *) token env tok
        )
      | `Str x -> R.Case ("Str",
          map_string_ env x
        )
      | `Raw_str tok -> R.Case ("Raw_str",
          (* pattern "'[^']*'" *) token env tok
        )
      | `Tran_str x -> R.Case ("Tran_str",
          map_translated_string env x
        )
      | `Ansi_c_str tok -> R.Case ("Ansi_c_str",
          (* pattern "\\$'([^']|\\\\')*'" *) token env tok
        )
      | `Num x -> R.Case ("Num",
          map_number env x
        )
      | `Expa x -> R.Case ("Expa",
          map_expansion env x
        )
      | `Simple_expa x -> R.Case ("Simple_expa",
          map_simple_expansion env x
        )
      | `Cmd_subs x -> R.Case ("Cmd_subs",
          map_command_substitution env x
        )
      | `Proc_subs x -> R.Case ("Proc_subs",
          map_process_substitution env x
        )
      | `Arit_expa x -> R.Case ("Arit_expa",
          map_arithmetic_expansion env x
        )
      | `Brace_exp x -> R.Case ("Brace_exp",
          map_brace_expression env x
        )
      )
    )
  )

and map_process_substitution (env : env) ((v1, v2, v3) : CST.process_substitution) =
  let v1 =
    (match v1 with
    | `LTLPAR tok -> R.Case ("LTLPAR",
        (* "<(" *) token env tok
      )
    | `GTLPAR tok -> R.Case ("GTLPAR",
        (* ">(" *) token env tok
      )
    )
  in
  let v2 = map_statements env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_program (env : env) (opt : CST.program) =
  (match opt with
  | Some x -> R.Option (Some (
      map_statements env x
    ))
  | None -> R.Option None)

and map_redirect (env : env) (x : CST.redirect) =
  (match x with
  | `File_redi x -> R.Case ("File_redi",
      map_file_redirect env x
    )
  | `Here_redi x -> R.Case ("Here_redi",
      map_herestring_redirect env x
    )
  )

and map_redirected_statement (env : env) (x : CST.redirected_statement) =
  (match x with
  | `Choice_redi_stmt_rep1_choice_file_redi (v1, v2) -> R.Case ("Choice_redi_stmt_rep1_choice_file_redi",
      let v1 = map_statement_not_pipeline env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `File_redi x -> R.Case ("File_redi",
              map_file_redirect env x
            )
          | `Here_redi x -> R.Case ("Here_redi",
              map_heredoc_redirect env x
            )
          )
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Choice_if_stmt_here_redi (v1, v2) -> R.Case ("Choice_if_stmt_here_redi",
      let v1 =
        (match v1 with
        | `If_stmt x -> R.Case ("If_stmt",
            map_if_statement env x
          )
        | `While_stmt x -> R.Case ("While_stmt",
            map_while_statement env x
          )
        )
      in
      let v2 = map_herestring_redirect env v2 in
      R.Tuple [v1; v2]
    )
  | `Rep1_choice_file_redi xs -> R.Case ("Rep1_choice_file_redi",
      R.List (List.map (map_redirect env) xs)
    )
  | `Here_redi x -> R.Case ("Here_redi",
      map_herestring_redirect env x
    )
  )

and map_semgrep_deep_expression (env : env) ((v1, v2, v3) : CST.semgrep_deep_expression) =
  let v1 = (* "<..." *) token env v1 in
  let v2 = map_literal env v2 in
  let v3 = (* "...>" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Choice_redi_stmt x -> R.Case ("Choice_redi_stmt",
      map_statement_not_subshell env x
    )
  | `Subs x -> R.Case ("Subs",
      map_subshell env x
    )
  )

and map_statement_not_pipeline (env : env) (x : CST.statement_not_pipeline) =
  (match x with
  | `Redi_stmt x -> R.Case ("Redi_stmt",
      map_redirected_statement env x
    )
  | `Var_assign x -> R.Case ("Var_assign",
      map_variable_assignment env x
    )
  | `Var_assigns x -> R.Case ("Var_assigns",
      map_variable_assignments env x
    )
  | `Cmd x -> R.Case ("Cmd",
      map_command env x
    )
  | `Decl_cmd x -> R.Case ("Decl_cmd",
      map_declaration_command env x
    )
  | `Unset_cmd x -> R.Case ("Unset_cmd",
      map_unset_command env x
    )
  | `Test_cmd x -> R.Case ("Test_cmd",
      map_test_command env x
    )
  | `Nega_cmd x -> R.Case ("Nega_cmd",
      map_negated_command env x
    )
  | `For_stmt x -> R.Case ("For_stmt",
      map_for_statement env x
    )
  | `C_style_for_stmt x -> R.Case ("C_style_for_stmt",
      map_c_style_for_statement env x
    )
  | `While_stmt x -> R.Case ("While_stmt",
      map_while_statement env x
    )
  | `If_stmt x -> R.Case ("If_stmt",
      map_if_statement env x
    )
  | `Case_stmt x -> R.Case ("Case_stmt",
      map_case_statement env x
    )
  | `List x -> R.Case ("List",
      map_list_ env x
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  | `Func_defi x -> R.Case ("Func_defi",
      map_function_definition env x
    )
  | `Subs x -> R.Case ("Subs",
      map_subshell env x
    )
  )

and map_statement_not_subshell (env : env) (x : CST.statement_not_subshell) =
  (match x with
  | `Redi_stmt x -> R.Case ("Redi_stmt",
      map_redirected_statement env x
    )
  | `Var_assign x -> R.Case ("Var_assign",
      map_variable_assignment env x
    )
  | `Var_assigns x -> R.Case ("Var_assigns",
      map_variable_assignments env x
    )
  | `Cmd x -> R.Case ("Cmd",
      map_command env x
    )
  | `Decl_cmd x -> R.Case ("Decl_cmd",
      map_declaration_command env x
    )
  | `Unset_cmd x -> R.Case ("Unset_cmd",
      map_unset_command env x
    )
  | `Test_cmd x -> R.Case ("Test_cmd",
      map_test_command env x
    )
  | `Nega_cmd x -> R.Case ("Nega_cmd",
      map_negated_command env x
    )
  | `For_stmt x -> R.Case ("For_stmt",
      map_for_statement env x
    )
  | `C_style_for_stmt x -> R.Case ("C_style_for_stmt",
      map_c_style_for_statement env x
    )
  | `While_stmt x -> R.Case ("While_stmt",
      map_while_statement env x
    )
  | `If_stmt x -> R.Case ("If_stmt",
      map_if_statement env x
    )
  | `Case_stmt x -> R.Case ("Case_stmt",
      map_case_statement env x
    )
  | `Pipe (v1, v2) -> R.Case ("Pipe",
      let v1 = map_statement_not_pipeline env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_anon_choice_BAR_9062646 env v1 in
          let v2 = map_statement_not_pipeline env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `List x -> R.Case ("List",
      map_list_ env x
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  | `Func_defi x -> R.Case ("Func_defi",
      map_function_definition env x
    )
  )

and map_statements (env : env) ((v1, v2, v3) : CST.statements) =
  let v1 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_statement env v1 in
      let v2 = map_terminator env v2 in
      R.Tuple [v1; v2]
    ) v1)
  in
  let v2 = map_statement env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_string_ (env : env) ((v1, v2, v3, v4) : CST.string_) =
  let v1 = (* "\"" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `Opt_DOLLAR_str_content (v1, v2) -> R.Case ("Opt_DOLLAR_str_content",
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* "$" *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 = (* string_content *) token env v2 in
            R.Tuple [v1; v2]
          )
        | `Expa x -> R.Case ("Expa",
            map_expansion env x
          )
        | `Simple_expa x -> R.Case ("Simple_expa",
            map_simple_expansion env x
          )
        | `Cmd_subs x -> R.Case ("Cmd_subs",
            map_command_substitution env x
          )
        | `Arit_expa x -> R.Case ("Arit_expa",
            map_arithmetic_expansion env x
          )
        )
      in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* concat *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "$" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "\"" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_subscript (env : env) ((v1, v2, v3, v4, v5, v6) : CST.subscript) =
  let v1 = (* variable_name *) token env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 =
    (match v3 with
    | `Choice_conc x -> R.Case ("Choice_conc",
        map_literal env x
      )
    | `Bin_exp x -> R.Case ("Bin_exp",
        map_binary_expression env x
      )
    | `Un_exp x -> R.Case ("Un_exp",
        map_unary_expression env x
      )
    | `Comp_stmt x -> R.Case ("Comp_stmt",
        map_compound_statement env x
      )
    | `Subs x -> R.Case ("Subs",
        map_subshell env x
      )
    )
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* concat *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  let v6 =
    (match v6 with
    | Some tok -> R.Option (Some (
        (* concat *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_subshell (env : env) ((v1, v2, v3) : CST.subshell) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_statements env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_terminated_statement (env : env) (x : CST.terminated_statement) =
  (match x with
  | `Rep1_choice_choice_redi_stmt_choice_SEMI xs -> R.Case ("Rep1_choice_choice_redi_stmt_choice_SEMI",
      R.List (List.map (fun (v1, v2) ->
        let v1 = map_statement env v1 in
        let v2 = map_terminator env v2 in
        R.Tuple [v1; v2]
      ) xs)
    )
  | `Opt_rep1_choice_choice_redi_stmt_choice_SEMI_ellips_cmd (v1, v2) -> R.Case ("Opt_rep1_choice_choice_redi_stmt_choice_SEMI_ellips_cmd",
      let v1 =
        (match v1 with
        | Some xs -> R.Option (Some (
            R.List (List.map (fun (v1, v2) ->
              let v1 = map_statement env v1 in
              let v2 = map_terminator env v2 in
              R.Tuple [v1; v2]
            ) xs)
          ))
        | None -> R.Option None)
      in
      let v2 = (* "..." *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_test_command (env : env) (v1 : CST.test_command) =
  (match v1 with
  | `LBRACK_opt_choice_exp_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_opt_choice_exp_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Exp x -> R.Case ("Exp",
                map_expression env x
              )
            | `Redi_stmt x -> R.Case ("Redi_stmt",
                map_redirected_statement env x
              )
            )
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LBRACKLBRACK_choice_exp_RBRACKRBRACK (v1, v2, v3) -> R.Case ("LBRACKLBRACK_choice_exp_RBRACKRBRACK",
      let v1 = (* "[[" *) token env v1 in
      let v2 =
        (match v2 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Test_cmd_bin_exp x -> R.Case ("Test_cmd_bin_exp",
            map_test_command_binary_expression env x
          )
        )
      in
      let v3 = (* "]]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_test_command_binary_expression (env : env) ((v1, v2, v3) : CST.test_command_binary_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = (* regex_no_space *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_translated_string (env : env) ((v1, v2) : CST.translated_string) =
  let v1 = (* "$" *) token env v1 in
  let v2 = map_string_ env v2 in
  R.Tuple [v1; v2]

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `Choice_tok_prec_p1_plusps_exp (v1, v2) -> R.Case ("Choice_tok_prec_p1_plusps_exp",
      let v1 =
        map_anon_choice_tok_prec_p1_plusps_0918a88 env v1
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_tok_prec_p1_dash_exp (v1, v2) -> R.Case ("Choice_tok_prec_p1_dash_exp",
      let v1 = map_anon_choice_tok_prec_p1_dash_a8e1cc3 env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `BANG_exp (v1, v2) -> R.Case ("BANG_exp",
      let v1 = (* "!" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Test_op_exp (v1, v2) -> R.Case ("Test_op_exp",
      let v1 = (* test_operator *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_unset_command (env : env) ((v1, v2) : CST.unset_command) =
  let v1 =
    (match v1 with
    | `Unset tok -> R.Case ("Unset",
        (* "unset" *) token env tok
      )
    | `Unse tok -> R.Case ("Unse",
        (* "unsetenv" *) token env tok
      )
    )
  in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Choice_conc x -> R.Case ("Choice_conc",
          map_literal env x
        )
      | `Choice_semg_meta x -> R.Case ("Choice_semg_meta",
          map_simple_variable_name env x
        )
      )
    ) v2)
  in
  R.Tuple [v1; v2]

and map_variable_assignment (env : env) (x : CST.variable_assignment) =
  (match x with
  | `Choice_semg_meta_eq_choice_choice_conc (v1, v2) -> R.Case ("Choice_semg_meta_eq_choice_choice_conc",
      let v1 =
        (match v1 with
        | `Semg_meta_eq tok -> R.Case ("Semg_meta_eq",
            (* pattern \$[A-Z_][A-Z_0-9]*= *) token env tok
          )
        | `Semg_meta_pluseq tok -> R.Case ("Semg_meta_pluseq",
            (* pattern \$[A-Z_][A-Z_0-9]*\+= *) token env tok
          )
        )
      in
      let v2 = map_anon_choice_lit_a931c8d env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_var_name_choice_EQ_choice_choice_conc (v1, v2, v3) -> R.Case ("Choice_var_name_choice_EQ_choice_choice_conc",
      let v1 =
        (match v1 with
        | `Var_name tok -> R.Case ("Var_name",
            (* variable_name *) token env tok
          )
        | `Subs x -> R.Case ("Subs",
            map_subscript env x
          )
        )
      in
      let v2 =
        (match v2 with
        | `EQ tok -> R.Case ("EQ",
            (* "=" *) token env tok
          )
        | `PLUSEQ tok -> R.Case ("PLUSEQ",
            (* "+=" *) token env tok
          )
        )
      in
      let v3 = map_anon_choice_lit_a931c8d env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_variable_assignments (env : env) ((v1, v2) : CST.variable_assignments) =
  let v1 = map_variable_assignment env v1 in
  let v2 =
    R.List (List.map (map_variable_assignment env) v2)
  in
  R.Tuple [v1; v2]

and map_while_statement (env : env) ((v1, v2, v3) : CST.while_statement) =
  let v1 =
    (match v1 with
    | `While tok -> R.Case ("While",
        (* "while" *) token env tok
      )
    | `Until tok -> R.Case ("Until",
        (* "until" *) token env tok
      )
    )
  in
  let v2 = map_terminated_statement env v2 in
  let v3 = map_do_group env v3 in
  R.Tuple [v1; v2; v3]

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

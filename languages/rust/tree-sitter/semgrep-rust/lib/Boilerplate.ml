(**
   Boilerplate to be used as a template when mapping the rust CST
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

let map_float_literal (env : env) (tok : CST.float_literal) =
  (* float_literal *) token env tok

let map_imm_tok_prec_p1_pat_4fd4a56 (env : env) (tok : CST.imm_tok_prec_p1_pat_4fd4a56) =
  (* pattern .* *) token env tok

let map_imm_tok_prec_p2_pat_262ec39 (env : env) (tok : CST.imm_tok_prec_p2_pat_262ec39) =
  (* pattern \/\/ *) token env tok

let map_char_literal (env : env) (tok : CST.char_literal) =
  (* char_literal *) token env tok

let map_block_comment_content (env : env) (tok : CST.block_comment_content) =
  (* block_comment_content *) token env tok

let map_metavariable (env : env) (tok : CST.metavariable) =
  (* pattern \$[a-zA-Z_]\w* *) token env tok

let map_pat_4fd4a56 (env : env) (tok : CST.pat_4fd4a56) =
  (* pattern .* *) token env tok

let map_anon_choice_PLUS_348fa54 (env : env) (x : CST.anon_choice_PLUS_348fa54) =
  (match x with
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `QMARK tok -> R.Case ("QMARK",
      (* "?" *) token env tok
    )
  )

let map_raw_string_literal_content (env : env) (tok : CST.raw_string_literal_content) =
  (* raw_string_literal_content *) token env tok

let map_string_content (env : env) (tok : CST.string_content) =
  (* string_content *) token env tok

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_block_comment_explicit (env : env) (() : CST.block_comment_explicit) =
  R.Tuple []

let map_outer_block_doc_comment_marker (env : env) (tok : CST.outer_block_doc_comment_marker) =
  (* outer_block_doc_comment_marker *) token env tok

let map_inner_block_doc_comment_marker (env : env) (tok : CST.inner_block_doc_comment_marker) =
  (* inner_block_doc_comment_marker *) token env tok

let map_line_doc_content (env : env) (tok : CST.line_doc_content) =
  (* line_doc_content *) token env tok

let map_pat_a3a04ed (env : env) (tok : CST.pat_a3a04ed) =
  (* pattern "[bc]\"" *) token env tok

let map_raw_string_literal_start (env : env) (tok : CST.raw_string_literal_start) =
  (* raw_string_literal_start *) token env tok

let map_anon_choice_u8_6dad923 (env : env) (x : CST.anon_choice_u8_6dad923) =
  (match x with
  | `U8 tok -> R.Case ("U8",
      (* "u8" *) token env tok
    )
  | `I8 tok -> R.Case ("I8",
      (* "i8" *) token env tok
    )
  | `U16 tok -> R.Case ("U16",
      (* "u16" *) token env tok
    )
  | `I16 tok -> R.Case ("I16",
      (* "i16" *) token env tok
    )
  | `U32 tok -> R.Case ("U32",
      (* "u32" *) token env tok
    )
  | `I32 tok -> R.Case ("I32",
      (* "i32" *) token env tok
    )
  | `U64 tok -> R.Case ("U64",
      (* "u64" *) token env tok
    )
  | `I64 tok -> R.Case ("I64",
      (* "i64" *) token env tok
    )
  | `U128 tok -> R.Case ("U128",
      (* "u128" *) token env tok
    )
  | `I128 tok -> R.Case ("I128",
      (* "i128" *) token env tok
    )
  | `Isize tok -> R.Case ("Isize",
      (* "isize" *) token env tok
    )
  | `Usize tok -> R.Case ("Usize",
      (* "usize" *) token env tok
    )
  | `F32 tok -> R.Case ("F32",
      (* "f32" *) token env tok
    )
  | `F64 tok -> R.Case ("F64",
      (* "f64" *) token env tok
    )
  | `Bool tok -> R.Case ("Bool",
      (* "bool" *) token env tok
    )
  | `Str tok -> R.Case ("Str",
      (* "str" *) token env tok
    )
  | `Char tok -> R.Case ("Char",
      (* "char" *) token env tok
    )
  )

let map_fragment_specifier (env : env) (x : CST.fragment_specifier) =
  (match x with
  | `Blk tok -> R.Case ("Blk",
      (* "block" *) token env tok
    )
  | `Expr tok -> R.Case ("Expr",
      (* "expr" *) token env tok
    )
  | `Expr_2021 tok -> R.Case ("Expr_2021",
      (* "expr_2021" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* "ident" *) token env tok
    )
  | `Item tok -> R.Case ("Item",
      (* "item" *) token env tok
    )
  | `Life tok -> R.Case ("Life",
      (* "lifetime" *) token env tok
    )
  | `Lit tok -> R.Case ("Lit",
      (* "literal" *) token env tok
    )
  | `Meta tok -> R.Case ("Meta",
      (* "meta" *) token env tok
    )
  | `Pat tok -> R.Case ("Pat",
      (* "pat" *) token env tok
    )
  | `Pat_param tok -> R.Case ("Pat_param",
      (* "pat_param" *) token env tok
    )
  | `Path tok -> R.Case ("Path",
      (* "path" *) token env tok
    )
  | `Stmt tok -> R.Case ("Stmt",
      (* "stmt" *) token env tok
    )
  | `Tt tok -> R.Case ("Tt",
      (* "tt" *) token env tok
    )
  | `Ty tok -> R.Case ("Ty",
      (* "ty" *) token env tok
    )
  | `Vis tok -> R.Case ("Vis",
      (* "vis" *) token env tok
    )
  )

let map_line_comment_explicit (env : env) (() : CST.line_comment_explicit) =
  R.Tuple []

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  (match x with
  | `Defa tok -> R.Case ("Defa",
      (* "default" *) token env tok
    )
  | `Union tok -> R.Case ("Union",
      (* "union" *) token env tok
    )
  | `Gen tok -> R.Case ("Gen",
      (* "gen" *) token env tok
    )
  | `Raw tok -> R.Case ("Raw",
      (* "raw" *) token env tok
    )
  )

let map_raw_string_literal_end (env : env) (tok : CST.raw_string_literal_end) =
  (* raw_string_literal_end *) token env tok

let map_integer_literal (env : env) (tok : CST.integer_literal) =
  (* integer_literal *) token env tok

let map_tok_prec_p1_lt (env : env) (tok : CST.tok_prec_p1_lt) =
  (* tok_prec_p1_lt *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_pat_1e84e62 (env : env) (tok : CST.pat_1e84e62) =
  (* pattern [^+*?]+ *) token env tok

let map_shebang (env : env) (tok : CST.shebang) =
  (* pattern #![\r\f\t\v ]*([^\[\n].*\
  )?\n *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_block_doc_comment_marker (env : env) (x : CST.block_doc_comment_marker) =
  (match x with
  | `Outer_blk_doc_comm_marker tok -> R.Case ("Outer_blk_doc_comm_marker",
      (* outer_block_doc_comment_marker *) token env tok
    )
  | `Inner_blk_doc_comm_marker tok -> R.Case ("Inner_blk_doc_comm_marker",
      (* inner_block_doc_comment_marker *) token env tok
    )
  )

let map_raw_string_literal (env : env) ((v1, v2, v3) : CST.raw_string_literal) =
  let v1 = (* raw_string_literal_start *) token env v1 in
  let v2 = (* raw_string_literal_content *) token env v2 in
  let v3 = (* raw_string_literal_end *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_negative_literal (env : env) ((v1, v2) : CST.negative_literal) =
  let v1 = (* "-" *) token env v1 in
  let v2 =
    (match v2 with
    | `Int_lit tok -> R.Case ("Int_lit",
        (* integer_literal *) token env tok
      )
    | `Float_lit tok -> R.Case ("Float_lit",
        (* float_literal *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

let map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) =
  let v1 =
    (match v1 with
    | `DQUOT tok -> R.Case ("DQUOT",
        (* "\"" *) token env tok
      )
    | `Pat_a3a04ed x -> R.Case ("Pat_a3a04ed",
        map_pat_a3a04ed env x
      )
    )
  in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      | `Str_content tok -> R.Case ("Str_content",
          (* string_content *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "\"" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_line_doc_comment_marker (env : env) (x : CST.line_doc_comment_marker) =
  (match x with
  | `Outer_line_doc_comm_marker tok -> R.Case ("Outer_line_doc_comm_marker",
      (* "/" *) token env tok
    )
  | `Inner_line_doc_comm_marker tok -> R.Case ("Inner_line_doc_comm_marker",
      (* "!" *) token env tok
    )
  )

let map_anon_choice_const_f077f79 (env : env) (x : CST.anon_choice_const_f077f79) =
  (match x with
  | `Const tok -> R.Case ("Const",
      (* "const" *) token env tok
    )
  | `Muta_spec tok -> R.Case ("Muta_spec",
      (* "mut" *) token env tok
    )
  )

let map_block_comment (env : env) ((v1, v2, v3) : CST.block_comment) =
  let v1 = (* "/*" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Blk_doc_comm_marker_opt_blk_comm_content (v1, v2) -> R.Case ("Blk_doc_comm_marker_opt_blk_comm_content",
            let v1 = map_block_doc_comment_marker env v1 in
            let v2 =
              (match v2 with
              | Some tok -> R.Option (Some (
                  (* block_comment_content *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Blk_comm_content tok -> R.Case ("Blk_comm_content",
            (* block_comment_content *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = (* "*/" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_extern_modifier (env : env) ((v1, v2) : CST.extern_modifier) =
  let v1 = (* "extern" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_string_literal env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Raw_str_lit x -> R.Case ("Raw_str_lit",
      map_raw_string_literal env x
    )
  | `Char_lit tok -> R.Case ("Char_lit",
      (* char_literal *) token env tok
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_boolean_literal env x
    )
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
    )
  | `Float_lit tok -> R.Case ("Float_lit",
      (* float_literal *) token env tok
    )
  )

let map_literal_pattern (env : env) (x : CST.literal_pattern) =
  (match x with
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Raw_str_lit x -> R.Case ("Raw_str_lit",
      map_raw_string_literal env x
    )
  | `Char_lit tok -> R.Case ("Char_lit",
      (* char_literal *) token env tok
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_boolean_literal env x
    )
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
    )
  | `Float_lit tok -> R.Case ("Float_lit",
      (* float_literal *) token env tok
    )
  | `Nega_lit x -> R.Case ("Nega_lit",
      map_negative_literal env x
    )
  )

let map_line_comment (env : env) ((v1, v2) : CST.line_comment) =
  let v1 = (* "//" *) token env v1 in
  let v2 =
    (match v2 with
    | `Imm_tok_prec_p2_pat_262ec39_pat_4fd4a56 (v1, v2) -> R.Case ("Imm_tok_prec_p2_pat_262ec39_pat_4fd4a56",
        let v1 = map_imm_tok_prec_p2_pat_262ec39 env v1 in
        let v2 = map_pat_4fd4a56 env v2 in
        R.Tuple [v1; v2]
      )
    | `Line_doc_comm_marker_line_doc_content (v1, v2) -> R.Case ("Line_doc_comm_marker_line_doc_content",
        let v1 = map_line_doc_comment_marker env v1 in
        let v2 = (* line_doc_content *) token env v2 in
        R.Tuple [v1; v2]
      )
    | `Imm_tok_prec_p1_pat_4fd4a56 x -> R.Case ("Imm_tok_prec_p1_pat_4fd4a56",
        map_imm_tok_prec_p1_pat_4fd4a56 env x
      )
    )
  in
  R.Tuple [v1; v2]

let map_anon_choice_field_id_f2cdd14 (env : env) (x : CST.anon_choice_field_id_f2cdd14) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
    )
  )

let map_anon_choice_field_id_8184947 (env : env) (x : CST.anon_choice_field_id_8184947) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Meta tok -> R.Case ("Meta",
      (* pattern \$[a-zA-Z_]\w* *) token env tok
    )
  )

let map_lifetime (env : env) ((v1, v2) : CST.lifetime) =
  let v1 = (* "'" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  R.Tuple [v1; v2]

let map_label (env : env) ((v1, v2) : CST.label) =
  let v1 = (* "'" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  R.Tuple [v1; v2]

let map_function_modifiers (env : env) (xs : CST.function_modifiers) =
  R.List (List.map (fun x ->
    (match x with
    | `Async tok -> R.Case ("Async",
        (* "async" *) token env tok
      )
    | `Defa tok -> R.Case ("Defa",
        (* "default" *) token env tok
      )
    | `Const tok -> R.Case ("Const",
        (* "const" *) token env tok
      )
    | `Unsafe tok -> R.Case ("Unsafe",
        (* "unsafe" *) token env tok
      )
    | `Extern_modi x -> R.Case ("Extern_modi",
        map_extern_modifier env x
      )
    )
  ) xs)

let map_non_special_token (env : env) (x : CST.non_special_token) =
  (match x with
  | `Choice_lit x -> R.Case ("Choice_lit",
      (match x with
      | `Lit x -> R.Case ("Lit",
          map_literal env x
        )
      | `Id tok -> R.Case ("Id",
          (* identifier *) token env tok
        )
      | `Muta_spec tok -> R.Case ("Muta_spec",
          (* "mut" *) token env tok
        )
      | `Self tok -> R.Case ("Self",
          (* "self" *) token env tok
        )
      | `Super tok -> R.Case ("Super",
          (* "super" *) token env tok
        )
      | `Crate tok -> R.Case ("Crate",
          (* "crate" *) token env tok
        )
      | `Choice_u8 x -> R.Case ("Choice_u8",
          map_anon_choice_u8_6dad923 env x
        )
      | `Rep1_choice_PLUS xs -> R.Case ("Rep1_choice_PLUS",
          R.List (List.map (fun x ->
            (match x with
            | `PLUS tok -> R.Case ("PLUS",
                (* "+" *) token env tok
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
            | `HAT tok -> R.Case ("HAT",
                (* "^" *) token env tok
              )
            | `BANG tok -> R.Case ("BANG",
                (* "!" *) token env tok
              )
            | `AMP tok -> R.Case ("AMP",
                (* "&" *) token env tok
              )
            | `BAR tok -> R.Case ("BAR",
                (* "|" *) token env tok
              )
            | `AMPAMP tok -> R.Case ("AMPAMP",
                (* "&&" *) token env tok
              )
            | `BARBAR tok -> R.Case ("BARBAR",
                (* "||" *) token env tok
              )
            | `LTLT tok -> R.Case ("LTLT",
                (* "<<" *) token env tok
              )
            | `GTGT tok -> R.Case ("GTGT",
                (* ">>" *) token env tok
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
            | `SLASHEQ tok -> R.Case ("SLASHEQ",
                (* "/=" *) token env tok
              )
            | `PERCEQ tok -> R.Case ("PERCEQ",
                (* "%=" *) token env tok
              )
            | `HATEQ tok -> R.Case ("HATEQ",
                (* "^=" *) token env tok
              )
            | `AMPEQ tok -> R.Case ("AMPEQ",
                (* "&=" *) token env tok
              )
            | `BAREQ tok -> R.Case ("BAREQ",
                (* "|=" *) token env tok
              )
            | `LTLTEQ tok -> R.Case ("LTLTEQ",
                (* "<<=" *) token env tok
              )
            | `GTGTEQ tok -> R.Case ("GTGTEQ",
                (* ">>=" *) token env tok
              )
            | `EQ tok -> R.Case ("EQ",
                (* "=" *) token env tok
              )
            | `EQEQ tok -> R.Case ("EQEQ",
                (* "==" *) token env tok
              )
            | `BANGEQ tok -> R.Case ("BANGEQ",
                (* "!=" *) token env tok
              )
            | `GT tok -> R.Case ("GT",
                (* ">" *) token env tok
              )
            | `LT tok -> R.Case ("LT",
                (* "<" *) token env tok
              )
            | `GTEQ tok -> R.Case ("GTEQ",
                (* ">=" *) token env tok
              )
            | `LTEQ tok -> R.Case ("LTEQ",
                (* "<=" *) token env tok
              )
            | `AT tok -> R.Case ("AT",
                (* "@" *) token env tok
              )
            | `X__ tok -> R.Case ("X__",
                (* "_" *) token env tok
              )
            | `DOT tok -> R.Case ("DOT",
                (* "." *) token env tok
              )
            | `DOTDOT tok -> R.Case ("DOTDOT",
                (* ".." *) token env tok
              )
            | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
                (* "..." *) token env tok
              )
            | `DOTDOTEQ tok -> R.Case ("DOTDOTEQ",
                (* "..=" *) token env tok
              )
            | `COMMA tok -> R.Case ("COMMA",
                (* "," *) token env tok
              )
            | `SEMI tok -> R.Case ("SEMI",
                (* ";" *) token env tok
              )
            | `COLON tok -> R.Case ("COLON",
                (* ":" *) token env tok
              )
            | `COLONCOLON tok -> R.Case ("COLONCOLON",
                (* "::" *) token env tok
              )
            | `DASHGT tok -> R.Case ("DASHGT",
                (* "->" *) token env tok
              )
            | `EQGT tok -> R.Case ("EQGT",
                (* "=>" *) token env tok
              )
            | `HASH tok -> R.Case ("HASH",
                (* "#" *) token env tok
              )
            | `QMARK tok -> R.Case ("QMARK",
                (* "?" *) token env tok
              )
            )
          ) xs)
        )
      | `SQUOT tok -> R.Case ("SQUOT",
          (* "'" *) token env tok
        )
      | `As tok -> R.Case ("As",
          (* "as" *) token env tok
        )
      | `Async tok -> R.Case ("Async",
          (* "async" *) token env tok
        )
      | `Await tok -> R.Case ("Await",
          (* "await" *) token env tok
        )
      | `Brk tok -> R.Case ("Brk",
          (* "break" *) token env tok
        )
      | `Const tok -> R.Case ("Const",
          (* "const" *) token env tok
        )
      | `Cont tok -> R.Case ("Cont",
          (* "continue" *) token env tok
        )
      | `Defa tok -> R.Case ("Defa",
          (* "default" *) token env tok
        )
      | `Enum tok -> R.Case ("Enum",
          (* "enum" *) token env tok
        )
      | `Fn tok -> R.Case ("Fn",
          (* "fn" *) token env tok
        )
      | `For tok -> R.Case ("For",
          (* "for" *) token env tok
        )
      | `Gen tok -> R.Case ("Gen",
          (* "gen" *) token env tok
        )
      | `If tok -> R.Case ("If",
          (* "if" *) token env tok
        )
      | `Impl tok -> R.Case ("Impl",
          (* "impl" *) token env tok
        )
      | `Let tok -> R.Case ("Let",
          (* "let" *) token env tok
        )
      | `Loop tok -> R.Case ("Loop",
          (* "loop" *) token env tok
        )
      | `Match tok -> R.Case ("Match",
          (* "match" *) token env tok
        )
      | `Mod tok -> R.Case ("Mod",
          (* "mod" *) token env tok
        )
      | `Pub tok -> R.Case ("Pub",
          (* "pub" *) token env tok
        )
      | `Ret tok -> R.Case ("Ret",
          (* "return" *) token env tok
        )
      | `Static tok -> R.Case ("Static",
          (* "static" *) token env tok
        )
      | `Struct tok -> R.Case ("Struct",
          (* "struct" *) token env tok
        )
      | `Trait tok -> R.Case ("Trait",
          (* "trait" *) token env tok
        )
      | `Type tok -> R.Case ("Type",
          (* "type" *) token env tok
        )
      | `Union tok -> R.Case ("Union",
          (* "union" *) token env tok
        )
      | `Unsafe tok -> R.Case ("Unsafe",
          (* "unsafe" *) token env tok
        )
      | `Use tok -> R.Case ("Use",
          (* "use" *) token env tok
        )
      | `Where tok -> R.Case ("Where",
          (* "where" *) token env tok
        )
      | `While tok -> R.Case ("While",
          (* "while" *) token env tok
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_for_lifetimes (env : env) ((v1, v2, v3, v4, v5, v6) : CST.for_lifetimes) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* "<" *) token env v2 in
  let v3 = map_lifetime env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_lifetime env v2 in
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
  let v6 = (* ">" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_anon_choice_life_c8fcee3 (env : env) (x : CST.anon_choice_life_c8fcee3) =
  (match x with
  | `Life x -> R.Case ("Life",
      map_lifetime env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_non_delim_token (env : env) (x : CST.non_delim_token) =
  (match x with
  | `Choice_choice_lit x -> R.Case ("Choice_choice_lit",
      map_non_special_token env x
    )
  | `DOLLAR tok -> R.Case ("DOLLAR",
      (* "$" *) token env tok
    )
  )

let rec map_token_tree (env : env) (x : CST.token_tree) =
  (match x with
  | `LPAR_rep_choice_tok_tree_RPAR (v1, v2, v3) -> R.Case ("LPAR_rep_choice_tok_tree_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = R.List (List.map (map_tokens env) v2) in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LBRACK_rep_choice_tok_tree_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_rep_choice_tok_tree_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 = R.List (List.map (map_tokens env) v2) in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LCURL_rep_choice_tok_tree_RCURL (v1, v2, v3) -> R.Case ("LCURL_rep_choice_tok_tree_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 = R.List (List.map (map_tokens env) v2) in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_tokens (env : env) (x : CST.tokens) =
  (match x with
  | `Tok_tree x -> R.Case ("Tok_tree",
      map_token_tree env x
    )
  | `Tok_repe (v1, v2, v3, v4, v5, v6) -> R.Case ("Tok_repe",
      let v1 = (* "$" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = R.List (List.map (map_tokens env) v3) in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_pat_1e84e62 env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_anon_choice_PLUS_348fa54 env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Meta tok -> R.Case ("Meta",
      (* pattern \$[a-zA-Z_]\w* *) token env tok
    )
  | `Choice_choice_lit x -> R.Case ("Choice_choice_lit",
      map_non_special_token env x
    )
  )

let rec map_token_pattern (env : env) (x : CST.token_pattern) =
  (match x with
  | `Tok_tree_pat x -> R.Case ("Tok_tree_pat",
      map_token_tree_pattern env x
    )
  | `Tok_repe_pat (v1, v2, v3, v4, v5, v6) -> R.Case ("Tok_repe_pat",
      let v1 = (* "$" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = R.List (List.map (map_token_pattern env) v3) in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_pat_1e84e62 env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_anon_choice_PLUS_348fa54 env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Tok_bind_pat (v1, v2, v3) -> R.Case ("Tok_bind_pat",
      let v1 = (* pattern \$[a-zA-Z_]\w* *) token env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_fragment_specifier env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Meta tok -> R.Case ("Meta",
      (* pattern \$[a-zA-Z_]\w* *) token env tok
    )
  | `Choice_choice_lit x -> R.Case ("Choice_choice_lit",
      map_non_special_token env x
    )
  )

and map_token_tree_pattern (env : env) (x : CST.token_tree_pattern) =
  (match x with
  | `LPAR_rep_tok_pat_RPAR (v1, v2, v3) -> R.Case ("LPAR_rep_tok_pat_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = R.List (List.map (map_token_pattern env) v2) in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LBRACK_rep_tok_pat_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_rep_tok_pat_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 = R.List (List.map (map_token_pattern env) v2) in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LCURL_rep_tok_pat_RCURL (v1, v2, v3) -> R.Case ("LCURL_rep_tok_pat_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 = R.List (List.map (map_token_pattern env) v2) in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let rec map_delim_token_tree (env : env) (x : CST.delim_token_tree) =
  (match x with
  | `LPAR_rep_delim_tokens_RPAR (v1, v2, v3) -> R.Case ("LPAR_rep_delim_tokens_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = R.List (List.map (map_delim_tokens env) v2) in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LBRACK_rep_delim_tokens_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_rep_delim_tokens_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 = R.List (List.map (map_delim_tokens env) v2) in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LCURL_rep_delim_tokens_RCURL (v1, v2, v3) -> R.Case ("LCURL_rep_delim_tokens_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 = R.List (List.map (map_delim_tokens env) v2) in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_delim_tokens (env : env) (x : CST.delim_tokens) =
  (match x with
  | `Non_delim_tok x -> R.Case ("Non_delim_tok",
      map_non_delim_token env x
    )
  | `Delim_tok_tree x -> R.Case ("Delim_tok_tree",
      map_delim_token_tree env x
    )
  )

let map_macro_rule (env : env) ((v1, v2, v3) : CST.macro_rule) =
  let v1 = map_token_tree_pattern env v1 in
  let v2 = (* "=>" *) token env v2 in
  let v3 = map_token_tree env v3 in
  R.Tuple [v1; v2; v3]

let map_macro_definition (env : env) ((v1, v2, v3) : CST.macro_definition) =
  let v1 = (* "macro_rules!" *) token env v1 in
  let v2 =
    (match v2 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Choice_defa x -> R.Case ("Choice_defa",
        map_reserved_identifier env x
      )
    )
  in
  let v3 =
    (match v3 with
    | `LPAR_rep_macro_rule_SEMI_opt_macro_rule_RPAR_SEMI (v1, v2, v3, v4, v5) -> R.Case ("LPAR_rep_macro_rule_SEMI_opt_macro_rule_RPAR_SEMI",
        let v1 = (* "(" *) token env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_macro_rule env v1 in
            let v2 = (* ";" *) token env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_macro_rule env x
            ))
          | None -> R.Option None)
        in
        let v4 = (* ")" *) token env v4 in
        let v5 = (* ";" *) token env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    | `LBRACK_rep_macro_rule_SEMI_opt_macro_rule_RBRACK_SEMI (v1, v2, v3, v4, v5) -> R.Case ("LBRACK_rep_macro_rule_SEMI_opt_macro_rule_RBRACK_SEMI",
        let v1 = (* "[" *) token env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_macro_rule env v1 in
            let v2 = (* ";" *) token env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_macro_rule env x
            ))
          | None -> R.Option None)
        in
        let v4 = (* "]" *) token env v4 in
        let v5 = (* ";" *) token env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    | `LCURL_rep_macro_rule_SEMI_opt_macro_rule_RCURL (v1, v2, v3, v4) -> R.Case ("LCURL_rep_macro_rule_SEMI_opt_macro_rule_RCURL",
        let v1 = (* "{" *) token env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_macro_rule env v1 in
            let v2 = (* ";" *) token env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_macro_rule env x
            ))
          | None -> R.Option None)
        in
        let v4 = (* "}" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  R.Tuple [v1; v2; v3]

let rec map_anon_choice_SEMI_226cc40 (env : env) (x : CST.anon_choice_SEMI_226cc40) =
  (match x with
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  | `Decl_list x -> R.Case ("Decl_list",
      map_declaration_list env x
    )
  )

and map_anon_choice_attr_item_cf43dc1 (env : env) (x : CST.anon_choice_attr_item_cf43dc1) =
  (match x with
  | `Attr_item x -> R.Case ("Attr_item",
      map_attribute_item env x
    )
  | `Inner_attr_item x -> R.Case ("Inner_attr_item",
      map_inner_attribute_item env x
    )
  )

and map_anon_choice_field_id_2c46bcf (env : env) (x : CST.anon_choice_field_id_2c46bcf) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Scoped_type_id x -> R.Case ("Scoped_type_id",
      map_scoped_type_identifier env x
    )
  )

and map_anon_choice_field_id_f1f5a37 (env : env) (x : CST.anon_choice_field_id_f1f5a37) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Scoped_id x -> R.Case ("Scoped_id",
      map_scoped_identifier env x
    )
  )

and map_anon_choice_field_pat_8e757e8 (env : env) (x : CST.anon_choice_field_pat_8e757e8) =
  (match x with
  | `Field_pat (v1, v2, v3) -> R.Case ("Field_pat",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "ref" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "mut" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Id_COLON_pat (v1, v2, v3) -> R.Case ("Id_COLON_pat",
            let v1 = (* identifier *) token env v1 in
            let v2 = (* ":" *) token env v2 in
            let v3 = map_pattern env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Rema_field_pat tok -> R.Case ("Rema_field_pat",
      (* ".." *) token env tok
    )
  )

and map_anon_choice_life_675c24c (env : env) (x : CST.anon_choice_life_675c24c) =
  (match x with
  | `Life x -> R.Case ("Life",
      map_lifetime env x
    )
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  | `Use_bounds (v1, v2, v3, v4, v5) -> R.Case ("Use_bounds",
      let v1 = (* "use" *) token env v1 in
      let v2 = map_tok_prec_p1_lt env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_anon_choice_life_c8fcee3 env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_life_c8fcee3 env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
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
    )
  )

and map_anon_choice_lit_pat_0884ef0 (env : env) (x : CST.anon_choice_lit_pat_0884ef0) =
  (match x with
  | `Lit_pat x -> R.Case ("Lit_pat",
      map_literal_pattern env x
    )
  | `Choice_self x -> R.Case ("Choice_self",
      map_path env x
    )
  )

and map_anon_choice_meta_5845bda (env : env) (x : CST.anon_choice_meta_5845bda) =
  (match x with
  | `Meta tok -> R.Case ("Meta",
      (* pattern \$[a-zA-Z_]\w* *) token env tok
    )
  | `Type_param (v1, v2, v3) -> R.Case ("Type_param",
      let v1 = (* identifier *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_trait_bounds env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_type_ env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Life_param (v1, v2) -> R.Case ("Life_param",
      let v1 = map_lifetime env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_trait_bounds env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Const_param (v1, v2, v3, v4, v5) -> R.Case ("Const_param",
      let v1 = (* "const" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type_ env v4 in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 =
              (match v2 with
              | `Blk x -> R.Case ("Blk",
                  map_block env x
                )
              | `Id tok -> R.Case ("Id",
                  (* identifier *) token env tok
                )
              | `Lit x -> R.Case ("Lit",
                  map_literal env x
                )
              | `Nega_lit x -> R.Case ("Nega_lit",
                  map_negative_literal env x
                )
              )
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_param_2c23cdc (env : env) (x : CST.anon_choice_param_2c23cdc) =
  (match x with
  | `Param x -> R.Case ("Param",
      map_parameter env x
    )
  | `Self_param (v1, v2, v3, v4) -> R.Case ("Self_param",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_lifetime env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "mut" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "self" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Vari_param (v1, v2, v3) -> R.Case ("Vari_param",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "mut" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_pattern env v1 in
            let v2 = (* ":" *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = (* "..." *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `X__ tok -> R.Case ("X__",
      (* "_" *) token env tok
    )
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  )

and map_anon_choice_pat_17a3e23 (env : env) (x : CST.anon_choice_pat_17a3e23) =
  (match x with
  | `Pat x -> R.Case ("Pat",
      map_pattern env x
    )
  | `Clos_exp x -> R.Case ("Clos_exp",
      map_closure_expression env x
    )
  )

and map_anon_choice_pat_4717dcc (env : env) (x : CST.anon_choice_pat_4717dcc) =
  (match x with
  | `Pat x -> R.Case ("Pat",
      map_pattern env x
    )
  | `Param x -> R.Case ("Param",
      map_parameter env x
    )
  )

and map_anon_choice_shor_field_init_9cb4441 (env : env) (x : CST.anon_choice_shor_field_init_9cb4441) =
  (match x with
  | `Shor_field_init (v1, v2) -> R.Case ("Shor_field_init",
      let v1 = R.List (List.map (map_attribute_item env) v1) in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Field_init (v1, v2, v3, v4) -> R.Case ("Field_init",
      let v1 = R.List (List.map (map_attribute_item env) v1) in
      let v2 = map_anon_choice_field_id_f2cdd14 env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Base_field_init x -> R.Case ("Base_field_init",
      map_base_field_initializer env x
    )
  )

and map_anon_choice_type_10b2b37 (env : env) (x : CST.anon_choice_type_10b2b37) =
  (match x with
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  | `Life x -> R.Case ("Life",
      map_lifetime env x
    )
  | `Higher_ranked_trait_bound x -> R.Case ("Higher_ranked_trait_bound",
      map_higher_ranked_trait_bound env x
    )
  )

and map_anon_choice_type_39799c3 (env : env) (x : CST.anon_choice_type_39799c3) =
  (match x with
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  | `Type_bind (v1, v2, v3, v4) -> R.Case ("Type_bind",
      let v1 = (* identifier *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_type_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Life x -> R.Case ("Life",
      map_lifetime env x
    )
  | `Lit x -> R.Case ("Lit",
      map_literal env x
    )
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  )

and map_anon_opt_rep_attr_item_exp_rep_COMMA_rep_attr_item_exp_3d9e0d4 (env : env) (opt : CST.anon_opt_rep_attr_item_exp_rep_COMMA_rep_attr_item_exp_3d9e0d4) =
  (match opt with
  | Some (v1, v2, v3) -> R.Option (Some (
      let v1 = R.List (List.map (map_attribute_item env) v1) in
      let v2 = map_expression env v2 in
      let v3 =
        map_anon_rep_COMMA_rep_attr_item_exp_f504d6c env v3
      in
      R.Tuple [v1; v2; v3]
    ))
  | None -> R.Option None)

and map_anon_pat_rep_COMMA_pat_2a80f16 (env : env) ((v1, v2) : CST.anon_pat_rep_COMMA_pat_2a80f16) =
  let v1 = map_pattern env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anon_rep_COMMA_rep_attr_item_exp_f504d6c (env : env) (xs : CST.anon_rep_COMMA_rep_attr_item_exp_f504d6c) =
  R.List (List.map (fun (v1, v2, v3) ->
    let v1 = (* "," *) token env v1 in
    let v2 = R.List (List.map (map_attribute_item env) v2) in
    let v3 = map_expression env v3 in
    R.Tuple [v1; v2; v3]
  ) xs)

and map_arguments (env : env) ((v1, v2, v3, v4) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    map_anon_opt_rep_attr_item_exp_rep_COMMA_rep_attr_item_exp_3d9e0d4 env v2
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

and map_array_type (env : env) ((v1, v2, v3, v4) : CST.array_type) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ";" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_associated_type (env : env) ((v1, v2, v3, v4, v5, v6) : CST.associated_type) =
  let v1 = (* "type" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_trait_bounds env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = map_path env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `EQ_exp (v1, v2) -> R.Case ("EQ_exp",
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          )
        | `Delim_tok_tree x -> R.Case ("Delim_tok_tree",
            map_delim_token_tree env x
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_attribute_item (env : env) ((v1, v2, v3, v4) : CST.attribute_item) =
  let v1 = (* "#" *) token env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_attribute env v3 in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_base_field_initializer (env : env) ((v1, v2) : CST.base_field_initializer) =
  let v1 = (* ".." *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
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
  | `Exp_choice_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_choice_EQEQ_exp",
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
  | `Exp_choice_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_choice_LTLT_exp",
      let v1 = map_expression env v1 in
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
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
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
        | `PERC tok -> R.Case ("PERC",
            (* "%" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) ((v1, v2, v3, v4, v5) : CST.block) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_label env v1 in
        let v2 = (* ":" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 = (* "{" *) token env v2 in
  let v3 = R.List (List.map (map_statement env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_bounded_type (env : env) ((v1, v2, v3) : CST.bounded_type) =
  let v1 = map_anon_choice_life_675c24c env v1 in
  let v2 = (* "+" *) token env v2 in
  let v3 = map_anon_choice_life_675c24c env v3 in
  R.Tuple [v1; v2; v3]

and map_bracketed_type (env : env) ((v1, v2, v3) : CST.bracketed_type) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | `Type x -> R.Case ("Type",
        map_type_ env x
      )
    | `Qual_type x -> R.Case ("Qual_type",
        map_qualified_type env x
      )
    )
  in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_closure_expression (env : env) ((v1, v2, v3, v4, v5) : CST.closure_expression) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "static" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "move" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_closure_parameters env v4 in
  let v5 =
    (match v5 with
    | `Opt_DASHGT_type_blk (v1, v2) -> R.Case ("Opt_DASHGT_type_blk",
        let v1 =
          (match v1 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* "->" *) token env v1 in
              let v2 = map_type_ env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v2 = map_block env v2 in
        R.Tuple [v1; v2]
      )
    | `Choice_exp x -> R.Case ("Choice_exp",
        (match x with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `X__ tok -> R.Case ("X__",
            (* "_" *) token env tok
          )
        )
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_closure_parameters (env : env) ((v1, v2, v3) : CST.closure_parameters) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_pat_4717dcc env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_pat_4717dcc env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "|" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_condition (env : env) (x : CST.condition) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Let_cond x -> R.Case ("Let_cond",
      map_let_condition env x
    )
  | `Let_chain x -> R.Case ("Let_chain",
      map_let_chain env x
    )
  )

and map_const_block (env : env) ((v1, v2) : CST.const_block) =
  let v1 = (* "const" *) token env v1 in
  let v2 = map_block env v2 in
  R.Tuple [v1; v2]

and map_const_item (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.const_item) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "const" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_type_ env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v7 = (* ";" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_declaration_statement env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_declaration_statement (env : env) (x : CST.declaration_statement) =
  (match x with
  | `Choice_const_item x -> R.Case ("Choice_const_item",
      (match x with
      | `Const_item x -> R.Case ("Const_item",
          map_const_item env x
        )
      | `Macro_invo x -> R.Case ("Macro_invo",
          map_macro_invocation env x
        )
      | `Macro_defi x -> R.Case ("Macro_defi",
          map_macro_definition env x
        )
      | `Empty_stmt tok -> R.Case ("Empty_stmt",
          (* ";" *) token env tok
        )
      | `Attr_item x -> R.Case ("Attr_item",
          map_attribute_item env x
        )
      | `Inner_attr_item x -> R.Case ("Inner_attr_item",
          map_inner_attribute_item env x
        )
      | `Mod_item x -> R.Case ("Mod_item",
          map_mod_item env x
        )
      | `Fore_mod_item x -> R.Case ("Fore_mod_item",
          map_foreign_mod_item env x
        )
      | `Struct_item x -> R.Case ("Struct_item",
          map_struct_item env x
        )
      | `Union_item x -> R.Case ("Union_item",
          map_union_item env x
        )
      | `Enum_item x -> R.Case ("Enum_item",
          map_enum_item env x
        )
      | `Type_item x -> R.Case ("Type_item",
          map_type_item env x
        )
      | `Func_item x -> R.Case ("Func_item",
          map_function_item env x
        )
      | `Func_sign_item x -> R.Case ("Func_sign_item",
          map_function_signature_item env x
        )
      | `Impl_item x -> R.Case ("Impl_item",
          map_impl_item env x
        )
      | `Trait_item x -> R.Case ("Trait_item",
          map_trait_item env x
        )
      | `Asso_type x -> R.Case ("Asso_type",
          map_associated_type env x
        )
      | `Let_decl x -> R.Case ("Let_decl",
          map_let_declaration env x
        )
      | `Use_decl x -> R.Case ("Use_decl",
          map_use_declaration env x
        )
      | `Extern_crate_decl x -> R.Case ("Extern_crate_decl",
          map_extern_crate_declaration env x
        )
      | `Static_item x -> R.Case ("Static_item",
          map_static_item env x
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 =
    (match v2 with
    | `Blk x -> R.Case ("Blk",
        map_block env x
      )
    | `If_exp x -> R.Case ("If_exp",
        map_if_expression env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_enum_item (env : env) ((v1, v2, v3, v4, v5, v6) : CST.enum_item) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "enum" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_enum_variant_list env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_enum_variant (env : env) ((v1, v2, v3, v4) : CST.enum_variant) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* identifier *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        (match x with
        | `Field_decl_list x -> R.Case ("Field_decl_list",
            map_field_declaration_list env x
          )
        | `Orde_field_decl_list x -> R.Case ("Orde_field_decl_list",
            map_ordered_field_declaration_list env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_enum_variant_list (env : env) ((v1, v2, v3, v4) : CST.enum_variant_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = R.List (List.map (map_attribute_item env) v1) in
        let v2 = map_enum_variant env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2, v3) ->
            let v1 = (* "," *) token env v1 in
            let v2 = R.List (List.map (map_attribute_item env) v2) in
            let v3 = map_enum_variant env v3 in
            R.Tuple [v1; v2; v3]
          ) v3)
        in
        R.Tuple [v1; v2; v3]
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

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Exp_except_range x -> R.Case ("Exp_except_range",
      map_expression_except_range env x
    )
  | `Range_exp x -> R.Case ("Range_exp",
      map_range_expression env x
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
  | `Member_access_ellips_exp (v1, v2, v3) -> R.Case ("Member_access_ellips_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "..." *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_expression_ending_with_block (env : env) (x : CST.expression_ending_with_block) =
  (match x with
  | `Unsafe_blk (v1, v2) -> R.Case ("Unsafe_blk",
      let v1 = (* "unsafe" *) token env v1 in
      let v2 = map_block env v2 in
      R.Tuple [v1; v2]
    )
  | `Async_blk (v1, v2, v3) -> R.Case ("Async_blk",
      let v1 = (* "async" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "move" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Gen_blk (v1, v2, v3) -> R.Case ("Gen_blk",
      let v1 = (* "gen" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "move" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Try_blk (v1, v2) -> R.Case ("Try_blk",
      let v1 = (* "try" *) token env v1 in
      let v2 = map_block env v2 in
      R.Tuple [v1; v2]
    )
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `If_exp x -> R.Case ("If_exp",
      map_if_expression env x
    )
  | `Match_exp (v1, v2, v3) -> R.Case ("Match_exp",
      let v1 = (* "match" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_match_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `While_exp (v1, v2, v3, v4) -> R.Case ("While_exp",
      let v1 =
        (match v1 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_label env v1 in
            let v2 = (* ":" *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v2 = (* "while" *) token env v2 in
      let v3 = map_condition env v3 in
      let v4 = map_block env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Loop_exp (v1, v2, v3) -> R.Case ("Loop_exp",
      let v1 =
        (match v1 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_label env v1 in
            let v2 = (* ":" *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v2 = (* "loop" *) token env v2 in
      let v3 = map_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `For_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("For_exp",
      let v1 =
        (match v1 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_label env v1 in
            let v2 = (* ":" *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v2 = (* "for" *) token env v2 in
      let v3 = map_pattern env v3 in
      let v4 = (* "in" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = map_block env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Const_blk x -> R.Case ("Const_blk",
      map_const_block env x
    )
  )

and map_expression_except_range (env : env) (x : CST.expression_except_range) =
  (match x with
  | `Un_exp (v1, v2) -> R.Case ("Un_exp",
      let v1 =
        (match v1 with
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `STAR tok -> R.Case ("STAR",
            (* "*" *) token env tok
          )
        | `BANG tok -> R.Case ("BANG",
            (* "!" *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Ref_exp (v1, v2, v3) -> R.Case ("Ref_exp",
      let v1 = (* "&" *) token env v1 in
      let v2 =
        (match v2 with
        | `Raw_choice_const (v1, v2) -> R.Case ("Raw_choice_const",
            let v1 = (* "raw" *) token env v1 in
            let v2 = map_anon_choice_const_f077f79 env v2 in
            R.Tuple [v1; v2]
          )
        | `Opt_muta_spec opt -> R.Case ("Opt_muta_spec",
            (match opt with
            | Some tok -> R.Option (Some (
                (* "mut" *) token env tok
              ))
            | None -> R.Option None)
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Try_exp (v1, v2) -> R.Case ("Try_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Assign_exp (v1, v2, v3) -> R.Case ("Assign_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Comp_assign_expr (v1, v2, v3) -> R.Case ("Comp_assign_expr",
      let v1 = map_expression env v1 in
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
        | `PERCEQ tok -> R.Case ("PERCEQ",
            (* "%=" *) token env tok
          )
        | `AMPEQ tok -> R.Case ("AMPEQ",
            (* "&=" *) token env tok
          )
        | `BAREQ tok -> R.Case ("BAREQ",
            (* "|=" *) token env tok
          )
        | `HATEQ tok -> R.Case ("HATEQ",
            (* "^=" *) token env tok
          )
        | `LTLTEQ tok -> R.Case ("LTLTEQ",
            (* "<<=" *) token env tok
          )
        | `GTGTEQ tok -> R.Case ("GTGTEQ",
            (* ">>=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Type_cast_exp (v1, v2, v3) -> R.Case ("Type_cast_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Call_exp (v1, v2) -> R.Case ("Call_exp",
      let v1 = map_expression_except_range env v1 in
      let v2 = map_arguments env v2 in
      R.Tuple [v1; v2]
    )
  | `Ret_exp x -> R.Case ("Ret_exp",
      map_return_expression env x
    )
  | `Yield_exp x -> R.Case ("Yield_exp",
      map_yield_expression env x
    )
  | `Lit x -> R.Case ("Lit",
      map_literal env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Choice_u8 x -> R.Case ("Choice_u8",
      map_anon_choice_u8_6dad923 env x
    )
  | `Choice_defa x -> R.Case ("Choice_defa",
      map_reserved_identifier env x
    )
  | `Self tok -> R.Case ("Self",
      (* "self" *) token env tok
    )
  | `Scoped_id x -> R.Case ("Scoped_id",
      map_scoped_identifier env x
    )
  | `Gene_func (v1, v2, v3) -> R.Case ("Gene_func",
      let v1 =
        (match v1 with
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Scoped_id x -> R.Case ("Scoped_id",
            map_scoped_identifier env x
          )
        | `Field_exp x -> R.Case ("Field_exp",
            map_field_expression env x
          )
        )
      in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_type_arguments env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Await_exp (v1, v2, v3) -> R.Case ("Await_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "await" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Field_exp x -> R.Case ("Field_exp",
      map_field_expression env x
    )
  | `Array_exp (v1, v2, v3, v4) -> R.Case ("Array_exp",
      let v1 = (* "[" *) token env v1 in
      let v2 = R.List (List.map (map_attribute_item env) v2) in
      let v3 =
        (match v3 with
        | `Exp_SEMI_exp (v1, v2, v3) -> R.Case ("Exp_SEMI_exp",
            let v1 = map_expression env v1 in
            let v2 = (* ";" *) token env v2 in
            let v3 = map_expression env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `Opt_rep_attr_item_exp_rep_COMMA_rep_attr_item_exp_opt_COMMA (v1, v2) -> R.Case ("Opt_rep_attr_item_exp_rep_COMMA_rep_attr_item_exp_opt_COMMA",
            let v1 =
              map_anon_opt_rep_attr_item_exp_rep_COMMA_rep_attr_item_exp_3d9e0d4 env v1
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
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Tuple_exp (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Tuple_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = R.List (List.map (map_attribute_item env) v2) in
      let v3 = map_expression env v3 in
      let v4 = (* "," *) token env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_expression env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v7 = (* ")" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Macro_invo x -> R.Case ("Macro_invo",
      map_macro_invocation env x
    )
  | `Unit_exp (v1, v2) -> R.Case ("Unit_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = (* ")" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Brk_exp (v1, v2, v3) -> R.Case ("Brk_exp",
      let v1 = (* "break" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_label env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Cont_exp (v1, v2) -> R.Case ("Cont_exp",
      let v1 = (* "continue" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_label env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Index_exp (v1, v2, v3, v4) -> R.Case ("Index_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Meta tok -> R.Case ("Meta",
      (* pattern \$[a-zA-Z_]\w* *) token env tok
    )
  | `Clos_exp x -> R.Case ("Clos_exp",
      map_closure_expression env x
    )
  | `Paren_exp (v1, v2, v3) -> R.Case ("Paren_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Semg_typed_meta x -> R.Case ("Semg_typed_meta",
            map_semgrep_typed_metavar env x
          )
        )
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Struct_exp (v1, v2) -> R.Case ("Struct_exp",
      let v1 =
        (match v1 with
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Scoped_type_id_in_exp_posi x -> R.Case ("Scoped_type_id_in_exp_posi",
            map_scoped_type_identifier_in_expression_position env x
          )
        | `Gene_type_with_turb x -> R.Case ("Gene_type_with_turb",
            map_generic_type_with_turbofish env x
          )
        )
      in
      let v2 = map_field_initializer_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_unsafe_blk x -> R.Case ("Choice_unsafe_blk",
      map_expression_ending_with_block env x
    )
  )

and map_expression_statement (env : env) (x : CST.expression_statement) =
  (match x with
  | `Choice_exp_SEMI x -> R.Case ("Choice_exp_SEMI",
      (match x with
      | `Exp_SEMI (v1, v2) -> R.Case ("Exp_SEMI",
          let v1 = map_expression env v1 in
          let v2 = (* ";" *) token env v2 in
          R.Tuple [v1; v2]
        )
      | `Choice_unsafe_blk x -> R.Case ("Choice_unsafe_blk",
          map_expression_ending_with_block env x
        )
      )
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

and map_extern_crate_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.extern_crate_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "extern" *) token env v2 in
  let v3 = (* "crate" *) token env v3 in
  let v4 = (* identifier *) token env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "as" *) token env v1 in
        let v2 = (* identifier *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_field_declaration (env : env) (x : CST.field_declaration) =
  (match x with
  | `Opt_visi_modi_id_COLON_type (v1, v2, v3, v4) -> R.Case ("Opt_visi_modi_id_COLON_type",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_visibility_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* identifier *) token env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_field_declaration_list (env : env) ((v1, v2, v3, v4) : CST.field_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = R.List (List.map (map_attribute_item env) v1) in
        let v2 = map_field_declaration env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2, v3) ->
            let v1 = (* "," *) token env v1 in
            let v2 = R.List (List.map (map_attribute_item env) v2) in
            let v3 = map_field_declaration env v3 in
            R.Tuple [v1; v2; v3]
          ) v3)
        in
        R.Tuple [v1; v2; v3]
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

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = map_anon_choice_field_id_f2cdd14 env v3 in
  R.Tuple [v1; v2; v3]

and map_field_initializer_list (env : env) ((v1, v2, v3, v4) : CST.field_initializer_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_shor_field_init_9cb4441 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_shor_field_init_9cb4441 env v2 in
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

and map_foreign_mod_item (env : env) ((v1, v2, v3) : CST.foreign_mod_item) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "unsafe" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_extern_modifier env v2 in
  let v3 = map_anon_choice_SEMI_226cc40 env v3 in
  R.Tuple [v1; v2; v3]

and map_function_item (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.function_item) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_function_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "fn" *) token env v3 in
  let v4 = map_anon_choice_field_id_8184947 env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_parameters env v6 in
  let v7 =
    (match v7 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "->" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v8 =
    (match v8 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v9 = map_block env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

and map_function_signature_item (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.function_signature_item) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_function_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "fn" *) token env v3 in
  let v4 = map_anon_choice_field_id_8184947 env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_parameters env v6 in
  let v7 =
    (match v7 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "->" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v8 =
    (match v8 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v9 = (* ";" *) token env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

and map_function_type (env : env) ((v1, v2, v3, v4) : CST.function_type) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_for_lifetimes env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Choice_id x -> R.Case ("Choice_id",
        map_anon_choice_field_id_2c46bcf env x
      )
    | `Opt_func_modifs_fn (v1, v2) -> R.Case ("Opt_func_modifs_fn",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_function_modifiers env x
            ))
          | None -> R.Option None)
        in
        let v2 = (* "fn" *) token env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v3 = map_parameters env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "->" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_generic_type (env : env) ((v1, v2) : CST.generic_type) =
  let v1 =
    (match v1 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Choice_defa x -> R.Case ("Choice_defa",
        map_reserved_identifier env x
      )
    | `Scoped_type_id x -> R.Case ("Scoped_type_id",
        map_scoped_type_identifier env x
      )
    )
  in
  let v2 = map_type_arguments env v2 in
  R.Tuple [v1; v2]

and map_generic_type_with_turbofish (env : env) ((v1, v2, v3) : CST.generic_type_with_turbofish) =
  let v1 = map_anon_choice_field_id_f1f5a37 env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = map_type_arguments env v3 in
  R.Tuple [v1; v2; v3]

and map_higher_ranked_trait_bound (env : env) ((v1, v2, v3) : CST.higher_ranked_trait_bound) =
  let v1 = (* "for" *) token env v1 in
  let v2 = map_type_parameters env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_if_expression (env : env) ((v1, v2, v3, v4) : CST.if_expression) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_condition env v2 in
  let v3 = map_block env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_else_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_impl_item (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.impl_item) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "unsafe" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "impl" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* "!" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | `Id tok -> R.Case ("Id",
              (* identifier *) token env tok
            )
          | `Scoped_type_id x -> R.Case ("Scoped_type_id",
              map_scoped_type_identifier env x
            )
          | `Gene_type x -> R.Case ("Gene_type",
              map_generic_type env x
            )
          )
        in
        let v3 = (* "for" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v5 = map_type_ env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | `Decl_list x -> R.Case ("Decl_list",
        map_declaration_list env x
      )
    | `SEMI tok -> R.Case ("SEMI",
        (* ";" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_inner_attribute_item (env : env) ((v1, v2, v3, v4, v5) : CST.inner_attribute_item) =
  let v1 = (* "#" *) token env v1 in
  let v2 = (* "!" *) token env v2 in
  let v3 = (* "[" *) token env v3 in
  let v4 = map_attribute env v4 in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_last_match_arm (env : env) ((v1, v2, v3, v4, v5) : CST.last_match_arm) =
  let v1 =
    R.List (List.map (map_anon_choice_attr_item_cf43dc1 env) v1)
  in
  let v2 = map_match_pattern env v2 in
  let v3 = (* "=>" *) token env v3 in
  let v4 = map_expression env v4 in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_let_chain (env : env) (x : CST.let_chain) =
  (match x with
  | `Let_chain_AMPAMP_let_cond (v1, v2, v3) -> R.Case ("Let_chain_AMPAMP_let_cond",
      let v1 = map_let_chain env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_let_condition env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Let_chain_AMPAMP_exp (v1, v2, v3) -> R.Case ("Let_chain_AMPAMP_exp",
      let v1 = map_let_chain env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Let_cond_AMPAMP_exp (v1, v2, v3) -> R.Case ("Let_cond_AMPAMP_exp",
      let v1 = map_let_condition env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Let_cond_AMPAMP_let_cond (v1, v2, v3) -> R.Case ("Let_cond_AMPAMP_let_cond",
      let v1 = map_let_condition env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_let_condition env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMPAMP_let_cond (v1, v2, v3) -> R.Case ("Exp_AMPAMP_let_cond",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_let_condition env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_let_condition (env : env) ((v1, v2, v3, v4) : CST.let_condition) =
  let v1 = (* "let" *) token env v1 in
  let v2 = map_pattern env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_expression env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_let_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.let_declaration) =
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
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "else" *) token env v1 in
        let v2 = map_block env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v7 = (* ";" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_macro_invocation (env : env) ((v1, v2, v3) : CST.macro_invocation) =
  let v1 =
    (match v1 with
    | `Scoped_id x -> R.Case ("Scoped_id",
        map_scoped_identifier env x
      )
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Choice_defa x -> R.Case ("Choice_defa",
        map_reserved_identifier env x
      )
    )
  in
  let v2 = (* "!" *) token env v2 in
  let v3 = map_delim_token_tree env v3 in
  R.Tuple [v1; v2; v3]

and map_match_arm (env : env) ((v1, v2, v3, v4) : CST.match_arm) =
  let v1 =
    R.List (List.map (map_anon_choice_attr_item_cf43dc1 env) v1)
  in
  let v2 = map_match_pattern env v2 in
  let v3 = (* "=>" *) token env v3 in
  let v4 =
    (match v4 with
    | `Exp_COMMA (v1, v2) -> R.Case ("Exp_COMMA",
        let v1 = map_expression env v1 in
        let v2 = (* "," *) token env v2 in
        R.Tuple [v1; v2]
      )
    | `Choice_unsafe_blk x -> R.Case ("Choice_unsafe_blk",
        map_expression_ending_with_block env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_match_block (env : env) ((v1, v2, v3) : CST.match_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = R.List (List.map (map_match_arm env) v1) in
        let v2 = map_last_match_arm env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_match_pattern (env : env) ((v1, v2) : CST.match_pattern) =
  let v1 = map_pattern env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "if" *) token env v1 in
        let v2 = map_condition env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_mod_item (env : env) ((v1, v2, v3, v4) : CST.mod_item) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "mod" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  let v4 = map_anon_choice_SEMI_226cc40 env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_or_pattern (env : env) (x : CST.or_pattern) =
  (match x with
  | `Pat_BAR_pat (v1, v2, v3) -> R.Case ("Pat_BAR_pat",
      let v1 = map_pattern env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `BAR_pat (v1, v2) -> R.Case ("BAR_pat",
      let v1 = (* "|" *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_ordered_field_declaration_list (env : env) ((v1, v2, v3, v4) : CST.ordered_field_declaration_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 = R.List (List.map (map_attribute_item env) v1) in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_visibility_modifier env x
            ))
          | None -> R.Option None)
        in
        let v3 = map_type_ env v3 in
        let v4 =
          R.List (List.map (fun (v1, v2, v3, v4) ->
            let v1 = (* "," *) token env v1 in
            let v2 = R.List (List.map (map_attribute_item env) v2) in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_visibility_modifier env x
                ))
              | None -> R.Option None)
            in
            let v4 = map_type_ env v4 in
            R.Tuple [v1; v2; v3; v4]
          ) v4)
        in
        R.Tuple [v1; v2; v3; v4]
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

and map_parameter (env : env) ((v1, v2, v3, v4) : CST.parameter) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "mut" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Pat x -> R.Case ("Pat",
        map_pattern env x
      )
    | `Self tok -> R.Case ("Self",
        (* "self" *) token env tok
      )
    )
  in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type_ env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_parameters (env : env) ((v1, v2, v3, v4) : CST.parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_attribute_item env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_anon_choice_param_2c23cdc env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2, v3) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_attribute_item env x
                ))
              | None -> R.Option None)
            in
            let v3 = map_anon_choice_param_2c23cdc env v3 in
            R.Tuple [v1; v2; v3]
          ) v3)
        in
        R.Tuple [v1; v2; v3]
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

and map_path (env : env) (x : CST.path) =
  (match x with
  | `Self tok -> R.Case ("Self",
      (* "self" *) token env tok
    )
  | `Choice_u8 x -> R.Case ("Choice_u8",
      map_anon_choice_u8_6dad923 env x
    )
  | `Meta tok -> R.Case ("Meta",
      (* pattern \$[a-zA-Z_]\w* *) token env tok
    )
  | `Super tok -> R.Case ("Super",
      (* "super" *) token env tok
    )
  | `Crate tok -> R.Case ("Crate",
      (* "crate" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Scoped_id x -> R.Case ("Scoped_id",
      map_scoped_identifier env x
    )
  | `Choice_defa x -> R.Case ("Choice_defa",
      map_reserved_identifier env x
    )
  )

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Lit_pat x -> R.Case ("Lit_pat",
      map_literal_pattern env x
    )
  | `Choice_u8 x -> R.Case ("Choice_u8",
      map_anon_choice_u8_6dad923 env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Scoped_id x -> R.Case ("Scoped_id",
      map_scoped_identifier env x
    )
  | `Gene_pat (v1, v2, v3) -> R.Case ("Gene_pat",
      let v1 = map_anon_choice_field_id_f1f5a37 env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_type_arguments env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Tuple_pat (v1, v2, v3, v4) -> R.Case ("Tuple_pat",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_anon_choice_pat_17a3e23 env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_pat_17a3e23 env v2 in
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
    )
  | `Tuple_struct_pat (v1, v2, v3, v4, v5) -> R.Case ("Tuple_struct_pat",
      let v1 =
        (match v1 with
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Scoped_id x -> R.Case ("Scoped_id",
            map_scoped_identifier env x
          )
        | `Gene_type_with_turb x -> R.Case ("Gene_type_with_turb",
            map_generic_type_with_turbofish env x
          )
        )
      in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_pat_rep_COMMA_pat_2a80f16 env x
          ))
        | None -> R.Option None)
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
  | `Struct_pat (v1, v2, v3, v4, v5) -> R.Case ("Struct_pat",
      let v1 = map_anon_choice_field_id_2c46bcf env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_anon_choice_field_pat_8e757e8 env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_field_pat_8e757e8 env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = (* "}" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Choice_defa x -> R.Case ("Choice_defa",
      map_reserved_identifier env x
    )
  | `Ref_pat_a3d7f54 (v1, v2) -> R.Case ("Ref_pat_a3d7f54",
      let v1 = (* "ref" *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    )
  | `Slice_pat (v1, v2, v3, v4) -> R.Case ("Slice_pat",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_pat_rep_COMMA_pat_2a80f16 env x
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
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Capt_pat (v1, v2, v3) -> R.Case ("Capt_pat",
      let v1 = (* identifier *) token env v1 in
      let v2 = (* "@" *) token env v2 in
      let v3 = map_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ref_pat_dbbcf07 (v1, v2, v3) -> R.Case ("Ref_pat_dbbcf07",
      let v1 = (* "&" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "mut" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Rema_field_pat tok -> R.Case ("Rema_field_pat",
      (* ".." *) token env tok
    )
  | `Mut_pat (v1, v2) -> R.Case ("Mut_pat",
      let v1 = (* "mut" *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    )
  | `Range_pat x -> R.Case ("Range_pat",
      map_range_pattern env x
    )
  | `Or_pat x -> R.Case ("Or_pat",
      map_or_pattern env x
    )
  | `Const_blk x -> R.Case ("Const_blk",
      map_const_block env x
    )
  | `Macro_invo x -> R.Case ("Macro_invo",
      map_macro_invocation env x
    )
  | `X__ tok -> R.Case ("X__",
      (* "_" *) token env tok
    )
  )

and map_pointer_type (env : env) ((v1, v2, v3) : CST.pointer_type) =
  let v1 = (* "*" *) token env v1 in
  let v2 = map_anon_choice_const_f077f79 env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_qualified_type (env : env) ((v1, v2, v3) : CST.qualified_type) =
  let v1 = map_type_ env v1 in
  let v2 = (* "as" *) token env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_range_expression (env : env) (x : CST.range_expression) =
  (match x with
  | `Exp_choice_DOTDOT_exp (v1, v2, v3) -> R.Case ("Exp_choice_DOTDOT_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `DOTDOT tok -> R.Case ("DOTDOT",
            (* ".." *) token env tok
          )
        | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
            (* "..." *) token env tok
          )
        | `DOTDOTEQ tok -> R.Case ("DOTDOTEQ",
            (* "..=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DOTDOT (v1, v2) -> R.Case ("Exp_DOTDOT",
      let v1 = map_expression env v1 in
      let v2 = (* ".." *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `DOTDOT_exp x -> R.Case ("DOTDOT_exp",
      map_base_field_initializer env x
    )
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  )

and map_range_pattern (env : env) (x : CST.range_pattern) =
  (match x with
  | `Choice_lit_pat_choice_choice_DOTDOTDOT_choice_lit_pat (v1, v2) -> R.Case ("Choice_lit_pat_choice_choice_DOTDOTDOT_choice_lit_pat",
      let v1 = map_anon_choice_lit_pat_0884ef0 env v1 in
      let v2 =
        (match v2 with
        | `Choice_DOTDOTDOT_choice_lit_pat (v1, v2) -> R.Case ("Choice_DOTDOTDOT_choice_lit_pat",
            let v1 =
              (match v1 with
              | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
                  (* "..." *) token env tok
                )
              | `DOTDOTEQ tok -> R.Case ("DOTDOTEQ",
                  (* "..=" *) token env tok
                )
              | `DOTDOT tok -> R.Case ("DOTDOT",
                  (* ".." *) token env tok
                )
              )
            in
            let v2 = map_anon_choice_lit_pat_0884ef0 env v2 in
            R.Tuple [v1; v2]
          )
        | `DOTDOT tok -> R.Case ("DOTDOT",
            (* ".." *) token env tok
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Choice_DOTDOTEQ_choice_lit_pat (v1, v2) -> R.Case ("Choice_DOTDOTEQ_choice_lit_pat",
      let v1 =
        (match v1 with
        | `DOTDOTEQ tok -> R.Case ("DOTDOTEQ",
            (* "..=" *) token env tok
          )
        | `DOTDOT tok -> R.Case ("DOTDOT",
            (* ".." *) token env tok
          )
        )
      in
      let v2 = map_anon_choice_lit_pat_0884ef0 env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_reference_type (env : env) ((v1, v2, v3, v4) : CST.reference_type) =
  let v1 = (* "&" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_lifetime env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "mut" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_type_ env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_removed_trait_bound (env : env) ((v1, v2) : CST.removed_trait_bound) =
  let v1 = (* "?" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_return_expression (env : env) (x : CST.return_expression) =
  (match x with
  | `Ret_exp (v1, v2) -> R.Case ("Ret_exp",
      let v1 = (* "return" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Ret tok -> R.Case ("Ret",
      (* "return" *) token env tok
    )
  )

and map_scoped_identifier (env : env) ((v1, v2, v3) : CST.scoped_identifier) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Choice_self x -> R.Case ("Choice_self",
            map_path env x
          )
        | `Brac_type x -> R.Case ("Brac_type",
            map_bracketed_type env x
          )
        | `Gene_type_with_turb x -> R.Case ("Gene_type_with_turb",
            map_generic_type_with_turbofish env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (match v3 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Super tok -> R.Case ("Super",
        (* "super" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_scoped_type_identifier (env : env) ((v1, v2, v3) : CST.scoped_type_identifier) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Choice_self x -> R.Case ("Choice_self",
            map_path env x
          )
        | `Gene_type_with_turb x -> R.Case ("Gene_type_with_turb",
            map_generic_type_with_turbofish env x
          )
        | `Brac_type x -> R.Case ("Brac_type",
            map_bracketed_type env x
          )
        | `Gene_type x -> R.Case ("Gene_type",
            map_generic_type env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 = (* "::" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_scoped_type_identifier_in_expression_position (env : env) ((v1, v2, v3) : CST.scoped_type_identifier_in_expression_position) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Choice_self x -> R.Case ("Choice_self",
            map_path env x
          )
        | `Gene_type_with_turb x -> R.Case ("Gene_type_with_turb",
            map_generic_type_with_turbofish env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 = (* "::" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_semgrep_typed_metavar (env : env) ((v1, v2, v3) : CST.semgrep_typed_metavar) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_expression_statement env x
    )
  | `Choice_choice_const_item x -> R.Case ("Choice_choice_const_item",
      map_declaration_statement env x
    )
  )

and map_static_item (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.static_item) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "static" *) token env v2 in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "ref" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "mut" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* identifier *) token env v5 in
  let v6 = (* ":" *) token env v6 in
  let v7 = map_type_ env v7 in
  let v8 =
    (match v8 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v9 = (* ";" *) token env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

and map_struct_item (env : env) ((v1, v2, v3, v4, v5) : CST.struct_item) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "struct" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | `Opt_where_clause_field_decl_list (v1, v2) -> R.Case ("Opt_where_clause_field_decl_list",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_where_clause env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_field_declaration_list env v2 in
        R.Tuple [v1; v2]
      )
    | `Orde_field_decl_list_opt_where_clause_SEMI (v1, v2, v3) -> R.Case ("Orde_field_decl_list_opt_where_clause_SEMI",
        let v1 = map_ordered_field_declaration_list env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_where_clause env x
            ))
          | None -> R.Option None)
        in
        let v3 = (* ";" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `SEMI tok -> R.Case ("SEMI",
        (* ";" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_trait_bounds (env : env) ((v1, v2, v3) : CST.trait_bounds) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_anon_choice_type_10b2b37 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "+" *) token env v1 in
      let v2 = map_anon_choice_type_10b2b37 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_trait_item (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.trait_item) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "unsafe" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = (* "trait" *) token env v3 in
  let v4 = (* identifier *) token env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_trait_bounds env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v8 = map_declaration_list env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_tuple_type (env : env) ((v1, v2, v3, v4, v5) : CST.tuple_type) =
  let v1 = (* "(" *) token env v1 in
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
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Abst_type (v1, v2, v3) -> R.Case ("Abst_type",
      let v1 = (* "impl" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "for" *) token env v1 in
            let v2 = map_type_parameters env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Scoped_type_id x -> R.Case ("Scoped_type_id",
            map_scoped_type_identifier env x
          )
        | `Remo_trait_bound x -> R.Case ("Remo_trait_bound",
            map_removed_trait_bound env x
          )
        | `Gene_type x -> R.Case ("Gene_type",
            map_generic_type env x
          )
        | `Func_type x -> R.Case ("Func_type",
            map_function_type env x
          )
        | `Tuple_type x -> R.Case ("Tuple_type",
            map_tuple_type env x
          )
        | `Boun_type x -> R.Case ("Boun_type",
            map_bounded_type env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Ref_type x -> R.Case ("Ref_type",
      map_reference_type env x
    )
  | `Meta tok -> R.Case ("Meta",
      (* pattern \$[a-zA-Z_]\w* *) token env tok
    )
  | `Poin_type x -> R.Case ("Poin_type",
      map_pointer_type env x
    )
  | `Gene_type x -> R.Case ("Gene_type",
      map_generic_type env x
    )
  | `Scoped_type_id x -> R.Case ("Scoped_type_id",
      map_scoped_type_identifier env x
    )
  | `Tuple_type x -> R.Case ("Tuple_type",
      map_tuple_type env x
    )
  | `Unit_type (v1, v2) -> R.Case ("Unit_type",
      let v1 = (* "(" *) token env v1 in
      let v2 = (* ")" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Array_type x -> R.Case ("Array_type",
      map_array_type env x
    )
  | `Func_type x -> R.Case ("Func_type",
      map_function_type env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Macro_invo x -> R.Case ("Macro_invo",
      map_macro_invocation env x
    )
  | `Never_type tok -> R.Case ("Never_type",
      (* "!" *) token env tok
    )
  | `Dyna_type (v1, v2) -> R.Case ("Dyna_type",
      let v1 = (* "dyn" *) token env v1 in
      let v2 =
        (match v2 with
        | `Higher_ranked_trait_bound x -> R.Case ("Higher_ranked_trait_bound",
            map_higher_ranked_trait_bound env x
          )
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Scoped_type_id x -> R.Case ("Scoped_type_id",
            map_scoped_type_identifier env x
          )
        | `Gene_type x -> R.Case ("Gene_type",
            map_generic_type env x
          )
        | `Func_type x -> R.Case ("Func_type",
            map_function_type env x
          )
        | `Tuple_type x -> R.Case ("Tuple_type",
            map_tuple_type env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Boun_type x -> R.Case ("Boun_type",
      map_bounded_type env x
    )
  | `Remo_trait_bound x -> R.Case ("Remo_trait_bound",
      map_removed_trait_bound env x
    )
  | `Choice_u8 x -> R.Case ("Choice_u8",
      map_anon_choice_u8_6dad923 env x
    )
  )

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5, v6) : CST.type_arguments) =
  let v1 = map_tok_prec_p1_lt env v1 in
  let v2 = map_anon_choice_type_39799c3 env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_trait_bounds env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_type_39799c3 env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_trait_bounds env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* ">" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_type_item (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.type_item) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "type" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* "=" *) token env v6 in
  let v7 = map_type_ env v7 in
  let v8 =
    (match v8 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v9 = (* ";" *) token env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5, v6) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = R.List (List.map (map_attribute_item env) v2) in
  let v3 = map_anon_choice_meta_5845bda env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 = R.List (List.map (map_attribute_item env) v2) in
      let v3 = map_anon_choice_meta_5845bda env v3 in
      R.Tuple [v1; v2; v3]
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* ">" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_union_item (env : env) ((v1, v2, v3, v4, v5, v6) : CST.union_item) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "union" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_field_declaration_list env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_use_clause (env : env) (x : CST.use_clause) =
  (match x with
  | `Choice_self x -> R.Case ("Choice_self",
      map_path env x
    )
  | `Use_as_clause (v1, v2, v3) -> R.Case ("Use_as_clause",
      let v1 = map_path env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Use_list x -> R.Case ("Use_list",
      map_use_list env x
    )
  | `Scoped_use_list (v1, v2, v3) -> R.Case ("Scoped_use_list",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_path env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_use_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Use_wild (v1, v2) -> R.Case ("Use_wild",
      let v1 =
        (match v1 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_path env x
                ))
              | None -> R.Option None)
            in
            let v2 = (* "::" *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v2 = (* "*" *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_use_declaration (env : env) ((v1, v2, v3, v4) : CST.use_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_visibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "use" *) token env v2 in
  let v3 = map_use_clause env v3 in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_use_list (env : env) ((v1, v2, v3, v4) : CST.use_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | `Use_clause x -> R.Case ("Use_clause",
              map_use_clause env x
            )
          )
        in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | `Use_clause x -> R.Case ("Use_clause",
                  map_use_clause env x
                )
              )
            in
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

and map_visibility_modifier (env : env) (x : CST.visibility_modifier) =
  (match x with
  | `Crate tok -> R.Case ("Crate",
      (* "crate" *) token env tok
    )
  | `Pub_opt_LPAR_choice_self_RPAR (v1, v2) -> R.Case ("Pub_opt_LPAR_choice_self_RPAR",
      let v1 = (* "pub" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = (* "(" *) token env v1 in
            let v2 =
              (match v2 with
              | `Self tok -> R.Case ("Self",
                  (* "self" *) token env tok
                )
              | `Super tok -> R.Case ("Super",
                  (* "super" *) token env tok
                )
              | `Crate tok -> R.Case ("Crate",
                  (* "crate" *) token env tok
                )
              | `In_choice_self (v1, v2) -> R.Case ("In_choice_self",
                  let v1 = (* "in" *) token env v1 in
                  let v2 = map_path env v2 in
                  R.Tuple [v1; v2]
                )
              )
            in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_where_clause (env : env) ((v1, v2) : CST.where_clause) =
  let v1 = (* "where" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_where_predicate env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_where_predicate env v2 in
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
  R.Tuple [v1; v2]

and map_where_predicate (env : env) ((v1, v2) : CST.where_predicate) =
  let v1 =
    (match v1 with
    | `Life x -> R.Case ("Life",
        map_lifetime env x
      )
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Scoped_type_id x -> R.Case ("Scoped_type_id",
        map_scoped_type_identifier env x
      )
    | `Gene_type x -> R.Case ("Gene_type",
        map_generic_type env x
      )
    | `Ref_type x -> R.Case ("Ref_type",
        map_reference_type env x
      )
    | `Poin_type x -> R.Case ("Poin_type",
        map_pointer_type env x
      )
    | `Tuple_type x -> R.Case ("Tuple_type",
        map_tuple_type env x
      )
    | `Array_type x -> R.Case ("Array_type",
        map_array_type env x
      )
    | `Higher_ranked_trait_bound x -> R.Case ("Higher_ranked_trait_bound",
        map_higher_ranked_trait_bound env x
      )
    | `Choice_u8 x -> R.Case ("Choice_u8",
        map_anon_choice_u8_6dad923 env x
      )
    )
  in
  let v2 = map_trait_bounds env v2 in
  R.Tuple [v1; v2]

and map_yield_expression (env : env) (x : CST.yield_expression) =
  (match x with
  | `Yield_exp (v1, v2) -> R.Case ("Yield_exp",
      let v1 = (* "yield" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Yield tok -> R.Case ("Yield",
      (* "yield" *) token env tok
    )
  )

let map_source_file (env : env) (x : CST.source_file) =
  (match x with
  | `Opt_sheb_rep_stmt (v1, v2) -> R.Case ("Opt_sheb_rep_stmt",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* pattern #![\r\f\t\v ]*([^\[\n].*\
  )?\n *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = R.List (List.map (map_statement env) v2) in
      R.Tuple [v1; v2]
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

let dump_tree root =
  map_source_file () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Line_comment (_loc, x) -> ("line_comment", "line_comment", map_line_comment env x)
  | `Block_comment (_loc, x) -> ("block_comment", "block_comment", map_block_comment env x)

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

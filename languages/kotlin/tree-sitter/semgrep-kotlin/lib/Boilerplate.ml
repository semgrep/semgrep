(**
   Boilerplate to be used as a template when mapping the kotlin CST
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

let map_parameter_modifier (env : env) (x : CST.parameter_modifier) =
  (match x with
  | `Vararg tok -> R.Case ("Vararg",
      (* "vararg" *) token env tok
    )
  | `Noin tok -> R.Case ("Noin",
      (* "noinline" *) token env tok
    )
  | `Cros tok -> R.Case ("Cros",
      (* "crossinline" *) token env tok
    )
  )

let map_pat_80e1d9b (env : env) (tok : CST.pat_80e1d9b) =
  (* pattern [uU]L? *) token env tok

let map_pat_f630af3 (env : env) (tok : CST.pat_f630af3) =
  (* pattern [^\r\n]* *) token env tok

let map_backtick_identifier (env : env) (tok : CST.backtick_identifier) =
  (* pattern `[^\r\n`]+` *) token env tok

let map_pat_a2e2132 (env : env) (tok : CST.pat_a2e2132) =
  (* pattern [0-9a-fA-F]{4} *) token env tok

let map_additive_operator (env : env) (x : CST.additive_operator) =
  (match x with
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  )

let map_function_modifier (env : env) (x : CST.function_modifier) =
  (match x with
  | `Tail tok -> R.Case ("Tail",
      (* "tailrec" *) token env tok
    )
  | `Op tok -> R.Case ("Op",
      (* "operator" *) token env tok
    )
  | `Infix tok -> R.Case ("Infix",
      (* "infix" *) token env tok
    )
  | `Inline tok -> R.Case ("Inline",
      (* "inline" *) token env tok
    )
  | `Exte tok -> R.Case ("Exte",
      (* "external" *) token env tok
    )
  | `Susp tok -> R.Case ("Susp",
      (* "suspend" *) token env tok
    )
  )

let map_class_modifier (env : env) (x : CST.class_modifier) =
  (match x with
  | `Sealed tok -> R.Case ("Sealed",
      (* "sealed" *) token env tok
    )
  | `Anno tok -> R.Case ("Anno",
      (* "annotation" *) token env tok
    )
  | `Data tok -> R.Case ("Data",
      (* "data" *) token env tok
    )
  | `Inner tok -> R.Case ("Inner",
      (* "inner" *) token env tok
    )
  | `Value tok -> R.Case ("Value",
      (* "value" *) token env tok
    )
  )

let map_hex_literal (env : env) (tok : CST.hex_literal) =
  (* hex_literal *) token env tok

let map_escaped_identifier (env : env) (tok : CST.escaped_identifier) =
  (* pattern "\\\\[tbrn'\"\\\\$]" *) token env tok

let map_integer_literal (env : env) (tok : CST.integer_literal) =
  (* integer_literal *) token env tok

let map_platform_modifier (env : env) (x : CST.platform_modifier) =
  (match x with
  | `Expect tok -> R.Case ("Expect",
      (* "expect" *) token env tok
    )
  | `Actual tok -> R.Case ("Actual",
      (* "actual" *) token env tok
    )
  )

let map_semgrep_named_ellipsis (env : env) (tok : CST.semgrep_named_ellipsis) =
  (* pattern \$\.\.\.[a-zA-Z_][a-zA-Z_0-9]* *) token env tok

let map_visibility_modifier (env : env) (x : CST.visibility_modifier) =
  (match x with
  | `Public tok -> R.Case ("Public",
      (* "public" *) token env tok
    )
  | `Priv tok -> R.Case ("Priv",
      (* "private" *) token env tok
    )
  | `Inte tok -> R.Case ("Inte",
      (* "internal" *) token env tok
    )
  | `Prot tok -> R.Case ("Prot",
      (* "protected" *) token env tok
    )
  )

let map_imm_tok_at (env : env) (tok : CST.imm_tok_at) =
  (* "@" *) token env tok

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_bin_literal (env : env) (tok : CST.bin_literal) =
  (* bin_literal *) token env tok

let map_real_literal (env : env) (tok : CST.real_literal) =
  (* real_literal *) token env tok

let map_import_list_delimiter (env : env) (tok : CST.import_list_delimiter) =
  (* import_list_delimiter *) token env tok

let map_comparison_operator (env : env) (x : CST.comparison_operator) =
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

let map_pat_b294348 (env : env) (tok : CST.pat_b294348) =
  (* pattern "[^\\n\\r'\\\\]" *) token env tok

let map_primary_constructor_keyword (env : env) (tok : CST.primary_constructor_keyword) =
  (* primary_constructor_keyword *) token env tok

let map_prefix_unary_operator (env : env) (x : CST.prefix_unary_operator) =
  (match x with
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  )

let map_assignment_and_operator (env : env) (x : CST.assignment_and_operator) =
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
  )

let map_label (env : env) (tok : CST.label) =
  (* label *) token env tok

let map_postfix_unary_operator (env : env) (x : CST.postfix_unary_operator) =
  (match x with
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  | `BANGBANG tok -> R.Case ("BANGBANG",
      (* "!!" *) token env tok
    )
  )

let map_variance_modifier (env : env) (x : CST.variance_modifier) =
  (match x with
  | `In tok -> R.Case ("In",
      (* "in" *) token env tok
    )
  | `Out tok -> R.Case ("Out",
      (* "out" *) token env tok
    )
  )

let map_string_end (env : env) (tok : CST.string_end) =
  (* string_end *) token env tok

let map_string_start (env : env) (tok : CST.string_start) =
  (* string_start *) token env tok

let map_import_dot (env : env) (tok : CST.import_dot) =
  (* import_dot *) token env tok

let map_pat_831065d (env : env) (tok : CST.pat_831065d) =
  (* pattern \$[a-zA-Z_][a-zA-Z_0-9]* *) token env tok

let map_multiplicative_operator (env : env) (x : CST.multiplicative_operator) =
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

let map_string_content (env : env) (tok : CST.string_content) =
  (* string_content *) token env tok

let map_member_modifier (env : env) (x : CST.member_modifier) =
  (match x with
  | `Over tok -> R.Case ("Over",
      (* "override" *) token env tok
    )
  | `Late tok -> R.Case ("Late",
      (* "lateinit" *) token env tok
    )
  )

let map_as_operator (env : env) (x : CST.as_operator) =
  (match x with
  | `As tok -> R.Case ("As",
      (* "as" *) token env tok
    )
  | `AsQM tok -> R.Case ("AsQM",
      (* "as?" *) token env tok
    )
  )

let map_binding_pattern_kind (env : env) (x : CST.binding_pattern_kind) =
  (match x with
  | `Val tok -> R.Case ("Val",
      (* "val" *) token env tok
    )
  | `Var tok -> R.Case ("Var",
      (* "var" *) token env tok
    )
  )

let map_safe_nav (env : env) (tok : CST.safe_nav) =
  (* safe_nav *) token env tok

let map_use_site_target (env : env) ((v1, v2) : CST.use_site_target) =
  let v1 =
    (match v1 with
    | `Field tok -> R.Case ("Field",
        (* "field" *) token env tok
      )
    | `Prop tok -> R.Case ("Prop",
        (* "property" *) token env tok
      )
    | `Get tok -> R.Case ("Get",
        (* "get" *) token env tok
      )
    | `Set tok -> R.Case ("Set",
        (* "set" *) token env tok
      )
    | `Rece tok -> R.Case ("Rece",
        (* "receiver" *) token env tok
      )
    | `Param tok -> R.Case ("Param",
        (* "param" *) token env tok
      )
    | `Setp tok -> R.Case ("Setp",
        (* "setparam" *) token env tok
      )
    | `Dele tok -> R.Case ("Dele",
        (* "delegate" *) token env tok
      )
    )
  in
  let v2 = (* ":" *) token env v2 in
  R.Tuple [v1; v2]

let map_automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  (* automatic_semicolon *) token env tok

let map_inheritance_modifier (env : env) (x : CST.inheritance_modifier) =
  (match x with
  | `Abst tok -> R.Case ("Abst",
      (* "abstract" *) token env tok
    )
  | `Final tok -> R.Case ("Final",
      (* "final" *) token env tok
    )
  | `Open tok -> R.Case ("Open",
      (* "open" *) token env tok
    )
  )

let map_alpha_identifier (env : env) (tok : CST.alpha_identifier) =
  (* pattern [\p{L}_][\p{L}_\p{Nd}]* *) token env tok

let map_equality_operator (env : env) (x : CST.equality_operator) =
  (match x with
  | `BANGEQ tok -> R.Case ("BANGEQ",
      (* "!=" *) token env tok
    )
  | `BANGEQEQ tok -> R.Case ("BANGEQEQ",
      (* "!==" *) token env tok
    )
  | `EQEQ tok -> R.Case ("EQEQ",
      (* "==" *) token env tok
    )
  | `EQEQEQ tok -> R.Case ("EQEQEQ",
      (* "===" *) token env tok
    )
  )

let map_shebang_line (env : env) ((v1, v2) : CST.shebang_line) =
  let v1 = (* "#!" *) token env v1 in
  let v2 = map_pat_f630af3 env v2 in
  R.Tuple [v1; v2]

let map_anon_choice_int_lit_9015f32 (env : env) (x : CST.anon_choice_int_lit_9015f32) =
  (match x with
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
    )
  | `Hex_lit tok -> R.Case ("Hex_lit",
      (* hex_literal *) token env tok
    )
  | `Bin_lit tok -> R.Case ("Bin_lit",
      (* bin_literal *) token env tok
    )
  )

let map_type_projection_modifier (env : env) (x : CST.type_projection_modifier) =
  map_variance_modifier env x

let map_member_access_operator (env : env) (x : CST.member_access_operator) =
  (match x with
  | `DOT tok -> R.Case ("DOT",
      (* "." *) token env tok
    )
  | `COLONCOLON tok -> R.Case ("COLONCOLON",
      (* "::" *) token env tok
    )
  | `Safe_nav tok -> R.Case ("Safe_nav",
      (* safe_nav *) token env tok
    )
  )

let map_modifier (env : env) (x : CST.modifier) =
  (match x with
  | `Class_modi x -> R.Case ("Class_modi",
      map_class_modifier env x
    )
  | `Member_modi x -> R.Case ("Member_modi",
      map_member_modifier env x
    )
  | `Visi_modi x -> R.Case ("Visi_modi",
      map_visibility_modifier env x
    )
  | `Func_modi x -> R.Case ("Func_modi",
      map_function_modifier env x
    )
  | `Prop_modi tok -> R.Case ("Prop_modi",
      (* "const" *) token env tok
    )
  | `Inhe_modi x -> R.Case ("Inhe_modi",
      map_inheritance_modifier env x
    )
  | `Param_modi x -> R.Case ("Param_modi",
      map_parameter_modifier env x
    )
  | `Plat_modi x -> R.Case ("Plat_modi",
      map_platform_modifier env x
    )
  )

let map_lexical_identifier (env : env) (x : CST.lexical_identifier) =
  (match x with
  | `Alpha_id tok -> R.Case ("Alpha_id",
      (* pattern [\p{L}_][\p{L}_\p{Nd}]* *) token env tok
    )
  | `Back_id tok -> R.Case ("Back_id",
      (* pattern `[^\r\n`]+` *) token env tok
    )
  )

let map_character_escape_seq (env : env) (x : CST.character_escape_seq) =
  (match x with
  | `Uni_char_lit (v1, v2) -> R.Case ("Uni_char_lit",
      let v1 = (* "\\u" *) token env v1 in
      let v2 = map_pat_a2e2132 env v2 in
      R.Tuple [v1; v2]
    )
  | `Esca_id tok -> R.Case ("Esca_id",
      (* pattern "\\\\[tbrn'\"\\\\$]" *) token env tok
    )
  )

let map_type_projection_modifiers (env : env) (xs : CST.type_projection_modifiers) =
  R.List (List.map (map_type_projection_modifier env) xs)

let map_simple_identifier (env : env) (x : CST.simple_identifier) =
  (match x with
  | `Choice_lexi_id x -> R.Case ("Choice_lexi_id",
      (match x with
      | `Lexi_id x -> R.Case ("Lexi_id",
          map_lexical_identifier env x
        )
      | `Expect tok -> R.Case ("Expect",
          (* "expect" *) token env tok
        )
      | `Data tok -> R.Case ("Data",
          (* "data" *) token env tok
        )
      | `Inner tok -> R.Case ("Inner",
          (* "inner" *) token env tok
        )
      | `Value tok -> R.Case ("Value",
          (* "value" *) token env tok
        )
      | `Actual tok -> R.Case ("Actual",
          (* "actual" *) token env tok
        )
      | `Set tok -> R.Case ("Set",
          (* "set" *) token env tok
        )
      | `Get tok -> R.Case ("Get",
          (* "get" *) token env tok
        )
      | `Over tok -> R.Case ("Over",
          (* "override" *) token env tok
        )
      | `Susp tok -> R.Case ("Susp",
          (* "suspend" *) token env tok
        )
      | `Anno tok -> R.Case ("Anno",
          (* "annotation" *) token env tok
        )
      | `Sealed tok -> R.Case ("Sealed",
          (* "sealed" *) token env tok
        )
      | `Late tok -> R.Case ("Late",
          (* "lateinit" *) token env tok
        )
      | `Tail tok -> R.Case ("Tail",
          (* "tailrec" *) token env tok
        )
      | `Op tok -> R.Case ("Op",
          (* "operator" *) token env tok
        )
      | `Infix tok -> R.Case ("Infix",
          (* "infix" *) token env tok
        )
      | `Inline tok -> R.Case ("Inline",
          (* "inline" *) token env tok
        )
      | `Exte tok -> R.Case ("Exte",
          (* "external" *) token env tok
        )
      | `Public tok -> R.Case ("Public",
          (* "public" *) token env tok
        )
      | `Priv tok -> R.Case ("Priv",
          (* "private" *) token env tok
        )
      | `Inte tok -> R.Case ("Inte",
          (* "internal" *) token env tok
        )
      | `Prot tok -> R.Case ("Prot",
          (* "protected" *) token env tok
        )
      | `Abst tok -> R.Case ("Abst",
          (* "abstract" *) token env tok
        )
      | `Final tok -> R.Case ("Final",
          (* "final" *) token env tok
        )
      | `Open tok -> R.Case ("Open",
          (* "open" *) token env tok
        )
      | `Const tok -> R.Case ("Const",
          (* "const" *) token env tok
        )
      | `Vararg tok -> R.Case ("Vararg",
          (* "vararg" *) token env tok
        )
      | `Noin tok -> R.Case ("Noin",
          (* "noinline" *) token env tok
        )
      | `Cros tok -> R.Case ("Cros",
          (* "crossinline" *) token env tok
        )
      | `Reif tok -> R.Case ("Reif",
          (* "reified" *) token env tok
        )
      | `Field tok -> R.Case ("Field",
          (* "field" *) token env tok
        )
      | `Prop tok -> R.Case ("Prop",
          (* "property" *) token env tok
        )
      | `Rece tok -> R.Case ("Rece",
          (* "receiver" *) token env tok
        )
      | `Param tok -> R.Case ("Param",
          (* "param" *) token env tok
        )
      | `Setp tok -> R.Case ("Setp",
          (* "setparam" *) token env tok
        )
      | `Dele tok -> R.Case ("Dele",
          (* "delegate" *) token env tok
        )
      | `Comp tok -> R.Case ("Comp",
          (* "companion" *) token env tok
        )
      | `Cons tok -> R.Case ("Cons",
          (* "constructor" *) token env tok
        )
      | `Init tok -> R.Case ("Init",
          (* "init" *) token env tok
        )
      | `Dyna tok -> R.Case ("Dyna",
          (* "dynamic" *) token env tok
        )
      | `Where tok -> R.Case ("Where",
          (* "where" *) token env tok
        )
      | `Catch tok -> R.Case ("Catch",
          (* "catch" *) token env tok
        )
      | `Fina tok -> R.Case ("Fina",
          (* "finally" *) token env tok
        )
      | `Enum tok -> R.Case ("Enum",
          (* "enum" *) token env tok
        )
      )
    )
  | `Cons tok -> R.Case ("Cons",
      (* "constructor" *) token env tok
    )
  | `Pat_831065d x -> R.Case ("Pat_831065d",
      map_pat_831065d env x
    )
  )

let map_return_at (env : env) ((v1, v2) : CST.return_at) =
  let v1 = (* "return@" *) token env v1 in
  let v2 = map_lexical_identifier env v2 in
  R.Tuple [v1; v2]

let map_this_expression (env : env) (x : CST.this_expression) =
  (match x with
  | `This tok -> R.Case ("This",
      (* "this" *) token env tok
    )
  | `This_at (v1, v2) -> R.Case ("This_at",
      let v1 = (* "this@" *) token env v1 in
      let v2 = map_lexical_identifier env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_identifier (env : env) ((v1, v2) : CST.identifier) =
  let v1 = map_simple_identifier env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 = map_simple_identifier env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let rec map_import_identifier (env : env) (x : CST.import_identifier) =
  (match x with
  | `Simple_id x -> R.Case ("Simple_id",
      map_simple_identifier env x
    )
  | `Import_id_import_dot_simple_id (v1, v2, v3) -> R.Case ("Import_id_import_dot_simple_id",
      let v1 = map_import_identifier env v1 in
      let v2 = (* import_dot *) token env v2 in
      let v3 = map_simple_identifier env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_callable_reference (env : env) ((v1, v2, v3) : CST.callable_reference) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_simple_identifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (match v3 with
    | `Simple_id x -> R.Case ("Simple_id",
        map_simple_identifier env x
      )
    | `Class tok -> R.Case ("Class",
        (* "class" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

let map_import_alias (env : env) ((v1, v2) : CST.import_alias) =
  let v1 = (* "as" *) token env v1 in
  let v2 = map_simple_identifier env v2 in
  R.Tuple [v1; v2]

let map_literal_constant (env : env) (x : CST.literal_constant) =
  (match x with
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_boolean_literal env x
    )
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
    )
  | `Hex_lit tok -> R.Case ("Hex_lit",
      (* hex_literal *) token env tok
    )
  | `Bin_lit tok -> R.Case ("Bin_lit",
      (* bin_literal *) token env tok
    )
  | `Char_lit (v1, v2, v3) -> R.Case ("Char_lit",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        (match v2 with
        | `Char_esc_seq x -> R.Case ("Char_esc_seq",
            map_character_escape_seq env x
          )
        | `Pat_b294348 x -> R.Case ("Pat_b294348",
            map_pat_b294348 env x
          )
        )
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Real_lit tok -> R.Case ("Real_lit",
      (* real_literal *) token env tok
    )
  | `Null_lit tok -> R.Case ("Null_lit",
      (* "null" *) token env tok
    )
  | `Long_lit (v1, v2) -> R.Case ("Long_lit",
      let v1 = map_anon_choice_int_lit_9015f32 env v1 in
      let v2 = (* "L" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Unsi_lit (v1, v2) -> R.Case ("Unsi_lit",
      let v1 = map_anon_choice_int_lit_9015f32 env v1 in
      let v2 = map_pat_80e1d9b env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_package_header (env : env) ((v1, v2, v3) : CST.package_header) =
  let v1 = (* "package" *) token env v1 in
  let v2 = map_identifier env v2 in
  let v3 = (* automatic_semicolon *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_import_header (env : env) ((v1, v2, v3, v4) : CST.import_header) =
  let v1 = (* "import" *) token env v1 in
  let v2 = map_import_identifier env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        (match x with
        | `Import_dot_wild_import (v1, v2) -> R.Case ("Import_dot_wild_import",
            let v1 = (* import_dot *) token env v1 in
            let v2 = (* "*" *) token env v2 in
            R.Tuple [v1; v2]
          )
        | `Import_alias x -> R.Case ("Import_alias",
            map_import_alias env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v4 = (* automatic_semicolon *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let rec map_annotated_lambda (env : env) ((v1, v2, v3) : CST.annotated_lambda) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* label *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_lambda_literal env v3 in
  R.Tuple [v1; v2; v3]

and map_annotation (env : env) (x : CST.annotation) =
  (match x with
  | `Single_anno (v1, v2, v3) -> R.Case ("Single_anno",
      let v1 = (* "@" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_use_site_target env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_unescaped_annotation env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Multi_anno (v1, v2, v3, v4, v5) -> R.Case ("Multi_anno",
      let v1 = (* "@" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_use_site_target env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "[" *) token env v3 in
      let v4 =
        R.List (List.map (map_unescaped_annotation env) v4)
      in
      let v5 = (* "]" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_opt_value_args_anno_lambda_7aee0dd (env : env) (x : CST.anon_choice_opt_value_args_anno_lambda_7aee0dd) =
  (match x with
  | `Opt_value_args_anno_lambda (v1, v2) -> R.Case ("Opt_value_args_anno_lambda",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_value_arguments env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_annotated_lambda env v2 in
      R.Tuple [v1; v2]
    )
  | `Value_args x -> R.Case ("Value_args",
      map_value_arguments env x
    )
  )

and map_anon_choice_param_b77c1d8 (env : env) (x : CST.anon_choice_param_b77c1d8) =
  (match x with
  | `Param x -> R.Case ("Param",
      map_parameter env x
    )
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  )

and map_anon_choice_user_type_83255f6 (env : env) (x : CST.anon_choice_user_type_83255f6) =
  (match x with
  | `User_type x -> R.Case ("User_type",
      map_user_type env x
    )
  | `Paren_user_type (v1, v2, v3) -> R.Case ("Paren_user_type",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_anon_choice_user_type_83255f6 env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_var_decl_1db8ddd (env : env) (x : CST.anon_choice_var_decl_1db8ddd) =
  (match x with
  | `Var_decl x -> R.Case ("Var_decl",
      map_variable_declaration env x
    )
  | `Multi_var_decl x -> R.Case ("Multi_var_decl",
      map_multi_variable_declaration env x
    )
  )

and map_anon_opt_rece_type_opt_DOT_cc9388e (env : env) (opt : CST.anon_opt_rece_type_opt_DOT_cc9388e) =
  (match opt with
  | Some (v1, v2) -> R.Option (Some (
      let v1 = map_receiver_type env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "." *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ))
  | None -> R.Option None)

and map_anonymous_initializer (env : env) ((v1, v2) : CST.anonymous_initializer) =
  let v1 = (* "init" *) token env v1 in
  let v2 = map_block env v2 in
  R.Tuple [v1; v2]

and map_assignment (env : env) (x : CST.assignment) =
  (match x with
  | `Dire_assi_exp_assign_and_op_exp (v1, v2, v3) -> R.Case ("Dire_assi_exp_assign_and_op_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = map_assignment_and_operator env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Dire_assi_exp_EQ_exp (v1, v2, v3) -> R.Case ("Dire_assi_exp_EQ_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Mult_exp (v1, v2, v3) -> R.Case ("Mult_exp",
      let v1 = map_expression env v1 in
      let v2 = map_multiplicative_operator env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Addi_exp (v1, v2, v3) -> R.Case ("Addi_exp",
      let v1 = map_expression env v1 in
      let v2 = map_additive_operator env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Range_exp (v1, v2, v3) -> R.Case ("Range_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `DOTDOT tok -> R.Case ("DOTDOT",
            (* ".." *) token env tok
          )
        | `DOTDOTLT tok -> R.Case ("DOTDOTLT",
            (* "..<" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Infix_exp (v1, v2, v3) -> R.Case ("Infix_exp",
      let v1 = map_expression env v1 in
      let v2 = map_simple_identifier env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Elvis_exp (v1, v2, v3) -> R.Case ("Elvis_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "?:" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Check_exp (v1, v2) -> R.Case ("Check_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `Opt_BANG_in_op_exp x -> R.Case ("Opt_BANG_in_op_exp",
            map_range_test env x
          )
        | `Opt_BANG_is_op_type x -> R.Case ("Opt_BANG_is_op_type",
            map_type_test env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Comp_exp (v1, v2, v3) -> R.Case ("Comp_exp",
      let v1 = map_expression env v1 in
      let v2 = map_comparison_operator env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Equa_exp (v1, v2, v3) -> R.Case ("Equa_exp",
      let v1 = map_expression env v1 in
      let v2 = map_equality_operator env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Conj_exp (v1, v2, v3) -> R.Case ("Conj_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Disj_exp (v1, v2, v3) -> R.Case ("Disj_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_call_suffix (env : env) ((v1, v2) : CST.call_suffix) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    map_anon_choice_opt_value_args_anno_lambda_7aee0dd env v2
  in
  R.Tuple [v1; v2]

and map_call_suffix_no_trailing_lambda (env : env) ((v1, v2) : CST.call_suffix_no_trailing_lambda) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_value_arguments env v2 in
  R.Tuple [v1; v2]

and map_catch_block (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.catch_block) =
  let v1 = (* "catch" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = R.List (List.map (map_annotation env) v3) in
  let v4 = map_simple_identifier env v4 in
  let v5 = (* ":" *) token env v5 in
  let v6 = map_type_ env v6 in
  let v7 = (* ")" *) token env v7 in
  let v8 = map_block env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_class_member_declarations env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_class_declaration (env : env) (x : CST.class_declaration) =
  (match x with
  | `Opt_modifs_choice_class_simple_id_opt_type_params_opt_prim_cons_opt_COLON_dele_specis_opt_type_consts_opt_class_body (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Opt_modifs_choice_class_simple_id_opt_type_params_opt_prim_cons_opt_COLON_dele_specis_opt_type_consts_opt_class_body",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Class tok -> R.Case ("Class",
            (* "class" *) token env tok
          )
        | `Opt_fun_inte (v1, v2) -> R.Case ("Opt_fun_inte",
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* "fun" *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 = (* "interface" *) token env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      let v3 = map_simple_identifier env v3 in
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
            map_primary_constructor env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_delegation_specifiers env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            map_type_constraints env x
          ))
        | None -> R.Option None)
      in
      let v8 =
        (match v8 with
        | Some x -> R.Option (Some (
            map_class_body env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Opt_modifs_enum_class_simple_id_opt_type_params_opt_prim_cons_opt_COLON_dele_specis_opt_type_consts_opt_enum_class_body (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("Opt_modifs_enum_class_simple_id_opt_type_params_opt_prim_cons_opt_COLON_dele_specis_opt_type_consts_opt_enum_class_body",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "enum" *) token env v2 in
      let v3 = (* "class" *) token env v3 in
      let v4 = map_simple_identifier env v4 in
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
            map_primary_constructor env x
          ))
        | None -> R.Option None)
      in
      let v7 =
        (match v7 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_delegation_specifiers env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v8 =
        (match v8 with
        | Some x -> R.Option (Some (
            map_type_constraints env x
          ))
        | None -> R.Option None)
      in
      let v9 =
        (match v9 with
        | Some x -> R.Option (Some (
            map_enum_class_body env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  )

and map_class_member_declaration (env : env) (x : CST.class_member_declaration) =
  (match x with
  | `Choice_decl x -> R.Case ("Choice_decl",
      (match x with
      | `Decl x -> R.Case ("Decl",
          map_declaration env x
        )
      | `Comp_obj x -> R.Case ("Comp_obj",
          map_companion_object env x
        )
      | `Anon_init x -> R.Case ("Anon_init",
          map_anonymous_initializer env x
        )
      | `Seco_cons x -> R.Case ("Seco_cons",
          map_secondary_constructor env x
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_class_member_declarations (env : env) (xs : CST.class_member_declarations) =
  R.List (List.map (fun (v1, v2) ->
    let v1 = map_class_member_declaration env v1 in
    let v2 = (* automatic_semicolon *) token env v2 in
    R.Tuple [v1; v2]
  ) xs)

and map_class_parameter (env : env) (x : CST.class_parameter) =
  (match x with
  | `Opt_modifs_opt_bind_pat_kind_simple_id_COLON_type_opt_EQ_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Opt_modifs_opt_bind_pat_kind_simple_id_COLON_type_opt_EQ_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_binding_pattern_kind env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_simple_identifier env v3 in
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
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_class_parameters (env : env) ((v1, v2, v3, v4) : CST.class_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_class_parameter env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_class_parameter env v2 in
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

and map_collection_literal (env : env) ((v1, v2, v3, v4, v5) : CST.collection_literal) =
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

and map_companion_object (env : env) ((v1, v2, v3, v4, v5, v6) : CST.companion_object) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "companion" *) token env v2 in
  let v3 = (* "object" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_simple_identifier env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_delegation_specifiers env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_class_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_constructor_delegation_call (env : env) ((v1, v2) : CST.constructor_delegation_call) =
  let v1 =
    (match v1 with
    | `This tok -> R.Case ("This",
        (* "this" *) token env tok
      )
    | `Super tok -> R.Case ("Super",
        (* "super" *) token env tok
      )
    )
  in
  let v2 = map_value_arguments env v2 in
  R.Tuple [v1; v2]

and map_constructor_invocation (env : env) ((v1, v2) : CST.constructor_invocation) =
  let v1 = map_user_type env v1 in
  let v2 = map_value_arguments env v2 in
  R.Tuple [v1; v2]

and map_control_structure_body (env : env) (x : CST.control_structure_body) =
  (match x with
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `Stmt x -> R.Case ("Stmt",
      map_statement env x
    )
  )

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Class_decl x -> R.Case ("Class_decl",
      map_class_declaration env x
    )
  | `Obj_decl (v1, v2, v3, v4, v5) -> R.Case ("Obj_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "object" *) token env v2 in
      let v3 = map_simple_identifier env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_delegation_specifiers env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_class_body env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Func_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("Func_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "fun" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_anon_opt_rece_type_opt_DOT_cc9388e env v4 in
      let v5 = map_simple_identifier env v5 in
      let v6 = map_function_value_parameters env v6 in
      let v7 =
        (match v7 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_type_ env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v8 =
        (match v8 with
        | Some x -> R.Option (Some (
            map_type_constraints env x
          ))
        | None -> R.Option None)
      in
      let v9 =
        (match v9 with
        | Some x -> R.Option (Some (
            map_function_body env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Prop_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("Prop_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_binding_pattern_kind env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_anon_opt_rece_type_opt_DOT_cc9388e env v4 in
      let v5 = map_anon_choice_var_decl_1db8ddd env v5 in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_type_constraints env x
          ))
        | None -> R.Option None)
      in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            (match x with
            | `EQ_exp (v1, v2) -> R.Case ("EQ_exp",
                let v1 = (* "=" *) token env v1 in
                let v2 = map_expression env v2 in
                R.Tuple [v1; v2]
              )
            | `Prop_dele x -> R.Case ("Prop_dele",
                map_property_delegate env x
              )
            )
          ))
        | None -> R.Option None)
      in
      let v8 =
        (match v8 with
        | Some tok -> R.Option (Some (
            (* ";" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v9 =
        (match v9 with
        | Some x -> R.Option (Some (
            (match x with
            | `Getter_opt_setter (v1, v2) -> R.Case ("Getter_opt_setter",
                let v1 = map_getter env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_setter env x
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              )
            | `Setter_opt_getter (v1, v2) -> R.Case ("Setter_opt_getter",
                let v1 = map_setter env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_getter env x
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Getter x -> R.Case ("Getter",
      map_getter env x
    )
  | `Setter x -> R.Case ("Setter",
      map_setter env x
    )
  | `Type_alias (v1, v2, v3, v4, v5, v6) -> R.Case ("Type_alias",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "typealias" *) token env v2 in
      let v3 = map_simple_identifier env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "=" *) token env v5 in
      let v6 = map_type_ env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_delegation_specifier (env : env) (x : CST.delegation_specifier) =
  (match x with
  | `Cons_invo x -> R.Case ("Cons_invo",
      map_constructor_invocation env x
    )
  | `Expl_dele (v1, v2, v3) -> R.Case ("Expl_dele",
      let v1 =
        (match v1 with
        | `User_type x -> R.Case ("User_type",
            map_user_type env x
          )
        | `Func_type x -> R.Case ("Func_type",
            map_function_type env x
          )
        )
      in
      let v2 = (* "by" *) token env v2 in
      let v3 = map_expression_no_trailing_lambda env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `User_type x -> R.Case ("User_type",
      map_user_type env x
    )
  | `Func_type x -> R.Case ("Func_type",
      map_function_type env x
    )
  | `LPAR_func_type_RPAR (v1, v2, v3) -> R.Case ("LPAR_func_type_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_function_type env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Susp_func_type (v1, v2) -> R.Case ("Susp_func_type",
      let v1 = (* "suspend" *) token env v1 in
      let v2 = map_function_type env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_delegation_specifiers (env : env) ((v1, v2) : CST.delegation_specifiers) =
  let v1 = map_delegation_specifier env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_delegation_specifier env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_directly_assignable_expression (env : env) (x : CST.directly_assignable_expression) =
  (match x with
  | `Post_un_exp (v1, v2) -> R.Case ("Post_un_exp",
      let v1 = map_primary_expression env v1 in
      let v2 =
        R.List (List.map (map_postfix_unary_suffix env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Simple_id x -> R.Case ("Simple_id",
      map_simple_identifier env x
    )
  )

and map_enum_class_body (env : env) ((v1, v2, v3, v4) : CST.enum_class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_enum_entries env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ";" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_class_member_declarations env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_enum_entries (env : env) ((v1, v2, v3) : CST.enum_entries) =
  let v1 = map_enum_entry env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_enum_entry env v2 in
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

and map_enum_entry (env : env) ((v1, v2, v3, v4) : CST.enum_entry) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_simple_identifier env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_value_arguments env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_class_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_un_exp x -> R.Case ("Choice_un_exp",
      (match x with
      | `Un_exp x -> R.Case ("Un_exp",
          map_unary_expression env x
        )
      | `Bin_exp x -> R.Case ("Bin_exp",
          map_binary_expression env x
        )
      | `Prim_exp x -> R.Case ("Prim_exp",
          map_primary_expression env x
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
  | `Typed_meta (v1, v2, v3, v4, v5) -> R.Case ("Typed_meta",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_simple_identifier env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type_ env v4 in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Semg_named_ellips tok -> R.Case ("Semg_named_ellips",
      (* pattern \$\.\.\.[a-zA-Z_][a-zA-Z_0-9]* *) token env tok
    )
  )

and map_expression_no_trailing_lambda (env : env) (x : CST.expression_no_trailing_lambda) =
  (match x with
  | `Un_exp x -> R.Case ("Un_exp",
      map_unary_expression env x
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Prim_exp_no_trai_lambda x -> R.Case ("Prim_exp_no_trai_lambda",
      map_primary_expression_no_trailing_lambda env x
    )
  )

and map_finally_block (env : env) ((v1, v2) : CST.finally_block) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_block env v2 in
  R.Tuple [v1; v2]

and map_function_body (env : env) (x : CST.function_body) =
  (match x with
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `EQ_exp (v1, v2) -> R.Case ("EQ_exp",
      let v1 = (* "=" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_function_literal (env : env) (x : CST.function_literal) =
  (match x with
  | `Lambda_lit x -> R.Case ("Lambda_lit",
      map_lambda_literal env x
    )
  | `Anon_func (v1, v2, v3, v4, v5, v6) -> R.Case ("Anon_func",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "suspend" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "fun" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_simple_user_type env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "." *) token env v1 in
                let v2 = map_simple_user_type env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 = (* "." *) token env v3 in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v4 = map_function_value_parameters env v4 in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_type_ env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_function_body env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_function_type (env : env) ((v1, v2, v3, v4) : CST.function_type) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_receiver_type env v1 in
        let v2 = (* "." *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 = map_function_type_parameters env v2 in
  let v3 = (* "->" *) token env v3 in
  let v4 = map_type_ env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_function_type_parameters (env : env) ((v1, v2, v3) : CST.function_type_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_param_b77c1d8 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_param_b77c1d8 env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_function_value_parameter (env : env) (x : CST.function_value_parameter) =
  (match x with
  | `Opt_param_modifs_param_opt_EQ_exp (v1, v2, v3) -> R.Case ("Opt_param_modifs_param_opt_EQ_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_parameter_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_parameter env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_function_value_parameters (env : env) ((v1, v2, v3, v4) : CST.function_value_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_function_value_parameter env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_function_value_parameter env v2 in
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

and map_generic_call_suffix (env : env) ((v1, v2) : CST.generic_call_suffix) =
  let v1 = map_type_arguments env v1 in
  let v2 =
    map_anon_choice_opt_value_args_anno_lambda_7aee0dd env v2
  in
  R.Tuple [v1; v2]

and map_generic_call_suffix_no_trailing_lambda (env : env) ((v1, v2) : CST.generic_call_suffix_no_trailing_lambda) =
  let v1 = map_type_arguments env v1 in
  let v2 = map_value_arguments env v2 in
  R.Tuple [v1; v2]

and map_getter (env : env) ((v1, v2, v3) : CST.getter) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "get" *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = (* ")" *) token env v2 in
        let v3 =
          (match v3 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* ":" *) token env v1 in
              let v2 = map_type_ env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v4 = map_function_body env v4 in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_if_expression (env : env) ((v1, v2, v3, v4, v5) : CST.if_expression) =
  let v1 = (* "if" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ")" *) token env v4 in
  let v5 =
    (match v5 with
    | `Cont_stru_body x -> R.Case ("Cont_stru_body",
        map_control_structure_body env x
      )
    | `Opt_cont_stru_body_opt_SEMI_else_choice_cont_stru_body (v1, v2, v3, v4) -> R.Case ("Opt_cont_stru_body_opt_SEMI_else_choice_cont_stru_body",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_control_structure_body env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* ";" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v3 = (* "else" *) token env v3 in
        let v4 =
          (match v4 with
          | `Cont_stru_body x -> R.Case ("Cont_stru_body",
              map_control_structure_body env x
            )
          | `SEMI tok -> R.Case ("SEMI",
              (* ";" *) token env tok
            )
          )
        in
        R.Tuple [v1; v2; v3; v4]
      )
    | `SEMI tok -> R.Case ("SEMI",
        (* ";" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_indexing_suffix (env : env) ((v1, v2, v3, v4) : CST.indexing_suffix) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_interpolation (env : env) (x : CST.interpolation) =
  (match x with
  | `DOLLARLCURL_exp_RCURL (v1, v2, v3) -> R.Case ("DOLLARLCURL_exp_RCURL",
      let v1 = (* "${" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `DOLLAR_simple_id (v1, v2) -> R.Case ("DOLLAR_simple_id",
      let v1 = (* "$" *) token env v1 in
      let v2 = map_simple_identifier env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_jump_expression (env : env) (x : CST.jump_expression) =
  (match x with
  | `Throw_exp (v1, v2) -> R.Case ("Throw_exp",
      let v1 = (* "throw" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_ret_opt_exp (v1, v2) -> R.Case ("Choice_ret_opt_exp",
      let v1 =
        (match v1 with
        | `Ret tok -> R.Case ("Ret",
            (* "return" *) token env tok
          )
        | `Ret_at x -> R.Case ("Ret_at",
            map_return_at env x
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Cont tok -> R.Case ("Cont",
      (* "continue" *) token env tok
    )
  | `Cont_at (v1, v2) -> R.Case ("Cont_at",
      let v1 = (* "continue@" *) token env v1 in
      let v2 = map_lexical_identifier env v2 in
      R.Tuple [v1; v2]
    )
  | `Brk tok -> R.Case ("Brk",
      (* "break" *) token env tok
    )
  | `Brk_at (v1, v2) -> R.Case ("Brk_at",
      let v1 = (* "break@" *) token env v1 in
      let v2 = map_lexical_identifier env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_lambda_literal (env : env) ((v1, v2, v3, v4) : CST.lambda_literal) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_lambda_parameters env x
            ))
          | None -> R.Option None)
        in
        let v2 = (* "->" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_lambda_parameter (env : env) (x : CST.lambda_parameter) =
  (match x with
  | `Var_decl x -> R.Case ("Var_decl",
      map_variable_declaration env x
    )
  | `Multi_var_decl_opt_COLON_type (v1, v2) -> R.Case ("Multi_var_decl_opt_COLON_type",
      let v1 = map_multi_variable_declaration env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_type_ env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_lambda_parameters (env : env) ((v1, v2) : CST.lambda_parameters) =
  let v1 = map_lambda_parameter env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_lambda_parameter env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_loop_statement (env : env) (x : CST.loop_statement) =
  (match x with
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("For_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = R.List (List.map (map_annotation env) v3) in
      let v4 = map_anon_choice_var_decl_1db8ddd env v4 in
      let v5 = (* "in" *) token env v5 in
      let v6 = map_expression env v6 in
      let v7 = (* ")" *) token env v7 in
      let v8 =
        (match v8 with
        | Some x -> R.Option (Some (
            map_control_structure_body env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `While_stmt (v1, v2, v3, v4, v5) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        (match v5 with
        | `SEMI tok -> R.Case ("SEMI",
            (* ";" *) token env tok
          )
        | `Cont_stru_body x -> R.Case ("Cont_stru_body",
            map_control_structure_body env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Do_while_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("Do_while_stmt",
      let v1 = (* "do" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_control_structure_body env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "while" *) token env v3 in
      let v4 = (* "(" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_modifiers (env : env) (xs : CST.modifiers) =
  R.List (List.map (fun x ->
    (match x with
    | `Anno x -> R.Case ("Anno",
        map_annotation env x
      )
    | `Modi x -> R.Case ("Modi",
        map_modifier env x
      )
    )
  ) xs)

and map_multi_variable_declaration (env : env) ((v1, v2, v3, v4) : CST.multi_variable_declaration) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_variable_declaration env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable_declaration env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_navigation_suffix (env : env) (x : CST.navigation_suffix) =
  (match x with
  | `Opt_type_args_member_access_op_choice_simple_id (v1, v2, v3) -> R.Case ("Opt_type_args_member_access_op_choice_simple_id",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_member_access_operator env v2 in
      let v3 =
        (match v3 with
        | `Simple_id x -> R.Case ("Simple_id",
            map_simple_identifier env x
          )
        | `Paren_exp x -> R.Case ("Paren_exp",
            map_parenthesized_expression env x
          )
        | `Class tok -> R.Case ("Class",
            (* "class" *) token env tok
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Member_access_op_ellips (v1, v2) -> R.Case ("Member_access_op_ellips",
      let v1 = map_member_access_operator env v1 in
      let v2 = (* "..." *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_not_nullable_type (env : env) ((v1, v2, v3, v4, v5) : CST.not_nullable_type) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_anon_choice_user_type_83255f6 env v2 in
  let v3 = (* "&" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_anon_choice_user_type_83255f6 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_nullable_type (env : env) ((v1, v2) : CST.nullable_type) =
  let v1 =
    (match v1 with
    | `Type_ref x -> R.Case ("Type_ref",
        map_type_reference env x
      )
    | `Paren_type x -> R.Case ("Paren_type",
        map_parenthesized_type env x
      )
    )
  in
  let v2 = R.List (List.map (token env (* "?" *)) v2) in
  R.Tuple [v1; v2]

and map_object_literal (env : env) (x : CST.object_literal) =
  (match x with
  | `Opt_data_obj_opt_COLON_dele_specis_class_body (v1, v2, v3, v4) -> R.Case ("Opt_data_obj_opt_COLON_dele_specis_class_body",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "data" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "object" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_delegation_specifiers env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = map_class_body env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Opt_data_obj_opt_COLON_dele_specis (v1, v2, v3) -> R.Case ("Opt_data_obj_opt_COLON_dele_specis",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "data" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "object" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_delegation_specifiers env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_parameter (env : env) ((v1, v2, v3) : CST.parameter) =
  let v1 = map_simple_identifier env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_parameter_modifiers (env : env) (xs : CST.parameter_modifiers) =
  R.List (List.map (fun x ->
    (match x with
    | `Anno x -> R.Case ("Anno",
        map_annotation env x
      )
    | `Param_modi x -> R.Case ("Param_modi",
        map_parameter_modifier env x
      )
    )
  ) xs)

and map_parameter_with_optional_type (env : env) ((v1, v2, v3) : CST.parameter_with_optional_type) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_parameter_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_simple_identifier env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_type (env : env) ((v1, v2, v3) : CST.parenthesized_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_postfix_unary_suffix (env : env) (x : CST.postfix_unary_suffix) =
  (match x with
  | `Post_un_op x -> R.Case ("Post_un_op",
      map_postfix_unary_operator env x
    )
  | `Navi_suffix x -> R.Case ("Navi_suffix",
      map_navigation_suffix env x
    )
  | `Inde_suffix x -> R.Case ("Inde_suffix",
      map_indexing_suffix env x
    )
  )

and map_prefix_expression (env : env) (x : CST.prefix_expression) =
  (match x with
  | `Prefix_un_op_exp (v1, v2) -> R.Case ("Prefix_un_op_exp",
      let v1 = map_prefix_unary_operator env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_anno_exp (v1, v2) -> R.Case ("Choice_anno_exp",
      let v1 =
        (match v1 with
        | `Anno x -> R.Case ("Anno",
            map_annotation env x
          )
        | `Label tok -> R.Case ("Label",
            (* label *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_primary_constructor (env : env) ((v1, v2) : CST.primary_constructor) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_modifiers env x
            ))
          | None -> R.Option None)
        in
        let v2 = (* primary_constructor_keyword *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 = map_class_parameters env v2 in
  R.Tuple [v1; v2]

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Simple_id x -> R.Case ("Simple_id",
      map_simple_identifier env x
    )
  | `Lit_cst x -> R.Case ("Lit_cst",
      map_literal_constant env x
    )
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Call_ref x -> R.Case ("Call_ref",
      map_callable_reference env x
    )
  | `Call_exp (v1, v2) -> R.Case ("Call_exp",
      let v1 = map_expression env v1 in
      let v2 = map_call_suffix env v2 in
      R.Tuple [v1; v2]
    )
  | `Gene_call_exp (v1, v2) -> R.Case ("Gene_call_exp",
      let v1 = map_expression env v1 in
      let v2 = map_generic_call_suffix env v2 in
      R.Tuple [v1; v2]
    )
  | `Func_lit x -> R.Case ("Func_lit",
      map_function_literal env x
    )
  | `Obj_lit x -> R.Case ("Obj_lit",
      map_object_literal env x
    )
  | `Coll_lit x -> R.Case ("Coll_lit",
      map_collection_literal env x
    )
  | `This_exp x -> R.Case ("This_exp",
      map_this_expression env x
    )
  | `Super_exp x -> R.Case ("Super_exp",
      map_super_expression env x
    )
  | `If_exp x -> R.Case ("If_exp",
      map_if_expression env x
    )
  | `When_exp x -> R.Case ("When_exp",
      map_when_expression env x
    )
  | `Try_exp x -> R.Case ("Try_exp",
      map_try_expression env x
    )
  | `Jump_exp x -> R.Case ("Jump_exp",
      map_jump_expression env x
    )
  )

and map_primary_expression_no_trailing_lambda (env : env) (x : CST.primary_expression_no_trailing_lambda) =
  (match x with
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Simple_id x -> R.Case ("Simple_id",
      map_simple_identifier env x
    )
  | `Lit_cst x -> R.Case ("Lit_cst",
      map_literal_constant env x
    )
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Call_ref x -> R.Case ("Call_ref",
      map_callable_reference env x
    )
  | `Call_exp_no_trai_lambda (v1, v2) -> R.Case ("Call_exp_no_trai_lambda",
      let v1 = map_expression_no_trailing_lambda env v1 in
      let v2 = map_call_suffix_no_trailing_lambda env v2 in
      R.Tuple [v1; v2]
    )
  | `Gene_call_exp_no_trai_lambda (v1, v2) -> R.Case ("Gene_call_exp_no_trai_lambda",
      let v1 = map_expression_no_trailing_lambda env v1 in
      let v2 =
        map_generic_call_suffix_no_trailing_lambda env v2
      in
      R.Tuple [v1; v2]
    )
  | `Func_lit x -> R.Case ("Func_lit",
      map_function_literal env x
    )
  | `Obj_lit x -> R.Case ("Obj_lit",
      map_object_literal env x
    )
  | `Coll_lit x -> R.Case ("Coll_lit",
      map_collection_literal env x
    )
  | `This_exp x -> R.Case ("This_exp",
      map_this_expression env x
    )
  | `Super_exp x -> R.Case ("Super_exp",
      map_super_expression env x
    )
  | `If_exp x -> R.Case ("If_exp",
      map_if_expression env x
    )
  | `When_exp x -> R.Case ("When_exp",
      map_when_expression env x
    )
  | `Try_exp x -> R.Case ("Try_exp",
      map_try_expression env x
    )
  | `Jump_exp x -> R.Case ("Jump_exp",
      map_jump_expression env x
    )
  )

and map_property_delegate (env : env) ((v1, v2) : CST.property_delegate) =
  let v1 = (* "by" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_range_test (env : env) ((v1, v2, v3) : CST.range_test) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "!" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "in" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_receiver_type (env : env) ((v1, v2) : CST.receiver_type) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Paren_type x -> R.Case ("Paren_type",
        map_parenthesized_type env x
      )
    | `Null_type x -> R.Case ("Null_type",
        map_nullable_type env x
      )
    | `Type_ref x -> R.Case ("Type_ref",
        map_type_reference env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_secondary_constructor (env : env) ((v1, v2, v3, v4, v5) : CST.secondary_constructor) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "constructor" *) token env v2 in
  let v3 = map_function_value_parameters env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_constructor_delegation_call env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_block env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_setter (env : env) ((v1, v2, v3) : CST.setter) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "set" *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3, v4, v5) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_parameter_with_optional_type env v2 in
        let v3 = (* ")" *) token env v3 in
        let v4 =
          (match v4 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* ":" *) token env v1 in
              let v2 = map_type_ env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v5 = map_function_body env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_simple_user_type (env : env) ((v1, v2) : CST.simple_user_type) =
  let v1 = map_simple_identifier env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Decl x -> R.Case ("Decl",
      map_declaration env x
    )
  | `Rep_choice_label_choice_assign (v1, v2) -> R.Case ("Rep_choice_label_choice_assign",
      let v1 =
        R.List (List.map (fun x ->
          (match x with
          | `Label tok -> R.Case ("Label",
              (* label *) token env tok
            )
          | `Anno x -> R.Case ("Anno",
              map_annotation env x
            )
          )
        ) v1)
      in
      let v2 =
        (match v2 with
        | `Assign x -> R.Case ("Assign",
            map_assignment env x
          )
        | `Loop_stmt x -> R.Case ("Loop_stmt",
            map_loop_statement env x
          )
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_statements (env : env) ((v1, v2, v3) : CST.statements) =
  let v1 = map_statement env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* automatic_semicolon *) token env v1 in
      let v2 = map_statement env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* automatic_semicolon *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) =
  let v1 = (* string_start *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Str_content tok -> R.Case ("Str_content",
          (* string_content *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      )
    ) v2)
  in
  let v3 = (* string_end *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_super_at (env : env) (x : CST.super_at) =
  (match x with
  | `Supe_lexi_id (v1, v2) -> R.Case ("Supe_lexi_id",
      let v1 = (* "super@" *) token env v1 in
      let v2 = map_lexical_identifier env v2 in
      R.Tuple [v1; v2]
    )
  | `Super_LT_type_GT_imm_tok_at_lexi_id (v1, v2, v3, v4, v5, v6) -> R.Case ("Super_LT_type_GT_imm_tok_at_lexi_id",
      let v1 = (* "super" *) token env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_type_ env v3 in
      let v4 = (* ">" *) token env v4 in
      let v5 = map_imm_tok_at env v5 in
      let v6 = map_lexical_identifier env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_super_expression (env : env) (x : CST.super_expression) =
  (match x with
  | `Super tok -> R.Case ("Super",
      (* "super" *) token env tok
    )
  | `Super_LT_type_GT (v1, v2, v3, v4) -> R.Case ("Super_LT_type_GT",
      let v1 = (* "super" *) token env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_type_ env v3 in
      let v4 = (* ">" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Super_at x -> R.Case ("Super_at",
      map_super_at env x
    )
  )

and map_try_expression (env : env) ((v1, v2, v3) : CST.try_expression) =
  let v1 = (* "try" *) token env v1 in
  let v2 = map_block env v2 in
  let v3 =
    (match v3 with
    | `Rep1_catch_blk_opt_fina_blk (v1, v2) -> R.Case ("Rep1_catch_blk_opt_fina_blk",
        let v1 = R.List (List.map (map_catch_block env) v1) in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_finally_block env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `Fina_blk x -> R.Case ("Fina_blk",
        map_finally_block env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_type_ (env : env) ((v1, v2) : CST.type_) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Func_type x -> R.Case ("Func_type",
        map_function_type env x
      )
    | `Paren_type x -> R.Case ("Paren_type",
        map_parenthesized_type env x
      )
    | `Null_type x -> R.Case ("Null_type",
        map_nullable_type env x
      )
    | `Type_ref x -> R.Case ("Type_ref",
        map_type_reference env x
      )
    | `Not_null_type x -> R.Case ("Not_null_type",
        map_not_nullable_type env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_projection env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_projection env v2 in
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

and map_type_constraint (env : env) ((v1, v2, v3, v4) : CST.type_constraint) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 = map_simple_identifier env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type_ env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_type_constraints (env : env) ((v1, v2, v3) : CST.type_constraints) =
  let v1 = (* "where" *) token env v1 in
  let v2 = map_type_constraint env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_constraint env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_type_modifier (env : env) (x : CST.type_modifier) =
  (match x with
  | `Anno x -> R.Case ("Anno",
      map_annotation env x
    )
  | `Susp tok -> R.Case ("Susp",
      (* "suspend" *) token env tok
    )
  )

and map_type_modifiers (env : env) (xs : CST.type_modifiers) =
  R.List (List.map (map_type_modifier env) xs)

and map_type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_parameter_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_simple_identifier env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_type_parameter_modifier (env : env) (x : CST.type_parameter_modifier) =
  (match x with
  | `Reif_modi tok -> R.Case ("Reif_modi",
      (* "reified" *) token env tok
    )
  | `Vari_modi x -> R.Case ("Vari_modi",
      map_type_projection_modifier env x
    )
  | `Anno x -> R.Case ("Anno",
      map_annotation env x
    )
  )

and map_type_parameter_modifiers (env : env) (xs : CST.type_parameter_modifiers) =
  R.List (List.map (map_type_parameter_modifier env) xs)

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter env v2 in
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

and map_type_projection (env : env) (x : CST.type_projection) =
  (match x with
  | `Opt_type_proj_modifs_type (v1, v2) -> R.Case ("Opt_type_proj_modifs_type",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_type_projection_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  )

and map_type_reference (env : env) (x : CST.type_reference) =
  (match x with
  | `User_type x -> R.Case ("User_type",
      map_user_type env x
    )
  | `Dyna tok -> R.Case ("Dyna",
      (* "dynamic" *) token env tok
    )
  )

and map_type_test (env : env) ((v1, v2, v3) : CST.type_test) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "!" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "is" *) token env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `Post_exp (v1, v2) -> R.Case ("Post_exp",
      let v1 = map_expression env v1 in
      let v2 = map_postfix_unary_operator env v2 in
      R.Tuple [v1; v2]
    )
  | `Inde_exp (v1, v2) -> R.Case ("Inde_exp",
      let v1 = map_expression env v1 in
      let v2 = map_indexing_suffix env v2 in
      R.Tuple [v1; v2]
    )
  | `Navi_exp (v1, v2) -> R.Case ("Navi_exp",
      let v1 = map_expression env v1 in
      let v2 = map_navigation_suffix env v2 in
      R.Tuple [v1; v2]
    )
  | `Prefix_exp x -> R.Case ("Prefix_exp",
      map_prefix_expression env x
    )
  | `As_exp (v1, v2, v3) -> R.Case ("As_exp",
      let v1 = map_expression env v1 in
      let v2 = map_as_operator env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Spread_exp (v1, v2) -> R.Case ("Spread_exp",
      let v1 = (* "*" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_unescaped_annotation (env : env) (x : CST.unescaped_annotation) =
  (match x with
  | `Cons_invo x -> R.Case ("Cons_invo",
      map_constructor_invocation env x
    )
  | `User_type x -> R.Case ("User_type",
      map_user_type env x
    )
  )

and map_user_type (env : env) ((v1, v2) : CST.user_type) =
  let v1 = map_simple_user_type env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 = map_simple_user_type env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_value_argument (env : env) ((v1, v2, v3, v4) : CST.value_argument) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_annotation env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_simple_identifier env v1 in
        let v2 = (* "=" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "*" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_expression env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_value_arguments (env : env) ((v1, v2, v3) : CST.value_arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_value_argument env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_value_argument env v2 in
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

and map_variable_declaration (env : env) ((v1, v2) : CST.variable_declaration) =
  let v1 = map_simple_identifier env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_when_condition (env : env) (x : CST.when_condition) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Range_test x -> R.Case ("Range_test",
      map_range_test env x
    )
  | `Type_test x -> R.Case ("Type_test",
      map_type_test env x
    )
  )

and map_when_entry (env : env) ((v1, v2, v3, v4) : CST.when_entry) =
  let v1 =
    (match v1 with
    | `When_cond_rep_COMMA_when_cond_opt_COMMA (v1, v2, v3) -> R.Case ("When_cond_rep_COMMA_when_cond_opt_COMMA",
        let v1 = map_when_condition env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_when_condition env v2 in
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
    | `Else tok -> R.Case ("Else",
        (* "else" *) token env tok
      )
    )
  in
  let v2 = (* "->" *) token env v2 in
  let v3 = map_control_structure_body env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* automatic_semicolon *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_when_expression (env : env) ((v1, v2, v3, v4, v5) : CST.when_expression) =
  let v1 = (* "when" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_when_subject env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "{" *) token env v3 in
  let v4 = R.List (List.map (map_when_entry env) v4) in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_when_subject (env : env) ((v1, v2, v3, v4) : CST.when_subject) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 = R.List (List.map (map_annotation env) v1) in
        let v2 = (* "val" *) token env v2 in
        let v3 = map_variable_declaration env v3 in
        let v4 = (* "=" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_expression env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_import_list (env : env) ((v1, v2) : CST.import_list) =
  let v1 = R.List (List.map (map_import_header env) v1) in
  let v2 = (* import_list_delimiter *) token env v2 in
  R.Tuple [v1; v2]

let map_file_annotation (env : env) ((v1, v2, v3, v4, v5) : CST.file_annotation) =
  let v1 = (* "@" *) token env v1 in
  let v2 = (* "file" *) token env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 =
    (match v4 with
    | `LBRACK_rep1_unes_anno_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_rep1_unes_anno_RBRACK",
        let v1 = (* "[" *) token env v1 in
        let v2 =
          R.List (List.map (map_unescaped_annotation env) v2)
        in
        let v3 = (* "]" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `Unes_anno x -> R.Case ("Unes_anno",
        map_unescaped_annotation env x
      )
    )
  in
  let v5 = (* automatic_semicolon *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_source_file (env : env) (x : CST.source_file) =
  (match x with
  | `Opt_sheb_line_rep_file_anno_opt_pack_header_rep_import_list_rep_stmt_semi (v1, v2, v3, v4, v5) -> R.Case ("Opt_sheb_line_rep_file_anno_opt_pack_header_rep_import_list_rep_stmt_semi",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_shebang_line env x
          ))
        | None -> R.Option None)
      in
      let v2 = R.List (List.map (map_file_annotation env) v2) in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_package_header env x
          ))
        | None -> R.Option None)
      in
      let v4 = R.List (List.map (map_import_list env) v4) in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_statement env v1 in
          let v2 = (* automatic_semicolon *) token env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Semg_exp (v1, v2) -> R.Case ("Semg_exp",
      let v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_line_comment (env : env) (tok : CST.line_comment) =
  (* line_comment *) token env tok

let map_multiline_comment (env : env) (tok : CST.multiline_comment) =
  (* multiline_comment *) token env tok

let dump_tree root =
  map_source_file () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Line_comment (_loc, x) -> ("line_comment", "line_comment", map_line_comment env x)
  | `Multiline_comment (_loc, x) -> ("multiline_comment", "multiline_comment", map_multiline_comment env x)

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

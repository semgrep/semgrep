(**
   Boilerplate to be used as a template when mapping the swift CST
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

let map_bang (env : env) (tok : CST.bang) =
  (* bang *) token env tok

let map_raw_str_continuing_indicator (env : env) (tok : CST.raw_str_continuing_indicator) =
  (* raw_str_continuing_indicator *) token env tok

let map_property_modifier (env : env) (x : CST.property_modifier) =
  (match x with
  | `Static tok -> R.Case ("Static",
      (* "static" *) token env tok
    )
  | `Dyna tok -> R.Case ("Dyna",
      (* "dynamic" *) token env tok
    )
  | `Opt tok -> R.Case ("Opt",
      (* "optional" *) token env tok
    )
  | `Class tok -> R.Case ("Class",
      (* "class" *) token env tok
    )
  )

let map_function_modifier (env : env) (x : CST.function_modifier) =
  (match x with
  | `Infix tok -> R.Case ("Infix",
      (* "infix" *) token env tok
    )
  | `Post tok -> R.Case ("Post",
      (* "postfix" *) token env tok
    )
  | `Prefix tok -> R.Case ("Prefix",
      (* "prefix" *) token env tok
    )
  )

let map_line_str_text (env : env) (tok : CST.line_str_text) =
  (* pattern "[^\\\\\"]+" *) token env tok

let map_async_modifier (env : env) (tok : CST.async_modifier) =
  (* async_modifier *) token env tok

let map_raw_str_part (env : env) (tok : CST.raw_str_part) =
  (* raw_str_part *) token env tok

let map_oct_literal (env : env) (tok : CST.oct_literal) =
  (* oct_literal *) token env tok

let map_tok_dollar_pat_88eeeaa (env : env) (tok : CST.tok_dollar_pat_88eeeaa) =
  (* tok_dollar_pat_88eeeaa *) token env tok

let map_pat_27d7db1 (env : env) (tok : CST.pat_27d7db1) =
  (* pattern (\/[^#]|[^/])*?\n\/# *) token env tok

let map_custom_operator_ (env : env) (tok : CST.custom_operator_) =
  (* custom_operator_ *) token env tok

let map_multi_line_str_text (env : env) (tok : CST.multi_line_str_text) =
  (* pattern "[^\\\\\"]+" *) token env tok

let map_plus_then_ws (env : env) (tok : CST.plus_then_ws) =
  (* plus_then_ws *) token env tok

let map_nil_coalescing_operator_custom (env : env) (tok : CST.nil_coalescing_operator_custom) =
  (* nil_coalescing_operator_custom *) token env tok

let map_semgrep_ellipsis_metavar (env : env) (tok : CST.semgrep_ellipsis_metavar) =
  (* pattern \$\.\.\.[a-zA-Z_][a-zA-Z_0-9]* *) token env tok

let map_catch_keyword (env : env) (tok : CST.catch_keyword) =
  (* catch_keyword *) token env tok

let map_arrow_operator_custom (env : env) (tok : CST.arrow_operator_custom) =
  (* arrow_operator_custom *) token env tok

let map_tok_pat_c201ddc (env : env) (tok : CST.tok_pat_c201ddc) =
  (* tok_pat_c201ddc *) token env tok

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_pat_c6c5536 (env : env) (tok : CST.pat_c6c5536) =
  (* pattern #\/\n *) token env tok

let map_raw_str_end_part (env : env) (tok : CST.raw_str_end_part) =
  (* raw_str_end_part *) token env tok

let map_disjunction_operator_custom (env : env) (tok : CST.disjunction_operator_custom) =
  (* disjunction_operator_custom *) token env tok

let map_implicit_semi (env : env) (tok : CST.implicit_semi) =
  (* implicit_semi *) token env tok

let map_optionally_valueful_control_keyword (env : env) (x : CST.optionally_valueful_control_keyword) =
  (match x with
  | `Ret tok -> R.Case ("Ret",
      (* "return" *) token env tok
    )
  | `Cont tok -> R.Case ("Cont",
      (* "continue" *) token env tok
    )
  | `Brk tok -> R.Case ("Brk",
      (* "break" *) token env tok
    )
  | `Yield tok -> R.Case ("Yield",
      (* "yield" *) token env tok
    )
  )

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
  | `EQ tok -> R.Case ("EQ",
      (* "=" *) token env tok
    )
  )

let map_tok_prec_n4_slash (env : env) (tok : CST.tok_prec_n4_slash) =
  (* tok_prec_n4_slash *) token env tok

let map_async_keyword_custom (env : env) (tok : CST.async_keyword_custom) =
  (* async_keyword_custom *) token env tok

let map_special_literal (env : env) (x : CST.special_literal) =
  (match x with
  | `HASH_36725ee tok -> R.Case ("HASH_36725ee",
      (* "#file" *) token env tok
    )
  | `HASH_ee0b998 tok -> R.Case ("HASH_ee0b998",
      (* "#fileID" *) token env tok
    )
  | `HASH_bd759bd tok -> R.Case ("HASH_bd759bd",
      (* "#filePath" *) token env tok
    )
  | `HASH_709af6a tok -> R.Case ("HASH_709af6a",
      (* "#line" *) token env tok
    )
  | `HASH_be35129 tok -> R.Case ("HASH_be35129",
      (* "#column" *) token env tok
    )
  | `HASH_96a7ced tok -> R.Case ("HASH_96a7ced",
      (* "#function" *) token env tok
    )
  | `HASH_4d47dbe tok -> R.Case ("HASH_4d47dbe",
      (* "#dsohandle" *) token env tok
    )
  )

let map_bin_literal (env : env) (tok : CST.bin_literal) =
  (* bin_literal *) token env tok

let map_default_keyword (env : env) (tok : CST.default_keyword) =
  (* default_keyword *) token env tok

let map_pat_888b548 (env : env) (tok : CST.pat_888b548) =
  (* pattern \{[0-9a-fA-F]+\} *) token env tok

let map_else_ (env : env) (tok : CST.else_) =
  (* else *) token env tok

let map_imm_tok_bang (env : env) (tok : CST.imm_tok_bang) =
  (* "!" *) token env tok

let map_throws_keyword (env : env) (tok : CST.throws_keyword) =
  (* throws_keyword *) token env tok

let map_raw_str_interpolation_start (env : env) (tok : CST.raw_str_interpolation_start) =
  (* pattern \\#*\( *) token env tok

let map_integer_literal (env : env) (tok : CST.integer_literal) =
  (* integer_literal *) token env tok

let map_inheritance_modifier (env : env) (x : CST.inheritance_modifier) =
  (match x with
  | `Final tok -> R.Case ("Final",
      (* "final" *) token env tok
    )
  )

let map_as_bang_custom (env : env) (tok : CST.as_bang_custom) =
  (* as_bang_custom *) token env tok

let map_value_binding_pattern (env : env) (x : CST.value_binding_pattern) =
  (match x with
  | `Var tok -> R.Case ("Var",
      (* "var" *) token env tok
    )
  | `Let tok -> R.Case ("Let",
      (* "let" *) token env tok
    )
  )

let map_where_keyword (env : env) (tok : CST.where_keyword) =
  (* where_keyword *) token env tok

let map_try_operator (env : env) (x : CST.try_operator) =
  (match x with
  | `Try tok -> R.Case ("Try",
      (* "try" *) token env tok
    )
  | `TryB tok -> R.Case ("TryB",
      (* "try!" *) token env tok
    )
  | `TryQ tok -> R.Case ("TryQ",
      (* "try?" *) token env tok
    )
  )

let map_dot_custom (env : env) (tok : CST.dot_custom) =
  (* dot_custom *) token env tok

let map_parameter_modifier (env : env) (x : CST.parameter_modifier) =
  (match x with
  | `Inout tok -> R.Case ("Inout",
      (* "inout" *) token env tok
    )
  | `ATes tok -> R.Case ("ATes",
      (* "@escaping" *) token env tok
    )
  | `ATau tok -> R.Case ("ATau",
      (* "@autoclosure" *) token env tok
    )
  )

let map_import_kind (env : env) (x : CST.import_kind) =
  (match x with
  | `Typeas tok -> R.Case ("Typeas",
      (* "typealias" *) token env tok
    )
  | `Struct tok -> R.Case ("Struct",
      (* "struct" *) token env tok
    )
  | `Class tok -> R.Case ("Class",
      (* "class" *) token env tok
    )
  | `Enum tok -> R.Case ("Enum",
      (* "enum" *) token env tok
    )
  | `Prot tok -> R.Case ("Prot",
      (* "protocol" *) token env tok
    )
  | `Let tok -> R.Case ("Let",
      (* "let" *) token env tok
    )
  | `Var tok -> R.Case ("Var",
      (* "var" *) token env tok
    )
  | `Func tok -> R.Case ("Func",
      (* "func" *) token env tok
    )
  )

let map_real_literal (env : env) (tok : CST.real_literal) =
  (* real_literal *) token env tok

let map_mutation_modifier (env : env) (x : CST.mutation_modifier) =
  (match x with
  | `Muta tok -> R.Case ("Muta",
      (* "mutating" *) token env tok
    )
  | `Nonm tok -> R.Case ("Nonm",
      (* "nonmutating" *) token env tok
    )
  )

let map_extended_regex_literal (env : env) (tok : CST.extended_regex_literal) =
  (* pattern #\/((\/[^#])|[^\n])+\/# *) token env tok

let map_multiline_comment (env : env) (tok : CST.multiline_comment) =
  (* multiline_comment *) token env tok

let map_conjunction_operator_custom (env : env) (tok : CST.conjunction_operator_custom) =
  (* conjunction_operator_custom *) token env tok

let map_hex_literal (env : env) (tok : CST.hex_literal) =
  (* hex_literal *) token env tok

let map_member_modifier (env : env) (x : CST.member_modifier) =
  (match x with
  | `Over tok -> R.Case ("Over",
      (* "override" *) token env tok
    )
  | `Conv tok -> R.Case ("Conv",
      (* "convenience" *) token env tok
    )
  | `Requ tok -> R.Case ("Requ",
      (* "required" *) token env tok
    )
  | `Noni tok -> R.Case ("Noni",
      (* "nonisolated" *) token env tok
    )
  )

let map_explicit_semi (env : env) (tok : CST.explicit_semi) =
  (* explicit_semi *) token env tok

let map_pat_88eeeaa (env : env) (tok : CST.pat_88eeeaa) =
  (* pattern [_\p{XID_Start}\p{Emoji}&&[^0-9#*]](\p{EMod}|\x{FE0F}\x{20E3}?)?([_\p{XID_Continue}\p{Emoji}\x{200D}](\p{EMod}|\x{FE0F}\x{20E3}?)?)* *) token env tok

let map_multiline_comment_explicit (env : env) (() : CST.multiline_comment_explicit) =
  R.Tuple []

let map_pat_c332828 (env : env) (tok : CST.pat_c332828) =
  (* pattern \$[0-9]+ *) token env tok

let map_eq_custom (env : env) (tok : CST.eq_custom) =
  (* eq_custom *) token env tok

let map_pat_f630af3 (env : env) (tok : CST.pat_f630af3) =
  (* pattern [^\r\n]* *) token env tok

let map_pat_97d645c (env : env) (tok : CST.pat_97d645c) =
  (* pattern `[^\r\n` ]*` *) token env tok

let map_rethrows_keyword (env : env) (tok : CST.rethrows_keyword) =
  (* rethrows_keyword *) token env tok

let map_as_quest_custom (env : env) (tok : CST.as_quest_custom) =
  (* as_quest_custom *) token env tok

let map_escaped_identifier (env : env) (tok : CST.escaped_identifier) =
  (* pattern "\\\\[0\\\\tnr\"'\\n]" *) token env tok

let map_minus_then_ws (env : env) (tok : CST.minus_then_ws) =
  (* minus_then_ws *) token env tok

let map_oneline_regex_literal (env : env) (tok : CST.oneline_regex_literal) =
  (* oneline_regex_literal *) token env tok

let map_as_custom (env : env) (tok : CST.as_custom) =
  (* as_custom *) token env tok

let map_statement_label (env : env) (tok : CST.statement_label) =
  (* statement_label *) token env tok

let map_eq_eq_custom (env : env) (tok : CST.eq_eq_custom) =
  (* eq_eq_custom *) token env tok

let map_ownership_modifier (env : env) (x : CST.ownership_modifier) =
  (match x with
  | `Weak tok -> R.Case ("Weak",
      (* "weak" *) token env tok
    )
  | `Unow_7c8c304 tok -> R.Case ("Unow_7c8c304",
      (* "unowned" *) token env tok
    )
  | `Unow_e455cde tok -> R.Case ("Unow_e455cde",
      (* "unowned(safe)" *) token env tok
    )
  | `Unow_8fda70e tok -> R.Case ("Unow_8fda70e",
      (* "unowned(unsafe)" *) token env tok
    )
  )

let map_bitwise_binary_operator (env : env) (x : CST.bitwise_binary_operator) =
  (match x with
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  | `BAR tok -> R.Case ("BAR",
      (* "|" *) token env tok
    )
  | `HAT tok -> R.Case ("HAT",
      (* "^" *) token env tok
    )
  | `LTLT tok -> R.Case ("LTLT",
      (* "<<" *) token env tok
    )
  | `GTGT tok -> R.Case ("GTGT",
      (* ">>" *) token env tok
    )
  )

let map_postfix_unary_operator (env : env) (x : CST.postfix_unary_operator) =
  (match x with
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  | `Bang tok -> R.Case ("Bang",
      (* bang *) token env tok
    )
  )

let map_constructor_function_decl (env : env) ((v1, v2) : CST.constructor_function_decl) =
  let v1 = (* "init" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Quest tok -> R.Case ("Quest",
            (* "?" *) token env tok
          )
        | `Bang tok -> R.Case ("Bang",
            (* bang *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_custom_operator (env : env) (x : CST.custom_operator) =
  (match x with
  | `Tok_pat_c201ddc x -> R.Case ("Tok_pat_c201ddc",
      map_tok_pat_c201ddc env x
    )
  | `Custom_op_ tok -> R.Case ("Custom_op_",
      (* custom_operator_ *) token env tok
    )
  )

let map_multiplicative_operator (env : env) (x : CST.multiplicative_operator) =
  (match x with
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `Tok_prec_n4_slash x -> R.Case ("Tok_prec_n4_slash",
      map_tok_prec_n4_slash env x
    )
  | `PERC tok -> R.Case ("PERC",
      (* "%" *) token env tok
    )
  )

let map_possibly_async_binding_pattern_kind (env : env) ((v1, v2) : CST.possibly_async_binding_pattern_kind) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* async_modifier *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_value_binding_pattern env v2 in
  R.Tuple [v1; v2]

let map_parameter_modifiers (env : env) (xs : CST.parameter_modifiers) =
  R.List (List.map (map_parameter_modifier env) xs)

let map_range_operator (env : env) (x : CST.range_operator) =
  (match x with
  | `Open_ended_range_op tok -> R.Case ("Open_ended_range_op",
      (* "..<" *) token env tok
    )
  | `Three_dot_op tok -> R.Case ("Three_dot_op",
      (* "..." *) token env tok
    )
  )

let map_setter_specifier (env : env) ((v1, v2) : CST.setter_specifier) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_mutation_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "set" *) token env v2 in
  R.Tuple [v1; v2]

let map_modify_specifier (env : env) ((v1, v2) : CST.modify_specifier) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_mutation_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "_modify" *) token env v2 in
  R.Tuple [v1; v2]

let map_non_local_scope_modifier (env : env) (x : CST.non_local_scope_modifier) =
  (match x with
  | `Member_modi x -> R.Case ("Member_modi",
      map_member_modifier env x
    )
  | `Visi_modi (v1, v2) -> R.Case ("Visi_modi",
      let v1 =
        (match v1 with
        | `Public tok -> R.Case ("Public",
            (* "public" *) token env tok
          )
        | `Priv tok -> R.Case ("Priv",
            (* "private" *) token env tok
          )
        | `Inte tok -> R.Case ("Inte",
            (* "internal" *) token env tok
          )
        | `File tok -> R.Case ("File",
            (* "fileprivate" *) token env tok
          )
        | `Open tok -> R.Case ("Open",
            (* "open" *) token env tok
          )
        )
      in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = (* "(" *) token env v1 in
            let v2 = (* "set" *) token env v2 in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Func_modi x -> R.Case ("Func_modi",
      map_function_modifier env x
    )
  | `Muta_modi x -> R.Case ("Muta_modi",
      map_mutation_modifier env x
    )
  | `Prop_modi x -> R.Case ("Prop_modi",
      map_property_modifier env x
    )
  | `Param_modi x -> R.Case ("Param_modi",
      map_parameter_modifier env x
    )
  )

let map_semi (env : env) (x : CST.semi) =
  (match x with
  | `Impl_semi tok -> R.Case ("Impl_semi",
      (* implicit_semi *) token env tok
    )
  | `Expl_semi tok -> R.Case ("Expl_semi",
      (* explicit_semi *) token env tok
    )
  )

let map_shebang_line (env : env) ((v1, v2) : CST.shebang_line) =
  let v1 = (* "#!" *) token env v1 in
  let v2 = map_pat_f630af3 env v2 in
  R.Tuple [v1; v2]

let map_simple_identifier (env : env) (x : CST.simple_identifier) =
  (match x with
  | `Pat_88eeeaa x -> R.Case ("Pat_88eeeaa",
      map_pat_88eeeaa env x
    )
  | `Pat_97d645c x -> R.Case ("Pat_97d645c",
      map_pat_97d645c env x
    )
  | `Pat_c332828 x -> R.Case ("Pat_c332828",
      map_pat_c332828 env x
    )
  | `Tok_dollar_pat_88eeeaa x -> R.Case ("Tok_dollar_pat_88eeeaa",
      map_tok_dollar_pat_88eeeaa env x
    )
  | `Actor tok -> R.Case ("Actor",
      (* "actor" *) token env tok
    )
  | `Lazy tok -> R.Case ("Lazy",
      (* "lazy" *) token env tok
    )
  )

let map_throws (env : env) (x : CST.throws) =
  (match x with
  | `Throws_kw tok -> R.Case ("Throws_kw",
      (* throws_keyword *) token env tok
    )
  | `Rethrs_kw tok -> R.Case ("Rethrs_kw",
      (* rethrows_keyword *) token env tok
    )
  )

let map_additive_operator (env : env) (x : CST.additive_operator) =
  (match x with
  | `Plus_then_ws tok -> R.Case ("Plus_then_ws",
      (* plus_then_ws *) token env tok
    )
  | `Minus_then_ws tok -> R.Case ("Minus_then_ws",
      (* minus_then_ws *) token env tok
    )
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  )

let map_locally_permitted_modifier (env : env) (x : CST.locally_permitted_modifier) =
  (match x with
  | `Owne_modi x -> R.Case ("Owne_modi",
      map_ownership_modifier env x
    )
  | `Inhe_modi x -> R.Case ("Inhe_modi",
      map_inheritance_modifier env x
    )
  | `Prop_beha_modi tok -> R.Case ("Prop_beha_modi",
      (* "lazy" *) token env tok
    )
  )

let map_regex_literal (env : env) (x : CST.regex_literal) =
  (match x with
  | `Exte_regex_lit tok -> R.Case ("Exte_regex_lit",
      (* pattern #\/((\/[^#])|[^\n])+\/# *) token env tok
    )
  | `Mult_regex_lit (v1, v2) -> R.Case ("Mult_regex_lit",
      let v1 = map_pat_c6c5536 env v1 in
      let v2 = map_pat_27d7db1 env v2 in
      R.Tuple [v1; v2]
    )
  | `Onel_regex_lit tok -> R.Case ("Onel_regex_lit",
      (* oneline_regex_literal *) token env tok
    )
  )

let map_str_escaped_char (env : env) (x : CST.str_escaped_char) =
  (match x with
  | `Esca_id tok -> R.Case ("Esca_id",
      (* pattern "\\\\[0\\\\tnr\"'\\n]" *) token env tok
    )
  | `Uni_char_lit (v1, v2, v3) -> R.Case ("Uni_char_lit",
      let v1 = (* "\\" *) token env v1 in
      let v2 = (* "u" *) token env v2 in
      let v3 = map_pat_888b548 env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

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
  | `Bang tok -> R.Case ("Bang",
      (* bang *) token env tok
    )
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  | `Dot tok -> R.Case ("Dot",
      (* dot_custom *) token env tok
    )
  | `Custom_op x -> R.Case ("Custom_op",
      map_custom_operator env x
    )
  )

let map_class_member_separator (env : env) (x : CST.class_member_separator) =
  (match x with
  | `Semi x -> R.Case ("Semi",
      map_semi env x
    )
  | `Mult_comm_expl x -> R.Case ("Mult_comm_expl",
      map_multiline_comment_explicit env x
    )
  )

let map_bound_identifier (env : env) (x : CST.bound_identifier) =
  map_simple_identifier env x

let map_getter_effects (env : env) (xs : CST.getter_effects) =
  R.List (List.map (fun x ->
    (match x with
    | `Async_kw tok -> R.Case ("Async_kw",
        (* async_keyword_custom *) token env tok
      )
    | `Throws x -> R.Case ("Throws",
        map_throws env x
      )
    )
  ) xs)

let map_as_operator (env : env) (x : CST.as_operator) =
  (match x with
  | `As tok -> R.Case ("As",
      (* as_custom *) token env tok
    )
  | `As_quest tok -> R.Case ("As_quest",
      (* as_quest_custom *) token env tok
    )
  | `As_bang tok -> R.Case ("As_bang",
      (* as_bang_custom *) token env tok
    )
  )

let map_equality_operator (env : env) (x : CST.equality_operator) =
  (match x with
  | `BANGEQ tok -> R.Case ("BANGEQ",
      (* "!=" *) token env tok
    )
  | `BANGEQEQ tok -> R.Case ("BANGEQEQ",
      (* "!==" *) token env tok
    )
  | `Eq_eq tok -> R.Case ("Eq_eq",
      (* eq_eq_custom *) token env tok
    )
  | `EQEQEQ tok -> R.Case ("EQEQEQ",
      (* "===" *) token env tok
    )
  )

let map_multi_line_string_content (env : env) (x : CST.multi_line_string_content) =
  (match x with
  | `Multi_line_str_text tok -> R.Case ("Multi_line_str_text",
      (* pattern "[^\\\\\"]+" *) token env tok
    )
  | `Str_esca_char x -> R.Case ("Str_esca_char",
      map_str_escaped_char env x
    )
  | `DQUOT tok -> R.Case ("DQUOT",
      (* "\"" *) token env tok
    )
  )

let map_line_string_content (env : env) (x : CST.line_string_content) =
  (match x with
  | `Line_str_text tok -> R.Case ("Line_str_text",
      (* pattern "[^\\\\\"]+" *) token env tok
    )
  | `Str_esca_char x -> R.Case ("Str_esca_char",
      map_str_escaped_char env x
    )
  )

let map_value_argument_label (env : env) (x : CST.value_argument_label) =
  (match x with
  | `Simple_id x -> R.Case ("Simple_id",
      map_bound_identifier env x
    )
  | `Async tok -> R.Case ("Async",
      (* "async" *) token env tok
    )
  )

let map_identifier (env : env) ((v1, v2) : CST.identifier) =
  let v1 = map_bound_identifier env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* dot_custom *) token env v1 in
      let v2 = map_bound_identifier env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_navigation_suffix (env : env) (x : CST.navigation_suffix) =
  (match x with
  | `Dot_choice_simple_id (v1, v2) -> R.Case ("Dot_choice_simple_id",
      let v1 = (* dot_custom *) token env v1 in
      let v2 =
        (match v2 with
        | `Simple_id x -> R.Case ("Simple_id",
            map_bound_identifier env x
          )
        | `Int_lit tok -> R.Case ("Int_lit",
            (* integer_literal *) token env tok
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Dot_semg_ellips (v1, v2) -> R.Case ("Dot_semg_ellips",
      let v1 = (* dot_custom *) token env v1 in
      let v2 = (* "..." *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_tuple_type_item_identifier (env : env) ((v1, v2, v3) : CST.tuple_type_item_identifier) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "_" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_bound_identifier env v2 in
  let v3 = (* ":" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_precedence_group_attribute (env : env) ((v1, v2, v3) : CST.precedence_group_attribute) =
  let v1 = map_bound_identifier env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 =
    (match v3 with
    | `Simple_id x -> R.Case ("Simple_id",
        map_bound_identifier env x
      )
    | `Bool_lit x -> R.Case ("Bool_lit",
        map_boolean_literal env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

let map_getter_specifier (env : env) ((v1, v2, v3) : CST.getter_specifier) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_mutation_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "get" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_getter_effects env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_referenceable_operator (env : env) (x : CST.referenceable_operator) =
  (match x with
  | `Custom_op x -> R.Case ("Custom_op",
      map_custom_operator env x
    )
  | `Comp_op x -> R.Case ("Comp_op",
      map_comparison_operator env x
    )
  | `Addi_op x -> R.Case ("Addi_op",
      map_additive_operator env x
    )
  | `Mult_op x -> R.Case ("Mult_op",
      map_multiplicative_operator env x
    )
  | `Equa_op x -> R.Case ("Equa_op",
      map_equality_operator env x
    )
  | `Assign_and_op x -> R.Case ("Assign_and_op",
      map_assignment_and_operator env x
    )
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  | `Bang tok -> R.Case ("Bang",
      (* bang *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  | `BAR tok -> R.Case ("BAR",
      (* "|" *) token env tok
    )
  | `HAT tok -> R.Case ("HAT",
      (* "^" *) token env tok
    )
  | `LTLT tok -> R.Case ("LTLT",
      (* "<<" *) token env tok
    )
  | `GTGT tok -> R.Case ("GTGT",
      (* ">>" *) token env tok
    )
  )

let map_availability_argument (env : env) (x : CST.availability_argument) =
  (match x with
  | `Id_int_lit_rep_DOT_int_lit (v1, v2, v3) -> R.Case ("Id_int_lit_rep_DOT_int_lit",
      let v1 = map_identifier env v1 in
      let v2 = (* integer_literal *) token env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "." *) token env v1 in
          let v2 = (* integer_literal *) token env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  )

let map_precedence_group_attributes (env : env) (xs : CST.precedence_group_attributes) =
  R.List (List.map (map_precedence_group_attribute env) xs)

let map_protocol_property_requirements (env : env) ((v1, v2, v3) : CST.protocol_property_requirements) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Getter_spec x -> R.Case ("Getter_spec",
          map_getter_specifier env x
        )
      | `Setter_spec x -> R.Case ("Setter_spec",
          map_setter_specifier env x
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_non_constructor_function_decl (env : env) ((v1, v2) : CST.non_constructor_function_decl) =
  let v1 = (* "func" *) token env v1 in
  let v2 =
    (match v2 with
    | `Simple_id x -> R.Case ("Simple_id",
        map_bound_identifier env x
      )
    | `Refe_op x -> R.Case ("Refe_op",
        map_referenceable_operator env x
      )
    )
  in
  R.Tuple [v1; v2]

let map_precedence_group_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.precedence_group_declaration) =
  let v1 = (* "precedencegroup" *) token env v1 in
  let v2 = map_bound_identifier env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_precedence_group_attributes env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let rec map_annotated_inheritance_specifier (env : env) ((v1, v2) : CST.annotated_inheritance_specifier) =
  let v1 = R.List (List.map (map_attribute env) v1) in
  let v2 = map_inheritance_specifier env v2 in
  R.Tuple [v1; v2]

and map_array_type (env : env) ((v1, v2, v3) : CST.array_type) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_assignment (env : env) ((v1, v2, v3) : CST.assignment) =
  let v1 = map_directly_assignable_expression env v1 in
  let v2 = map_assignment_and_operator env v2 in
  let v3 = map_directly_assignable_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_associatedtype_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.associatedtype_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "associatedtype" *) token env v2 in
  let v3 = map_bound_identifier env v3 in
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
    | Some x -> R.Option (Some (
        map_type_constraints env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* eq_custom *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_attribute (env : env) ((v1, v2, v3) : CST.attribute) =
  let v1 = (* "@" *) token env v1 in
  let v2 = map_user_type env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_attribute_argument env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_attribute_argument env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 = (* ")" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_attribute_argument (env : env) (x : CST.attribute_argument) =
  (match x with
  | `Simple_id_COLON_exp (v1, v2, v3) -> R.Case ("Simple_id_COLON_exp",
      let v1 = map_bound_identifier env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_directly_assignable_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp x -> R.Case ("Exp",
      map_directly_assignable_expression env x
    )
  | `Rep1_simple_id_COLON xs -> R.Case ("Rep1_simple_id_COLON",
      R.List (List.map (fun (v1, v2) ->
        let v1 = map_bound_identifier env v1 in
        let v2 = (* ":" *) token env v2 in
        R.Tuple [v1; v2]
      ) xs)
    )
  | `Rep1_simple_id_int_lit_rep_DOT_int_lit (v1, v2, v3) -> R.Case ("Rep1_simple_id_int_lit_rep_DOT_int_lit",
      let v1 = R.List (List.map (map_bound_identifier env) v1) in
      let v2 = (* integer_literal *) token env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "." *) token env v1 in
          let v2 = (* integer_literal *) token env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_basic_literal (env : env) (x : CST.basic_literal) =
  (match x with
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
    )
  | `Hex_lit tok -> R.Case ("Hex_lit",
      (* hex_literal *) token env tok
    )
  | `Oct_lit tok -> R.Case ("Oct_lit",
      (* oct_literal *) token env tok
    )
  | `Bin_lit tok -> R.Case ("Bin_lit",
      (* bin_literal *) token env tok
    )
  | `Real_lit tok -> R.Case ("Real_lit",
      (* real_literal *) token env tok
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_boolean_literal env x
    )
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Regex_lit x -> R.Case ("Regex_lit",
      map_regex_literal env x
    )
  | `Nil tok -> R.Case ("Nil",
      (* "nil" *) token env tok
    )
  )

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Mult_exp (v1, v2, v3) -> R.Case ("Mult_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = map_multiplicative_operator env v2 in
      let v3 = map_directly_assignable_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Addi_exp (v1, v2, v3) -> R.Case ("Addi_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = map_additive_operator env v2 in
      let v3 = map_directly_assignable_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Range_exp (v1, v2, v3) -> R.Case ("Range_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = map_range_operator env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Infix_exp (v1, v2, v3) -> R.Case ("Infix_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = map_custom_operator env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Nil_coal_exp (v1, v2, v3) -> R.Case ("Nil_coal_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 =
        (* nil_coalescing_operator_custom *) token env v2
      in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Check_exp (v1, v2, v3) -> R.Case ("Check_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = (* "is" *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Equa_exp (v1, v2, v3) -> R.Case ("Equa_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = map_equality_operator env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Comp_exp (v1, v2, v3) -> R.Case ("Comp_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = map_comparison_operator env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Conj_exp (v1, v2, v3) -> R.Case ("Conj_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = (* conjunction_operator_custom *) token env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Disj_exp (v1, v2, v3) -> R.Case ("Disj_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = (* disjunction_operator_custom *) token env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bitw_oper (v1, v2, v3) -> R.Case ("Bitw_oper",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = map_bitwise_binary_operator env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_binding_kind_and_pattern (env : env) ((v1, v2) : CST.binding_kind_and_pattern) =
  let v1 = map_possibly_async_binding_pattern_kind env v1 in
  let v2 = map_no_expr_pattern_already_bound env v2 in
  R.Tuple [v1; v2]

and map_binding_pattern (env : env) ((v1, v2, v3) : CST.binding_pattern) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "case" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_value_binding_pattern env v2 in
  let v3 = map_no_expr_pattern_already_bound env v3 in
  R.Tuple [v1; v2; v3]

and map_binding_pattern_no_expr (env : env) ((v1, v2) : CST.binding_pattern_no_expr) =
  let v1 =
    (match v1 with
    | `Univ_allo_pat x -> R.Case ("Univ_allo_pat",
        map_universally_allowed_pattern env x
      )
    | `Bind_pat x -> R.Case ("Bind_pat",
        map_binding_pattern env x
      )
    | `Bound_id x -> R.Case ("Bound_id",
        map_bound_identifier env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "?" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_binding_pattern_with_expr (env : env) ((v1, v2) : CST.binding_pattern_with_expr) =
  let v1 =
    (match v1 with
    | `Univ_allo_pat x -> R.Case ("Univ_allo_pat",
        map_universally_allowed_pattern env x
      )
    | `Bind_pat x -> R.Case ("Bind_pat",
        map_binding_pattern env x
      )
    | `Exp x -> R.Case ("Exp",
        map_directly_assignable_expression env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "?" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

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

and map_bodyless_function_declaration (env : env) ((v1, v2, v3) : CST.bodyless_function_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "class" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    map_modifierless_function_declaration_no_body env v3
  in
  R.Tuple [v1; v2; v3]

and map_call_expression (env : env) ((v1, v2) : CST.call_expression) =
  let v1 = map_directly_assignable_expression env v1 in
  let v2 = map_call_suffix env v2 in
  R.Tuple [v1; v2]

and map_call_suffix (env : env) (x : CST.call_suffix) =
  (match x with
  | `Value_args x -> R.Case ("Value_args",
      map_expr_hack_at_ternary_binary_call_suffix env x
    )
  | `Fn_call_lambda_args x -> R.Case ("Fn_call_lambda_args",
      map_fn_call_lambda_arguments env x
    )
  | `Value_args_fn_call_lambda_args (v1, v2) -> R.Case ("Value_args_fn_call_lambda_args",
      let v1 =
        map_expr_hack_at_ternary_binary_call_suffix env v1
      in
      let v2 = map_fn_call_lambda_arguments env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_capture_list (env : env) ((v1, v2, v3, v4) : CST.capture_list) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_capture_list_item env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_capture_list_item env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_capture_list_item (env : env) (x : CST.capture_list_item) =
  (match x with
  | `Self_exp tok -> R.Case ("Self_exp",
      (* "self" *) token env tok
    )
  | `Opt_owne_modi_simple_id_opt_equal_sign_exp (v1, v2, v3) -> R.Case ("Opt_owne_modi_simple_id_opt_equal_sign_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_ownership_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_bound_identifier env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* eq_custom *) token env v1 in
            let v2 = map_directly_assignable_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_catch_block (env : env) ((v1, v2, v3, v4) : CST.catch_block) =
  let v1 = (* catch_keyword *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_binding_pattern_no_expr env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_function_body env v4 in
  R.Tuple [v1; v2; v3; v4]

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

and map_class_declaration (env : env) ((v1, v2) : CST.class_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_modifierless_class_declaration env v2 in
  R.Tuple [v1; v2]

and map_class_member_declarations (env : env) ((v1, v2, v3) : CST.class_member_declarations) =
  let v1 = map_type_level_declaration env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_class_member_separator env v1 in
      let v2 = map_type_level_declaration env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_class_member_separator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_computed_getter (env : env) ((v1, v2, v3) : CST.computed_getter) =
  let v1 = R.List (List.map (map_attribute env) v1) in
  let v2 = map_getter_specifier env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_function_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_computed_modify (env : env) ((v1, v2, v3) : CST.computed_modify) =
  let v1 = R.List (List.map (map_attribute env) v1) in
  let v2 = map_modify_specifier env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_function_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_computed_property (env : env) ((v1, v2, v3) : CST.computed_property) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | `Opt_stmts opt -> R.Case ("Opt_stmts",
        (match opt with
        | Some x -> R.Option (Some (
            map_statements env x
          ))
        | None -> R.Option None)
      )
    | `Rep_choice_comp_getter xs -> R.Case ("Rep_choice_comp_getter",
        R.List (List.map (fun x ->
          (match x with
          | `Comp_getter x -> R.Case ("Comp_getter",
              map_computed_getter env x
            )
          | `Comp_setter x -> R.Case ("Comp_setter",
              map_computed_setter env x
            )
          | `Comp_modify x -> R.Case ("Comp_modify",
              map_computed_modify env x
            )
          )
        ) xs)
      )
    )
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_computed_setter (env : env) ((v1, v2, v3, v4) : CST.computed_setter) =
  let v1 = R.List (List.map (map_attribute env) v1) in
  let v2 = map_setter_specifier env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_bound_identifier env v2 in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_function_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_constructor_suffix (env : env) (x : CST.constructor_suffix) =
  (match x with
  | `Cons_value_args x -> R.Case ("Cons_value_args",
      map_constructor_value_arguments env x
    )
  | `Fn_call_lambda_args x -> R.Case ("Fn_call_lambda_args",
      map_fn_call_lambda_arguments env x
    )
  | `Cons_value_args_fn_call_lambda_args (v1, v2) -> R.Case ("Cons_value_args_fn_call_lambda_args",
      let v1 = map_constructor_value_arguments env v1 in
      let v2 = map_fn_call_lambda_arguments env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_constructor_value_arguments (env : env) ((v1, v2, v3) : CST.constructor_value_arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_interpolation_contents env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_control_transfer_statement (env : env) (x : CST.control_transfer_statement) =
  (match x with
  | `Throw_stmt x -> R.Case ("Throw_stmt",
      map_throw_statement env x
    )
  | `Opti_valu_cont_kw_opt_exp (v1, v2) -> R.Case ("Opti_valu_cont_kw_opt_exp",
      let v1 = map_optionally_valueful_control_keyword env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_directly_assignable_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_deinit_declaration (env : env) ((v1, v2, v3) : CST.deinit_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "deinit" *) token env v2 in
  let v3 = map_function_body env v3 in
  R.Tuple [v1; v2; v3]

and map_deprecated_operator_declaration_body (env : env) ((v1, v2, v3) : CST.deprecated_operator_declaration_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Simple_id x -> R.Case ("Simple_id",
          map_bound_identifier env x
        )
      | `Basic_lit x -> R.Case ("Basic_lit",
          map_basic_literal env x
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_dictionary_literal_item (env : env) ((v1, v2, v3) : CST.dictionary_literal_item) =
  let v1 = map_directly_assignable_expression env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_directly_assignable_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_dictionary_type (env : env) ((v1, v2, v3, v4, v5) : CST.dictionary_type) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type_ env v4 in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_direct_or_indirect_binding (env : env) ((v1, v2) : CST.direct_or_indirect_binding) =
  let v1 =
    (match v1 with
    | `Bind_kind_and_pat x -> R.Case ("Bind_kind_and_pat",
        map_binding_kind_and_pattern env x
      )
    | `Case_bind_pat_no_expr (v1, v2) -> R.Case ("Case_bind_pat_no_expr",
        let v1 = (* "case" *) token env v1 in
        let v2 = map_binding_pattern_no_expr env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_annotation env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_directly_assignable_expression (env : env) (x : CST.directly_assignable_expression) =
  map_expression env x

and map_do_statement (env : env) ((v1, v2, v3) : CST.do_statement) =
  let v1 = (* "do" *) token env v1 in
  let v2 = map_function_body env v2 in
  let v3 = R.List (List.map (map_catch_block env) v3) in
  R.Tuple [v1; v2; v3]

and map_else_options (env : env) (x : CST.else_options) =
  (match x with
  | `Blk x -> R.Case ("Blk",
      map_function_body env x
    )
  | `If_stmt x -> R.Case ("If_stmt",
      map_if_statement env x
    )
  )

and map_enum_class_body (env : env) ((v1, v2, v3) : CST.enum_class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Enum_entry x -> R.Case ("Enum_entry",
          map_enum_entry env x
        )
      | `Type_level_decl x -> R.Case ("Type_level_decl",
          map_type_level_declaration env x
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_enum_entry (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.enum_entry) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "indirect" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = (* "case" *) token env v3 in
  let v4 = map_bound_identifier env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_enum_entry_suffix env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_bound_identifier env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_enum_entry_suffix env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    ) v6)
  in
  let v7 =
    (match v7 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_enum_entry_suffix (env : env) (x : CST.enum_entry_suffix) =
  (match x with
  | `Enum_type_params (v1, v2, v3) -> R.Case ("Enum_type_params",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3, v4) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_tuple_type_item_identifier env x
                ))
              | None -> R.Option None)
            in
            let v2 = map_type_ env v2 in
            let v3 =
              (match v3 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = (* eq_custom *) token env v1 in
                  let v2 = map_directly_assignable_expression env v2 in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            let v4 =
              R.List (List.map (fun (v1, v2, v3, v4) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_tuple_type_item_identifier env x
                    ))
                  | None -> R.Option None)
                in
                let v3 = map_type_ env v3 in
                let v4 =
                  (match v4 with
                  | Some (v1, v2) -> R.Option (Some (
                      let v1 = (* eq_custom *) token env v1 in
                      let v2 = map_directly_assignable_expression env v2 in
                      R.Tuple [v1; v2]
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2; v3; v4]
              ) v4)
            in
            R.Tuple [v1; v2; v3; v4]
          ))
        | None -> R.Option None)
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Equal_sign_exp (v1, v2) -> R.Case ("Equal_sign_exp",
      let v1 = (* eq_custom *) token env v1 in
      let v2 = map_directly_assignable_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_expr_hack_at_ternary_binary_call_suffix (env : env) (x : CST.expr_hack_at_ternary_binary_call_suffix) =
  map_value_arguments env x

and map_expr_hack_at_ternary_binary_suffix (env : env) (x : CST.expr_hack_at_ternary_binary_suffix) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_directly_assignable_expression env x
    )
  | `Expr_hack_at_tern_bin_call (v1, v2) -> R.Case ("Expr_hack_at_tern_bin_call",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 =
        map_expr_hack_at_ternary_binary_call_suffix env v2
      in
      R.Tuple [v1; v2]
    )
  )

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_simple_id x -> R.Case ("Choice_simple_id",
      (match x with
      | `Simple_id x -> R.Case ("Simple_id",
          map_bound_identifier env x
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
      | `Prim_exp x -> R.Case ("Prim_exp",
          map_primary_expression env x
        )
      | `Assign x -> R.Case ("Assign",
          map_assignment env x
        )
      | `Exp_imme_quest (v1, v2) -> R.Case ("Exp_imme_quest",
          let v1 = map_directly_assignable_expression env v1 in
          let v2 = (* "?" *) token env v2 in
          R.Tuple [v1; v2]
        )
      | `Async tok -> R.Case ("Async",
          (* "async" *) token env tok
        )
      )
    )
  | `Semg_exp_ellips tok -> R.Case ("Semg_exp_ellips",
      (* "..." *) token env tok
    )
  | `Semg_ellips_meta tok -> R.Case ("Semg_ellips_meta",
      (* pattern \$\.\.\.[a-zA-Z_][a-zA-Z_0-9]* *) token env tok
    )
  | `Semg_deep_ellips (v1, v2, v3) -> R.Case ("Semg_deep_ellips",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_directly_assignable_expression env v2 in
      let v3 = map_custom_operator env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_fn_call_lambda_arguments (env : env) ((v1, v2) : CST.fn_call_lambda_arguments) =
  let v1 = map_lambda_literal env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = map_bound_identifier env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_lambda_literal env v3 in
      R.Tuple [v1; v2; v3]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_for_statement (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.for_statement) =
  let v1 = (* "for" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_try_operator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "await" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_binding_pattern_no_expr env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_type_annotation env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* "in" *) token env v6 in
  let v7 = map_directly_assignable_expression env v7 in
  let v8 =
    (match v8 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v9 = map_function_body env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

and map_function_body (env : env) (x : CST.function_body) =
  map_block env x

and map_function_declaration (env : env) ((v1, v2) : CST.function_declaration) =
  let v1 = map_bodyless_function_declaration env v1 in
  let v2 = map_function_body env v2 in
  R.Tuple [v1; v2]

and map_function_type (env : env) ((v1, v2, v3, v4, v5) : CST.function_type) =
  let v1 =
    (match v1 with
    | `Tuple_type x -> R.Case ("Tuple_type",
        map_tuple_type env x
      )
    | `Unan_type x -> R.Case ("Unan_type",
        map_unannotated_type env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* async_keyword_custom *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_throws env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* arrow_operator_custom *) token env v4 in
  let v5 = map_type_ env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_function_value_parameter (env : env) ((v1, v2, v3) : CST.function_value_parameter) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_parameter env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* eq_custom *) token env v1 in
        let v2 = map_directly_assignable_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_function_value_parameters (env : env) (xs : CST.function_value_parameters) =
  R.List (List.map (fun (v1, v2, v3) ->
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
    let v3 = (* ")" *) token env v3 in
    R.Tuple [v1; v2; v3]
  ) xs)

and map_guard_statement (env : env) ((v1, v2, v3, v4, v5) : CST.guard_statement) =
  let v1 = (* "guard" *) token env v1 in
  let v2 = map_if_condition_sequence_item env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_if_condition_sequence_item env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* else *) token env v4 in
  let v5 = map_function_body env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_if_condition_sequence_item (env : env) (x : CST.if_condition_sequence_item) =
  (match x with
  | `If_let_bind (v1, v2, v3) -> R.Case ("If_let_bind",
      let v1 = map_direct_or_indirect_binding env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* eq_custom *) token env v1 in
            let v2 = map_directly_assignable_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_where_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Exp x -> R.Case ("Exp",
      map_directly_assignable_expression env x
    )
  | `Avai_cond (v1, v2, v3, v4, v5) -> R.Case ("Avai_cond",
      let v1 = (* "#available" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_availability_argument env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_availability_argument env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_if_statement (env : env) ((v1, v2, v3, v4, v5) : CST.if_statement) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_if_condition_sequence_item env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_if_condition_sequence_item env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = map_function_body env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* else *) token env v1 in
        let v2 = map_else_options env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_import_declaration (env : env) ((v1, v2, v3, v4) : CST.import_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "import" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_import_kind env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_identifier env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_inheritance_specifier (env : env) (x : CST.inheritance_specifier) =
  (match x with
  | `User_type x -> R.Case ("User_type",
      map_user_type env x
    )
  | `Func_type x -> R.Case ("Func_type",
      map_function_type env x
    )
  )

and map_inheritance_specifiers (env : env) ((v1, v2) : CST.inheritance_specifiers) =
  let v1 = map_annotated_inheritance_specifier env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `COMMA tok -> R.Case ("COMMA",
            (* "," *) token env tok
          )
        | `AMP tok -> R.Case ("AMP",
            (* "&" *) token env tok
          )
        )
      in
      let v2 = map_annotated_inheritance_specifier env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_interpolation (env : env) ((v1, v2, v3) : CST.interpolation) =
  let v1 = (* "\\(" *) token env v1 in
  let v2 = map_interpolation_contents env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_interpolation_contents (env : env) ((v1, v2) : CST.interpolation_contents) =
  let v1 = map_value_argument env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_value_argument env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_key_path_component (env : env) (x : CST.key_path_component) =
  (match x with
  | `Simple_id_rep_key_path_postfs (v1, v2) -> R.Case ("Simple_id_rep_key_path_postfs",
      let v1 = map_bound_identifier env v1 in
      let v2 =
        R.List (List.map (map_key_path_postfixes env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Rep1_key_path_postfs xs -> R.Case ("Rep1_key_path_postfs",
      R.List (List.map (map_key_path_postfixes env) xs)
    )
  )

and map_key_path_postfixes (env : env) (x : CST.key_path_postfixes) =
  (match x with
  | `QMARK tok -> R.Case ("QMARK",
      (* "?" *) token env tok
    )
  | `Bang tok -> R.Case ("Bang",
      (* bang *) token env tok
    )
  | `Self tok -> R.Case ("Self",
      (* "self" *) token env tok
    )
  | `LBRACK_opt_value_arg_rep_COMMA_value_arg_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_opt_value_arg_rep_COMMA_value_arg_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_interpolation_contents env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_labeled_statement (env : env) ((v1, v2) : CST.labeled_statement) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* statement_label *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `For_stmt x -> R.Case ("For_stmt",
        map_for_statement env x
      )
    | `While_stmt x -> R.Case ("While_stmt",
        map_while_statement env x
      )
    | `Repeat_while_stmt x -> R.Case ("Repeat_while_stmt",
        map_repeat_while_statement env x
      )
    | `Do_stmt x -> R.Case ("Do_stmt",
        map_do_statement env x
      )
    | `If_stmt x -> R.Case ("If_stmt",
        map_if_statement env x
      )
    | `Guard_stmt x -> R.Case ("Guard_stmt",
        map_guard_statement env x
      )
    | `Switch_stmt x -> R.Case ("Switch_stmt",
        map_switch_statement env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_lambda_function_type (env : env) ((v1, v2, v3, v4) : CST.lambda_function_type) =
  let v1 =
    (match v1 with
    | `Lambda_func_type_params x -> R.Case ("Lambda_func_type_params",
        map_lambda_function_type_parameters env x
      )
    | `LPAR_opt_lambda_func_type_params_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_lambda_func_type_params_RPAR",
        let v1 = (* "(" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_lambda_function_type_parameters env x
            ))
          | None -> R.Option None)
        in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* async_keyword_custom *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_throws env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* arrow_operator_custom *) token env v1 in
        let v2 = map_possibly_implicitly_unwrapped_type env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_lambda_function_type_parameters (env : env) ((v1, v2) : CST.lambda_function_type_parameters) =
  let v1 = map_lambda_parameter env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_lambda_parameter env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_lambda_literal (env : env) ((v1, v2, v3, v4) : CST.lambda_literal) =
  let v1 =
    (match v1 with
    | `LCURL tok -> R.Case ("LCURL",
        (* "{" *) token env tok
      )
    | `HATLCURL tok -> R.Case ("HATLCURL",
        (* "^{" *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_lambda_type_declaration env x
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

and map_lambda_parameter (env : env) (v1 : CST.lambda_parameter) =
  (match v1 with
  | `Self_exp tok -> R.Case ("Self_exp",
      (* "self" *) token env tok
    )
  | `Simple_id x -> R.Case ("Simple_id",
      map_bound_identifier env x
    )
  | `Opt_simple_id_simple_id_COLON_opt_param_modifs_poss_impl_unwr_type (v1, v2, v3, v4, v5) -> R.Case ("Opt_simple_id_simple_id_COLON_opt_param_modifs_poss_impl_unwr_type",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_bound_identifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_bound_identifier env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_parameter_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_possibly_implicitly_unwrapped_type env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_lambda_type_declaration (env : env) ((v1, v2, v3, v4) : CST.lambda_type_declaration) =
  let v1 = R.List (List.map (map_attribute env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_capture_list env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_lambda_function_type env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "in" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_local_declaration (env : env) (x : CST.local_declaration) =
  (match x with
  | `Local_prop_decl (v1, v2) -> R.Case ("Local_prop_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_locally_permitted_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_modifierless_property_declaration env v2 in
      R.Tuple [v1; v2]
    )
  | `Local_typeas_decl (v1, v2) -> R.Case ("Local_typeas_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_locally_permitted_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_modifierless_typealias_declaration env v2 in
      R.Tuple [v1; v2]
    )
  | `Local_func_decl (v1, v2) -> R.Case ("Local_func_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_locally_permitted_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_modifierless_function_declaration env v2 in
      R.Tuple [v1; v2]
    )
  | `Local_class_decl (v1, v2) -> R.Case ("Local_class_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_locally_permitted_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_modifierless_class_declaration env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_local_statement (env : env) (x : CST.local_statement) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_directly_assignable_expression env x
    )
  | `Local_decl x -> R.Case ("Local_decl",
      map_local_declaration env x
    )
  | `Labe_stmt x -> R.Case ("Labe_stmt",
      map_labeled_statement env x
    )
  | `Cont_tran_stmt x -> R.Case ("Cont_tran_stmt",
      map_control_transfer_statement env x
    )
  )

and map_locally_permitted_modifiers (env : env) (xs : CST.locally_permitted_modifiers) =
  R.List (List.map (fun x ->
    (match x with
    | `Attr x -> R.Case ("Attr",
        map_attribute env x
      )
    | `Loca_perm_modi x -> R.Case ("Loca_perm_modi",
        map_locally_permitted_modifier env x
      )
    )
  ) xs)

and map_modifierless_class_declaration (env : env) (x : CST.modifierless_class_declaration) =
  (match x with
  | `Choice_class_simple_id_opt_type_params_opt_COLON_inhe_specis_opt_type_consts_class_body (v1, v2, v3, v4, v5, v6) -> R.Case ("Choice_class_simple_id_opt_type_params_opt_COLON_inhe_specis_opt_type_consts_class_body",
      let v1 =
        (match v1 with
        | `Class tok -> R.Case ("Class",
            (* "class" *) token env tok
          )
        | `Struct tok -> R.Case ("Struct",
            (* "struct" *) token env tok
          )
        | `Actor tok -> R.Case ("Actor",
            (* "actor" *) token env tok
          )
        )
      in
      let v2 = map_bound_identifier env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_inheritance_specifiers env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_type_constraints env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_class_body env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Exte_unan_type_opt_type_params_opt_COLON_inhe_specis_opt_type_consts_class_body (v1, v2, v3, v4, v5, v6) -> R.Case ("Exte_unan_type_opt_type_params_opt_COLON_inhe_specis_opt_type_consts_class_body",
      let v1 = (* "extension" *) token env v1 in
      let v2 = map_unannotated_type env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_inheritance_specifiers env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_type_constraints env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_class_body env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Opt_indi_enum_simple_id_opt_type_params_opt_COLON_inhe_specis_opt_type_consts_enum_class_body (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Opt_indi_enum_simple_id_opt_type_params_opt_COLON_inhe_specis_opt_type_consts_enum_class_body",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "indirect" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "enum" *) token env v2 in
      let v3 = map_bound_identifier env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_inheritance_specifiers env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_type_constraints env x
          ))
        | None -> R.Option None)
      in
      let v7 = map_enum_class_body env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  )

and map_modifierless_function_declaration (env : env) ((v1, v2) : CST.modifierless_function_declaration) =
  let v1 =
    map_modifierless_function_declaration_no_body env v1
  in
  let v2 = map_function_body env v2 in
  R.Tuple [v1; v2]

and map_modifierless_function_declaration_no_body (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.modifierless_function_declaration_no_body) =
  let v1 =
    (match v1 with
    | `Cons_func_decl x -> R.Case ("Cons_func_decl",
        map_constructor_function_decl env x
      )
    | `Non_cons_func_decl x -> R.Case ("Non_cons_func_decl",
        map_non_constructor_function_decl env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_function_value_parameters env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* async_keyword_custom *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_throws env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* arrow_operator_custom *) token env v1 in
        let v2 = map_possibly_implicitly_unwrapped_type env v2 in
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
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_modifierless_property_declaration (env : env) ((v1, v2, v3) : CST.modifierless_property_declaration) =
  let v1 = map_possibly_async_binding_pattern_kind env v1 in
  let v2 =
    map_single_modifierless_property_declaration env v2
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        map_single_modifierless_property_declaration env v2
      in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_modifierless_typealias_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.modifierless_typealias_declaration) =
  let v1 = (* "typealias" *) token env v1 in
  let v2 = map_bound_identifier env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* eq_custom *) token env v4 in
  let v5 = map_type_ env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_modifiers (env : env) (xs : CST.modifiers) =
  R.List (List.map (fun x ->
    (match x with
    | `Non_local_scope_modi x -> R.Case ("Non_local_scope_modi",
        map_non_local_scope_modifier env x
      )
    | `Rep1_choice_attr x -> R.Case ("Rep1_choice_attr",
        map_locally_permitted_modifiers env x
      )
    )
  ) xs)

and map_navigable_type_expression (env : env) (x : CST.navigable_type_expression) =
  (match x with
  | `User_type x -> R.Case ("User_type",
      map_user_type env x
    )
  | `Array_type x -> R.Case ("Array_type",
      map_array_type env x
    )
  | `Dict_type x -> R.Case ("Dict_type",
      map_dictionary_type env x
    )
  )

and map_no_expr_pattern_already_bound (env : env) ((v1, v2) : CST.no_expr_pattern_already_bound) =
  let v1 =
    (match v1 with
    | `Univ_allo_pat x -> R.Case ("Univ_allo_pat",
        map_universally_allowed_pattern env x
      )
    | `Bound_id x -> R.Case ("Bound_id",
        map_bound_identifier env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "?" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_operator_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.operator_declaration) =
  let v1 =
    (match v1 with
    | `Prefix tok -> R.Case ("Prefix",
        (* "prefix" *) token env tok
      )
    | `Infix tok -> R.Case ("Infix",
        (* "infix" *) token env tok
      )
    | `Post tok -> R.Case ("Post",
        (* "postfix" *) token env tok
      )
    )
  in
  let v2 = (* "operator" *) token env v2 in
  let v3 = map_referenceable_operator env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_bound_identifier env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_deprecated_operator_declaration_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_parameter (env : env) (x : CST.parameter) =
  (match x with
  | `Opt_simple_id_simple_id_COLON_opt_param_modifs_poss_impl_unwr_type_opt_three_dot_op (v1, v2, v3, v4, v5, v6) -> R.Case ("Opt_simple_id_simple_id_COLON_opt_param_modifs_poss_impl_unwr_type_opt_three_dot_op",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_bound_identifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_bound_identifier env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_parameter_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_possibly_implicitly_unwrapped_type env v5 in
      let v6 =
        (match v6 with
        | Some tok -> R.Option (Some (
            (* "..." *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Semg_ellips_meta tok -> R.Case ("Semg_ellips_meta",
      (* pattern \$\.\.\.[a-zA-Z_][a-zA-Z_0-9]* *) token env tok
    )
  )

and map_possibly_implicitly_unwrapped_type (env : env) ((v1, v2) : CST.possibly_implicitly_unwrapped_type) =
  let v1 = map_type_ env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_imm_tok_bang env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Tuple_exp (v1, v2, v3, v4, v5) -> R.Case ("Tuple_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_bound_identifier env v1 in
            let v2 = (* ":" *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = map_directly_assignable_expression env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2, v3) ->
          let v1 = (* "," *) token env v1 in
          let v2 =
            (match v2 with
            | Some (v1, v2) -> R.Option (Some (
                let v1 = map_bound_identifier env v1 in
                let v2 = (* ":" *) token env v2 in
                R.Tuple [v1; v2]
              ))
            | None -> R.Option None)
          in
          let v3 = map_directly_assignable_expression env v3 in
          R.Tuple [v1; v2; v3]
        ) v4)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Basic_lit x -> R.Case ("Basic_lit",
      map_basic_literal env x
    )
  | `Lambda_lit x -> R.Case ("Lambda_lit",
      map_lambda_literal env x
    )
  | `Spec_lit x -> R.Case ("Spec_lit",
      map_special_literal env x
    )
  | `Play_lit (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Play_lit",
      let v1 =
        (match v1 with
        | `HASH_38ce0d0 tok -> R.Case ("HASH_38ce0d0",
            (* "#colorLiteral" *) token env tok
          )
        | `HASH_34ae46a tok -> R.Case ("HASH_34ae46a",
            (* "#fileLiteral" *) token env tok
          )
        | `HASH_71f9c0e tok -> R.Case ("HASH_71f9c0e",
            (* "#imageLiteral" *) token env tok
          )
        )
      in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_bound_identifier env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_directly_assignable_expression env v5 in
      let v6 =
        R.List (List.map (fun (v1, v2, v3, v4) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_bound_identifier env v2 in
          let v3 = (* ":" *) token env v3 in
          let v4 = map_directly_assignable_expression env v4 in
          R.Tuple [v1; v2; v3; v4]
        ) v6)
      in
      let v7 = (* ")" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Array_lit (v1, v2, v3, v4) -> R.Case ("Array_lit",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_directly_assignable_expression env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_directly_assignable_expression env v2 in
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
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Dict_lit (v1, v2, v3, v4) -> R.Case ("Dict_lit",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | `COLON tok -> R.Case ("COLON",
            (* ":" *) token env tok
          )
        | `Dict_lit_item_rep_COMMA_dict_lit_item (v1, v2) -> R.Case ("Dict_lit_item_rep_COMMA_dict_lit_item",
            let v1 = map_dictionary_literal_item env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_dictionary_literal_item env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          )
        )
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
  | `Self_exp tok -> R.Case ("Self_exp",
      (* "self" *) token env tok
    )
  | `Super_exp v1 -> R.Case ("Super_exp",
      (* "super" *) token env v1
    )
  | `Try_exp (v1, v2) -> R.Case ("Try_exp",
      let v1 = map_try_operator env v1 in
      let v2 =
        (match v2 with
        | `Exp x -> R.Case ("Exp",
            map_directly_assignable_expression env x
          )
        | `Bin_exp x -> R.Case ("Bin_exp",
            map_binary_expression env x
          )
        | `Call_exp x -> R.Case ("Call_exp",
            map_call_expression env x
          )
        | `Tern_exp x -> R.Case ("Tern_exp",
            map_ternary_expression env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Await_exp (v1, v2) -> R.Case ("Await_exp",
      let v1 = (* "await" *) token env v1 in
      let v2 =
        (match v2 with
        | `Exp x -> R.Case ("Exp",
            map_directly_assignable_expression env x
          )
        | `Call_exp x -> R.Case ("Call_exp",
            map_call_expression env x
          )
        | `Tern_exp x -> R.Case ("Tern_exp",
            map_ternary_expression env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Refe_op x -> R.Case ("Refe_op",
      map_referenceable_operator env x
    )
  | `Key_path_exp (v1, v2, v3) -> R.Case ("Key_path_exp",
      let v1 = (* "\\" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Simple_user_type x -> R.Case ("Simple_user_type",
                map_simple_user_type env x
              )
            | `Array_type x -> R.Case ("Array_type",
                map_array_type env x
              )
            | `Dict_type x -> R.Case ("Dict_type",
                map_dictionary_type env x
              )
            )
          ))
        | None -> R.Option None)
      in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "." *) token env v1 in
          let v2 = map_key_path_component env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Key_path_str_exp (v1, v2, v3, v4) -> R.Case ("Key_path_str_exp",
      let v1 = (* "#keyPath" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_directly_assignable_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Three_dot_op tok -> R.Case ("Three_dot_op",
      (* "..." *) token env tok
    )
  )

and map_property_declaration (env : env) ((v1, v2) : CST.property_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_modifierless_property_declaration env v2 in
  R.Tuple [v1; v2]

and map_protocol_body (env : env) ((v1, v2, v3) : CST.protocol_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_protocol_member_declarations env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_protocol_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.protocol_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "protocol" *) token env v2 in
  let v3 = map_bound_identifier env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_inheritance_specifiers env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_type_constraints env x
      ))
    | None -> R.Option None)
  in
  let v7 = map_protocol_body env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_protocol_member_declaration (env : env) (x : CST.protocol_member_declaration) =
  (match x with
  | `Body_func_decl_opt_func_body (v1, v2) -> R.Case ("Body_func_decl_opt_func_body",
      let v1 = map_bodyless_function_declaration env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_function_body env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Deinit_decl x -> R.Case ("Deinit_decl",
      map_deinit_declaration env x
    )
  | `Prot_prop_decl (v1, v2, v3, v4, v5) -> R.Case ("Prot_prop_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_binding_kind_and_pattern env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_annotation env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_constraints env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_protocol_property_requirements env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Typeas_decl x -> R.Case ("Typeas_decl",
      map_typealias_declaration env x
    )
  | `Asso_decl x -> R.Case ("Asso_decl",
      map_associatedtype_declaration env x
    )
  | `Subs_decl x -> R.Case ("Subs_decl",
      map_subscript_declaration env x
    )
  )

and map_protocol_member_declarations (env : env) ((v1, v2, v3) : CST.protocol_member_declarations) =
  let v1 = map_protocol_member_declaration env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_semi env v1 in
      let v2 = map_protocol_member_declaration env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_semi env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_raw_str_interpolation (env : env) ((v1, v2, v3) : CST.raw_str_interpolation) =
  let v1 = (* pattern \\#*\( *) token env v1 in
  let v2 = map_interpolation_contents env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_repeat_while_statement (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.repeat_while_statement) =
  let v1 = (* "repeat" *) token env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  let v5 = (* "while" *) token env v5 in
  let v6 = map_if_condition_sequence_item env v6 in
  let v7 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_if_condition_sequence_item env v2 in
      R.Tuple [v1; v2]
    ) v7)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_simple_user_type (env : env) ((v1, v2) : CST.simple_user_type) =
  let v1 = map_bound_identifier env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_single_modifierless_property_declaration (env : env) ((v1, v2, v3, v4) : CST.single_modifierless_property_declaration) =
  let v1 = map_no_expr_pattern_already_bound env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_annotation env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_constraints env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        (match x with
        | `Equal_sign_exp (v1, v2) -> R.Case ("Equal_sign_exp",
            let v1 = (* eq_custom *) token env v1 in
            let v2 = map_directly_assignable_expression env v2 in
            R.Tuple [v1; v2]
          )
        | `Comp_prop x -> R.Case ("Comp_prop",
            map_computed_property env x
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_statements (env : env) ((v1, v2, v3) : CST.statements) =
  let v1 = map_local_statement env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_semi env v1 in
      let v2 = map_local_statement env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_semi env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_string_literal (env : env) (x : CST.string_literal) =
  (match x with
  | `Line_str_lit (v1, v2, v3) -> R.Case ("Line_str_lit",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Line_str_content x -> R.Case ("Line_str_content",
              map_line_string_content env x
            )
          | `Interp x -> R.Case ("Interp",
              map_interpolation env x
            )
          )
        ) v2)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Multi_line_str_lit (v1, v2, v3) -> R.Case ("Multi_line_str_lit",
      let v1 = (* "\"\"\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Multi_line_str_content x -> R.Case ("Multi_line_str_content",
              map_multi_line_string_content env x
            )
          | `Interp x -> R.Case ("Interp",
              map_interpolation env x
            )
          )
        ) v2)
      in
      let v3 = (* "\"\"\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Raw_str_lit (v1, v2) -> R.Case ("Raw_str_lit",
      let v1 =
        R.List (List.map (fun (v1, v2, v3) ->
          let v1 = (* raw_str_part *) token env v1 in
          let v2 = map_raw_str_interpolation env v2 in
          let v3 =
            (match v3 with
            | Some tok -> R.Option (Some (
                (* raw_str_continuing_indicator *) token env tok
              ))
            | None -> R.Option None)
          in
          R.Tuple [v1; v2; v3]
        ) v1)
      in
      let v2 = (* raw_str_end_part *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_subscript_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.subscript_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "subscript" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_function_value_parameters env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* arrow_operator_custom *) token env v1 in
        let v2 = map_possibly_implicitly_unwrapped_type env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_type_constraints env x
      ))
    | None -> R.Option None)
  in
  let v7 = map_computed_property env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_switch_entry (env : env) ((v1, v2, v3, v4, v5) : CST.switch_entry) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Case_switch_pat_opt_where_kw_exp_rep_COMMA_switch_pat (v1, v2, v3, v4) -> R.Case ("Case_switch_pat_opt_where_kw_exp_rep_COMMA_switch_pat",
        let v1 = (* "case" *) token env v1 in
        let v2 = map_switch_pattern env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_where_clause env x
            ))
          | None -> R.Option None)
        in
        let v4 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_switch_pattern env v2 in
            R.Tuple [v1; v2]
          ) v4)
        in
        R.Tuple [v1; v2; v3; v4]
      )
    | `Defa_kw tok -> R.Case ("Defa_kw",
        (* default_keyword *) token env tok
      )
    )
  in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_statements env v4 in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "fallthrough" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_switch_pattern (env : env) (x : CST.switch_pattern) =
  map_binding_pattern_with_expr env x

and map_switch_statement (env : env) ((v1, v2, v3, v4, v5) : CST.switch_statement) =
  let v1 = (* "switch" *) token env v1 in
  let v2 = map_directly_assignable_expression env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 = R.List (List.map (map_switch_entry env) v4) in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_ternary_expression (env : env) ((v1, v2, v3, v4, v5) : CST.ternary_expression) =
  let v1 = map_directly_assignable_expression env v1 in
  let v2 = (* "?" *) token env v2 in
  let v3 = map_directly_assignable_expression env v3 in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_expr_hack_at_ternary_binary_suffix env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_throw_statement (env : env) ((v1, v2) : CST.throw_statement) =
  let v1 = (* "throw" *) token env v1 in
  let v2 = map_directly_assignable_expression env v2 in
  R.Tuple [v1; v2]

and map_tuple_pattern (env : env) ((v1, v2, v3, v4) : CST.tuple_pattern) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_tuple_pattern_item env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_tuple_pattern_item env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_tuple_pattern_item (env : env) (x : CST.tuple_pattern_item) =
  (match x with
  | `Simple_id_COLON_bind_pat_with_expr (v1, v2, v3) -> R.Case ("Simple_id_COLON_bind_pat_with_expr",
      let v1 = map_bound_identifier env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_switch_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bind_pat_with_expr x -> R.Case ("Bind_pat_with_expr",
      map_switch_pattern env x
    )
  )

and map_tuple_type (env : env) ((v1, v2, v3) : CST.tuple_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_tuple_type_item env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_tuple_type_item env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_tuple_type_item (env : env) ((v1, v2, v3) : CST.tuple_type_item) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_tuple_type_item_identifier env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_parameter_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Opt_type_modifs_unan_type (v1, v2) -> R.Case ("Opt_type_modifs_unan_type",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_type_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_unannotated_type env v2 in
      R.Tuple [v1; v2]
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_type_annotation (env : env) ((v1, v2) : CST.type_annotation) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_possibly_implicitly_unwrapped_type env v2 in
  R.Tuple [v1; v2]

and map_type_arguments (env : env) ((v1, v2, v3, v4) : CST.type_arguments) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_type_casting_pattern (env : env) (x : CST.type_casting_pattern) =
  (match x with
  | `Is_type (v1, v2) -> R.Case ("Is_type",
      let v1 = (* "is" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Bind_pat_no_expr_as_type (v1, v2, v3) -> R.Case ("Bind_pat_no_expr_as_type",
      let v1 = map_binding_pattern_no_expr env v1 in
      let v2 = (* as_custom *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_type_constraint (env : env) (x : CST.type_constraint) =
  (match x with
  | `Inhe_cons (v1, v2, v3, v4) -> R.Case ("Inhe_cons",
      let v1 = R.List (List.map (map_attribute env) v1) in
      let v2 = map_identifier env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_possibly_implicitly_unwrapped_type env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Equa_cons (v1, v2, v3, v4) -> R.Case ("Equa_cons",
      let v1 = R.List (List.map (map_attribute env) v1) in
      let v2 = map_identifier env v2 in
      let v3 =
        (match v3 with
        | `Equal_sign tok -> R.Case ("Equal_sign",
            (* eq_custom *) token env tok
          )
        | `Eq_eq tok -> R.Case ("Eq_eq",
            (* eq_eq_custom *) token env tok
          )
        )
      in
      let v4 = map_type_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_type_constraints (env : env) ((v1, v2, v3) : CST.type_constraints) =
  let v1 = (* where_keyword *) token env v1 in
  let v2 = map_type_constraint env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_constraint env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_type_level_declaration (env : env) (x : CST.type_level_declaration) =
  (match x with
  | `Choice_import_decl x -> R.Case ("Choice_import_decl",
      (match x with
      | `Import_decl x -> R.Case ("Import_decl",
          map_import_declaration env x
        )
      | `Prop_decl x -> R.Case ("Prop_decl",
          map_property_declaration env x
        )
      | `Typeas_decl x -> R.Case ("Typeas_decl",
          map_typealias_declaration env x
        )
      | `Func_decl x -> R.Case ("Func_decl",
          map_function_declaration env x
        )
      | `Class_decl x -> R.Case ("Class_decl",
          map_class_declaration env x
        )
      | `Prot_decl x -> R.Case ("Prot_decl",
          map_protocol_declaration env x
        )
      | `Deinit_decl x -> R.Case ("Deinit_decl",
          map_deinit_declaration env x
        )
      | `Subs_decl x -> R.Case ("Subs_decl",
          map_subscript_declaration env x
        )
      | `Op_decl x -> R.Case ("Op_decl",
          map_operator_declaration env x
        )
      | `Prec_group_decl x -> R.Case ("Prec_group_decl",
          map_precedence_group_declaration env x
        )
      | `Asso_decl x -> R.Case ("Asso_decl",
          map_associatedtype_declaration env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_type_modifiers (env : env) (xs : CST.type_modifiers) =
  R.List (List.map (map_attribute env) xs)

and map_type_parameter (env : env) (x : CST.type_parameter) =
  (match x with
  | `Opt_type_param_modifs_simple_id_opt_COLON_type (v1, v2, v3) -> R.Case ("Opt_type_param_modifs_simple_id_opt_COLON_type",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_type_parameter_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_bound_identifier env v2 in
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
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_type_parameter_modifiers (env : env) (xs : CST.type_parameter_modifiers) =
  R.List (List.map (map_attribute env) xs)

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
    | Some x -> R.Option (Some (
        map_type_constraints env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* ">" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_typealias_declaration (env : env) ((v1, v2) : CST.typealias_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_modifierless_typealias_declaration env v2 in
  R.Tuple [v1; v2]

and map_unannotated_type (env : env) (x : CST.unannotated_type) =
  (match x with
  | `User_type x -> R.Case ("User_type",
      map_user_type env x
    )
  | `Tuple_type x -> R.Case ("Tuple_type",
      map_tuple_type env x
    )
  | `Func_type x -> R.Case ("Func_type",
      map_function_type env x
    )
  | `Array_type x -> R.Case ("Array_type",
      map_array_type env x
    )
  | `Dict_type x -> R.Case ("Dict_type",
      map_dictionary_type env x
    )
  | `Opt_type (v1, v2) -> R.Case ("Opt_type",
      let v1 =
        (match v1 with
        | `User_type x -> R.Case ("User_type",
            map_user_type env x
          )
        | `Tuple_type x -> R.Case ("Tuple_type",
            map_tuple_type env x
          )
        | `Array_type x -> R.Case ("Array_type",
            map_array_type env x
          )
        | `Dict_type x -> R.Case ("Dict_type",
            map_dictionary_type env x
          )
        )
      in
      let v2 = R.List (List.map (token env (* "?" *)) v2) in
      R.Tuple [v1; v2]
    )
  | `Meta (v1, v2, v3) -> R.Case ("Meta",
      let v1 = map_unannotated_type env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (match v3 with
        | `Type tok -> R.Case ("Type",
            (* "Type" *) token env tok
          )
        | `Prot tok -> R.Case ("Prot",
            (* "Protocol" *) token env tok
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Opaque_type (v1, v2) -> R.Case ("Opaque_type",
      let v1 = (* "some" *) token env v1 in
      let v2 = map_unannotated_type env v2 in
      R.Tuple [v1; v2]
    )
  | `Exis_type (v1, v2) -> R.Case ("Exis_type",
      let v1 = (* "any" *) token env v1 in
      let v2 = map_unannotated_type env v2 in
      R.Tuple [v1; v2]
    )
  | `Prot_comp_type (v1, v2) -> R.Case ("Prot_comp_type",
      let v1 = map_unannotated_type env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "&" *) token env v1 in
          let v2 = map_unannotated_type env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  )

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `Post_exp (v1, v2) -> R.Case ("Post_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = map_postfix_unary_operator env v2 in
      R.Tuple [v1; v2]
    )
  | `Call_exp x -> R.Case ("Call_exp",
      map_call_expression env x
    )
  | `Cons_exp (v1, v2) -> R.Case ("Cons_exp",
      let v1 =
        (match v1 with
        | `Array_type x -> R.Case ("Array_type",
            map_array_type env x
          )
        | `Dict_type x -> R.Case ("Dict_type",
            map_dictionary_type env x
          )
        | `User_type x -> R.Case ("User_type",
            map_user_type env x
          )
        )
      in
      let v2 = map_constructor_suffix env v2 in
      R.Tuple [v1; v2]
    )
  | `Navi_exp (v1, v2) -> R.Case ("Navi_exp",
      let v1 =
        (match v1 with
        | `Navi_type_exp x -> R.Case ("Navi_type_exp",
            map_navigable_type_expression env x
          )
        | `Exp x -> R.Case ("Exp",
            map_directly_assignable_expression env x
          )
        )
      in
      let v2 = map_navigation_suffix env v2 in
      R.Tuple [v1; v2]
    )
  | `Prefix_exp (v1, v2) -> R.Case ("Prefix_exp",
      let v1 = map_prefix_unary_operator env v1 in
      let v2 = map_directly_assignable_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `As_exp (v1, v2, v3) -> R.Case ("As_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = map_as_operator env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Sele_exp (v1, v2, v3, v4, v5) -> R.Case ("Sele_exp",
      let v1 = (* "#selector" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            (match x with
            | `Gett tok -> R.Case ("Gett",
                (* "getter:" *) token env tok
              )
            | `Sett tok -> R.Case ("Sett",
                (* "setter:" *) token env tok
              )
            )
          ))
        | None -> R.Option None)
      in
      let v4 = map_directly_assignable_expression env v4 in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Open_start_range_exp (v1, v2) -> R.Case ("Open_start_range_exp",
      let v1 = map_range_operator env v1 in
      let v2 = map_directly_assignable_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Open_end_range_exp (v1, v2) -> R.Case ("Open_end_range_exp",
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = (* "..." *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_universally_allowed_pattern (env : env) (x : CST.universally_allowed_pattern) =
  (match x with
  | `Wild_pat tok -> R.Case ("Wild_pat",
      (* "_" *) token env tok
    )
  | `Tuple_pat x -> R.Case ("Tuple_pat",
      map_tuple_pattern env x
    )
  | `Type_cast_pat x -> R.Case ("Type_cast_pat",
      map_type_casting_pattern env x
    )
  | `Case_pat (v1, v2, v3, v4, v5) -> R.Case ("Case_pat",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "case" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_user_type env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* dot_custom *) token env v3 in
      let v4 = map_bound_identifier env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_tuple_pattern env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_user_type (env : env) ((v1, v2) : CST.user_type) =
  let v1 = map_simple_user_type env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* dot_custom *) token env v1 in
      let v2 = map_simple_user_type env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_value_argument (env : env) ((v1, v2) : CST.value_argument) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Rep1_value_arg_label_COLON xs -> R.Case ("Rep1_value_arg_label_COLON",
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_value_argument_label env v1 in
          let v2 = (* ":" *) token env v2 in
          R.Tuple [v1; v2]
        ) xs)
      )
    | `Opt_value_arg_label_COLON_exp (v1, v2) -> R.Case ("Opt_value_arg_label_COLON_exp",
        let v1 =
          (match v1 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = map_value_argument_label env v1 in
              let v2 = (* ":" *) token env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v2 = map_directly_assignable_expression env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2]

and map_value_arguments (env : env) (v1 : CST.value_arguments) =
  (match v1 with
  | `LPAR_opt_value_arg_rep_COMMA_value_arg_RPAR x -> R.Case ("LPAR_opt_value_arg_rep_COMMA_value_arg_RPAR",
      map_constructor_value_arguments env x
    )
  | `LBRACK_opt_value_arg_rep_COMMA_value_arg_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_opt_value_arg_rep_COMMA_value_arg_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_interpolation_contents env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_where_clause (env : env) ((v1, v2) : CST.where_clause) =
  let v1 = (* where_keyword *) token env v1 in
  let v2 = map_directly_assignable_expression env v2 in
  R.Tuple [v1; v2]

and map_while_statement (env : env) ((v1, v2, v3, v4, v5, v6) : CST.while_statement) =
  let v1 = (* "while" *) token env v1 in
  let v2 = map_if_condition_sequence_item env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_if_condition_sequence_item env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "{" *) token env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* "}" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_global_declaration (env : env) (x : CST.global_declaration) =
  (match x with
  | `Import_decl x -> R.Case ("Import_decl",
      map_import_declaration env x
    )
  | `Prop_decl x -> R.Case ("Prop_decl",
      map_property_declaration env x
    )
  | `Typeas_decl x -> R.Case ("Typeas_decl",
      map_typealias_declaration env x
    )
  | `Func_decl x -> R.Case ("Func_decl",
      map_function_declaration env x
    )
  | `Class_decl x -> R.Case ("Class_decl",
      map_class_declaration env x
    )
  | `Prot_decl x -> R.Case ("Prot_decl",
      map_protocol_declaration env x
    )
  | `Op_decl x -> R.Case ("Op_decl",
      map_operator_declaration env x
    )
  | `Prec_group_decl x -> R.Case ("Prec_group_decl",
      map_precedence_group_declaration env x
    )
  | `Asso_decl x -> R.Case ("Asso_decl",
      map_associatedtype_declaration env x
    )
  )

let map_top_level_statement (env : env) (x : CST.top_level_statement) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_directly_assignable_expression env x
    )
  | `Global_decl x -> R.Case ("Global_decl",
      map_global_declaration env x
    )
  | `Labe_stmt x -> R.Case ("Labe_stmt",
      map_labeled_statement env x
    )
  | `Throw_stmt x -> R.Case ("Throw_stmt",
      map_throw_statement env x
    )
  )

let map_source_file (env : env) ((v1, v2) : CST.source_file) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_shebang_line env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_top_level_statement env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_semi env v1 in
            let v2 = map_top_level_statement env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_semi env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_directive (env : env) (tok : CST.directive) =
  (* directive *) token env tok

let map_diagnostic (env : env) (tok : CST.diagnostic) =
  (* diagnostic *) token env tok

let dump_tree root =
  map_source_file () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Multiline_comment (_loc, x) -> ("multiline_comment", "multiline_comment", map_multiline_comment env x)
  | `Directive (_loc, x) -> ("directive", "directive", map_directive env x)
  | `Diagnostic (_loc, x) -> ("diagnostic", "diagnostic", map_diagnostic env x)

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

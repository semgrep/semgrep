module CST = Tree_sitter_swift.CST
module PI = Parse_info
module H = Parse_tree_sitter_helpers
module G = AST_generic
module H2 = AST_generic_helpers

(**
   Boilerplate to be used as a template when mapping the swift CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type context = Program | Pattern
type env = context H.env

let token = H.token
let str = H.str
let todo (env : env) _ = failwith "not implemented"

(* There are several places where Swift expects a type but the generic AST
 * expects an expression. *)
let expr_of_type type_ =
  G.OtherExpr (("TypeExpr", PI.unsafe_fake_info ""), [ G.T type_ ]) |> G.e

(* Semicolons are associated in the CST with the following statement rather than
 * the previous statement. The grammar is slightly more brief this way, but it
 * leads to a strange CST structure. We might consider updating the grammar to
 * make this unnecessary. *)
let associate_statement_semis (fst_stmt : 'a) (stmts : ('b * 'a) list)
    (last_semi : 'b option) : ('a * 'b option) list =
  let rec f = function
    | [] -> ([], last_semi)
    | (prev_semi, stmt) :: tl ->
        let lst, semi = f tl in
        ((stmt, semi) :: lst, Some prev_semi)
  in
  let lst, semi = f stmts in
  (fst_stmt, semi) :: lst

let in_pattern env =
  match env.H.extra with
  | Program -> false
  | Pattern -> true

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-swift/Boilerplate.ml *)

let map_bitwise_binary_operator (env : env) (x : CST.bitwise_binary_operator) =
  match x with
  | `AMP tok -> (G.BitAnd, (* "&" *) token env tok)
  | `BAR tok -> (G.BitOr, (* "|" *) token env tok)
  | `HAT tok -> (G.BitXor, (* "^" *) token env tok)
  | `LTLT tok -> (G.LSL, (* "<<" *) token env tok)
  | `GTGT tok ->
      (* Swift uses an arithmetic right shift:
       * https://docs.swift.org/swift-book/LanguageGuide/AdvancedOperators.html#ID36
       * *)
      (G.ASR, (* ">>" *) token env tok)

let map_function_modifier (env : env) (x : CST.function_modifier) =
  match x with
  | `Infix tok -> (* "infix" *) token env tok
  | `Post tok -> (* "postfix" *) token env tok
  | `Prefix tok -> (* "prefix" *) token env tok

let map_binding_pattern_kind (env : env) (x : CST.binding_pattern_kind) =
  match x with
  | `Var tok -> (* "var" *) token env tok
  | `Let tok -> (* "let" *) token env tok

let map_possibly_async_binding_pattern_kind (env : env)
    ((v1, v2) : CST.possibly_async_binding_pattern_kind) =
  let v1 =
    match v1 with
    | Some tok -> (* async_modifier *) token env tok |> todo env
    | None -> ()
  in
  let v2 = map_binding_pattern_kind env v2 in
  v2

let map_comparison_operator (env : env) (x : CST.comparison_operator) =
  match x with
  | `LT tok -> (G.Lt, (* "<" *) token env tok)
  | `GT tok -> (G.Gt, (* ">" *) token env tok)
  | `LTEQ tok -> (G.LtE, (* "<=" *) token env tok)
  | `GTEQ tok -> (G.GtE, (* ">=" *) token env tok)

let map_assignment_and_operator (env : env) (x : CST.assignment_and_operator) =
  match x with
  | `PLUSEQ tok -> (Some G.Plus, (* "+=" *) token env tok)
  | `DASHEQ tok -> (Some G.Minus, (* "-=" *) token env tok)
  | `STAREQ tok -> (Some G.Mult, (* "*=" *) token env tok)
  | `SLASHEQ tok -> (Some G.Div, (* "/=" *) token env tok)
  | `PERCEQ tok -> (Some G.Mod, (* "%=" *) token env tok)
  | `EQ tok -> (None, (* "=" *) token env tok)

let map_ownership_modifier (env : env) (x : CST.ownership_modifier) =
  match x with
  | `Weak tok -> (* "weak" *) token env tok
  | `Unow_7c8c304 tok -> (* "unowned" *) token env tok
  | `Unow_e455cde tok -> (* "unowned(safe)" *) token env tok
  | `Unow_8fda70e tok -> (* "unowned(unsafe)" *) token env tok

let map_import_kind (env : env) (x : CST.import_kind) =
  match x with
  | `Typeas tok -> (* "typealias" *) token env tok
  | `Struct tok -> (* "struct" *) token env tok
  | `Class tok -> (* "class" *) token env tok
  | `Enum tok -> (* "enum" *) token env tok
  | `Prot tok -> (* "protocol" *) token env tok
  | `Let tok -> (* "let" *) token env tok
  | `Var tok -> (* "var" *) token env tok
  | `Func tok -> (* "func" *) token env tok

let map_optionally_valueful_control_keyword (env : env)
    (x : CST.optionally_valueful_control_keyword) (expr : G.expr option) semi =
  match x with
  | `Ret tok ->
      let tok = (* "return" *) token env tok in
      G.Return (tok, expr, semi) |> G.s
  | `Cont tok -> (* "continue" *) token env tok |> todo env
  | `Brk tok -> (* "break" *) token env tok |> todo env
  | `Yield tok -> (* "yield" *) token env tok |> todo env

let map_multiplicative_operator (env : env) (x : CST.multiplicative_operator) :
    G.operator * G.tok =
  match x with
  | `STAR tok -> (G.Mult, (* "*" *) token env tok)
  | `SLASH tok -> (G.Div, (* "/" *) token env tok)
  | `PERC tok -> (G.Mod, (* "%" *) token env tok)

let map_inheritance_modifier (env : env) (x : CST.inheritance_modifier) =
  match x with
  | `Final tok -> (* "final" *) token env tok

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  match x with
  | `True tok ->
      let tok = (* "true" *) token env tok in
      G.Bool (true, tok)
  | `False tok ->
      let tok = (* "false" *) token env tok in
      G.Bool (false, tok)

let map_member_modifier (env : env) (x : CST.member_modifier) =
  match x with
  | `Over tok -> (* "override" *) token env tok
  | `Conv tok -> (* "convenience" *) token env tok
  | `Requ tok -> (* "required" *) token env tok

let map_try_operator (env : env) (x : CST.try_operator) =
  match x with
  | `Try tok -> (* "try" *) token env tok
  | `TryB tok -> (* "try!" *) token env tok
  | `TryQ tok -> (* "try?" *) token env tok

let map_special_literal (env : env) (x : CST.special_literal) =
  match x with
  | `HASH_36725ee tok -> (* "#file" *) token env tok
  | `HASH_ee0b998 tok -> (* "#fileID" *) token env tok
  | `HASH_bd759bd tok -> (* "#filePath" *) token env tok
  | `HASH_709af6a tok -> (* "#line" *) token env tok
  | `HASH_be35129 tok -> (* "#column" *) token env tok
  | `HASH_96a7ced tok -> (* "#function" *) token env tok
  | `HASH_4d47dbe tok -> (* "#dsohandle" *) token env tok

let map_integer_literal (env : env) (tok : CST.integer_literal) : G.literal =
  let s, t = str env tok in
  G.Int (int_of_string_opt s, t)

let map_mutation_modifier (env : env) (x : CST.mutation_modifier) =
  match x with
  | `Muta tok -> (* "mutating" *) token env tok
  | `Nonm tok -> (* "nonmutating" *) token env tok

let map_property_modifier (env : env) (x : CST.property_modifier) =
  match x with
  | `Static tok -> (* "static" *) token env tok
  | `Dyna tok -> (* "dynamic" *) token env tok
  | `Opt tok -> (* "optional" *) token env tok
  | `Class tok -> (* "class" *) token env tok

let map_parameter_modifier (env : env) (x : CST.parameter_modifier) =
  match x with
  | `Inout tok -> (* "inout" *) token env tok
  | `ATes tok -> (* "@escaping" *) token env tok
  | `ATau tok -> (* "@autoclosure" *) token env tok

let map_throws (env : env) (x : CST.throws) =
  match x with
  | `Throws_kw tok -> (* throws_keyword *) token env tok
  | `Rethrs_kw tok -> (* rethrows_keyword *) token env tok

let map_postfix_unary_operator (env : env) (x : CST.postfix_unary_operator)
    (e : G.expr) =
  match x with
  | `PLUSPLUS tok ->
      G.special (G.IncrDecr (G.Incr, G.Postfix), (* "++" *) token env tok) [ e ]
  | `DASHDASH tok ->
      G.special (G.IncrDecr (G.Decr, G.Postfix), (* "--" *) token env tok) [ e ]
  | `Bang tok ->
      G.special (G.Op G.NotNullPostfix, (* bang *) token env tok) [ e ]

let map_locally_permitted_modifier (env : env)
    (x : CST.locally_permitted_modifier) =
  match x with
  | `Owne_modi x -> map_ownership_modifier env x
  | `Prop_beha_modi tok -> (* "lazy" *) token env tok
  | `Inhe_modi x -> map_inheritance_modifier env x

let map_custom_operator (env : env) ((v1, v2) : CST.custom_operator) =
  let ((s1, tok1) as v1) = (* tok_choice_pat_3425898 *) str env v1 in
  match v2 with
  | Some tok ->
      let s2, tok2 = (* "<" *) str env tok in
      (s1 ^ s2, PI.combine_infos tok1 [ tok2 ])
  | None -> v1

let map_setter_specifier (env : env) ((v1, v2) : CST.setter_specifier) =
  let v1 =
    match v1 with
    | Some x -> map_mutation_modifier env x
    | None -> todo env ()
  in
  let v2 = (* "set" *) token env v2 in
  todo env (v1, v2)

let map_modify_specifier (env : env) ((v1, v2) : CST.modify_specifier) =
  let v1 =
    match v1 with
    | Some x -> map_mutation_modifier env x
    | None -> todo env ()
  in
  let v2 = (* "_modify" *) token env v2 in
  todo env (v1, v2)

let map_constructor_function_decl (env : env)
    ((v1, v2) : CST.constructor_function_decl) =
  (* TODO special-case the constructor somehow? *)
  let v1 = (* "init" *) str env v1 in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `Quest tok -> (* "?" *) token env tok |> todo env
        | `Bang tok -> (* bang *) token env tok |> todo env)
    | None -> ()
  in
  v1

let map_additive_operator (env : env) (x : CST.additive_operator) :
    G.operator * G.tok =
  match x with
  | `Plus_then_ws tok
  | `PLUS tok ->
      (G.Plus, (* "+" *) token env tok)
  | `Minus_then_ws tok
  | `DASH tok ->
      (G.Minus, (* "-" *) token env tok)

let map_non_local_scope_modifier (env : env) (x : CST.non_local_scope_modifier)
    =
  match x with
  | `Member_modi x -> map_member_modifier env x
  | `Visi_modi (v1, v2) ->
      let v1 =
        match v1 with
        | `Public tok -> (* "public" *) token env tok
        | `Priv tok -> (* "private" *) token env tok
        | `Inte tok -> (* "internal" *) token env tok
        | `File tok -> (* "fileprivate" *) token env tok
        | `Open tok -> (* "open" *) token env tok
      in
      let v2 =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = (* "(" *) token env v1 in
            let v2 = (* "set" *) token env v2 in
            let v3 = (* ")" *) token env v3 in
            todo env (v1, v2, v3)
        | None -> todo env ()
      in
      todo env (v1, v2)
  | `Func_modi x -> map_function_modifier env x
  | `Muta_modi x -> map_mutation_modifier env x
  | `Prop_modi x -> map_property_modifier env x
  | `Param_modi x -> map_parameter_modifier env x

let map_parameter_modifiers (env : env) (xs : CST.parameter_modifiers) =
  Common.map (map_parameter_modifier env) xs

let map_simple_identifier (env : env) (x : CST.simple_identifier) : G.ident =
  match x with
  | `Pat_9d0cc04 tok ->
      (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok
  | `Pat_97d645c tok -> (* pattern `[^\r\n` ]*` *) str env tok
  | `Pat_c332828 tok -> (* pattern \$[0-9]+ *) str env tok
  | `Tok_dollar_pat_9d0cc04 tok -> (* tok_dollar_pat_9d0cc04 *) str env tok

let map_bound_identifier (env : env) (x : CST.bound_identifier) =
  map_simple_identifier env x

let map_equality_operator (env : env) (x : CST.equality_operator) =
  match x with
  | `BANGEQ tok -> (G.NotEq, (* "!=" *) token env tok)
  | `BANGEQEQ tok -> (G.NotPhysEq, (* "!==" *) token env tok)
  | `Eq_eq tok -> (G.Eq, (* eq_eq_custom *) token env tok)
  | `EQEQEQ tok -> (G.PhysEq, (* "===" *) token env tok)

let map_range_operator (env : env) (x : CST.range_operator) =
  match x with
  | `Open_ended_range_op tok ->
      (* open_ended_range_operator_custom *) token env tok
  | `Three_dot_op tok -> (* three_dot_operator_custom *) token env tok

let map_str_escaped_char (env : env) (x : CST.str_escaped_char) =
  match x with
  | `Esca_id tok -> (* pattern "\\\\[0\\\\tnr\"'\\n]" *) str env tok
  | `Uni_char_lit (v1, v2, v3) ->
      let s1, t1 = (* "\\" *) str env v1 in
      let s2, t2 = (* "u" *) str env v2 in
      let s3, t3 = (* pattern \{[0-9a-fA-F]+\} *) str env v3 in
      (String.concat "" [ s1; s2; s3 ], PI.combine_infos t1 [ t2; t3 ])

let map_prefix_unary_operator (env : env) (x : CST.prefix_unary_operator)
    (e : G.expr) =
  match x with
  | `PLUSPLUS tok ->
      G.special (G.IncrDecr (G.Incr, G.Prefix), (* "++" *) token env tok) [ e ]
  | `DASHDASH tok ->
      G.special (G.IncrDecr (G.Decr, G.Prefix), (* "--" *) token env tok) [ e ]
  | `DASH tok ->
      let op = (G.Minus, (* "-" *) token env tok) in
      G.opcall op [ e ]
  | `PLUS tok ->
      let op = (G.Plus, (* "+" *) token env tok) in
      G.opcall op [ e ]
  | `Bang tok ->
      let op = (G.Not, (* bang *) token env tok) in
      G.opcall op [ e ]
  | `AMP tok -> G.Ref ((* "&" *) token env tok, e) |> G.e
  | `TILDE tok ->
      let op = (G.BitNot, (* "~" *) token env tok) in
      G.opcall op [ e ]
  | `Dot tok ->
      let dot = (* dot_custom *) token env tok in
      let field_name =
        (* TODO restructure the grammar so that this isn't necessary *)
        match e with
        | { G.e = G.N name; _ } -> G.FN name
        | _ ->
            (* I (nmote) don't believe that this is valid Swift code, but the
             * grammar currently allows it... *)
            G.FDynamic e
      in
      (* This is an implicit member expression:
       * https://docs.swift.org/swift-book/ReferenceManual/Expressions.html#ID394
       *
       * Rather than writing out the target of the member expression explicitly,
       * Swift allows the programmer to omit it in certain cases where it can be
       * inferred.
       *)
      let receiver = G.OtherExpr (("Implicit", dot), []) |> G.e in
      G.DotAccess (receiver, dot, field_name) |> G.e
  | `Custom_op x ->
      let op = map_custom_operator env x in
      G.Call (G.N (H2.name_of_id op) |> G.e, G.fake_bracket [ G.Arg e ]) |> G.e

let map_as_operator (env : env) (x : CST.as_operator) =
  match x with
  | `As tok -> (* as_custom *) token env tok
  | `As_quest tok -> (* as_quest_custom *) token env tok
  | `As_bang tok -> (* as_bang_custom *) token env tok

let map_operator_declaration (env : env)
    ((v1, v2, v3, v4) : CST.operator_declaration) =
  let v1 =
    match v1 with
    | `Prefix tok -> (* "prefix" *) token env tok
    | `Infix tok -> (* "infix" *) token env tok
    | `Post tok -> (* "postfix" *) token env tok
  in
  let v2 = (* "operator" *) token env v2 in
  let v3 = map_custom_operator env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_simple_identifier env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2, v3, v4)

let map_identifier (env : env) ((v1, v2) : CST.identifier) =
  let v1 = map_simple_identifier env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* dot_custom *) token env v1 in
        let v2 = map_simple_identifier env v2 in
        v2)
      v2
  in
  v1 :: v2

let map_navigation_suffix (env : env) ((v1, v2) : CST.navigation_suffix) :
    G.tok * G.field_name =
  let v1 = (* dot_custom *) token env v1 in
  let v2 =
    match v2 with
    | `Simple_id x -> G.FN (map_simple_identifier env x |> H2.name_of_id)
    | `Int_lit tok -> G.FDynamic (G.L (map_integer_literal env tok) |> G.e)
  in
  (v1, v2)

let map_precedence_group_attribute (env : env)
    ((v1, v2, v3) : CST.precedence_group_attribute) =
  let v1 = map_simple_identifier env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 =
    match v3 with
    | `Simple_id x -> map_simple_identifier env x
    | `Bool_lit x -> map_boolean_literal env x |> todo env
  in
  todo env (v1, v2, v3)

let map_tuple_type_item_identifier (env : env)
    ((v1, v2, v3) : CST.tuple_type_item_identifier) =
  let v1 =
    match v1 with
    | Some tok -> (* "_" *) token env tok
    | None -> todo env ()
  in
  let v2 = map_simple_identifier env v2 in
  let v3 = (* ":" *) token env v3 in
  todo env (v1, v2, v3)

let map_referenceable_operator (env : env) (x : CST.referenceable_operator) =
  match x with
  | `Custom_op x -> map_custom_operator env x |> todo env
  | `Comp_op x -> map_comparison_operator env x |> todo env
  | `Addi_op x -> map_additive_operator env x |> todo env
  | `Mult_op x -> map_multiplicative_operator env x |> todo env
  | `Equa_op x -> map_equality_operator env x |> todo env
  | `PLUSPLUS tok -> (* "++" *) token env tok
  | `DASHDASH tok -> (* "--" *) token env tok
  | `Bang tok -> (* bang *) token env tok
  | `TILDE tok -> (* "~" *) token env tok

let map_multi_line_string_content (env : env)
    (x : CST.multi_line_string_content) =
  match x with
  | `Multi_line_str_text tok -> (* pattern "[^\\\\\"]+" *) str env tok
  | `Str_esca_char x -> map_str_escaped_char env x
  | `DQUOT tok -> (* "\"" *) str env tok

let map_line_string_content (env : env) (x : CST.line_string_content) =
  match x with
  | `Line_str_text tok -> (* pattern "[^\\\\\"]+" *) str env tok
  | `Str_esca_char x -> map_str_escaped_char env x

let map_getter_effects (env : env) (xs : CST.getter_effects) =
  Common.map
    (fun x ->
      match x with
      | `Async_kw tok -> (* async_keyword_custom *) token env tok
      | `Throws x -> map_throws env x)
    xs

let map_precedence_group_attributes (env : env)
    (xs : CST.precedence_group_attributes) =
  Common.map (map_precedence_group_attribute env) xs

let map_non_constructor_function_decl (env : env)
    ((v1, v2) : CST.non_constructor_function_decl) =
  let v1 = (* "func" *) token env v1 in
  let v2 =
    match v2 with
    | `Simple_id x -> map_simple_identifier env x
    | `Refe_op x -> map_referenceable_operator env x |> todo env
    | `Bitw_bin_op x -> map_bitwise_binary_operator env x |> todo env
  in
  v2

let map_getter_specifier (env : env) ((v1, v2, v3) : CST.getter_specifier) =
  let v1 =
    match v1 with
    | Some x -> map_mutation_modifier env x
    | None -> todo env ()
  in
  let v2 = (* "get" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_getter_effects env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

let map_availability_argument (env : env) (x : CST.availability_argument) =
  match x with
  | `Id_int_lit_rep_DOT_int_lit (v1, v2, v3) ->
      let v1 = map_identifier env v1 in
      let v2 = (* integer_literal *) token env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "." *) token env v1 in
            let v2 = (* integer_literal *) token env v2 in
            todo env (v1, v2))
          v3
      in
      todo env (v1, v2, v3)
  | `STAR tok -> (* "*" *) token env tok

let map_precedence_group_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.precedence_group_declaration) =
  let v1 = (* "precedencegroup" *) token env v1 in
  let v2 = map_simple_identifier env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 =
    match v4 with
    | Some x -> map_precedence_group_attributes env x
    | None -> todo env ()
  in
  let v5 = (* "}" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

let map_protocol_property_requirements (env : env)
    ((v1, v2, v3) : CST.protocol_property_requirements) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Getter_spec x -> map_getter_specifier env x
        | `Setter_spec x -> map_setter_specifier env x)
      v2
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

let rec map_annotated_inheritance_specifier (env : env)
    ((v1, v2) : CST.annotated_inheritance_specifier) =
  let v1 = Common.map (map_attribute env) v1 in
  (* TODO attribute *)
  let v2 = map_inheritance_specifier env v2 in
  v2

and map_enum_entry_suffix (env : env) (x : CST.enum_entry_suffix) =
  match x with
  | `Enum_type_params (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2, v3, v4) ->
            let v1 =
              match v1 with
              | Some x -> map_tuple_type_item_identifier env x
              | None -> todo env ()
            in
            let v2 = map_type_ env v2 in
            let v3 =
              match v3 with
              | Some (v1, v2) ->
                  let v1 = (* eq_custom *) token env v1 in
                  let v2 = map_expression env v2 in
                  todo env (v1, v2)
              | None -> todo env ()
            in
            let v4 =
              Common.map
                (fun (v1, v2, v3, v4) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 =
                    match v2 with
                    | Some x -> map_tuple_type_item_identifier env x
                    | None -> todo env ()
                  in
                  let v3 = map_type_ env v3 in
                  let v4 =
                    match v4 with
                    | Some (v1, v2) ->
                        let v1 = (* eq_custom *) token env v1 in
                        let v2 = map_expression env v2 in
                        todo env (v1, v2)
                    | None -> todo env ()
                  in
                  todo env (v1, v2, v3, v4))
                v4
            in
            todo env (v1, v2, v3, v4)
        | None -> todo env ()
      in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `Equal_sign_exp (v1, v2) ->
      let v1 = (* eq_custom *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)

and map_type_casting_pattern (env : env) (x : CST.type_casting_pattern) =
  match x with
  | `Is_type (v1, v2) ->
      let v1 = (* "is" *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
  | `Bind_pat_no_expr_as_type (v1, v2, v3) ->
      let v1 = map_binding_pattern_no_expr env v1 in
      let v2 = (* as_custom *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)

and map_computed_getter (env : env) ((v1, v2, v3) : CST.computed_getter) =
  let v1 = Common.map (map_attribute env) v1 in
  let v2 = map_getter_specifier env v2 in
  let v3 =
    match v3 with
    | Some x -> map_function_body env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_computed_modify (env : env) ((v1, v2, v3) : CST.computed_modify) =
  let v1 = Common.map (map_attribute env) v1 in
  let v2 = map_modify_specifier env v2 in
  let v3 =
    match v3 with
    | Some x -> map_function_body env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_computed_property (env : env) ((v1, v2, v3) : CST.computed_property) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with
    | `Opt_stmts opt -> (
        match opt with
        | Some x -> map_statements env x
        | None -> todo env ())
    | `Rep_choice_comp_getter xs ->
        Common.map
          (fun x ->
            match x with
            | `Comp_getter x -> map_computed_getter env x
            | `Comp_setter x -> map_computed_setter env x
            | `Comp_modify x -> map_computed_modify env x)
          xs
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_computed_setter (env : env) ((v1, v2, v3, v4) : CST.computed_setter) =
  let v1 = Common.map (map_attribute env) v1 in
  let v2 = map_setter_specifier env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2, v3) ->
        let v1 = (* "(" *) token env v1 in
        let v2 = map_simple_identifier env v2 in
        let v3 = (* ")" *) token env v3 in
        todo env (v1, v2, v3)
    | None -> todo env ()
  in
  let v4 =
    match v4 with
    | Some x -> map_function_body env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3, v4)

and map_array_type (env : env) ((v1, v2, v3) : CST.array_type) : G.type_ =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 = (* "]" *) token env v3 in
  G.TyArray ((v1, None, v3), v2) |> G.t

and map_associatedtype_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.associatedtype_declaration) =
  let v1 =
    match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ()
  in
  let v2 = (* "associatedtype" *) token env v2 in
  let v3 = map_simple_identifier env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v5 =
    match v5 with
    | Some x -> map_type_constraints env x
    | None -> todo env ()
  in
  let v6 =
    match v6 with
    | Some (v1, v2) ->
        let v1 = (* eq_custom *) token env v1 in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2, v3, v4, v5, v6)

and map_attribute_argument (env : env) (x : CST.attribute_argument) =
  match x with
  | `Simple_id_COLON_exp (v1, v2, v3) ->
      let v1 = map_bound_identifier env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp x -> map_expression env x
  | `Rep1_simple_id_COLON xs ->
      Common.map
        (fun (v1, v2) ->
          let v1 = map_bound_identifier env v1 in
          let v2 = (* ":" *) token env v2 in
          todo env (v1, v2))
        xs
      |> todo env
  | `Rep1_simple_id_int_lit_rep_DOT_int_lit (v1, v2, v3) ->
      let v1 = Common.map (map_bound_identifier env) v1 in
      let v2 = (* integer_literal *) token env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "." *) token env v1 in
            let v2 = (* integer_literal *) token env v2 in
            todo env (v1, v2))
          v3
      in
      todo env (v1, v2, v3)

and map_attribute (env : env) (x : CST.attribute) =
  match x with
  | `Rectype (v1, v2, v3) ->
      let v1 = (* "@" *) token env v1 in
      let v2 = map_user_type env v2 in
      let v3 =
        match v3 with
        | Some (v1, v2, v3, v4) ->
            let v1 = (* "(" *) token env v1 in
            let v2 = map_attribute_argument env v2 in
            let v3 =
              Common.map
                (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_attribute_argument env v2 in
                  todo env (v1, v2))
                v3
            in
            let v4 = (* ")" *) token env v4 in
            todo env (v1, v2, v3, v4)
        | None -> todo env ()
      in
      todo env (v1, v2, v3)

(* Returns G.expr rather than G.literal because interpolated string literals are
 * represented as G.Call expressions. *)
and map_basic_literal (env : env) (x : CST.basic_literal) : G.expr =
  match x with
  | `Int_lit tok
  | `Hex_lit tok
  | `Oct_lit tok
  | `Bin_lit tok ->
      G.L (map_integer_literal env tok) |> G.e
  | `Real_lit tok ->
      let s, t = str env tok in
      G.L (G.Float (float_of_string_opt s, t)) |> G.e
  | `Bool_lit x -> G.L (map_boolean_literal env x) |> G.e
  | `Str_lit x -> map_string_literal env x
  | `Nil tok -> G.L (G.Null ((* "nil" *) token env tok)) |> G.e

and map_binary_expression (env : env) (x : CST.binary_expression) =
  match x with
  | `Mult_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_multiplicative_operator env v2 in
      let v3 = map_expression env v3 in
      G.opcall v2 [ v1; v3 ]
  | `Addi_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_additive_operator env v2 in
      let v3 = map_expression env v3 in
      G.opcall v2 [ v1; v3 ]
  | `Range_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_range_operator env v2 in
      let v3 = map_expression env v3 in
      G.opcall (G.Range, v2) [ v1; v3 ]
  | `Infix_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_custom_operator env v2 in
      let v3 = map_expression env v3 in
      G.Call
        (G.N (H2.name_of_id v2) |> G.e, G.fake_bracket [ G.Arg v1; G.Arg v3 ])
      |> G.e
  | `Nil_coal_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* nil_coalescing_operator_custom *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (G.Nullish, v2) [ v1; v3 ]
  | `Check_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "is" *) token env v2 in
      let v3 = map_type_ env v3 in
      G.special (G.Instanceof, v2) [ v1; expr_of_type v3 ]
  | `Equa_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_equality_operator env v2 in
      let v3 = map_expression env v3 in
      G.opcall v2 [ v1; v3 ]
  | `Comp_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_comparison_operator env v2 in
      let v3 = map_expression env v3 in
      G.opcall v2 [ v1; v3 ]
  | `Conj_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* conjunction_operator_custom *) token env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      G.opcall (G.And, v2) [ v1; v3 ]
  | `Disj_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* disjunction_operator_custom *) token env v2 in
      let v3 = map_expression env v3 in
      G.opcall (G.Or, v2) [ v1; v3 ]
  | `Bitw_oper (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_bitwise_binary_operator env v2 in
      let v3 = map_expression env v3 in
      G.opcall v2 [ v1; v3 ]

and map_binding_pattern (env : env) ((v1, v2, v3) : CST.binding_pattern) =
  let v1 =
    match v1 with
    | Some tok -> (* "case" *) token env tok
    | None -> todo env ()
  in
  let v2 = map_binding_pattern_kind env v2 in
  let v3 = map_no_expr_pattern_already_bound env v3 in
  todo env (v1, v2, v3)

and map_binding_pattern_with_expr (env : env)
    ((v1, v2) : CST.binding_pattern_with_expr) =
  let v1 =
    match v1 with
    | `Univ_allo_pat x -> map_universally_allowed_pattern env x
    | `Bind_pat x -> map_binding_pattern env x
    | `Exp x -> (
        match (map_expression env x).G.e with
        (* The parser parses `foo` in `let (foo) = ...` as a full-blown
         * expression, rather than an identifier special-case. So, we should
         * unwrap that here to get the desired generic AST output. *)
        | G.N (G.Id (id, id_info)) -> G.PatId (id, id_info)
        | _ -> todo env ())
  in
  let v2 =
    match v2 with
    | Some tok -> (* "?" *) token env tok |> todo env
    | None -> ()
  in
  v1

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_statements env x
    | None -> []
  in
  let v3 = (* "}" *) token env v3 in
  G.Block (v1, v2, v3) |> G.s

and map_bodyless_function_declaration (env : env) ~in_class
    ((v1, v2, v3) : CST.bodyless_function_declaration) body =
  let v1 =
    match v1 with
    | Some x -> map_modifiers env x |> todo env
    | None -> ()
  in
  let v2 =
    match v2 with
    | Some tok -> (* "class" *) token env tok |> todo env
    | None -> ()
  in
  let v3 =
    map_modifierless_function_declaration_no_body env ~in_class v3 body
  in
  v3

and map_call_expression (env : env) ((v1, v2) : CST.call_expression) : G.expr =
  let v1 = map_expression env v1 in
  let v2 = map_call_suffix env v2 in
  G.Call (v1, v2) |> G.e

and map_call_suffix (env : env) (v1 : CST.call_suffix) : G.arguments =
  match v1 with
  | `Value_args x -> map_expr_hack_at_ternary_binary_call_suffix env x
  | `Lambda_lit_rep_simple_id_COLON_lambda_lit (v1, v2) ->
      (* When one or more lambda literals are provided after (or instead of)
       * parenthesized arguments to a function call, they are the final
       * arguments to that function call. In this case, the labels are not
       * provided even if the function call would normally require them. *)
      (* TODO update the tree-sitter-swift grammar to allow closures to be
       * passed after the parenthesized arguments, rather than just instead of
       * them. *)
      (* TODO make sure that Semgrep can find these arguments when looking for
       * the corresponding labeled argument. e.g. `foo(bar: { x in x + 1 })`
       * is the same as `foo { x in x + 1 }` and a search for `foo` called with
       * a labeled `bar` argument should probably find the latter as well as the
       * former. *)
      let anon_arg = G.Arg (map_lambda_literal env v1) in
      let labeled_args =
        Common.map
          (fun (v1, v2, v3) ->
            let name = map_simple_identifier env v1 in
            let _colon = (* ":" *) token env v2 in
            let lambda = map_lambda_literal env v3 in
            G.ArgKwd (name, lambda))
          v2
      in
      anon_arg :: labeled_args |> G.fake_bracket

and map_capture_list (env : env) ((v1, v2, v3, v4, v5) : CST.capture_list) =
  let v1 = Common.map (map_attribute env) v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_capture_list_item env v3 in
  let v4 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_capture_list_item env v2 in
        todo env (v1, v2))
      v4
  in
  let v5 = (* "]" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_capture_list_item (env : env) (x : CST.capture_list_item) =
  match x with
  | `Self_exp tok -> (* "self" *) token env tok
  | `Opt_owne_modi_simple_id_opt_equal_sign_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some x -> map_ownership_modifier env x
        | None -> todo env ()
      in
      let v2 = map_simple_identifier env v2 in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = (* eq_custom *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3)

and map_catch_block (env : env) ((v1, v2, v3, v4) : CST.catch_block) =
  let v1 = (* catch_keyword *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_binding_pattern_no_expr env x
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some x -> map_where_clause env x
    | None -> todo env ()
  in
  let v4 = map_function_body env v4 in
  todo env (v1, v2, v3, v4)

and map_class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_class_member_declarations env x
    | None -> []
  in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_class_declaration (env : env) ((v1, v2) : CST.class_declaration) =
  let v1 =
    match v1 with
    | Some x -> map_modifiers env x |> todo env
    | None -> ()
  in
  let v2 = map_modifierless_class_declaration env v2 in
  v2

and map_class_member_declarations (env : env)
    ((v1, v2, v3) : CST.class_member_declarations) =
  let v1 = map_type_level_declaration env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* semi *) token env v1 in
        let v2 = map_type_level_declaration env v2 in
        v2)
      v2
  in
  let fields =
    List.concat_map
      (fun stmts -> List.map (fun stmt -> G.F stmt) stmts)
      (v1 :: v2)
  in
  let v3 =
    match v3 with
    | Some tok ->
        let _ = (* semi *) token env tok in
        ()
    | None -> ()
  in
  fields

and map_constructor_suffix (env : env) (v1 : CST.constructor_suffix) =
  match v1 with
  | `Cons_value_args x -> map_constructor_value_arguments env x
  | `Lambda_lit x -> G.fake_bracket [ G.Arg (map_lambda_literal env x) ]

and map_constructor_value_arguments (env : env)
    ((v1, v2, v3) : CST.constructor_value_arguments) : G.arguments =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_arguments env x
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and map_control_transfer_statement (env : env)
    (x : CST.control_transfer_statement) semi =
  match x with
  | `Throw_stmt x -> map_throw_statement env x
  | `Opti_valu_cont_kw_opt_exp (v1, v2) ->
      let v2 =
        match v2 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      map_optionally_valueful_control_keyword env v1 v2 semi

and map_deinit_declaration (env : env) ((v1, v2, v3) : CST.deinit_declaration) =
  let v1 =
    match v1 with
    | Some x -> map_modifiers env x |> todo env
    | None -> ()
  in
  let v2 = (* "deinit" *) str env v2 in
  let v3 = map_function_body env v3 in
  let entity = G.basic_entity v2 in
  let definition_kind =
    G.FuncDef
      { fkind = (G.Method, snd v2); fparams = []; frettype = None; fbody = v3 }
  in
  G.DefStmt (entity, definition_kind) |> G.s

and map_dictionary_literal_item (env : env)
    ((v1, v2, v3) : CST.dictionary_literal_item) =
  let v1 = map_expression env v1 in
  let _v2 = (* ":" *) token env v2 in
  let v3 = map_expression env v3 in
  G.Container (G.Tuple, G.fake_bracket [ v1; v3 ]) |> G.e

and map_dictionary_type (env : env) ((v1, v2, v3, v4, v5) : CST.dictionary_type)
    =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type_ env v4 in
  let v5 = (* "]" *) token env v5 in
  (* Modeled after Semgrep treats map types in Go. In Swift, [Int: Int] is
   * equivalent to Dictionary<Int, Int>, so we'll just desugar to that. *)
  let dict_name = H2.name_of_id ("Dictionary", v1) in
  G.TyApply (G.TyN dict_name |> G.t, (v1, [ G.TA v2; G.TA v4 ], v5)) |> G.t

and map_binding_kind_and_pattern (env : env)
    ((v1, v2) : CST.binding_kind_and_pattern) =
  let v1 = map_possibly_async_binding_pattern_kind env v1 in
  let v2 = map_no_expr_pattern_already_bound env v2 in
  todo env (v1, v2)

and map_direct_or_indirect_binding (env : env)
    ((v1, v2) : CST.direct_or_indirect_binding) =
  let v1 =
    match v1 with
    | `Bind_kind_and_pat x -> map_binding_kind_and_pattern env x
    | `Case_bind_pat_no_expr (v1, v2) ->
        let v1 = (* "case" *) token env v1 in
        let v2 = map_binding_pattern_no_expr env v2 in
        todo env (v1, v2)
  in
  let v2 =
    match v2 with
    | Some x -> map_type_annotation env x
    | None -> todo env ()
  in
  todo env (v1, v2)

and map_directly_assignable_expression (env : env)
    (x : CST.directly_assignable_expression) =
  match x with
  | `Simple_id x ->
      let id = map_simple_identifier env x in
      G.N (H2.name_of_id id) |> G.e
  | `Navi_exp x -> map_navigation_expression env x
  | `Call_exp x -> map_call_expression env x
  | `Tuple_exp x -> map_tuple_expression env x
  | `Self_exp tok -> map_self_expression env tok

and map_do_statement (env : env) ((v1, v2, v3) : CST.do_statement) =
  let v1 = (* "do" *) token env v1 in
  let v2 = map_function_body env v2 in
  let v3 = Common.map (map_catch_block env) v3 in
  todo env (v1, v2, v3)

and map_else_options (env : env) (x : CST.else_options) =
  match x with
  | `Blk x -> map_function_body env x
  | `If_stmt x -> map_if_statement env x

and map_enum_class_body (env : env) ((v1, v2, v3) : CST.enum_class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Enum_entry x -> map_enum_entry env x
        | `Type_level_decl x -> map_type_level_declaration env x)
      v2
    |> List.concat_map (fun entries ->
           Common.map (fun entry -> G.F entry) entries)
  in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_enum_entry (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.enum_entry) =
  let v1 =
    match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ()
  in
  let v2 =
    match v2 with
    | Some tok -> (* "indirect" *) token env tok
    | None -> todo env ()
  in
  let v3 = (* "case" *) token env v3 in
  let v4 = map_simple_identifier env v4 in
  let v5 =
    match v5 with
    | Some x -> map_enum_entry_suffix env x
    | None -> todo env ()
  in
  let v6 =
    Common.map
      (fun (v1, v2, v3) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_simple_identifier env v2 in
        let v3 =
          match v3 with
          | Some x -> map_enum_entry_suffix env x
          | None -> todo env ()
        in
        todo env (v1, v2, v3))
      v6
  in
  let v7 =
    match v7 with
    | Some tok -> (* ";" *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Choice_simple_id x -> (
      match x with
      | `Simple_id x ->
          let id = map_simple_identifier env x in
          G.N (H2.name_of_id id) |> G.e
      | `Un_exp x -> map_unary_expression env x
      | `Bin_exp x -> map_binary_expression env x
      | `Tern_exp x -> map_ternary_expression env x
      | `Prim_exp x -> map_primary_expression env x
      | `Assign (v1, v2, v3) -> (
          let v1 = map_directly_assignable_expression env v1 in
          let op, optok = map_assignment_and_operator env v2 in
          let v3 = map_expression env v3 in
          match op with
          | None -> G.Assign (v1, optok, v3) |> G.e
          | Some op -> G.AssignOp (v1, (op, optok), v3) |> G.e)
      | `Exp_imme_quest (v1, v2) ->
          let v1 = map_expression env v1 in
          let v2 = (* "?" *) token env v2 in
          (* This is how optional chaining is parsed. It looks like the fact that
           * it's an optional chain is just discarded when analyzing JS, so this
           * should be fine for now. *)
          v1
      | `Async tok ->
          (* In this context, async is just a normal identifier *)
          let id = str env tok in
          G.N (H2.name_of_id id) |> G.e)
  | `Semg_ellips tok ->
      let tok = (* three_dot_operator_custom *) token env tok in
      G.Ellipsis tok |> G.e

and map_for_statement (env : env)
    ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.for_statement) =
  let v1 = (* "for" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_try_operator env x
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some tok -> (* "await" *) token env tok
    | None -> todo env ()
  in
  let v4 = map_binding_pattern_no_expr env v4 in
  let v5 =
    match v5 with
    | Some x -> map_type_annotation env x
    | None -> todo env ()
  in
  let v6 = (* "in" *) token env v6 in
  let v7 = map_expression env v7 in
  let v8 =
    match v8 with
    | Some x -> map_where_clause env x
    | None -> todo env ()
  in
  let v9 = map_function_body env v9 in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)

and map_function_body (env : env) (x : CST.function_body) : G.function_body =
  G.FBStmt (map_block env x)

and map_function_declaration (env : env) ~in_class
    ((v1, v2) : CST.function_declaration) =
  let v2 = map_function_body env v2 in
  let v1 = map_bodyless_function_declaration env ~in_class v1 v2 in
  v1

and map_function_type (env : env) ((v1, v2, v3, v4, v5) : CST.function_type) =
  let v1 = map_tuple_type env v1 in
  let v2 =
    match v2 with
    | Some tok -> (* async_keyword_custom *) token env tok
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some x -> map_throws env x
    | None -> todo env ()
  in
  let v4 = (* arrow_operator_custom *) token env v4 in
  let v5 = map_type_ env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_function_value_parameter (env : env)
    ((v1, v2, v3) : CST.function_value_parameter) : G.parameter =
  let v1 =
    match v1 with
    | Some x -> map_attribute env x |> todo env
    | None -> ()
  in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = (* eq_custom *) token env v1 in
        let v2 = map_expression env v2 in
        Some v2
    | None -> None
  in
  map_parameter env v2 v3

and map_function_value_parameters (env : env)
    ((v1, v2, v3) : CST.function_value_parameters) : G.parameters =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_function_value_parameter env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_function_value_parameter env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  v2

and map_guard_statement (env : env) ((v1, v2, v3, v4, v5) : CST.guard_statement)
    =
  let v1 = (* "guard" *) token env v1 in
  let v2 = map_if_condition_sequence_item env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_if_condition_sequence_item env v2 in
        todo env (v1, v2))
      v3
  in
  let v4 = (* else *) token env v4 in
  let v5 = map_function_body env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_if_condition_sequence_item (env : env)
    (x : CST.if_condition_sequence_item) =
  match x with
  | `If_let_bind (v1, v2, v3) ->
      let v1 = map_direct_or_indirect_binding env v1 in
      let v2 = (* eq_custom *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp x -> map_expression env x
  | `Avai_cond (v1, v2, v3, v4, v5) ->
      let v1 = (* "#available" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_availability_argument env v3 in
      let v4 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_availability_argument env v2 in
            todo env (v1, v2))
          v4
      in
      let v5 = (* ")" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)

and map_if_statement (env : env) ((v1, v2, v3, v4, v5) : CST.if_statement) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_if_condition_sequence_item env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_if_condition_sequence_item env v2 in
        todo env (v1, v2))
      v3
  in
  let v4 = map_function_body env v4 in
  let v5 =
    match v5 with
    | Some (v1, v2) ->
        let v1 = (* else *) token env v1 in
        let v2 = map_else_options env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2, v3, v4, v5)

and map_import_declaration (env : env)
    ((v1, v2, v3, v4) : CST.import_declaration) =
  let v1 =
    match v1 with
    | Some x -> map_modifiers env x |> todo env
    | None -> ()
  in
  let v2 = (* "import" *) token env v2 in
  let v3 =
    match v3 with
    (* TODO do something with the import kind? *)
    | Some x -> ignore (map_import_kind env x)
    | None -> ()
  in
  let v4 = map_identifier env v4 in
  (* TODO Use ImportFrom for `import foo.bar.baz`? *)
  G.DirectiveStmt
    (G.ImportAll (v2, G.DottedName v4, PI.unsafe_fake_info "") |> G.d)
  |> G.s

and map_inheritance_specifier (env : env) (x : CST.inheritance_specifier) =
  match x with
  | `User_type x -> map_user_type env x
  | `Func_type x -> map_function_type env x |> todo env

and map_inheritance_specifiers (env : env)
    ((v1, v2) : CST.inheritance_specifiers) =
  let v1 = map_annotated_inheritance_specifier env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 =
          (* TODO definitively determine if there is a difference between these
           * two in this context. I (nmote) did some basic testing and didn't
           * notice a difference. *)
          match v1 with
          | `COMMA tok -> (* "," *) token env tok
          | `AMP tok -> (* "&" *) token env tok
        in
        let v2 = map_annotated_inheritance_specifier env v2 in
        v2)
      v2
  in
  v1 :: v2 |> Common.map (fun t -> (t, None))

and map_interpolation (env : env) ((v1, v2, v3) : CST.interpolation) =
  let v1 = (* "\\(" *) token env v1 in
  let v2 = map_interpolation_contents env v2 in
  let v3 = (* ")" *) token env v3 in
  v2

and map_interpolation_contents (env : env)
    ((v1, v2) : CST.interpolation_contents) : G.expr =
  let v1 = map_value_argument env v1 in
  let v2 =
    List.concat_map
      (fun (v1, v2) ->
        let _comma = (* "," *) token env v1 in
        map_value_argument env v2)
      v2
  in
  (* The grammar allows for multiple arguments in an interpolation, but Swift
   * actually only allows one. So, ignore the rest.
   *
   * TODO should the grammar be updated to more closely match Swift here? *)
  match v1 with
  | [ G.Arg e ] -> e
  (* I (nmote) believe that the only other valid argument configuration would be
   * something like "\(_: 2)", which there is no real reason to use, and Swift
   * won't allow the others at all. *)
  | _ ->
      G.OtherExpr (("UnknownInterpolation", PI.unsafe_fake_info ""), []) |> G.e

and map_arguments (env : env) ((v1, v2) : CST.interpolation_contents) :
    G.argument list =
  let v1 = map_value_argument env v1 in
  let v2 =
    List.concat_map
      (fun (v1, v2) ->
        let _comma = (* "," *) token env v1 in
        map_value_argument env v2)
      v2
  in
  v1 @ v2

and map_key_path_component (env : env) (x : CST.key_path_component) =
  match x with
  | `Simple_id_rep_key_path_postfs (v1, v2) ->
      let v1 = map_simple_identifier env v1 in
      let v2 = Common.map (map_key_path_postfixes env) v2 in
      todo env (v1, v2)
  | `Rep1_key_path_postfs xs -> Common.map (map_key_path_postfixes env) xs

and map_key_path_postfixes (env : env) (x : CST.key_path_postfixes) =
  match x with
  | `QMARK tok -> (* "?" *) token env tok
  | `Bang tok -> (* bang *) token env tok
  | `Self tok -> (* "self" *) token env tok
  | `LBRACK_opt_value_arg_rep_COMMA_value_arg_RBRACK (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_arguments env x
        | None -> todo env ()
      in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)

and map_labeled_statement (env : env) ((v1, v2) : CST.labeled_statement) =
  let v1 =
    match v1 with
    | Some tok -> (* statement_label *) token env tok
    | None -> todo env ()
  in
  let v2 =
    match v2 with
    | `For_stmt x -> map_for_statement env x
    | `While_stmt x -> map_while_statement env x
    | `Repeat_while_stmt x -> map_repeat_while_statement env x
    | `Do_stmt x -> map_do_statement env x
    | `If_stmt x -> map_if_statement env x
    | `Guard_stmt x -> map_guard_statement env x
    | `Switch_stmt x -> map_switch_statement env x
  in
  todo env (v1, v2)

and map_lambda_function_type (env : env)
    ((v1, v2, v3, v4) : CST.lambda_function_type) : G.parameter list =
  let v1 =
    match v1 with
    | `Lambda_func_type_params x -> map_lambda_function_type_parameters env x
    | `LPAR_opt_lambda_func_type_params_RPAR (v1, v2, v3) ->
        let v1 = (* "(" *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> map_lambda_function_type_parameters env x
          | None -> todo env ()
        in
        let v3 = (* ")" *) token env v3 in
        todo env (v1, v2, v3)
  in
  let v2 =
    match v2 with
    | Some tok -> (* async_keyword_custom *) token env tok |> todo env
    | None -> None
  in
  let v3 =
    match v3 with
    | Some x -> map_throws env x |> todo env
    | None -> None
  in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let v1 = (* arrow_operator_custom *) token env v1 in
        let v2 = map_possibly_implicitly_unwrapped_type env v2 in
        todo env (v1, v2)
    | None -> None
  in
  (* TODO return info other than paremeter list *)
  v1

and map_lambda_function_type_parameters (env : env)
    ((v1, v2) : CST.lambda_function_type_parameters) : G.parameter list =
  let v1 = map_lambda_parameter env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        map_lambda_parameter env v2)
      v2
  in
  v1 :: v2

and map_lambda_literal (env : env) ((v1, v2, v3, v4, v5) : CST.lambda_literal) :
    G.expr =
  let v1 = (* "{" *) token env v1 in
  (* TODO include captures *)
  let _captures =
    match v2 with
    | Some x -> map_capture_list env x
    | None -> []
  in
  let params =
    match v3 with
    | Some (v1, v2) ->
        let v1 =
          match v1 with
          | Some x -> map_lambda_function_type env x
          | None -> []
        in
        let _in = (* "in" *) token env v2 in
        v1
    | None -> []
  in
  let body =
    let stmts =
      match v4 with
      (* TODO model implicit return for single-statement closures *)
      | Some x -> map_statements env x
      | None -> []
    in
    (* Fake brackets here since the brackets delimit the lambda expression as a
     * whole, not just the statements *)
    (* TODO consider using `in` and the closing bracket as the delimiters *)
    G.FBStmt (G.Block (G.fake_bracket stmts) |> G.s)
  in
  let v5 = (* "}" *) token env v5 in
  let def =
    {
      G.fkind = (G.LambdaKind, v1);
      fparams = params;
      (* TODO include return type if provided *)
      frettype = None;
      fbody = body;
    }
  in
  G.Lambda def |> G.e

and map_lambda_parameter (env : env) ((v1, v2) : CST.lambda_parameter) :
    G.parameter =
  let v1 =
    match v1 with
    | Some x -> map_attribute env x
    | None -> None
  in
  let v2 =
    match v2 with
    | `Self_exp tok -> (* "self" *) token env tok |> todo env
    | `Simple_id x -> G.Param (map_simple_identifier env x |> G.param_of_id)
    | `Opt_simple_id_simple_id_COLON_opt_param_modifs_poss_impl_unwr_type
        (v1, v2, v3, v4, v5) ->
        let v1 =
          match v1 with
          | Some x -> map_simple_identifier env x
          | None -> todo env ()
        in
        let v2 = map_simple_identifier env v2 in
        let v3 = (* ":" *) token env v3 in
        let v4 =
          match v4 with
          | Some x -> map_parameter_modifiers env x
          | None -> todo env ()
        in
        let v5 = map_possibly_implicitly_unwrapped_type env v5 in
        todo env (v1, v2, v3, v4, v5)
  in
  v2

and map_local_declaration (env : env) (x : CST.local_declaration) =
  match x with
  | `Local_prop_decl (v1, v2) ->
      let v1 =
        match v1 with
        | Some x -> map_locally_permitted_modifiers env x
        | None -> todo env ()
      in
      let v2 = map_modifierless_property_declaration env v2 in
      todo env (v1, v2)
  | `Local_typeas_decl (v1, v2) ->
      let v1 =
        match v1 with
        | Some x -> map_locally_permitted_modifiers env x
        | None -> todo env ()
      in
      let v2 = map_modifierless_typealias_declaration env v2 in
      todo env (v1, v2)
  | `Local_func_decl (v1, v2) ->
      let v1 =
        match v1 with
        | Some x -> map_locally_permitted_modifiers env x
        | None -> todo env ()
      in
      let v2 = map_modifierless_function_declaration env v2 in
      todo env (v1, v2)
  | `Local_class_decl (v1, v2) ->
      let v1 =
        match v1 with
        | Some x -> map_locally_permitted_modifiers env x
        | None -> todo env ()
      in
      let v2 = map_modifierless_class_declaration env v2 in
      todo env (v1, v2)

and map_local_statement (env : env) (x : CST.local_statement)
    (semi : CST.semi option) : G.stmt =
  let semi =
    match semi with
    | Some semi -> token env semi
    | None -> G.sc
  in
  match x with
  | `Exp x ->
      let expr = map_expression env x in
      G.ExprStmt (expr, semi) |> G.s
  | `Local_decl x -> map_local_declaration env x
  | `Labe_stmt x -> map_labeled_statement env x |> todo env
  | `Cont_tran_stmt x -> map_control_transfer_statement env x semi

and map_locally_permitted_modifiers (env : env)
    (xs : CST.locally_permitted_modifiers) =
  Common.map
    (fun x ->
      match x with
      | `Attr x -> map_attribute env x
      | `Loca_perm_modi x -> map_locally_permitted_modifier env x |> todo env)
    xs

and construct_class_declaration :
      'body.
      env ->
      ?kind:G.class_kind ->
      ?attrs:'attrs ->
      G.tok ->
      CST.simple_identifier ->
      CST.type_parameters option ->
      'inheritance_specifiers ->
      CST.type_constraints option ->
      'body ->
      (env -> 'body -> G.field list G.bracket) ->
      G.stmt =
 fun env ?(kind = G.Class) ?attrs class_token name tparams
     inheritance_specifiers type_constraints body map_body ->
  let name = map_simple_identifier env name in
  let tparams =
    match tparams with
    | Some x -> map_type_parameters env x
    | None -> []
  in
  let extends =
    (* Swift allows classes to have at most one superclass, followed by a list
     * of protocols that the class implements. If there is a superclass, it must
     * be the first in the list, but there may not be a superclass.
     * Unfortunately, there is no way to tell without resolving names whether
     * the first inheritance specifier is a superclass or a protocol. Because of
     * this, it seems to be the most consistent to put everything into
     * `cextends` and leave `cimplements` empty. *)
    match inheritance_specifiers with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_inheritance_specifiers env v2 in
        v2
    | None -> []
  in
  let type_constraints =
    (* TODO handle type constraints. Type constraints can apply to type
     * parameters in the current definition, or to other type parameters in
     * scope. *)
    match type_constraints with
    | Some x -> map_type_constraints env x |> todo env
    | None -> ()
  in
  let body = map_body env body in
  let entity = G.basic_entity ?attrs ~tparams name in
  let definition_kind =
    {
      G.ckind = (kind, class_token);
      cextends = extends;
      cimplements = [];
      cmixins = [];
      cparams = [];
      cbody = body;
    }
  in
  G.DefStmt (entity, G.ClassDef definition_kind) |> G.s

and map_modifierless_class_declaration (env : env)
    (x : CST.modifierless_class_declaration) =
  match x with
  | `Choice_class_simple_id_opt_type_params_opt_COLON_inhe_specis_opt_type_consts_class_body
      (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (* TODO differentiate between class and struct? Maybe use RecordClass
         * attribute? *)
        match v1 with
        | `Class tok -> (* "class" *) token env tok
        | `Struct tok -> (* "struct" *) token env tok
      in
      construct_class_declaration env v1 v2 v3 v4 v5 v6 map_class_body
  | `Exte_user_type_opt_type_params_opt_COLON_inhe_specis_opt_type_consts_class_body
      (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "extension" *) token env v1 in
      let v2 = map_user_type env v2 in
      let v3 =
        match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ()
      in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = map_inheritance_specifiers env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v5 =
        match v5 with
        | Some x -> map_type_constraints env x
        | None -> todo env ()
      in
      let v6 = map_class_body env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Opt_indi_enum_simple_id_opt_type_params_opt_COLON_inhe_specis_opt_type_consts_enum_class_body
      (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | Some tok -> (* "indirect" *) token env tok |> todo env
        | None -> ()
      in
      let v2 = (* "enum" *) token env v2 in
      let attrs = [ G.KeywordAttr (G.EnumClass, v2) ] in
      construct_class_declaration env ~attrs v2 v3 v4 v5 v6 v7
        map_enum_class_body

and map_modifierless_function_declaration (env : env)
    ((v1, v2) : CST.modifierless_function_declaration) =
  let v2 = map_function_body env v2 in
  let in_class = todo env () in
  let v1 = map_modifierless_function_declaration_no_body env ~in_class v1 v2 in
  todo env (v1, v2)

and map_modifierless_function_declaration_no_body (env : env) ~in_class
    ((v1, v2, v3, v4, v5, v6, v7) :
      CST.modifierless_function_declaration_no_body) (body : G.function_body) =
  let v1 =
    match v1 with
    | `Cons_func_decl x -> map_constructor_function_decl env x
    | `Non_cons_func_decl x -> map_non_constructor_function_decl env x
  in
  let v2 =
    match v2 with
    | Some x -> map_type_parameters env x
    | None -> []
  in
  let v3 = map_function_value_parameters env v3 in
  let v4 =
    match v4 with
    | Some tok -> (* async_keyword_custom *) token env tok |> todo env
    | None -> ()
  in
  let v5 =
    match v5 with
    | Some x -> map_throws env x |> todo env
    | None -> ()
  in
  let v6 =
    match v6 with
    | Some (v1, v2) ->
        let v1 = (* arrow_operator_custom *) token env v1 in
        let v2 = map_possibly_implicitly_unwrapped_type env v2 in
        Some v2
    | None -> None
  in
  let v7 =
    match v7 with
    | Some x -> map_type_constraints env x |> todo env
    | None -> ()
  in
  let entity = G.basic_entity ~tparams:v2 v1 in
  let kind = if in_class then G.Method else G.Function in
  let definition_kind =
    G.FuncDef
      { fkind = (kind, snd v1); fparams = v3; frettype = v6; fbody = body }
  in
  G.DefStmt (entity, definition_kind) |> G.s

and map_single_modifierless_property_declaration (env : env)
    ((pat, tannot, tconstraints, init) :
      CST.single_modifierless_property_declaration) : G.stmt =
  let pat = map_no_expr_pattern_already_bound env pat in
  (* TODO Desugar single-element tuples? *)
  let pat =
    let entity_name =
      match pat with
      (* Unwrap PatId into a regular name. map_property_binding_pattern has to
       * return a pattern because it is recursive, and might need to compose its
       * return value into larger patterns. So, we have to unwrap here. *)
      | G.PatId (id, id_info) -> G.EN (G.Id (id, id_info))
      | pattern -> G.EPattern pattern
    in
    { G.name = entity_name; attrs = []; tparams = [] }
  in
  let tannot =
    match tannot with
    | Some x -> Some (map_type_annotation env x)
    | None -> None
  in
  let tconstraints =
    match tconstraints with
    | Some x -> map_type_constraints env x |> todo env
    | None -> ()
  in
  let init =
    match init with
    | Some x ->
        let x =
          match x with
          | `Equal_sign_exp (v1, v2) ->
              let v1 = (* eq_custom *) token env v1 in
              let v2 = map_expression env v2 in
              v2
          | `Comp_prop x -> map_computed_property env x
        in
        Some x
    | None -> None
  in
  G.DefStmt (pat, G.VarDef { vinit = init; vtype = tannot }) |> G.s

and map_modifierless_property_declaration (env : env)
    ((v1, v2, v3) : CST.modifierless_property_declaration) : G.stmt list =
  (* kind *)
  let v1 = map_possibly_async_binding_pattern_kind env v1 in
  let stmt1 = map_single_modifierless_property_declaration env v2 in
  let stmts =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        map_single_modifierless_property_declaration env v2)
      v3
  in
  stmt1 :: stmts

and map_modifierless_typealias_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.modifierless_typealias_declaration) =
  let v1 = (* "typealias" *) token env v1 in
  let v2 = map_simple_identifier env v2 in
  let v3 =
    match v3 with
    | Some x -> map_type_parameters env x
    | None -> []
  in
  let v4 = (* eq_custom *) token env v4 in
  let v5 = map_type_ env v5 in
  G.DefStmt
    (G.basic_entity ~tparams:v3 v2, G.TypeDef { G.tbody = G.AliasType v5 })
  |> G.s

and map_modifiers (env : env) (xs : CST.modifiers) =
  Common.map
    (fun x ->
      match x with
      | `Non_local_scope_modi x -> map_non_local_scope_modifier env x
      | `Rep1_choice_attr x -> map_locally_permitted_modifiers env x |> todo env)
    xs

and map_navigable_type_expression (env : env)
    (x : CST.navigable_type_expression) =
  match x with
  | `User_type x -> map_user_type env x
  | `Array_type x -> map_array_type env x
  | `Dict_type x -> map_dictionary_type env x

and map_navigation_expression (env : env) ((v1, v2) : CST.navigation_expression)
    : G.expr =
  let v1 =
    match v1 with
    | `Navi_type_exp x ->
        (* This happens with constructs like `Dictionary<Int, Int>.thing`. This
         * structure is documented here:
         * https://docs.swift.org/swift-book/ReferenceManual/Expressions.html#ID400
         *
         * > An explicit member expression allows access to the members of a
         * > named type, a tuple, or a module.
         *
         * It's quite clear that a type can appear in this position, but the
         * generic AST expects an expression. *)
        let type_ = map_navigable_type_expression env x in
        expr_of_type type_
    | `Exp x -> map_expression env x
  in
  let dot, suffix = map_navigation_suffix env v2 in
  G.DotAccess (v1, dot, suffix) |> G.e

and map_tuple_pattern_item (env : env) (x : CST.tuple_pattern_item) =
  match x with
  | `Simple_id_COLON_bind_pat_with_expr (v1, v2, v3) ->
      let v1 = map_bound_identifier env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_switch_pattern env v3 in
      todo env (v1, v2, v3)
  | `Bind_pat_with_expr x -> map_switch_pattern env x

and map_tuple_pattern (env : env) ((v1, v2, v3, v4) : CST.tuple_pattern) :
    G.pattern =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_tuple_pattern_item env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_tuple_pattern_item env v2 in
        v2)
      v3
  in
  let v4 = (* ")" *) token env v4 in
  G.PatTuple (v1, v2 :: v3, v4)

and map_no_expr_pattern_already_bound (env : env)
    ((v1, v2) : CST.no_expr_pattern_already_bound) =
  let v1 =
    match v1 with
    | `Univ_allo_pat x -> map_universally_allowed_pattern env x
    | `Bound_id x ->
        let id = map_bound_identifier env x in
        let id_info = G.empty_id_info () in
        G.PatId (id, id_info)
  in
  let v2 =
    match v2 with
    | Some tok -> (* "?" *) token env tok |> todo env
    | None -> ()
  in
  v1

and map_binding_pattern_no_expr (env : env)
    ((v1, v2) : CST.binding_pattern_no_expr) =
  let v1 =
    match v1 with
    | `Univ_allo_pat x -> map_universally_allowed_pattern env x
    | `Bind_pat x -> map_binding_pattern env x
    | `Bound_id x -> map_bound_identifier env x |> todo env
  in
  let v2 =
    match v2 with
    | Some tok -> (* "?" *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2)

and map_universally_allowed_pattern (env : env)
    (x : CST.universally_allowed_pattern) : G.pattern =
  match x with
  | `Wild_pat tok -> (* "_" *) token env tok |> todo env
  | `Tuple_pat x -> map_tuple_pattern env x
  | `Type_cast_pat x -> map_type_casting_pattern env x
  | `Case_pat (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some tok -> (* "case" *) token env tok
        | None -> todo env ()
      in
      let v2 =
        match v2 with
        | Some x -> map_user_type env x
        | None -> todo env ()
      in
      let v3 = (* dot_custom *) token env v3 in
      let v4 = map_bound_identifier env v4 in
      let v5 =
        match v5 with
        | Some x -> map_tuple_pattern env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5)

and map_parameter (env : env) ((v1, v2, v3, v4, v5, v6) : CST.parameter) default
    =
  let v1 =
    (* If present, this is the externally-visible label for this parameter. In
     * this context, the local name will be more relevant, so since we can
     * currently only represent one in the AST, let's omit this one. *)
    (* TODO include the externally-visible param label in the AST? *)
    match v1 with
    | Some x ->
        let _ = map_simple_identifier env x in
        ()
    | None -> ()
  in
  let v2 = map_simple_identifier env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 =
    match v4 with
    | Some x -> map_parameter_modifiers env x |> todo env
    | None -> ()
  in
  let v5 = map_possibly_implicitly_unwrapped_type env v5 in
  let param = G.param_of_id ~pdefault:default ~ptype:(Some v5) v2 in
  match v6 with
  | Some tok ->
      let dots = (* three_dot_operator_custom *) token env tok in
      G.ParamRest (dots, param)
  | None -> G.Param param

and map_possibly_implicitly_unwrapped_type (env : env)
    ((v1, v2) : CST.possibly_implicitly_unwrapped_type) =
  let v1 = map_type_ env v1 in
  let v2 =
    match v2 with
    | Some tok -> (* "!" *) token env tok |> todo env
    | None -> ()
  in
  v1

and map_primary_expression (env : env) (x : CST.primary_expression) : G.expr =
  match x with
  | `Tuple_exp x -> map_tuple_expression env x
  | `Basic_lit x -> map_basic_literal env x
  | `Lambda_lit x -> map_lambda_literal env x
  | `Spec_lit x -> map_special_literal env x |> todo env
  | `Play_lit (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | `HASH_38ce0d0 tok -> (* "#colorLiteral" *) token env tok
        | `HASH_34ae46a tok -> (* "#fileLiteral" *) token env tok
        | `HASH_71f9c0e tok -> (* "#imageLiteral" *) token env tok
      in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_simple_identifier env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 =
        Common.map
          (fun (v1, v2, v3, v4) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_simple_identifier env v2 in
            let v3 = (* ":" *) token env v3 in
            let v4 = map_expression env v4 in
            todo env (v1, v2, v3, v4))
          v6
      in
      let v7 = (* ")" *) token env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Array_lit (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = map_expression env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  map_expression env v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let v3 =
        match v3 with
        | Some tok -> Some ((* "," *) token env tok)
        | None -> None
      in
      let v4 = (* "]" *) token env v4 in
      G.Container (G.Array, (v1, v2, v4)) |> G.e
  | `Dict_lit (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | `COLON tok ->
            let _colon = (* ":" *) token env tok in
            (* Empty dict literal *)
            []
        | `Dict_lit_item_rep_COMMA_dict_lit_item (v1, v2) ->
            let v1 = map_dictionary_literal_item env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  map_dictionary_literal_item env v2)
                v2
            in
            v1 :: v2
      in
      let () =
        (* Optional trailing comma *)
        match v3 with
        | Some tok -> (* "," *) ignore (token env tok)
        | None -> ()
      in
      let v4 = (* "]" *) token env v4 in
      G.Container (G.Dict, (v1, v2, v4)) |> G.e
  | `Self_exp tok -> map_self_expression env tok
  | `Super_exp v1 -> G.IdSpecial (G.Super, (* "super" *) token env v1) |> G.e
  | `Try_exp (v1, v2) ->
      let v1 = map_try_operator env v1 in
      let v2 =
        match v2 with
        | `Exp x -> map_expression env x
        | `Bin_exp x -> map_binary_expression env x
        | `Call_exp x -> map_call_expression env x
        | `Tern_exp x -> map_ternary_expression env x
      in
      (* This is not like a try statement in most languages.
       * https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html *)
      (* TODO differentiate between the try kinds? *)
      G.OtherExpr (("Try", v1), [ G.E v2 ]) |> G.e
  | `Await_exp (v1, v2) ->
      let v1 = (* "await" *) token env v1 in
      let v2 =
        match v2 with
        | `Exp x -> map_expression env x
        | `Call_exp x -> map_call_expression env x
        | `Tern_exp x -> map_ternary_expression env x
      in
      G.Await (v1, v2) |> G.e
  | `Refe_op x -> map_referenceable_operator env x |> todo env
  | `Key_path_exp (v1, v2, v3) ->
      let v1 = (* "\\" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `Simple_user_type x ->
                let id, targs = map_simple_user_type env x in
                let name = H2.name_of_id id in
                let name = H2.add_type_args_opt_to_name name targs in
                G.TyN name |> G.t
            | `Array_type x -> map_array_type env x
            | `Dict_type x -> map_dictionary_type env x)
        | None -> todo env ()
      in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "." *) token env v1 in
            let v2 = map_key_path_component env v2 in
            todo env (v1, v2))
          v3
      in
      todo env (v1, v2, v3)
  | `Key_path_str_exp (v1, v2, v3, v4) ->
      let v1 = (* "#keyPath" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Three_dot_op tok ->
      let tok = (* three_dot_operator_custom *) token env tok in
      if in_pattern env then G.Ellipsis tok |> G.e
      else
        let op = (G.Range, tok) in
        G.opcall op [ G.L (G.Null tok) |> G.e; G.L (G.Null tok) |> G.e ]

and map_self_expression (env : env) tok =
  G.IdSpecial (G.Self, (* "self" *) token env tok) |> G.e

and map_property_declaration (env : env) ((v1, v2) : CST.property_declaration) =
  let v1 =
    match v1 with
    | Some x -> map_modifiers env x |> todo env
    | None -> ()
  in
  let v2 = map_modifierless_property_declaration env v2 in
  v2

and map_protocol_body (env : env) ((v1, v2, v3) : CST.protocol_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_protocol_member_declarations env x
    | None -> []
  in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_protocol_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.protocol_declaration) =
  let v1 =
    match v1 with
    | Some x -> map_modifiers env x |> todo env
    | None -> ()
  in
  let v2 = (* "protocol" *) token env v2 in
  construct_class_declaration env ~kind:G.Interface v2 v3 None None v6 v7
    map_protocol_body

and map_protocol_member_declaration (env : env)
    (x : CST.protocol_member_declaration) =
  match x with
  | `Body_func_decl_opt_func_body (v1, v2) ->
      let v2 =
        match v2 with
        | Some x -> map_function_body env x
        | None -> G.FBNothing
      in
      let v1 = map_bodyless_function_declaration env ~in_class:true v1 v2 in
      todo env v1
  | `Deinit_decl x -> map_deinit_declaration env x
  | `Prot_prop_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ()
      in
      let v2 = map_binding_kind_and_pattern env v2 in
      let v3 =
        match v3 with
        | Some x -> map_type_annotation env x
        | None -> todo env ()
      in
      let v4 =
        match v4 with
        | Some x -> map_type_constraints env x
        | None -> todo env ()
      in
      let v5 = map_protocol_property_requirements env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Typeas_decl x -> map_typealias_declaration env x
  | `Asso_decl x -> map_associatedtype_declaration env x
  | `Subs_decl x -> map_subscript_declaration env x

and map_protocol_member_declarations (env : env)
    ((v1, v2, v3) : CST.protocol_member_declarations) =
  let v1 = map_protocol_member_declaration env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* semi *) token env v1 in
        let v2 = map_protocol_member_declaration env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with
    | Some tok -> (* semi *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_raw_str_interpolation (env : env)
    ((v1, v2, v3) : CST.raw_str_interpolation) =
  let v1 = (* pattern \\#*\( *) token env v1 in
  let v2 = map_interpolation_contents env v2 in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_repeat_while_statement (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.repeat_while_statement) =
  let v1 = (* "repeat" *) token env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_statements env x
    | None -> todo env ()
  in
  let v4 = (* "}" *) token env v4 in
  let v5 = (* "while" *) token env v5 in
  let v6 = map_if_condition_sequence_item env v6 in
  let v7 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_if_condition_sequence_item env v2 in
        todo env (v1, v2))
      v7
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_simple_user_type (env : env) (x : CST.simple_user_type) :
    G.ident * G.type_arguments option =
  match x with
  | `Rectype (v1, v2) -> (
      let v1 = map_simple_identifier env v1 in
      match v2 with
      | Some x -> (v1, Some (map_type_arguments env x))
      | None -> (v1, None))

and map_statements (env : env) ((v1, v2, v3) : CST.statements) =
  let stmts = associate_statement_semis v1 v2 v3 in
  Common.map (fun (stmt, semi) -> map_local_statement env stmt semi) stmts

and map_string_literal (env : env) (x : CST.string_literal) : G.expr =
  match x with
  | `Line_str_lit (v1, v2, v3) ->
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        Common.map
          (fun x ->
            match x with
            | `Line_str_content x ->
                Common.Left3 (map_line_string_content env x)
            | `Interp x -> Common.Middle3 (map_interpolation env x))
          v2
      in
      let v3 = (* "\"" *) token env v3 in
      (* TODO combine multiple Line_str_content entries into a single string? *)
      G.interpolated (v1, v2, v3)
  | `Multi_line_str_lit (v1, v2, v3) ->
      let v1 = (* "\"\"\"" *) token env v1 in
      let v2 =
        Common.map
          (fun x ->
            match x with
            | `Multi_line_str_content x ->
                Common.Left3 (map_multi_line_string_content env x)
            | `Interp x -> Common.Middle3 (map_interpolation env x))
          v2
      in
      let v3 = (* "\"\"\"" *) token env v3 in
      (* TODO combine multiple Multi_line_str_content entries into a single
       * string? *)
      G.interpolated (v1, v2, v3)
  | `Raw_str_lit (v1, v2) ->
      let v1 =
        Common.map
          (fun (v1, v2, v3) ->
            let v1 = (* raw_str_part *) token env v1 in
            let v2 = map_raw_str_interpolation env v2 in
            let v3 =
              match v3 with
              | Some tok -> (* raw_str_continuing_indicator *) token env tok
              | None -> todo env ()
            in
            todo env (v1, v2, v3))
          v1
      in
      let v2 = (* raw_str_end_part *) token env v2 in
      todo env (v1, v2)

and map_subscript_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.subscript_declaration) =
  let v1 =
    match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ()
  in
  let v2 = (* "subscript" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_type_parameters env x
    | None -> todo env ()
  in
  let v4 = map_function_value_parameters env v4 in
  let v5 =
    match v5 with
    | Some (v1, v2) ->
        let v1 = (* arrow_operator_custom *) token env v1 in
        let v2 = map_possibly_implicitly_unwrapped_type env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v6 =
    match v6 with
    | Some x -> map_type_constraints env x
    | None -> todo env ()
  in
  let v7 = map_computed_property env v7 in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_switch_entry (env : env) ((v1, v2, v3, v4, v5) : CST.switch_entry) =
  let v1 =
    match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ()
  in
  let v2 =
    match v2 with
    | `Case_switch_pat_opt_where_kw_exp_rep_COMMA_switch_pat (v1, v2, v3, v4) ->
        let v1 = (* "case" *) token env v1 in
        let v2 = map_switch_pattern env v2 in
        let v3 =
          match v3 with
          | Some x -> map_where_clause env x
          | None -> todo env ()
        in
        let v4 =
          Common.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_switch_pattern env v2 in
              todo env (v1, v2))
            v4
        in
        todo env (v1, v2, v3, v4)
    | `Defa_kw tok -> (* default_keyword *) token env tok
  in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_statements env v4 in
  let v5 =
    match v5 with
    | Some tok -> (* "fallthrough" *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2, v3, v4, v5)

and map_switch_pattern (env : env) (x : CST.switch_pattern) =
  map_binding_pattern_with_expr env x

and map_switch_statement (env : env)
    ((v1, v2, v3, v4, v5) : CST.switch_statement) =
  let v1 = (* "switch" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 = Common.map (map_switch_entry env) v4 in
  let v5 = (* "}" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_expr_hack_at_ternary_binary_suffix (env : env)
    (x : CST.expr_hack_at_ternary_binary_suffix) =
  match x with
  | `Exp x -> map_expression env x
  | `Expr_hack_at_tern_bin_call (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_expr_hack_at_ternary_binary_call_suffix env v2 in
      G.Call (v1, v2) |> G.e

and map_expr_hack_at_ternary_binary_call_suffix (env : env)
    (x : CST.expr_hack_at_ternary_binary_call_suffix) =
  map_value_arguments env x

and map_ternary_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.ternary_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "?" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_expr_hack_at_ternary_binary_suffix env v5 in
  G.Conditional (v1, v3, v5) |> G.e

and map_throw_statement (env : env) ((v1, v2) : CST.throw_statement) =
  let v1 = (* "throw" *) token env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_tuple_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.tuple_expression) : G.expr =
  let v1 = (* "(" *) token env v1 in
  (* TODO handle labels *)
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_simple_identifier env v1 in
        let v2 = (* ":" *) token env v2 in
        ()
    | None -> ()
  in
  let v3 = map_expression env v3 in
  let v4 =
    Common.map
      (fun (v1, v2, v3) ->
        let v1 = (* "," *) token env v1 in
        let v2 =
          match v2 with
          | Some (v1, v2) ->
              let v1 = map_simple_identifier env v1 in
              let v2 = (* ":" *) token env v2 in
              ()
          | None -> ()
        in
        let v3 = map_expression env v3 in
        v3)
      v4
  in
  let v5 = (* ")" *) token env v5 in
  let exprs = v3 :: v4 in
  match exprs with
  (* The grammar doesn't differentiate, but according to the Swift spec:
   *
   * "A single expression inside parentheses is a parenthesized expression."
   *
   * https://docs.swift.org/swift-book/ReferenceManual/Expressions.html#ID552
   *
   * See also
   * https://docs.swift.org/swift-book/ReferenceManual/Expressions.html#ID395
   *)
  | [ e ] -> e
  | _ -> G.Container (G.Tuple, (v1, exprs, v5)) |> G.e

and map_tuple_type (env : env) ((v1, v2, v3) : CST.tuple_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_tuple_type_item env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_tuple_type_item env v2 in
              todo env (v1, v2))
            v2
        in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_tuple_type_item (env : env) ((v1, v2, v3) : CST.tuple_type_item) =
  let v1 =
    match v1 with
    | Some x -> map_tuple_type_item_identifier env x
    | None -> todo env ()
  in
  let v2 =
    match v2 with
    | Some x -> map_parameter_modifiers env x
    | None -> todo env ()
  in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_type_ (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Rectype (v1, v2) ->
      let _v1 =
        match v1 with
        | Some x -> map_type_modifiers env x |> todo env
        | None -> None
      in
      let v2 = map_unannotated_type env v2 in
      (* TODO include type modifiers *)
      v2

and map_type_annotation (env : env) ((v1, v2) : CST.type_annotation) : G.type_ =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_possibly_implicitly_unwrapped_type env v2 in
  v2

and map_type_arguments (env : env) (x : CST.type_arguments) : G.type_arguments =
  match x with
  | `Rectype (v1, v2, v3, v4) ->
      let v1 = (* "<" *) token env v1 in
      let v2 = G.TA (map_type_ env v2) in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            G.TA (map_type_ env v2))
          v3
      in
      let v4 = (* ">" *) token env v4 in
      (v1, v2 :: v3, v4)

and map_type_constraint (env : env) (x : CST.type_constraint) =
  match x with
  | `Inhe_cons (v1, v2, v3, v4) ->
      let v1 = Common.map (map_attribute env) v1 in
      let v2 = map_identifier env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_possibly_implicitly_unwrapped_type env v4 in
      todo env (v1, v2, v3, v4)
  | `Equa_cons (v1, v2, v3, v4) ->
      let v1 = Common.map (map_attribute env) v1 in
      let v2 = map_identifier env v2 in
      let v3 =
        match v3 with
        | `Equal_sign tok -> (* eq_custom *) token env tok
        | `Eq_eq tok -> (* eq_eq_custom *) token env tok
      in
      let v4 = map_type_ env v4 in
      todo env (v1, v2, v3, v4)

and map_type_constraints (env : env) ((v1, v2, v3) : CST.type_constraints) =
  let v1 = (* where_keyword *) token env v1 in
  let v2 = map_type_constraint env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_type_constraint env v2 in
        todo env (v1, v2))
      v3
  in
  todo env (v1, v2, v3)

and map_type_level_declaration (env : env) (x : CST.type_level_declaration) :
    G.stmt list =
  match x with
  | `Import_decl x -> [ map_import_declaration env x ]
  | `Prop_decl x -> map_property_declaration env x
  | `Typeas_decl x -> [ map_typealias_declaration env x ]
  | `Func_decl x -> [ map_function_declaration env ~in_class:true x ]
  | `Class_decl x -> [ map_class_declaration env x ]
  | `Prot_decl x -> [ map_protocol_declaration env x ]
  | `Deinit_decl x -> [ map_deinit_declaration env x ]
  | `Subs_decl x -> [ map_subscript_declaration env x ]
  | `Op_decl x -> map_operator_declaration env x
  | `Prec_group_decl x -> map_precedence_group_declaration env x
  | `Asso_decl x -> [ map_associatedtype_declaration env x ]

and map_type_modifiers (env : env) (x : CST.type_modifiers) =
  match x with
  | `Rectype x -> map_type_parameter_modifiers env x

and map_type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let v1 =
    match v1 with
    | Some x -> map_type_parameter_modifiers env x |> todo env
    | None -> ()
  in
  let v2 = map_simple_identifier env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        [ v2 ]
    | None -> []
  in
  G.tparam_of_id ~tp_bounds:v3 v2

and map_type_parameter_modifiers (env : env) (xs : CST.type_parameter_modifiers)
    =
  Common.map (map_attribute env) xs

and map_type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_type_parameter env v2 in
        v2)
      v3
  in
  let v4 = (* ">" *) token env v4 in
  v2 :: v3

and map_typealias_declaration (env : env) ((v1, v2) : CST.typealias_declaration)
    =
  let v1 =
    match v1 with
    | Some x -> map_modifiers env x |> todo env
    | None -> ()
  in
  let v2 = map_modifierless_typealias_declaration env v2 in
  v2

and map_unannotated_type (env : env) (x : CST.unannotated_type) =
  match x with
  | `User_type x -> map_user_type env x
  | `Tuple_type x -> map_tuple_type env x
  | `Func_type x -> map_function_type env x
  | `Array_type x -> map_array_type env x
  | `Dict_type x -> map_dictionary_type env x
  | `Opt_type (v1, v2) ->
      let v1 =
        match v1 with
        | `User_type x -> map_user_type env x
        | `Tuple_type x -> map_tuple_type env x
        | `Array_type x -> map_array_type env x
        | `Dict_type x -> map_dictionary_type env x
      in
      let v2 = Common.map (token env (* "?" *)) v2 in
      todo env (v1, v2)
  | `Meta (v1, v2, v3) ->
      let v1 = map_unannotated_type env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        match v3 with
        | `Type tok -> (* "Type" *) token env tok
        | `Prot tok -> (* "Protocol" *) token env tok
      in
      todo env (v1, v2, v3)
  | `Opaque_type (v1, v2) ->
      let v1 = (* "some" *) token env v1 in
      let v2 = map_user_type env v2 in
      todo env (v1, v2)
  | `Prot_comp_type (v1, v2) ->
      let v1 = map_unannotated_type env v1 in
      let v2 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "&" *) token env v1 in
            let v2 = map_unannotated_type env v2 in
            todo env (v1, v2))
          v2
      in
      todo env (v1, v2)

and map_unary_expression (env : env) (x : CST.unary_expression) : G.expr =
  match x with
  | `Post_exp (e, op) ->
      let e = map_expression env e in
      map_postfix_unary_operator env op e
  | `Call_exp x -> map_call_expression env x
  | `Cons_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `Array_type x -> map_array_type env x
        | `Dict_type x -> map_dictionary_type env x
        | `User_type x -> map_user_type env x
      in
      let v2 = map_constructor_suffix env v2 in
      G.New (G.fake "new", v1, v2) |> G.e
  | `Navi_exp x -> map_navigation_expression env x
  | `Prefix_exp (v1, v2) ->
      let e = map_expression env v2 in
      map_prefix_unary_operator env v1 e
  | `As_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      (* TODO differentiate between the `as` kinds *)
      let v2 = map_as_operator env v2 in
      let v3 = map_type_ env v3 in
      G.Cast (v3, v2, v1) |> G.e
  | `Sele_exp (v1, v2, v3, v4, v5) ->
      let v1 = (* "#selector" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> (
            match x with
            | `Gett tok -> (* "getter:" *) token env tok
            | `Sett tok -> (* "setter:" *) token env tok)
        | None -> todo env ()
      in
      let v4 = map_expression env v4 in
      let v5 = (* ")" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Open_start_range_exp (v1, v2) ->
      let v1 = map_range_operator env v1 in
      let v2 = map_expression env v2 in
      let op = (G.Range, v1) in
      (* TODO differentiate between different range operators? Ruby currently
       * does not. *)
      G.opcall op [ G.L (G.Null v1) |> G.e; v2 ]
  | `Open_end_range_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = (* three_dot_operator_custom *) token env v2 in
      let op = (G.Range, v2) in
      G.opcall op [ v1; G.L (G.Null v2) |> G.e ]

and map_user_type (env : env) (x : CST.user_type) : G.type_ =
  match x with
  | `Rectype (v1, v2) ->
      let v1 = map_simple_user_type env v1 in
      let v2 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* dot_custom *) token env v1 in
            map_simple_user_type env v2)
          v2
      in
      let name = H2.name_of_ids_with_opt_typeargs (v1 :: v2) in
      G.TyN name |> G.t

and map_value_argument (env : env) ((v1, v2) : CST.value_argument) :
    G.argument list =
  let v1 =
    match v1 with
    | Some x -> Some (map_type_modifiers env x) |> todo env
    | None -> None
  in
  let v2 =
    match v2 with
    | `Rep1_simple_id_COLON xs ->
        (* This isn't exactly a function *call*. Simply providing the labels for
         * the arguments creates a new function that can be called without later
         * providing labels for the actual arguments, but it does not call the
         * function in question. *)
        Common.map
          (fun (id, colon) ->
            let id = map_simple_identifier env id in
            let _colon = (* ":" *) token env colon in
            G.OtherArg (("LabelArguments", snd id), [ G.I id ]))
          xs
    | `Opt_choice_simple_id_COLON_exp (label, expr) -> (
        let expr = map_expression env expr in
        match label with
        | Some (name, colon) ->
            let name =
              match name with
              | `Simple_id x -> map_simple_identifier env x
              | `Async tok ->
                  (* TODO It might be worth handling this specially, since it's
                   * special-cased in the grammar. *)
                  (* "async" *)
                  str env tok
            in
            let _colon = (* ":" *) token env colon in
            [ G.ArgKwd (name, expr) ]
        | None -> [ G.Arg expr ])
  in
  v2

and map_value_arguments (env : env) (v1 : CST.value_arguments) : G.arguments =
  match v1 with
  | `LPAR_opt_value_arg_rep_COMMA_value_arg_RPAR x ->
      map_constructor_value_arguments env x
  | `LBRACK_opt_value_arg_rep_COMMA_value_arg_RBRACK (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_arguments env x
        | None -> []
      in
      let v3 = (* "]" *) token env v3 in
      (v1, v2, v3)

and map_where_clause (env : env) ((v1, v2) : CST.where_clause) =
  let v1 = (* where_keyword *) token env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_while_statement (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.while_statement) =
  let v1 = (* "while" *) token env v1 in
  let v2 = map_if_condition_sequence_item env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_if_condition_sequence_item env v2 in
        todo env (v1, v2))
      v3
  in
  let v4 = (* "{" *) token env v4 in
  let v5 =
    match v5 with
    | Some x -> map_statements env x
    | None -> todo env ()
  in
  let v6 = (* "}" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

let map_global_declaration (env : env) (x : CST.global_declaration) :
    G.stmt list =
  match x with
  | `Import_decl x -> [ map_import_declaration env x ]
  | `Prop_decl x -> map_property_declaration env x
  | `Typeas_decl x -> [ map_typealias_declaration env x ]
  | `Func_decl x -> [ map_function_declaration env ~in_class:false x ]
  | `Class_decl x -> [ map_class_declaration env x ]
  | `Prot_decl x -> [ map_protocol_declaration env x ]
  | `Op_decl x -> map_operator_declaration env x
  | `Prec_group_decl x -> map_precedence_group_declaration env x
  | `Asso_decl x -> [ map_associatedtype_declaration env x ]

let map_top_level_statement (env : env) (x : CST.top_level_statement)
    (semi : CST.semi option) =
  match x with
  | `Exp x ->
      let expr = map_expression env x in
      let semi =
        match semi with
        | Some tok -> token env tok
        | None -> G.sc
      in
      [ G.ExprStmt (expr, semi) |> G.s ]
  | `Global_decl x -> map_global_declaration env x
  | `Labe_stmt x -> map_labeled_statement env x
  | `Throw_stmt x -> map_throw_statement env x |> todo env

let map_source_file (env : env) ((_shebang, program) : CST.source_file) : G.any
    =
  let stmts =
    match program with
    | None -> []
    | Some (fst_stmt, stmts, last_semi) ->
        associate_statement_semis fst_stmt stmts last_semi
  in
  let stmts =
    List.concat_map
      (fun (stmt, semi) -> map_top_level_statement env stmt semi)
      stmts
  in
  G.Pr stmts

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_swift.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Program } in
      match map_source_file env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_swift.Parse.string str)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = Pattern } in
      match map_source_file env cst with
      | G.Pr [ x ] -> G.S x
      | G.Pr xs -> G.Ss xs
      | G.Ss [ x ] -> G.S x
      | x -> x)

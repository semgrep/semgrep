(**
   Boilerplate to be used as a template when mapping the hack CST
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

let map_heredoc_end (env : env) (tok : CST.heredoc_end) =
  (* heredoc_end *) token env tok

let map_pat_466b599 (env : env) (tok : CST.pat_466b599) =
  (* pattern function\s*\( *) token env tok

let map_false_ (env : env) (x : CST.false_) =
  (match x with
  | `False_68934a3 tok -> R.Case ("False_68934a3",
      (* "false" *) token env tok
    )
  | `False_f8320b2 tok -> R.Case ("False_f8320b2",
      (* "False" *) token env tok
    )
  | `FALSE tok -> R.Case ("FALSE",
      (* "FALSE" *) token env tok
    )
  )

let map_pat_b6fe07e (env : env) (tok : CST.pat_b6fe07e) =
  (* pattern <\?[hH][hH] *) token env tok

let map_xhp_identifier (env : env) (tok : CST.xhp_identifier) =
  (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok

let map_string_ (env : env) (tok : CST.string_) =
  (* string *) token env tok

let map_type_modifier (env : env) (x : CST.type_modifier) =
  (match x with
  | `AT tok -> R.Case ("AT",
      (* "@" *) token env tok
    )
  | `QMARK tok -> R.Case ("QMARK",
      (* "?" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  )

let map_float_ (env : env) (tok : CST.float_) =
  (* float *) token env tok

let map_integer (env : env) (tok : CST.integer) =
  (* integer *) token env tok

let map_heredoc_start_newline (env : env) (tok : CST.heredoc_start_newline) =
  (* heredoc_start_newline *) token env tok

let map_scope_identifier (env : env) (x : CST.scope_identifier) =
  (match x with
  | `Self tok -> R.Case ("Self",
      (* "self" *) token env tok
    )
  | `Parent tok -> R.Case ("Parent",
      (* "parent" *) token env tok
    )
  | `Static tok -> R.Case ("Static",
      (* "static" *) token env tok
    )
  )

let map_use_type (env : env) (x : CST.use_type) =
  (match x with
  | `Name tok -> R.Case ("Name",
      (* "namespace" *) token env tok
    )
  | `Func tok -> R.Case ("Func",
      (* "function" *) token env tok
    )
  | `Type tok -> R.Case ("Type",
      (* "type" *) token env tok
    )
  | `Const tok -> R.Case ("Const",
      (* "const" *) token env tok
    )
  )

let map_xhp_category_identifier (env : env) (tok : CST.xhp_category_identifier) =
  (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok

let map_true_ (env : env) (x : CST.true_) =
  (match x with
  | `True_b326b50 tok -> R.Case ("True_b326b50",
      (* "true" *) token env tok
    )
  | `True_f827cf4 tok -> R.Case ("True_f827cf4",
      (* "True" *) token env tok
    )
  | `TRUE tok -> R.Case ("TRUE",
      (* "TRUE" *) token env tok
    )
  )

let map_heredoc_end_newline (env : env) (tok : CST.heredoc_end_newline) =
  (* heredoc_end_newline *) token env tok

let map_heredoc_body (env : env) (tok : CST.heredoc_body) =
  (* heredoc_body *) token env tok

let map_semgrep_identifier (env : env) (tok : CST.semgrep_identifier) =
  (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok

let map_xhp_comment (env : env) (tok : CST.xhp_comment) =
  (* xhp_comment *) token env tok

let map_visibility_modifier (env : env) (x : CST.visibility_modifier) =
  (match x with
  | `Public tok -> R.Case ("Public",
      (* "public" *) token env tok
    )
  | `Prot tok -> R.Case ("Prot",
      (* "protected" *) token env tok
    )
  | `Priv tok -> R.Case ("Priv",
      (* "private" *) token env tok
    )
  )

let map_variable (env : env) (tok : CST.variable) =
  (* variable *) token env tok

let map_tok_pdyn_p1_as (env : env) (tok : CST.tok_pdyn_p1_as) =
  (* tok_pdyn_p1_as *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok

let map_xhp_class_identifier (env : env) (tok : CST.xhp_class_identifier) =
  (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok

let map_tok_pdyn_n1_as (env : env) (tok : CST.tok_pdyn_n1_as) =
  (* tok_pdyn_n1_as *) token env tok

let map_xhp_string (env : env) (tok : CST.xhp_string) =
  (* xhp_string *) token env tok

let map_semgrep_variadic_identifier (env : env) (tok : CST.semgrep_variadic_identifier) =
  (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok

let map_collection_type (env : env) (x : CST.collection_type) =
  (match x with
  | `Array tok -> R.Case ("Array",
      (* "array" *) token env tok
    )
  | `Varray tok -> R.Case ("Varray",
      (* "varray" *) token env tok
    )
  | `Darray tok -> R.Case ("Darray",
      (* "darray" *) token env tok
    )
  | `Vec tok -> R.Case ("Vec",
      (* "vec" *) token env tok
    )
  | `Dict tok -> R.Case ("Dict",
      (* "dict" *) token env tok
    )
  | `Keyset tok -> R.Case ("Keyset",
      (* "keyset" *) token env tok
    )
  )

let map_tok_lcurldollar_pat_0e8e4b6 (env : env) (tok : CST.tok_lcurldollar_pat_0e8e4b6) =
  (* tok_lcurldollar_pat_0e8e4b6 *) token env tok

let map_null (env : env) (x : CST.null) =
  (match x with
  | `Null_37a6259 tok -> R.Case ("Null_37a6259",
      (* "null" *) token env tok
    )
  | `Null_bbb93ef tok -> R.Case ("Null_bbb93ef",
      (* "Null" *) token env tok
    )
  | `NULL tok -> R.Case ("NULL",
      (* "NULL" *) token env tok
    )
  )

let map_anon_choice_QMARKDASHGT_ce9cc19 (env : env) (x : CST.anon_choice_QMARKDASHGT_ce9cc19) =
  (match x with
  | `QMARKDASHGT tok -> R.Case ("QMARKDASHGT",
      (* "?->" *) token env tok
    )
  | `DASHGT tok -> R.Case ("DASHGT",
      (* "->" *) token env tok
    )
  )

let map_heredoc_start (env : env) (tok : CST.heredoc_start) =
  (* heredoc_start *) token env tok

let map_empty_statement (env : env) (x : CST.empty_statement) =
  (match x with
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_anon_choice_str_d42aa42 (env : env) (x : CST.anon_choice_str_d42aa42) =
  (match x with
  | `Str tok -> R.Case ("Str",
      (* string *) token env tok
    )
  | `Int tok -> R.Case ("Int",
      (* integer *) token env tok
    )
  )

let map_xhp_category_declaration (env : env) ((v1, v2, v3, v4) : CST.xhp_category_declaration) =
  let v1 = (* "category" *) token env v1 in
  let v2 =
    (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env v2
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_trait_alias_clause (env : env) ((v1, v2, v3) : CST.trait_alias_clause) =
  let v1 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v1
  in
  let v2 = (* "as" *) token env v2 in
  let v3 =
    (match v3 with
    | `Visi_modi_opt_id (v1, v2) -> R.Case ("Visi_modi_opt_id",
        let v1 = map_visibility_modifier env v1 in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `Opt_visi_modi_id (v1, v2) -> R.Case ("Opt_visi_modi_id",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_visibility_modifier env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v2
        in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2; v3]

let map_prefixed_string (env : env) ((v1, v2) : CST.prefixed_string) =
  let v1 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v1
  in
  let v2 = (* string *) token env v2 in
  R.Tuple [v1; v2]

let map_semgrep_extended_identifier (env : env) (x : CST.semgrep_extended_identifier) =
  (match x with
  | `Semg_id tok -> R.Case ("Semg_id",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok
    )
  )

let map_qualified_identifier (env : env) (x : CST.qualified_identifier) =
  (match x with
  | `Choice_opt_id_rep1_back_id x -> R.Case ("Choice_opt_id_rep1_back_id",
      (match x with
      | `Opt_id_rep1_back_id (v1, v2) -> R.Case ("Opt_id_rep1_back_id",
          let v1 =
            (match v1 with
            | Some tok -> R.Option (Some (
                (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok
              ))
            | None -> R.Option None)
          in
          let v2 =
            R.List (List.map (fun (v1, v2) ->
              let v1 = (* "\\" *) token env v1 in
              let v2 =
                (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v2
              in
              R.Tuple [v1; v2]
            ) v2)
          in
          R.Tuple [v1; v2]
        )
      | `Id tok -> R.Case ("Id",
          (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok
        )
      )
    )
  | `Semg_id tok -> R.Case ("Semg_id",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  )

let map_xhp_identifier_ (env : env) (x : CST.xhp_identifier_) =
  (match x with
  | `Xhp_id tok -> R.Case ("Xhp_id",
      (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
    )
  | `Xhp_class_id tok -> R.Case ("Xhp_class_id",
      (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
    )
  )

let rec map_xhp_attribute_expression (env : env) (x : CST.xhp_attribute_expression) =
  (match x with
  | `Xhp_id tok -> R.Case ("Xhp_id",
      (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
    )
  | `Xhp_class_id tok -> R.Case ("Xhp_class_id",
      (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
    )
  | `Xhp_cate_id tok -> R.Case ("Xhp_cate_id",
      (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
    )
  | `Xhp_bin_exp (v1, v2, v3) -> R.Case ("Xhp_bin_exp",
      let v1 = map_xhp_attribute_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_xhp_attribute_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Xhp_post_un_exp (v1, v2) -> R.Case ("Xhp_post_un_exp",
      let v1 = map_xhp_attribute_expression env v1 in
      let v2 =
        (match v2 with
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
      in
      R.Tuple [v1; v2]
    )
  | `Xhp_paren_exp (v1, v2, v3, v4) -> R.Case ("Xhp_paren_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_xhp_attribute_expression env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_xhp_attribute_expression env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_class_modifier (env : env) (x : CST.class_modifier) =
  (match x with
  | `Abst_modi tok -> R.Case ("Abst_modi",
      (* "abstract" *) token env tok
    )
  | `Final_modi tok -> R.Case ("Final_modi",
      (* "final" *) token env tok
    )
  )

let map_member_modifier (env : env) (x : CST.member_modifier) =
  (match x with
  | `Visi_modi x -> R.Case ("Visi_modi",
      map_visibility_modifier env x
    )
  | `Static_modi tok -> R.Case ("Static_modi",
      (* "static" *) token env tok
    )
  | `Abst_modi tok -> R.Case ("Abst_modi",
      (* "abstract" *) token env tok
    )
  | `Final_modi tok -> R.Case ("Final_modi",
      (* "final" *) token env tok
    )
  )

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Str tok -> R.Case ("Str",
      (* string *) token env tok
    )
  | `Int tok -> R.Case ("Int",
      (* integer *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* float *) token env tok
    )
  | `True x -> R.Case ("True",
      map_true_ env x
    )
  | `False x -> R.Case ("False",
      map_false_ env x
    )
  | `Null x -> R.Case ("Null",
      map_null env x
    )
  )

let map_primitive_type (env : env) (x : CST.primitive_type) =
  (match x with
  | `Bool tok -> R.Case ("Bool",
      (* "bool" *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* "float" *) token env tok
    )
  | `Int tok -> R.Case ("Int",
      (* "int" *) token env tok
    )
  | `Str tok -> R.Case ("Str",
      (* "string" *) token env tok
    )
  | `Arra tok -> R.Case ("Arra",
      (* "arraykey" *) token env tok
    )
  | `Void tok -> R.Case ("Void",
      (* "void" *) token env tok
    )
  | `Nonn tok -> R.Case ("Nonn",
      (* "nonnull" *) token env tok
    )
  | `Null x -> R.Case ("Null",
      map_null env x
    )
  | `Mixed tok -> R.Case ("Mixed",
      (* "mixed" *) token env tok
    )
  | `Dyna tok -> R.Case ("Dyna",
      (* "dynamic" *) token env tok
    )
  | `Nore tok -> R.Case ("Nore",
      (* "noreturn" *) token env tok
    )
  )

let map_xhp_enum_type (env : env) ((v1, v2, v3, v4, v5, v6) : CST.xhp_enum_type) =
  let v1 = (* "enum" *) token env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 = map_anon_choice_str_d42aa42 env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_str_d42aa42 env v2 in
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
  let v6 = (* "}" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_trait_select_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.trait_select_clause) =
  let v1 = map_qualified_identifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
  in
  let v4 = (* "insteadof" *) token env v4 in
  let v5 = map_qualified_identifier env v5 in
  let v6 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_qualified_identifier env v2 in
      R.Tuple [v1; v2]
    ) v6)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_namespace_identifier (env : env) (x : CST.namespace_identifier) =
  (match x with
  | `Qual_id_opt_back (v1, v2) -> R.Case ("Qual_id_opt_back",
      let v1 = map_qualified_identifier env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "\\" *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Back tok -> R.Case ("Back",
      (* "\\" *) token env tok
    )
  )

let rec map_type_constant_ (env : env) ((v1, v2, v3) : CST.type_constant_) =
  let v1 =
    (match v1 with
    | `Qual_id x -> R.Case ("Qual_id",
        map_qualified_identifier env x
      )
    | `Type_cst_ x -> R.Case ("Type_cst_",
        map_type_constant_ env x
      )
    )
  in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
  in
  R.Tuple [v1; v2; v3]

let map_xhp_close (env : env) ((v1, v2, v3) : CST.xhp_close) =
  let v1 = (* "</" *) token env v1 in
  let v2 = map_xhp_identifier_ env v2 in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_xhp_children_declaration (env : env) ((v1, v2, v3, v4) : CST.xhp_children_declaration) =
  let v1 = (* "children" *) token env v1 in
  let v2 = map_xhp_attribute_expression env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_xhp_attribute_expression env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_keyword (env : env) (x : CST.keyword) =
  (match x with
  | `Type tok -> R.Case ("Type",
      (* "type" *) token env tok
    )
  | `Newt tok -> R.Case ("Newt",
      (* "newtype" *) token env tok
    )
  | `Shape tok -> R.Case ("Shape",
      (* "shape" *) token env tok
    )
  | `Tupe tok -> R.Case ("Tupe",
      (* "tupe" *) token env tok
    )
  | `Clone tok -> R.Case ("Clone",
      (* "clone" *) token env tok
    )
  | `New tok -> R.Case ("New",
      (* "new" *) token env tok
    )
  | `Print tok -> R.Case ("Print",
      (* "print" *) token env tok
    )
  | `Choice_bool x -> R.Case ("Choice_bool",
      map_primitive_type env x
    )
  | `Choice_array x -> R.Case ("Choice_array",
      map_collection_type env x
    )
  )

let map_anonymous_function_use_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.anonymous_function_use_clause) =
  let v1 = (* "use" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* variable *) token env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* variable *) token env v2 in
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
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_scoped_identifier (env : env) ((v1, v2, v3) : CST.scoped_identifier) =
  let v1 =
    (match v1 with
    | `Qual_id x -> R.Case ("Qual_id",
        map_qualified_identifier env x
      )
    | `Var tok -> R.Case ("Var",
        (* variable *) token env tok
      )
    | `Scope_id x -> R.Case ("Scope_id",
        map_scope_identifier env x
      )
    | `Choice_xhp_id x -> R.Case ("Choice_xhp_id",
        map_xhp_identifier_ env x
      )
    | `Pipe_var tok -> R.Case ("Pipe_var",
        (* "$$" *) token env tok
      )
    )
  in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (match v3 with
    | `Id tok -> R.Case ("Id",
        (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok
      )
    | `Var tok -> R.Case ("Var",
        (* variable *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

let map_use_clause (env : env) ((v1, v2, v3) : CST.use_clause) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_use_type env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_namespace_identifier env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "as" *) token env v1 in
        let v2 =
          (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v2
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_anon_choice_semg_exte_id_8bbc8de (env : env) (x : CST.anon_choice_semg_exte_id_8bbc8de) =
  (match x with
  | `Semg_exte_id x -> R.Case ("Semg_exte_id",
      map_semgrep_extended_identifier env x
    )
  | `Choice_type x -> R.Case ("Choice_type",
      map_keyword env x
    )
  )

let rec map_anon_choice_comp_stmt_c6c6bb4 (env : env) (x : CST.anon_choice_comp_stmt_c6c6bb4) =
  (match x with
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

and map_anon_choice_exp_1701d0a (env : env) (x : CST.anon_choice_exp_1701d0a) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Elem_init (v1, v2, v3) -> R.Case ("Elem_init",
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb (env : env) ((v1, v2, v3) : CST.anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb) =
  let v1 = map_anon_choice_exp_1701d0a env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_exp_1701d0a env v2 in
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

and map_anon_choice_field_spec_0e0e023 (env : env) (x : CST.anon_choice_field_spec_0e0e023) =
  (match x with
  | `Field_spec (v1, v2, v3, v4) -> R.Case ("Field_spec",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "?" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_expression env v2 in
      let v3 = (* "=>" *) token env v3 in
      let v4 = map_type_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  )

and map_anon_exp_rep_COMMA_exp_0bb260c (env : env) ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anonymous_function_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.anonymous_function_expression) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "function" *) token env v2 in
  let v3 = map_parameters env v3 in
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
        map_anonymous_function_use_clause env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_compound_statement env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_argument (env : env) (x : CST.argument) =
  (match x with
  | `Opt_choice_inout_modi_exp (v1, v2) -> R.Case ("Opt_choice_inout_modi_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            (match x with
            | `Inout_modi tok -> R.Case ("Inout_modi",
                (* "inout" *) token env tok
              )
            | `Vari_modi tok -> R.Case ("Vari_modi",
                (* "..." *) token env tok
              )
            )
          ))
        | None -> R.Option None)
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Semg_vari_id tok -> R.Case ("Semg_vari_id",
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok
    )
  )

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
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
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_array_ (env : env) ((v1, v2, v3, v4, v5) : CST.array_) =
  let v1 = map_collection_type env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "[" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_as_expression (env : env) ((v1, v2, v3) : CST.as_expression) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `Tok_pdyn_n1_as x -> R.Case ("Tok_pdyn_n1_as",
        map_tok_pdyn_n1_as env x
      )
    | `QMARKas tok -> R.Case ("QMARKas",
        (* "?as" *) token env tok
      )
    )
  in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_attribute_modifier (env : env) ((v1, v2, v3, v4, v5, v6) : CST.attribute_modifier) =
  let v1 = (* "<<" *) token env v1 in
  let v2 = map_qualified_identifier env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_arguments env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_qualified_identifier env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_arguments env x
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
  let v6 = (* ">>" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_awaitable_expression (env : env) ((v1, v2) : CST.awaitable_expression) =
  let v1 = (* "async" *) token env v1 in
  let v2 = map_compound_statement env v2 in
  R.Tuple [v1; v2]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_BARGT_exp (v1, v2, v3) -> R.Case ("Exp_BARGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_QMARKQMARK_exp (v1, v2, v3) -> R.Case ("Exp_QMARKQMARK_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "??" *) token env v2 in
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
  | `Exp_EQEQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "===" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQGT_exp (v1, v2, v3) -> R.Case ("Exp_LTEQGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
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
  | `Exp_DOT_exp (v1, v2, v3) -> R.Case ("Exp_DOT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
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
  | `Exp_QMARKCOLON_exp (v1, v2, v3) -> R.Case ("Exp_QMARKCOLON_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "?:" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQ_exp (v1, v2, v3) -> R.Case ("Exp_EQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_QMARKQMARKEQ_exp (v1, v2, v3) -> R.Case ("Exp_QMARKQMARKEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "??=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DOTEQ_exp (v1, v2, v3) -> R.Case ("Exp_DOTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ".=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAREQ_exp (v1, v2, v3) -> R.Case ("Exp_BAREQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HATEQ_exp (v1, v2, v3) -> R.Case ("Exp_HATEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMPEQ_exp (v1, v2, v3) -> R.Case ("Exp_AMPEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTLTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTGTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PLUSEQ_exp (v1, v2, v3) -> R.Case ("Exp_PLUSEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "+=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASHEQ_exp (v1, v2, v3) -> R.Case ("Exp_DASHEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "-=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAREQ_exp (v1, v2, v3) -> R.Case ("Exp_STAREQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "*=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASHEQ_exp (v1, v2, v3) -> R.Case ("Exp_SLASHEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "/=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERCEQ_exp (v1, v2, v3) -> R.Case ("Exp_PERCEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STARSTAREQ_exp (v1, v2, v3) -> R.Case ("Exp_STARSTAREQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "**=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_braced_expression (env : env) ((v1, v2, v3) : CST.braced_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_call_expression (env : env) ((v1, v2, v3) : CST.call_expression) =
  let v1 =
    (match v1 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Choice_array x -> R.Case ("Choice_array",
        map_collection_type env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_arguments env v3 in
  R.Tuple [v1; v2; v3]

and map_cast_expression (env : env) ((v1, v2, v3, v4) : CST.cast_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Array tok -> R.Case ("Array",
        (* "array" *) token env tok
      )
    | `Int tok -> R.Case ("Int",
        (* "int" *) token env tok
      )
    | `Float tok -> R.Case ("Float",
        (* "float" *) token env tok
      )
    | `Str tok -> R.Case ("Str",
        (* "string" *) token env tok
      )
    | `Bool tok -> R.Case ("Bool",
        (* "bool" *) token env tok
      )
    )
  in
  let v3 = (* ")" *) token env v3 in
  let v4 = map_expression env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_catch_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_ env v3 in
  let v4 = (* variable *) token env v4 in
  let v5 = (* ")" *) token env v5 in
  let v6 = map_compound_statement env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_class_const_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_const_declaration) =
  let v1 = R.List (List.map (map_member_modifier env) v1) in
  let v2 = (* "const" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_ env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_class_const_declarator env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_class_const_declarator env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_class_const_declarator (env : env) ((v1, v2) : CST.class_const_declarator) =
  let v1 = map_anon_choice_semg_exte_id_8bbc8de env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_collection (env : env) ((v1, v2, v3, v4) : CST.collection) =
  let v1 = map_qualified_identifier env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_const_declarator (env : env) ((v1, v2, v3) : CST.const_declarator) =
  let v1 = map_anon_choice_semg_exte_id_8bbc8de env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Func_decl (v1, v2, v3) -> R.Case ("Func_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_function_declaration_header env v2 in
      let v3 = map_anon_choice_comp_stmt_c6c6bb4 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Class_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) -> R.Case ("Class_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_class_modifier env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_class_modifier env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "xhp" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = (* "class" *) token env v5 in
      let v6 =
        (match v6 with
        | `Semg_exte_id x -> R.Case ("Semg_exte_id",
            map_semgrep_extended_identifier env x
          )
        | `Choice_xhp_id x -> R.Case ("Choice_xhp_id",
            map_xhp_identifier_ env x
          )
        )
      in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v8 =
        (match v8 with
        | Some x -> R.Option (Some (
            map_extends_clause env x
          ))
        | None -> R.Option None)
      in
      let v9 =
        (match v9 with
        | Some x -> R.Option (Some (
            map_implements_clause env x
          ))
        | None -> R.Option None)
      in
      let v10 =
        (match v10 with
        | Some x -> R.Option (Some (
            map_where_clause env x
          ))
        | None -> R.Option None)
      in
      let v11 = map_member_declarations env v11 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11]
    )
  | `Inte_decl (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Inte_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "interface" *) token env v2 in
      let v3 = map_semgrep_extended_identifier env v3 in
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
            map_extends_clause env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_where_clause env x
          ))
        | None -> R.Option None)
      in
      let v7 = map_member_declarations env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Trait_decl (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Trait_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "trait" *) token env v2 in
      let v3 = map_semgrep_extended_identifier env v3 in
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
            map_implements_clause env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_where_clause env x
          ))
        | None -> R.Option None)
      in
      let v7 = map_member_declarations env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Alias_decl (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Alias_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Type tok -> R.Case ("Type",
            (* "type" *) token env tok
          )
        | `Newt tok -> R.Case ("Newt",
            (* "newtype" *) token env tok
          )
        )
      in
      let v3 = map_semgrep_extended_identifier env v3 in
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
            let v1 = (* "as" *) token env v1 in
            let v2 = map_type_ env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v6 = (* "=" *) token env v6 in
      let v7 = map_type_ env v7 in
      let v8 = (* ";" *) token env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Enum_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("Enum_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "enum" *) token env v2 in
      let v3 = map_semgrep_extended_identifier env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_type_ env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "as" *) token env v1 in
            let v2 = map_type_ env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v7 = (* "{" *) token env v7 in
      let v8 = R.List (List.map (map_enumerator env) v8) in
      let v9 = (* "}" *) token env v9 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Name_decl (v1, v2) -> R.Case ("Name_decl",
      let v1 = (* "namespace" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Qual_id_SEMI (v1, v2) -> R.Case ("Qual_id_SEMI",
                let v1 = map_qualified_identifier env v1 in
                let v2 = (* ";" *) token env v2 in
                R.Tuple [v1; v2]
              )
            | `Opt_qual_id_comp_stmt (v1, v2) -> R.Case ("Opt_qual_id_comp_stmt",
                let v1 =
                  (match v1 with
                  | Some x -> R.Option (Some (
                      map_qualified_identifier env x
                    ))
                  | None -> R.Option None)
                in
                let v2 = map_compound_statement env v2 in
                R.Tuple [v1; v2]
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Const_decl (v1, v2, v3, v4, v5) -> R.Case ("Const_decl",
      let v1 = (* "const" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_const_declarator env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_const_declarator env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_embedded_brace_expression (env : env) ((v1, v2) : CST.embedded_brace_expression) =
  let v1 = map_embedded_brace_expression_ env v1 in
  let v2 = (* "}" *) token env v2 in
  R.Tuple [v1; v2]

and map_embedded_brace_expression_ (env : env) (x : CST.embedded_brace_expression_) =
  (match x with
  | `Tok_lcur_pat_0e8e4b6 x -> R.Case ("Tok_lcur_pat_0e8e4b6",
      map_tok_lcurldollar_pat_0e8e4b6 env x
    )
  | `Embe_brace_call_exp (v1, v2) -> R.Case ("Embe_brace_call_exp",
      let v1 = map_embedded_brace_expression_ env v1 in
      let v2 = map_arguments env v2 in
      R.Tuple [v1; v2]
    )
  | `Embe_brace_subs_exp (v1, v2, v3, v4) -> R.Case ("Embe_brace_subs_exp",
      let v1 = map_embedded_brace_expression_ env v1 in
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
    )
  | `Embe_brace_sele_exp (v1, v2, v3) -> R.Case ("Embe_brace_sele_exp",
      let v1 = map_embedded_brace_expression_ env v1 in
      let v2 = map_anon_choice_QMARKDASHGT_ce9cc19 env v2 in
      let v3 = map_variablish env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_enumerator (env : env) ((v1, v2, v3, v4) : CST.enumerator) =
  let v1 = map_semgrep_extended_identifier env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_here x -> R.Case ("Choice_here",
      (match x with
      | `Here x -> R.Case ("Here",
          map_heredoc env x
        )
      | `Array x -> R.Case ("Array",
          map_array_ env x
        )
      | `Tuple x -> R.Case ("Tuple",
          map_tuple env x
        )
      | `Shape x -> R.Case ("Shape",
          map_shape env x
        )
      | `Coll x -> R.Case ("Coll",
          map_collection env x
        )
      | `Choice_str x -> R.Case ("Choice_str",
          map_literal env x
        )
      | `Choice_var x -> R.Case ("Choice_var",
          map_variablish env x
        )
      | `Pref_str x -> R.Case ("Pref_str",
          map_prefixed_string env x
        )
      | `Paren_exp x -> R.Case ("Paren_exp",
          map_parenthesized_expression env x
        )
      | `Bin_exp x -> R.Case ("Bin_exp",
          map_binary_expression env x
        )
      | `Prefix_un_exp x -> R.Case ("Prefix_un_exp",
          map_prefix_unary_expression env x
        )
      | `Post_un_exp x -> R.Case ("Post_un_exp",
          map_postfix_unary_expression env x
        )
      | `Is_exp x -> R.Case ("Is_exp",
          map_is_expression env x
        )
      | `As_exp x -> R.Case ("As_exp",
          map_as_expression env x
        )
      | `Awai_exp x -> R.Case ("Awai_exp",
          map_awaitable_expression env x
        )
      | `Yield_exp x -> R.Case ("Yield_exp",
          map_yield_expression env x
        )
      | `Cast_exp x -> R.Case ("Cast_exp",
          map_cast_expression env x
        )
      | `Tern_exp x -> R.Case ("Tern_exp",
          map_ternary_expression env x
        )
      | `Lambda_exp x -> R.Case ("Lambda_exp",
          map_lambda_expression env x
        )
      | `Call_exp x -> R.Case ("Call_exp",
          map_call_expression env x
        )
      | `Sele_exp x -> R.Case ("Sele_exp",
          map_selection_expression env x
        )
      | `New_exp x -> R.Case ("New_exp",
          map_new_expression env x
        )
      | `Incl_exp x -> R.Case ("Incl_exp",
          map_include_expression env x
        )
      | `Requ_exp x -> R.Case ("Requ_exp",
          map_require_expression env x
        )
      | `Anon_func_exp x -> R.Case ("Anon_func_exp",
          map_anonymous_function_expression env x
        )
      | `Xhp_exp x -> R.Case ("Xhp_exp",
          map_xhp_expression env x
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

and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = map_expression env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

and map_extends_clause (env : env) ((v1, v2, v3) : CST.extends_clause) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_field_initializer (env : env) ((v1, v2, v3) : CST.field_initializer) =
  let v1 =
    (match v1 with
    | `Str tok -> R.Case ("Str",
        (* string *) token env tok
      )
    | `Scoped_id x -> R.Case ("Scoped_id",
        map_scoped_identifier env x
      )
    )
  in
  let v2 = (* "=>" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_compound_statement env v2 in
  R.Tuple [v1; v2]

and map_function_declaration_header (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.function_declaration_header) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "function" *) token env v2 in
  let v3 = map_semgrep_extended_identifier env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_parameters env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_attribute_modifier env x
            ))
          | None -> R.Option None)
        in
        let v3 = map_type_ env v3 in
        R.Tuple [v1; v2; v3]
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
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_heredoc (env : env) ((v1, v2, v3, v4, v5, v6) : CST.heredoc) =
  let v1 = (* "<<<" *) token env v1 in
  let v2 = (* heredoc_start *) token env v2 in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* heredoc_start_newline *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    R.List (List.map (fun x ->
      (match x with
      | `Here_body tok -> R.Case ("Here_body",
          (* heredoc_body *) token env tok
        )
      | `Var tok -> R.Case ("Var",
          (* variable *) token env tok
        )
      | `Embe_brace_exp x -> R.Case ("Embe_brace_exp",
          map_embedded_brace_expression env x
        )
      )
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* heredoc_end_newline *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* heredoc_end *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_implements_clause (env : env) ((v1, v2, v3) : CST.implements_clause) =
  let v1 = (* "implements" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_include_expression (env : env) ((v1, v2) : CST.include_expression) =
  let v1 =
    (match v1 with
    | `Incl tok -> R.Case ("Incl",
        (* "include" *) token env tok
      )
    | `Incl_once tok -> R.Case ("Incl_once",
        (* "include_once" *) token env tok
      )
    )
  in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_is_expression (env : env) ((v1, v2, v3) : CST.is_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "is" *) token env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_lambda_expression (env : env) ((v1, v2, v3, v4, v5) : CST.lambda_expression) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute_modifier env x
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
    | `Single_param_params tok -> R.Case ("Single_param_params",
        (* variable *) token env tok
      )
    | `Params_opt_COLON_choice_type_spec (v1, v2) -> R.Case ("Params_opt_COLON_choice_type_spec",
        let v1 = map_parameters env v1 in
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
  in
  let v4 = (* "==>" *) token env v4 in
  let v5 =
    (match v5 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Comp_stmt x -> R.Case ("Comp_stmt",
        map_compound_statement env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_member_declarations (env : env) ((v1, v2, v3) : CST.member_declarations) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Class_const_decl x -> R.Case ("Class_const_decl",
          map_class_const_declaration env x
        )
      | `Meth_decl x -> R.Case ("Meth_decl",
          map_method_declaration env x
        )
      | `Prop_decl x -> R.Case ("Prop_decl",
          map_property_declaration env x
        )
      | `Type_const_decl x -> R.Case ("Type_const_decl",
          map_type_const_declaration env x
        )
      | `Trait_use_clause x -> R.Case ("Trait_use_clause",
          map_trait_use_clause env x
        )
      | `Requ_imples_clause x -> R.Case ("Requ_imples_clause",
          map_require_implements_clause env x
        )
      | `Requ_extends_clause x -> R.Case ("Requ_extends_clause",
          map_require_extends_clause env x
        )
      | `Xhp_attr_decl x -> R.Case ("Xhp_attr_decl",
          map_xhp_attribute_declaration env x
        )
      | `Xhp_chil_decl x -> R.Case ("Xhp_chil_decl",
          map_xhp_children_declaration env x
        )
      | `Xhp_cate_decl x -> R.Case ("Xhp_cate_decl",
          map_xhp_category_declaration env x
        )
      | `Ellips tok -> R.Case ("Ellips",
          (* "..." *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_method_declaration (env : env) ((v1, v2, v3, v4) : CST.method_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = R.List (List.map (map_member_modifier env) v2) in
  let v3 = map_function_declaration_header env v3 in
  let v4 = map_anon_choice_comp_stmt_c6c6bb4 env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_new_expression (env : env) ((v1, v2, v3, v4) : CST.new_expression) =
  let v1 = (* "new" *) token env v1 in
  let v2 = map_variablish env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_arguments env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_parameter (env : env) (x : CST.parameter) =
  (match x with
  | `Opt_attr_modi_opt_visi_modi_opt_inout_modi_opt_choice_type_spec_opt_vari_modi_var_opt_EQ_exp (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Opt_attr_modi_opt_visi_modi_opt_inout_modi_opt_choice_type_spec_opt_vari_modi_var_opt_EQ_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_visibility_modifier env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "inout" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* "..." *) token env tok
          ))
        | None -> R.Option None)
      in
      let v6 = (* variable *) token env v6 in
      let v7 =
        (match v7 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Vari_modi tok -> R.Case ("Vari_modi",
            (* "..." *) token env tok
          )
        | `Param_rep_COMMA_param_opt_COMMA (v1, v2, v3) -> R.Case ("Param_rep_COMMA_param_opt_COMMA",
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
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_postfix_unary_expression (env : env) ((v1, v2) : CST.postfix_unary_expression) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
        (* "++" *) token env tok
      )
    | `DASHDASH tok -> R.Case ("DASHDASH",
        (* "--" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

and map_prefix_unary_expression (env : env) (x : CST.prefix_unary_expression) =
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
  | `PLUSPLUS_exp (v1, v2) -> R.Case ("PLUSPLUS_exp",
      let v1 = (* "++" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `DASHDASH_exp (v1, v2) -> R.Case ("DASHDASH_exp",
      let v1 = (* "--" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Print_exp (v1, v2) -> R.Case ("Print_exp",
      let v1 = (* "print" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Clone_exp (v1, v2) -> R.Case ("Clone_exp",
      let v1 = (* "clone" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Await_exp (v1, v2) -> R.Case ("Await_exp",
      let v1 = (* "await" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `AT_exp (v1, v2) -> R.Case ("AT_exp",
      let v1 = (* "@" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_property_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.property_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = R.List (List.map (map_member_modifier env) v2) in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_ env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_property_declarator env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_property_declarator env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_property_declarator (env : env) ((v1, v2) : CST.property_declarator) =
  let v1 = (* variable *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_require_expression (env : env) ((v1, v2) : CST.require_expression) =
  let v1 =
    (match v1 with
    | `Requ tok -> R.Case ("Requ",
        (* "require" *) token env tok
      )
    | `Requ_once tok -> R.Case ("Requ_once",
        (* "require_once" *) token env tok
      )
    )
  in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_require_extends_clause (env : env) ((v1, v2, v3, v4, v5) : CST.require_extends_clause) =
  let v1 = (* "require" *) token env v1 in
  let v2 = (* "extends" *) token env v2 in
  let v3 = map_type_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_require_implements_clause (env : env) ((v1, v2, v3, v4, v5) : CST.require_implements_clause) =
  let v1 = (* "require" *) token env v1 in
  let v2 = (* "implements" *) token env v2 in
  let v3 = map_type_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_selection_expression (env : env) ((v1, v2, v3) : CST.selection_expression) =
  let v1 =
    (match v1 with
    | `Choice_var x -> R.Case ("Choice_var",
        map_variablish env x
      )
    | `As_exp x -> R.Case ("As_exp",
        map_as_expression env x
      )
    )
  in
  let v2 = map_anon_choice_QMARKDASHGT_ce9cc19 env v2 in
  let v3 =
    (match v3 with
    | `Choice_var x -> R.Case ("Choice_var",
        map_variablish env x
      )
    | `Braced_exp x -> R.Case ("Braced_exp",
        map_braced_expression env x
      )
    | `Choice_type x -> R.Case ("Choice_type",
        map_keyword env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_shape (env : env) ((v1, v2, v3, v4) : CST.shape) =
  let v1 = (* "shape" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_field_initializer env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_field_initializer env v2 in
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

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Choice_func_decl x -> R.Case ("Choice_func_decl",
      map_declaration env x
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  | `Empty_stmt x -> R.Case ("Empty_stmt",
      map_empty_statement env x
    )
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_expression_statement env x
    )
  | `Ret_stmt (v1, v2, v3) -> R.Case ("Ret_stmt",
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Brk_stmt (v1, v2, v3) -> R.Case ("Brk_stmt",
      let v1 = (* "break" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cont_stmt (v1, v2, v3) -> R.Case ("Cont_stmt",
      let v1 = (* "continue" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Throw_stmt (v1, v2, v3) -> R.Case ("Throw_stmt",
      let v1 = (* "throw" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Echo_stmt (v1, v2, v3, v4) -> R.Case ("Echo_stmt",
      let v1 = (* "echo" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Unset_stmt (v1, v2, v3, v4, v5) -> R.Case ("Unset_stmt",
      let v1 = (* "unset" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_variablish env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_variablish env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Use_stmt (v1, v2, v3) -> R.Case ("Use_stmt",
      let v1 = (* "use" *) token env v1 in
      let v2 =
        (match v2 with
        | `Use_clause_rep_COMMA_use_clause_opt_COMMA (v1, v2, v3) -> R.Case ("Use_clause_rep_COMMA_use_clause_opt_COMMA",
            let v1 = map_use_clause env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_use_clause env v2 in
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
        | `Opt_use_type_name_id_LCURL_use_clause_rep_COMMA_use_clause_opt_COMMA_RCURL (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Opt_use_type_name_id_LCURL_use_clause_rep_COMMA_use_clause_opt_COMMA_RCURL",
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_use_type env x
                ))
              | None -> R.Option None)
            in
            let v2 = map_namespace_identifier env v2 in
            let v3 = (* "{" *) token env v3 in
            let v4 = map_use_clause env v4 in
            let v5 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_use_clause env v2 in
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
            let v7 = (* "}" *) token env v7 in
            R.Tuple [v1; v2; v3; v4; v5; v6; v7]
          )
        )
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `If_stmt (v1, v2, v3, v4, v5) -> R.Case ("If_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2, v3) ->
          let v1 =
            (match v1 with
            | `Elseif tok -> R.Case ("Elseif",
                (* "elseif" *) token env tok
              )
            | `Else_if (v1, v2) -> R.Case ("Else_if",
                let v1 = (* "else" *) token env v1 in
                let v2 = (* "if" *) token env v2 in
                R.Tuple [v1; v2]
              )
            )
          in
          let v2 = map_parenthesized_expression env v2 in
          let v3 = map_statement env v3 in
          R.Tuple [v1; v2; v3]
        ) v4)
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "else" *) token env v1 in
            let v2 = map_statement env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `While_stmt (v1, v2, v3) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Do_stmt (v1, v2, v3, v4, v5) -> R.Case ("Do_stmt",
      let v1 = (* "do" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = (* "while" *) token env v3 in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("For_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_exp_rep_COMMA_exp_0bb260c env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ";" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_exp_rep_COMMA_exp_0bb260c env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* ";" *) token env v6 in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            map_anon_exp_rep_COMMA_exp_0bb260c env x
          ))
        | None -> R.Option None)
      in
      let v8 = (* ")" *) token env v8 in
      let v9 = map_statement env v9 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Switch_stmt (v1, v2, v3, v4, v5) -> R.Case ("Switch_stmt",
      let v1 = (* "switch" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        R.List (List.map (fun x ->
          (match x with
          | `Switch_case x -> R.Case ("Switch_case",
              map_switch_case env x
            )
          | `Switch_defa x -> R.Case ("Switch_defa",
              map_switch_default env x
            )
          )
        ) v4)
      in
      let v5 = (* "}" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Fore_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("Fore_stmt",
      let v1 = (* "foreach" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "await" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = map_tok_pdyn_p1_as env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_variablish env v1 in
            let v2 = (* "=>" *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v7 = map_variablish env v7 in
      let v8 = (* ")" *) token env v8 in
      let v9 = map_statement env v9 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Try_stmt (v1, v2, v3, v4) -> R.Case ("Try_stmt",
      let v1 = (* "try" *) token env v1 in
      let v2 = map_compound_statement env v2 in
      let v3 = R.List (List.map (map_catch_clause env) v3) in
      let v4 =
        (match v4 with
        | `Catch_clause x -> R.Case ("Catch_clause",
            map_catch_clause env x
          )
        | `Fina_clause x -> R.Case ("Fina_clause",
            map_finally_clause env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Conc_stmt (v1, v2) -> R.Case ("Conc_stmt",
      let v1 = (* "concurrent" *) token env v1 in
      let v2 = map_compound_statement env v2 in
      R.Tuple [v1; v2]
    )
  | `Using_stmt (v1, v2, v3) -> R.Case ("Using_stmt",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "await" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "using" *) token env v2 in
      let v3 =
        (match v3 with
        | `Exp_stmt x -> R.Case ("Exp_stmt",
            map_expression_statement env x
          )
        | `LPAR_exp_rep_COMMA_exp_RPAR_choice_comp_stmt (v1, v2, v3, v4, v5) -> R.Case ("LPAR_exp_rep_COMMA_exp_RPAR_choice_comp_stmt",
            let v1 = (* "(" *) token env v1 in
            let v2 = map_expression env v2 in
            let v3 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_expression env v2 in
                R.Tuple [v1; v2]
              ) v3)
            in
            let v4 = (* ")" *) token env v4 in
            let v5 = map_anon_choice_comp_stmt_c6c6bb4 env v5 in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = R.List (List.map (map_statement env) v4) in
  R.Tuple [v1; v2; v3; v4]

and map_switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = (* "default" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = R.List (List.map (map_statement env) v3) in
  R.Tuple [v1; v2; v3]

and map_ternary_expression (env : env) ((v1, v2, v3, v4, v5) : CST.ternary_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "?" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_expression env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_trait_use_clause (env : env) ((v1, v2, v3, v4) : CST.trait_use_clause) =
  let v1 = (* "use" *) token env v1 in
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
    | `LCURL_rep_choice_trait_select_clause_SEMI_RCURL (v1, v2, v3) -> R.Case ("LCURL_rep_choice_trait_select_clause_SEMI_RCURL",
        let v1 = (* "{" *) token env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | `Trait_select_clause x -> R.Case ("Trait_select_clause",
                  map_trait_select_clause env x
                )
              | `Trait_alias_clause x -> R.Case ("Trait_alias_clause",
                  map_trait_alias_clause env x
                )
              )
            in
            let v2 = (* ";" *) token env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 = (* "}" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `SEMI tok -> R.Case ("SEMI",
        (* ";" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_tuple (env : env) ((v1, v2, v3, v4) : CST.tuple) =
  let v1 = (* "tuple" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_expression env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
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

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Type_spec (v1, v2, v3) -> R.Case ("Type_spec",
      let v1 = R.List (List.map (map_type_modifier env) v1) in
      let v2 =
        (match v2 with
        | `Choice_bool x -> R.Case ("Choice_bool",
            map_primitive_type env x
          )
        | `Qual_id x -> R.Case ("Qual_id",
            map_qualified_identifier env x
          )
        | `Choice_array x -> R.Case ("Choice_array",
            map_collection_type env x
          )
        | `Choice_xhp_id x -> R.Case ("Choice_xhp_id",
            map_xhp_identifier_ env x
          )
        )
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Type_cst (v1, v2) -> R.Case ("Type_cst",
      let v1 = R.List (List.map (map_type_modifier env) v1) in
      let v2 = map_type_constant_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Shape_type_spec (v1, v2, v3, v4, v5) -> R.Case ("Shape_type_spec",
      let v1 = R.List (List.map (map_type_modifier env) v1) in
      let v2 = (* "shape" *) token env v2 in
      let v3 = (* "(" *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_anon_choice_field_spec_0e0e023 env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_field_spec_0e0e023 env v2 in
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
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Func_type_spec (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Func_type_spec",
      let v1 = R.List (List.map (map_type_modifier env) v1) in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_pat_466b599 env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2, v3, v4, v5) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* "inout" *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 = map_type_ env v2 in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* "..." *) token env tok
                ))
              | None -> R.Option None)
            in
            let v4 =
              R.List (List.map (fun (v1, v2, v3, v4) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some tok -> R.Option (Some (
                      (* "inout" *) token env tok
                    ))
                  | None -> R.Option None)
                in
                let v3 = map_type_ env v3 in
                let v4 =
                  (match v4 with
                  | Some tok -> R.Option (Some (
                      (* "..." *) token env tok
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2; v3; v4]
              ) v4)
            in
            let v5 =
              (match v5 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3; v4; v5]
          ))
        | None -> R.Option None)
      in
      let v5 = (* ")" *) token env v5 in
      let v6 = (* ":" *) token env v6 in
      let v7 = map_type_ env v7 in
      let v8 = (* ")" *) token env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Tuple_type_spec (v1, v2, v3, v4, v5, v6) -> R.Case ("Tuple_type_spec",
      let v1 = R.List (List.map (map_type_modifier env) v1) in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_type_ env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_type_ env v2 in
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
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_type_arguments (env : env) ((v1, v2, v3) : CST.type_arguments) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_type_ env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_ env v2 in
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
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_type_const_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.type_const_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = R.List (List.map (map_member_modifier env) v2) in
  let v3 = (* "const" *) token env v3 in
  let v4 = (* "type" *) token env v4 in
  let v5 = map_semgrep_extended_identifier env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "as" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v8 =
    (match v8 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v9 = (* ";" *) token env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

and map_type_parameter (env : env) ((v1, v2, v3, v4) : CST.type_parameter) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `Reify tok -> R.Case ("Reify",
            (* "reify" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
  in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `As tok -> R.Case ("As",
            (* "as" *) token env tok
          )
        | `Super tok -> R.Case ("Super",
            (* "super" *) token env tok
          )
        )
      in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

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

and map_variablish (env : env) (x : CST.variablish) =
  (match x with
  | `Var tok -> R.Case ("Var",
      (* variable *) token env tok
    )
  | `Pipe_var tok -> R.Case ("Pipe_var",
      (* "$$" *) token env tok
    )
  | `List_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("List_exp",
      let v1 = (* "list" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 =
            (match v2 with
            | Some x -> R.Option (Some (
                map_expression env x
              ))
            | None -> R.Option None)
          in
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
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Subs_exp (v1, v2, v3, v4) -> R.Case ("Subs_exp",
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
    )
  | `Qual_id x -> R.Case ("Qual_id",
      map_qualified_identifier env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Call_exp x -> R.Case ("Call_exp",
      map_call_expression env x
    )
  | `Scoped_id x -> R.Case ("Scoped_id",
      map_scoped_identifier env x
    )
  | `Scope_id x -> R.Case ("Scope_id",
      map_scope_identifier env x
    )
  | `Sele_exp x -> R.Case ("Sele_exp",
      map_selection_expression env x
    )
  | `Choice_xhp_id x -> R.Case ("Choice_xhp_id",
      map_xhp_identifier_ env x
    )
  )

and map_where_clause (env : env) ((v1, v2) : CST.where_clause) =
  let v1 = (* "where" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_where_constraint env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_where_constraint (env : env) ((v1, v2, v3) : CST.where_constraint) =
  let v1 = map_type_ env v1 in
  let v2 =
    (match v2 with
    | `As tok -> R.Case ("As",
        (* "as" *) token env tok
      )
    | `Super tok -> R.Case ("Super",
        (* "super" *) token env tok
      )
    | `EQ tok -> R.Case ("EQ",
        (* "=" *) token env tok
      )
    )
  in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_xhp_attribute (env : env) (x : CST.xhp_attribute) =
  (match x with
  | `Xhp_id_EQ_choice_str (v1, v2, v3) -> R.Case ("Xhp_id_EQ_choice_str",
      let v1 =
        (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (match v3 with
        | `Str tok -> R.Case ("Str",
            (* string *) token env tok
          )
        | `Braced_exp x -> R.Case ("Braced_exp",
            map_braced_expression env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_braced_exp x -> R.Case ("Choice_braced_exp",
      (match x with
      | `Braced_exp x -> R.Case ("Braced_exp",
          map_braced_expression env x
        )
      | `Xhp_spread_exp x -> R.Case ("Xhp_spread_exp",
          map_xhp_spread_expression env x
        )
      )
    )
  )

and map_xhp_attribute_declaration (env : env) ((v1, v2, v3, v4) : CST.xhp_attribute_declaration) =
  let v1 = (* "attribute" *) token env v1 in
  let v2 = map_xhp_class_attribute env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_xhp_class_attribute env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_xhp_class_attribute (env : env) ((v1, v2, v3, v4) : CST.xhp_class_attribute) =
  let v1 =
    (match v1 with
    | `Choice_type_spec x -> R.Case ("Choice_type_spec",
        map_type_ env x
      )
    | `Xhp_enum_type x -> R.Case ("Xhp_enum_type",
        map_xhp_enum_type env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        (match x with
        | `ATre tok -> R.Case ("ATre",
            (* "@required" *) token env tok
          )
        | `ATla tok -> R.Case ("ATla",
            (* "@lateinit" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_xhp_expression (env : env) (x : CST.xhp_expression) =
  (match x with
  | `Xhp_open_close (v1, v2, v3, v4) -> R.Case ("Xhp_open_close",
      let v1 = (* "<" *) token env v1 in
      let v2 = map_xhp_identifier_ env v2 in
      let v3 = R.List (List.map (map_xhp_attribute env) v3) in
      let v4 = (* "/>" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Xhp_open_rep_choice_xhp_str_xhp_close (v1, v2, v3) -> R.Case ("Xhp_open_rep_choice_xhp_str_xhp_close",
      let v1 = map_xhp_open env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Xhp_str tok -> R.Case ("Xhp_str",
              (* xhp_string *) token env tok
            )
          | `Xhp_comm tok -> R.Case ("Xhp_comm",
              (* xhp_comment *) token env tok
            )
          | `Braced_exp x -> R.Case ("Braced_exp",
              map_braced_expression env x
            )
          | `Xhp_exp x -> R.Case ("Xhp_exp",
              map_xhp_expression env x
            )
          )
        ) v2)
      in
      let v3 = map_xhp_close env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_xhp_open (env : env) ((v1, v2, v3, v4) : CST.xhp_open) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_xhp_identifier_ env v2 in
  let v3 = R.List (List.map (map_xhp_attribute env) v3) in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_xhp_spread_expression (env : env) ((v1, v2, v3, v4) : CST.xhp_spread_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 = (* "..." *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_yield_expression (env : env) ((v1, v2) : CST.yield_expression) =
  let v1 = (* "yield" *) token env v1 in
  let v2 = map_anon_choice_exp_1701d0a env v2 in
  R.Tuple [v1; v2]

let map_script (env : env) ((v1, v2) : CST.script) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_pat_b6fe07e env x
      ))
    | None -> R.Option None)
  in
  let v2 = R.List (List.map (map_statement env) v2) in
  R.Tuple [v1; v2]

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_script () root
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

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

let entity_of_pattern ?(attrs = []) (pat : G.pattern) : G.entity =
  (* TODO Desugar single-element tuples? *)
  let entity_name =
    match pat with
    (* Unwrap PatId into a regular name. map_property_binding_pattern has to
     * return a pattern because it is recursive, and might need to compose its
     * return value into larger patterns. So, we have to unwrap here. *)
    | G.PatId (id, id_info) -> G.EN (G.Id (id, id_info))
    | pattern -> G.EPattern pattern
  in
  { G.name = entity_name; attrs; tparams = [] }

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-swift/Boilerplate.ml *)

let map_bitwise_binary_operator (env : env) (x : CST.bitwise_binary_operator) =
  match x with
  | `AMP tok -> (G.BitAnd, (* "&" *) str env tok)
  | `BAR tok -> (G.BitOr, (* "|" *) str env tok)
  | `HAT tok -> (G.BitXor, (* "^" *) str env tok)
  | `LTLT tok -> (G.LSL, (* "<<" *) str env tok)
  | `GTGT tok ->
      (* Swift uses an arithmetic right shift:
       * https://docs.swift.org/swift-book/LanguageGuide/AdvancedOperators.html#ID36
       * *)
      (G.ASR, (* ">>" *) str env tok)

let map_function_modifier (env : env) (x : CST.function_modifier) =
  match x with
  | `Infix tok ->
      (* "infix" *)
      G.unhandled_keywordattr (str env tok)
  | `Post tok ->
      (* "postfix" *)
      G.unhandled_keywordattr (str env tok)
  | `Prefix tok ->
      (* "prefix" *)
      G.unhandled_keywordattr (str env tok)

let map_binding_pattern_kind (env : env) (x : CST.binding_pattern_kind) :
    G.ident list =
  match x with
  | `Var tok -> (* "var" *) [ str env tok ]
  | `Let tok -> (* "let" *) [ str env tok ]

let map_possibly_async_binding_pattern_kind (env : env)
    ((v1, v2) : CST.possibly_async_binding_pattern_kind) =
  let async =
    match v1 with
    | Some tok ->
        (* async_modifier *)
        [ str env tok ]
    | None -> []
  in
  async @ map_binding_pattern_kind env v2

let map_binding_pattern_kind_to_attr (env : env) (x : CST.binding_pattern_kind)
    : G.attribute list =
  match x with
  | `Var tok -> (* "var" *) [ G.attr G.Mutable (token env tok) ]
  | `Let tok -> (* "let" *) [ G.attr G.Const (token env tok) ]

let map_possibly_async_binding_pattern_kind_to_attr (env : env)
    ((v1, v2) : CST.possibly_async_binding_pattern_kind) =
  let async =
    match v1 with
    | Some tok ->
        (* async_modifier *)
        [ G.attr G.Async (token env tok) ]
    | None -> []
  in
  async @ map_binding_pattern_kind_to_attr env v2

let map_comparison_operator (env : env) (x : CST.comparison_operator) =
  match x with
  | `LT tok -> (G.Lt, (* "<" *) str env tok)
  | `GT tok -> (G.Gt, (* ">" *) str env tok)
  | `LTEQ tok -> (G.LtE, (* "<=" *) str env tok)
  | `GTEQ tok -> (G.GtE, (* ">=" *) str env tok)

let map_assignment_and_operator (env : env) (x : CST.assignment_and_operator) =
  match x with
  | `PLUSEQ tok -> (Some G.Plus, (* "+=" *) token env tok)
  | `DASHEQ tok -> (Some G.Minus, (* "-=" *) token env tok)
  | `STAREQ tok -> (Some G.Mult, (* "*=" *) token env tok)
  | `SLASHEQ tok -> (Some G.Div, (* "/=" *) token env tok)
  | `PERCEQ tok -> (Some G.Mod, (* "%=" *) token env tok)
  | `EQ tok -> (None, (* "=" *) token env tok)

let map_ownership_modifier (env : env) (x : CST.ownership_modifier) =
  (* These have to do with garbage collection and probably do not matter. *)
  match x with
  | `Weak tok ->
      (* "weak" *)
      G.unhandled_keywordattr (str env tok)
  | `Unow_7c8c304 tok ->
      (* "unowned" *)
      G.unhandled_keywordattr (str env tok)
  | `Unow_e455cde tok ->
      (* "unowned(safe)" *)
      G.unhandled_keywordattr (str env tok)
  | `Unow_8fda70e tok ->
      (* "unowned(unsafe)" *)
      G.unhandled_keywordattr (str env tok)

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
  | `Cont tok -> (
      let tok = (* "continue" *) token env tok in
      match expr with
      | Some { G.e = N (Id (id, _)); _ } ->
          G.Continue (tok, LId id, semi) |> G.s
      (* Need to have an identifier to continue to. It's not possible to have an expression which is not an identifier.
         https://docs.swift.org/swift-book/ReferenceManual/Statements.html#grammar_continue-statement *)
      | Some { G.e = _; _ } -> raise Common.Impossible
      | None -> G.Continue (tok, LNone, semi) |> G.s)
  | `Brk tok -> (
      let tok = (* "break" *) token env tok in
      match expr with
      | Some { G.e = N (Id (id, _)); _ } -> G.Break (tok, LId id, semi) |> G.s
      (* Need to have an identifier to break to. It's not possible to have an expression which is not an identifier.
         https://docs.swift.org/swift-book/ReferenceManual/Statements.html#grammar_break-statement *)
      | Some { G.e = _; _ } -> raise Common.Impossible
      | None -> G.Break (tok, LNone, semi) |> G.s)
  | `Yield tok ->
      let tok = (* "yield" *) token env tok in
      (* Not Python, so set the flag to false *)
      G.ExprStmt (G.Yield (tok, expr, false) |> G.e, semi) |> G.s

let map_multiplicative_operator (env : env) (x : CST.multiplicative_operator) :
    G.operator * G.ident =
  match x with
  | `STAR tok -> (G.Mult, (* "*" *) str env tok)
  | `SLASH tok -> (G.Div, (* "/" *) str env tok)
  | `PERC tok -> (G.Mod, (* "%" *) str env tok)

let map_inheritance_modifier (env : env) (x : CST.inheritance_modifier) =
  match x with
  | `Final tok -> (* "final" *) G.attr G.Final (token env tok)

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
  | `Over tok ->
      (* "override" *)
      G.attr G.Override (token env tok)
  | `Conv tok ->
      (* "convenience" *)
      G.unhandled_keywordattr (str env tok)
  | `Requ tok ->
      (* "required" *)
      G.unhandled_keywordattr (str env tok)
  | `Noni tok ->
      (* nonisolated *)
      G.unhandled_keywordattr (str env tok)

let map_try_operator (env : env) (x : CST.try_operator) =
  match x with
  | `Try tok -> (* "try" *) token env tok
  | `TryB tok -> (* "try!" *) token env tok
  | `TryQ tok -> (* "try?" *) token env tok

let map_special_literal (env : env) (x : CST.special_literal) =
  (match x with
  | `HASH_36725ee tok -> (* "#file" *) G.OtherExpr (str env tok, [])
  | `HASH_ee0b998 tok -> (* "#fileID" *) G.OtherExpr (str env tok, [])
  | `HASH_bd759bd tok -> (* "#filePath" *) G.OtherExpr (str env tok, [])
  | `HASH_709af6a tok -> (* "#line" *) G.OtherExpr (str env tok, [])
  | `HASH_be35129 tok -> (* "#column" *) G.OtherExpr (str env tok, [])
  | `HASH_96a7ced tok -> (* "#function" *) G.OtherExpr (str env tok, [])
  | `HASH_4d47dbe tok -> (* "#dsohandle" *) G.OtherExpr (str env tok, []))
  |> G.e

let map_integer_literal (env : env) (tok : CST.integer_literal) : G.literal =
  let s, t = str env tok in
  G.Int (int_of_string_opt s, t)

let map_mutation_modifier (env : env) (x : CST.mutation_modifier) =
  match x with
  | `Muta tok ->
      (* "mutating" *)
      G.unhandled_keywordattr (str env tok)
  | `Nonm tok ->
      (* "nonmutating" *)
      G.unhandled_keywordattr (str env tok)

let map_property_modifier (env : env) (x : CST.property_modifier) =
  match x with
  | `Static tok ->
      (* "static" *)
      G.attr G.Static (token env tok)
  (* Has to do with the Obj-C runtime... probably not important *)
  | `Dyna tok ->
      (* "dynamic" *)
      G.unhandled_keywordattr (str env tok)
  | `Opt tok ->
      (* Seems close enough to other Optional field uses.*)
      (* "optional" *)
      G.attr G.Optional (token env tok)
  | `Class tok ->
      (* "class" *)
      G.unhandled_keywordattr (str env tok)

let map_parameter_modifier (env : env) (x : CST.parameter_modifier) =
  (* TODO? I don't think any of these will actually matter.
   * `inout` potentially can have ramifications for constant propagation...
   * but I don't think we have the machinery to deal with that right now.
   *)
  match x with
  | `Inout tok ->
      (* "inout" *)
      G.unhandled_keywordattr (str env tok)
  | `ATes tok ->
      (* "@escaping" *)
      G.unhandled_keywordattr (str env tok)
  | `ATau tok ->
      (* "@autoclosure" *)
      G.unhandled_keywordattr (str env tok)

let map_throws (env : env) (x : CST.throws) =
  match x with
  | `Throws_kw tok -> (* throws_keyword *) G.attr G.Throws (token env tok)
  | `Rethrs_kw tok -> (* rethrows_keyword *) G.attr G.Rethrows (token env tok)

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
  | `Inhe_modi x -> map_inheritance_modifier env x

let map_custom_operator (env : env) (x : CST.custom_operator) =
  match x with
  | `Tok_pat_c201ddc x -> str env x
  | `Custom_op_ tok -> (* custom_operator_ *) str env tok

let map_setter_specifier (env : env) ((v1, v2) : CST.setter_specifier) =
  let v1 =
    match v1 with
    | Some x -> map_mutation_modifier env x |> todo env
    | None -> ()
  in
  str env v2

let map_modify_specifier (env : env) ((v1, v2) : CST.modify_specifier) =
  let v1 =
    match v1 with
    | Some x -> map_mutation_modifier env x |> todo env
    | None -> ()
  in
  let v2 = (* "_modify" *) str env v2 in
  v2

let map_constructor_function_decl (env : env)
    ((v1, v2) : CST.constructor_function_decl) =
  (* TODO special-case the constructor somehow? *)
  let v1 = (* "init" *) str env v1 in
  (* Bangs won't change the type, so we don't care about them. Question marks will, though. *)
  let is_quest =
    match v2 with
    | Some x -> (
        match x with
        | `Quest tok -> (* "?" *) true
        | `Bang tok -> (* bang *) false)
    | None -> false
  in
  (is_quest, v1)

let map_additive_operator (env : env) (x : CST.additive_operator) :
    G.operator * G.ident =
  match x with
  | `Plus_then_ws tok
  | `PLUS tok ->
      (G.Plus, (* "+" *) str env tok)
  | `Minus_then_ws tok
  | `DASH tok ->
      (G.Minus, (* "-" *) str env tok)

let map_non_local_scope_modifier (env : env) (x : CST.non_local_scope_modifier)
    =
  match x with
  | `Member_modi x -> map_member_modifier env x
  | `Visi_modi (v1, v2) -> (
      let attr =
        G.KeywordAttr
          (match v1 with
          (* See https://docs.swift.org/swift-book/ReferenceManual/Declarations.html#grammar_access-level-modifier
             `public`, `internal`, and `open` behave understandably, but `fileprivate` and `private` are
             kind of weird...
             We can keep it here for now and then change it if necessary.*)
          | `Public tok -> (* "public" *) (G.Public, token env tok)
          | `Priv tok -> (* "private" *) (G.Private, token env tok)
          | `Inte tok -> (* "internal" *) (G.Protected, token env tok)
          | `File tok -> (* "fileprivate" *) (G.Public, token env tok)
          | `Open tok -> (* "open" *) (G.Public, token env tok))
      in
      match v2 with
      | Some (v1, v2, v3) -> G.OtherAttribute (str env v2, [ G.At attr ])
      | None -> attr)
  | `Func_modi x -> map_function_modifier env x
  | `Muta_modi x -> map_mutation_modifier env x
  | `Prop_modi x -> map_property_modifier env x
  | `Param_modi x -> map_parameter_modifier env x
  | `Prop_beha_modi tok -> (* "lazy" *) G.attr G.Lazy (token env tok)

let map_parameter_modifiers (env : env) (xs : CST.parameter_modifiers) =
  Common.map (map_parameter_modifier env) xs

let map_simple_identifier (env : env) (x : CST.simple_identifier) : G.ident =
  match x with
  | `Pat_88eeeaa tok ->
      (* pattern [_\p{XID_Start}][_\p{XID_Continue}]* *) str env tok
  | `Pat_97d645c tok -> (* pattern `[^\r\n` ]*` *) str env tok
  | `Pat_c332828 tok -> (* pattern \$[0-9]+ *) str env tok
  | `Tok_dollar_pat_88eeeaa tok -> (* tok_dollar_pat_9d0cc04 *) str env tok
  | `Actor tok -> (* "actor" *) str env tok

let map_bound_identifier (env : env) (x : CST.bound_identifier) =
  map_simple_identifier env x

let map_equality_operator (env : env) (x : CST.equality_operator) =
  match x with
  | `BANGEQ tok -> (G.NotEq, (* "!=" *) str env tok)
  | `BANGEQEQ tok -> (G.NotPhysEq, (* "!==" *) str env tok)
  | `Eq_eq tok -> (G.Eq, (* eq_eq_custom *) str env tok)
  | `EQEQEQ tok -> (G.PhysEq, (* "===" *) str env tok)

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

let map_referenceable_operator (env : env) (x : CST.referenceable_operator) =
  match x with
  | `Custom_op x ->
      let ((s, tok) as ident) = map_custom_operator env x in
      ((s, tok), G.N (H2.name_of_id ident))
  | `Comp_op x ->
      let op, (s, tok) = map_comparison_operator env x in
      ((s, tok), G.IdSpecial (G.Op op, tok))
  | `Addi_op x ->
      let op, (s, tok) = map_additive_operator env x in
      ((s, tok), G.IdSpecial (G.Op op, tok))
  | `Mult_op x ->
      let op, (s, tok) = map_multiplicative_operator env x in
      ((s, tok), G.IdSpecial (G.Op op, tok))
  | `Equa_op x ->
      let op, (s, tok) = map_equality_operator env x in
      ((s, tok), G.IdSpecial (G.Op op, tok))
  | `Assign_and_op x ->
      let x = map_assignment_and_operator in
      todo env x
  (* TODO There is no good reason for these to be postfix, but this is not determinable right now.
     Fix later.
  *)
  | `PLUSPLUS tok ->
      (* "++" *)
      let s, tok = str env tok in
      ((s, tok), G.IdSpecial (G.IncrDecr (G.Incr, G.Postfix), tok))
  | `DASHDASH tok ->
      (* "--" *)
      let s, tok = str env tok in
      ((s, tok), G.IdSpecial (G.IncrDecr (G.Decr, G.Postfix), tok))
  | `Bang tok ->
      (* bang *)
      let s, tok = str env tok in
      ((s, tok), G.IdSpecial (G.Op G.Not, tok))
  | `TILDE tok ->
      (* "~" *)
      let s, tok = str env tok in
      ((s, tok), G.IdSpecial (G.Op G.BitNot, tok))
  | `BAR tok ->
      (* "|" *)
      let s, tok = str env tok in
      ((s, tok), G.IdSpecial (G.Op G.BitOr, tok))
  | `HAT tok ->
      (* "^" *)
      let s, tok = str env tok in
      ((s, tok), G.IdSpecial (G.Op G.BitXor, tok))
  | `LTLT tok ->
      (* "<<" *)
      let s, tok = str env tok in
      ((s, tok), G.IdSpecial (G.Op G.LSL, tok))
  | `GTGT tok ->
      (* ">>" *)
      let s, tok = str env tok in
      ((s, tok), G.IdSpecial (G.Op G.ASR, tok))

let map_operator_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.operator_declaration) : G.stmt =
  let v1 =
    match v1 with
    | `Prefix tok -> (* "prefix" *) str env tok
    | `Infix tok -> (* "infix" *) str env tok
    | `Post tok -> (* "postfix" *) str env tok
  in
  let v2 = (* "operator" *) token env v2 in
  let v3 = map_referenceable_operator env v3 |> fst in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_simple_identifier env v2 in
        Some v2
    | None -> None
  in
  (* Operator declaration body. According to the tree-sitter-swift grammar: "The
   * Swift compiler no longer accepts these, but some very old code still uses
   * it." *)
  let _ = v5 in
  (* Swift allows you to declare custom operators. The operator declaration just
   * introduces the operator and specifies whether it is a prefix, infix, or
   * postfix operator, as well as optionally allows a precedence declaration.
   * Defining the actual behavior of an operator is done in another construct.
   *
   * Hopefully we can get good enough results without tracking the precedence
   * and fixity of every custom operator and using that to inform parsing.
   *
   * https://docs.swift.org/swift-book/LanguageGuide/AdvancedOperators.html#ID46
   *)
  let anys = [ v1; v3 ] @ Option.to_list v4 |> Common.map (fun x -> G.I x) in
  G.OtherStmt (G.OS_Todo, anys) |> G.s

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
  let name = map_simple_identifier env v1 |> H2.name_of_id in
  let v2 = (* ":" *) token env v2 in
  let v3 =
    match v3 with
    | `Simple_id x ->
        G.NamedAttr
          ( v2,
            name,
            G.fake_bracket
              [
                G.Arg (G.N (map_simple_identifier env x |> H2.name_of_id) |> G.e);
              ] )
    | `Bool_lit x ->
        G.NamedAttr
          ( v2,
            name,
            G.fake_bracket [ G.Arg (G.L (map_boolean_literal env x) |> G.e) ] )
  in
  G.At v3

let map_tuple_type_item_identifier (env : env)
    ((v1, v2, v3) : CST.tuple_type_item_identifier) =
  (* I don't really know why we permit underscores here. *)
  let v1 =
    match v1 with
    | Some tok -> (* "_" *) token env tok |> todo env
    | None -> ()
  in
  let v2 = map_simple_identifier env v2 in
  let v3 = (* ":" *) token env v3 in
  v2

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
      | `Throws x -> map_throws env x |> todo env)
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
    | `Refe_op x -> map_referenceable_operator env x |> fst
  in
  v2

let map_getter_specifier (env : env) ((v1, v2, v3) : CST.getter_specifier) =
  let v1 =
    match v1 with
    | Some x -> map_mutation_modifier env x |> todo env
    | None -> ()
  in
  let v2 = (* "get" *) str env v2 in
  let v3 =
    match v3 with
    | Some x -> map_getter_effects env x |> todo env
    | None -> ()
  in
  v2

let map_availability_argument (env : env) (x : CST.availability_argument) =
  (* This does not seem important semantically.
     Availability arguments just allow code to be annotated with its availability
     with respect to certain macOS (and related) versions.
     https://docs.swift.org/swift-book/ReferenceManual/Statements.html#grammar_availability-condition
  *)
  match x with
  | `Id_int_lit_rep_DOT_int_lit (v1, v2, v3) ->
      let v1 = map_identifier env v1 in
      let v2 = (* integer_literal *) str env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "." *) token env v1 in
            let v2 = (* integer_literal *) token env v2 in
            G.Tk v2)
          v3
      in
      G.OtherExpr (v2, G.Di v1 :: G.Tk (v2 |> snd) :: v3) |> G.e
  | `STAR tok ->
      let v = str env tok in
      (* "*" *)
      G.OtherExpr (v, [ G.Tk (v |> snd) ]) |> G.e

let map_precedence_group_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.precedence_group_declaration) =
  let v1 = (* "precedencegroup" *) token env v1 in
  let v2 = map_simple_identifier env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 =
    match v4 with
    | Some x -> map_precedence_group_attributes env x
    | None -> []
  in
  let v5 = (* "}" *) token env v5 in
  G.OtherStmt (G.OS_Todo, G.I v2 :: v4) |> G.s

let map_protocol_property_requirements (env : env)
    ((v1, v2, v3) : CST.protocol_property_requirements) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Getter_spec x -> map_getter_specifier env x |> snd
        | `Setter_spec x -> map_setter_specifier env x |> snd)
      v2
  in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

let rec map_annotated_inheritance_specifier (env : env)
    ((v1, v2) : CST.annotated_inheritance_specifier) =
  let v1 = Common.map (map_attribute env) v1 in
  (* TODO attribute *)
  let v2 = map_inheritance_specifier env v2 in
  v2

(* Similarly to how Java handles enum fields, each are parsed as an
   EnumEntryDef.
*)
and map_enum_entry_suffix (env : env) (ent : G.entity)
    (x : CST.enum_entry_suffix) : G.stmt =
  match x with
  | `Enum_type_params (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let init_tok = v1 in
      let fields =
        match v2 with
        | Some (v1, v2, v3, v4) ->
            (* This is like how CPP does it. If there's no name, the entity just
               has an anonymous name.
            *)
            let mk_field id_opt ty expr_opt =
              let name =
                match id_opt with
                | Some x ->
                    let ident = map_tuple_type_item_identifier env x in
                    G.EN (G.Id (ident, G.empty_id_info ()))
                | None -> G.OtherEntity (("AnonTupleField", init_tok), [])
              in
              let ty = map_type_ env ty in
              let ent = { G.name; attrs = []; tparams = [] } in
              G.DefStmt (ent, G.FieldDefColon { vinit = None; vtype = Some ty })
            in
            let field_first = mk_field v1 v2 v3 in
            let field_rest =
              Common.map
                (fun (v1, v2, v3, v4) ->
                  let v1 = (* "," *) token env v1 in
                  mk_field v2 v3 v4)
                v4
            in
            Common.map (fun x -> G.F (x |> G.s)) (field_first :: field_rest)
        | None -> []
      in
      let v3 = (* ")" *) token env v3 in
      let ty = G.TyRecordAnon ((G.Class, v1), (v1, fields, v3)) |> G.t in
      let defkind =
        G.EnumEntryDef
          { ee_args = Some (G.fake_bracket [ G.ArgType ty ]); ee_body = None }
      in
      G.DefStmt (ent, defkind) |> G.s
  | `Equal_sign_exp (v1, v2) ->
      let v1 = (* eq_custom *) token env v1 in
      let exp = map_expression env v2 in
      let defkind =
        G.EnumEntryDef
          { ee_args = Some (G.fake_bracket [ G.arg exp ]); ee_body = None }
      in
      G.DefStmt (ent, defkind) |> G.s

and map_type_casting_pattern (env : env) (x : CST.type_casting_pattern) =
  match x with
  | `Is_type (v1, v2) ->
      let v1 = (* "is" *) token env v1 in
      let v2 = map_type_ env v2 in
      G.PatType v2
  | `Bind_pat_no_expr_as_type (v1, v2, v3) ->
      let v1 = map_binding_pattern_no_expr env v1 in
      let v2 = (* as_custom *) token env v2 in
      let v3 = map_type_ env v3 in
      (* This is only kind of correct. It's actually a subclassing thing. *)
      PatTyped (v1, v3)

and map_computed_getter (env : env) ((v1, v2, v3) : CST.computed_getter) =
  let v1 = Common.map (map_attribute env) v1 in
  let v2 = map_getter_specifier env v2 in
  (* Appending to a singleton because I want the attributes in the same order.
   *)
  let attrs = v1 @ [ G.unhandled_keywordattr v2 ] in
  let v3 =
    match v3 with
    | Some x -> G.FBStmt (map_function_body env x)
    | None -> G.FBNothing
  in
  G.DefStmt
    ( { G.name = G.OtherEntity (v2, []); attrs = []; tparams = [] },
      G.FuncDef
        {
          G.fkind = (G.Method, v2 |> snd);
          G.fparams = [];
          G.frettype = None;
          G.fbody = v3;
        } )
  |> G.s

and map_computed_modify (env : env) ((v1, v2, v3) : CST.computed_modify) =
  let v1 = Common.map (map_attribute env) v1 in
  let v2 = map_modify_specifier env v2 in
  (* Appending to a singleton because I want the attributes in the same order.
   *)
  let attrs = v1 @ [ G.unhandled_keywordattr v2 ] in
  let fbody =
    match v3 with
    | Some x -> G.FBStmt (map_function_body env x)
    | None -> G.FBNothing
  in
  G.DefStmt
    ( { G.name = G.OtherEntity (v2, []); attrs; tparams = [] },
      G.FuncDef
        {
          G.fkind = (G.Method, v2 |> snd);
          G.fparams = [];
          G.frettype = None;
          G.fbody;
        } )
  |> G.s

and map_computed_property (env : env) ((v1, v2, v3) : CST.computed_property) =
  let v1 = (* "{" *) token env v1 in
  let v3 = (* "}" *) token env v3 in
  match v2 with
  | `Opt_stmts opt ->
      let stmts =
        match opt with
        | Some x -> map_statements env x
        | _ -> []
      in
      G.Block (v1, stmts, v3) |> G.s
  | `Rep_choice_comp_getter xs ->
      let getters_setters =
        Common.map
          (fun x ->
            match x with
            | `Comp_getter x -> map_computed_getter env x
            | `Comp_setter x -> map_computed_setter env x
            | `Comp_modify x -> map_computed_modify env x)
          xs
      in
      G.Block (v1, getters_setters, v3) |> G.s

and map_computed_setter (env : env) ((v1, v2, v3, v4) : CST.computed_setter) =
  let v1 = Common.map (map_attribute env) v1 in
  let v2 = map_setter_specifier env v2 in
  (* Appending to a singleton because I want the attributes in the same order.
   *)
  let attrs = v1 @ [ G.unhandled_keywordattr v2 ] in
  (* TODO? There's some weird semantics here about a name for the argument to
     pass in to willSet and didSet.
     I'm gonna say these don't matter for now.
  *)
  let fparams =
    match v3 with
    | Some (v1, v2, v3) ->
        let v1 = (* "(" *) token env v1 in
        let v2 = map_simple_identifier env v2 in
        let v3 = (* ")" *) token env v3 in
        [ G.Param (G.param_of_id v2) ]
    | None -> []
  in
  let fbody =
    match v4 with
    | Some x -> G.FBStmt (map_function_body env x)
    | None -> G.FBNothing
  in
  G.DefStmt
    ( { G.name = G.OtherEntity (v2, []); attrs; tparams = [] },
      G.FuncDef
        {
          G.fkind = (G.Method, v2 |> snd);
          G.fparams;
          G.frettype = None;
          G.fbody;
        } )
  |> G.s

and map_array_type (env : env) ((v1, v2, v3) : CST.array_type) : G.type_ =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 = (* "]" *) token env v3 in
  G.TyArray ((v1, None, v3), v2) |> G.t

and map_associatedtype_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.associatedtype_declaration) =
  let v2 = (* "associatedtype" *) token env v2 in
  (* a default value for the associated type is a modifier on the actual
     definition
  *)
  let modifiers =
    let default =
      match v6 with
      | Some (v1, ty) ->
          let v1 = (* eq_custom *) token env v1 in
          let ty = map_type_ env ty in
          [ G.OtherAttribute (("DefaultType", v2), [ G.T ty ]) ]
      | None -> []
    in
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> [])
    @ default
  in
  let id = map_simple_identifier env v3 in
  (* however, type constraints are modifiers on the type *)
  let protocol =
    let tconstraints =
      match v5 with
      | Some x -> map_type_constraints env x
      | None -> []
    in
    match v4 with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        [ G.T { v2 with G.t_attrs = tconstraints @ v2.G.t_attrs } ]
    | None -> []
  in
  G.DefStmt
    ( G.basic_entity ~attrs:modifiers id,
      G.TypeDef { tbody = G.OtherTypeKind (("Protocol", v2), protocol) } )
  |> G.s

and map_attribute_argument (env : env) (x : CST.attribute_argument) =
  match x with
  | `Simple_id_COLON_exp (v1, v2, v3) ->
      let v1 = map_bound_identifier env v1 in
      let v3 = map_expression env v3 in
      G.OtherArg (str env v2, [ G.I v1; G.E v3 ])
  | `Exp x ->
      G.OtherArg
        (("AttrArgExp", G.fake "AttrArgExp"), [ G.E (map_expression env x) ])
  | `Rep1_simple_id_COLON xs ->
      G.OtherArg
        ( ("AttrBoundIds", G.fake "AttrBoundIds"),
          Common.map
            (fun (v1, v2) ->
              let v1 = map_bound_identifier env v1 in
              let v2 = (* ":" *) token env v2 in
              G.I v1)
            xs )
  | `Rep1_simple_id_int_lit_rep_DOT_int_lit (v1, v2, v3) ->
      let v1 = Common.map (map_bound_identifier env) v1 in
      let v2 = (* integer_literal *) token env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "." *) token env v1 in
            let v2 = (* integer_literal *) token env v2 in
            G.Tk v2)
          v3
      in
      G.OtherArg
        (("AttrBoundIdAndInts", G.fake "AttrBoundIdAndInts"), G.Tk v2 :: v3)

and map_attribute (env : env) ((v1, v2, v3) : CST.attribute) =
  let at_tok = (* "@" *) token env v1 in
  let attr_name = map_user_type_name env v2 in
  let args =
    match v3 with
    | Some (v1, v2, v3, v4) ->
        let v1 = (* "(" *) token env v1 in
        let v2 = map_attribute_argument env v2 in
        let v3 =
          Common.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = map_attribute_argument env v2 in
              v2)
            v3
        in
        let v4 = (* ")" *) token env v4 in
        (v1, v2 :: v3, v4)
    | None -> G.fake_bracket []
  in
  G.NamedAttr (at_tok, attr_name, args)

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
  let opcall (op, (s, tok)) = G.opcall (op, tok) in
  match x with
  | `Mult_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_multiplicative_operator env v2 in
      let v3 = map_expression env v3 in
      opcall v2 [ v1; v3 ]
  | `Addi_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_additive_operator env v2 in
      let v3 = map_expression env v3 in
      opcall v2 [ v1; v3 ]
  | `Range_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_range_operator env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      G.opcall (G.Range, v2) [ v1; v3 ]
  | `Infix_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_custom_operator env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      G.Call
        (G.N (H2.name_of_id v2) |> G.e, G.fake_bracket [ G.Arg v1; G.Arg v3 ])
      |> G.e
  | `Nil_coal_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* nil_coalescing_operator_custom *) token env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      G.opcall (G.Nullish, v2) [ v1; v3 ]
  | `Check_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "is" *) token env v2 in
      let v3 = map_type_ env v3 in
      G.special (G.Instanceof, v2) [ v1; expr_of_type v3 ]
  | `Equa_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_equality_operator env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      opcall v2 [ v1; v3 ]
  | `Comp_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_comparison_operator env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      opcall v2 [ v1; v3 ]
  | `Conj_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* conjunction_operator_custom *) token env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      G.opcall (G.And, v2) [ v1; v3 ]
  | `Disj_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* disjunction_operator_custom *) token env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      G.opcall (G.Or, v2) [ v1; v3 ]
  | `Bitw_oper (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_bitwise_binary_operator env v2 in
      let v3 = map_expr_hack_at_ternary_binary_suffix env v3 in
      opcall v2 [ v1; v3 ]

and apply_pattern_kinds (env : env) (pat : G.pattern) kinds =
  List.fold_right (fun kind pat -> G.OtherPat (kind, [ G.P pat ])) kinds pat

and map_binding_pattern (env : env) ((_v1, v2, v3) : CST.binding_pattern) =
  let pat = map_no_expr_pattern_already_bound env v3 in
  let kinds = map_binding_pattern_kind env v2 in
  apply_pattern_kinds env pat kinds

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
        | G.L lit -> G.PatLiteral lit
        | _ -> todo env ())
  in
  (* As elsewhere, it looks like this question mark isn't important right now.
     See `map_expression`.
  *)
  let v2 =
    match v2 with
    | Some tok ->
        (* "?" *)
        let todo = token env tok in
        ()
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
  let attrs =
    match v1 with
    | Some x -> map_modifiers env x
    | None -> []
  in
  (* I'm not sure this does anything. "class" is already a possible modifier
     from the `map_modifiers` - it's a property modifier.
  *)
  let v2 =
    match v2 with
    | Some tok ->
        (* "class" *)
        token env tok |> todo env
    | None -> ()
  in
  let v3 =
    map_modifierless_function_declaration_no_body env ~in_class ~attrs v3 body
  in
  v3

and map_call_expression (env : env) ((v1, v2) : CST.call_expression) : G.expr =
  let v1 = map_expression env v1 in
  let v2 = map_call_suffix env v2 in
  G.Call (v1, v2) |> G.e

and map_call_suffix (env : env) (v1 : CST.call_suffix) : G.arguments =
  match v1 with
  | `Value_args x -> map_expr_hack_at_ternary_binary_call_suffix env x
  | `Fn_call_lambda_args x ->
      map_fn_call_lambda_arguments env x |> G.fake_bracket
  | `Value_args_fn_call_lambda_args (v1, v2) ->
      let _b1, v1, _b2 = map_expr_hack_at_ternary_binary_call_suffix env v1 in
      let v2 = map_fn_call_lambda_arguments env v2 in
      v1 @ v2 |> G.fake_bracket

and map_fn_call_lambda_arguments (env : env)
    ((v1, v2) : CST.fn_call_lambda_arguments) : G.argument list =
  (* When one or more lambda literals are provided after (or instead of)
   * parenthesized arguments to a function call, they are the final arguments to
   * that function call. In this case, the labels are not provided even if the
   * function call would normally require them. *)
  (* TODO make sure that Semgrep can find these arguments when looking for the
   * corresponding labeled argument. e.g. `foo(bar: { x in x + 1 })` is the same
   * as `foo { x in x + 1 }` and a search for `foo` called with a labeled `bar`
   * argument should probably find the latter as well as the former. *)
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
  anon_arg :: labeled_args

and map_capture_list (env : env) ((v1, v2, v3, v4) : CST.capture_list) :
    G.parameter list =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_capture_list_item env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        map_capture_list_item env v2)
      v3
  in
  let v4 = (* "]" *) token env v4 in
  v2 :: v3

and map_capture_list_item (env : env) (x : CST.capture_list_item) =
  match x with
  | `Self_exp tok ->
      (* "self" *) G.Param (G.param_of_id ("self", token env tok))
  | `Opt_owne_modi_simple_id_opt_equal_sign_exp (v1, v2_tok, v3) ->
      let pattrs =
        match v1 with
        | Some x -> [ map_ownership_modifier env x ]
        | None -> []
      in
      let id = map_simple_identifier env v2_tok in
      let pdefault =
        match v3 with
        | Some (v1, v2) ->
            let expr = G.N (H2.name_of_id id) |> G.e in
            let v1 = (* eq_custom *) token env v1 in
            Some (map_expression env v2)
        | None -> None
      in
      G.Param (G.param_of_id ~pattrs ~pdefault id)

and map_catch_block (env : env) ((v1, v2, v3, v4) : CST.catch_block) =
  let catch_tok = (* catch_keyword *) token env v1 in
  let pat =
    match (v2, v3) with
    (* Similar to how Python does it: *)
    | None, None -> G.PatUnderscore (Parse_info.fake_info catch_tok "_")
    | Some v2, None -> map_binding_pattern_no_expr env v2
    (* This is impossible according to the Swift grammar - you can't have a `where`
       on the caught thing unless there was a pattern to modify in the first place.
    *)
    | None, Some _ -> raise Common.Impossible
    | Some v2, Some v3 ->
        G.PatWhen (map_binding_pattern_no_expr env v2, map_where_clause env v3)
  in
  let stmt = map_function_body env v4 in
  (catch_tok, G.CatchPattern pat, stmt)

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
  let modifiers =
    match v1 with
    | Some x -> map_modifiers env x
    | None -> []
  in
  map_modifierless_class_declaration env modifiers v2

and map_class_member_separator (env : env) (x : CST.class_member_separator) =
  match x with
  | `Semi tok -> map_semi env tok |> snd
  | `Mult_comm_expl x -> map_multiline_comment_explicit env x

and map_multiline_comment_explicit (env : env)
    (() : CST.multiline_comment_explicit) =
  G.sc

and map_class_member_declarations (env : env)
    ((v1, v2, v3) : CST.class_member_declarations) =
  let v1 = map_type_level_declaration env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = map_class_member_separator env v1 in
        let v2 = map_type_level_declaration env v2 in
        v2)
      v2
  in
  let fields =
    List.concat_map
      (fun stmts -> Common.map (fun stmt -> G.F stmt) stmts)
      (v1 :: v2)
  in
  let v3 =
    match v3 with
    | Some x ->
        let _ = map_class_member_separator env x in
        ()
    | None -> ()
  in
  fields

and map_constructor_suffix (env : env) (v1 : CST.constructor_suffix) =
  match v1 with
  | `Cons_value_args x -> map_constructor_value_arguments env x
  | `Fn_call_lambda_args x ->
      map_fn_call_lambda_arguments env x |> G.fake_bracket
  | `Cons_value_args_fn_call_lambda_args (v1, v2) ->
      let _b1, v1, _b2 = map_constructor_value_arguments env v1 in
      let v2 = map_fn_call_lambda_arguments env v2 in
      v1 @ v2 |> G.fake_bracket

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
  | `Throw_stmt x -> map_throw_statement env x semi
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
    | Some x -> map_modifiers env x
    | None -> []
  in
  let v2 = (* "deinit" *) str env v2 in
  let v3 = map_function_body env v3 in
  let entity = G.basic_entity ~attrs:v1 v2 in
  let definition_kind =
    G.FuncDef
      {
        fkind = (G.Method, snd v2);
        fparams = [];
        frettype = None;
        fbody = G.FBStmt v3;
      }
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
    ((v1, v2) : CST.binding_kind_and_pattern) : G.ident list * G.pattern =
  let pat = map_no_expr_pattern_already_bound env v2 in
  let kinds = map_possibly_async_binding_pattern_kind env v1 in
  (kinds, pat)

and map_direct_or_indirect_binding (env : env)
    ((v1, v2) : CST.direct_or_indirect_binding) =
  let pat =
    match v1 with
    | `Bind_kind_and_pat x ->
        let kinds, pat = map_binding_kind_and_pattern env x in
        apply_pattern_kinds env pat kinds
    | `Case_bind_pat_no_expr (v1, v2) ->
        let v1 = (* "case" *) token env v1 in
        map_binding_pattern_no_expr env v2
  in
  let add_pat_type pat =
    match v2 with
    | Some x -> G.PatTyped (pat, map_type_annotation env x)
    | None -> pat
  in
  pat |> add_pat_type

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
  | `Post_exp (e, op) ->
      let e = map_expression env e in
      map_postfix_unary_operator env op e

and map_do_statement (env : env) ((v1, v2, v3) : CST.do_statement) =
  let do_tok = (* "do" *) token env v1 in
  let v2 = map_function_body env v2 in
  let v3 = Common.map (map_catch_block env) v3 in
  (* TODO? A do statement is not quite the same as a `try`... but it's close
     enough?
  *)
  G.Try (do_tok, v2, v3, None) |> G.s

and map_else_options (env : env) (x : CST.else_options) =
  match x with
  | `Blk x -> map_block env x
  | `If_stmt x -> map_if_statement env x

and map_enum_class_body (enum_ident : G.ident) (env : env)
    ((v1, v2, v3) : CST.enum_class_body) =
  let v1 = (* "{" *) token env v1 in
  let v3 = (* "}" *) token env v3 in
  (* We map every enum variant into an EnumEntryDef, even if it's a
     raw style vs union style enum.
  *)
  let fields =
    Common.map
      (fun x ->
        match x with
        | `Enum_entry x -> map_enum_entry env x
        | `Type_level_decl x -> map_type_level_declaration env x)
      v2
    |> List.concat_map (fun fields -> Common.map (fun x -> G.F x) fields)
  in
  (v1, fields, v3)

and map_enum_entry (env : env) ((v1, v2, v3, v4, v5, v6, _v7) : CST.enum_entry)
    : G.stmt list =
  let modifiers =
    let v2 =
      match v2 with
      | Some tok ->
          (* "indirect" *)
          [ G.unhandled_keywordattr (str env tok) ]
      | None -> []
    in
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> [])
    @ v2
  in
  let mk_variant id suffix_opt =
    let ent = G.basic_entity ~attrs:modifiers (map_simple_identifier env v4) in
    match v5 with
    | Some x -> map_enum_entry_suffix env ent x
    | None ->
        G.DefStmt (ent, G.EnumEntryDef { ee_args = None; ee_body = None })
        |> G.s
  in
  let variant_first = mk_variant v4 v5 in
  let variants_rest = Common.map (fun (v1, v2, v3) -> mk_variant v2 v3) v6 in
  variant_first :: variants_rest

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
  | `Semg_exp_ellips tok ->
      let tok = (* three_dot_operator_custom *) token env tok in
      G.Ellipsis tok |> G.e
  | `Semg_deep_ellips (v1, v2, v3) ->
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let str, tok = (* "...>" *) map_custom_operator env v3 in
      (* See https://github.com/returntocorp/ocaml-tree-sitter-semgrep/pull/343
       * for information on why ...> is parsed as a custom operator.
       *
       * When this is handled, check that the custom operator actually is `...>`
       * and fail if not. That way, if parsing is fixed later, we won't start
       * failing to parse rules where people accidentally wrote `<... x ..>` or
       * something similar.
       * *)
      todo env (v1, v2, v3)

and map_for_statement (env : env)
    ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.for_statement) =
  let for_tok = (* "for" *) token env v1 in
  let pat =
    let pat_init = map_binding_pattern_no_expr env v4 in
    let add_pat_ty pat =
      match v5 with
      | Some x -> G.PatTyped (pat, map_type_annotation env x)
      | None -> pat
    in
    let add_pat_where pat =
      match v8 with
      | Some x -> G.PatWhen (pat, map_where_clause env x)
      | None -> pat
    in
    let add_pat_try pat =
      match v2 with
      | Some x (* "try" *) ->
          G.OtherPat (("Try", map_try_operator env x), [ G.P pat ])
      | None -> pat
    in
    let add_pat_await pat =
      match v3 with
      | Some tok (* "await" *) ->
          G.OtherPat (("Await", token env tok), [ G.P pat ])
      | None -> pat
    in
    pat_init |> add_pat_ty |> add_pat_where |> add_pat_try |> add_pat_await
  in
  let header =
    let in_tok = (* "in" *) token env v6 in
    let exp = map_expression env v7 in
    G.ForEach (pat, in_tok, exp)
  in
  let body = map_function_body env v9 in
  G.For (for_tok, header, body) |> G.s

and map_function_body (env : env) (x : CST.function_body) : G.stmt =
  map_block env x

and map_function_declaration (env : env) ~in_class
    ((v1, v2) : CST.function_declaration) =
  let v2 = map_function_body env v2 in
  let v1 = map_bodyless_function_declaration env ~in_class v1 (G.FBStmt v2) in
  v1

and map_function_type (env : env) ((v1, v2, v3, v4, v5) : CST.function_type) =
  let v1 =
    match v1 with
    | `Tuple_type x -> map_tuple_type env x
    | `Unan_type x -> map_unannotated_type env x
  in
  let attrs =
    let v2 =
      match v2 with
      | Some tok ->
          (* async_keyword_custom *) [ G.attr G.Async (token env tok) ]
      | None -> []
    in
    let v3 =
      match v3 with
      | Some x -> [ map_throws env x ]
      | None -> []
    in
    v2 @ v3
  in
  let v4 = (* arrow_operator_custom *) token env v4 in
  let v5 = map_type_ env v5 in
  G.TyFun ([ G.Param (G.param_of_type ~pattrs:attrs v1) ], v5) |> G.t

and map_function_value_parameter (env : env)
    ((v1, v2, v3) : CST.function_value_parameter) : G.parameter =
  let modifiers =
    match v1 with
    | Some x -> [ map_attribute env x ]
    | None -> []
  in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = (* eq_custom *) token env v1 in
        let v2 = map_expression env v2 in
        Some v2
    | None -> None
  in
  map_parameter env v2 v3 ~attrs:modifiers

and map_function_value_parameters (env : env)
    (xs : CST.function_value_parameters) : G.parameters =
  (* Normally there is only one parameter list, but evidently Swift used to
   * allow multiple, and the tree-sitter grammar handles that. This is unlikely
   * to come up with any frequency, but when it does we'll just combine into one
   * parameter list *)
  List.concat_map
    (fun (v1, v2, v3) ->
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
      v2)
    xs

and map_guard_statement (env : env) ((v1, v2, v3, v4, v5) : CST.guard_statement)
    =
  (* A guard statement seems to be semantically equivalent to an "if not".
   *)
  let v1 = (* "guard" *) token env v1 in
  let v2 = map_if_condition_sequence_item env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_if_condition_sequence_item env v2 in
        v2)
      v3
  in
  let cond = combine_conds v2 v3 in
  let body = map_function_body env v5 in
  G.If (v1, G.Cond (G.special (G.Op G.Not, G.fake "not") [ cond ]), body, None)
  |> G.s

(* Swift allows conditions to appear in a comma-separated list. The semantics are such that
   it only proceeds into the case if all of them are true. It also short-circuits.
   In other words, it's just an AND of all of their values. So we choose to represent it as
   such with this function.
*)
and combine_conds cond conds =
  List.fold_left
    (fun acc x ->
      G.Call
        ( G.IdSpecial (G.Op G.And, G.fake "&&") |> G.e,
          G.fake_bracket [ G.Arg acc; G.Arg x ] )
      |> G.e)
    cond conds

and map_if_condition_sequence_item (env : env)
    (x : CST.if_condition_sequence_item) : G.expr =
  match x with
  | `If_let_bind (v1, v2, v3) ->
      let v1 = map_direct_or_indirect_binding env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = (* eq_custom *) token env v1 in
            let v2 = map_expression env v2 in
            v2
        | None ->
            (* Does not appear to be valid Swift code *)
            failwith "Missing initializer in condition"
      in
      let v3 =
        match v3 with
        | Some x ->
            (* No longer valid Swift code, but added to the grammar to support
             * very old Swift code that used to be legal *)
            todo env x
        | None -> ()
      in
      G.LetPattern (v1, v2) |> G.e
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
            v2)
          v4
      in
      let v5 = (* ")" *) token env v5 in
      (* See `map_repeat_while_statement` if this returns a non-`Cond`.
       *)
      combine_conds v3 v4

and map_if_statement (env : env) ((v1, v2, v3, v4, v5) : CST.if_statement) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_if_condition_sequence_item env v2 in
  let v3 =
    (* TODO: looks like we could desugar this to a bunch of And expressions, but
     * need to double-check semantics. For now just raise if we encounter this.
     * *)
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_if_condition_sequence_item env v2 in
        v2)
      v3
  in
  let cond = combine_conds v2 v3 in
  let v4 = map_block env v4 in
  let v5 =
    match v5 with
    | Some (v1, v2) ->
        let v1 = (* else *) token env v1 in
        let v2 = map_else_options env v2 in
        Some v2
    | None -> None
  in
  G.If (v1, G.Cond cond, v4, v5) |> G.s

and map_import_declaration (env : env)
    ((v1, v2, v3, v4) : CST.import_declaration) =
  let attrs =
    match v1 with
    | Some x -> map_modifiers env x
    | None -> []
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
    {
      G.d = G.ImportAll (v2, G.DottedName v4, PI.unsafe_fake_info "");
      d_attrs = attrs;
    }
  |> G.s

and map_inheritance_specifier (env : env) (x : CST.inheritance_specifier) =
  match x with
  | `User_type x -> map_user_type env x
  | `Func_type x -> map_function_type env x

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
    let ident_of x =
      let tok = token env x in
      (PI.str_of_info tok, tok)
    in
    Option.map ident_of v1
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
  match v1 with
  | None -> v2
  | Some ident -> G.Label (ident, v2) |> G.s

(* This is a misleading name, it has nothing to do with the lambda's type...
   This is really mapping over the lambda's parameters.
*)
and map_lambda_function_type (env : env)
    ((v1, v2, v3, v4) : CST.lambda_function_type) :
    G.parameter list * G.type_ option =
  let params =
    match v1 with
    | `Lambda_func_type_params x -> map_lambda_function_type_parameters env x
    | `LPAR_opt_lambda_func_type_params_RPAR (v1, v2, v3) ->
        let v1 = (* "(" *) token env v1 in
        let v2 =
          match v2 with
          | Some x -> map_lambda_function_type_parameters env x
          | None -> []
        in
        let v3 = (* ")" *) token env v3 in
        v2
  in
  let attrs =
    let v2 =
      match v2 with
      | Some tok ->
          (* async_keyword_custom *) [ G.attr G.Async (token env tok) ]
      | None -> []
    in
    let v3 =
      match v3 with
      | Some x -> [ map_throws env x ]
      | None -> []
    in
    v2 @ v3
  in
  let rettype =
    match v4 with
    | Some (v1, v2) ->
        let v1 = (* arrow_operator_custom *) token env v1 in
        let v2 = map_possibly_implicitly_unwrapped_type env v2 in
        Some { v2 with G.t_attrs = attrs @ v2.G.t_attrs }
    | None -> None
  in
  (params, rettype)

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

and map_lambda_literal (env : env) ((v1, v2, v3, v4) : CST.lambda_literal) :
    G.expr =
  let v1 =
    match v1 with
    | `LCURL tok -> (* "{" *) token env tok
    | `HATLCURL tok -> (* "^{" *) token env tok
  in
  let captures, params, rettype =
    match v2 with
    | Some x -> map_lambda_type_declaration env x
    | None -> ([], [], None)
  in
  let body =
    let stmts =
      match v3 with
      (* TODO model implicit return for single-statement closures *)
      | Some x -> map_statements env x
      | None -> []
    in
    (* Fake brackets here since the brackets delimit the lambda expression as a
     * whole, not just the statements *)
    (* TODO consider using `in` and the closing bracket as the delimiters *)
    (* We inject into an `OtherStmtWithStmt` here because we needed somewhere for the
       capture list to go. This is not permitted with just a regular statement, so there is
       a new variant `OSWS_Closure` to signify this.
    *)
    G.FBStmt
      (G.OtherStmtWithStmt
         ( G.OSWS_Closure,
           Common.map (fun x -> G.Pa x) captures,
           G.Block (G.fake_bracket stmts) |> G.s )
      |> G.s)
  in
  let v4 = (* "}" *) token env v4 in
  let def =
    {
      G.fkind = (G.LambdaKind, v1);
      fparams = params;
      frettype = rettype;
      fbody = body;
    }
  in
  G.Lambda def |> G.e

and map_lambda_type_declaration (env : env)
    ((v1, v2, v3, v4) : CST.lambda_type_declaration) =
  let v1 = Common.map (map_attribute env) v1 in
  let captures =
    match v2 with
    | Some x -> map_capture_list env x
    | None -> []
  in
  let params, rettype =
    match v3 with
    | Some x -> map_lambda_function_type env x
    | None -> ([], None)
  in
  let v4 = (* "in" *) token env v4 in
  (captures, params, rettype)

and map_lambda_parameter (env : env) (v1 : CST.lambda_parameter) : G.parameter =
  match v1 with
  | `Self_exp tok -> (* "self" *) G.OtherParam (("Self", token env tok), [])
  | `Simple_id x -> G.Param (map_simple_identifier env x |> G.param_of_id)
  | `Opt_simple_id_simple_id_COLON_opt_param_modifs_poss_impl_unwr_type
      (v1, v2, v3, v4, v5) ->
      (* TODO As with `map_parameter`, we only support internal names right now. *)
      let _v1 =
        match v1 with
        | Some x -> map_simple_identifier env x |> todo env
        | None -> ()
      in
      let v2 = map_simple_identifier env v2 in
      let v3 = (* ":" *) token env v3 in
      let attrs =
        match v4 with
        | Some x -> map_parameter_modifiers env x
        | None -> []
      in
      let v5 = map_possibly_implicitly_unwrapped_type env v5 in
      let ty = { v5 with G.t_attrs = attrs @ v5.G.t_attrs } in
      G.Param (G.param_of_id ~ptype:(Some ty) ~pattrs:attrs v2)

and map_local_declaration (env : env) (x : CST.local_declaration) : G.stmt list
    =
  match x with
  | `Local_prop_decl (v1, v2) ->
      let modifiers =
        match v1 with
        | Some x -> map_locally_permitted_modifiers env x
        | None -> []
      in
      map_modifierless_property_declaration env modifiers v2
  | `Local_typeas_decl (v1, v2) ->
      let modifiers =
        match v1 with
        | Some x -> map_locally_permitted_modifiers env x
        | None -> []
      in
      [ map_modifierless_typealias_declaration env modifiers v2 ]
  | `Local_func_decl (v1, v2) ->
      let v1 =
        match v1 with
        | Some x -> map_locally_permitted_modifiers env x
        | None -> []
      in
      map_modifierless_function_declaration env v1 v2
  | `Local_class_decl (v1, v2) ->
      let modifiers =
        match v1 with
        | Some x -> map_locally_permitted_modifiers env x
        | None -> []
      in
      [ map_modifierless_class_declaration env modifiers v2 ]

and map_semi (env : env) (x : CST.semi) =
  match x with
  | `Impl_semi tok -> (`Implicit, (* implicit_semi *) token env tok)
  | `Expl_semi tok -> (`Explicit, (* explicit_semi *) token env tok)

and map_semi_opt (env : env) (semi : CST.semi option) : G.sc =
  match semi with
  | None -> G.sc
  | Some semi -> (
      let kind, tok = map_semi env semi in
      (* Swift can have implicit semicolons, see
       * https://github.com/alex-pinkus/tree-sitter-swift/issues/179
       *
       * These implicit semicolons are inserted at the beginning of the next
       * line, and mess up our range data. To fix this, we check if we have an
       * implicit semicolon, and if so, we just insert a fake semicolon, which
       * doesn't bork the range.
       *)
      match kind with
      | `Implicit -> G.sc
      | `Explicit -> tok)

and map_local_statement (env : env) (x : CST.local_statement)
    (semi : CST.semi option) : G.stmt list =
  let semi = map_semi_opt env semi in
  match x with
  | `Exp x ->
      let expr = map_expression env x in
      [ G.ExprStmt (expr, semi) |> G.s ]
  | `Local_decl x -> map_local_declaration env x
  | `Labe_stmt x -> [ map_labeled_statement env x ]
  | `Cont_tran_stmt x -> [ map_control_transfer_statement env x semi ]

and map_locally_permitted_modifiers (env : env)
    (xs : CST.locally_permitted_modifiers) =
  Common.map
    (fun x ->
      match x with
      | `Attr x -> map_attribute env x
      | `Loca_perm_modi x -> map_locally_permitted_modifier env x)
    xs

and construct_class_def :
      'body.
      env ->
      ?kind:G.class_kind ->
      G.tok ->
      'inheritance_specifiers ->
      G.entity ->
      CST.type_constraints option ->
      'body ->
      (env -> 'body -> G.field list G.bracket) ->
      G.stmt =
 fun env ?(kind = G.Class) class_token inheritance_specifiers ent
     type_constraints body map_body ->
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
    | Some x -> map_type_constraints env x
    | None -> []
  in
  let body = map_body env body in
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
  let entity = { ent with G.attrs = type_constraints @ ent.G.attrs } in
  G.DefStmt (entity, G.ClassDef definition_kind) |> G.s

and map_modifierless_class_declaration (env : env) (attrs : G.attribute list)
    (x : CST.modifierless_class_declaration) =
  match x with
  | `Choice_class_simple_id_opt_type_params_opt_COLON_inhe_specis_opt_type_consts_class_body
      (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (* TODO differentiate between class, struct, and actor? Maybe use
         * RecordClass attribute? *)
        match v1 with
        | `Class tok -> (* "class" *) token env tok
        | `Struct tok -> (* "struct" *) token env tok
        | `Actor tok -> (* "actor" *) token env tok
      in
      let v2 = map_simple_identifier env v2 in
      let tparams =
        match v3 with
        | Some x -> map_type_parameters env x
        | None -> []
      in
      let entity = G.basic_entity ~tparams ~attrs v2 in
      construct_class_def env v1 v4 entity v5 v6 map_class_body
  | `Exte_unan_type_opt_type_params_opt_COLON_inhe_specis_opt_type_consts_class_body
      (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "extension" *) token env v1 in
      let name =
        match v2 with
        | `User_type x -> name_of_user_type env x
        | _ -> todo env v2
      in
      let tparams =
        match v3 with
        | Some x -> map_type_parameters env x
        | None -> []
      in
      let entity = { G.name = EN name; attrs; tparams } in
      (* Extensions basically allow you to reopen an existing class. They don't
       * really fit in perfectly to the existing class kinds that we have in the
       * generic AST, but an ordinary class is probably the closest.
       *
       * https://docs.swift.org/swift-book/LanguageGuide/Extensions.html *)
      construct_class_def env v1 v4 entity v5 v6 map_class_body
  | `Opt_indi_enum_simple_id_opt_type_params_opt_COLON_inhe_specis_opt_type_consts_enum_class_body
      (v1, v2, v3, v4, v5, v6, v7) ->
      let v2 = (* "enum" *) token env v2 in
      let attrs =
        let indirect_kw =
          match v1 with
          | Some tok ->
              (* "indirect" *)
              [ G.unhandled_keywordattr (str env tok) ]
          | None -> []
        in
        indirect_kw @ [ G.KeywordAttr (G.EnumClass, v2) ] @ attrs
      in
      let v3 = map_simple_identifier env v3 in
      (* If there are any tparams, then this must be a raw-value style enum. *)
      let tparams =
        match v4 with
        | Some x -> map_type_parameters env x
        | None -> []
      in
      (* Basically, we can tell if we have a raw type if it's a certain kind of
         base type like integers, floating point values, or whatever.
         It's just kind of a pain to extract that information.
      *)
      let entity = G.basic_entity ~attrs ~tparams v3 in
      construct_class_def env v2 v5 entity v6 v7 (map_enum_class_body v3)

and map_modifierless_function_declaration (env : env) (attrs : G.attribute list)
    ((v1, v2) : CST.modifierless_function_declaration) =
  let v2 = map_function_body env v2 in
  let in_class = todo env () in
  let v1 =
    map_modifierless_function_declaration_no_body env ~in_class ~attrs v1
      (G.FBStmt v2)
  in
  [ v1 ]

and map_type_with_modifiers env ty attrs =
  { ty with G.t_attrs = ty.G.t_attrs @ attrs }

and map_modifierless_function_declaration_no_body (env : env) ~in_class
    ?(attrs = [])
    ((v1, v2, v3, v4, v5, v6, v7) :
      CST.modifierless_function_declaration_no_body) (body : G.function_body) =
  let is_quest, v1 =
    match v1 with
    | `Cons_func_decl x -> map_constructor_function_decl env x
    | `Non_cons_func_decl x -> (false, map_non_constructor_function_decl env x)
  in
  let v2 =
    match v2 with
    | Some x -> map_type_parameters env x
    | None -> []
  in
  let v3 = map_function_value_parameters env v3 in
  let rettype_attrs =
    let v4 =
      match v4 with
      | Some tok ->
          (* async_keyword_custom *) [ G.attr G.Async (token env tok) ]
      | None -> []
    in
    let v5 =
      match v5 with
      | Some x -> [ map_throws env x ]
      | None -> []
    in
    v4 @ v5
  in
  let type_constraints =
    match v7 with
    | Some x -> map_type_constraints env x
    | None -> []
  in
  let frettype =
    match v6 with
    | Some (v1, v2) ->
        let _ = (* arrow_operator_custom *) token env v1 in
        let ty = map_possibly_implicitly_unwrapped_type env v2 in
        let res =
          map_type_with_modifiers env ty (type_constraints @ rettype_attrs)
        in
        Some (if is_quest then G.TyQuestion (res, G.fake "?") |> G.t else res)
    | None -> None
  in

  let attrs = attrs in
  let entity = G.basic_entity ~tparams:v2 ~attrs v1 in
  let kind = if in_class then G.Method else G.Function in
  let definition_kind =
    G.FuncDef { fkind = (kind, snd v1); fparams = v3; frettype; fbody = body }
  in
  G.DefStmt (entity, definition_kind) |> G.s

and map_single_modifierless_property_declaration (env : env)
    (attrs : G.attribute list)
    ((pat, tannot, tconstraints, init) :
      CST.single_modifierless_property_declaration) : G.stmt =
  let pat = map_no_expr_pattern_already_bound env pat in
  let entity = entity_of_pattern ~attrs pat in
  let tconstraints =
    match tconstraints with
    | Some x -> map_type_constraints env x
    | None -> []
  in
  let tannot =
    match tannot with
    | Some x -> Some (map_type_annotation ~attrs:tconstraints env x)
    | None -> None
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
          | `Comp_prop x -> G.StmtExpr (map_computed_property env x) |> G.e
        in
        Some x
    | None -> None
  in
  G.DefStmt (entity, G.VarDef { vinit = init; vtype = tannot }) |> G.s

and map_modifierless_property_declaration (env : env) (attrs : G.attribute list)
    ((v1, v2, v3) : CST.modifierless_property_declaration) : G.stmt list =
  (* kind *)
  let attrs = attrs @ map_possibly_async_binding_pattern_kind_to_attr env v1 in
  let stmt1 = map_single_modifierless_property_declaration env attrs v2 in
  let stmts =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        map_single_modifierless_property_declaration env attrs v2)
      v3
  in
  stmt1 :: stmts

and map_modifierless_typealias_declaration (env : env)
    (attrs : G.attribute list)
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
    ( G.basic_entity ~tparams:v3 ~attrs v2,
      G.TypeDef { G.tbody = G.AliasType v5 } )
  |> G.s

and map_modifiers (env : env) (xs : CST.modifiers) =
  List.concat_map
    (fun x ->
      match x with
      | `Non_local_scope_modi x -> [ map_non_local_scope_modifier env x ]
      | `Rep1_choice_attr x -> map_locally_permitted_modifiers env x)
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
    G.pattern list G.bracket =
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
  (v1, v2 :: v3, v4)

and map_no_expr_pattern_already_bound (env : env)
    ((v1, v2) : CST.no_expr_pattern_already_bound) : G.pattern =
  let v1 =
    match v1 with
    | `Univ_allo_pat x -> map_universally_allowed_pattern env x
    | `Bound_id x ->
        let id = map_bound_identifier env x in
        let id_info = G.empty_id_info () in
        G.PatId (id, id_info)
  in
  let add_quest pat =
    match v2 with
    | Some tok ->
        (* "?" *)
        G.OtherPat (("?", token env tok), [ G.P pat ])
    | None -> v1
  in
  v1 |> add_quest

and map_binding_pattern_no_expr (env : env)
    ((v1, v2) : CST.binding_pattern_no_expr) =
  let pat =
    match v1 with
    | `Univ_allo_pat x -> map_universally_allowed_pattern env x
    | `Bind_pat x -> map_binding_pattern env x
    | `Bound_id x ->
        let info = map_bound_identifier env x in
        let id_info = G.empty_id_info () in
        G.PatId (info, id_info)
  in
  let add_pat_quest pat =
    match v2 with
    | Some tok ->
        (* "?" *)
        G.OtherPat (("?", token env tok), [ G.P pat ])
    | None -> pat
  in
  pat |> add_pat_quest

and map_universally_allowed_pattern (env : env)
    (x : CST.universally_allowed_pattern) : G.pattern =
  match x with
  | `Wild_pat tok -> (* "_" *) G.PatUnderscore (token env tok)
  | `Tuple_pat x -> G.PatTuple (map_tuple_pattern env x)
  | `Type_cast_pat x -> map_type_casting_pattern env x
  | `Case_pat (v1, v2, v3, v4, v5) ->
      let v3 = (* dot_custom *) token env v3 in
      let id = map_bound_identifier env v4 in
      let id_info = G.empty_id_info () in
      let pat_init = G.PatId (id, id_info) in
      let add_pat_args name pat =
        match Option.map (map_tuple_pattern env) v5 with
        | None -> pat
        | Some (_, pats, _) -> G.PatConstructor (name, pats)
      in
      let add_pat_type pat =
        match v2 with
        | Some x -> G.PatTyped (pat, map_user_type env x)
        | None -> pat
      in
      pat_init |> add_pat_args (G.Id (id, id_info)) |> add_pat_type

and map_parameter (env : env) ((v1, v2, v3, v4, v5, v6) : CST.parameter)
    ?(attrs = []) default =
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
  let attrs =
    (match v4 with
    | Some x -> map_parameter_modifiers env x
    | None -> [])
    @ attrs
  in
  let v5 = map_possibly_implicitly_unwrapped_type env v5 in
  let ptype = Some { v5 with G.t_attrs = attrs @ v5.G.t_attrs } in
  let param = G.param_of_id ~pdefault:default ~ptype v2 in
  match v6 with
  | Some tok ->
      let dots = (* three_dot_operator_custom *) token env tok in
      G.ParamRest (dots, param)
  | None -> G.Param param

and map_possibly_implicitly_unwrapped_type (env : env)
    ((v1, v2) : CST.possibly_implicitly_unwrapped_type) =
  let v1 = map_type_ env v1 in
  v1

and map_primary_expression (env : env) (x : CST.primary_expression) : G.expr =
  match x with
  | `Tuple_exp x -> map_tuple_expression env x
  | `Basic_lit x -> map_basic_literal env x
  | `Lambda_lit x -> map_lambda_literal env x
  | `Spec_lit x -> map_special_literal env x
  | `Play_lit (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | `HASH_38ce0d0 tok ->
            (* "#colorLiteral" *)
            G.OtherExpr (("ColorLiteral", token env tok), [])
        | `HASH_34ae46a tok ->
            (* "#fileLiteral" *) G.OtherExpr (("FileLiteral", token env tok), [])
        | `HASH_71f9c0e tok ->
            (* "#imageLiteral" *)
            G.OtherExpr (("ImageLiteral", token env tok), [])
      in
      let mk_arg ident exp = G.ArgKwd (ident, exp) in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_simple_identifier env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      let rest =
        Common.map
          (fun (v1, v2, v3, v4) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_simple_identifier env v2 in
            let v3 = (* ":" *) token env v3 in
            let v4 = map_expression env v4 in
            mk_arg v2 v4)
          v6
      in
      let v7 = (* ")" *) token env v7 in
      G.Call (v1 |> G.e, (v2, mk_arg v3 v5 :: rest, v7)) |> G.e
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
  | `Refe_op x -> map_referenceable_operator env x |> snd |> G.e
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
      G.Call (G.OtherExpr (("KeyPath", v1), []) |> G.e, (v2, [ G.Arg v3 ], v4))
      |> G.e
  | `Three_dot_op tok ->
      let tok = (* three_dot_operator_custom *) token env tok in
      if in_pattern env then G.Ellipsis tok |> G.e
      else
        let op = (G.Range, tok) in
        G.opcall op [ G.L (G.Null tok) |> G.e; G.L (G.Null tok) |> G.e ]

and map_self_expression (env : env) tok =
  G.IdSpecial (G.Self, (* "self" *) token env tok) |> G.e

and map_property_declaration (env : env) ((v1, v2) : CST.property_declaration) =
  (* These modifiers apply to each consecutive declaration here.
     So pass them down and distribute them.
  *)
  let modifiers =
    match v1 with
    | Some x -> map_modifiers env x
    | None -> []
  in
  map_modifierless_property_declaration env modifiers v2

and map_protocol_body (env : env) ((v1, v2, v3) : CST.protocol_body) :
    G.field list G.bracket =
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
  let modifiers =
    match v1 with
    | Some x -> map_modifiers env x
    | None -> []
  in
  let v2 = (* "protocol" *) token env v2 in
  let v3 = map_simple_identifier env v3 in
  let tparams =
    match v4 with
    | Some x -> Some (map_type_parameters env x)
    | None -> None
  in
  let entity = G.basic_entity ~attrs:modifiers ?tparams v3 in
  construct_class_def env ~kind:G.Interface v2 v5 entity v6 v7 map_protocol_body

and map_protocol_member_declaration (env : env)
    (x : CST.protocol_member_declaration) : G.field =
  match x with
  | `Body_func_decl_opt_func_body (v1, v2) ->
      let v2 =
        match v2 with
        | Some x -> G.FBStmt (map_function_body env x)
        | None -> G.FBNothing
      in
      let v1 = map_bodyless_function_declaration env ~in_class:true v1 v2 in
      G.F v1
  | `Deinit_decl x -> G.F (map_deinit_declaration env x)
  | `Prot_prop_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | Some x -> map_modifiers env x
        | None -> []
      in
      let kinds, pat = map_binding_kind_and_pattern env v2 in
      let pat = apply_pattern_kinds env pat kinds in
      let entity = entity_of_pattern ~attrs:v1 pat in
      let tconstraints =
        match v4 with
        | Some x -> map_type_constraints env x
        | None -> []
      in
      let v3 =
        match v3 with
        | Some x -> Some (map_type_annotation ~attrs:tconstraints env x)
        | None -> None
      in
      (* TODO do something with these *)
      let v5 = map_protocol_property_requirements env v5 in
      (* TODO desugar PatID? *)
      let stmt =
        G.DefStmt (entity, G.VarDef { vinit = None; vtype = v3 }) |> G.s
      in
      G.F stmt
  | `Typeas_decl x -> G.F (map_typealias_declaration env x)
  | `Asso_decl x -> G.F (map_associatedtype_declaration env x)
  | `Subs_decl x -> G.F (map_subscript_declaration env x)

and map_protocol_member_declarations (env : env)
    ((v1, v2, v3) : CST.protocol_member_declarations) : G.field list =
  let v1 = map_protocol_member_declaration env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = map_semi env v1 in
        let v2 = map_protocol_member_declaration env v2 in
        v2)
      v2
  in
  let v3 =
    match v3 with
    | Some tok ->
        let sc = map_semi env tok in
        ()
    | None -> ()
  in
  v1 :: v2

and map_raw_str_interpolation (env : env)
    ((v1, v2, v3) : CST.raw_str_interpolation) =
  let v1 = (* pattern \\#*\( *) token env v1 in
  let v2 = map_interpolation_contents env v2 in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_repeat_while_statement (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.repeat_while_statement) =
  let repeat_tok = (* "repeat" *) token env v1 in
  let stmt =
    let left = (* "{" *) token env v2 in
    let right = (* "}" *) token env v4 in
    let stmts =
      match v3 with
      | Some x -> map_statements env x
      | None -> []
    in
    G.Block (left, stmts, right) |> G.s
  in
  let v5 = (* "while" *) token env v5 in
  let expr = map_if_condition_sequence_item env v6 in
  (* TODO: multiple conds *)
  let v7 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_if_condition_sequence_item env v2 in
        todo env (v1, v2))
      v7
  in
  let cond = combine_conds expr v7 in
  G.DoWhile (repeat_tok, stmt, cond) |> G.s

and map_simple_user_type (env : env) ((v1, v2) : CST.simple_user_type) :
    G.ident * G.type_arguments option =
  let v1 = map_simple_identifier env v1 in
  match v2 with
  | Some x -> (v1, Some (map_type_arguments env x))
  | None -> (v1, None)

and map_statements (env : env) ((v1, v2, v3) : CST.statements) =
  let stmts = associate_statement_semis v1 v2 v3 in
  List.concat_map (fun (stmt, semi) -> map_local_statement env stmt semi) stmts

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
  let v2 = (* "subscript" *) token env v2 in
  let attrs =
    match v1 with
    | Some x -> map_modifiers env x
    | None -> []
  in
  let tparams =
    match v3 with
    | Some x -> map_type_parameters env x
    | None -> []
  in
  let fparams = map_function_value_parameters env v4 in
  let frettype =
    match v5 with
    | Some (v1, v2) ->
        let v1 = (* arrow_operator_custom *) token env v1 in
        let v2 = map_possibly_implicitly_unwrapped_type env v2 in
        Some v2
    | None -> None
  in
  let v6 =
    match v6 with
    | Some x -> map_type_constraints env x
    | None -> []
  in
  let ent =
    { G.name = G.OtherEntity (("Subscript", v2), []); attrs; tparams }
  in
  let v7 = map_computed_property env v7 in
  G.DefStmt
    ( ent,
      G.FuncDef
        { fkind = (G.Method, v2); fparams; frettype; fbody = G.FBStmt v7 } )
  |> G.s

and map_switch_entry (env : env) ((v1, v2, v3, v4, v5) : CST.switch_entry) =
  (* These are weird case-specific attributes... probably not important. *)
  let v1 =
    match v1 with
    | Some x -> map_modifiers env x |> todo env
    | None -> ()
  in
  let case =
    match v2 with
    | `Case_switch_pat_opt_where_kw_exp_rep_COMMA_switch_pat (v1, v2, v3, v4) ->
        let case_tok = (* "case" *) token env v1 in
        let pat_init = map_switch_pattern env v2 in
        let add_pat_where pat =
          match v3 with
          | Some x -> G.PatWhen (pat, map_where_clause env x)
          | None -> pat
        in
        let rest_pats =
          Common.map (fun (_v1, v2) -> map_switch_pattern env v2) v4
        in
        let first_pat = pat_init |> add_pat_where in
        (* Yeah, I know, but I want them in the same order, and we're using a
           binary tree instead of a list...
        *)
        let later_pats =
          List.fold_right
            (fun pat acc_opt ->
              match acc_opt with
              | None -> Some pat
              | Some acc_pat -> Some (G.PatDisj (pat, acc_pat)))
            rest_pats None
        in
        let final_pat =
          match later_pats with
          | None -> first_pat
          | Some pat -> G.PatDisj (first_pat, pat)
        in
        G.Case (case_tok, final_pat)
    | `Defa_kw tok -> (* default_keyword *) G.Default (token env tok)
  in
  let v3 = (* ":" *) token env v3 in
  let stmt = G.Block (G.fake_bracket (map_statements env v4)) in
  (* For now, don't deal with fallthrough. *)
  let v5 =
    match v5 with
    | Some tok -> (* "fallthrough" *) token env tok |> todo env
    | None -> ()
  in
  G.CasesAndBody ([ case ], stmt |> G.s)

and map_switch_pattern (env : env) (x : CST.switch_pattern) =
  map_binding_pattern_with_expr env x

and map_switch_statement (env : env)
    ((v1, v2, v3, v4, v5) : CST.switch_statement) =
  let switch_tok = (* "switch" *) token env v1 in
  let expr = map_expression env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 = Common.map (map_switch_entry env) v4 in
  let v5 = (* "}" *) token env v5 in
  G.Switch (switch_tok, Some (G.Cond expr), v4) |> G.s

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

and map_throw_statement (env : env) ((v1, v2) : CST.throw_statement)
    (semi : G.sc) =
  let v1 = (* "throw" *) token env v1 in
  let v2 = map_expression env v2 in
  G.Throw (v1, v2, semi) |> G.s

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
              v2)
            v2
        in
        Common.map (fun x -> G.F x) (v1 :: v2)
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  G.TyRecordAnon ((G.Class, v1), (v1, v2, v3)) |> G.t

and map_tuple_type_item (env : env) ((v1, v2, v3) : CST.tuple_type_item) =
  let ent =
    match v1 with
    | Some x -> G.basic_entity (map_tuple_type_item_identifier env x)
    | None ->
        {
          G.name =
            G.OtherEntity (("AnonTupleField", G.fake "AnonTupleField"), []);
          attrs = [];
          tparams = [];
        }
  in
  let v2 =
    match v2 with
    | Some x -> map_parameter_modifiers env x |> todo env
    | None -> []
  in
  let v3 = map_type_ env v3 in
  G.DefStmt (ent, G.FieldDefColon { vinit = None; vtype = Some v3 }) |> G.s

and map_type_ (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Opt_type_modifs_unan_type (v1, v2) ->
      let v1 =
        match v1 with
        | Some x -> map_type_modifiers env x
        | None -> []
      in
      let v2 = map_unannotated_type env v2 in
      { v2 with G.t_attrs = v1 @ v2.G.t_attrs }
  | `Semg_ellips tok -> (* "..." *) token env tok |> todo env

and map_type_annotation (env : env) ?(attrs = [])
    ((v1, v2) : CST.type_annotation) : G.type_ =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_possibly_implicitly_unwrapped_type env v2 in
  map_type_with_modifiers env v2 attrs

and map_type_arguments (env : env) ((v1, v2, v3, v4) : CST.type_arguments) :
    G.type_arguments =
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
      let v4 = map_possibly_implicitly_unwrapped_type env v4 in
      let base_type = G.ArgType (G.TyN (H2.name_of_ids v2) |> G.t) in
      let conformed_protocol = G.ArgType v4 in
      G.NamedAttr
        ( token env v3,
          H2.name_of_id (str env v3),
          G.fake_bracket [ base_type; conformed_protocol ] )
  | `Equa_cons (v1, v2, v3, v4) ->
      let v1 = Common.map (map_attribute env) v1 in
      let v2 = map_identifier env v2 in
      let first_type = G.ArgType (G.TyN (H2.name_of_ids v2) |> G.t) in
      let ((_, v3_tok) as v3_str) =
        match v3 with
        | `Equal_sign tok -> (* eq_custom *) str env tok
        | `Eq_eq tok -> (* eq_eq_custom *) str env tok
      in
      let v4 = map_type_ env v4 in
      G.NamedAttr
        ( v3_tok,
          H2.name_of_id v3_str,
          G.fake_bracket [ first_type; G.ArgType v4 ] )

and map_type_constraints (env : env) ((v1, v2, v3) : CST.type_constraints) =
  let v1 = (* where_keyword *) token env v1 in
  let v2 = map_type_constraint env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_type_constraint env v2 in
        v2)
      v3
  in
  v2 :: v3

and map_type_level_declaration (env : env) (x : CST.type_level_declaration) :
    G.stmt list =
  match x with
  | `Choice_import_decl x -> (
      match x with
      | `Import_decl x -> [ map_import_declaration env x ]
      | `Prop_decl x -> map_property_declaration env x
      | `Typeas_decl x -> [ map_typealias_declaration env x ]
      | `Func_decl x -> [ map_function_declaration env ~in_class:true x ]
      | `Class_decl x -> [ map_class_declaration env x ]
      | `Prot_decl x -> [ map_protocol_declaration env x ]
      | `Deinit_decl x -> [ map_deinit_declaration env x ]
      | `Subs_decl x -> [ map_subscript_declaration env x ]
      | `Op_decl x -> [ map_operator_declaration env x ]
      | `Prec_group_decl x -> [ map_precedence_group_declaration env x ]
      | `Asso_decl x -> [ map_associatedtype_declaration env x ])
  | `Semg_ellips tok (* "..." *) ->
      let tok = (* three_dot_operator_custom *) token env tok in
      [ G.ExprStmt (G.Ellipsis tok |> G.e, G.sc) |> G.s ]

and map_type_modifiers (env : env) (x : CST.type_modifiers) =
  map_type_parameter_modifiers env x

and map_type_parameter (env : env) (x : CST.type_parameter) =
  match x with
  | `Opt_type_param_modifs_simple_id_opt_COLON_type (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some x -> map_type_parameter_modifiers env x
        | None -> []
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
      G.tparam_of_id ~tp_attrs:v1 ~tp_bounds:v3 v2
  | `Semg_ellips tok -> (* "..." *) token env tok |> todo env

and map_type_parameter_modifiers (env : env) (xs : CST.type_parameter_modifiers)
    =
  Common.map (map_attribute env) xs

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters)
    =
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
  (* TODO handle type constraints? *)
  let v4 =
    match v4 with
    | Some x ->
        let _ = map_type_constraints env x in
        ()
    | None -> ()
  in
  let v5 = (* ">" *) token env v5 in
  v2 :: v3

and map_typealias_declaration (env : env) ((v1, v2) : CST.typealias_declaration)
    =
  let modifiers =
    match v1 with
    | Some x -> map_modifiers env x
    | None -> []
  in
  map_modifierless_typealias_declaration env modifiers v2

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
      List.fold_left (fun acc quest -> G.TyQuestion (acc, quest) |> G.t) v1 v2
  | `Meta (v1, v2, v3) ->
      let v1 = map_unannotated_type env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        match v3 with
        | `Type tok -> (* "Type" *) G.unhandled_keywordattr (str env tok)
        | `Prot tok -> (* "Protocol" *) G.unhandled_keywordattr (str env tok)
      in
      { v1 with G.t_attrs = v3 :: v1.G.t_attrs }
  | `Opaque_type (v1, v2) ->
      let v2 = map_unannotated_type env v2 in
      {
        v2 with
        G.t_attrs = G.unhandled_keywordattr (str env v1) :: v2.G.t_attrs;
      }
  | `Exis_type (v1, v2) ->
      let v1 = (* "any" *) token env v1 in
      let v2 = map_unannotated_type env v2 in
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
      let v1 = (* "#selector" *) str env v1 in
      let v2 = (* "(" *) token env v2 in
      let add_label expr =
        match v3 with
        | Some x ->
            (match x with
            | `Gett tok ->
                (* "getter:" *)
                G.OtherExpr (str env tok, [ G.E expr ])
            | `Sett tok ->
                (* "setter:" *)
                G.OtherExpr (str env tok, [ G.E expr ]))
            |> G.e
        | None -> expr
      in
      G.OtherExpr (v1, [ G.E (map_expression env v4 |> add_label) ]) |> G.e
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

and name_of_user_type (env : env) ((v1, v2) : CST.user_type) : G.name =
  let v1 = map_simple_user_type env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* dot_custom *) token env v1 in
        map_simple_user_type env v2)
      v2
  in
  H2.name_of_ids_with_opt_typeargs (v1 :: v2)

and map_user_type (env : env) (t : CST.user_type) : G.type_ =
  let name = name_of_user_type env t in
  G.TyN name |> G.t

and map_user_type_name (env : env) (t : CST.user_type) : G.name =
  name_of_user_type env t

and map_value_argument (env : env) ((v1, v2) : CST.value_argument) :
    G.argument list =
  let v1 =
    match v1 with
    | Some x -> Some (map_type_modifiers env x)
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
            G.OtherArg (id, [ G.I id ]))
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
  v2

and map_while_statement (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.while_statement) =
  let while_tok = (* "while" *) token env v1 in
  (* TODO: As with if: looks like we could desugar this to a bunch of And expressions, but
     * need to double-check semantics. For now just raise if we encounter this.
     * *)
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        map_if_condition_sequence_item env v2)
      v3
  in
  let cond = combine_conds (map_if_condition_sequence_item env v2) v3 in
  let stmt =
    let left = (* "{" *) token env v4 in
    let right = (* "}" *) token env v6 in
    let stmts =
      match v5 with
      | Some x -> map_statements env x
      | None -> []
    in
    G.Block (left, stmts, right) |> G.s
  in
  G.While (while_tok, G.Cond cond, stmt) |> G.s

let map_global_declaration (env : env) (x : CST.global_declaration) :
    G.stmt list =
  match x with
  | `Import_decl x -> [ map_import_declaration env x ]
  | `Prop_decl x -> map_property_declaration env x
  | `Typeas_decl x -> [ map_typealias_declaration env x ]
  | `Func_decl x -> [ map_function_declaration env ~in_class:false x ]
  | `Class_decl x -> [ map_class_declaration env x ]
  | `Prot_decl x -> [ map_protocol_declaration env x ]
  | `Op_decl x -> [ map_operator_declaration env x ]
  | `Prec_group_decl x -> [ map_precedence_group_declaration env x ]
  | `Asso_decl x -> [ map_associatedtype_declaration env x ]

let map_top_level_statement (env : env) (x : CST.top_level_statement)
    (semi : CST.semi option) =
  match x with
  | `Exp x ->
      let expr = map_expression env x in
      let semi = map_semi_opt env semi in
      [ G.ExprStmt (expr, semi) |> G.s ]
  | `Global_decl x -> map_global_declaration env x
  | `Labe_stmt x -> [ map_labeled_statement env x ]
  | `Throw_stmt x -> [ map_throw_statement env x (map_semi_opt env semi) ]

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
      map_source_file env cst)

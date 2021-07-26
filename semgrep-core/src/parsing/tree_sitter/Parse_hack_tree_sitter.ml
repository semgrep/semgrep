(*
   Map a Hack CST obtained from the tree-sitter parser directly to the generic AST.

   This file is derived from and kept in sync with the generated
   file 'semgrep-hack/lib/Boilerplate.ml'.
*)

open Common
module G = AST_generic
module CST = Tree_sitter_hack.CST
module H = Parse_tree_sitter_helpers
module PI = Parse_info

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
   Since we use the same code for handling semgrep patterns and target
   programs, this allows us to choose how to convert ambiguous constructs
   such as $FOO.
*)
type env = unit H.env

let token = H.token

let str = H.str

let _fk = Parse_info.fake_info ""

(*
   Temporarily avoid warnings about these things being unused.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

let () =
  ignore pr;
  (* from Common *)
  ignore str

(* Remove this function when everything is done *)
let todo (env : env) _ = failwith "not implemented"

(* Below are manual additions.
   There are more helpers in AST_generic.ml and AST_generic_helpers.ml.
 *)

let empty_stmt env t =
  let t = token env t (* ";" *) in
  G.Block (t, [], t) |> G.s

(* TODO: How can we avoid using this? *)
let unwrap_qualified_identifier qual = (match qual with 
  | G.Id (ident, id_info) -> [ident]
  (* Unwrap all existing identifiers *)
  | IdQualified ((ident, name_info), id_info) -> let name_info = name_info.name_qualifier in
  let idents = (match name_info with 
  | Some QDots (dotted_ident) -> dotted_ident @ [ident]
  | _ -> raise Impossible
  ) in idents
)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-hack/Boilerplate.ml *)

(* Q: The commented-out items below are in the Boilerplate.ml, but are not used.
   So why do they exist? This comment applies to a later commented code as well.
*)

(*
let heredoc_start (env : env) (tok : CST.heredoc_start) =
  (* heredoc_start *) token env tok

let identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok

let variable (env : env) (tok : CST.variable) =
  (* variable *) token env tok

let heredoc_end (env : env) (tok : CST.heredoc_end) =
  (* heredoc_end *) token env tok

let tok_lcurldollar_pat_0e8e4b6 (env : env) (tok : CST.tok_lcurldollar_pat_0e8e4b6) =
  (* tok_lcurldollar_pat_0e8e4b6 *) token env tok
*)

let use_type (env : env) (x : CST.use_type) =
  (match x with
  | `Name tok -> (* "namespace" *) token env tok
  | `Func tok -> (* "function" *) token env tok
  | `Type tok -> (* "type" *) token env tok
  | `Const tok -> (* "const" *) token env tok
  )

let visibility_modifier (env : env) (x : CST.visibility_modifier) =
  (match x with
  | `Public tok -> (* "public" *) G.KeywordAttr(Public, (token env tok))
  | `Prot tok -> (* "protected" *) G.KeywordAttr(Protected, (token env tok))
  | `Priv tok -> (* "private" *) G.KeywordAttr(Private, (token env tok))
  )

(*
let xhp_category_identifier (env : env) (tok : CST.xhp_category_identifier) =
  (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok

let xhp_comment (env : env) (tok : CST.xhp_comment) =
  (* xhp_comment *) token env tok

let float_ (env : env) (tok : CST.float_) =
  (* float *) token env tok

let pat_466b599 (env : env) (tok : CST.pat_466b599) =
  (* pattern function\s*\( *) token env tok

let xhp_identifier (env : env) (tok : CST.xhp_identifier) =
  (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok

let pat_b6fe07e (env : env) (tok : CST.pat_b6fe07e) =
  (* pattern <\?[hH][hH] *) token env tok

let heredoc_body (env : env) (tok : CST.heredoc_body) =
  (* heredoc_body *) token env tok

let integer (env : env) (tok : CST.integer) =
  (* integer *) token env tok
*)

(* Note: This method is considered unused because instances were replaced
   were replaced with todos. TODO: Re-enable scope_identifier.
*)
let _scope_identifier (env : env) (x : CST.scope_identifier) =
  (match x with
  | `Self tok -> (* "self" *) token env tok
  | `Parent tok -> (* "parent" *) token env tok
  | `Static tok -> (* "static" *) token env tok
  )

(*
let xhp_string (env : env) (tok : CST.xhp_string) =
  (* xhp_string *) token env tok

let xhp_class_identifier (env : env) (tok : CST.xhp_class_identifier) =
  (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
*)

let null (env : env) (x : CST.null) =
  (* Q: Sometimes we need str env tok and sometimes we need token env tok.
     I couldn't figure out how to handle that within this method, so we just pass
     up the token.Safe/effective to pass up all token processing?
  *)
  (match x with
  | `Null_37a6259 tok -> (* "null" *) tok
  | `Null_bbb93ef tok -> (* "Null" *) tok
  | `NULL tok -> (* "NULL" *) tok
  )

let collection_type (env : env) (x : CST.collection_type) =
  (match x with
  (* Note: Changing `token` to `str` may backfire later *)
  | `Array tok -> (* "array" *) str env tok
  | `Varray tok -> (* "varray" *) str env tok
  | `Darray tok -> (* "darray" *) str env tok
  | `Vec tok -> (* "vec" *) str env tok
  | `Dict tok -> (* "dict" *) str env tok
  | `Keyset tok -> (* "keyset" *) str env tok
  )

let type_modifier (env : env) (x : CST.type_modifier) =
  (match x with
  | `AT tok -> (* "@" *) token env tok
  | `QMARK tok -> (* "?" *) token env tok
  | `TILDE tok -> (* "~" *) token env tok
  )

let false_ (env : env) (x : CST.false_) =
  (match x with
  | `False_68934a3 tok -> (* "false" *) token env tok
  | `False_f8320b2 tok -> (* "False" *) token env tok
  | `FALSE tok -> (* "FALSE" *) token env tok
  )

(*
let string_ (env : env) (tok : CST.string_) =
  (* string *) token env tok
 *)

let true_ (env : env) (x : CST.true_) =
  (match x with
  | `True_b326b50 tok -> (* "true" *) token env tok
  | `True_f827cf4 tok -> (* "True" *) token env tok
  | `TRUE tok -> (* "TRUE" *) token env tok
  )

let anon_choice_QMARKDASHGT_ce9cc19 (env : env) (x : CST.anon_choice_QMARKDASHGT_ce9cc19) =
  (match x with
  | `QMARKDASHGT tok -> (* "?->" *) token env tok
  | `DASHGT tok -> (* "->" *) token env tok
  )

let trait_alias_clause (env : env) ((v1, v2, v3) : CST.trait_alias_clause) =
  let v1 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v1
  in
  let v2 = (* "as" *) token env v2 in
  let v3 =
    (match v3 with
    | `Visi_modi_opt_id (v1, v2) ->
        let v1 = visibility_modifier env v1 in
        let v2 =
          (match v2 with
          | Some tok ->
              (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `Opt_visi_modi_id (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> visibility_modifier env x
          | None -> todo env ())
        in
        let v2 =
          (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v2
        in
        todo env (v1, v2)
    )
  in
  todo env (v1, v2, v3)

let xhp_category_declaration (env : env) ((v1, v2, v3, v4) : CST.xhp_category_declaration) =
  let v1 = (* "category" *) token env v1 in
  let v2 =
    (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env v2
  in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env v2
      in
      todo env (v1, v2)
    ) v3
  in
  let v4 = (* ";" *) token env v4 in
  todo env (v1, v2, v3, v4)

let qualified_identifier (env : env) (x : CST.qualified_identifier) : G.name =
  (match x with
  | `Opt_id_rep1_back_id (v1, v2) ->
      let v1 =
        (match v1 with
        | Some tok ->
            (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
            Some (str env tok)
        | None -> None)
      in
      let v2 =
        List.map (fun (v1, v2) ->
          let v1 = (* "\\" *) token env v1 in
          let v2 =
            (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env v2
          in
          v2
        ) v2
      in
      (* Q: Is it fine to ignore if the name is fully vs partially qualified? *)
      let ids = List.rev (match v1 with
      | Some v1 -> v1 :: v2
      | None -> v2) in
      let ident = List.hd (ids) in (* Q: Bad OCaml? But we know lists can't be empty... *)
      let qual = G.QDots(List.rev(List.tl (ids))) in
      G.IdQualified(
        (ident, {name_qualifier = Some qual; name_typeargs = None;}),
        G.empty_id_info()
      )
  | `Id tok ->
      (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
      let ident = str env tok in
      Id (ident, G.empty_id_info())
  )

let xhp_identifier_ (env : env) (x : CST.xhp_identifier_) =
  (match x with
  | `Xhp_id tok ->
      (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) str env tok
  | `Xhp_class_id tok ->
      (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) str env tok
  )

let rec xhp_attribute_expression (env : env) (x : CST.xhp_attribute_expression) =
  (match x with
  | `Xhp_id tok ->
      (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
  | `Xhp_class_id tok ->
      (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
  | `Xhp_cate_id tok ->
      (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
  | `Xhp_bin_exp (v1, v2, v3) ->
      let v1 = xhp_attribute_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = xhp_attribute_expression env v3 in
      todo env (v1, v2, v3)
  | `Xhp_post_un_exp (v1, v2) ->
      let v1 = xhp_attribute_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> (* "+" *) token env tok
        | `STAR tok -> (* "*" *) token env tok
        | `QMARK tok -> (* "?" *) token env tok
        )
      in
      todo env (v1, v2)
  | `Xhp_paren_exp (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = xhp_attribute_expression env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = xhp_attribute_expression env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  )

let primitive_type (env : env) (x : CST.primitive_type) =
  (match x with
  | `Bool tok -> (* "bool" *) (str env tok)
  | `Float tok -> (* "float" *) (str env tok)
  | `Int tok -> (* "int" *) (str env tok)
  | `Str tok -> (* "string" *) (str env tok)
  | `Arra tok -> (* "arraykey" *) (str env tok)
  | `Void tok -> (* "void" *) (str env tok)
  | `Nonn tok -> (* "nonnull" *) (str env tok)
  (* Q: Why can't we access `x` directly before pass... Getting type error? *)
  | `Null x -> (str env (null env x))
  | `Mixed tok -> (* "mixed" *) (str env tok)
  | `Dyna tok -> (* "dynamic" *) (str env tok)
  | `Nore tok -> (* "noreturn" *) (str env tok)
  )

let member_modifier (env : env) (x : CST.member_modifier) =
  (match x with
  | `Visi_modi x -> visibility_modifier env x
  | `Static_modi tok -> (* "static" *) G.KeywordAttr(Static, (token env tok))
  | `Abst_modi tok -> (* "abstract" *) G.KeywordAttr(Abstract, (token env tok))
  | `Final_modi tok -> (* "final" *) G.KeywordAttr(Final, (token env tok))
  )

let class_modifier (env : env) (x : CST.class_modifier) =
  (match x with
  | `Abst_modi tok -> (* "abstract" *) G.KeywordAttr(Abstract, (token env tok))
  | `Final_modi tok -> (* "final" *) G.KeywordAttr(Final, (token env tok))
  )

let anon_choice_str_d42aa42 (env : env) (x : CST.anon_choice_str_d42aa42) =
  (match x with
  | `Str tok -> (* string *) token env tok
  | `Int tok -> (* integer *) token env tok
  )

let literal (env : env) (x : CST.literal) : G.literal =
  (match x with
  | `Str tok -> (* string *) G.String (str env tok)
  | `Int tok -> (* integer *) 
      let s, tok = str env tok in
      G.Int (int_of_string_opt s, tok)
  | `Float tok -> (* float *)
      let s, tok = str env tok in
      G.Float (float_of_string_opt s, tok) 
  | `True x -> G.Bool (true, true_ env x)
  | `False x -> G.Bool (false, false_ env x)
  | `Null x -> G.Null (token env (null env x))
  )

let namespace_identifier (env : env) (x : CST.namespace_identifier) =
  (match x with
  | `Qual_id_opt_back (v1, v2) ->
      let v1 = qualified_identifier env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* "\\" *) Some(token env tok)
        | None -> None)
      in
      Some(v1)
  | `Back tok -> (* "\\" *) None (* token env tok *)
  )

let rec type_constant_ (env : env) ((v1, v2, v3) : CST.type_constant_) =
  let v1 =
    (match v1 with
    | `Qual_id x -> qualified_identifier env x
    | `Type_cst_ x -> type_constant_ env x
    )
  in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
  in
  todo env (v1, v2, v3)

let trait_select_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.trait_select_clause) =
  let v1 = qualified_identifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
  in
  let v4 = (* "insteadof" *) token env v4 in
  let v5 = qualified_identifier env v5 in
  let v6 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = qualified_identifier env v2 in
      todo env (v1, v2)
    ) v6
  in
  todo env (v1, v2, v3, v4, v5, v6)

let xhp_close (env : env) ((v1, v2, v3) : CST.xhp_close) =
  let v1 = (* "</" *) token env v1 in
  let v2 = xhp_identifier_ env v2 in
  let v3 = (* ">" *) token env v3 in
  todo env (v1, v2, v3)

let xhp_children_declaration (env : env) ((v1, v2, v3, v4) : CST.xhp_children_declaration) =
  let v1 = (* "children" *) token env v1 in
  let v2 = xhp_attribute_expression env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = xhp_attribute_expression env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = (* ";" *) token env v4 in
  todo env (v1, v2, v3, v4)

let keyword (env : env) (x : CST.keyword) =
  (match x with
  | `Type tok -> (* "type" *) str env tok
  | `Newt tok -> (* "newtype" *) str env tok
  | `Shape tok -> (* "shape" *) str env tok
  | `Tupe tok -> (* "tupe" *) str env tok
  | `Clone tok -> (* "clone" *) str env tok
  | `New tok -> (* "new" *) str env tok
  | `Print tok -> (* "print" *) str env tok
  | `Choice_bool x -> primitive_type env x
  | `Choice_array x -> collection_type env x
  )

let xhp_enum_type (env : env) ((v1, v2, v3, v4, v5, v6) : CST.xhp_enum_type) =
  let v1 = (* "enum" *) token env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 = anon_choice_str_d42aa42 env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = anon_choice_str_d42aa42 env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 =
    (match v5 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v6 = (* "}" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

let scoped_identifier (env : env) ((v1, v2, v3) : CST.scoped_identifier) =
  let v1 =
    (* Q: This is ugly, but is now a helper *)
    (match v1 with
    | `Qual_id x -> let qual = qualified_identifier env x in unwrap_qualified_identifier qual
    | `Var tok -> (* variable *) todo env tok (* token env tok *)
    | `Scope_id x -> todo env x (* scope_identifier env x *) (* TODO: Will need to break open scopes? *)
    | `Choice_xhp_id x -> todo env x (* xhp_identifier_ env x *)
    | `Pipe_var tok -> (* "$$" *) todo env tok (* token env tok *)
    )
  in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (match v3 with
    | `Id tok ->
        (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env tok
    | `Var tok -> (* variable *) str env tok
    )
  in
  let qual = G.QDots(v1) in
  G.IdQualified(
    (v3, {name_qualifier = Some qual; name_typeargs = None;}),
    G.empty_id_info()
  )

let anonymous_function_use_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.anonymous_function_use_clause) =
  let v1 = (* "use" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* variable *) token env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* variable *) token env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 =
    (match v5 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v6 = (* ")" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

let use_clause (env : env) ((v1, v2, v3) : CST.use_clause) =
  (* Q: How to represent `use` type? *)
  let v1 =
    (match v1 with
    | Some x -> Some (use_type env x)
    | None -> None)
  in
  let v2 = namespace_identifier env v2 in
  let v3 : G.alias option =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = (* "as" *) token env v1 in
        let v2 =
          (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env v2
        in
        Some (v2, G.empty_id_info())
    | None -> None)
  in
  (match v2 with
  | Some x -> Some(unwrap_qualified_identifier x), v3
  | None -> None, v3
  )

let anon_choice_id_0f53960 (env : env) (x : CST.anon_choice_id_0f53960) =
  (match x with
  | `Id tok ->
      (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env tok
  | `Choice_type x -> keyword env x
  )

(* inline_compound_statement *)
(* TODO: change how treesitter grammar does this? We think SEMI is empty stmt? *)
let rec anon_choice_comp_stmt_c6c6bb4 (env : env) (x : CST.anon_choice_comp_stmt_c6c6bb4) =
  (match x with
  | `Comp_stmt x -> compound_statement env x
  | `SEMI tok -> (* ";" *) empty_stmt env tok
  )

and anon_choice_exp_1701d0a (env : env) (x : CST.anon_choice_exp_1701d0a) =
  (match x with
  | `Exp x -> expression env x
  | `Elem_init (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = expression env v3 in
      G.Assign(v1, v2, v3)
  )

and anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb (env : env) ((v1, v2, v3) : CST.anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb) =
  let v1 = anon_choice_exp_1701d0a env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = anon_choice_exp_1701d0a env v2 in
      v2
    ) v2
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) Some(token env tok)
    | None -> None)
  in
  v1 :: v2

and anon_choice_field_spec_0e0e023 (env : env) (x : CST.anon_choice_field_spec_0e0e023) =
  (match x with
  | `Field_spec (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "?" *) [G.KeywordAttr(Optional, token env tok)]
        | None -> [])
      in
      let v2 = expression env v2 in
      let v3 = (* "=>" *) token env v3 in
      let v4 = type_ env v4 in
      let ent : G.entity = {
        name = G.EDynamic v2; 
        attrs = v1;
        tparams = [];
      } in
      let def : G.variable_definition = {
        vinit = None; (* Note: This could never exist so I feel like I'm using the wrong stmt here *)
        vtype = Some v4;
      } in
      (* Q: What is the write def type here? If we should even be using definitions
         I'm a little lost here as to how to represent shapes... *)
      G.DefStmt(ent, FieldDefColon def)
  | `DOTDOTDOT tok -> (* "..." *) G.ExprStmt(IdSpecial(G.Spread, token env tok), G.sc)
      (* TODO: This could be considered an abuse. Compare to native Spread associated with Fields *)
  )

and anon_exp_rep_COMMA_exp_0bb260c (env : env) ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) =
  let v1 = expression env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = expression env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and argument (env : env) ((v1, v2) : CST.argument) : G.argument =
  (* TODO: Add modifier support *)
  let v1 =
    (match v1 with
    | Some x ->
        (match x with
        | `Inout_modi tok -> (* "inout" *) Some (token env tok)
        | `Vari_modi tok -> (* "..." *) Some (token env tok)
        )
    | None -> None)
  in
  let v2 = expression env v2 in
  G.Arg v2

and arguments (env : env) ((v1, v2, v3) : CST.arguments) : G.arguments G.bracket =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = argument env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = argument env v2 in
            v2
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> (* "," *) Some (token env tok)
          | None -> None)
        in
        v1 :: v2
    | None -> [])
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and as_expression (env : env) ((v1, v2, v3) : CST.as_expression) =
  let v1 = expression env v1 in
  let v2 =
    (match v2 with
    | `As tok -> (* as *) token env tok
    | `QMARKas tok -> (* "?as" *) token env tok
    )
  in
  let v3 = type_ env v3 in
  (* Q: Is this a cast? What to do with v2? *)
  G.Cast(v3, v1)

and attribute_modifier (env : env) ((v1, v2, v3, v4, v5, v6) : CST.attribute_modifier) : G.attribute =
  (* Q: Attributes are actually just constructors!
     Should we treat them as such? We can have an attribute (singular)
     be:
     - entities with NamedAttr -> name_or_dynamic and EDynamic of expr
     - OtherAttribute with OA_Expr and the Constructor Expr
     - Named Attr with args being args *)
  let v1 = (* "<<" *) token env v1 in
  let v2 = qualified_identifier env v2 in
  let v3 =
    (match v3 with
    | Some x -> arguments env x
    | None -> G.fake_bracket [])
  in
  (* TODO: Forced to use Call instead of Constructor here because Constructor doesn't use G.name.
     Could also try using unwrap_qualified_identifier, but then will also need to convert args back to
     expressions.
  *)
  let constr_call = G.Call((G.N v2), v3) in
  let first_attr_mod = G.E constr_call in
  let v4 =
    List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 = qualified_identifier env v2 in
      let v3 =
        (match v3 with
        | Some x -> arguments env x
        | None -> G.fake_bracket [])
      in
      G.E (G.Call((G.N v2), v3))
    ) v4
  in
  let v5 =
    (match v5 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None)
  in
  let v6 = (* ">>" *) token env v6 in
  G.OtherAttribute(G.OA_Expr, first_attr_mod :: v4)

and binary_expression (env : env) (x : CST.binary_expression) : G.expr =
  (match x with
  | `Exp_BARGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "|>" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Pipe, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "??" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Nullish, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Or, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.And, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitOr, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_HAT_exp (v1, v2, v3) ->
      (* Q: Comment in generic file says PHP has a xor shortcut operator as G.Xor? *)
      let v1 = expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitXor, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitAnd, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Eq, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.NotEq, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "===" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.PhysEq, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.NotPhysEq, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Lt, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Gt, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.LtE, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.GtE, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_LTEQGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "<=>" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Cmp, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.LSL, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.LSR, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Plus, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Minus, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_DOT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Concat, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Mult, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Div, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Mod, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Pow, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  | `Exp_QMARKCOLON_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "?:" *) token env v2 in
      let v3 = expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Elvis, v2), G.fake_bracket [ G.Arg v1; G.Arg v3 ])
  
  (* These are all assignment.
     TODO: Consider splitting out in grammar.
     Q: Does this overlap with VarDef? *)
  | `Exp_EQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = expression env v3 in
      Assign (v1, v2, v3)
  | `Exp_QMARKQMARKEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "??=" *) token env v2, G.Nullish in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  | `Exp_DOTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* ".=" *) token env v2, G.Concat in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  | `Exp_BAREQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "|=" *) token env v2, G.BitOr in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  | `Exp_HATEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "^=" *) token env v2, G.BitXor in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  | `Exp_AMPEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "&=" *) token env v2, G.BitAnd in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  | `Exp_LTLTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "<<=" *) token env v2, G.LSL in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  | `Exp_GTGTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* ">>=" *) token env v2, G.LSR in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  | `Exp_PLUSEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "+=" *) token env v2, G.Plus in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  | `Exp_DASHEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "-=" *) token env v2, G.Minus in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  | `Exp_STAREQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "*=" *) token env v2, G.Mult in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  | `Exp_SLASHEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "/=" *) token env v2, G.Div in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  | `Exp_PERCEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "%=" *) token env v2, G.Mod in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  | `Exp_STARSTAREQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "**=" *) token env v2, G.Pow in
      let v3 = expression env v3 in
      AssignOp(v1, (op, v2), v3)
  )

and braced_expression (env : env) ((v1, v2, v3) : CST.braced_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 = expression env v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and call_expression (env : env) ((v1, v2, v3) : CST.call_expression) =
  let v1 =
    (match v1 with
    | `Exp x -> expression env x
    | `Choice_array x -> G.N(G.Id ((collection_type env x), G.empty_id_info())) 
    )
  in
  let v2 =
    (match v2 with
    | Some x -> type_arguments env x
    | None -> None)
  in
  let v3 = arguments env v3 in
  G.Call(v1, v3)

and catch_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = type_ env v3 in
  let v4 = (* variable *) token env v4 in
  let v5 = (* ")" *) token env v5 in
  let v6 = compound_statement env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and class_const_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_const_declaration) =
  let v1 = List.map (member_modifier env) v1 in
  let v2 = (* "const" *) [G.KeywordAttr(Const, (token env v2))] in
  let attrs = v1 @ v2 in
  let v3 =
    (match v3 with
    | Some x -> Some(type_ env x)
    | None -> None)
  in
  (* TODO: I dislike this pattern of reaching into objects and editing them.
      How do I avoid this? By passing down :yikes: *)
  let v6 = (* ";" *) token env v6 in
  let v4 = class_const_declarator env v4 attrs v3 in
  let v5 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = class_const_declarator env v2 attrs v3 in
      v2
    ) v5
  in
  let defs = v4 :: v5 in
  (* Q: Represent statements series without blocks? Use function [[AST_generic.basic_field]]? *)
  (* Could get around it here by changing parent, but not always applicable/sensible *)
  G.Block(G.fake_bracket defs)

and class_const_declarator (env : env) ((v1, v2) : CST.class_const_declarator) attrs vtype =
  let v1 = anon_choice_id_0f53960 env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = expression env v2 in
        Some v2
    | None -> None)
  in
  let ent = G.basic_entity v1 attrs in
  let def : G.variable_definition = {
    vinit = v2;
    vtype = vtype;
  }
  in
  G.DefStmt(ent, G.VarDef def) |> G.s
  (* TODO: Handle const declaration without assignment. Is this VarDef? *)
  (* Q: VarDef vs Assign! *)

and compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) : G.stmt =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (statement env) v2 in
  let v3 = (* "}" *) token env v3 in
  G.Block (v1, v2, v3) |> G.s

and const_declarator (env : env) ((v1, v2, v3) : CST.const_declarator) =
  let v1 = anon_choice_id_0f53960 env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = expression env v3 in
  todo env (v1, v2, v3)

and declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Func_decl (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> [attribute_modifier env x]
        | None -> [])
      in
      let v2, identifier = function_declaration_header env v2 in
      let v3 = anon_choice_comp_stmt_c6c6bb4 env v3 in      
      let def = {v2 with fbody = v3} in
      let ent = G.basic_entity identifier v1 in
      G.DefStmt (ent, G.FuncDef def)
  | `Class_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) ->
      let v1 =
        (match v1 with
        | Some x -> [attribute_modifier env x]
        | None -> [])
      in 
      let v2 =
        (match v2 with
        | Some x -> [class_modifier env x]
        | None -> [])
      in
      let v3 =
        (match v3 with
        | Some x -> [class_modifier env x]
        | None -> [])
      in
      let v4 =
        (match v4 with
        | Some tok -> (* "xhp" *)
          (* Q: Does it make sense for the token and name to both be the xhp identifier?
             Is NamedAttr even correct? *)
          [G.NamedAttr(token env tok, Id (str env tok, G.empty_id_info()), G.fake_bracket [])]
        | None -> [])
      in
      let v5 = (* "class" *) token env v5 in
      let v6 =
        (match v6 with
        | `Id tok ->
            (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env tok
        | `Choice_xhp_id x -> xhp_identifier_ env x
        )
      in
      let v7 =
        (match v7 with
        | Some x -> Some(type_parameters env x)
        | None -> None)
      in
      let v8 =
        (match v8 with
        | Some x -> extends_clause env x
        | None -> [])
      in
      let v9 =
        (match v9 with
        | Some x -> implements_clause env x
        | None -> [])
      in
      let v10 =
        (* Q: What does this even do? *)
        (match v10 with
        | Some x -> where_clause env x
        | None -> None)
      in
      let v11 = member_declarations env v11 in
      let def : G.class_definition = {
        ckind = (G.Class, v5);
        cextends = v8;
        cimplements = v9;
        cmixins = [];
        cparams = [];
        cbody = v11;
      } in
      let attrs = v1 @ v2 @ v3 @ v4
      in
      G.DefStmt (G.basic_entity v6 attrs, G.ClassDef def)
  | `Inte_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some x -> attribute_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "interface" *) token env v2 in
      let v3 =
        (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
      in
      let v4 =
        (match v4 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> extends_clause env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> where_clause env x
        | None -> todo env ())
      in
      let v7 = member_declarations env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Trait_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some x -> attribute_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "trait" *) token env v2 in
      let v3 =
        (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
      in
      let v4 =
        (match v4 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> implements_clause env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> where_clause env x
        | None -> todo env ())
      in
      let v7 = member_declarations env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Alias_decl (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        (match v1 with
        | Some x -> [attribute_modifier env x]
        | None -> [])
      in
      let v2 =
        (match v2 with
        | `Type tok -> (* "type" *) token env tok
        | `Newt tok -> (* "newtype" *) token env tok
        )
      in
      let v4 =
        (match v4 with
        | Some x -> type_parameters env x
        | None -> [])
      in
      (* Q: Type params vs type attributes in generic? Which to use here?
         Put within Name or pass to Apply?*)
      let v3 =
        (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env v3
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) ->
            let v1 = (* "as" *) token env v1 in
            let v2 = type_ env v2 in
            todo env (v1, v2)
        | None -> None)
      in
      (* Q: AliasType vs NewType? *)
      let v6 = (* "=" *) token env v6 in
      let v7 = type_ env v7 in
      let v8 = (* ";" *) token env v8 in
      G.DefStmt(G.basic_entity v3 (v1), G.TypeDef { tbody = AliasType v7 })
  | `Enum_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 =
        (match v1 with
        | Some x -> attribute_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "enum" *) token env v2 in
      let v3 =
        (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
      in
      let v4 = (* ":" *) token env v4 in
      let v5 = type_ env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = (* "as" *) token env v1 in
            let v2 = type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v7 = (* "{" *) token env v7 in
      let v8 = List.map (enumerator env) v8 in
      let v9 = (* "}" *) token env v9 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Name_decl (v1, v2) ->
      let v1 = (* "namespace" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Qual_id_SEMI (v1, v2) ->
                let v1 = qualified_identifier env v1 in
                let v2 = (* ";" *) token env v2 in
                Some (unwrap_qualified_identifier v1)
            | `Opt_qual_id_comp_stmt (v1, v2) ->
                let v1 =
                  (match v1 with
                  | Some x -> Some (qualified_identifier env x |> unwrap_qualified_identifier)
                  | None -> None)
                in
                (* TODO: Handle namespace with block inside *)
                let v2 = compound_statement env v2 in
                v1
            )
        | None -> None)
      in
      (match v2 with
      | Some v2 -> G.DirectiveStmt(G.Package (v1, v2))
      (* TODO: I think this is wrong and Package end should instead be used to handle namespaces with block inside? But how? *)
      | None -> G.DirectiveStmt(G.PackageEnd (v1))
      )
  | `Const_decl (v1, v2, v3, v4, v5) ->
      let v1 = (* "const" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> type_ env x
        | None -> todo env ())
      in
      let v3 = const_declarator env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = const_declarator env v2 in
          todo env (v1, v2)
        ) v4
      in
      let v5 = (* ";" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  )

and embedded_brace_expression (env : env) ((v1, v2) : CST.embedded_brace_expression) =
  let v1 = embedded_brace_expression_ env v1 in
  let v2 = (* "}" *) token env v2 in
  todo env (v1, v2)

and embedded_brace_expression_ (env : env) (x : CST.embedded_brace_expression_) =
  (match x with
  | `Tok_lcur_pat_0e8e4b6 tok ->
      (* tok_lcurldollar_pat_0e8e4b6 *) token env tok
  | `Embe_brace_call_exp (v1, v2) ->
      let v1 = embedded_brace_expression_ env v1 in
      let v2 = arguments env v2 in
      todo env (v1, v2)
  | `Embe_brace_subs_exp (v1, v2, v3, v4) ->
      let v1 = embedded_brace_expression_ env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> expression env x
        | None -> todo env ())
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Embe_brace_sele_exp (v1, v2, v3) ->
      let v1 = embedded_brace_expression_ env v1 in
      let v2 = anon_choice_QMARKDASHGT_ce9cc19 env v2 in
      let v3 = variablish env v3 in
      todo env (v1, v2, v3)
  )

and enumerator (env : env) ((v1, v2, v3, v4) : CST.enumerator) =
  let v1 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v1
  in
  let v2 = (* "=" *) token env v2 in
  let v3 = expression env v3 in
  let v4 = (* ";" *) token env v4 in
  todo env (v1, v2, v3, v4)

and expression (env : env) (x : CST.expression) : G.expr =
  (match x with
  | `Here (v1, v2, v3, v4) ->
      let v1 = (* "<<<" *) token env v1 in
      let v2 = (* heredoc_start *) token env v2 in
      let v3 =
        List.map (fun x ->
          (match x with
          | `Here_body tok -> (* heredoc_body *) token env tok
          | `Var tok -> (* variable *) token env tok
          | `Embe_brace_exp x -> embedded_brace_expression env x
          )
        ) v3
      in
      let v4 = (* heredoc_end *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Array (v1, v2, v3, v4, v5) ->
      let v1 = collection_type env v1 in
      let v2 =
        (match v2 with
        | Some x -> type_arguments env x
        | None -> None)
      in
      let v3 = (* "[" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x ->
            anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb env x
        | None -> [])
      in
      let v5 = (* "]" *) token env v5 in
      (* TODO: Do not generalize to Dict *)
      G.Container(Dict, (v3, v4, v5))
  | `Tuple (v1, v2, v3, v4) ->
      let v1 = (* "tuple" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) ->
            let v1 = expression env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = expression env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> (* "," *) token env tok
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | None -> todo env ())
      in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Shape (v1, v2, v3, v4) ->
      let v1 = (* "shape" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) ->
            let v1 = field_initializer env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = field_initializer env v2 in
                v2
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> (* "," *) Some(token env tok)
              | None -> None)
            in
            v1 :: v2
        | None -> [])
      in
      let v4 = (* ")" *) token env v4 in
      G.Container(Dict, (v2, v3, v4))
  | `Coll (v1, v2, v3, v4) ->
      let v1 = qualified_identifier env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x ->
            anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb env x
        | None -> todo env ())
      in
      let v4 = (* "}" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Choice_str x -> G.L (literal env x)
  | `Choice_var x -> variablish env x
  | `Pref_str (v1, v2) ->
      let v1 =
        (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v1
      in
      let v2 = (* string *) token env v2 in
      todo env (v1, v2)
  | `Paren_exp x -> parenthesized_expression env x
  | `Bin_exp x -> binary_expression env x
  | `Prefix_un_exp x -> prefix_unary_expression env x
  | `Post_un_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2, op =
        (match v2 with
        | `PLUSPLUS tok -> (* "++" *) token env tok, G.Incr
        | `DASHDASH tok -> (* "--" *) token env tok, G.Decr
        )
      in
      G.Call (G.IdSpecial (IncrDecr (op, Postfix), v2), G.fake_bracket [ G.Arg v1 ])
  | `Is_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "is" *) token env v2 in
      let v3 = type_ env v3 in
      G.Call (G.IdSpecial (Op Is, v2), G.fake_bracket [ G.Arg v1; G.ArgType v3; ])
  | `As_exp x -> as_expression env x
  | `Awai_exp (v1, v2) ->
      let v1 = (* "async" *) token env v1 in
      let v2 = compound_statement env v2 in
      todo env (v1, v2)
  | `Yield_exp (v1, v2) ->
      let v1 = (* "yield" *) token env v1 in
      let v2 = anon_choice_exp_1701d0a env v2 in
      todo env (v1, v2)
  | `Cast_exp (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | `Array tok -> (* "array" *) token env tok
        | `Int tok -> (* "int" *) token env tok
        | `Float tok -> (* "float" *) token env tok
        | `Str tok -> (* "string" *) token env tok
        | `Bool tok -> (* "bool" *) token env tok
        )
      in
      let v3 = (* ")" *) token env v3 in
      let v4 = expression env v4 in
      todo env (v1, v2, v3, v4)
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Lambda_exp (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> [attribute_modifier env x]
        | None -> [])
      in
      let v2 =
        (match v2 with
        | Some tok -> (* "async" *)  [G.KeywordAttr (G.Async, token env tok)]
        | None -> [])
      in
      let v3, return_type =
        (match v3 with
        (* TODO: Break out this common N(Id) call into helper? *)
        | `Single_param_params tok -> (* variable *) [G.ParamClassic(G.param_of_id(str env tok))], None
        | `Params_opt_COLON_choice_type_spec (v1, v2) ->
            let v1 = parameters env v1 in
            let v2 =
              (match v2 with
              | Some (v1, v2) ->
                  let v1 = (* ":" *) token env v1 in
                  let v2 = type_ env v2 in
                  Some(v2)
              | None -> None)
            in
            v1, v2
        )
      in
      let v4 = (* "==>" *) token env v4 in
      let v5 =
        (match v5 with
        | `Exp x -> G.exprstmt (expression env x)
        | `Comp_stmt x -> compound_statement env x
        )
      in
      let def : G.function_definition = {
        fkind = (G.LambdaKind, v4); (* Q: Is the arrow the token here? Arrow vs LambdaKind? *)
        fparams = v3;
        frettype = return_type;
        fbody = v5;
      }
      in
      G.Lambda(def)
  | `Call_exp x -> call_expression env x
  | `Sele_exp x -> selection_expression env x
  | `New_exp (v1, v2, v3, v4) ->
      let v1 = (* "new" *) token env v1 in
      let v2 = variablish env v2 in
      let v3 =
        (match v3 with
        | Some x -> type_arguments env x
        | None -> None)
      in
      let v4 = arguments env v4 in
      (* Q: Is the IdSpecial token the `new` or identifier? *)
      (* TODO: HIGH PRIORITY: THIS IS SO WRONG? Call within a call? *)
      G.Call(G.IdSpecial(New, v1), G.fake_bracket [G.Arg (G.Call(v2, v4))])
  | `Incl_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `Incl tok -> (* "include" *) token env tok
        | `Incl_once tok -> (* "include_once" *) token env tok
        )
      in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `Requ_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `Requ tok -> (* "require" *) token env tok
        | `Requ_once tok -> (* "require_once" *) token env tok
        )
      in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `Anon_func_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "async" *) token env tok
        | None -> todo env ())
      in
      let v2 = (* "function" *) token env v2 in
      let v3 = parameters env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> anonymous_function_use_clause env x
        | None -> todo env ())
      in
      let v6 = compound_statement env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Xhp_exp x -> xhp_expression env x
  )

and expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = expression env v1 in
  let v2 = (* ";" *) token env v2 in
  G.ExprStmt(v1, v2)

and extends_clause (env : env) ((v1, v2, v3) : CST.extends_clause) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = type_ env v2 in
      v2
    ) v3
  in
  v2 :: v3

and field_initializer (env : env) ((v1, v2, v3) : CST.field_initializer) =
  (* Q: What should this even be? Assign? *)
  let v1 =
    (match v1 with
    | `Str tok -> (* string *) G.Id ((str env tok), G.empty_id_info())
    | `Scoped_id x -> scoped_identifier env x
    )
  in
  let v2 = (* "=>" *) token env v2 in
  let v3 = expression env v3 in
  G.Assign(G.N v1, v2, v3)

and finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = compound_statement env v2 in
  todo env (v1, v2)

and function_declaration_header (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.function_declaration_header) : G.function_definition * G.label =
  let async_modifier =
    (match v1 with
    | Some tok -> (* "async" *) Some (G.KeywordAttr (G.Async, token env tok))
    | None -> None)
  in
  let function_keyword = (* "function" *) token env v2 in
  let identifier =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env v3
  in
  let type_parameters =
    (match v4 with
    | Some x -> type_parameters env x
    | None -> [])
  in
  let parameters = parameters env v5 in
  let attribute_modifier_and_return_type =
    (match v6 with
    | Some (v1, v2, v3) ->
        let v1 = (* ":" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> Some (attribute_modifier env x)
          | None -> None)
        in
        let v3 = type_ env v3 in
        Some v3
    | None -> None)
  in
  let where_clause =
    (match v7 with
    | Some x -> where_clause env x
    | None -> None)
  in
  {
    fkind = (G.Function, function_keyword);
    fparams = parameters;
    frettype = attribute_modifier_and_return_type;
    fbody = G.empty_fbody;(* To be replaced in parent with real statement *)
  },
  identifier

and implements_clause (env : env) ((v1, v2, v3) : CST.implements_clause) =
  let v1 = (* "implements" *) token env v1 in
  let v2 = type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = type_ env v2 in
      v2
    ) v3
  in
  v2 :: v3

and member_declarations (env : env) ((v1, v2, v3) : CST.member_declarations) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Class_const_decl x -> G.FieldStmt(class_const_declaration env x |> G.s)
      | `Meth_decl x -> G.FieldStmt(method_declaration env x |> G.s)
      | `Prop_decl x -> G.FieldStmt(property_declaration env x |> G.s)
      | `Type_const_decl x -> G.FieldStmt(type_const_declaration env x)
      | `Trait_use_clause x -> trait_use_clause env x
      | `Requ_imples_clause x ->
          require_implements_clause env x
      | `Requ_extends_clause x -> require_extends_clause env x
      | `Xhp_attr_decl x -> G.FieldStmt(xhp_attribute_declaration env x)
      | `Xhp_chil_decl x -> xhp_children_declaration env x
      | `Xhp_cate_decl x -> xhp_category_declaration env x
      )
    ) v2
  in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and method_declaration (env : env) ((v1, v2, v3, v4) : CST.method_declaration) =
  let v1 =
    (match v1 with
    | Some x -> [attribute_modifier env x]
    | None -> [])
  in
  let v2 = List.map (member_modifier env) v2 in
  let v3, identifier = function_declaration_header env v3 in
  let v4 = anon_choice_comp_stmt_c6c6bb4 env v4 in
  let def = {v3 with fbody = v4} in
  let ent = G.basic_entity identifier (v1 @ v2) in
  G.DefStmt (ent, G.FuncDef def)

and parameter (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.parameter) : G.parameter =
  let v1 =
    (match v1 with
    | Some x -> Some (attribute_modifier env x)
    | None -> None) (* Could also be []? Preference? *)
  in
  let v2 =
    (match v2 with
    | Some x -> Some (visibility_modifier env x)
    | None -> None)
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "inout" *) Some (token env tok)
    | None -> None)
  in
  let v4 =
    (match v4 with
    | Some x -> Some (type_ env x)
    | None -> None)
  in
  let v5 =
    (match v5 with
    | Some tok -> (* "..." *) Some (token env tok)
    | None -> None)
  in
  let v6 = (* variable *) str env v6 in
  let v7 =
    (match v7 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = expression env v2 in
        todo env (v1, v2)
    | None -> None)
  in
  (* Q: May not be ?classic?
     TODO: need to check if v5 exists and handle.*)
  ParamClassic {
    pname = Some v6;
    ptype = v4;
    pdefault = v7;
    pattrs = [];
    pinfo = G.basic_id_info (Param, G.sid_TODO); (* Q: But why sid_TODO? Like what is this info? *)
  }

and parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let _v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Vari_modi tok -> (* "..." *) todo env tok (* Added TODO from `token env tok` *)
        | `Param_rep_COMMA_param_opt_COMMA (v1, v2, v3) ->
            let v1 = parameter env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = parameter env v2 in
                v2
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> (* "," *) Some (token env tok)
              | None -> None)
            in
            v1 :: v2
        )
    | None -> [])
  in
  let _v3 = (* ")" *) token env v3 in
  v2

and parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = expression env v2 in
  let v3 = (* ")" *) token env v3 in
  (* Q: Do something better here? *)
  v2

and prefix_unary_expression (env : env) (x : CST.prefix_unary_expression) =
  (match x with
  | `BANG_exp (v1, v2) ->
      let v1 = (* "!" *) token env v1 in
      let v2 = expression env v2 in
      G.Call (G.IdSpecial(G.Op Not, v1), G.fake_bracket [ G.Arg v2 ])
  | `TILDE_exp (v1, v2) ->
      let v1 = (* "~" *) token env v1 in
      let v2 = expression env v2 in
      G.Call (G.IdSpecial(G.Op BitNot, v1), G.fake_bracket [ G.Arg v2 ])
  | `DASH_exp (v1, v2) ->
      let v1 = (* "-" *) token env v1 in
      let v2 = expression env v2 in
      G.Call (G.IdSpecial(G.Op Minus, v1), G.fake_bracket [ G.Arg v2 ])
  | `PLUS_exp (v1, v2) ->
      let v1 = (* "+" *) token env v1 in
      let v2 = expression env v2 in
      G.Call (G.IdSpecial(G.Op Plus, v1), G.fake_bracket [ G.Arg v2 ])
  | `PLUSPLUS_exp (v1, v2) ->
      let v1 = (* "++" *) token env v1 in
      let v2 = expression env v2 in
      G.Call (G.IdSpecial (IncrDecr (Incr, Prefix), v1), G.fake_bracket [ G.Arg v2 ])
  | `DASHDASH_exp (v1, v2) ->
      let v1 = (* "--" *) token env v1 in
      let v2 = expression env v2 in
      G.Call (G.IdSpecial (IncrDecr (Decr, Prefix), v1), G.fake_bracket [ G.Arg v2 ])
  | `Print_exp (v1, v2) ->
      let v1 = (* "print" *) token env v1 in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `Clone_exp (v1, v2) ->
      let v1 = (* "clone" *) token env v1 in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `Await_exp (v1, v2) ->
      let v1 = (* "await" *) token env v1 in
      let v2 = expression env v2 in
      Await(v1, v2)
  | `AT_exp (v1, v2) ->
      let v1 = (* "@" *) token env v1 in
      let v2 = expression env v2 in
      todo env (v1, v2)
  )

and property_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.property_declaration) =
  let v1 =
    (match v1 with
    | Some x -> [attribute_modifier env x]
    | None -> [])
  in
  let v2 = List.map (member_modifier env) v2 in
  let attrs = v1 @ v2 in
  let v3 =
    (match v3 with
    | Some x -> Some(type_ env x)
    | None -> None)
  in
  let v4 = property_declarator env v4 attrs v3 in
  let v5 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = property_declarator env v2 attrs v3 in
      v2
    ) v5
  in
  let defs = v4 :: v5 in
  let v6 = (* ";" *) token env v6 in
  G.Block(G.fake_bracket defs)

and property_declarator (env : env) ((v1, v2) : CST.property_declarator) attrs vtype =
  let v1 = (* variable *) str env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = expression env v2 in
        Some v2
    | None -> None)
  in
  let ent = G.basic_entity v1 attrs in
  let def : G.variable_definition = {
    vinit = v2;
    vtype = vtype;
  }
  in
  G.DefStmt(ent, G.VarDef def) |> G.s

and require_extends_clause (env : env) ((v1, v2, v3, v4, v5) : CST.require_extends_clause) =
  let v1 = (* "require" *) token env v1 in
  let v2 = (* "extends" *) token env v2 in
  let v3 = type_ env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = type_ env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 = (* ";" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and require_implements_clause (env : env) ((v1, v2, v3, v4, v5) : CST.require_implements_clause) =
  let v1 = (* "require" *) token env v1 in
  let v2 = (* "implements" *) token env v2 in
  let v3 = type_ env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = type_ env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 = (* ";" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and selection_expression (env : env) ((v1, v2, v3) : CST.selection_expression) =
  let v1 =
    (match v1 with
    | `Choice_var x -> variablish env x
    | `As_exp x -> as_expression env x
    )
  in
  let v2 = anon_choice_QMARKDASHGT_ce9cc19 env v2 in
  let v3 =
    (match v3 with
    | `Choice_var x -> variablish env x
    | `Braced_exp x -> braced_expression env x
    | `Choice_type x -> G.N(G.Id ((keyword env x), G.empty_id_info()))
    )
  in
  G.DotAccess(v1, v2, G.EDynamic v3)

and statement (env : env) (x : CST.statement) =
  (match x with
  | `Choice_func_decl x -> declaration env x |> G.s
  | `Comp_stmt x -> compound_statement env x
  | `Empty_stmt tok -> (* ";" *) empty_stmt env tok
  | `Exp_stmt x -> expression_statement env x |> G.s
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> 
            let v1 = expression env x in
            Some v1
        | None -> None)
      in
      let v3 = (* ";" *) token env v3 in
      G.Return (v1, v2, v3) |> G.s
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = (* "break" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> expression env x
        | None -> todo env ())
      in
      let v3 = (* ";" *) token env v3 in
      todo env (v1, v2, v3)
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = (* "continue" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> expression env x
        | None -> todo env ())
      in
      let v3 = (* ";" *) token env v3 in
      todo env (v1, v2, v3)
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = (* "throw" *) token env v1 in
      let v2 = expression env v2 in
      let v3 = (* ";" *) token env v3 in
      G.Throw(v1, v2, v3) |> G.s
  | `Echo_stmt (v1, v2, v3, v4) ->
      let v1 = (* "echo" *) str env v1 in
      let v2 = G.Arg (expression env v2) in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = G.Arg(expression env v2) in
          v2
        ) v3
      in
      let v4 = (* ";" *) token env v4 in
      let exprs = v2 :: v3 in 
      let iden = G.Id (v1, G.empty_id_info()) in
      (* TODO: This doesn't seem exactly right?
          I feel like it should be IdSpecial maybe? *)
      G.ExprStmt(G.Call((G.N iden), G.fake_bracket exprs), v4) |> G.s
  | `Unset_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "unset" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = variablish env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = variablish env v2 in
                todo env (v1, v2)
              ) v2
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v4 = (* ")" *) token env v4 in
      let v5 = (* ";" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Use_stmt (v1, v2, v3) ->
    (* Q: ImportAs vs ImportFrom? Both use alias option. *)
    (* Q: What do comma seperated use statements mean? And how do they alias? *)
      let v1 = (* "use" *) token env v1 in
      let create_directive use_clause prefix_idents =
        let idents, alias = use_clause in
        let idents = (match idents with
          | Some x -> x
          | None -> raise Impossible
        ) in
        G.s(
          G.DirectiveStmt(
           G.ImportAs(v1, G.DottedName (prefix_idents @ idents), alias)))
      in
      let v2 =
        (match v2 with
        | `Use_clause_rep_COMMA_use_clause_opt_COMMA (v1, v2, v3) ->
            let v1 = create_directive (use_clause env v1) [] in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = use_clause env v2 in
                (* Q: Module name vs identifier? What's the difference? *)
                create_directive v2 []
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> (* "," *) Some(token env tok)
              | None -> None)
            in
            (* Q: Is it improper to use blocks to represent directive groups? *)
            (* Lua seems to get around this by return statement lists here and flattening way up *)
            G.Block (G.fake_bracket (v1 :: v2))
        | `Opt_use_type_name_id_LCURL_use_clause_rep_COMMA_use_clause_opt_COMMA_RCURL (v1, v2, v3, v4, v5, v6, v7) ->
            (* Q: How to represent `use` type? We are also possibly passed it up from use_clause. *)
            let v1 =
              (match v1 with
              | Some x -> Some(use_type env x)
              | None -> None)
            in
            let v2 = namespace_identifier env v2 in
            let ident_prefix = (match v2 with
            | Some x -> unwrap_qualified_identifier x
            | None -> []
            ) in
            let v3 = (* "{" *) token env v3 in
            let v4 = create_directive(use_clause env v4) ident_prefix in
            let v5 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = use_clause env v2 in
                create_directive v2 ident_prefix
              ) v5
            in
            let v6 =
              (match v6 with
              | Some tok -> (* "," *) Some(token env tok)
              | None -> None)
            in
            let v7 = (* "}" *) token env v7 in
            G.Block (G.fake_bracket (v4 :: v5))
        )
      in
      let v3 = (* ";" *) token env v3 in
      v2 |> G.s
  | `If_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement env v3 in
      (* Q: Should we be folding? Where does elif go? *)
      let v4 =
        List.map (fun (v1, v2, v3) ->
          let v1 =
            (match v1 with
            | `Elseif tok -> (* "elseif" *) token env tok
            | `Else_if (v1, v2) ->
                let v1 = (* "else" *) token env v1 in
                let v2 = (* "if" *) token env v2 in
                v2 (* TODO: Concat tokens somehow? *)
            )
          in
          let v2 = parenthesized_expression env v2 in
          let v3 = statement env v3 in
          G.If(v1, v2, v3, None) |> G.s
        ) v4
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) ->
            let v1 = (* "else" *) token env v1 in
            let v2 = statement env v2 in
            [v2]
        | None -> [])
      in
      (* TODO: Figure out if this is even a proper representation *)
      G.If(v1, v2, v3, Some (G.Block(G.fake_bracket (v4 @ v5)) |> G.s)) |> G.s
  | `While_stmt (v1, v2, v3) ->
      let v1 = (* "while" *) token env v1 in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement env v3 in
      todo env (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "do" *) token env v1 in
      let v2 = statement env v2 in
      let v3 = (* "while" *) token env v3 in
      let v4 = parenthesized_expression env v4 in
      let v5 = (* ";" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> anon_exp_rep_COMMA_exp_0bb260c env x
        | None -> todo env ())
      in
      let v4 = (* ";" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> anon_exp_rep_COMMA_exp_0bb260c env x
        | None -> todo env ())
      in
      let v6 = (* ";" *) token env v6 in
      let v7 =
        (match v7 with
        | Some x -> anon_exp_rep_COMMA_exp_0bb260c env x
        | None -> todo env ())
      in
      let v8 = (* ")" *) token env v8 in
      let v9 = statement env v9 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Switch_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "switch" *) token env v1 in
      let v2 = parenthesized_expression env v2 in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        List.map (fun x ->
          (match x with
          | `Switch_case x -> switch_case env x
          | `Switch_defa x -> switch_default env x
          )
        ) v4
      in
      let v5 = (* "}" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Fore_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = (* "foreach" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = expression env v3 in
      let v4 =
        (match v4 with
        | Some tok -> (* "await" *) token env tok
        | None -> todo env ())
      in
      let v5 = (* as *) token env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = variablish env v1 in
            let v2 = (* "=>" *) token env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v7 = variablish env v7 in
      let v8 = (* ")" *) token env v8 in
      let v9 = statement env v9 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Try_stmt (v1, v2, v3) ->
      let v1 = (* "try" *) token env v1 in
      let v2 = compound_statement env v2 in
      let v3 =
        List.map (fun x ->
          (match x with
          | `Catch_clause x -> catch_clause env x
          | `Fina_clause x -> finally_clause env x
          )
        ) v3
      in
      todo env (v1, v2, v3)
  | `Conc_stmt (v1, v2) ->
      let v1 = (* "concurrent" *) token env v1 in
      let v2 = compound_statement env v2 in
      todo env (v1, v2)
  | `Using_stmt (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "await" *) token env tok
        | None -> todo env ())
      in
      let v2 = (* "using" *) token env v2 in
      let v3 =
        (match v3 with
        | `Exp_stmt x -> expression_statement env x
        | `LPAR_exp_rep_COMMA_exp_RPAR_choice_comp_stmt (v1, v2, v3, v4, v5) ->
            let v1 = (* "(" *) token env v1 in
            let v2 = expression env v2 in
            let v3 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = expression env v2 in
                todo env (v1, v2)
              ) v3
            in
            let v4 = (* ")" *) token env v4 in
            let v5 = anon_choice_comp_stmt_c6c6bb4 env v5 in
            todo env (v1, v2, v3, v4, v5)
        )
      in
      todo env (v1, v2, v3)
  )

and switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = expression env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = List.map (statement env) v4 in
  todo env (v1, v2, v3, v4)

and switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = (* "default" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = List.map (statement env) v3 in
  todo env (v1, v2, v3)

and trait_use_clause (env : env) ((v1, v2, v3, v4) : CST.trait_use_clause) =
  let v1 = (* "use" *) token env v1 in
  let v2 = type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | `LCURL_rep_choice_trait_select_clause_SEMI_RCURL (v1, v2, v3) ->
        let v1 = (* "{" *) token env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | `Trait_select_clause x -> trait_select_clause env x
              | `Trait_alias_clause x -> trait_alias_clause env x
              )
            in
            let v2 = (* ";" *) token env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 = (* "}" *) token env v3 in
        todo env (v1, v2, v3)
    | `SEMI tok -> (* ";" *) token env tok
    )
  in
  todo env (v1, v2, v3, v4)

and type_ (env : env) (x : CST.type_) : G.type_=
  (match x with
  | `Type_spec (v1, v2, v3) ->
      let v1 = List.map (type_modifier env) v1 in
      let v2 =
        (match v2 with
        | `Choice_bool x -> G.TyBuiltin(primitive_type env x)
        | `Qual_id x -> G.TyN(qualified_identifier env x)
        | `Choice_array x -> G.TyBuiltin(collection_type env x)
        | `Choice_xhp_id x -> G.TyN(Id ((xhp_identifier_ env x), G.empty_id_info()))
        )
      in
      let v3 =
        (match v3 with
        | Some x -> type_arguments env x
        | None -> None)
      in
      v2
  | `Type_cst (v1, v2) ->
      let v1 = List.map (type_modifier env) v1 in
      let v2 = type_constant_ env v2 in
      todo env (v1, v2)
  | `Shape_type_spec (v1, v2, v3, v4, v5) ->
      let v1 = List.map (type_modifier env) v1 in
      let v2 = (* "shape" *) token env v2 in
      let v3 = (* "(" *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2, v3) ->
            let v1 = G.FieldStmt(anon_choice_field_spec_0e0e023 env v1 |> G.s) in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = G.FieldStmt(anon_choice_field_spec_0e0e023 env v2 |> G.s) in
                v2
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> (* "," *) Some(token env tok)
              | None -> None)
            in
            v1 :: v2
        | None -> [])
      in
      let v5 = (* ")" *) token env v5 in
      G.TyRecordAnon(v2, (v3, v4, v5))
  | `Func_type_spec (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = List.map (type_modifier env) v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = (* pattern function\s*\( *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2, v3, v4, v5) ->
            let v1 =
              (match v1 with
              | Some tok -> (* "inout" *) Some(token env tok)
              | None -> None)
            in
            let v2 =  G.ParamClassic(G.param_of_type (type_ env v2)) in
            let v3 =
              (match v3 with
              | Some tok -> (* "..." *) Some(token env tok)
              | None -> None)
            in
            let v4 =
              List.map (fun (v1, v2, v3, v4) ->
              (* TODO: Handle `...` and `inout` *)
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some tok -> (* "inout" *) Some(token env tok)
                  | None -> None)
                in
                let v3 = G.ParamClassic(G.param_of_type (type_ env v3)) in
                let v4 =
                  (match v4 with
                  | Some tok -> (* "..." *) Some(token env tok)
                  | None -> None)
                in
                v3
              ) v4
            in
            let v5 =
              (match v5 with
              | Some tok -> (* "," *) Some(token env tok)
              | None -> None)
            in
            v2 :: v4
        | None -> [])
      in
      let v5 = (* ")" *) token env v5 in
      let v6 = (* ":" *) token env v6 in
      let v7 = type_ env v7 in
      let v8 = (* ")" *) token env v8 in
      G.TyFun(v4, v7)
  | `Tuple_type_spec (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.map (type_modifier env) v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = type_ env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = type_ env v2 in
          v2
        ) v4
      in
      let v5 =
        (match v5 with
        | Some tok -> (* "," *) Some(token env tok)
        | None -> None)
      in
      let v6 = (* ")" *) token env v6 in
      G.TyTuple(v2, (v3 :: v4), v6)
  )

and type_arguments (env : env) ((v1, v2, v3) : CST.type_arguments) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = G.TypeArg(type_ env v1) in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = G.TypeArg(type_ env v2) in
            v2
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> (* "," *) Some(token env tok)
          | None -> None)
        in
        Some(v1 :: v2)
    | None -> None)
  in
  let v3 = (* ">" *) token env v3 in
  (match v2 with
  | Some v2 -> Some(v1, v2, v3)
  | None -> None
  )
  

and type_const_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.type_const_declaration) =
  let v1 =
    (match v1 with
    | Some x -> [attribute_modifier env x]
    | None -> [])
  in
  let v2 = List.map (member_modifier env) v2 in
  let v3 = (* "const" *) [G.KeywordAttr(Const, (token env v3))] in
  let v4 = (* "type" *) token env v4 in
  let v5 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env v5
  in
  let v6 =
    (match v6 with
    | Some x -> Some(type_parameters env x)
    | None -> None)
  in
  (* Q: How to represent this `as __type__`? It is a constraint? Make an attribute?
     OTP_Constrained on type param? But then can't be builtin *)
  let v7 =
    (match v7 with
    | Some (v1, v2) ->
        let v1 = (* "as" *) token env v1 in
        let v2 = type_ env v2 in
        Some v2
    | None -> None)
  in
  let v8 =
    (match v8 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = type_ env v2 in
        Some(v2)
    | None -> None)
  in
  let v9 = (* ";" *) token env v9 in
  (match v8 with
  (* Q: AliasType vs NewType? *)
  | Some v8 -> G.DefStmt(G.basic_entity v5 (v1 @ v2 @ v3), G.TypeDef { tbody = AliasType v8 }) |> G.s
  (* TODO: WHAT TO DO IN THIS CASE? WHAT IS `const type T1;` doing? *)
  | None -> G.DefStmt(G.basic_entity v5 (v1 @ v2 @ v3), G.OtherDef(G.OD_Todo, [])) |> G.s
  )

and type_parameter (env : env) ((v1, v2, v3, v4) : CST.type_parameter) =
  let v1 =
    (match v1 with
    | Some x -> attribute_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `PLUS tok -> (* "+" *) token env tok
        | `DASH tok -> (* "-" *) token env tok
        | `Reify tok -> (* "reify" *) token env tok
        )
    | None -> todo env ())
  in
  let v3 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
  in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `As tok -> (* "as" *) token env tok
        | `Super tok -> (* "super" *) token env tok
        )
      in
      let v2 = type_ env v2 in
      todo env (v1, v2)
    ) v4
  in
  todo env (v1, v2, v3, v4)

and type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = type_parameter env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = type_parameter env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v5 = (* ">" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and variablish (env : env) (x : CST.variablish) =
  (match x with
  | `Var tok -> (* variable *) G.N (Id (str env tok, G.empty_id_info ()))
  | `Pipe_var tok -> (* "$$" *) todo env x (* TODO: token env tok *)
  | `List_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "list" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> [expression env x]
        | None -> [])
      in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 =
            (match v2 with
            | Some x -> [expression env x]
            | None -> [])
          in
          v2
        ) v4
      in
      let exprs = v3 @ (List.flatten v4) in
      let v5 =
        (match v5 with
        | Some tok -> (* "," *) Some(token env tok)
        | None -> None)
      in
      let v6 = (* ")" *) token env v6 in
      G.Container(List, (v2, exprs,v6))
  | `Subs_exp (v1, v2, v3, v4) ->
      let v1 = expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> expression env x
        | None -> todo env ()) (* TODO: This creates a new element... *)
      in
      let v4 = (* "]" *) token env v4 in
      G.ArrayAccess(v1, (v2, v3, v4))
  | `Qual_id x -> G.N (qualified_identifier env x)
  | `Paren_exp x -> parenthesized_expression env x
  | `Call_exp x -> call_expression env x
  | `Scoped_id x -> G.N (scoped_identifier env x)
  | `Scope_id x -> todo env x (* TODO: scope_identifier env x *)
  | `Sele_exp x -> selection_expression env x
  | `Choice_xhp_id x -> todo env x (* TODO: xhp_identifier_ env x *)
  )

and where_clause (env : env) ((v1, v2) : CST.where_clause) =
  let v1 = (* "where" *) token env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = where_constraint env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ())
      in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and where_constraint (env : env) ((v1, v2, v3) : CST.where_constraint) =
  let v1 = type_ env v1 in
  let v2 =
    (match v2 with
    | `As tok -> (* "as" *) token env tok
    | `Super tok -> (* "super" *) token env tok
    | `EQ tok -> (* "=" *) token env tok
    )
  in
  let v3 = type_ env v3 in
  todo env (v1, v2, v3)

and xhp_attribute (env : env) (x : CST.xhp_attribute) =
  (match x with
  | `Xhp_id_EQ_choice_str (v1, v2, v3) ->
      let v1 =
        (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (match v3 with
        | `Str tok -> (* string *) token env tok
        | `Braced_exp x -> todo env x (* TODO: braced_expression env x *)
        )
      in
      todo env (v1, v2, v3)
  | `Choice_braced_exp x ->
      (match x with
      | `Braced_exp x -> braced_expression env x
      | `Xhp_spread_exp x -> xhp_spread_expression env x
      )
  )

and xhp_attribute_declaration (env : env) ((v1, v2, v3, v4) : CST.xhp_attribute_declaration) =
  let v1 = (* "attribute" *) token env v1 in
  let v2 = xhp_class_attribute env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = xhp_class_attribute env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = (* ";" *) token env v4 in
  todo env (v1, v2, v3, v4)

and xhp_class_attribute (env : env) ((v1, v2, v3, v4) : CST.xhp_class_attribute) =
  let v1 =
    (match v1 with
    | `Choice_type_spec x -> type_ env x
    | `Xhp_enum_type x -> xhp_enum_type env x
    )
  in
  let v2 =
    (match v2 with
    | Some tok ->
        (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x ->
        (match x with
        | `ATre tok -> (* "@required" *) token env tok
        | `ATla tok -> (* "@lateinit" *) token env tok
        )
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and xhp_expression (env : env) (x : CST.xhp_expression) =
  (match x with
  | `Xhp_open_close (v1, v2, v3, v4) ->
      let v1 = (* "<" *) token env v1 in
      let v2 = xhp_identifier_ env v2 in
      let v3 = List.map (xhp_attribute env) v3 in
      let v4 = (* "/>" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Xhp_open_rep_choice_xhp_str_xhp_close (v1, v2, v3) ->
      let v1 = xhp_open env v1 in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Xhp_str tok -> (* xhp_string *) token env tok
          | `Xhp_comm tok -> (* xhp_comment *) token env tok
          | `Braced_exp x -> todo env x (* TODO: braced_expression env x *)
          | `Xhp_exp x -> todo env x (* TODO: xhp_expression env x *)
          )
        ) v2
      in
      let v3 = xhp_close env v3 in
      todo env (v1, v2, v3)
  )

and xhp_open (env : env) ((v1, v2, v3, v4) : CST.xhp_open) =
  let v1 = (* "<" *) token env v1 in
  let v2 = xhp_identifier_ env v2 in
  let v3 = List.map (xhp_attribute env) v3 in
  let v4 = (* ">" *) token env v4 in
  todo env (v1, v2, v3, v4)

and xhp_spread_expression (env : env) ((v1, v2, v3, v4) : CST.xhp_spread_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 = (* "..." *) token env v2 in
  let v3 = expression env v3 in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

let script (env : env) ((v1, v2) : CST.script) : G.program =
  let _v1 =
    (match v1 with
    | Some tok -> (* pattern <\?[hH][hH] *) token env tok |> ignore
    | None -> ())
  in
  List.map (statement env) v2


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_hack.Parse.file file ())
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in

      try script env cst
      with Failure "not implemented" as exn ->
        let s = Printexc.get_backtrace () in
        pr2 "Some constructs are not handled yet";
        pr2 "CST was:";
        CST.dump_tree cst;
        pr2 "Original backtrace:";
        pr2 s;
        raise exn)

(* todo: special mode to convert Ellipsis in the right construct! (This comment was copied from Parse_lua. *)
let parse_pattern str =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_hack.Parse.string str ())
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      match script env cst with
      | [ { G.s = G.ExprStmt (e, _); _ } ] -> G.E e
      | [ x ] -> G.S x
      | xs -> G.Ss xs)

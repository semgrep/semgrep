(**
   Derive a javascript AST from a tree-sitter typescript CST.

   This is derived from generated code 'typescript/lib/Boilerplate.ml'
   in ocaml-tree-sitter-lang and reuse functions from
   Parse_javascript_tree_sitter since the typescript tree-sitter grammar
   itself extends the tree-sitter javascript grammar.
*)

module AST = Ast_js
module H = Parse_tree_sitter_helpers
module G = AST_generic
open Ast_js

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = H.env
let fb = G.fake_bracket

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(*
   We extend the javascript parsing module. Types are
   partially compatible.
*)
module JS_CST = Parse_javascript_tree_sitter_priv.CST
module JS = Parse_javascript_tree_sitter_priv
module CST = Tree_sitter_typescript.CST

let todo (env : env) _ =
   failwith "not implemented"

let hash_bang_line (env : env) (tok : CST.hash_bang_line) =
  JS.token env tok (* pattern #!.* *)

let import (env : env) (tok : CST.import) =
  let id = JS.identifier env tok (* import *) in
  JS.idexp id

let accessibility_modifier (env : env) (x : CST.accessibility_modifier) =
  (match x with
  | `Public tok -> JS.token env tok (* "public" *)
  | `Priv tok -> JS.token env tok (* "private" *)
  | `Prot tok -> JS.token env tok (* "protected" *)
  )

let predefined_type (env : env) (x : CST.predefined_type) =
  (match x with
  | `Any tok -> JS.token env tok (* "any" *)
  | `Num tok -> JS.token env tok (* "number" *)
  | `Bool tok -> JS.token env tok (* "boolean" *)
  | `Str tok -> JS.token env tok (* "string" *)
  | `Symb tok -> JS.token env tok (* "symbol" *)
  | `Void tok -> JS.token env tok (* "void" *)
  )

let anon_choice_PLUSPLUS (env : env) (x : CST.anon_choice_PLUSPLUS) =
  (match x with
  | `PLUSPLUS tok -> JS.token env tok (* "++" *)
  | `DASHDASH tok -> JS.token env tok (* "--" *)
  )

let anon_choice_type (env : env) (x : CST.anon_choice_type) =
  (match x with
  | `Type tok -> JS.token env tok (* "type" *)
  | `Typeof tok -> JS.token env tok (* "typeof" *)
  )

let jsx_identifier (env : env) (tok : CST.jsx_identifier) =
  JS.token env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)

let automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  JS.token env tok (* automatic_semicolon *)

let jsx_text (env : env) (tok : CST.jsx_text) =
  JS.token env tok (* pattern [^{}<>]+ *)

let anon_choice_get (env : env) (x : CST.anon_choice_get) =
  (match x with
  | `Get tok -> JS.token env tok (* "get" *)
  | `Set tok -> JS.token env tok (* "set" *)
  | `STAR tok -> JS.token env tok (* "*" *)
  )

let reserved_identifier (env : env) (x : CST.reserved_identifier) =
  (match x with
  | `Decl tok -> JS.identifier env tok (* "declare" *)
  | `Name tok -> JS.identifier env tok (* "namespace" *)
  | `Type tok -> JS.identifier env tok (* "type" *)
  | `Public tok -> JS.identifier env tok (* "public" *)
  | `Priv tok -> JS.identifier env tok (* "private" *)
  | `Prot tok -> JS.identifier env tok (* "protected" *)
  | `Read tok -> JS.identifier env tok (* "readonly" *)
  | `Module tok -> JS.identifier env tok (* "module" *)
  | `Any tok -> JS.identifier env tok (* "any" *)
  | `Num tok -> JS.identifier env tok (* "number" *)
  | `Bool tok -> JS.identifier env tok (* "boolean" *)
  | `Str tok -> JS.identifier env tok (* "string" *)
  | `Symb tok -> JS.identifier env tok (* "symbol" *)
  | `Void tok -> JS.identifier env tok (* "void" *)
  | `Export tok -> JS.identifier env tok (* "export" *)
  | `Choice_get x ->
      (match x with
      | `Get tok -> JS.identifier env tok (* "get" *)
      | `Set tok -> JS.identifier env tok (* "set" *)
      | `Async tok -> JS.identifier env tok (* "async" *)
      | `Static tok -> JS.identifier env tok (* "static" *)
      )
  )

let anon_choice_COMMA (env : env) (x : CST.anon_choice_COMMA) =
  (match x with
  | `COMMA tok -> JS.token env tok (* "," *)
  | `Choice_auto_semi x -> JS.semicolon env x
  )

let import_export_specifier (env : env) ((v1, v2, v3) : CST.import_export_specifier) =
(*
  let v1 =
    (match v1 with
    | Some x -> anon_choice_type env x
    | None -> todo env ())
  in
*)
  JS.import_export_specifier env (v2, v3)

let rec anon_choice_type_id (env : env) (x : CST.anon_choice_type_id) =
  (match x with
  | `Id tok -> [JS.identifier env tok] (* identifier *)
  | `Nested_id x -> nested_identifier env x
  )

and nested_identifier (env : env) ((v1, v2, v3) : CST.nested_identifier) =
  let v1 = anon_choice_type_id env v1 in
  let v2 = JS.token env v2 (* "." *) in
  let v3 = JS.identifier env v3 (* identifier *) in
  v1 @ [v3]

let concat_nested_identifier env (idents : ident list) : ident =
  let str = idents |> List.map fst |> String.concat "." in
  str, H.combine_infos env (idents |> List.map snd)

(* TODO: add 'foo = require(...)' to AST?
   Treating it like assignment and function application for now.
 *)
let import_require_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.import_require_clause) =
  let v1 = JS.identifier env v1 (* identifier *) |> JS.idexp in
  let v2 = JS.token env v2 (* "=" *) in
  let v3 = JS.identifier env v3 (* "require" *) |> JS.idexp in
  let v4 = JS.token env v4 (* "(" *) in
  let v5 = JS.string_ env v5 |> JS.idexp in
  let v6 = JS.token env v6 (* ")" *) in
  let args = (v4, [v5], v6) in
  let apply = Apply (v3, args) in
  Assign (v1, v2, apply)

let literal_type (env : env) (x : CST.literal_type) =
  (match x with
  | `Num_ (v1, v2) ->
      let v1 =
        (match v1 with
        | `DASH tok -> JS.token env tok (* "-" *)
        | `PLUS tok -> JS.token env tok (* "+" *)
        )
      in
      let v2 = JS.token env v2 (* number *) in
      todo env (v1, v2)
  | `Num tok -> JS.token env tok (* number *)
  | `Str x -> todo env (JS.string_ env x)
  | `True tok -> JS.token env tok (* "true" *)
  | `False tok -> JS.token env tok (* "false" *)
  )

let nested_type_identifier (env : env) ((v1, v2, v3) : CST.nested_type_identifier) =
  let v1 = anon_choice_type_id env v1 in
  let v2 = JS.token env v2 (* "." *) in
  let v3 = JS.token env v3 (* identifier *) in
  todo env (v1, v2, v3)

let anon_choice_type_id2 (env : env) (x : CST.anon_choice_type_id2) =
  (match x with
  | `Id tok -> JS.token env tok (* identifier *)
  | `Nested_type_id x -> nested_type_identifier env x
  )

let anon_choice_rese_id (env : env) (x : CST.anon_choice_rese_id) : ident =
  (match x with
  | `Choice_decl x -> reserved_identifier env x
  | `Id tok -> JS.identifier env tok (* identifier *)
  )

let identifier_reference (env : env) (x : CST.identifier_reference) : ident =
  (match x with
  | `Id tok -> JS.identifier env tok (* identifier *)
  | `Choice_decl x -> reserved_identifier env x
  )

let rec parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let _v1 = JS.token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | `Exp_opt_type_anno (v1, v2) ->
        let v1 = expression env v1 in
        let _v2 () =
          (match v2 with
          | Some x -> type_annotation env x
          | None -> todo env ())
        in
        v1
    | `Seq_exp x -> sequence_expression env x
    )
  in
  let _v3 = JS.token env v3 (* ")" *) in
  v2

and destructuring_pattern (env : env) (x : CST.destructuring_pattern) : expr =
  (match x with
  | `Obj x -> let o = object_ env x in Obj o
  | `Array x -> array_ env x
  )

and variable_declaration (env : env) ((v1, v2, v3, v4) : CST.variable_declaration) =
  let v1 = Var, JS.token env v1 (* "var" *) in
  let v2 = variable_declarator env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = JS.token env v1 (* "," *) in
      let v2 = variable_declarator env v2 in
      v2
    ) v3
  in
  let _v4 = JS.semicolon env v4 in
  let vars = v2::v3 in
  build_vars v1 vars

and function_ (env : env) ((v1, v2, v3, v4, v5) : CST.function_) =
  let v1 =
    (match v1 with
    | Some tok -> JS.token env tok (* "async" *)
    | None -> todo env ())
  in
  let v2 = JS.token env v2 (* "function" *) in
  let v3 =
    (match v3 with
    | Some tok -> JS.token env tok (* identifier *)
    | None -> todo env ())
  in
  let v4 = call_signature env v4 in
  let v5 = statement_block env v5 in
  todo env (v1, v2, v3, v4, v5)

and generic_type (env : env) ((v1, v2) : CST.generic_type) =
  let v1 = anon_choice_type_id2 env v1 in
  let v2 = type_arguments env v2 in
  todo env (v1, v2)

and implements_clause (env : env) ((v1, v2, v3) : CST.implements_clause) =
  let v1 = JS.token env v1 (* "implements" *) in
  let v2 = type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = JS.token env v1 (* "," *) in
      let v2 = type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and anon_choice_exp (env : env) (x : CST.anon_choice_exp) =
  (match x with
  | `Exp x -> expression env x
  | `Spread_elem x -> spread_element env x
  )

and switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = JS.token env v1 (* "default" *) in
  let v2 = JS.token env v2 (* ":" *) in
  let v3 = List.map (statement env) v3 in
  todo env (v1, v2, v3)

and binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "||" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* ">>" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* ">>>" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "<<" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "&" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "^" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "|" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "+" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "-" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "*" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "/" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "%" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "**" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "<" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "<=" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "==" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "===" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "!=" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "!==" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* ">=" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* ">" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "??" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_inst_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "instanceof" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_in_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "in" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  )

and arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = JS.token env v1 (* "(" *) in
  let v2 =
    anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp env v2
  in
  let v3 = JS.token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and generator_function_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.generator_function_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> JS.token env tok (* "async" *)
    | None -> todo env ())
  in
  let v2 = JS.token env v2 (* "function" *) in
  let v3 = JS.token env v3 (* "*" *) in
  let v4 = JS.token env v4 (* identifier *) in
  let v5 = call_signature env v5 in
  let v6 = statement_block env v6 in
  let v7 =
    (match v7 with
    | Some tok -> JS.token env tok (* automatic_semicolon *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and variable_declarator (env : env) ((v1, v2, v3) : CST.variable_declarator) =
  let v1 = anon_choice_type_id_ env v1 in
  let v2 =
    (match v2 with
    | Some x -> type_annotation env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> initializer_ env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and sequence_expression (env : env) ((v1, v2, v3) : CST.sequence_expression) =
  let v1 = expression env v1 in
  let v2 = JS.token env v2 (* "," *) in
  let v3 =
    (match v3 with
    | `Seq_exp x -> sequence_expression env x
    | `Exp x -> expression env x
    )
  in
  Apply (IdSpecial (Seq, v2), fb [v1; v3])

and type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) =
  let v1 = JS.token env v1 (* "<" *) in
  let v2 = type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = JS.token env v1 (* "," *) in
      let v2 = type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> JS.token env tok (* "," *)
    | None -> todo env ())
  in
  let v5 = JS.token env v5 (* ">" *) in
  todo env (v1, v2, v3, v4, v5)

and class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = JS.token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Deco x -> decorator env x
      | `Meth_defi_opt_choice_auto_semi (v1, v2) ->
          let v1 = method_definition env v1 in
          let v2 =
            (match v2 with
            | Some x -> JS.semicolon env x
            | None -> todo env ())
          in
          todo env (v1, v2)
      | `Choice_abst_meth_sign_choice_choice_auto_semi (v1, v2) ->
          let v1 =
            (match v1 with
            | `Abst_meth_sign x -> abstract_method_signature env x
            | `Index_sign x -> index_signature env x
            | `Meth_sign x -> method_signature env x
            | `Public_field_defi x -> public_field_definition env x
            )
          in
          let v2 =
            (match v2 with
            | `Choice_auto_semi x -> JS.semicolon env x
            | `COMMA tok -> JS.token env tok (* "," *)
            )
          in
          todo env (v1, v2)
      )
    ) v2
  in
  let v3 = JS.token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let v1 = JS.token env v1 (* identifier *) in
  let v2 =
    (match v2 with
    | Some x -> constraint_ env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> default_type env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and member_expression (env : env) ((v1, v2, v3) : CST.member_expression) : expr =
  let v1 =
    (match v1 with
    | `Exp x -> expression env x
    | `Id tok -> JS.identifier env tok |> JS.idexp (* identifier *)
    | `Super tok -> JS.super env tok (* "super" *)
    | `Choice_decl x -> reserved_identifier env x |> JS.idexp
    )
  in
  let v2 = JS.token env v2 (* "." *) in
  let v3 = JS.token env v3 (* identifier *) in
  todo env (v1, v2, v3)

and anon_choice_pair (env : env) (x : CST.anon_choice_pair) =
  (match x with
  | `Pair (v1, v2, v3) ->
      let v1 = property_name env v1 in
      let v2 = JS.token env v2 (* ":" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Spread_elem x -> spread_element env x
  | `Meth_defi x -> method_definition env x
  | `Assign_pat (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Choice_choice_decl x -> anon_choice_rese_id env x |> JS.idexp
        | `Choice_obj x -> destructuring_pattern env x
        )
      in
      let v2 = JS.token env v2 (* "=" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Choice_id x -> identifier_reference env x |> JS.idexp
  )

and subscript_expression (env : env) ((v1, v2, v3, v4) : CST.subscript_expression) : expr =
  let v1 =
    (match v1 with
    | `Exp x -> expression env x
    | `Super tok -> JS.super env tok (* "super" *)
    )
  in
  let _v2 = JS.token env v2 (* "[" *) in
  let v3 = expressions env v3 in
  let _v4 = JS.token env v4 (* "]" *) in
  ArrAccess (v1, v3)

and initializer_ (env : env) ((v1, v2) : CST.initializer_) =
  let v1 = JS.token env v1 (* "=" *) in
  let v2 = expression env v2 in
  todo env (v1, v2)

and constructable_expression (env : env) (x : CST.constructable_expression) : expr =
  (match x with
  | `This tok -> JS.this env tok (* "this" *)
  | `Id tok -> JS.identifier_exp env tok (* identifier *)
  | `Choice_decl x ->
      let id = reserved_identifier env x in
      JS.idexp id
  | `Num tok ->
      let n = JS.number env tok (* number *) in
      Num n
  | `Str x ->
      let s = JS.string_ env x in
      String s
  | `Temp_str x -> template_string env x
  | `Regex (v1, v2, v3, v4) ->
      let v1 = JS.token env v1 (* "/" *) in
      let v2 = JS.token env v2 (* regex_pattern *) in
      let v3 = JS.token env v3 (* "/" *) in
      let v4 =
        (match v4 with
        | Some tok -> JS.token env tok (* pattern [a-z]+ *)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `True tok -> Bool (true, JS.token env tok) (* "true" *)
  | `False tok -> Bool (false, JS.token env tok) (* "false" *)
  | `Null tok -> IdSpecial (Null, JS.token env tok) (* "null" *)
  | `Unde tok -> IdSpecial (Undefined, JS.token env tok) (* "undefined" *)
  | `Import tok -> JS.identifier env tok (* import *) |> JS.idexp
  | `Obj x -> let o = object_ env x in Obj o
  | `Array x -> array_ env x
  | `Func x -> function_ env x
  | `Arrow_func (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some tok -> JS.token env tok (* "async" *)
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | `Choice_choice_decl x -> anon_choice_rese_id env x
        | `Call_sign x -> call_signature env x
        )
      in
      let v3 = JS.token env v3 (* "=>" *) in
      let v4 =
        (match v4 with
        | `Exp x -> expression env x
        | `Stmt_blk x -> statement_block env x
        )
      in
      todo env (v1, v2, v3, v4)
  | `Gene_func (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some tok -> JS.token env tok (* "async" *)
        | None -> todo env ())
      in
      let v2 = JS.token env v2 (* "function" *) in
      let v3 = JS.token env v3 (* "*" *) in
      let v4 =
        (match v4 with
        | Some tok -> JS.token env tok (* identifier *)
        | None -> todo env ())
      in
      let v5 = call_signature env v5 in
      let v6 = statement_block env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Class (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.map (decorator env) v1 in
      let v2 = JS.token env v2 (* "class" *) in
      let v3 =
        (match v3 with
        | Some tok -> JS.token env tok (* identifier *)
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> class_heritage env x
        | None -> todo env ())
      in
      let v6 = class_body env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Paren_exp x -> parenthesized_expression env x
  | `Subs_exp x -> subscript_expression env x
  | `Member_exp x -> member_expression env x
  | `Meta_prop (v1, v2, v3) ->
      let v1 = JS.token env v1 (* "new" *) in
      let v2 = JS.token env v2 (* "." *) in
      let v3 = JS.token env v3 (* "target" *) in
      todo env (v1, v2, v3)
  | `New_exp (v1, v2, v3) ->
      let v1 = JS.token env v1 (* "new" *) in
      let v2 = constructable_expression env v2 in
      let v3 =
        (match v3 with
        | Some x -> arguments env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  )

and anon_choice_prop_name (env : env) (x : CST.anon_choice_prop_name) =
  (match x with
  | `Prop_name x -> property_name env x
  | `Enum_assign (v1, v2) ->
      let v1 = property_name env v1 in
      let v2 = initializer_ env v2 in
      todo env (v1, v2)
  )

and module__ (env : env) ((v1, v2) : CST.module__) =
  let v1 = (* module identifier *)
    (match v1 with
    | `Str x -> JS.string_ env x
    | `Id tok -> JS.identifier env tok (* identifier *)
    | `Nested_id x ->
        nested_identifier env x
        |> concat_nested_identifier env
    )
  in
  let v2 = (* optional module body *)
    (match v2 with
    | Some x -> Some (statement_block env x)
    | None -> None)
  in
  (v1, v2)

and expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = expressions env v1 in
  let v2 = JS.semicolon env v2 in
  todo env (v1, v2)

and catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = JS.token env v1 (* "catch" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = JS.token env v1 (* "(" *) in
        let v2 = anon_choice_type_id_ env v2 in
        let v3 = JS.token env v3 (* ")" *) in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = statement_block env v3 in
  todo env (v1, v2, v3)

and object_type (env : env) ((v1, v2, v3) : CST.object_type) =
  let v1 =
    (match v1 with
    | `LCURL tok -> JS.token env tok (* "{" *)
    | `LCURLBAR tok -> JS.token env tok (* "{|" *)
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 =
          (match v1 with
          | Some x ->
              (match x with
              | `COMMA tok -> JS.token env tok (* "," *)
              | `SEMI tok -> JS.token env tok (* ";" *)
              )
          | None -> todo env ())
        in
        let v2 = anon_choice_export_stmt env v2 in
        let v3 =
          List.map (fun (v1, v2) ->
            let v1 = anon_choice_COMMA env v1 in
            let v2 = anon_choice_export_stmt env v2 in
            todo env (v1, v2)
          ) v3
        in
        let v4 =
          (match v4 with
          | Some x -> anon_choice_COMMA env x
          | None -> todo env ())
        in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | `RCURL tok -> JS.token env tok (* "}" *)
    | `BARRCURL tok -> JS.token env tok (* "|}" *)
    )
  in
  todo env (v1, v2, v3)

and anon_choice_type_id_ (env : env) (x : CST.anon_choice_type_id_) =
  (match x with
  | `Id tok -> JS.identifier env tok (* identifier *) |> JS.idexp
  | `Choice_obj x -> destructuring_pattern env x
  )

and template_string (env : env) ((v1, v2, v3) : CST.template_string) =
  let v1 = JS.token env v1 (* "`" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Temp_chars tok -> JS.token env tok (* template_chars *)
      | `Esc_seq tok -> JS.token env tok (* escape_sequence *)
      | `Temp_subs x -> template_substitution env x
      )
    ) v2
  in
  let v3 = JS.token env v3 (* "`" *) in
  todo env (v1, v2, v3)

and decorator (env : env) ((v1, v2) : CST.decorator) =
  let v1 = JS.token env v1 (* "@" *) in
  let v2 =
    (match v2 with
    | `Choice_id x ->
        let id = identifier_reference env x in
        [id], None
    | `Deco_member_exp x ->
        let ids = JS.decorator_member_expression env x in
        ids, None
    | `Deco_call_exp x ->
        let ids, args = decorator_call_expression env x in
        ids, Some args
    )
  in
  (v1, v2)

and internal_module (env : env) ((v1, v2) : CST.internal_module) =
  let _v1 = JS.token env v1 (* "namespace" *) in
  let v2 = module__ env v2 in
  v2

and anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp (env : env) (opt : CST.anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp) =
  (match opt with
  | Some (v1, v2) ->
      let v1 =
        (match v1 with
        | Some x -> anon_choice_exp env x
        | None -> todo env ())
      in
      let v2 = anon_rep_COMMA_opt_choice_exp env v2 in
      todo env (v1, v2)
  | None -> todo env ())

and for_header (env : env) ((v1, v2, v3, v4, v5, v6) : CST.for_header) =
  let v1 = JS.token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Var tok -> JS.token env tok (* "var" *)
        | `Let tok -> JS.token env tok (* "let" *)
        | `Const tok -> JS.token env tok (* "const" *)
        )
    | None -> todo env ())
  in
  let v3 = anon_choice_paren_exp env v3 in
  let v4 =
    (match v4 with
    | `In tok -> JS.token env tok (* "in" *)
    | `Of tok -> JS.token env tok (* "of" *)
    )
  in
  let v5 = expressions env v5 in
  let v6 = JS.token env v6 (* ")" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and expression (env : env) (x : CST.expression) : expr =
  (match x with
  | `As_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "as" *) in
      let v3 =
        (match v3 with
        | `Type x -> type_ env x
        | `Temp_str x -> template_string env x
        )
      in
      todo env (v1, v2, v3)
  | `Non_null_exp (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = JS.token env v2 (* "!" *) in
      todo env (v1, v2)
  | `Inte_module x ->
      let name, opt_body = internal_module env x in
      todo env opt_body
  | `Super tok -> JS.super env tok (* "super" *)
  | `Type_asse (v1, v2) ->
      let v1 = type_arguments env v1 in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `Choice_this x -> constructable_expression env x
  | `Assign_exp (v1, v2, v3) ->
      let v1 = anon_choice_paren_exp env v1 in
      let v2 = JS.token env v2 (* "=" *) in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Augm_assign_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Member_exp x -> member_expression env x
        | `Subs_exp x -> subscript_expression env x
        | `Choice_get x ->
                let id = reserved_identifier env x in
                idexp id
        | `Id tok ->
                let id = identifier env tok (* identifier *) in
                idexp id
        | `Paren_exp x -> parenthesized_expression env x
        )
      in
      let (op, tok) =
        (match v2 with
        | `PLUSEQ tok -> G.Plus, JS.token env tok (* "+=" *)
        | `DASHEQ tok -> G.Minus, JS.token env tok (* "-=" *)
        | `STAREQ tok -> G.Mult, JS.token env tok (* "*=" *)
        | `SLASHEQ tok -> G.Div, JS.token env tok (* "/=" *)
        | `PERCEQ tok -> G.Mod, JS.token env tok (* "%=" *)
        | `HATEQ tok -> G.BitXor, JS.token env tok (* "^=" *)
        | `AMPEQ tok -> G.BitAnd, JS.token env tok (* "&=" *)
        | `BAREQ tok -> G.BitOr, JS.token env tok (* "|=" *)
        | `GTGTEQ tok -> G.LSR, JS.token env tok (* ">>=" *)
        | `GTGTGTEQ tok -> G.ASR, JS.token env tok (* ">>>=" *)
        | `LTLTEQ tok -> G.LSL, JS.token env tok (* "<<=" *)
        | `STARSTAREQ tok -> G.Pow, JS.token env tok (* "**=" *)
        )
      in
      let v3 = expression env v3 in
      todo env (v1, v2, v3)
  | `Await_exp (v1, v2) ->
      let v1 = JS.token env v1 (* "await" *) in
      let v2 = expression env v2 in
      Apply (IdSpecial (Await, v1), fb [v2])
  | `Un_exp x -> unary_expression env x
  | `Bin_exp x -> binary_expression env x
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let _v2 = JS.token env v2 (* "?" *) in
      let v3 = expression env v3 in
      let _v4 = JS.token env v4 (* ":" *) in
      let v5 = expression env v5 in
      Conditional (v1, v3, v5)
  | `Update_exp x -> update_expression env x
  | `Call_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Exp x -> expression env x
        | `Super tok -> super env tok (* "super" *)
        | `Func x ->
                let (f, idopt) = function_ env x in
                Fun (f, idopt)
        )
      in
      let _v2 () =
        (match v2 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | `Args x ->
                let args = arguments env x in
                Apply (v1, args)
        | `Temp_str x ->
                let (t1, xs, t2) = template_string env x in
                Apply (IdSpecial (Encaps true, t1),
                  (t1, v1::xs, t2))
        )
      in
      v3
  | `Yield_exp (v1, v2) ->
      let v1 = JS.token env v1 (* "yield" *) in
      let v2 =
        (match v2 with
        | `STAR_exp (v1bis, v2) ->
            let v1bis = JS.token env v1bis (* "*" *) in
            let v2 = expression env v2 in
            Apply (IdSpecial (YieldStar, v1), fb [v2])
        | `Opt_exp opt ->
            (match opt with
            | Some x ->
                let x = expression env x in
                Apply (IdSpecial (Yield, v1), fb [x])
            | None ->
                Apply (IdSpecial (Yield, v1), fb [])
          )
        )
      in
      v2
  )

and anon_choice_paren_exp (env : env) (x : CST.anon_choice_paren_exp) =
  (match x with
  | `Paren_exp x -> parenthesized_expression env x
  | `Choice_member_exp x -> lhs_expression env x
  )

and primary_type (env : env) (x : CST.primary_type) =
  (match x with
  | `Paren_type (v1, v2, v3) ->
      let v1 = JS.token env v1 (* "(" *) in
      let v2 = type_ env v2 in
      let v3 = JS.token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  | `Pred_type x -> predefined_type env x
  | `Id tok -> JS.token env tok (* identifier *)
  | `Nested_type_id x -> nested_type_identifier env x
  | `Gene_type x -> generic_type env x
  | `Type_pred (v1, v2, v3) ->
      let v1 = JS.token env v1 (* identifier *) in
      let v2 = JS.token env v2 (* "is" *) in
      let v3 = type_ env v3 in
      todo env (v1, v2, v3)
  | `Obj_type x -> object_type env x
  | `Array_type (v1, v2, v3) ->
      let v1 = primary_type env v1 in
      let v2 = JS.token env v2 (* "[" *) in
      let v3 = JS.token env v3 (* "]" *) in
      todo env (v1, v2, v3)
  | `Tuple_type (v1, v2, v3, v4) ->
      let v1 = JS.token env v1 (* "[" *) in
      let v2 = type_ env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = JS.token env v1 (* "," *) in
          let v2 = type_ env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = JS.token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Flow_maybe_type (v1, v2) ->
      let v1 = JS.token env v1 (* "?" *) in
      let v2 = primary_type env v2 in
      todo env (v1, v2)
  | `Type_query (v1, v2) ->
      let v1 = JS.token env v1 (* "typeof" *) in
      let v2 = anon_choice_type_id env v2 in
      todo env (v1, v2)
  | `Index_type_query (v1, v2) ->
      let v1 = JS.token env v1 (* "keyof" *) in
      let v2 = anon_choice_type_id2 env v2 in
      todo env (v1, v2)
  | `This tok -> JS.token env tok (* "this" *)
  | `Exis_type tok -> JS.token env tok (* "*" *)
  | `Lit_type x -> literal_type env x
  | `Lookup_type (v1, v2, v3, v4) ->
      let v1 = primary_type env v1 in
      let v2 = JS.token env v2 (* "[" *) in
      let v3 = type_ env v3 in
      let v4 = JS.token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  )

and index_signature (env : env) ((v1, v2, v3, v4) : CST.index_signature) =
  let v1 = JS.token env v1 (* "[" *) in
  let v2 =
    (match v2 with
    | `Choice_id_COLON_pred_type (v1, v2, v3) ->
        let v1 = identifier_reference env v1 in
        let v2 = JS.token env v2 (* ":" *) in
        let v3 = predefined_type env v3 in
        todo env (v1, v2, v3)
    | `Mapped_type_clause x -> mapped_type_clause env x
    )
  in
  let v3 = JS.token env v3 (* "]" *) in
  let v4 = type_annotation env v4 in
  todo env (v1, v2, v3, v4)

and unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `BANG_exp (v1, v2) ->
      let v1 = JS.token env v1 (* "!" *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `TILDE_exp (v1, v2) ->
      let v1 = JS.token env v1 (* "~" *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `DASH_exp (v1, v2) ->
      let v1 = JS.token env v1 (* "-" *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `PLUS_exp (v1, v2) ->
      let v1 = JS.token env v1 (* "+" *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `Typeof_exp (v1, v2) ->
      let v1 = JS.token env v1 (* "typeof" *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `Void_exp (v1, v2) ->
      let v1 = JS.token env v1 (* "void" *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
  | `Delete_exp (v1, v2) ->
      let v1 = JS.token env v1 (* "delete" *) in
      let v2 = expression env v2 in
      todo env (v1, v2)
  )

and formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) =
  let v1 = JS.token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 = List.map (decorator env) v1 in
        let v2 = anon_choice_requ_param env v2 in
        let v3 =
          List.map (fun (v1, v2, v3) ->
            let v1 = JS.token env v1 (* "," *) in
            let v2 = List.map (decorator env) v2 in
            let v3 = anon_choice_requ_param env v3 in
            todo env (v1, v2, v3)
          ) v3
        in
        let v4 =
          (match v4 with
          | Some tok -> JS.token env tok (* "," *)
          | None -> todo env ())
        in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  let v3 = JS.token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and default_type (env : env) ((v1, v2) : CST.default_type) =
  let v1 = JS.token env v1 (* "=" *) in
  let v2 = type_ env v2 in
  todo env (v1, v2)

and switch_body (env : env) ((v1, v2, v3) : CST.switch_body) =
  let v1 = JS.token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Switch_case x -> switch_case env x
      | `Switch_defa x -> switch_default env x
      )
    ) v2
  in
  let v3 = JS.token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and mapped_type_clause (env : env) ((v1, v2, v3) : CST.mapped_type_clause) =
  let v1 = JS.token env v1 (* identifier *) in
  let v2 = JS.token env v2 (* "in" *) in
  let v3 = type_ env v3 in
  todo env (v1, v2, v3)

and statement (env : env) (x : CST.statement) =
  (match x with
  | `Export_stmt x -> export_statement env x
  | `Import_stmt (v1, v2, v3, v4) ->
      let v1 = JS.token env v1 (* "import" *) in
      let v2 =
        (match v2 with
        | Some x -> anon_choice_type env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | `Import_clause_from_clause (v1, v2) ->
            let v1 = import_clause env v1 in
            let v2 = from_clause env v2 in
            todo env (v1, v2)
        | `Import_requ_clause x -> import_require_clause env x
        | `Str x -> string_ env x
        )
      in
      let v4 = semicolon env v4 in
      todo env (v1, v2, v3, v4)
  | `Debu_stmt (v1, v2) ->
      let v1 = JS.token env v1 (* "debugger" *) in
      let v2 = semicolon env v2 in
      todo env (v1, v2)
  | `Exp_stmt x -> expression_statement env x
  | `Decl x -> declaration env x
  | `Stmt_blk x -> statement_block env x
  | `If_stmt (v1, v2, v3, v4) ->
      let v1 = JS.token env v1 (* "if" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = JS.token env v1 (* "else" *) in
            let v2 = statement env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = JS.token env v1 (* "switch" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = switch_body env v3 in
      todo env (v1, v2, v3)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = JS.token env v1 (* "for" *) in
      let v2 = JS.token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | `Lexi_decl x -> lexical_declaration env x
        | `Var_decl x -> variable_declaration env x
        | `Exp_stmt x -> expression_statement env x
        | `Empty_stmt tok -> JS.token env tok (* ";" *)
        )
      in
      let v4 =
        (match v4 with
        | `Exp_stmt x -> expression_statement env x
        | `Empty_stmt tok -> JS.token env tok (* ";" *)
        )
      in
      let v5 =
        (match v5 with
        | Some x -> expressions env x
        | None -> todo env ())
      in
      let v6 = JS.token env v6 (* ")" *) in
      let v7 = statement env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `For_in_stmt (v1, v2, v3, v4) ->
      let v1 = JS.token env v1 (* "for" *) in
      let v2 =
        (match v2 with
        | Some tok -> JS.token env tok (* "await" *)
        | None -> todo env ())
      in
      let v3 = for_header env v3 in
      let v4 = statement env v4 in
      todo env (v1, v2, v3, v4)
  | `While_stmt (v1, v2, v3) ->
      let v1 = JS.token env v1 (* "while" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement env v3 in
      todo env (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = JS.token env v1 (* "do" *) in
      let v2 = statement env v2 in
      let v3 = JS.token env v3 (* "while" *) in
      let v4 = parenthesized_expression env v4 in
      let v5 = semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Try_stmt (v1, v2, v3, v4) ->
      let v1 = JS.token env v1 (* "try" *) in
      let v2 = statement_block env v2 in
      let v3 =
        (match v3 with
        | Some x -> catch_clause env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> finally_clause env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `With_stmt (v1, v2, v3) ->
      let v1 = JS.token env v1 (* "with" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement env v3 in
      todo env (v1, v2, v3)
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = JS.token env v1 (* "break" *) in
      let v2 =
        (match v2 with
        | Some tok -> JS.token env tok (* identifier *)
        | None -> todo env ())
      in
      let v3 = semicolon env v3 in
      todo env (v1, v2, v3)
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = JS.token env v1 (* "continue" *) in
      let v2 =
        (match v2 with
        | Some tok -> JS.token env tok (* identifier *)
        | None -> todo env ())
      in
      let v3 = semicolon env v3 in
      todo env (v1, v2, v3)
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = JS.token env v1 (* "return" *) in
      let v2 =
        (match v2 with
        | Some x -> expressions env x
        | None -> todo env ())
      in
      let v3 = semicolon env v3 in
      todo env (v1, v2, v3)
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = JS.token env v1 (* "throw" *) in
      let v2 = expressions env v2 in
      let v3 = semicolon env v3 in
      todo env (v1, v2, v3)
  | `Empty_stmt tok -> JS.token env tok (* ";" *)
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = identifier_reference env v1 in
      let v2 = JS.token env v2 (* ":" *) in
      let v3 = statement env v3 in
      todo env (v1, v2, v3)
  )

and method_definition (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.method_definition) =
  let v1 =
    (match v1 with
    | Some x -> accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some tok -> JS.token env tok (* "static" *)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> JS.token env tok (* "readonly" *)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some tok -> JS.token env tok (* "async" *)
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> anon_choice_get env x
    | None -> todo env ())
  in
  let v6 = property_name env v6 in
  let v7 =
    (match v7 with
    | Some tok -> JS.token env tok (* "?" *)
    | None -> todo env ())
  in
  let v8 = call_signature env v8 in
  let v9 = statement_block env v9 in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)

and class_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.class_declaration) =
  let v1 = List.map (decorator env) v1 in
  let v2 = JS.token env v2 (* "class" *) in
  let v3 = JS.token env v3 (* identifier *) in
  let v4 =
    (match v4 with
    | Some x -> type_parameters env x
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> class_heritage env x
    | None -> todo env ())
  in
  let v6 = class_body env v6 in
  let v7 =
    (match v7 with
    | Some tok -> JS.token env tok (* automatic_semicolon *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let v1 = JS.token env v1 (* "[" *) in
  let v2 =
    anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp env v2
  in
  let v3 = JS.token env v3 (* "]" *) in
  todo env (v1, v2, v3)

and export_statement (env : env) (x : CST.export_statement) =
  (match x with
  | `Choice_export_choice_STAR_from_clause_choice_auto_semi x ->
      (match x with
      | `Export_choice_STAR_from_clause_choice_auto_semi (v1, v2) ->
          let v1 = JS.token env v1 (* "export" *) in
          let v2 =
            (match v2 with
            | `STAR_from_clause_choice_auto_semi (v1, v2, v3) ->
                let v1 = JS.token env v1 (* "*" *) in
                let v2 = from_clause env v2 in
                let v3 = semicolon env v3 in
                todo env (v1, v2, v3)
            | `Export_clause_from_clause_choice_auto_semi (v1, v2, v3) ->
                let v1 = export_clause env v1 in
                let v2 = from_clause env v2 in
                let v3 = semicolon env v3 in
                todo env (v1, v2, v3)
            | `Export_clause_choice_auto_semi (v1, v2) ->
                let v1 = export_clause env v1 in
                let v2 = semicolon env v2 in
                todo env (v1, v2)
            )
          in
          todo env (v1, v2)
      | `Rep_deco_export_choice_decl (v1, v2, v3) ->
          let v1 = List.map (decorator env) v1 in
          let v2 = JS.token env v2 (* "export" *) in
          let v3 =
            (match v3 with
            | `Decl x -> declaration env x
            | `Defa_exp_choice_auto_semi (v1, v2, v3) ->
                let v1 = JS.token env v1 (* "default" *) in
                let v2 = expression env v2 in
                let v3 = semicolon env v3 in
                todo env (v1, v2, v3)
            )
          in
          todo env (v1, v2, v3)
      )
  | `Export_EQ_id_choice_auto_semi (v1, v2, v3, v4) ->
      let v1 = JS.token env v1 (* "export" *) in
      let v2 = JS.token env v2 (* "=" *) in
      let v3 = JS.token env v3 (* identifier *) in
      let v4 = semicolon env v4 in
      todo env (v1, v2, v3, v4)
  | `Export_as_name_id_choice_auto_semi (v1, v2, v3, v4, v5) ->
      let v1 = JS.token env v1 (* "export" *) in
      let v2 = JS.token env v2 (* "as" *) in
      let v3 = JS.token env v3 (* "namespace" *) in
      let v4 = JS.token env v4 (* identifier *) in
      let v5 = semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  )

and type_annotation (env : env) ((v1, v2) : CST.type_annotation) =
  let v1 = JS.token env v1 (* ":" *) in
  let v2 = type_ env v2 in
  todo env (v1, v2)

and anon_rep_COMMA_opt_choice_exp (env : env) (xs : CST.anon_rep_COMMA_opt_choice_exp) =
  List.map (fun (v1, v2) ->
    let v1 = JS.token env v1 (* "," *) in
    let v2 =
      (match v2 with
      | Some x -> anon_choice_exp env x
      | None -> todo env ())
    in
    todo env (v1, v2)
  ) xs

and decorator_call_expression (env : env) ((v1, v2) : CST.decorator_call_expression) =
  let v1 = JS.anon_choice_id_ref env v1 in
  let v2 = arguments env v2 in
  todo env (v1, v2)

and update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Exp_choice_PLUSPLUS (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = anon_choice_PLUSPLUS env v2 in
      todo env (v1, v2)
  | `Choice_PLUSPLUS_exp (v1, v2) ->
      let v1 = anon_choice_PLUSPLUS env v1 in
      let v2 = expression env v2 in
      todo env (v1, v2)
  )

and anon_choice_export_stmt (env : env) (x : CST.anon_choice_export_stmt) =
  (match x with
  | `Export_stmt x -> export_statement env x
  | `Prop_sign (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some x -> accessibility_modifier env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some tok -> JS.token env tok (* "static" *)
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some tok -> JS.token env tok (* "readonly" *)
        | None -> todo env ())
      in
      let v4 = property_name env v4 in
      let v5 =
        (match v5 with
        | Some tok -> JS.token env tok (* "?" *)
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> type_annotation env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Call_sign_ x -> call_signature env x
  | `Cons_sign (v1, v2, v3, v4) ->
      let v1 = JS.token env v1 (* "new" *) in
      let v2 =
        (match v2 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v3 = formal_parameters env v3 in
      let v4 =
        (match v4 with
        | Some x -> type_annotation env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `Index_sign x -> index_signature env x
  | `Meth_sign x -> method_signature env x
  )

and public_field_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.public_field_definition) =
  let v1 =
    (match v1 with
    | Some x -> accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Opt_static_opt_read (v1, v2) ->
        let v1 =
          (match v1 with
          | Some tok -> JS.token env tok (* "static" *)
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | Some tok -> JS.token env tok (* "readonly" *)
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `Opt_abst_opt_read (v1, v2) ->
        let v1 =
          (match v1 with
          | Some tok -> JS.token env tok (* "abstract" *)
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | Some tok -> JS.token env tok (* "readonly" *)
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `Opt_read_opt_abst (v1, v2) ->
        let v1 =
          (match v1 with
          | Some tok -> JS.token env tok (* "readonly" *)
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | Some tok -> JS.token env tok (* "abstract" *)
          | None -> todo env ())
        in
        todo env (v1, v2)
    )
  in
  let v3 = property_name env v3 in
  let v4 =
    (match v4 with
    | Some x ->
        (match x with
        | `QMARK tok -> JS.token env tok (* "?" *)
        | `BANG tok -> JS.token env tok (* "!" *)
        )
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> type_annotation env x
    | None -> todo env ())
  in
  let v6 =
    (match v6 with
    | Some x -> initializer_ env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6)

and anon_choice_choice_type_id (env : env) (x : CST.anon_choice_choice_type_id) =
  (match x with
  | `Choice_id x -> anon_choice_type_id3 env x
  | `Exp x -> expression env x
  )

and lexical_declaration (env : env) ((v1, v2, v3, v4) : CST.lexical_declaration) =
  let v1 =
    (match v1 with
    | `Let tok -> JS.token env tok (* "let" *)
    | `Const tok -> JS.token env tok (* "const" *)
    )
  in
  let v2 = variable_declarator env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = JS.token env v1 (* "," *) in
      let v2 = variable_declarator env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = semicolon env v4 in
  todo env (v1, v2, v3, v4)

and extends_clause (env : env) ((v1, v2, v3) : CST.extends_clause) =
  let v1 = JS.token env v1 (* "extends" *) in
  let v2 = anon_choice_choice_type_id env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = JS.token env v1 (* "," *) in
      let v2 = anon_choice_choice_type_id env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and anon_choice_requ_param (env : env) (x : CST.anon_choice_requ_param) =
  (match x with
  | `Requ_param (v1, v2, v3) ->
      let v1 = parameter_name env v1 in
      let v2 =
        (match v2 with
        | Some x -> type_annotation env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> initializer_ env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Rest_param (v1, v2, v3) ->
      let v1 = JS.token env v1 (* "..." *) in
      let v2 = JS.token env v2 (* identifier *) in
      let v3 =
        (match v3 with
        | Some x -> type_annotation env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Opt_param (v1, v2, v3, v4) ->
      let v1 = parameter_name env v1 in
      let v2 = JS.token env v2 (* "?" *) in
      let v3 =
        (match v3 with
        | Some x -> type_annotation env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> initializer_ env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  )

and enum_body (env : env) ((v1, v2, v3) : CST.enum_body) =
  let v1 = JS.token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = anon_choice_prop_name env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = JS.token env v1 (* "," *) in
            let v2 = anon_choice_prop_name env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> JS.token env tok (* "," *)
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = JS.token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and class_heritage (env : env) (x : CST.class_heritage) =
  (match x with
  | `Extends_clause_opt_imples_clause (v1, v2) ->
      let v1 = extends_clause env v1 in
      let v2 =
        (match v2 with
        | Some x -> implements_clause env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Imples_clause x -> implements_clause env x
  )

and property_name (env : env) (x : CST.property_name) =
  (match x with
  | `Choice_id x -> identifier_reference env x
  | `Str x -> string_ env x
  | `Num tok -> JS.token env tok (* number *)
  | `Comp_prop_name (v1, v2, v3) ->
      let v1 = JS.token env v1 (* "[" *) in
      let v2 = expression env v2 in
      let v3 = JS.token env v3 (* "]" *) in
      todo env (v1, v2, v3)
  )

and switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = JS.token env v1 (* "case" *) in
  let v2 = expressions env v2 in
  let v3 = JS.token env v3 (* ":" *) in
  let v4 = List.map (statement env) v4 in
  todo env (v1, v2, v3, v4)

and spread_element (env : env) ((v1, v2) : CST.spread_element) =
  let v1 = JS.token env v1 (* "..." *) in
  let v2 = expression env v2 in
  todo env (v1, v2)

and expressions (env : env) (x : CST.expressions) : expr =
  (match x with
  | `Exp x -> expression env x
  | `Seq_exp x -> sequence_expression env x
  )

and abstract_method_signature (env : env) ((v1, v2, v3, v4, v5, v6) : CST.abstract_method_signature) =
  let v1 =
    (match v1 with
    | Some x -> accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 = JS.token env v2 (* "abstract" *) in
  let v3 =
    (match v3 with
    | Some x -> anon_choice_get env x
    | None -> todo env ())
  in
  let v4 = property_name env v4 in
  let v5 =
    (match v5 with
    | Some tok -> JS.token env tok (* "?" *)
    | None -> todo env ())
  in
  let v6 = call_signature env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = JS.token env v1 (* "finally" *) in
  let v2 = statement_block env v2 in
  todo env (v1, v2)

and call_signature (env : env) ((v1, v2, v3) : CST.call_signature) =
  let v1 =
    (match v1 with
    | Some x -> type_parameters env x
    | None -> todo env ())
  in
  let v2 = formal_parameters env v2 in
  let v3 =
    (match v3 with
    | Some x -> type_annotation env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and object_ (env : env) ((v1, v2, v3) : CST.object_) : obj_ =
  let v1 = JS.token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> anon_choice_pair env x
          | None -> todo env ())
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = JS.token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | Some x -> anon_choice_pair env x
              | None -> todo env ())
            in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = JS.token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and type_ (env : env) (x : CST.type_) =
  (match x with
  | `Prim_type x -> primary_type env x
  | `Union_type (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> type_ env x
        | None -> todo env ())
      in
      let v2 = JS.token env v2 (* "|" *) in
      let v3 = type_ env v3 in
      todo env (v1, v2, v3)
  | `Inte_type (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> type_ env x
        | None -> todo env ())
      in
      let v2 = JS.token env v2 (* "&" *) in
      let v3 = type_ env v3 in
      todo env (v1, v2, v3)
  | `Func_type (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v2 = formal_parameters env v2 in
      let v3 = JS.token env v3 (* "=>" *) in
      let v4 = type_ env v4 in
      todo env (v1, v2, v3, v4)
  | `Cons_type (v1, v2, v3, v4, v5) ->
      let v1 = JS.token env v1 (* "new" *) in
      let v2 =
        (match v2 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v3 = formal_parameters env v3 in
      let v4 = JS.token env v4 (* "=>" *) in
      let v5 = type_ env v5 in
      todo env (v1, v2, v3, v4, v5)
  )

and type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) =
  let v1 = JS.token env v1 (* "<" *) in
  let v2 = type_parameter env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = JS.token env v1 (* "," *) in
      let v2 = type_parameter env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> JS.token env tok (* "," *)
    | None -> todo env ())
  in
  let v5 = JS.token env v5 (* ">" *) in
  todo env (v1, v2, v3, v4, v5)

and constraint_ (env : env) ((v1, v2) : CST.constraint_) =
  let v1 =
    (match v1 with
    | `Extends tok -> JS.token env tok (* "extends" *)
    | `COLON tok -> JS.token env tok (* ":" *)
    )
  in
  let v2 = type_ env v2 in
  todo env (v1, v2)

and parameter_name (env : env) ((v1, v2, v3) : CST.parameter_name) =
  let v1 =
    (match v1 with
    | Some x -> accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some tok -> JS.token env tok (* "readonly" *)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | `Id tok -> JS.token env tok (* identifier *)
    | `Choice_decl x -> reserved_identifier env x
    | `Choice_obj x -> destructuring_pattern env x
    | `This tok -> JS.token env tok (* "this" *)
    )
  in
  todo env (v1, v2, v3)

and lhs_expression (env : env) (x : CST.lhs_expression) =
  (match x with
  | `Member_exp x -> member_expression env x
  | `Subs_exp x -> subscript_expression env x
  | `Id tok -> JS.token env tok (* identifier *)
  | `Choice_decl x -> reserved_identifier env x
  | `Choice_obj x -> destructuring_pattern env x
  )

and statement_block (env : env) ((v1, v2, v3, v4) : CST.statement_block) =
  let v1 = JS.token env v1 (* "{" *) in
  let v2 = List.map (statement env) v2 in
  let v3 = JS.token env v3 (* "}" *) in
  let v4 =
    (match v4 with
    | Some tok -> JS.token env tok (* automatic_semicolon *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and function_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.function_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> JS.token env tok (* "async" *)
    | None -> todo env ())
  in
  let v2 = JS.token env v2 (* "function" *) in
  let v3 = JS.token env v3 (* identifier *) in
  let v4 = call_signature env v4 in
  let v5 = statement_block env v5 in
  let v6 =
    (match v6 with
    | Some tok -> JS.token env tok (* automatic_semicolon *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6)

and anon_choice_type_id3 (env : env) (x : CST.anon_choice_type_id3) =
  (match x with
  | `Id tok -> JS.token env tok (* identifier *)
  | `Nested_type_id x -> nested_type_identifier env x
  | `Gene_type x -> generic_type env x
  )

and template_substitution (env : env) ((v1, v2, v3) : CST.template_substitution) =
  let v1 = JS.token env v1 (* "${" *) in
  let v2 = expressions env v2 in
  let v3 = JS.token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and method_signature (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.method_signature) =
  let v1 =
    (match v1 with
    | Some x -> accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some tok -> JS.token env tok (* "static" *)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> JS.token env tok (* "readonly" *)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some tok -> JS.token env tok (* "async" *)
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> anon_choice_get env x
    | None -> todo env ())
  in
  let v6 = property_name env v6 in
  let v7 =
    (match v7 with
    | Some tok -> JS.token env tok (* "?" *)
    | None -> todo env ())
  in
  let v8 = call_signature env v8 in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8)

and declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Choice_func_decl x ->
      (match x with
      | `Func_decl x -> function_declaration env x
      | `Gene_func_decl x ->
          generator_function_declaration env x
      | `Class_decl x -> class_declaration env x
      | `Lexi_decl x -> lexical_declaration env x
      | `Var_decl x -> variable_declaration env x
      )
  | `Func_sign (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some tok -> JS.token env tok (* "async" *)
        | None -> todo env ())
      in
      let v2 = JS.token env v2 (* "function" *) in
      let v3 = JS.token env v3 (* identifier *) in
      let v4 = call_signature env v4 in
      let v5 = semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Abst_class_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = JS.token env v1 (* "abstract" *) in
      let v2 = JS.token env v2 (* "class" *) in
      let v3 = JS.token env v3 (* identifier *) in
      let v4 =
        (match v4 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> class_heritage env x
        | None -> todo env ())
      in
      let v6 = class_body env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Module (v1, v2) ->
      let v1 = JS.token env v1 (* "module" *) in
      let v2 = module__ env v2 in
      todo env (v1, v2)
  | `Inte_module x -> internal_module env x
  | `Type_alias_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = JS.token env v1 (* "type" *) in
      let v2 = JS.token env v2 (* identifier *) in
      let v3 =
        (match v3 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v4 = JS.token env v4 (* "=" *) in
      let v5 = type_ env v5 in
      let v6 = semicolon env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Enum_decl (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some tok -> JS.token env tok (* "const" *)
        | None -> todo env ())
      in
      let v2 = JS.token env v2 (* "enum" *) in
      let v3 = JS.token env v3 (* identifier *) in
      let v4 = enum_body env v4 in
      todo env (v1, v2, v3, v4)
  | `Inte_decl (v1, v2, v3, v4, v5) ->
      let v1 = JS.token env v1 (* "interface" *) in
      let v2 = JS.token env v2 (* identifier *) in
      let v3 =
        (match v3 with
        | Some x -> type_parameters env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> extends_clause env x
        | None -> todo env ())
      in
      let v5 = object_type env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Import_alias (v1, v2, v3, v4, v5) ->
      let v1 = JS.token env v1 (* "import" *) in
      let v2 = JS.token env v2 (* identifier *) in
      let v3 = JS.token env v3 (* "=" *) in
      let v4 = anon_choice_type_id env v4 in
      let v5 = semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Ambi_decl (v1, v2) ->
      let v1 = JS.token env v1 (* "declare" *) in
      let v2 =
        (match v2 with
        | `Decl x -> declaration env x
        | `Global_stmt_blk (v1, v2) ->
            let v1 = JS.token env v1 (* "global" *) in
            let v2 = statement_block env v2 in
            todo env (v1, v2)
        | `Module_DOT_id_COLON_type (v1, v2, v3, v4, v5) ->
            let v1 = JS.token env v1 (* "module" *) in
            let v2 = JS.token env v2 (* "." *) in
            let v3 = JS.token env v3 (* identifier *) in
            let v4 = JS.token env v4 (* ":" *) in
            let v5 = type_ env v5 in
            todo env (v1, v2, v3, v4, v5)
        )
      in
      todo env (v1, v2)
  )

let program (env : env) ((v1, v2) : CST.program) =
  let v1 =
    (match v1 with
    | Some tok -> JS.token env tok (* pattern #!.* *)
    | None -> todo env ())
  in
  let v2 = List.map (statement env) v2 in
  todo env (v1, v2)

let jsx_expression (env : env) ((v1, v2, v3) : CST.jsx_expression) =
  let v1 = JS.token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Exp x -> expression env x
        | `Seq_exp x -> sequence_expression env x
        | `Spread_elem x -> spread_element env x
        )
    | None -> todo env ())
  in
  let v3 = JS.token env v3 (* "}" *) in
  todo env (v1, v2, v3)

let rec jsx_opening_element (env : env) ((v1, v2, v3, v4) : CST.jsx_opening_element) =
  let v1 = JS.token env v1 (* "<" *) in
  let v2 =
    (match v2 with
    | `Choice_choice_jsx_id x -> jsx_attribute_name env x
    | `Choice_id_opt_type_args (v1, v2) ->
        let v1 = anon_choice_type_id env v1 in
        let v2 =
          (match v2 with
          | Some x -> type_arguments env x
          | None -> todo env ())
        in
        todo env (v1, v2)
    )
  in
  let v3 = List.map (jsx_attribute_ env) v3 in
  let v4 = JS.token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)

and jsx_attribute_ (env : env) (x : CST.jsx_attribute_) =
  (match x with
  | `Jsx_attr (v1, v2) ->
      let v1 = jsx_attribute_name env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = JS.token env v1 (* "=" *) in
            let v2 = jsx_attribute_value env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Jsx_exp x -> jsx_expression env x
  )

and jsx_child (env : env) (x : CST.jsx_child) =
  (match x with
  | `Jsx_text tok -> JS.token env tok (* pattern [^{}<>]+ *)
  | `Choice_jsx_elem x -> jsx_element_ env x
  | `Jsx_exp x -> jsx_expression env x
  )

and jsx_element_ (env : env) (x : CST.jsx_element_) =
  (match x with
  | `Jsx_elem (v1, v2, v3) ->
      let v1 = jsx_opening_element env v1 in
      let v2 = List.map (jsx_child env) v2 in
      let v3 = jsx_closing_element env v3 in
      todo env (v1, v2, v3)
  | `Jsx_self_clos_elem (v1, v2, v3, v4, v5) ->
      let v1 = JS.token env v1 (* "<" *) in
      let v2 = jsx_element_name env v2 in
      let v3 = List.map (jsx_attribute_ env) v3 in
      let v4 = JS.token env v4 (* "/" *) in
      let v5 = JS.token env v5 (* ">" *) in
      todo env (v1, v2, v3, v4, v5)
  )

and jsx_attribute_value (env : env) (x : CST.jsx_attribute_value) =
  (match x with
  | `Str x -> string_ env x
  | `Jsx_exp x -> jsx_expression env x
  | `Choice_jsx_elem x -> jsx_element_ env x
  | `Jsx_frag (v1, v2, v3, v4, v5, v6) ->
      let v1 = JS.token env v1 (* "<" *) in
      let v2 = JS.token env v2 (* ">" *) in
      let v3 = List.map (jsx_child env) v3 in
      let v4 = JS.token env v4 (* "<" *) in
      let v5 = JS.token env v5 (* "/" *) in
      let v6 = JS.token env v6 (* ">" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  )

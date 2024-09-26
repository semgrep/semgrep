(* David Frankel
 *
 * Copyright (c) 2021 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common.Operators
open Fpath_.Operators

let src = Logs.Src.create "parser_hack"

module Log = (val Logs.src_log src : Logs.LOG)

(*
   Map a Hack CST obtained from the tree-sitter parser directly to the generic
   AST.

   This file is derived from and kept in sync with the generated
   file 'semgrep-hack/lib/Boilerplate.ml'.
*)
module G = AST_generic
module CST = Tree_sitter_hack.CST
module H = Parse_tree_sitter_helpers
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type mode = Pattern | Target
type env = mode H.env

let token = H.token
let str = H.str
let fk tok = Tok.fake_tok tok ""
let fb = Tok.unsafe_fake_bracket

(* Remove this function when everything is done *)
let todo (_env : env) _ = failwith "not implemented"

(* TODO: Make sure not using any semgrep extensions for non-semgrep cases? *)

(* Below are manual additions.
   There are more helpers in AST_generic.ml and AST_generic_helpers.ml.
*)

let empty_stmt env t =
  let t = token env t (* ";" *) in
  G.Block (t, [], t) |> G.s

let todo_deprecation_stmt = G.Block (Tok.unsafe_fake_bracket [])

let basic_typed_entity id attrs tparams : G.entity =
  G.basic_entity id ~attrs ?tparams

let stringify_without_quotes str =
  let s, t = str in
  let s =
    match s with
    | s when s =~ "^\"\\(.*\\)\"$" -> Common.matched1 s
    | s when s =~ "^\'\\(.*\\)\'$" -> Common.matched1 s
    | _ ->
        Log.warn (fun m -> m "weird string literal: %s" s);
        s
  in
  G.String (fb (s, t))

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-hack/Boilerplate.ml *)

let use_type (env : env) (x : CST.use_type) =
  match x with
  | `Name tok -> (* "namespace" *) token env tok
  | `Func tok -> (* "function" *) token env tok
  | `Type tok -> (* "type" *) token env tok
  | `Const tok -> (* "const" *) token env tok

let visibility_modifier (env : env) (x : CST.visibility_modifier) =
  match x with
  | `Public tok -> (* "public" *) G.KeywordAttr (Public, token env tok)
  | `Prot tok -> (* "protected" *) G.KeywordAttr (Protected, token env tok)
  | `Priv tok -> (* "private" *) G.KeywordAttr (Private, token env tok)

let scope_identifier (env : env) (x : CST.scope_identifier) =
  (match x with
  | `Self tok -> (* "self" *) G.IdSpecial (Self, token env tok)
  | `Parent tok -> (* "parent" *) G.IdSpecial (Parent, token env tok)
  (* Q: Add IdSpecial? *)
  | `Static tok -> (* "static" *) G.N (G.Id (str env tok, G.empty_id_info ())))
  |> G.e

let null (env : env) (x : CST.null) =
  match x with
  | `Null_37a6259 tok -> (* "null" *) str env tok
  | `Null_bbb93ef tok -> (* "Null" *) str env tok
  | `NULL tok -> (* "NULL" *) str env tok

let collection_type (env : env) (x : CST.collection_type) =
  match x with
  | `Array tok -> (* "array" *) str env tok
  | `Varray tok -> (* "varray" *) str env tok
  | `Darray tok -> (* "darray" *) str env tok
  | `Vec tok -> (* "vec" *) str env tok
  | `Dict tok -> (* "dict" *) str env tok
  | `Keyset tok -> (* "keyset" *) str env tok

let type_modifier (env : env) (x : CST.type_modifier) =
  match x with
  | `AT tok -> (* "@" *) token env tok
  | `QMARK tok -> (* "?" *) token env tok
  | `TILDE tok -> (* "~" *) token env tok

let false_ (env : env) (x : CST.false_) =
  match x with
  | `False_68934a3 tok -> (* "false" *) token env tok
  | `False_f8320b2 tok -> (* "False" *) token env tok
  | `FALSE tok -> (* "FALSE" *) token env tok

let true_ (env : env) (x : CST.true_) =
  match x with
  | `True_b326b50 tok -> (* "true" *) token env tok
  | `True_f827cf4 tok -> (* "True" *) token env tok
  | `TRUE tok -> (* "TRUE" *) token env tok

let selection_expression_selector (env : env)
    (x : CST.anon_choice_QMARKDASHGT_ce9cc19) =
  match x with
  | `QMARKDASHGT tok -> (* "?->" *) token env tok
  | `DASHGT tok -> (* "->" *) token env tok

let trait_alias_clause (env : env) ((v1, v2, v3) : CST.trait_alias_clause) =
  let v1 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v1
  in
  let v2 = (* "as" *) token env v2 in
  let v3 =
    match v3 with
    | `Visi_modi_opt_id (v1, v2) ->
        let v1 = visibility_modifier env v1 in
        let v2 =
          match v2 with
          | Some tok ->
              (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
              token env tok
          | None -> todo env ()
        in
        todo env (v1, v2)
    | `Opt_visi_modi_id (v1, v2) ->
        let v1 =
          match v1 with
          | Some x -> visibility_modifier env x
          | None -> todo env ()
        in
        let v2 =
          (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v2
        in
        todo env (v1, v2)
  in
  todo env (v1, v2, v3)

let xhp_category_declaration (env : env)
    ((v1, v2, v3, v4) : CST.xhp_category_declaration) =
  let _v1 = (* "category" *) token env v1 in
  let _v2TODO =
    (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) str env v2
  in
  let _v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 =
          (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) str env v2
        in
        v2)
      v3
  in
  let _v4 = (* ";" *) token env v4 in
  (* TODO: Do we care about pre-v3 support? Not doing for now.
     THIS DOES NOTHING *)
  todo_deprecation_stmt

let qualified_identifier (env : env) (x : CST.qualified_identifier) :
    G.dotted_ident =
  match x with
  | `Choice_opt_id_rep1_back_id x -> (
      match x with
      | `Opt_id_rep1_back_id (v1, v2) ->
          let v1 =
            match v1 with
            | Some tok ->
                (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
                [ str env tok ]
            | None -> []
          in
          let v2 =
            List_.map
              (fun (v1, v2) ->
                let _v1 = (* "\\" *) token env v1 in
                let v2 =
                  (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
                  str env v2
                in
                v2)
              v2
          in
          v1 @ v2
      | `Id tok ->
          (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
          let ident = str env tok in
          [ ident ])
  | `Semg_id tok -> (* pattern \$[A-Z_][A-Z_0-9]* *) [ str env tok ]

let empty_statement (env : env) (x : CST.empty_statement) =
  match x with
  | `SEMI tok -> (* ";" *) empty_stmt env tok
  | `Ellips tok ->
      let tok = token env tok in
      (* "..." *) G.ExprStmt (G.Ellipsis tok |> G.e, fk tok) |> G.s

let semgrep_extended_identifier (env : env)
    (x : CST.semgrep_extended_identifier) =
  match x with
  | `Semg_id tok -> (* pattern \$[A-Z_][A-Z_0-9]* *) str env tok
  | `Id tok ->
      (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env tok

let xhp_identifier_ (env : env) (x : CST.xhp_identifier_) =
  match x with
  | `Xhp_id tok ->
      (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) str env tok
  | `Xhp_class_id tok ->
      (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) str env tok

let rec xhp_attribute_expression (env : env) (x : CST.xhp_attribute_expression)
    =
  match x with
  | `Xhp_id tok ->
      (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
  | `Xhp_class_id tok ->
      (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
  | `Xhp_cate_id tok ->
      (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
  | `Xhp_bin_exp (v1, v2, v3) ->
      (* TODO: Cannot find any docs on what this even is*)
      let v1 = xhp_attribute_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = xhp_attribute_expression env v3 in
      todo env (v1, v2, v3)
  | `Xhp_post_un_exp (v1, v2) ->
      (* TODO: Cannot find any docs on what this even is*)
      let v1 = xhp_attribute_expression env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (* "+" *) token env tok
        | `STAR tok -> (* "*" *) token env tok
        | `QMARK tok -> (* "?" *) token env tok
      in
      todo env (v1, v2)
  | `Xhp_paren_exp (v1, v2, v3, v4) ->
      (* TODO: Cannot find any docs on what this even is*)
      let v1 = (* "(" *) token env v1 in
      let v2 = xhp_attribute_expression env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = xhp_attribute_expression env v2 in
            v2)
          v3
      in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)

let primitive_type (env : env) (x : CST.primitive_type) =
  match x with
  | `Bool tok -> (* "bool" *) str env tok
  | `Float tok -> (* "float" *) str env tok
  | `Int tok -> (* "int" *) str env tok
  | `Str tok -> (* "string" *) str env tok
  | `Arra tok -> (* "arraykey" *) str env tok
  | `Void tok -> (* "void" *) str env tok
  | `Nonn tok -> (* "nonnull" *) str env tok
  | `Null x -> null env x
  | `Mixed tok -> (* "mixed" *) str env tok
  | `Dyna tok -> (* "dynamic" *) str env tok
  | `Nore tok -> (* "noreturn" *) str env tok

let member_modifier (env : env) (x : CST.member_modifier) =
  match x with
  | `Visi_modi x -> visibility_modifier env x
  | `Static_modi tok -> (* "static" *) G.KeywordAttr (Static, token env tok)
  | `Abst_modi tok -> (* "abstract" *) G.KeywordAttr (Abstract, token env tok)
  | `Final_modi tok -> (* "final" *) G.KeywordAttr (Final, token env tok)

let class_modifier (env : env) (x : CST.class_modifier) =
  match x with
  | `Abst_modi tok -> (* "abstract" *) G.KeywordAttr (Abstract, token env tok)
  | `Final_modi tok -> (* "final" *) G.KeywordAttr (Final, token env tok)

let xhp_enum_key (env : env) (x : CST.anon_choice_str_d42aa42) =
  match x with
  | `Str tok -> (* string *) str env tok
  | `Int tok -> (* integer *) str env tok

let literal (env : env) (x : CST.literal) : G.literal =
  match x with
  | `Str tok -> (* string *) stringify_without_quotes (str env tok)
  | `Int tok ->
      (* integer *)
      let s, tok = str env tok in
      G.Int (Parsed_int.parse (s, tok))
  | `Float tok ->
      (* float *)
      let s, tok = str env tok in
      G.Float (float_of_string_opt s, tok)
  | `True x -> G.Bool (true, true_ env x)
  | `False x -> G.Bool (false, false_ env x)
  | `Null x -> G.Null (null env x |> snd)

let namespace_identifier (env : env) (x : CST.namespace_identifier) =
  match x with
  | `Qual_id_opt_back (v1, v2) ->
      let v1 = qualified_identifier env v1 in
      let _v2 =
        match v2 with
        | Some tok -> (* "\\" *) Some (token env tok)
        | None -> None
      in
      Some v1
  (* TODO? QTop? *)
  | `Back _tok -> (* "\\" *) None

(* token env tok *)

let rec type_constant_ (env : env) ((v1, v2, v3) : CST.type_constant_) :
    G.dotted_ident =
  let v1 =
    match v1 with
    | `Qual_id x -> qualified_identifier env x
    | `Type_cst_ x -> type_constant_ env x
  in
  let _v2 = (* "::" *) token env v2 in
  let v3 = (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env v3 in
  v1 @ [ v3 ]

let trait_select_clause (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.trait_select_clause) =
  let v1 = qualified_identifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
  in
  let v4 = (* "insteadof" *) token env v4 in
  let v5 = qualified_identifier env v5 in
  let v6 =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = qualified_identifier env v2 in
        todo env (v1, v2))
      v6
  in
  todo env (v1, v2, v3, v4, v5, v6)

let xhp_close (env : env) ((v1, v2, v3) : CST.xhp_close) =
  let v1 = (* "</" *) token env v1 in
  let v2 = [ snd (xhp_identifier_ env v2) ] in
  let v3 = (* ">" *) [ token env v3 ] in
  Tok.combine_toks v1 (v2 @ v3)

let _xhp_children_declaration (env : env)
    ((v1, v2, v3, v4) : CST.xhp_children_declaration) =
  let _v1 = (* "children" *) token env v1 in
  let _v2TODO = xhp_attribute_expression env v2 in
  let _v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = xhp_attribute_expression env v2 in
        v2)
      v3
  in
  let _v4 = (* ";" *) token env v4 in
  todo_deprecation_stmt

let keyword (env : env) (x : CST.keyword) =
  match x with
  | `Type tok -> (* "type" *) str env tok
  | `Newt tok -> (* "newtype" *) str env tok
  | `Shape tok -> (* "shape" *) str env tok
  | `Tupe tok -> (* "tupe" *) str env tok
  | `Clone tok -> (* "clone" *) str env tok
  | `New tok -> (* "new" *) str env tok
  | `Print tok -> (* "print" *) str env tok
  | `Choice_bool x -> primitive_type env x
  | `Choice_array x -> collection_type env x

let xhp_enum_type (env : env) ((v1, v2, v3, v4, v5, v6) : CST.xhp_enum_type)
    ident =
  let v1 = (* "enum" *) token env v1 in
  let _v2 = (* "{" *) token env v2 in
  let v3 = G.OrEnum (xhp_enum_key env v3, None) in
  let v4 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = G.OrEnum (xhp_enum_key env v2, None) in
        v2)
      v4
  in
  let _v5 =
    match v5 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  let _v6 = (* "}" *) token env v6 in
  (* This pass-down is really wrong. Need anon type name. *)
  (* Could also make type via nested TyOr of names but that's also wrong... *)
  (* Q: Just want type, so can't do def stmt... Should really be TypeDef in parent? *)
  let def =
    G.DefStmt (G.basic_entity ident, TypeDef { tbody = OrType (v3 :: v4) })
    |> G.s
  in
  G.OtherType (("EnumAnon", v1), [ G.S def ]) |> G.t

let scoped_identifier (env : env) ((v1, v2, v3) : CST.scoped_identifier) :
    G.dotted_ident =
  let v1 =
    match v1 with
    | `Qual_id x -> qualified_identifier env x
    | `Var tok -> (* variable *) [ str env tok ]
    | `Scope_id x ->
        (* Note: scope_identifier doesn't really work here because we need to unwrap *)
        [
          (match x with
          | `Self tok -> (* "self" *) str env tok
          | `Parent tok -> (* "self" *) str env tok
          | `Static tok -> (* "self" *) str env tok);
        ]
    | `Choice_xhp_id x -> [ xhp_identifier_ env x ]
    | `Pipe_var tok -> (* "$$" *) [ str env tok ]
  in
  let _v2 = (* "::" *) token env v2 in
  let v3 =
    match v3 with
    | `Id tok ->
        (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env tok
    | `Var tok -> (* variable *) str env tok
  in
  v1 @ [ v3 ]

let _anonymous_function_use_clause (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.anonymous_function_use_clause) =
  let v1 = (* "use" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* variable *) token env v3 in
  let v4 =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = (* variable *) token env v2 in
        todo env (v1, v2))
      v4
  in
  let v5 =
    match v5 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  let v6 = (* ")" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

let use_clause (env : env) ((v1, v2, v3) : CST.use_clause) =
  (* Q: How to represent `use` type? *)
  let _v1TODO =
    match v1 with
    | Some x -> Some (use_type env x)
    | None -> None
  in
  let namespace_ident = namespace_identifier env v2 in
  let v3 : G.alias option =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = (* "as" *) token env v1 in
        let v2 =
          (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env v2
        in
        Some (v2, G.empty_id_info ())
    | None -> None
    (* TODO: Enable block to enable auto-aliasing imports to their shortened versions *)
    (* (
        match namespace_ident with
        | Some x ->
            Some (Common.hd_exn "unexpected empty list" (unwrap_qualified_identifier x), G.empty_id_info ())
        | None -> None) *)
  in
  match namespace_ident with
  | Some xs -> (Some xs, v3)
  | None -> (None, v3)

let const_declarator_id (env : env) (x : CST.anon_choice_semg_exte_id_8bbc8de) =
  match x with
  | `Semg_exte_id x -> semgrep_extended_identifier env x
  | `Choice_type x -> keyword env x

(* TODO: change how treesitter grammar does this? We think SEMI is empty stmt? *)
let rec inline_compound_statement (env : env)
    (x : CST.anon_choice_comp_stmt_c6c6bb4) =
  match x with
  | `Comp_stmt x -> compound_statement env x
  | `SEMI tok -> (* ";" *) empty_stmt env tok

and anon_choice_exp_1701d0a (env : env) (x : CST.anon_choice_exp_1701d0a) =
  match x with
  | `Exp x -> expression env x
  | `Elem_init (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = expression env v3 in
      G.Assign (v1, v2, v3) |> G.e

and anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb (env : env)
    ((v1, v2, v3) : CST.anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb)
    =
  let v1 = anon_choice_exp_1701d0a env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = anon_choice_exp_1701d0a env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  v1 :: v2

and shape_field_specifier (env : env) (x : CST.anon_choice_field_spec_0e0e023) =
  match x with
  | `Field_spec (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some tok -> (* "?" *) [ G.KeywordAttr (Optional, token env tok) ]
        | None -> []
      in
      let v2 = expression env v2 in
      let _v3 = (* "=>" *) token env v3 in
      let v4 = type_ env v4 in
      let ent : G.entity =
        { name = G.EDynamic v2; attrs = v1; tparams = None }
      in
      let def : G.variable_definition =
        {
          (* Note: This could never exist. Am I using the wrong type here? *)
          vinit = None;
          vtype = Some v4;
          vtok = G.no_sc;
        }
      in
      G.DefStmt (ent, FieldDefColon def)
  | `DOTDOTDOT tok ->
      (* "..." *)
      G.ExprStmt (G.IdSpecial (G.Spread, token env tok) |> G.e, G.sc)

(* TODO: This could be considered an abuse. Compare to native Spread associated with Fields *)
and anon_exp_rep_COMMA_exp_0bb260c (env : env)
    ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) =
  let v1 = expression env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        (* Q: Change tree-sitter to be less permissive in what's allowed here? *)
        let v2 = expression env v2 in
        v2)
      v2
  in
  v1 :: v2

and argument (env : env) (x : CST.argument) : G.argument =
  match x with
  | `Opt_choice_inout_modi_exp (v1, v2) ->
      (* TODO: Add modifier support *)
      let _v1 =
        match v1 with
        | Some x -> (
            match x with
            | `Inout_modi tok -> (* "inout" *) Some (token env tok)
            | `Vari_modi tok -> (* "..." *) Some (token env tok))
        | None -> None
      in
      let v2 = expression env v2 in
      G.Arg v2
  | `Semg_vari_id tok ->
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *)
      G.Arg (G.N (G.Id (str env tok, G.empty_id_info ())) |> G.e)

and arguments (env : env) ((v1, v2, v3) : CST.arguments) : G.arguments =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = argument env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = argument env v2 in
              v2)
            v2
        in
        let _v3 =
          match v3 with
          | Some tok -> (* "," *) Some (token env tok)
          | None -> None
        in
        v1 :: v2
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and as_expression (env : env) ((v1, v2, v3) : CST.as_expression) =
  let v1 = expression env v1 in
  let v2 =
    (* MISS: Do not capture ?as vs as *)
    match v2 with
    | `Tok_pdyn_n1_as tok -> (* as *) token env tok
    | `QMARKas tok -> (* "?as" *) token env tok
  in
  let v3 = type_ env v3 in
  G.Cast (v3, v2, v1) |> G.e

and attribute_modifier (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.attribute_modifier) : G.attribute list =
  (* Attributes are actually constructors *)
  let v1 = (* "<<" *) token env v1 in
  let v2 = qualified_identifier env v2 in
  let v3 =
    match v3 with
    | Some x -> arguments env x
    | None -> Tok.unsafe_fake_bracket []
  in
  let n = H2.name_of_ids v2 in
  let attr1 = G.NamedAttr (v1, n, v3) in
  let v4 =
    List_.map
      (fun (v1, v2, v3) ->
        let v1 = (* "," *) token env v1 in
        let v2 = qualified_identifier env v2 in
        let v3 =
          match v3 with
          | Some x -> arguments env x
          | None -> Tok.unsafe_fake_bracket []
        in
        let n = H2.name_of_ids v2 in
        G.NamedAttr (v1, n, v3))
      v4
  in
  let _v5 =
    match v5 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  let _v6 = (* ">>" *) token env v6 in
  attr1 :: v4

and binary_expression (env : env) (x : CST.binary_expression) : G.expr =
  match x with
  | `Exp_BARGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "|>" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Pipe, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "??" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Nullish, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Or, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.And, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.BitOr, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_HAT_exp (v1, v2, v3) ->
      (* Q: Comment in generic file says PHP has a xor shortcut operator as G.Xor? *)
      let v1 = expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.BitXor, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.BitAnd, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Eq, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.NotEq, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "===" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.PhysEq, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.NotPhysEq, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Lt, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Gt, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.LtE, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.GtE, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_LTEQGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "<=>" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Cmp, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.LSL, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.LSR, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Plus, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Minus, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_DOT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Concat, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Mult, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Div, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Mod, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Pow, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  | `Exp_QMARKCOLON_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "?:" *) token env v2 in
      let v3 = expression env v3 in
      G.Call
        ( G.IdSpecial (G.Op G.Elvis, v2) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1; G.Arg v3 ] )
      |> G.e
  (* These are all assignment below.
     TODO: Consider splitting out in grammar.
     Q: Does this overlap with VarDef? *)
  | `Exp_EQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = expression env v3 in
      G.Assign (v1, v2, v3) |> G.e
  | `Exp_QMARKQMARKEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "??=" *) (token env v2, G.Nullish) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e
  | `Exp_DOTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* ".=" *) (token env v2, G.Concat) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e
  | `Exp_BAREQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "|=" *) (token env v2, G.BitOr) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e
  | `Exp_HATEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "^=" *) (token env v2, G.BitXor) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e
  | `Exp_AMPEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "&=" *) (token env v2, G.BitAnd) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e
  | `Exp_LTLTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "<<=" *) (token env v2, G.LSL) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e
  | `Exp_GTGTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* ">>=" *) (token env v2, G.LSR) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e
  | `Exp_PLUSEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "+=" *) (token env v2, G.Plus) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e
  | `Exp_DASHEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "-=" *) (token env v2, G.Minus) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e
  | `Exp_STAREQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "*=" *) (token env v2, G.Mult) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e
  | `Exp_SLASHEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "/=" *) (token env v2, G.Div) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e
  | `Exp_PERCEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "%=" *) (token env v2, G.Mod) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e
  | `Exp_STARSTAREQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2, op = (* "**=" *) (token env v2, G.Pow) in
      let v3 = expression env v3 in
      G.AssignOp (v1, (op, v2), v3) |> G.e

and braced_expression (env : env) ((v1, v2, v3) : CST.braced_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 = expression env v2 in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and call_expression (env : env) ((v1, v2, v3) : CST.call_expression) =
  let v1 =
    match v1 with
    | `Exp x -> expression env x
    | `Choice_array x ->
        G.N (G.Id (collection_type env x, G.empty_id_info ())) |> G.e
  in
  let _v2TODO =
    match v2 with
    | Some x -> type_arguments env x
    | None -> None
  in
  let v3 = arguments env v3 in
  G.Call (v1, v3) |> G.e

and catch_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let _v2 = (* "(" *) token env v2 in
  let v3 = type_ env v3 in
  let v4 = (* variable *) Some (str env v4) in
  let _v5 = (* ")" *) token env v5 in
  let v6 = compound_statement env v6 in
  let exn = G.CatchParam (G.param_of_type v3 ?pname:v4) in
  (v1, exn, v6)

and class_const_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.class_const_declaration) =
  let v1 = List_.map (member_modifier env) v1 in
  let v2 = (* "const" *) [ G.KeywordAttr (Const, token env v2) ] in
  let attrs = v1 @ v2 in
  let type_ =
    match v3 with
    | Some x -> Some (type_ env x)
    | None -> None
  in
  let _v6 = (* ";" *) token env v6 in
  let v4 = G.F (class_const_declarator env v4 attrs type_) in
  let v5 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = G.F (class_const_declarator env v2 attrs type_) in
        v2)
      v5
  in
  v4 :: v5

and class_const_declarator (env : env) ((v1, v2) : CST.class_const_declarator)
    attrs vtype =
  (* TODO: Verify we are properly handling this (esp. when without assignment) *)
  let v1 = const_declarator_id env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let _v1 = (* "=" *) token env v1 in
        let v2 = expression env v2 in
        Some v2
    | None -> None
  in
  let ent = G.basic_entity v1 ~attrs in
  let def : G.variable_definition = { vinit = v2; vtype; vtok = G.no_sc } in
  G.DefStmt (ent, G.VarDef def) |> G.s

and compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) :
    G.stmt =
  let v1 = (* "{" *) token env v1 in
  let v2 = List_.map (statement env) v2 in
  let v3 = (* "}" *) token env v3 in
  G.Block (v1, v2, v3) |> G.s

and const_declarator (env : env) ((v1, v2, v3) : CST.const_declarator) attrs
    vtype =
  let v1 = const_declarator_id env v1 in
  let _v2 = (* "=" *) token env v2 in
  let v3 = expression env v3 in
  G.DefStmt
    (G.basic_entity v1 ~attrs, VarDef { vinit = Some v3; vtype; vtok = G.no_sc })

and declaration (env : env) (x : CST.declaration) =
  match x with
  | `Func_decl (v1, v2, v3) ->
      let attrs =
        match v1 with
        | Some x -> attribute_modifier env x
        | None -> []
      in
      let func_def, identifier, type_params =
        function_declaration_header env v2
      in
      let compound_statement = inline_compound_statement env v3 in
      let def = { func_def with fbody = G.FBStmt compound_statement } in
      let ent = basic_typed_entity identifier attrs type_params in
      G.DefStmt (ent, G.FuncDef def)
  | `Class_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) ->
      let v1 =
        match v1 with
        | Some x -> attribute_modifier env x
        | None -> []
      in
      let v2 =
        match v2 with
        | Some x -> [ class_modifier env x ]
        | None -> []
      in
      let v3 =
        match v3 with
        | Some x -> [ class_modifier env x ]
        | None -> []
      in
      let v4 =
        match v4 with
        | Some tok ->
            (* "xhp" *)
            let ((_, tok) as ident) = str env tok in
            [
              G.NamedAttr
                ( fk tok,
                  Id (ident, G.empty_id_info ()),
                  Tok.unsafe_fake_bracket [] );
            ]
        | None -> []
      in
      let v5 = (* "class" *) token env v5 in
      let id =
        match v6 with
        | `Semg_exte_id x -> semgrep_extended_identifier env x
        | `Choice_xhp_id x -> xhp_identifier_ env x
      in
      let type_params = Option.map (type_parameters env) v7 in
      let v8 =
        match v8 with
        | Some x -> extends_clause env x
        | None -> []
      in
      let v9 =
        match v9 with
        | Some x -> implements_clause env x
        | None -> []
      in
      let v10 =
        (* Q: What does this even do? *)
        match v10 with
        | Some x -> where_clause env x
        | None -> []
      in
      let v11 = member_declarations env v11 in
      let def : G.class_definition =
        {
          ckind = (G.Class, v5);
          cextends = v8;
          cimplements = v9;
          cmixins = v10;
          cparams = fb [];
          cbody = v11;
        }
      in
      let attrs = v1 @ v2 @ v3 @ v4 in
      G.DefStmt (basic_typed_entity id attrs type_params, G.ClassDef def)
  | `Inte_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | Some x -> attribute_modifier env x
        | None -> []
      in
      let v2 = (* "interface" *) token env v2 in
      let id = semgrep_extended_identifier env v3 in
      let type_params = Option.map (type_parameters env) v4 in
      let v5 =
        match v5 with
        | Some x -> extends_clause env x
        | None -> []
      in
      let v6 =
        match v6 with
        | Some x -> where_clause env x
        | None -> []
      in
      let v7 = member_declarations env v7 in
      let def : G.class_definition =
        {
          ckind = (G.Interface, v2);
          cextends = v5;
          cimplements = [];
          cmixins = v6;
          cparams = fb [];
          cbody = v7;
        }
      in
      let attrs = v1 in
      G.DefStmt (basic_typed_entity id attrs type_params, G.ClassDef def)
  | `Trait_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | Some x -> attribute_modifier env x
        | None -> []
      in
      let v2 = (* "trait" *) token env v2 in
      let id = semgrep_extended_identifier env v3 in
      let type_params = Option.map (type_parameters env) v4 in
      let v5 =
        match v5 with
        | Some x -> implements_clause env x
        | None -> []
      in
      let v6 =
        match v6 with
        | Some x -> where_clause env x
        | None -> []
      in
      let v7 = member_declarations env v7 in
      let def : G.class_definition =
        {
          ckind = (G.Trait, v2);
          cextends = [];
          cimplements = v5;
          cmixins = v6;
          cparams = fb [];
          cbody = v7;
        }
      in
      let attrs = v1 in
      G.DefStmt (basic_typed_entity id attrs type_params, G.ClassDef def)
  | `Alias_decl (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        match v1 with
        | Some x -> attribute_modifier env x
        | None -> []
      in
      let _v2 =
        match v2 with
        | `Type tok -> (* "type" *) token env tok
        | `Newt tok -> (* "newtype" *) token env tok
      in
      let type_params = Option.map (type_parameters env) v4 in
      (* Q: Type params vs type attributes in generic? Which to use here?
         Put within Name or pass to Apply?*)
      let id = semgrep_extended_identifier env v3 in
      let v5 =
        match v5 with
        (* TODO: Handle type constraint. Just an attr? *)
        | Some (v1, v2) ->
            let v1 = (* "as" *) str env v1 in
            let v2 = type_ env v2 in
            [ G.OtherAttribute (v1, [ G.T v2 ]) ]
        | None -> []
      in
      (* Q: AliasType vs NewType? *)
      let _v6 = (* "=" *) token env v6 in
      let v7 = type_ env v7 in
      let _v8 = (* ";" *) token env v8 in
      G.DefStmt
        ( basic_typed_entity id (v1 @ v5) type_params,
          G.TypeDef { tbody = AliasType v7 } )
  | `Enum_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 =
        match v1 with
        | Some x -> attribute_modifier env x
        | None -> []
      in
      let _v2 = (* "enum" *) token env v2 in
      let v3 = semgrep_extended_identifier env v3 in
      let _v4 = (* ":" *) token env v4 in
      let _v5TODO = type_ env v5 in
      let _v6 =
        match v6 with
        (* TODO: Use this type constraint... *)
        | Some (v1, v2) ->
            let _v1 = (* "as" *) token env v1 in
            let v2 = Some (type_ env v2) in
            v2
        | None -> None
      in
      let _v7 = (* "{" *) token env v7 in
      let v8 = List_.map (enumerator env) v8 in
      let _v9 = (* "}" *) token env v9 in
      G.DefStmt (G.basic_entity v3 ~attrs:v1, TypeDef { tbody = G.OrType v8 })
  | `Name_decl (v1, v2) -> (
      let v1 = (* "namespace" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `Qual_id_SEMI (v1, v2) ->
                let v1 = qualified_identifier env v1 in
                let _v2 = (* ";" *) token env v2 in
                Some v1
            | `Opt_qual_id_comp_stmt (v1, v2) ->
                let v1 =
                  match v1 with
                  | Some x -> Some (qualified_identifier env x)
                  | None -> None
                in
                (* TODO: Handle namespace with block inside *)
                let _v2TODO = compound_statement env v2 in
                v1)
        | None -> None
      in
      match v2 with
      | Some v2 -> G.DirectiveStmt (G.Package (v1, v2) |> G.d)
      (* TODO: I think this is wrong and PackageEnd should instead be used to handle namespaces with block inside? But how? *)
      | None -> G.DirectiveStmt (G.PackageEnd v1 |> G.d))
  | `Const_decl (v1, v2, v3, v4, v5) ->
      let attr = (* "const" *) [ G.KeywordAttr (Const, token env v1) ] in
      let type_ =
        match v2 with
        | Some x -> Some (type_ env x)
        | None -> None
      in
      let v3 = const_declarator env v3 attr type_ |> G.s in
      let v4 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = const_declarator env v2 attr type_ |> G.s in
            v2)
          v4
      in
      let _v5 = (* ";" *) token env v5 in
      (* TODO: Refactor parent to allow flattening *)
      G.Block (Tok.unsafe_fake_bracket (v3 :: v4))

and embedded_brace_expression (env : env)
    ((v1, v2) : CST.embedded_brace_expression) =
  let v1 = embedded_brace_expression_ env v1 in
  let _v2 = (* "}" *) token env v2 in
  v1

and embedded_brace_expression_ (env : env) (x : CST.embedded_brace_expression_)
    =
  match x with
  | `Tok_lcur_pat_0e8e4b6 tok ->
      (* TODO: Will this have extra leading `{`? *)
      (* tok_lcurldollar_pat_0e8e4b6 *)
      G.N (Id (str env tok, G.empty_id_info ())) |> G.e
  | `Embe_brace_call_exp (v1, v2) ->
      let v1 = embedded_brace_expression_ env v1 in
      let v2 = arguments env v2 in
      G.Call (v1, v2) |> G.e
  | `Embe_brace_subs_exp (v1, v2, v3, v4) ->
      let v1 = embedded_brace_expression_ env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> expression env x
        | None -> G.OtherExpr (("ArrayAppend", v2), []) |> G.e
      in
      let v4 = (* "]" *) token env v4 in
      G.ArrayAccess (v1, (v2, v3, v4)) |> G.e
  | `Embe_brace_sele_exp (v1, v2, v3) -> (
      let v1 = embedded_brace_expression_ env v1 in
      let v2 = selection_expression_selector env v2 in
      let v3 = variablish env v3 in
      (* TODO: The TSH grammar improperly makes the first item in the
         selection expression at the top level. This breaks support for ellipsis
         because items aren't properly nested.
         See dots_method_chaining.hack *)
      match v3.e with
      | G.Ellipsis dots -> G.DotAccessEllipsis (v1, dots) |> G.e
      | _ -> G.DotAccess (v1, v2, G.FDynamic v3) |> G.e)

and enumerator (env : env) ((v1, v2, v3, v4) : CST.enumerator) =
  let v1 = semgrep_extended_identifier env v1 in
  let _v2 = (* "=" *) token env v2 in
  let v3 = expression env v3 in
  let _v4 = (* ";" *) token env v4 in
  G.OrEnum (v1, Some v3)

and expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Choice_here x -> (
      match x with
      | `Here (v1, v2, v3, v4, v5, v6) ->
          let v1 = (* "<<<" *) token env v1 in
          let v2 = (* heredoc_start *) Tok.combine_toks v1 [ token env v2 ] in
          let heredoc_start =
            match v3 with
            | Some tok ->
                (* heredoc_start_newline *)
                Tok.combine_toks v2 [ token env tok ]
            | None -> v2
          in
          let v4 =
            List_.map
              (fun x ->
                G.Arg
                  (match x with
                  | `Here_body tok ->
                      (* heredoc_body *)
                      G.L (G.String (fb (str env tok))) |> G.e
                  | `Var tok ->
                      (* variable *)
                      G.N (Id (str env tok, G.empty_id_info ())) |> G.e
                  | `Embe_brace_exp x -> embedded_brace_expression env x))
              v4
          in
          let v6 = (* heredoc_end *) token env v6 in
          let heredoc_end =
            match v5 with
            | Some tok ->
                (* heredoc_end_newline *) Tok.combine_toks v6 [ token env tok ]
            | None -> v6
          in
          G.Call
            ( G.IdSpecial (ConcatString InterpolatedConcat, fk v6) |> G.e,
              (heredoc_start, v4, heredoc_end) )
          |> G.e
      | `Array (v1, v2, v3, v4, v5) ->
          let _collectionTODO = collection_type env v1 in
          let _v2TODO =
            match v2 with
            | Some x -> type_arguments env x
            | None -> None
          in
          let v3 = (* "[" *) token env v3 in
          let v4 =
            match v4 with
            | Some x ->
                anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb env x
            | None -> []
          in
          let v5 = (* "]" *) token env v5 in
          let collection_type =
            match v1 with
            | `Array _tok -> (* "array" *) G.Dict
            | `Varray _tok -> (* "varray" *) G.Array
            | `Darray _tok -> (* "darray" *) G.Dict
            | `Vec _tok -> (* "vec" *) G.Array
            | `Dict _tok -> (* "dict" *) G.Dict
            | `Keyset _tok -> (* "keyset" *) G.Set
          in
          G.Container (collection_type, (v3, v4, v5)) |> G.e
      | `Tuple (v1, v2, v3, v4) ->
          (* Note: Purposefully not using Tuple so hope that's correct *)
          let _v1 = (* "tuple" *) token env v1 in
          let v2 = (* "(" *) token env v2 in
          let v3 =
            match v3 with
            | Some (v1, v2, v3) ->
                let v1 = expression env v1 in
                let v2 =
                  List_.map
                    (fun (v1, v2) ->
                      let _v1 = (* "," *) token env v1 in
                      let v2 = expression env v2 in
                      v2)
                    v2
                in
                let _v3 =
                  match v3 with
                  | Some tok -> (* "," *) Some (token env tok)
                  | None -> None
                in
                v1 :: v2
            | None -> []
          in
          let v4 = (* ")" *) token env v4 in
          G.Container (List, (v2, v3, v4)) |> G.e
      | `Shape (v1, v2, v3, v4) ->
          (* Q: Is Shape is just a DArray, should it still be treated like a Record? *)
          let _v1 = (* "shape" *) token env v1 in
          let v2 = (* "(" *) token env v2 in
          let v3 =
            match v3 with
            | Some (v1, v2, v3) ->
                let v1 = G.F (field_initializer env v1 |> G.s) in
                let v2 =
                  List_.map
                    (fun (v1, v2) ->
                      let _v1 = (* "," *) token env v1 in
                      let v2 = G.F (field_initializer env v2 |> G.s) in
                      v2)
                    v2
                in
                let _v3 =
                  match v3 with
                  | Some tok -> (* "," *) Some (token env tok)
                  | None -> None
                in
                v1 :: v2
            | None -> []
          in
          let v4 = (* ")" *) token env v4 in
          G.Record (v2, v3, v4) |> G.e
      | `Coll (v1, v2, v3, v4) ->
          let _v1TODO = qualified_identifier env v1 in
          let v2 = (* "{" *) token env v2 in
          let v3 =
            match v3 with
            | Some x ->
                anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb env x
            | None -> []
          in
          let v4 = (* "}" *) token env v4 in
          (* TODO: Include identifier *)
          G.Container (Dict, (v2, v3, v4)) |> G.e
          (* Q: Is this the correct way to handle? Is Expr, so can't use VarDef
             But are we actually creating a var? *)
      | `Choice_str x -> G.L (literal env x) |> G.e
      | `Choice_var x -> variablish env x
      | `Pref_str (v1, v2) ->
          let v1 =
            (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env v1
          in
          let v2 = (* string *) str env v2 in
          (* Q: This feels weird with the fst and snd *)
          G.Call
            ( G.IdSpecial (EncodedString (fst v1), snd v1) |> G.e,
              Tok.unsafe_fake_bracket
                [ G.Arg (G.L (stringify_without_quotes v2) |> G.e) ] )
          |> G.e
      | `Paren_exp x -> parenthesized_expression env x
      | `Bin_exp x -> binary_expression env x
      | `Prefix_un_exp x -> prefix_unary_expression env x
      | `Post_un_exp (v1, v2) ->
          let v1 = expression env v1 in
          let v2, op =
            match v2 with
            | `PLUSPLUS tok -> (* "++" *) (token env tok, G.Incr)
            | `DASHDASH tok -> (* "--" *) (token env tok, G.Decr)
          in
          G.Call
            ( G.IdSpecial (IncrDecr (op, Postfix), v2) |> G.e,
              Tok.unsafe_fake_bracket [ G.Arg v1 ] )
          |> G.e
      | `Is_exp (v1, v2, v3) ->
          let v1 = expression env v1 in
          let v2 = (* "is" *) token env v2 in
          let v3 = type_ env v3 in
          G.Call
            ( G.IdSpecial (Op Is, v2) |> G.e,
              Tok.unsafe_fake_bracket [ G.Arg v1; G.ArgType v3 ] )
          |> G.e
      | `As_exp x -> as_expression env x
      | `Awai_exp (v1, v2) ->
          (* This is awaitable block, not just await keyword *)
          (* Q: How to handle this and concurrent? *)
          let _v1TODO = (* "async" *) token env v1 in
          let v2 = G.stmt_to_expr (compound_statement env v2) in
          (* TODO: This can't possibly be right *)
          G.stmt_to_expr (G.OtherStmt (OS_Async, [ G.E v2 ]) |> G.s)
      | `Yield_exp (v1, v2) ->
          let v1 = (* "yield" *) token env v1 in
          let v2 = anon_choice_exp_1701d0a env v2 in
          G.Yield (v1, Some v2, true) |> G.e
          (* Q: What is this last field for? *)
      | `Cast_exp (v1, v2, v3, v4) ->
          (* Q: Should I really be using G.Cast here? *)
          let v1 = (* "(" *) token env v1 in
          let v2 =
            match v2 with
            | `Array tok -> (* "array" *) G.ty_builtin (str env tok)
            | `Int tok -> (* "int" *) G.ty_builtin (str env tok)
            | `Float tok -> (* "float" *) G.ty_builtin (str env tok)
            | `Str tok -> (* "string" *) G.ty_builtin (str env tok)
            | `Bool tok -> (* "bool" *) G.ty_builtin (str env tok)
          in
          let _v3 = (* ")" *) token env v3 in
          let v4 = expression env v4 in
          G.Cast (v2, v1, v4) |> G.e
      | `Tern_exp (v1, v2, v3, v4, v5) ->
          let v1 = expression env v1 in
          let _v2 = (* "?" *) token env v2 in
          let v3 = expression env v3 in
          let _v4 = (* ":" *) token env v4 in
          let v5 = expression env v5 in
          G.Conditional (v1, v3, v5) |> G.e
      | `Lambda_exp (v1, v2, v3, v4, v5) ->
          let _v1TODO =
            match v1 with
            | Some x -> attribute_modifier env x
            | None -> []
          in
          let _v2TODO =
            match v2 with
            | Some tok ->
                (* "async" *) [ G.KeywordAttr (G.Async, token env tok) ]
            | None -> []
          in
          let fparams, return_type =
            match v3 with
            | `Single_param_params tok ->
                (* variable *)
                (fb [ G.Param (G.param_of_id (str env tok)) ], None)
            | `Params_opt_COLON_choice_type_spec (v1, v2) ->
                let v1 = parameters env v1 in
                let v2 =
                  match v2 with
                  | Some (v1, v2) ->
                      let _v1 = (* ":" *) token env v1 in
                      let v2 = type_ env v2 in
                      Some v2
                  | None -> None
                in
                (v1, v2)
          in
          let v4 = (* "==>" *) token env v4 in
          let v5 =
            match v5 with
            | `Exp x -> G.FBExpr (expression env x)
            | `Comp_stmt x -> G.FBStmt (compound_statement env x)
          in
          let def : G.function_definition =
            {
              fkind = (G.LambdaKind, v4);
              (* Q: Is the arrow the token here? Arrow vs LambdaKind? *)
              fparams;
              frettype = return_type;
              fbody = v5;
            }
          in
          G.Lambda def |> G.e
      | `Call_exp x -> call_expression env x
      | `Sele_exp x -> selection_expression env x
      | `New_exp (v1, v2, v3, v4) ->
          let tnew = (* "new" *) token env v1 in
          let ty = H2.expr_to_type (variablish env v2) in
          let _v3TODO =
            match v3 with
            | Some x -> type_arguments env x
            | None -> None
          in
          let v4 = arguments env v4 in
          G.New (tnew, ty, G.empty_id_info (), v4) |> G.e
      | `Incl_exp (v1, v2) ->
          (* Q: See question below in Requ *)
          let v1 =
            match v1 with
            | `Incl tok -> (* "include" *) str env tok
            | `Incl_once tok -> (* "include_once" *) str env tok
          in
          let v2 = expression env v2 in
          G.OtherExpr (v1, [ G.E v2 ]) |> G.e
      | `Requ_exp (v1, v2) ->
          (* Q: What makes this an expression and not a directive statement? *)
          let v1 =
            match v1 with
            | `Requ tok -> (* "require" *) str env tok
            | `Requ_once tok -> (* "require_once" *) str env tok
          in
          let v2 = expression env v2 in
          G.OtherExpr (v1, [ G.E v2 ]) |> G.e
      | `Anon_func_exp (v1, v2, v3, v4, v5, v6) ->
          (* TODO: Anon is not the same as Lambda. These are PHP style. *)
          let _v1TODO =
            match v1 with
            | Some tok ->
                (* "async" *) [ G.KeywordAttr (G.Async, token env tok) ]
            | None -> []
          in
          let v2 = (* "function" *) token env v2 in
          let v3 = parameters env v3 in
          let v4 =
            match v4 with
            | Some (v1, v2) ->
                let _v1 = (* ":" *) token env v1 in
                let v2 = type_ env v2 in
                Some v2
            | None -> None
          in
          let _v5TODO =
            match v5 with
            (* TODO: No way to capture this in generic *)
            | Some _x -> None (* Some (anonymous_function_use_clause env x) *)
            | None -> None
          in
          let v6 = compound_statement env v6 in
          let def : G.function_definition =
            {
              fkind = (G.LambdaKind, v2);
              fparams = v3;
              frettype = v4;
              fbody = G.FBStmt v6;
            }
          in
          G.Lambda def |> G.e
      | `Xhp_exp x -> G.Xml (xhp_expression env x) |> G.e)
  | `Ellips tok -> (* "..." *) G.Ellipsis (token env tok) |> G.e
  | `Deep_ellips (v1, v2, v3) ->
      let v1 = (* "<..." *) token env v1 in
      let v2 = expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      G.DeepEllipsis (v1, v2, v3) |> G.e

and expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = expression env v1 in
  let v2 = (* ";" *) token env v2 in
  G.ExprStmt (v1, v2)

and extends_clause (env : env) ((v1, v2, v3) : CST.extends_clause) :
    G.class_parent list =
  let _v1 = (* "extends" *) token env v1 in
  let v2 = type_ env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = type_ env v2 in
        v2)
      v3
  in
  v2 :: v3 |> List_.map (fun ty -> (ty, None))

and field_initializer (env : env) ((v1, v2, v3) : CST.field_initializer) =
  let v1 =
    match v1 with
    | `Str tok -> (* string *) G.basic_entity (str env tok)
    | `Scoped_id x ->
        let x = scoped_identifier env x in
        { name = G.EN (H2.name_of_ids x); attrs = []; tparams = None }
  in
  let _v2 = (* "=>" *) token env v2 in
  let v3 = expression env v3 in
  let def : G.variable_definition =
    {
      vtype = None;
      (* Do we want to represent that this must be a string? *)
      vinit = Some v3;
      vtok = G.no_sc;
    }
  in
  G.DefStmt (v1, FieldDefColon def)

and finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = compound_statement env v2 in
  (v1, v2)

and function_declaration_header (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.function_declaration_header) :
    G.function_definition * G.label * G.type_parameters option =
  let _async_modifierTODO =
    match v1 with
    | Some tok -> (* "async" *) Some (G.KeywordAttr (G.Async, token env tok))
    | None -> None
  in
  let function_keyword = (* "function" *) token env v2 in
  let identifier = semgrep_extended_identifier env v3 in
  let type_params = Option.map (type_parameters env) v4 in
  let parameters = parameters env v5 in
  let attribute_modifier_and_return_type =
    match v6 with
    | Some (v1, v2, v3) ->
        let _v1 = (* ":" *) token env v1 in
        let _v2TODO =
          match v2 with
          | Some x -> attribute_modifier env x
          | None -> []
        in
        let v3 = type_ env v3 in
        Some v3
    | None -> None
  in
  let _where_clauseTODO =
    match v7 with
    | Some x -> where_clause env x
    | None -> []
  in
  ( {
      fkind = (G.Function, function_keyword);
      fparams = parameters;
      frettype = attribute_modifier_and_return_type;
      fbody =
        G.FBDecl G.sc
        (* To be replaced in parent with real statement. Could also replace with passthrough strategy *);
    },
    identifier,
    type_params )

and implements_clause (env : env) ((v1, v2, v3) : CST.implements_clause) =
  let _v1 = (* "implements" *) token env v1 in
  let v2 = type_ env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = type_ env v2 in
        v2)
      v3
  in
  v2 :: v3

and member_declarations (env : env) ((v1, v2, v3) : CST.member_declarations) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    List_.map
      (fun x ->
        match x with
        | `Class_const_decl x -> class_const_declaration env x
        | `Meth_decl x -> [ G.F (method_declaration env x |> G.s) ]
        | `Prop_decl x -> property_declaration env x
        | `Type_const_decl x -> [ G.F (type_const_declaration env x) ]
        (* TODO: Add Trait use support *)
        | `Trait_use_clause _xTODO ->
            [ (*  G.FieldStmt (trait_use_clause env x |> G.s) *) ]
        | `Requ_imples_clause x ->
            [ G.F (require_implements_clause env x |> G.s) ]
        | `Requ_extends_clause x ->
            [ G.F (require_extends_clause env x |> G.s) ]
        | `Xhp_attr_decl x -> xhp_attribute_declaration env x
        | `Xhp_chil_decl _xTODO ->
            (* TODO: Figure out what this even is *)
            [ (* G.FieldStmt (xhp_children_declaration env x |> G.s) *) ]
        | `Xhp_cate_decl x -> [ G.F (xhp_category_declaration env x |> G.s) ]
        | `Ellips tok ->
            let tok = token env tok in
            (* "..." *)
            [ G.field_ellipsis tok ])
      v2
  in
  let v3 = (* "}" *) token env v3 in
  (v1, List_.flatten v2, v3)

and method_declaration (env : env) ((v1, v2, v3, v4) : CST.method_declaration) =
  let v1 =
    match v1 with
    | Some x -> attribute_modifier env x
    | None -> []
  in
  let v2 = List_.map (member_modifier env) v2 in
  let func_def, identifier, type_args = function_declaration_header env v3 in
  let v4 = inline_compound_statement env v4 in
  let def = { func_def with fbody = G.FBStmt v4 } in
  let ent = basic_typed_entity identifier (v1 @ v2) type_args in
  G.DefStmt (ent, G.FuncDef def)

and parameter (env : env) (x : CST.parameter) : G.parameter =
  match x with
  | `Opt_attr_modi_opt_visi_modi_opt_inout_modi_opt_choice_type_spec_opt_vari_modi_var_opt_EQ_exp
      (v1, v2, v3, v4, v5, v6, v7) -> (
      let v1 =
        match v1 with
        | Some x -> attribute_modifier env x
        | None -> []
      in
      let v2 =
        match v2 with
        | Some x -> [ visibility_modifier env x ]
        | None -> []
      in
      let v3 =
        (* Q: Mutable? This is the best keyword I could think of. *)
        match v3 with
        | Some tok -> (* "inout" *) [ G.KeywordAttr (Mutable, token env tok) ]
        | None -> []
      in
      let v4 =
        match v4 with
        | Some x -> Some (type_ env x)
        | None -> None
      in
      let v5 =
        match v5 with
        | Some tok -> (* "..." *) Some (token env tok)
        | None -> None
      in
      let v6 = (* variable *) str env v6 in
      let v7 =
        match v7 with
        | Some (v1, v2) ->
            let _v1 = (* "=" *) token env v1 in
            let v2 = expression env v2 in
            Some v2
        | None -> None
      in
      let param : G.parameter_classic =
        {
          pname = Some v6;
          ptype = v4;
          pdefault = v7;
          pattrs = v1 @ v2 @ v3;
          pinfo =
            G.basic_id_info (Parameter, G.SId.unsafe_default)
            (* Q: But why sid_TODO? Like what is this info? *);
        }
      in
      match v5 with
      | Some tok -> (* "..." *) G.ParamRest (tok, param)
      | None -> G.Param param)
  | `Ellips tok -> (* "..." *) G.ParamEllipsis (token env tok)

and parameters (env : env) ((v1, v2, v3) : CST.parameters) : G.parameters =
  let lp = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `Vari_modi tok ->
            (* "..." *)
            let empty_param =
              {
                G.pattrs = [];
                pinfo = G.empty_id_info ();
                ptype = None;
                pname = None;
                pdefault = None;
              }
            in
            if env.extra =*= Pattern then [ G.ParamEllipsis (token env tok) ]
              (* Q: TODO: Even though not from Semgrep, can I use ParamEllipsis here? *)
            else [ G.ParamRest (token env tok, empty_param) ]
        | `Param_rep_COMMA_param_opt_COMMA (v1, v2, v3) ->
            let v1 = parameter env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = parameter env v2 in
                  v2)
                v2
            in
            let _v3 =
              match v3 with
              | Some tok -> (* "," *) Some (token env tok)
              | None -> None
            in
            v1 :: v2)
    | None -> []
  in
  let rp = (* ")" *) token env v3 in
  (lp, v2, rp)

and parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) =
  let _v1 = (* "(" *) token env v1 in
  let v2 = expression env v2 in
  let _v3 = (* ")" *) token env v3 in
  (* Q: Do something better here? *)
  v2

and prefix_unary_expression (env : env) (x : CST.prefix_unary_expression) =
  match x with
  | `BANG_exp (v1, v2) ->
      let v1 = (* "!" *) token env v1 in
      let v2 = expression env v2 in
      G.Call
        (G.IdSpecial (G.Op Not, v1) |> G.e, Tok.unsafe_fake_bracket [ G.Arg v2 ])
      |> G.e
  | `TILDE_exp (v1, v2) ->
      let v1 = (* "~" *) token env v1 in
      let v2 = expression env v2 in
      G.Call
        ( G.IdSpecial (G.Op BitNot, v1) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v2 ] )
      |> G.e
  | `DASH_exp (v1, v2) ->
      let v1 = (* "-" *) token env v1 in
      let v2 = expression env v2 in
      G.Call
        ( G.IdSpecial (G.Op Minus, v1) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v2 ] )
      |> G.e
  | `PLUS_exp (v1, v2) ->
      let v1 = (* "+" *) token env v1 in
      let v2 = expression env v2 in
      G.Call
        ( G.IdSpecial (G.Op Plus, v1) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v2 ] )
      |> G.e
  | `PLUSPLUS_exp (v1, v2) ->
      let v1 = (* "++" *) token env v1 in
      let v2 = expression env v2 in
      G.Call
        ( G.IdSpecial (IncrDecr (Incr, Prefix), v1) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v2 ] )
      |> G.e
  | `DASHDASH_exp (v1, v2) ->
      let v1 = (* "--" *) token env v1 in
      let v2 = expression env v2 in
      G.Call
        ( G.IdSpecial (IncrDecr (Decr, Prefix), v1) |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v2 ] )
      |> G.e
  | `Print_exp (v1, v2) ->
      let v1 = (* "print" *) str env v1 in
      let v2 = expression env v2 in
      let id = G.N (G.Id (v1, G.empty_id_info ())) |> G.e in
      (* Q: Is this the right way to handle? Same for clone *)
      G.Call (id, Tok.unsafe_fake_bracket [ G.Arg v2 ]) |> G.e
  | `Clone_exp (v1, v2) ->
      let v1 = (* "clone" *) str env v1 in
      let v2 = expression env v2 in
      let id = G.N (G.Id (v1, G.empty_id_info ())) |> G.e in
      G.Call (id, Tok.unsafe_fake_bracket [ G.Arg v2 ]) |> G.e
  | `Await_exp (v1, v2) ->
      let v1 = (* "await" *) token env v1 in
      let v2 = expression env v2 in
      G.Await (v1, v2) |> G.e
  | `AT_exp (v1, v2) ->
      (* Silences errors *)
      let v1 = (* "@" *) token env v1 in
      let v2 = expression env v2 in
      (* TODO: Is this good? *)
      G.OtherExpr (("AtExpr", v1), [ G.E v2 ]) |> G.e

and property_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.property_declaration) =
  let v1 =
    match v1 with
    | Some x -> attribute_modifier env x
    | None -> []
  in
  let v2 = List_.map (member_modifier env) v2 in
  let attrs = v1 @ v2 in
  let type_ =
    match v3 with
    | Some x -> Some (type_ env x)
    | None -> None
  in
  let v4 = G.F (property_declarator env v4 attrs type_) in
  let v5 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = G.F (property_declarator env v2 attrs type_) in
        v2)
      v5
  in
  let _v6 = (* ";" *) token env v6 in
  v4 :: v5

and property_declarator (env : env) ((v1, v2) : CST.property_declarator) attrs
    vtype =
  let v1 = (* variable *) str env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let _v1 = (* "=" *) token env v1 in
        let v2 = expression env v2 in
        Some v2
    | None -> None
  in
  let ent = G.basic_entity v1 ~attrs in
  let def : G.variable_definition = { vinit = v2; vtype; vtok = G.no_sc } in
  G.DefStmt (ent, G.VarDef def) |> G.s

and require_extends_clause (env : env)
    ((v1, v2, v3, v4, v5) : CST.require_extends_clause) =
  let v1 = (* "require" *) [ G.TodoK (str env v1) ] in
  let v2 = (* "extends" *) [ G.TodoK (str env v2) ] in
  let v3 = [ G.T (type_ env v3) ] in
  let v4 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = G.T (type_ env v2) in
        v2)
      v4
  in
  let _v5 = (* ";" *) token env v5 in
  G.OtherStmt (OS_Todo, v1 @ v2 @ v3 @ v4)

and require_implements_clause (env : env)
    ((v1, v2, v3, v4, v5) : CST.require_implements_clause) =
  let v1 = (* "require" *) [ G.TodoK (str env v1) ] in
  let v2 = (* "implements" *) [ G.TodoK (str env v2) ] in
  let v3 = [ G.T (type_ env v3) ] in
  let v4 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = G.T (type_ env v2) in
        v2)
      v4
  in
  let _v5 = (* ";" *) token env v5 in
  G.OtherStmt (OS_Todo, v1 @ v2 @ v3 @ v4)

and selection_expression (env : env) ((v1, v2, v3) : CST.selection_expression) =
  let v1 =
    match v1 with
    | `Choice_var x -> variablish env x
    | `As_exp x -> as_expression env x
  in
  let v2 = selection_expression_selector env v2 in
  let v3 =
    match v3 with
    | `Choice_var x -> variablish env x
    | `Braced_exp x -> Tok.unbracket (braced_expression env x)
    | `Choice_type x -> G.N (G.Id (keyword env x, G.empty_id_info ())) |> G.e
  in
  match v3.e with
  | G.Ellipsis dots -> G.DotAccessEllipsis (v1, dots) |> G.e
  | _ -> G.DotAccess (v1, v2, G.FDynamic v3) |> G.e

and statement (env : env) (x : CST.statement) =
  match x with
  | `Choice_func_decl x -> declaration env x |> G.s
  | `Comp_stmt x -> compound_statement env x
  | `Empty_stmt x -> empty_statement env x
  | `Exp_stmt x -> expression_statement env x |> G.s
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = (* "return" *) token env v1 in
      let v2 =
        match v2 with
        | Some x ->
            let v1 = expression env x in
            Some v1
        | None -> None
      in
      let v3 = (* ";" *) token env v3 in
      G.Return (v1, v2, v3) |> G.s
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = (* "break" *) token env v1 in
      let v2 =
        match v2 with
        (* Q: Is this too permissive of typing? Losing specificity here... *)
        | Some x -> G.LDynamic (expression env x)
        | None -> G.LNone
      in
      let v3 = (* ";" *) token env v3 in
      G.Break (v1, v2, v3) |> G.s
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = (* "continue" *) token env v1 in
      let v2 =
        match v2 with
        (* Q: Same as above for LDynamic *)
        | Some x -> G.LDynamic (expression env x)
        | None -> G.LNone
      in
      let v3 = (* ";" *) token env v3 in
      G.Continue (v1, v2, v3) |> G.s
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = (* "throw" *) token env v1 in
      let v2 = expression env v2 in
      let v3 = (* ";" *) token env v3 in
      G.Throw (v1, v2, v3) |> G.s
  | `Echo_stmt (v1, v2, v3, v4) ->
      let v1 = (* "echo" *) str env v1 in
      let v2 = G.Arg (expression env v2) in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = G.Arg (expression env v2) in
            v2)
          v3
      in
      let v4 = (* ";" *) token env v4 in
      let exprs = v2 :: v3 in
      let iden = G.Id (v1, G.empty_id_info ()) in
      G.ExprStmt
        (G.Call (G.N iden |> G.e, Tok.unsafe_fake_bracket exprs) |> G.e, v4)
      |> G.s
  | `Unset_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "unset" *) token env v1 in
      let _v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = variablish env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = variablish env v2 in
                  G.E v2)
                v2
            in
            G.E v1 :: v2
        | None -> []
      in
      let _v4 = (* ")" *) token env v4 in
      let v5 = (* ";" *) token env v5 in
      (* Q: Better to just use Call? *)
      G.ExprStmt (G.OtherExpr (("Unset", v1), v3) |> G.e, v5) |> G.s
  | `Use_stmt (v1, v2, v3) ->
      (* Q: What do comma seperated use statements mean? And how do they alias? *)
      let v1 = (* "use" *) token env v1 in
      let create_directive use_clause prefix_idents =
        let idents, alias = use_clause in
        let idents =
          match idents with
          | Some x -> x
          | None -> raise Common.Impossible
        in
        G.s
          (G.DirectiveStmt
             (G.ImportAs (v1, G.DottedName (prefix_idents @ idents), alias)
             |> G.d))
      in
      let v2 =
        match v2 with
        | `Use_clause_rep_COMMA_use_clause_opt_COMMA (v1, v2, v3) ->
            let v1 = create_directive (use_clause env v1) [] in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = use_clause env v2 in
                  (* Q: Module name vs identifier? What's the difference? *)
                  create_directive v2 [])
                v2
            in
            let _v3 =
              match v3 with
              | Some tok -> (* "," *) Some (token env tok)
              | None -> None
            in
            (* Q: Is it improper to use blocks to represent directive groups? *)
            (* Lua seems to get around this by return statement lists here and flattening way up *)
            G.Block (Tok.unsafe_fake_bracket (v1 :: v2))
        | `Opt_use_type_name_id_LCURL_use_clause_rep_COMMA_use_clause_opt_COMMA_RCURL
            (v1, v2, v3, v4, v5, v6, v7) ->
            (* Q: How to represent `use` type? We are also possibly passed it up from use_clause. *)
            let _v1TODO =
              match v1 with
              | Some x -> Some (use_type env x)
              | None -> None
            in
            let v2 = namespace_identifier env v2 in
            let ident_prefix =
              match v2 with
              | Some x -> x
              | None -> []
            in
            let _v3 = (* "{" *) token env v3 in
            let v4 = create_directive (use_clause env v4) ident_prefix in
            let v5 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = use_clause env v2 in
                  create_directive v2 ident_prefix)
                v5
            in
            let _v6 =
              match v6 with
              | Some tok -> (* "," *) Some (token env tok)
              | None -> None
            in
            let _v7 = (* "}" *) token env v7 in
            G.Block (Tok.unsafe_fake_bracket (v4 :: v5))
      in
      let _v3 = (* ";" *) token env v3 in
      v2 |> G.s
  | `If_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement env v3 in
      (* Q: Should we be folding? Where does elif go? *)
      let v4 =
        List_.map
          (fun (v1, v2, v3) ->
            let v1 =
              match v1 with
              | `Elseif tok -> (* "elseif" *) token env tok
              | `Else_if (v1, v2) ->
                  let v1 = (* "else" *) token env v1 in
                  let v2 = (* "if" *) token env v2 in
                  Tok.combine_toks v1 [ v2 ]
            in
            let v2 = parenthesized_expression env v2 in
            let v3 = statement env v3 in
            G.If (v1, G.Cond v2, v3, None) |> G.s)
          v4
      in
      let v5 =
        match v5 with
        | Some (v1, v2) ->
            let _v1 = (* "else" *) token env v1 in
            let v2 = statement env v2 in
            [ v2 ]
        | None -> []
      in
      (* TODO: Figure out if this is even a proper representation *)
      G.If
        ( v1,
          G.Cond v2,
          v3,
          Some (G.Block (Tok.unsafe_fake_bracket (v4 @ v5)) |> G.s) )
      |> G.s
  | `While_stmt (v1, v2, v3) ->
      let v1 = (* "while" *) token env v1 in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement env v3 in
      G.While (v1, G.Cond v2, v3) |> G.s
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "do" *) token env v1 in
      let v2 = statement env v2 in
      let _v3 = (* "while" *) token env v3 in
      let v4 = parenthesized_expression env v4 in
      let _v5 = (* ";" *) token env v5 in
      G.DoWhile (v1, v2, v4) |> G.s
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = (* "for" *) token env v1 in
      let _v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        (* Q: Always use ForInitExpr since that is what we get back? *)
        | Some x ->
            let cast x = G.ForInitExpr x in
            List_.map cast (anon_exp_rep_COMMA_exp_0bb260c env x)
        | None -> []
      in
      let _v4 = (* ";" *) token env v4 in
      let v5 =
        (* Q: Can we use Seq here? Or Block? How does it change those other areas?
            Really just how to represent? *)
        match v5 with
        | Some x -> Some (G.Seq (anon_exp_rep_COMMA_exp_0bb260c env x) |> G.e)
        | None -> None
      in
      let _v6 = (* ";" *) token env v6 in
      let v7 =
        match v7 with
        | Some x -> Some (G.Seq (anon_exp_rep_COMMA_exp_0bb260c env x) |> G.e)
        | None -> None
      in
      let _v8 = (* ")" *) token env v8 in
      let v9 = statement env v9 in
      let header = G.ForClassic (v3, v5, v7) in
      G.For (v1, header, v9) |> G.s
  | `Switch_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "switch" *) token env v1 in
      let v2 = Some (G.Cond (parenthesized_expression env v2)) in
      let _v3 = (* "{" *) token env v3 in
      let v4 =
        List_.map
          (fun x ->
            match x with
            | `Switch_case x -> switch_case env x
            | `Switch_defa x -> switch_default env x)
          v4
      in
      let _v5 = (* "}" *) token env v5 in
      G.Switch (v1, v2, v4) |> G.s
  | `Fore_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = (* "foreach" *) token env v1 in
      let _v2 = (* "(" *) token env v2 in
      (* TODO: Modify TSH to make directly to pattern? *)
      (* TODO: Split expression to allow pattern use and expr use? *)
      let v3 =
        G.OtherPat (("ExprToPattern", v1), [ G.E (expression env v3) ])
      in
      let _v4TODO =
        match v4 with
        | Some tok -> (* "await" *) Some (token env tok)
        | None -> None
      in
      let v5 = (* as *) token env v5 in
      let v7 = variablish env v7 in
      (* TODO: There's a lot going on here. Test it! *)
      let v6 =
        match v6 with
        | Some (v1, v2) ->
            let v1 = variablish env v1 in
            let v2 = (* "=>" *) token env v2 in
            (* Q: Should this be variable definition? *)
            G.Assign (v1, v2, v7) |> G.e
        | None -> v7
      in
      let _v8 = (* ")" *) token env v8 in
      let v9 = statement env v9 in
      let header = G.ForEach (v3, v5, v6) in
      G.For (v1, header, v9) |> G.s
  | `Try_stmt (v1, v2, v3, v4) ->
      let v1 = (* "try" *) token env v1 in
      let v2 = compound_statement env v2 in
      let v3 = List_.map (catch_clause env) v3 in
      let v4 =
        match v4 with
        | `Catch_clause x ->
            let catch = [ catch_clause env x ] in
            G.Try (v1, v2, v3 @ catch, None, None) |> G.s
        | `Fina_clause x ->
            let finally = Some (finally_clause env x) in
            G.Try (v1, v2, v3, None, finally) |> G.s
      in
      v4
  | `Conc_stmt (v1, v2) ->
      let _v1TODO = (* "concurrent" *) token env v1 in
      let v2 = compound_statement env v2 in
      (* TODO: Add support? Find keyword? *)
      G.OtherStmt (G.OS_Todo, [ G.S v2 ]) |> G.s
  | `Using_stmt (v1, v2, v3) ->
      let _v1TODO =
        (* Q: Where to apply await? *)
        match v1 with
        | Some tok -> (* "await" *) Some (token env tok)
        | None -> None
      in
      let using_stmt = (* "using" *) token env v2 in
      let v3 =
        match v3 with
        (* Q: Last stmt is optional. How to represent? *)
        | `Exp_stmt x ->
            G.WithUsingResource
              ( using_stmt,
                [ expression_statement env x |> G.s ],
                G.Block (Tok.unsafe_fake_bracket []) |> G.s )
        | `LPAR_exp_rep_COMMA_exp_RPAR_choice_comp_stmt (v1, v2, v3, v4, v5) ->
            let v1 = (* "(" *) token env v1 in
            let v2 = G.ExprStmt (expression env v2, fk v1) |> G.s in
            let v3 =
              List_.map
                (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = G.ExprStmt (expression env v2, fk v1) |> G.s in
                  v2)
                v3
            in
            let _v4 = (* ")" *) token env v4 in
            let exprs = v2 :: v3 in
            let v5 = inline_compound_statement env v5 in
            G.WithUsingResource (using_stmt, exprs, v5)
      in
      v3 |> G.s

and switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = expression env v2 in
  let _v3 = (* ":" *) token env v3 in
  let v4 = List_.map (statement env) v4 in
  (* Q: Also, this is terrible.... Make expression more specific to be pattern? How to handle? *)
  (* G.CasesAndBody([Case(v1, OtherPat(OP_Expr, [G.E(v2)]))], G.Block(G.fake_bracket v4) |> G.s) *)
  (* Well can also do this... *)
  (* Q: Why is the case a list? To handle multiple cases with the same body?
     We don't support this in the grammar. We just don't have a break... *)
  G.CasesAndBody
    ([ CaseEqualExpr (v1, v2) ], G.Block (Tok.unsafe_fake_bracket v4) |> G.s)

and switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = (* "default" *) token env v1 in
  let _v2 = (* ":" *) token env v2 in
  let v3 = List_.map (statement env) v3 in
  (* Q: Again. Is it appropriate to use Block here? *)
  G.CasesAndBody ([ G.Default v1 ], G.Block (Tok.unsafe_fake_bracket v3) |> G.s)

and _trait_use_clause (env : env) ((v1, v2, v3, v4) : CST.trait_use_clause) =
  let v1 = (* "use" *) token env v1 in
  let v2 = type_ env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = type_ env v2 in
        v2)
      v3
  in
  let v4 =
    match v4 with
    | `LCURL_rep_choice_trait_select_clause_SEMI_RCURL (v1, v2, v3) ->
        let v1 = (* "{" *) token env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let v1 =
                match v1 with
                | `Trait_select_clause x -> trait_select_clause env x
                | `Trait_alias_clause x -> trait_alias_clause env x
              in
              let v2 = (* ";" *) token env v2 in
              todo env (v1, v2))
            v2
        in
        let v3 = (* "}" *) token env v3 in
        todo env (v1, v2, v3)
    | `SEMI tok -> (* ";" *) token env tok
  in
  todo env (v1, v2, v3, v4)

and type_ (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Type_spec (v1, v2, v3) ->
      let _v1TODO = List_.map (type_modifier env) v1 in
      let v3 =
        match v3 with
        | Some x -> type_arguments env x
        | None -> None
      in
      let v2 =
        match v2 with
        | `Choice_bool x -> G.ty_builtin (primitive_type env x)
        | `Qual_id x ->
            let xs = qualified_identifier env x in
            let n = H2.name_of_ids xs in
            G.TyN (H2.add_type_args_opt_to_name n v3) |> G.t
        | `Choice_array x -> G.ty_builtin (collection_type env x)
        | `Choice_xhp_id x ->
            let id = xhp_identifier_ env x in
            let n = H2.name_of_id id in
            G.TyN (H2.add_type_args_opt_to_name n v3) |> G.t
      in
      v2
  | `Type_cst (v1, v2) ->
      (* TODO: What to do with modifier? *)
      let _v1TODO = List_.map (type_modifier env) v1 in
      let v2 = type_constant_ env v2 in
      G.TyN (H2.name_of_ids v2) |> G.t
  | `Shape_type_spec (v1, v2, v3, v4, v5) ->
      let _v1TODO = List_.map (type_modifier env) v1 in
      let v2 = (* "shape" *) token env v2 in
      let v3 = (* "(" *) token env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2, v3) ->
            let v1 = G.F (shape_field_specifier env v1 |> G.s) in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = G.F (shape_field_specifier env v2 |> G.s) in
                  v2)
                v2
            in
            let _v3 =
              match v3 with
              | Some tok -> (* "," *) Some (token env tok)
              | None -> None
            in
            v1 :: v2
        | None -> []
      in
      let v5 = (* ")" *) token env v5 in
      G.TyRecordAnon ((G.Class, v2), (v3, v4, v5)) |> G.t
  | `Func_type_spec (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let _v1TODO = List_.map (type_modifier env) v1 in
      let _v2 = (* "(" *) token env v2 in
      let _v3TODO = (* pattern function\s*\( *) token env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2, v3, v4, v5) ->
            let _v1TODO =
              match v1 with
              | Some tok -> (* "inout" *) Some (token env tok)
              | None -> None
            in
            let v2 = G.Param (G.param_of_type (type_ env v2)) in
            let _v3TODO =
              match v3 with
              | Some tok -> (* "..." *) Some (token env tok)
              | None -> None
            in
            let v4 =
              List_.map
                (fun (v1, v2, v3, v4) ->
                  (* TODO: Handle `...` *)
                  let _v1 = (* "," *) token env v1 in
                  let v2 =
                    match v2 with
                    | Some tok ->
                        (* "inout" *) [ G.KeywordAttr (Mutable, token env tok) ]
                    | None -> []
                  in
                  let v3 =
                    let param = G.param_of_type (type_ env v3) in
                    G.Param { param with pattrs = v2 }
                  in
                  let _v4TODO =
                    match v4 with
                    | Some tok -> (* "..." *) Some (token env tok)
                    | None -> None
                  in
                  v3)
                v4
            in
            let _v5 =
              match v5 with
              | Some tok -> (* "," *) Some (token env tok)
              | None -> None
            in
            v2 :: v4
        | None -> []
      in
      let _v5 = (* ")" *) token env v5 in
      let _v6 = (* ":" *) token env v6 in
      let v7 = type_ env v7 in
      let _v8 = (* ")" *) token env v8 in
      G.TyFun (v4, v7) |> G.t
  | `Tuple_type_spec (v1, v2, v3, v4, v5, v6) ->
      let _v1TODO = List_.map (type_modifier env) v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = type_ env v3 in
      let v4 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = type_ env v2 in
            v2)
          v4
      in
      let _v5 =
        match v5 with
        | Some tok -> (* "," *) Some (token env tok)
        | None -> None
      in
      let v6 = (* ")" *) token env v6 in
      G.TyTuple (v2, v3 :: v4, v6) |> G.t

and type_arguments (env : env) ((v1, v2, v3) : CST.type_arguments) :
    G.type_arguments option =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = G.TA (type_ env v1) in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = G.TA (type_ env v2) in
              v2)
            v2
        in
        let _v3 =
          match v3 with
          | Some tok -> (* "," *) Some (token env tok)
          | None -> None
        in
        Some (v1 :: v2)
    | None -> None
  in
  let v3 = (* ">" *) token env v3 in
  match v2 with
  | Some v2 -> Some (v1, v2, v3)
  | None -> None

and type_const_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.type_const_declaration) =
  let v1 =
    match v1 with
    | Some x -> attribute_modifier env x
    | None -> []
  in
  let v2 = List_.map (member_modifier env) v2 in
  let v3 = (* "const" *) [ G.KeywordAttr (Const, token env v3) ] in
  let _v4 = (* "type" *) token env v4 in
  let id = semgrep_extended_identifier env v5 in
  let type_params = Option.map (type_parameters env) v6 in
  (* Q: How to represent this `as __type__`? It is a constraint? Make an attribute?
     OTP_Constrained on type param? But then can't be builtin *)
  let _v7TODO =
    match v7 with
    | Some (v1, v2) ->
        let _v1 = (* "as" *) token env v1 in
        let v2 = type_ env v2 in
        Some v2
    | None -> None
  in
  let v8 =
    match v8 with
    | Some (v1, v2) ->
        let _v1 = (* "=" *) token env v1 in
        let v2 = type_ env v2 in
        Some v2
    | None -> None
  in
  let v9 = (* ";" *) token env v9 in
  match v8 with
  (* Q: AliasType vs NewType? *)
  | Some v8 ->
      G.DefStmt
        ( basic_typed_entity id (v1 @ v2 @ v3) type_params,
          G.TypeDef { tbody = AliasType v8 } )
      |> G.s
  (* TODO: WHAT TO DO IN THIS CASE? WHAT IS `const type T1;` doing? *)
  | None ->
      G.DefStmt
        ( basic_typed_entity id (v1 @ v2 @ v3) type_params,
          G.TypeDef { tbody = AbstractType v9 } )
      |> G.s

and type_parameter (env : env) ((v1, v2, v3, v4) : CST.type_parameter) :
    G.type_parameter =
  let tp_attrs =
    match v1 with
    | Some x -> attribute_modifier env x
    | None -> []
  in
  let tp_variance =
    match v2 with
    | Some x -> (
        match x with
        | `PLUS tok -> (* "+" *) Some (G.Covariant, token env tok)
        | `DASH tok -> (* "-" *) Some (G.Contravariant, token env tok)
        | `Reify tok ->
            (* TODO? what is that? *)
            let _t = token env tok in
            (* "reify" *)
            None)
    | None -> None
  in
  let tp_id =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) str env v3
  in
  let tp_bounds =
    List_.map
      (fun (v1, v2) ->
        let _v1TODO =
          match v1 with
          | `As tok -> (* "as" *) token env tok
          | `Super tok -> (* "super" *) token env tok
        in
        let v2 = type_ env v2 in
        v2)
      v4
  in
  let tp_default = None in
  TP { G.tp_id; tp_attrs; tp_bounds; tp_variance; tp_default }

and type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) :
    G.type_parameters =
  let lt = (* "<" *) token env v1 in
  let v2 = type_parameter env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = type_parameter env v2 in
        v2)
      v3
  in
  let _v4 =
    match v4 with
    | Some tok -> (* "," *) Some (token env tok)
    | None -> None
  in
  let gt = (* ">" *) token env v5 in
  (lt, v2 :: v3, gt)

and variablish (env : env) (x : CST.variablish) : G.expr =
  match x with
  | `Var tok -> (* variable *) G.N (Id (str env tok, G.empty_id_info ())) |> G.e
  (* Q: Not anything special for pipe? *)
  | `Pipe_var tok ->
      (* "$$" *) G.N (Id (str env tok, G.empty_id_info ())) |> G.e
  | `List_exp (v1, v2, v3, v4, v5, v6) ->
      let _v1TODO = (* "list" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> [ expression env x ]
        | None -> []
      in
      let v4 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 =
              match v2 with
              | Some x -> [ expression env x ]
              | None -> []
            in
            v2)
          v4
      in
      let exprs = v3 @ List_.flatten v4 in
      let _v5 =
        match v5 with
        | Some tok -> (* "," *) Some (token env tok)
        | None -> None
      in
      let v6 = (* ")" *) token env v6 in
      G.Container (List, (v2, exprs, v6)) |> G.e
  | `Subs_exp (v1, v2, v3, v4) ->
      let v1 = expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v4 = (* "]" *) token env v4 in
      let v3 =
        match v3 with
        | Some x -> G.ArrayAccess (v1, (v2, expression env x, v4))
        | None -> G.OtherExpr (("ArrayAppend", v2), [])
      in
      v3 |> G.e
  | `Qual_id x -> G.N (qualified_identifier env x |> H2.name_of_ids) |> G.e
  | `Paren_exp x -> parenthesized_expression env x
  | `Call_exp x -> call_expression env x
  | `Scoped_id x -> G.N (scoped_identifier env x |> H2.name_of_ids) |> G.e
  | `Scope_id x -> scope_identifier env x
  | `Sele_exp x -> selection_expression env x
  | `Choice_xhp_id x ->
      let id = xhp_identifier_ env x in
      G.N (H2.name_of_id id) |> G.e

and where_clause (env : env) ((v1, v2) : CST.where_clause) =
  (* TODO: What keyword is this? *)
  (* TODO: I don't really know what this language feature is.... *)
  (* TODO: This is really wrong, because it also needs to be part of a FuncDef *)
  (* Q: Should this become a cmixins? *)
  let twhere = (* "where" *) token env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let frst, snd, thrd = where_constraint env v1 in
        let _v2 =
          match v2 with
          | Some tok -> (* "," *) Some (token env tok)
          | None -> None
        in
        G.OtherType (("Where", twhere), [ G.T frst; G.TodoK snd; G.T thrd ])
        |> G.t)
      v2
  in
  v2

and where_constraint (env : env) ((v1, v2, v3) : CST.where_constraint) =
  let v1 = type_ env v1 in
  let v2 =
    match v2 with
    | `As tok -> (* "as" *) str env tok
    | `Super tok -> (* "super" *) str env tok
    | `EQ tok -> (* "=" *) str env tok
  in
  let v3 = type_ env v3 in
  (v1, v2, v3)

and xhp_attribute (env : env) (x : CST.xhp_attribute) =
  match x with
  | `Xhp_id_EQ_choice_str (v1, v2, v3) ->
      let v1 =
        (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) str env v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        match v3 with
        | `Str tok ->
            (* string *) G.L (stringify_without_quotes (str env tok)) |> G.e
        | `Braced_exp x -> Tok.unbracket (braced_expression env x)
      in
      G.XmlAttr (v1, v2, v3)
  | `Choice_braced_exp x ->
      let x =
        match x with
        | `Braced_exp x -> braced_expression env x
        | `Xhp_spread_exp x -> xhp_spread_expression env x
      in
      G.XmlAttrExpr x

and xhp_attribute_declaration (env : env)
    ((v1, v2, v3, v4) : CST.xhp_attribute_declaration) =
  let ((_, tok) as ident) = str env v1 in
  let attr_tok =
    (* "attribute" *)
    G.NamedAttr
      (fk tok, G.Id (ident, G.empty_id_info ()), Tok.unsafe_fake_bracket [])
  in
  (* Q: Is this what we want to do with the keyword? *)
  let v2 = xhp_class_attribute env v2 attr_tok in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = xhp_class_attribute env v2 attr_tok in
        v2)
      v3
  in
  let _v4 = (* ";" *) token env v4 in
  v2 :: v3

and xhp_class_attribute (env : env) ((v1, v2, v3, v4) : CST.xhp_class_attribute)
    attr_tok =
  (* TODO: Is TSH too permissive here with the conditions? Could we really have opt name and field? *)
  let v2 =
    match v2 with
    | Some tok ->
        (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) str env tok
    (* TODO: Fix in t-s-h *)
    | None -> raise Common.Impossible
  in
  let v1 =
    match v1 with
    | `Choice_type_spec x -> type_ env x
    | `Xhp_enum_type x -> xhp_enum_type env x v2
  in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = (* "=" *) token env v1 in
        let v2 = expression env v2 in
        Some v2
    | None -> None
  in
  let v4 =
    (* TODO: Split off @ token in TSH? *)
    match v4 with
    | Some x -> (
        match x with
        | `ATre tok ->
            (* "@required" *)
            let ((_, tok) as ident) = str env tok in
            [
              G.NamedAttr
                ( fk tok,
                  Id (ident, G.empty_id_info ()),
                  Tok.unsafe_fake_bracket [] );
            ]
        | `ATla tok ->
            (* "@lateinit" *)
            let ((_, tok) as ident) = str env tok in
            [
              G.NamedAttr
                ( fk tok,
                  Id (ident, G.empty_id_info ()),
                  Tok.unsafe_fake_bracket [] );
            ])
    | None -> []
  in
  (* Q: Something feels off here... Originally did VarDef with init...*)
  (* But then it had to be enum... So did TypeDef, but then went back... *)
  let ent = G.basic_entity v2 ~attrs:(attr_tok :: v4) in
  let def = (ent, G.VarDef { vinit = v3; vtype = Some v1; vtok = G.no_sc }) in
  G.fld def

and xhp_expression (env : env) (x : CST.xhp_expression) : G.xml =
  match x with
  | `Xhp_open_close (v1, v2, v3, v4) ->
      let v1 = (* "<" *) token env v1 in
      let v2 = G.Id (xhp_identifier_ env v2, G.empty_id_info ()) in
      let v3 = List_.map (xhp_attribute env) v3 in
      let v4 = (* "/>" *) token env v4 in
      { xml_kind = G.XmlSingleton (v1, v2, v4); xml_attrs = v3; xml_body = [] }
  | `Xhp_open_rep_choice_xhp_str_xhp_close (v1, v2, v3) ->
      let opening_open_tok, opening_ident, opening_attrs, opening_close_tok =
        xhp_open env v1
      in
      let v2 =
        List_.map
          (fun x ->
            match x with
            | `Xhp_str tok -> (* xhp_string *) G.XmlText (str env tok)
            | `Xhp_comm tok ->
                (* xhp_comment *)
                let _x = token env tok in
                G.XmlExpr (Tok.unsafe_fake_bracket None)
            | `Braced_exp x ->
                let v1, v2, v3 = braced_expression env x in
                G.XmlExpr (v1, Some v2, v3)
            | `Xhp_exp x -> G.XmlXml (xhp_expression env x))
          v2
      in
      let v3 = xhp_close env v3 in
      {
        xml_kind =
          G.XmlClassic (opening_open_tok, opening_ident, opening_close_tok, v3);
        xml_attrs = opening_attrs;
        xml_body = v2;
      }

and xhp_open (env : env) ((v1, v2, v3, v4) : CST.xhp_open) =
  let v1 = (* "<" *) token env v1 in
  let v2 = G.Id (xhp_identifier_ env v2, G.empty_id_info ()) in
  let v3 = List_.map (xhp_attribute env) v3 in
  let v4 = (* ">" *) token env v4 in
  (v1, v2, v3, v4)

and xhp_spread_expression (env : env)
    ((v1, v2, v3, v4) : CST.xhp_spread_expression) =
  (* Q: How to differentiate this spread and Semgrep spread? Do I even need to? *)
  (* TODO: Support native ellipsis *)
  let v1 = (* "{" *) token env v1 in
  let _v2TODO = (* "..." *) token env v2 in
  let v3 = expression env v3 in
  let v4 = (* "}" *) token env v4 in
  (v1, v3, v4)

let script (env : env) ((v1, v2) : CST.script) : G.program =
  let _v1 =
    match v1 with
    | Some tok -> (* pattern <\?[hH][hH] *) token env tok |> ignore
    | None -> ()
  in
  List_.map (statement env) v2

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_hack.Parse.file !!file)
    (fun cst _extras ->
      let extra = Target in
      let env = { H.file; conv = H.line_col_to_pos file; extra } in
      try script env cst with
      | Failure "not implemented" as exn ->
          let e = Exception.catch exn in
          (* TODO: Use Boilerplate.dumper now on Raw_tree
           * H.debug_sexp_cst_after_error (CST.sexp_of_script cst);
           *)
          Exception.reraise e)

let parse_expression_or_source_file str =
  let res = Tree_sitter_hack.Parse.string str in
  match res.errors with
  | [] -> res
  | _ ->
      let expr_str = str ^ ";" in
      Tree_sitter_hack.Parse.string expr_str

let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_expression_or_source_file str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      (* TODO: do we need a special mode to convert $FOO in the
       * right construct? Is $XXX ambiguous in a semgrep context?
       * Imitate what we do in php_to_generic.ml?
       *)
      let extra = Pattern in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra } in
      (* TODO: G.Ss (script env cst) but regressions *)
      match script env cst with
      | [ { G.s = G.ExprStmt (e, _); _ } ] -> G.E e
      | [ x ] -> G.S x
      | xs -> G.Ss xs)

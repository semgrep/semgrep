(* Yoann Padioleau
 *
 * Copyright (c) 2021-2024 Semgrep Inc.
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
open Common
open Fpath_.Operators
module CST = Tree_sitter_ocaml.CST
module H = Parse_tree_sitter_helpers
open AST_ocaml

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* OCaml parser using tree-sitter-lang/semgrep-ocaml and converting
 * to ../ast/ast_ml.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * ml_to_generic.ml
 *
 * TODO:
 *  - see use of ()
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let token = H.token
let str = H.str

(* Raw identifiers denote the same name as their unescaped spelling. Keep the
 * original token so source ranges and autofixes still include the escape. *)
let identifier env tok =
  let name, tok = str env tok in
  let name =
    if String.starts_with ~prefix:"\\#" name then Str.string_after name 2
    else name
  in
  (name, tok)

(* like in parser_ml.mly *)
let seq1 = function
  | [ x ] -> x
  | xs -> Sequence (Tok.unsafe_fake_bracket xs)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-ocaml/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the ocaml CST
   to another type of tree.
*)

(* conversion_specification *)

let map_unit_ (env : env) (x : CST.unit_) : literal =
  match x with
  | `LPAR_RPAR (v1, v2) ->
      let v1 =
        token env v1
        (* "(" *)
      in
      let v2 =
        token env v2
        (* ")" *)
      in
      Unit (v1, v2)
  | `Begin_end (v1, v2) ->
      let v1 =
        token env v1
        (* "begin" *)
      in
      let v2 =
        token env v2
        (* "end" *)
      in
      Unit (v1, v2)

let map_boolean (env : env) (x : CST.boolean) : literal =
  match x with
  | `True tok -> Bool (true, (* "true" *) token env tok)
  | `False tok -> Bool (false, (* "false" *) token env tok)

let map_or_operator (env : env) (x : CST.or_operator) : string wrap = str env x

let map_and_operator (env : env) (x : CST.and_operator) : string wrap =
  str env x

let map_anon_choice_EQ_4ccabd6 (env : env) (x : CST.anon_choice_EQ_4ccabd6) =
  match x with
  | `EQ tok -> (* "=" *) token env tok
  (* TODO *)
  | `COLONEQ tok -> (* ":=" *) token env tok

let map_anon_choice_priv_c7cc539 (env : env) (x : CST.anon_choice_priv_c7cc539)
    =
  match x with
  | `Priv tok -> (* "private" *) token env tok
  | `Virt tok -> (* pattern "\\\\[\\\\\"'ntbr ]" *) token env tok

let map_anon_choice_TILDE_72781e5 (env : env)
    (x : CST.anon_choice_TILDE_72781e5) =
  match x with
  | `TILDE tok ->
      (* "~" *)
      let x = token env tok in
      (true, x)
  | `QMARK tok ->
      (* "?" *)
      let x = token env tok in
      (false, x)

let map_sign_operator (env : env) (x : CST.sign_operator) : string wrap =
  match x with
  | `Pat_dece469 tok -> str env tok (* "/[+.]/" *)
  | `Pat_79c7248 tok -> str env tok (* "/[+-]\." *)

let map_anon_choice_muta_d43fe41 (env : env) (x : CST.anon_choice_muta_d43fe41)
    =
  match x with
  | `Muta tok ->
      let x =
        token env tok
        (* "mutable" *)
      in
      Either.Left x
  | `Virt tok ->
      let x = token env tok in
      Either.Right x

(* "virtual" *)

let map_assign_operator (env : env) (x : CST.assign_operator) =
  str env x (* := *)

let map_anon_choice_PLUS_da42005 (env : env) x =
  match x with
  | `PLUS tok ->
      let x =
        token env tok
        (* "+" *)
      in
      Either.Left x
  | `DASH tok ->
      let x =
        token env tok
        (* "-" *)
      in
      Either.Right x

let map_mult_operator (env : env) (x : CST.mult_operator) : string wrap =
  str env x

let map_escape_sequence (env : env) (x : CST.escape_sequence) : string wrap =
  match x with
  | `Pat_60fc52b tok -> str env tok (* pattern "\\\\[\\\\\"'ntbr ]" *)
  | `Pat_86b875b tok -> str env tok (* pattern \\[0-9][0-9][0-9] *)
  | `Pat_3bf11d1 tok -> str env tok (* pattern \\x[0-9A-Fa-f][0-9A-Fa-f] *)
  | `Pat_21333c0 tok -> str env tok

(* pattern \\o[0-3][0-7][0-7] *)

let map_pow_operator (env : env) (x : CST.pow_operator) = str env x
let map_hash_operator (env : env) (tok : CST.hash_operator) = str env tok

let map_add_operator (env : env) (x : CST.add_operator) =
  match x with
  | `Pat_dece469 x -> str env x
  | `Pat_79c7248 x -> str env x
  | `Tok_choice_plus_rep1_pat_2ed1ddf x -> str env x

let map_quoted_string_content (env : env) (xs : CST.quoted_string_content) =
  List.map
    (fun x ->
      match x with
      | `Imm_tok_pat_dcdac4f tok -> str env tok (* pattern \s *)
      | `Imm_tok_pat_6c51254 tok -> str env tok (* pattern \[@ *)
      | `Pat_714c625 tok -> str env tok (* pattern [^%@|]+|%|@|\| *)
      | `Null tok -> str env tok (* null *)
      | `Conv_spec tok -> str env tok (* conversion_specification *)
      | `Pretty_prin_indi tok -> str env tok
      (* pattern @([\[\], ;.{}?]|\\n|<[0-9]+>) *))
    xs

let map_constructor_name (env : env) (x : CST.constructor_name) : ident =
  match x with
  | `Uppe_id tok -> str env tok (* pattern "[A-Z][a-zA-Z0-9_']*" *)
  | `LPAR_COLONCOLON_RPAR (v1, v2, v3) ->
      let _v1 =
        token env v1
        (* "(" *)
      in
      let v2 =
        str env v2
        (* "::" *)
      in
      let _v3 =
        token env v3
        (* ")" *)
      in
      v2

let rec map_module_path (env : env) (x : CST.module_path) : qualifier =
  match x with
  | `Uppe_id tok ->
      let x =
        str env tok
        (* pattern "[A-Z][a-zA-Z0-9_']*" *)
      in
      [ x ]
  | `Module_path_DOT_uppe_id (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let _v2 =
        token env v2
        (* "." *)
      in
      let v3 =
        str env v3
        (* pattern "[A-Z][a-zA-Z0-9_']*" *)
      in
      v1 @ [ v3 ]

(* todo: special type for _? *)
let map_anon_choice_module_name_ba4e400 (env : env)
    (x : CST.anon_choice_module_name_ba4e400) : ident =
  match x with
  | `Uppe_id tok -> str env tok (* pattern "[A-Z][a-zA-Z0-9_']*" *)
  | `X__ tok -> str env tok

(* "_" *)

(* TODO: handle functor arguments in path *)
let rec map_extended_module_path (env : env) (x : CST.extended_module_path) :
    qualifier =
  match x with
  | `Choice_uppe_id x -> (
      match x with
      | `Uppe_id tok ->
          let x =
            str env tok
            (* pattern "[A-Z][a-zA-Z0-9_']*" *)
          in
          [ x ]
      | `Exte_module_path_DOT_uppe_id (v1, v2, v3) ->
          let v1 = map_extended_module_path env v1 in
          let _v2 =
            token env v2
            (* "." *)
          in
          let v3 =
            str env v3
            (* pattern "[A-Z][a-zA-Z0-9_']*" *)
          in
          v1 @ [ v3 ])
  | `Exte_module_path_LPAR_exte_module_path_RPAR (v1, v2, v3, v4) ->
      let v1 = map_extended_module_path env v1 in
      let _v2 =
        token env v2
        (* "(" *)
      in
      let _v3TODO = map_extended_module_path env v3 in
      let _v4 =
        token env v4
        (* ")" *)
      in
      v1

(* Unlike the older name-only path mapping, a local type open retains functor
 * applications as well as the qualified names on both sides. *)
let rec map_type_open_path env (x : CST.extended_module_path) =
  match x with
  | `Choice_uppe_id (`Uppe_id id) -> TypeOpenName [ str env id ]
  | `Choice_uppe_id (`Exte_module_path_DOT_uppe_id (path, _, id)) ->
      TypeOpenDot (map_type_open_path env path, str env id)
  | `Exte_module_path_LPAR_exte_module_path_RPAR (fn, _, arg, _) ->
      TypeOpenApply (map_type_open_path env fn, map_type_open_path env arg)

let map_string_content (env : env) (xs : CST.string_content) =
  List.map
    (fun x ->
      match x with
      | `Imm_tok_pat_dcdac4f tok -> str env tok
      | `Imm_tok_pat_6c51254 tok -> str env tok
      | `Pat_19aaf34 tok -> str env tok (* pattern "[^\\\\\"%@]+|%|@" *)
      | `Null tok -> str env tok (* null *)
      | `Esc_seq x -> map_escape_sequence env x
      | `Pat_6cdf4be tok -> str env tok (* pattern \\u\{[0-9A-Fa-f]+\} *)
      | `Pat_c122fdd tok -> str env tok (* pattern \\\n[\t ]* *)
      | `Conv_spec tok -> str env tok (* conversion_specification *)
      | `Pretty_prin_indi tok -> str env tok
      (* pattern @([\[\], ;.{}?]|\\n|<[0-9]+>) *))
    xs

let map_character_content (env : env) (x : CST.character_content) : string wrap
    =
  match x with
  | `Imm_tok_pat_dcb14fe tok -> ("\n", token env tok)
  | `Imm_tok_pat_0de46b0 tok -> str env tok
  | `Null tok -> str env tok (* null *)
  | `Esc_seq x -> map_escape_sequence env x

let map_infix_operator (env : env) (x : CST.infix_operator) =
  match x with
  | `Pow_op x -> map_pow_operator env x
  | `Mult_op x -> map_mult_operator env x
  | `Add_op x -> map_add_operator env x
  | `Concat_op tok -> str env tok (* concat_operator *)
  | `Rel_op tok -> str env tok (* rel_operator *)
  | `And_op x -> map_and_operator env x
  | `Or_op x -> map_or_operator env x
  | `Assign_op x -> map_assign_operator env x

let map_module_type_name (env : env) (x : CST.module_type_name) : ident =
  match x with
  | `Uppe_id tok -> str env tok (* pattern "[A-Z][a-zA-Z0-9_']*" *)
  | `Lowe_id tok -> identifier env tok

(* pattern "[a-z_][a-zA-Z0-9_']*" *)

let map_abstract_type (env : env) ((v1, v2) : CST.abstract_type) =
  let v1 =
    token env v1
    (* "type" *)
  in
  let v2 = List.map (identifier env) (* pattern "[a-z_][a-zA-Z0-9_']*" *) v2 in
  (v1, v2)

let map_anon_choice_inst_var_name_1804c1b (env : env)
    (x : CST.anon_choice_inst_var_name_1804c1b) : ident =
  match x with
  | `Lowe_id tok -> identifier env tok (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  | `Uppe_id tok -> str env tok

(* pattern "[A-Z][a-zA-Z0-9_']*" *)

let map_label (env : env) ((v1, v2) : CST.label) =
  let v1 = map_anon_choice_TILDE_72781e5 env v1 in
  let v2 =
    identifier env v2
    (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  in
  (v1, v2)

let map_constructor_path (env : env) (x : CST.constructor_path) : name =
  match x with
  | `Cons_name x ->
      let x = map_constructor_name env x in
      ([], x)
  | `Module_path_DOT_cons_name (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let _v2 =
        token env v2
        (* "." *)
      in
      let v3 = map_constructor_name env v3 in
      (v1, v3)

let map_indexing_operator_path (env : env) (x : CST.indexing_operator_path) :
    name =
  match x with
  | `Inde_op tok ->
      let x =
        str env tok
        (* indexing_operator *)
      in
      ([], x)
  | `Module_path_DOT_inde_op (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let _v2 =
        token env v2
        (* "." *)
      in
      let v3 =
        str env v3
        (* indexing_operator *)
      in
      (v1, v3)

let map_class_path (env : env) (x : CST.class_path) : name =
  match x with
  | `Lowe_id tok ->
      let x =
        identifier env tok
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      ([], x)
  | `Module_path_DOT_lowe_id (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let _v2 =
        token env v2
        (* "." *)
      in
      let v3 =
        identifier env v3
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      (v1, v3)

let map_field_path (env : env) (x : CST.field_path) : name =
  match x with
  | `Lowe_id tok ->
      let x =
        identifier env tok
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      ([], x)
  | `Module_path_DOT_lowe_id (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let _v2 =
        token env v2
        (* "." *)
      in
      let v3 =
        identifier env v3
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      (v1, v3)

let map_type_constructor_path (env : env) (x : CST.type_constructor_path) : name
    =
  match x with
  | `Lowe_id tok ->
      let x =
        identifier env tok
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      ([], x)
  | `Exte_module_path_DOT_lowe_id (v1, v2, v3) ->
      let v1 = map_extended_module_path env v1 in
      let _v2 =
        token env v2
        (* "." *)
      in
      let v3 =
        identifier env v3
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      (v1, v3)

let map_class_type_path (env : env) (x : CST.class_type_path) : name =
  match x with
  | `Lowe_id tok ->
      let x =
        identifier env tok
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      ([], x)
  | `Exte_module_path_DOT_lowe_id (v1, v2, v3) ->
      let v1 = map_extended_module_path env v1 in
      let _v2 =
        token env v2
        (* "." *)
      in
      let v3 =
        identifier env v3
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      (v1, v3)

let map_string_ (env : env) ((v1, v2, v3) : CST.string_) : string wrap =
  let v1 =
    token env v1
    (* "\"" *)
  in
  let v2 =
    match v2 with
    | Some x -> map_string_content env x
    | None -> []
  in
  let v3 =
    token env v3
    (* "\"" *)
  in
  let s = v2 |> List.map fst |> String.concat "" in
  let ts = v2 |> List.map snd in
  (s, Tok.combine_toks v1 (ts @ [ v3 ]))

let map_parenthesized_operator (env : env)
    ((v1, v2, v3) : CST.parenthesized_operator) : string wrap =
  let _v1 =
    token env v1
    (* "(" *)
  in
  let v2 =
    match v2 with
    | `Prefix_op tok -> str env tok (* prefix_operator *)
    | `Hash_op x -> map_hash_operator env x
    | `Infix_op x -> map_infix_operator env x
    | `DOT_inde_op_choice_LPAR_opt_SEMI_DOTDOT_RPAR_opt_LTDASH (v1, v2, v3, v4)
      ->
        let v1 =
          token env v1
          (* "." *)
        in
        let v2 =
          str env v2
          (* indexing_operator *)
        in
        let v3 =
          match v3 with
          | `LPAR_opt_SEMI_DOTDOT_RPAR (v1, v2, v3) ->
              let v1 =
                str env v1
                (* "(" *)
              in
              let v2 =
                match v2 with
                | Some (v1, v2) ->
                    let v1 =
                      str env v1
                      (* ";" *)
                    in
                    let v2 =
                      str env v2
                      (* ".." *)
                    in
                    [ v1; v2 ]
                | None -> []
              in
              let v3 =
                str env v3
                (* ")" *)
              in
              [ v1 ] @ v2 @ [ v3 ]
          | `LBRACK_opt_SEMI_DOTDOT_RBRACK (v1, v2, v3) ->
              let v1 =
                str env v1
                (* "[" *)
              in
              let v2 =
                match v2 with
                | Some (v1, v2) ->
                    let v1 =
                      str env v1
                      (* ";" *)
                    in
                    let v2 =
                      str env v2
                      (* ".." *)
                    in
                    [ v1; v2 ]
                | None -> []
              in
              let v3 =
                str env v3
                (* "]" *)
              in
              [ v1 ] @ v2 @ [ v3 ]
          | `LCURL_opt_SEMI_DOTDOT_RCURL (v1, v2, v3) ->
              let v1 =
                str env v1
                (* "{" *)
              in
              let v2 =
                match v2 with
                | Some (v1, v2) ->
                    let v1 =
                      str env v1
                      (* ";" *)
                    in
                    let v2 =
                      str env v2
                      (* ".." *)
                    in
                    [ v1; v2 ]
                | None -> []
              in
              let v3 =
                str env v3
                (* "}" *)
              in
              [ v1 ] @ v2 @ [ v3 ]
        in

        let v4 =
          match v4 with
          | Some tok ->
              let x =
                str env tok
                (* "<-" *)
              in
              [ x ]
          | None -> []
        in
        let xs = [ v2 ] @ v3 @ v4 in
        let s = "." ^ (xs |> List.map fst |> String.concat "") in
        (s, Tok.combine_toks v1 (xs |> List.map snd))
    | `Let_op tok -> str env tok (* let_operator *)
    | `Let_and_op tok -> str env tok (* let_and_operator *)
    | `Match_op tok -> str env tok
    (* match_operator *)
  in
  let _v3 =
    token env v3
    (* ")" *)
  in
  v2

let map_module_type_path (env : env) (x : CST.module_type_path) : name =
  match x with
  | `Choice_uppe_id x ->
      let x = map_module_type_name env x in
      ([], x)
  | `Exte_module_path_DOT_choice_uppe_id (v1, v2, v3) ->
      let v1 = map_extended_module_path env v1 in
      let _v2 =
        token env v2
        (* "." *)
      in
      let v3 = map_module_type_name env v3 in
      (v1, v3)

(* A polymorphic-variant tag is not a type name. Prefix the ident so the
 * generic AST still shows which tag was written. *)
let type_of_tag (tick, id) =
  TyName ([], ("`" ^ fst id, Tok.combine_toks tick [ snd id ]))

let rec module_type_as_types = function
  | ModuleTyPath n -> [ TyName n ]
  | ModuleTySig _items -> []
  | ModuleTyFunctor (params, _arrow, codomain) ->
      List.concat_map module_parameter_as_types params
      @ module_type_as_types codomain
  | ModuleTyArrow (domain, _arrow, codomain) ->
      module_type_as_types domain @ module_type_as_types codomain
  | ModuleTyWith (mty, tys) -> module_type_as_types mty @ tys
  | ModuleTyOf _
  | ModuleTyTodo _ ->
      []

and module_parameter_as_types = function
  | ModuleParam (_id, mty) -> module_type_as_types mty
  | ModuleParamUnit _tok -> []

let types_of_qualifier qualifier =
  match List.rev qualifier with
  | [] -> []
  | id :: rest -> [ TyName (List.rev rest, id) ]

let map_parenthesized_abstract_type (env : env)
    ((v1, v2, v3) : CST.parenthesized_abstract_type) =
  let v1 =
    token env v1
    (* "(" *)
  in
  let v2 = map_abstract_type env v2 in
  let _v3 =
    token env v3
    (* ")" *)
  in
  (v1, v2, v3)

let map_attribute_id (env : env) ((v1, v2) : CST.attribute_id) : dotted_ident =
  let v1 = map_anon_choice_inst_var_name_1804c1b env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "." *)
        in
        let v2 = map_anon_choice_inst_var_name_1804c1b env v2 in
        v2)
      v2
  in
  v1 :: v2

let map_directive (env : env) ((v1, v2) : CST.directive) =
  let v1 =
    token env v1
    (* "#" *)
  in
  let v2 = map_anon_choice_inst_var_name_1804c1b env v2 in
  (v1, v2)

let map_tag (env : env) ((v1, v2) : CST.tag) =
  let v1 =
    token env v1
    (* "`" *)
  in
  let v2 = map_anon_choice_inst_var_name_1804c1b env v2 in
  (v1, v2)

let map_type_variable (env : env) ((v1, v2) : CST.type_variable) : ident =
  let v1 =
    token env v1
    (* "'" *)
  in
  let v2 = map_anon_choice_inst_var_name_1804c1b env v2 in
  ("'" ^ fst v2, Tok.combine_toks v1 [ snd v2 ])

let map_polymorphic_variant_pattern (env : env)
    ((v1, v2) : CST.polymorphic_variant_pattern) : pattern =
  let v1 =
    token env v1
    (* "#" *)
  in
  let _v2 = map_type_constructor_path env v2 in
  PatTodo (("PolyVariant", v1), [])

let number env tok =
  let s, t =
    str env tok
    (* number *)
  in
  match Parsed_int.parse (s, t) with
  | (Some _, _) as pi -> Int pi
  | _ ->
      let fopt = float_of_string_opt s in
      Float (fopt, t)

let map_constant (env : env) (x : CST.constant) : literal =
  match x with
  | `Num tok -> number env tok
  | `Char (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "'" *)
      in
      let s, v2 = map_character_content env v2 in
      let v3 =
        token env v3
        (* "'" *)
      in
      Char (s, Tok.combine_toks v1 [ v2; v3 ])
  | `Str x ->
      let x = map_string_ env x in
      String x
  | `Quoted_str_ (v1, (v2, v3, v4), v5) ->
      let v1 =
        token env v1
        (* "{" *)
      in
      let v2 =
        token env v2
        (* left_quoted_string_delimiter *)
      in
      let v3 =
        match v3 with
        | Some x -> map_quoted_string_content env x
        | None -> []
      in
      let v4 =
        token env v4
        (* right_quoted_string_delimiter *)
      in
      let v5 =
        token env v5
        (* "}" *)
      in
      let s = v3 |> List.map fst |> String.concat "" in
      let xs = [ v2 ] @ List.map snd v3 @ [ v4; v5 ] in
      String (s, Tok.combine_toks v1 xs)
  | `Bool x -> map_boolean env x
  | `Unit x -> map_unit_ env x

let map_value_pattern (env : env) (x : CST.value_pattern) : ident =
  match x with
  | `Simple_value_pat tok ->
      identifier env tok (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  | `Paren_op x -> map_parenthesized_operator env x

let map_value_name (env : env) (x : CST.value_name) : ident =
  match x with
  | `Lowe_id tok -> identifier env tok (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  | `Paren_op x -> map_parenthesized_operator env x

let map_attribute (env : env) ((v1, v2) : CST.attribute) : dotted_ident =
  let _v1 =
    token env v1
    (* "%" *)
  in
  let v2 = map_attribute_id env v2 in
  v2

let map_attribute_opt env v =
  match v with
  | Some x -> Some (map_attribute env x)
  | None -> None

let map_type_param (env : env) ((v1, v2) : CST.type_param) : type_parameter =
  let _v1TODO =
    match v1 with
    | Some x -> (
        match x with
        | `PLUS_opt_BANG (v1, v2) ->
            let v1 =
              token env v1
              (* "+" *)
            in
            let v2 =
              match v2 with
              | Some tok -> [ token env tok (* "!" *) ]
              | None -> []
            in
            v1 :: v2
        | `DASH_opt_BANG (v1, v2) ->
            let v1 =
              token env v1
              (* "-" *)
            in
            let v2 =
              match v2 with
              | Some tok -> [ token env tok (* "!" *) ]
              | None -> []
            in
            v1 :: v2
        | `BANG_opt_choice_PLUS (v1, v2) ->
            let v1 =
              token env v1
              (* "!" *)
            in
            let _v2TODO =
              match v2 with
              | Some x -> [ map_anon_choice_PLUS_da42005 env x ]
              | None -> []
            in
            [ v1 ])
    | None -> []
  in
  let v2 =
    match v2 with
    | `Type_var x -> map_type_variable env x
    | `X__ tok -> str env tok
    (* "_" *)
  in
  TyParam v2

let map_signed_constant (env : env) (x : CST.signed_constant) : literal =
  match x with
  | `Cst x -> map_constant env x
  | `Signed_num (v1, v2) -> (
      let op, t1 =
        str env v1
        (* /[+-]/ *)
      in
      let v2 =
        number env v2
        (* tok_choice_pat_4349e4b *)
      in
      match (op, v2) with
      | "+", Int pi ->
          Int (Parsed_int.map_tok (fun t2 -> Tok.combine_toks t1 [ t2 ]) pi)
      | "+", Float (opt, t2) -> Float (opt, Tok.combine_toks t1 [ t2 ])
      (* TODO: negate nums *)
      | "-", Int pi ->
          Int (Parsed_int.map_tok (fun t2 -> Tok.combine_toks t1 [ t2 ]) pi)
      | "-", Float (_opt, t2) -> Float (None, Tok.combine_toks t1 [ t2 ])
      | _, _ ->
          raise
            (Parsing_error.Other_error ("Impossible negate of not number", t1)))

let map_value_path (env : env) (x : CST.value_path) : name =
  match x with
  | `Value_name x ->
      let x = map_value_name env x in
      ([], x)
  | `Module_path_DOT_value_name (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let _v2 =
        token env v2
        (* "." *)
      in
      let v3 = map_value_name env v3 in
      (v1, v3)

let map_type_params (env : env) (x : CST.type_params) : type_parameters =
  match x with
  | `Type_param x -> Tok.unsafe_fake_bracket [ map_type_param env x ]
  | `LPAR_type_param_rep_COMMA_type_param_RPAR (v1, v2, v3, v4) ->
      let lp =
        token env v1
        (* "(" *)
      in
      let v2 = map_type_param env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let _v1 =
              token env v1
              (* "," *)
            in
            let v2 = map_type_param env v2 in
            v2)
          v3
      in
      let rp =
        token env v4
        (* ")" *)
      in
      (lp, v2 :: v3, rp)

let map_anon_LBRACK_type_param_rep_COMMA_type_param_RBRACK_cea5434 (env : env)
    ((v1, v2, v3, v4) :
      CST.anon_LBRACK_type_param_rep_COMMA_type_param_RBRACK_cea5434) :
    type_parameters =
  let lb =
    token env v1
    (* "[" *)
  in
  let v2 = map_type_param env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "," *)
        in
        let v2 = map_type_param env v2 in
        v2)
      v3
  in
  let rb =
    token env v4
    (* "]" *)
  in
  (lb, v2 :: v3, rb)

let map_range_pattern (env : env) ((v1, v2, v3) : CST.range_pattern) =
  let v1 = map_signed_constant env v1 in
  let v2 =
    token env v2
    (* ".." *)
  in
  let v3 = map_signed_constant env v3 in
  PatTodo (("Range", v2), [ PatLiteral v1; PatLiteral v3 ])

let map_toplevel_directive (env : env) ((v1, v2) : CST.toplevel_directive) :
    item =
  let tsharp, _id = map_directive env v1 in
  let _v2TODO =
    match v2 with
    | Some x -> (
        match x with
        | `Cst x ->
            let x = map_constant env x in
            [ E (L x) ]
        | `Value_path x ->
            let x = map_value_path env x in
            [ E (Name x) ]
        | `Module_path x ->
            let xs = map_module_path env x in
            xs |> List.map (fun x -> Id x))
    | None -> []
  in
  ItemTodo (("Directive", tsharp), []) |> mki

let rec map_anon_bind_pat_rep_SEMI_bind_pat_opt_SEMI_ddad36f (env : env)
    ((v1, v2, v3) : CST.anon_bind_pat_rep_SEMI_bind_pat_opt_SEMI_ddad36f) :
    pattern list =
  let v1 = map_binding_pattern env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* ";" *)
        in
        let v2 = map_binding_pattern env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with
    | Some tok -> [ token env tok (* ";" *) ]
    | None -> []
  in
  v1 :: v2

and map_anon_choice_cons_type_771aabb (env : env)
    (x : CST.anon_choice_cons_type_771aabb) : type_ list =
  match x with
  | `Cons_type (v1, v2, v3, v4, v5) ->
      let _v1 =
        token env v1
        (* "type" *)
      in
      let _v2 =
        match v2 with
        | Some x -> Some (map_type_params env x)
        | None -> None
      in
      let path = map_type_constructor_path env v3 in
      let _eq, ty = map_type_equation env v4 in
      let _v5 = List.map (map_type_constraint env) v5 in
      [ TyName path; ty ]
  | `Cons_module (v1, v2, v3, v4) ->
      let _v1 =
        token env v1
        (* "module" *)
      in
      let left = map_module_path env v2 in
      let _v3 = map_anon_choice_EQ_4ccabd6 env v3 in
      let right = map_extended_module_path env v4 in
      types_of_qualifier left @ types_of_qualifier right
  | `Cons_module_type (v1, v2, v3, v4, v5) ->
      let _v1 =
        token env v1
        (* "module" *)
      in
      let _v2 =
        token env v2
        (* "type" *)
      in
      let path = map_module_type_path env v3 in
      let _v4 = map_anon_choice_EQ_4ccabd6 env v4 in
      let rhs = map_module_type env v5 in
      TyName path :: module_type_as_types rhs

and map_anon_choice_meth_type_d8c4bdc (env : env)
    (x : CST.anon_choice_meth_type_d8c4bdc) =
  match x with
  | `Meth_type (v1, v2) ->
      let _v1 =
        token env v1
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      let _v2 = map_polymorphic_typed env v2 in
      ()
  | `Simple_type x ->
      let _ = map_simple_type env x in
      ()

and map_anon_choice_simple_type_9a0619a (env : env)
    (x : CST.anon_choice_simple_type_9a0619a) : type_ list bracket =
  match x with
  | `Simple_type x -> Tok.unsafe_fake_bracket [ map_simple_type env x ]
  | `LPAR_type_rep_COMMA_type_RPAR (v1, v2, v3, v4) ->
      let lp =
        token env v1
        (* "(" *)
      in
      let v2 = map_type_ env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let _v1 =
              token env v1
              (* "," *)
            in
            let v2 = map_type_ env v2 in
            v2)
          v3
      in
      let rp =
        token env v4
        (* ")" *)
      in
      (lp, v2 :: v3, rp)

and map_anon_cons_decl_rep_BAR_cons_decl_fc0ccc5 (env : env)
    ((v1, v2) : CST.anon_cons_decl_rep_BAR_cons_decl_fc0ccc5) =
  let v1 = map_constructor_declaration env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "|" *)
        in
        let v2 = map_constructor_declaration env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_anon_exp_rep_SEMI_exp_opt_SEMI_72c923c (env : env)
    ((v1, v2, v3) : CST.anon_exp_rep_SEMI_exp_opt_SEMI_72c923c) : expr list =
  let v1 = map_expression env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* ";" *)
        in
        let v2 = map_expression env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with
    | Some tok -> [ token env tok (* ";" *) ]
    | None -> []
  in
  v1 :: v2

and map_anon_pat_rep_SEMI_pat_opt_SEMI_35217e1 (env : env)
    ((v1, v2, v3) : CST.anon_pat_rep_SEMI_pat_opt_SEMI_35217e1) : pattern list =
  let v1 = map_pattern env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* ";" *)
        in
        let v2 = map_pattern env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with
    | Some tok -> [ token env tok (* ";" *) ]
    | None -> []
  in
  v1 :: v2

and map_argument (env : env) (x : CST.argument) : argument =
  match x with
  | `Simple_exp x ->
      let x = map_simple_expression env x in
      Arg x
  | `Labe_arg x -> map_labeled_argument env x

and map_array_binding_pattern (env : env)
    ((v1, v2, v3) : CST.array_binding_pattern) : pattern =
  let v1 =
    token env v1
    (* "[|" *)
  in
  let v2 =
    match v2 with
    | Some x -> map_anon_bind_pat_rep_SEMI_bind_pat_opt_SEMI_ddad36f env x
    | None -> []
  in
  let _v3 =
    token env v3
    (* "|]" *)
  in
  PatTodo (("ArrayLiteral", v1), v2)

and map_array_expression (env : env) ((v1, v2, v3) : CST.array_expression) =
  let v1 =
    token env v1
    (* "[|" *)
  in
  let v2 =
    match v2 with
    | Some x -> map_anon_exp_rep_SEMI_exp_opt_SEMI_72c923c env x
    | None -> []
  in
  let _v3 =
    token env v3
    (* "|]" *)
  in
  ExprTodo (("ArrayLiteral", v1), v2)

and map_array_get_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.array_get_expression) =
  let v1 = map_simple_expression env v1 in
  let v2 =
    token env v2
    (* "." *)
  in
  let _v3TODO =
    match v3 with
    | Some x -> Some (map_indexing_operator_path env x)
    | None -> None
  in
  let _v4 =
    token env v4
    (* "(" *)
  in
  let v5 = map_sequence_expression env v5 |> seq1 in
  let _v6 =
    token env v6
    (* ")" *)
  in
  ExprTodo (("Array", v2), [ v1; v5 ])

and map_array_pattern (env : env) ((v1, v2, v3) : CST.array_pattern) =
  let v1 =
    token env v1
    (* "[|" *)
  in
  let v2 =
    match v2 with
    | Some x -> map_anon_pat_rep_SEMI_pat_opt_SEMI_35217e1 env x
    | None -> []
  in
  let _v3 =
    token env v3
    (* "|]" *)
  in
  PatTodo (("Array", v1), v2)

and map_attribute_payload (env : env) (x : CST.attribute_payload) : item list =
  match x with
  | `Stru x ->
      let xs = map_structure env x in
      xs
  | `COLON_opt_choice_type (v1, v2) ->
      let v1 =
        token env v1
        (* ":" *)
      in
      let _v2 =
        match v2 with
        | None -> ()
        | Some v2 -> (
            match v2 with
            | `Type x ->
                let _ty = map_type_ env x in
                ()
            | `Sign x ->
                let _x = map_signature env x in
                ())
      in
      [ ItemTodo (("AttrColon", v1), []) |> mki ]
  | `QMARK_pat_opt_guard (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "?" *)
      in
      let _v2 = map_pattern env v2 in
      let _v3 =
        match v3 with
        | Some x -> Some (map_guard env x)
        | None -> None
      in
      [ ItemTodo (("AttrQuestion", v1), []) |> mki ]

and map_attribute_payload_opt env v =
  match v with
  | Some x -> map_attribute_payload env x
  | None -> []

and map_bigarray_get_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.bigarray_get_expression) =
  let v1 = map_simple_expression env v1 in
  let v2 =
    token env v2
    (* "." *)
  in
  let _v3 =
    match v3 with
    | Some x -> Some (map_indexing_operator_path env x)
    | None -> None
  in
  let _v4 =
    token env v4
    (* "{" *)
  in
  let v5 = map_sequence_expression env v5 |> seq1 in
  let _v6 =
    token env v6
    (* "}" *)
  in
  ExprTodo (("BigArray", v2), [ v1; v5 ])

and map_simple_binding_pattern (env : env) (x : CST.simple_binding_pattern) :
    pattern =
  match x with
  | `Value_name x ->
      let x = map_value_name env x in
      PatVar x
  | `Signed_cst x ->
      let x = map_signed_constant env x in
      PatLiteral x
  | `Typed_bind_pat (v1, v2, v3, v4) ->
      let _v1 =
        token env v1
        (* "(" *)
      in
      let v2 = map_binding_pattern env v2 in
      let t, v3 = map_typed env v3 in
      let _v4 =
        token env v4
        (* ")" *)
      in
      PatTyped (v2, t, v3)
  | `Cons_path x ->
      let x = map_constructor_path env x in
      PatConstructor (x, None)
  | `Tag x ->
      let x = map_tag env x in
      PatPolyVariant (x, None)
  | `Poly_vari_pat x -> map_polymorphic_variant_pattern env x
  | `Record_bind_pat x -> map_record_binding_pattern env x
  | `List_bind_pat x -> map_list_binding_pattern env x
  | `Array_bind_pat x -> map_array_binding_pattern env x
  | `Local_open_bind_pat (v1, v2, v3) ->
      let _v1 = map_module_path env v1 in
      let v2 =
        token env v2
        (* "." *)
      in
      let v3 =
        match v3 with
        | `LPAR_opt_bind_pat_RPAR (v1, v2, v3) -> (
            let v1 =
              token env v1
              (* "(" *)
            in
            let _v3 =
              token env v3
              (* ")" *)
            in
            match v2 with
            | Some x ->
                let p = map_binding_pattern env x in
                PatTodo (("PatLocalOpen", v1), [ p ])
            | None -> PatTodo (("PatLocalOpen", v1), []))
        | `List_bind_pat x -> map_list_binding_pattern env x
        | `Array_bind_pat x -> map_array_binding_pattern env x
        | `Record_bind_pat x -> map_record_binding_pattern env x
      in
      PatTodo (("PatLocalOpen", v2), [ v3 ])
  | `Pack_pat x -> map_package_pattern env x
  | `Paren_bind_pat (v1, v2, v3) ->
      let _v1 =
        token env v1
        (* "(" *)
      in
      let v2 = map_binding_pattern env v2 in
      let _v3 =
        token env v3
        (* ")" *)
      in
      v2
  | `Choice_exte x -> PatTodo (map_extension env x, [])

and map_binding_pattern (env : env) (x : CST.binding_pattern) : pattern =
  match x with
  | `Simple_bind_pat x -> map_simple_binding_pattern env x
  | `Alias_bind_pat (v1, v2, v3) ->
      let v1 = map_binding_pattern env v1 in
      let _v2 =
        token env v2
        (* "as" *)
      in
      let v3 = map_value_name env v3 in
      PatAs (v1, v3)
  | `Or_bind_pat_anon x -> map_or_binding_pattern env x
  | `Cons_bind_pat_1ca6430 (v1, abstract, v2) -> (
      let v1 = map_constructor_path env v1 in
      let v2 = map_binding_pattern env v2 in
      match abstract with
      | None -> PatConstructor (v1, Some v2)
      | Some x ->
          let _, (tok, names), _ = map_parenthesized_abstract_type env x in
          PatConstructorAbstract (v1, tok, names, v2))
  | `Tag_bind_pat (v1, v2) ->
      let v1 = map_tag env v1 in
      let v2 = map_binding_pattern env v2 in
      PatPolyVariant (v1, Some v2)
  | `Tuple_bind_pat x ->
      PatTuple (Tok.unsafe_fake_bracket (map_tuple_binding_pattern env x))
  | `Cons_bind_pat_f2d0ae9 (v1, v2, v3) ->
      let v1 = map_binding_pattern env v1 in
      let v2 =
        token env v2
        (* "::" *)
      in
      let v3 = map_binding_pattern env v3 in
      PatConsInfix (v1, v2, v3)
  | `Range_pat x -> map_range_pattern env x
  | `Lazy_bind_pat (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "lazy" *)
      in
      let _v2TODO = map_attribute_opt env v2 in
      let v3 = map_binding_pattern env v3 in
      PatTodo (("Lazy", v1), [ v3 ])

and map_class_binding (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.class_binding) : class_binding =
  let _v1 =
    match v1 with
    | Some tok -> Some (token env tok) (* "virtual" *)
    | None -> None
  in
  let c_tparams =
    match v2 with
    | Some x ->
        Some
          (map_anon_LBRACK_type_param_rep_COMMA_type_param_RBRACK_cea5434 env x)
    | None -> None
  in
  let c_name =
    identifier env v3
    (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  in
  let c_params = List.map (map_parameter_ env) v4 in
  let _v5 =
    match v5 with
    | Some x -> Some (map_class_typed env x)
    | None -> None
  in
  let c_body =
    match v6 with
    | Some (v1, v2) ->
        let _v1 =
          token env v1
          (* "=" *)
        in
        let v2 = map_class_expression env v2 in
        Some v2
    | None -> None
  in
  let _v7 = List.map (map_item_attribute env) v7 in
  { c_name; c_tparams; c_params; c_body }

and map_class_definition (env : env) ((v1, v2, v3, v4) : CST.class_definition) :
    item =
  let tclass =
    token env v1
    (* "class" *)
  in
  let _attrs = map_attribute_opt env v2 in
  let v3 = map_class_binding env v3 in
  let v4 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "and" *)
        in
        let v2 = map_class_binding env v2 in
        v2)
      v4
  in
  Class (tclass, v3 :: v4) |> mki

and map_class_expression (env : env) (x : CST.class_expression) : class_expr =
  match x with
  | `Simple_class_exp x -> map_simple_class_expression env x
  | `Class_func (v1, v2, v3, v4) ->
      let tfun =
        token env v1
        (* "fun" *)
      in
      let _v2 = List.map (map_parameter_ env) v2 in
      let _v3 =
        token env v3
        (* "->" *)
      in
      let v4 = map_class_expression env v4 in
      ClTodo (("ClassFunc", tfun), [ v4 ])
  | `Class_app (v1, v2) ->
      let v1 = map_simple_class_expression env v1 in
      let _v2TODO = List.map (map_argument env) v2 in
      v1
  | `Let_class_exp (v1, v2, v3) ->
      let _v1 = map_value_definition env v1 in
      let tin =
        token env v2
        (* "in" *)
      in
      let v3 = map_class_expression env v3 in
      ClTodo (("LetClass", tin), [ v3 ])
  | `Let_open_class_exp (v1, v2, v3, v4) ->
      let tlet =
        token env v1
        (* "let" *)
      in
      let _v2 = map_open_module env v2 in
      let _v3 =
        token env v3
        (* "in" *)
      in
      let v4 = map_class_expression env v4 in
      ClTodo (("LetOpenClass", tlet), [ v4 ])

and map_class_field (env : env) (x : CST.class_field) : class_field =
  match x with
  | `Inhe_defi (v1, v2, v3, v4, v5) ->
      let tinherit =
        token env v1
        (* "inherit" *)
      in
      let _v2 =
        match v2 with
        | Some tok -> Some (token env tok) (* "!" *)
        | None -> None
      in
      let _v3 = map_class_expression env v3 in
      let _v4 =
        match v4 with
        | Some (v1, v2) ->
            let _v1 =
              token env v1
              (* "as" *)
            in
            let v2 = map_value_pattern env v2 in
            Some v2
        | None -> None
      in
      let _v5 = List.map (map_item_attribute env) v5 in
      CfldTodo ("Inherit", tinherit)
  | `Inst_var_defi (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let inst_tok =
        token env v1
        (* "val" *)
      in
      let _v2 =
        match v2 with
        | Some tok -> Some (token env tok) (* "!" *)
        | None -> None
      in
      let _v3 = List.map (map_anon_choice_muta_d43fe41 env) v3 in
      let inst_name =
        identifier env v4
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      let inst_type = map_typed_opt env v5 in
      let _v6 =
        match v6 with
        | Some (v1, v2) ->
            let _v1 =
              token env v1
              (* ":>" *)
            in
            let v2 = map_type_ env v2 in
            Some v2
        | None -> None
      in
      let inst_expr =
        match v7 with
        | Some (v1, v2) ->
            let _v1 =
              token env v1
              (* "=" *)
            in
            let v2 = map_sequence_expression env v2 |> seq1 in
            Some v2
        | None -> None
      in
      let _v8 = List.map (map_item_attribute env) v8 in
      InstanceVar { inst_tok; inst_name; inst_type; inst_expr }
  | `Meth_defi (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let m_tok =
        token env v1
        (* "method" *)
      in
      let _v2 =
        match v2 with
        | Some tok -> Some (token env tok) (* "!" *)
        | None -> None
      in
      let _v3 = List.map (map_anon_choice_priv_c7cc539 env) v3 in
      let m_name =
        identifier env v4
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      let m_params = List.map (map_parameter_ env) v5 in
      let m_rettype =
        match v6 with
        | Some x -> Some (map_polymorphic_typed env x |> snd)
        | None -> None
      in
      let m_body =
        match v7 with
        | Some (v1, v2) ->
            let _v1 =
              token env v1
              (* "=" *)
            in
            let v2 = map_sequence_expression env v2 |> seq1 in
            Some v2
        | None -> None
      in
      let _v8 = List.map (map_item_attribute env) v8 in
      Method { m_tok; m_name; m_params; m_rettype; m_body }
  | `Type_param_cons x ->
      let tk = map_type_parameter_constraint env x in
      CfldTodo ("Constraint", tk)
  | `Class_init (v1, v2, v3) ->
      let tinitializer =
        token env v1
        (* "initializer" *)
      in
      let _v2 = map_sequence_expression env v2 |> seq1 in
      let _v3 = List.map (map_item_attribute env) v3 in
      CfldTodo ("ClassInit", tinitializer)
  | `Choice_item_exte x ->
      let t = map_item_extension env x in
      CfldTodo t

and map_class_field_specification (env : env)
    (x : CST.class_field_specification) =
  match x with
  | `Inhe_spec (v1, v2, v3) ->
      let _v1 =
        token env v1
        (* "inherit" *)
      in
      let _v2 = map_simple_class_type env v2 in
      let _v3 = List.map (map_item_attribute env) v3 in
      ()
  | `Inst_var_spec (v1, v2, v3, v4, v5) ->
      let _v1 =
        token env v1
        (* "val" *)
      in
      let _v2 = List.map (map_anon_choice_muta_d43fe41 env) v2 in
      let _v3 =
        token env v3
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      let _v4 = map_typed env v4 in
      let _v5 = List.map (map_item_attribute env) v5 in
      ()
  | `Meth_spec (v1, v2, v3, v4, v5) ->
      let _v1 =
        token env v1
        (* "method" *)
      in
      let _v2 = List.map (map_anon_choice_priv_c7cc539 env) v2 in
      let _v3 =
        token env v3
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      let _v4 = map_polymorphic_typed env v4 in
      let _v5 = List.map (map_item_attribute env) v5 in
      ()
  | `Type_param_cons x ->
      let _v1 = map_type_parameter_constraint env x in
      ()
  | `Choice_item_exte x ->
      let _t = map_item_extension env x in
      ()

and map_class_type (env : env) (x : CST.class_type) =
  match x with
  | `Simple_class_type x ->
      let _ = map_simple_class_type env x in
      ()
  | `Class_func_type (arg, _, result) ->
      let _arg = map_argument_type env arg in
      map_class_type env result

and map_class_type_binding (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.class_type_binding) =
  let _v1 =
    match v1 with
    | Some tok -> Some (token env tok) (* "virtual" *)
    | None -> None
  in
  let _v2 =
    match v2 with
    | Some x ->
        Some
          (map_anon_LBRACK_type_param_rep_COMMA_type_param_RBRACK_cea5434 env x)
    | None -> None
  in
  let _v3 =
    token env v3
    (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  in
  let _v4 =
    token env v4
    (* "=" *)
  in
  let _v5 = map_simple_class_type env v5 in
  let _v6 = List.map (map_item_attribute env) v6 in
  ()

and map_class_type_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.class_type_definition) =
  let v1 =
    token env v1
    (* "class" *)
  in
  let _v2 =
    token env v2
    (* "type" *)
  in
  let _v3 = map_attribute_opt env v3 in
  let _v4 = map_class_type_binding env v4 in
  let _v5 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "and" *)
        in
        let v2 = map_class_type_binding env v2 in
        v2)
      v5
  in
  ItemTodo (("Class", v1), []) |> mki

and map_class_typed (env : env) ((v1, v2) : CST.class_typed) =
  let _v1 =
    token env v1
    (* ":" *)
  in
  let v2 = map_class_type env v2 in
  v2

and map_constructor_argument (env : env) (x : CST.constructor_argument) :
    type_ list =
  match x with
  | `Simple_type_rep_STAR_simple_type (v1, v2) ->
      let v1 = map_simple_type env v1 in
      let v2 =
        List.map
          (fun (v1, v2) ->
            let _v1 =
              token env v1
              (* "*" *)
            in
            let v2 = map_simple_type env v2 in
            v2)
          v2
      in
      v1 :: v2
  | `Record_decl x ->
      let l, _xs, _r = map_record_declaration env x in
      [ TyTodo (("InlineRecord", l), []) ]

and map_constructor_declaration (env : env)
    ((v1, v2) : CST.constructor_declaration) : ident * type_ list =
  let (v1 : ident) =
    match v1 with
    | `Cons_name x -> map_constructor_name env x
    | `Choice_LBRACK_RBRACK x -> (
        match x with
        | `LBRACK_RBRACK (v1, v2) ->
            let v1 =
              token env v1
              (* "[" *)
            in
            let v2 =
              token env v2
              (* "]" *)
            in
            ("[]", Tok.combine_toks v1 [ v2 ])
        | `LPAR_RPAR (v1, v2) ->
            let v1 =
              token env v1
              (* "(" *)
            in
            let v2 =
              token env v2
              (* ")" *)
            in
            ("()", Tok.combine_toks v1 [ v2 ])
        | `True tok -> str env tok (* "true" *)
        | `False tok -> str env tok (* "false" *))
  in

  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `Of_cons_arg (v1, v2) ->
            let _v1 =
              token env v1
              (* "of" *)
            in
            let v2 = map_constructor_argument env v2 in
            v2
        (* GADTs TODO! *)
        | `COLON_opt_rep1_type_var_DOT_opt_cons_arg_DASHGT_simple_type
            (v1, v2, v3, v4) ->
            let _v1 =
              token env v1
              (* ":" *)
            in
            let _v2 =
              match v2 with
              | None -> ()
              | Some (v1, v2) ->
                  let _v1 = List.map (map_type_variable env) v1 in
                  let _v2 = (* '.' *) token env v2 in
                  ()
            in
            let _v3 =
              match v3 with
              | None -> ()
              | Some (v1, v2) ->
                  let _v1 = map_constructor_argument env v1 in
                  let _v2 = (* '->' *) token env v2 in
                  ()
            in
            let _v4 = map_simple_type env v4 in
            []
        | `EQ_cons_path (v1, v2) ->
            let _v1 =
              token env v1
              (* "=" *)
            in
            let _v2 = map_constructor_path env v2 in
            [])
    | None -> []
  in
  (v1, v2)

and map_do_clause (env : env) ((v1, v2, v3) : CST.do_clause) : expr =
  let v1 =
    token env v1
    (* "do" *)
  in
  let v3 =
    token env v3
    (* "done" *)
  in
  let v2 =
    match v2 with
    | Some x -> Sequence (v1, map_sequence_expression env x, v3)
    | None -> Sequence (v1, [], v3)
  in
  v2

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let _v1 =
    token env v1
    (* "else" *)
  in
  let v2 = map_expression env v2 in
  v2

and map_exception_definition (env : env)
    ((v1, v2, v3, v4) : CST.exception_definition) =
  let v1 =
    token env v1
    (* "exception" *)
  in
  let _v2 = map_attribute_opt env v2 in
  let id, tys = map_constructor_declaration env v3 in
  let v4 = List.map (map_item_attribute env) v4 in
  { i = Exception (v1, id, tys); iattrs = v4 }

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Simple_exp x -> map_simple_expression env x
  | `Tuple_exp x -> Tuple (map_tuple_expression env x)
  | `Cons_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        str env v2
        (* "::" *)
      in
      let v3 = map_expression env v3 in
      (* TODO? Constructor or Infix? *)
      Infix (v1, v2, v3)
  | `App_exp (v1, v2) -> (
      let v1 = map_simple_expression env v1 in
      let v2 = List.map (map_argument env) v2 in
      (* TODO? parser_ml.mly has a special grammar rule
       * for constructor with arguments. In grammar.js there is
       * no such rule so we need this hack
       *)
      match (v1, v2) with
      | Constructor (x, None), [ Arg e ] -> Constructor (x, Some e)
      | _else_ -> Call (v1, v2))
  | `Infix_exp x -> map_infix_expression env x
  | `Sign_exp (v1, v2) ->
      let v1 = map_sign_operator env v1 in
      let v2 = map_expression env v2 in
      Prefix (v1, v2)
  | `Set_exp (v1, v2, v3) -> (
      let tarrow =
        token env v2
        (* "<-" *)
      in
      let e2 = map_expression env v3 in
      match v1 with
      | `Field_get_exp x ->
          let e1, t, fld = map_field_get_expression env x in
          FieldAssign (e1, t, fld, tarrow, e2)
      | `Array_get_exp x ->
          let e1 = map_array_get_expression env x in
          ExprTodo (("ExtAssign", tarrow), [ e1; e2 ])
      | `Str_get_exp x ->
          let e1 = map_string_get_expression env x in
          ExprTodo (("ExtAssign", tarrow), [ e1; e2 ])
      | `Biga_get_exp x ->
          let e1 = map_bigarray_get_expression env x in
          ExprTodo (("ExtAssign", tarrow), [ e1; e2 ])
      (* ex: data <- foo, where data is a mutable field in a class/object *)
      | `Inst_var_name tok ->
          let id = identifier env tok in
          ExprTodo (("ObjFieldAssign", tarrow), [ name_of_id id; e2 ]))
  | `If_exp (v1, v2, v3, v4, v5) ->
      let v1 =
        token env v1
        (* "if" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = map_sequence_expression env v3 |> seq1 in
      let v4 = map_then_clause env v4 in
      let v5 =
        match v5 with
        | Some x -> Some (map_else_clause env x)
        | None -> None
      in
      If (v1, v3, v4, v5)
  | `While_exp (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "while" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = map_sequence_expression env v3 |> seq1 in
      let v4 = map_do_clause env v4 in
      While (v1, v3, v4)
  | `For_exp (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        token env v1
        (* "for" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = map_value_pattern env v3 in
      let _v4 =
        token env v4
        (* "=" *)
      in
      let v5 = map_sequence_expression env v5 |> seq1 in
      let v6 =
        match v6 with
        | `To tok -> To (token env tok) (* "to" *)
        | `Downto tok -> Downto (token env tok)
        (* "downto" *)
      in
      let v7 = map_sequence_expression env v7 |> seq1 in
      let v8 = map_do_clause env v8 in
      For (v1, v3, v5, v6, v7, v8)
  | `Match_exp (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | `Match_opt_attr (v1, v2) ->
            let v1 =
              token env v1
              (* "match" *)
            in
            let _v2 = map_attribute_opt env v2 in
            v1
        | `Match_op tok -> token env tok
        (* match_operator *)
      in
      let v2 = map_sequence_expression env v2 |> seq1 in
      let _v3 =
        token env v3
        (* "with" *)
      in
      let v4 = map_match_cases env v4 in
      Match (v1, v2, v4)
  | `Func_exp (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "function" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = map_match_cases env v3 in
      Function (v1, v3)
  | `Fun_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        token env v1
        (* "fun" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = List.map (map_parameter_ env) v3 in
      let _v4TODO =
        match v4 with
        | Some x -> Some (map_simple_typed env x)
        | None -> None
      in
      let _v5 =
        token env v5
        (* "->" *)
      in
      let v6 = map_sequence_expression env v6 |> seq1 in
      Fun (v1, v3, v6)
  | `Try_exp (v1, v2, v3, v4, v5) ->
      let v1 =
        token env v1
        (* "try" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = map_sequence_expression env v3 |> seq1 in
      let _v4 =
        token env v4
        (* "with" *)
      in
      let v5 = map_match_cases env v5 in
      Try (v1, v3, v5)
  | `Let_exp (v1, v2, v3) ->
      let t, rec_opt, xs = map_value_definition env v1 in
      let _v2 =
        token env v2
        (* "in" *)
      in
      let v3 = map_sequence_expression env v3 |> seq1 in
      LetIn (t, rec_opt, xs, v3)
  | `Assert_exp (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "assert" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = map_simple_expression env v3 in
      ExprTodo (("Assert", v1), [ v3 ])
  | `Lazy_exp (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "lazy" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = map_simple_expression env v3 in
      ExprTodo (("Lazy", v1), [ v3 ])
  | `Let_module_exp (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "let" *)
      in
      let _v2 = map_module_definition env v2 in
      let _v3 =
        token env v3
        (* "in" *)
      in
      let v4 = map_sequence_expression env v4 |> seq1 in
      ExprTodo (("LocalModule", v1), [ v4 ])
  | `Let_open_exp (v1, v2, v3, v4) -> (
      let tlet =
        token env v1
        (* "let" *)
      in
      let mod_ = map_open_module env v2 in
      let tin =
        token env v3
        (* "in" *)
      in
      let e = map_sequence_expression env v4 |> seq1 in
      match mod_ with
      | { i = Open (_topen, n); _ } -> LetOpen (tlet, n, tin, e)
      | _else_ -> ExprTodo (("LetLocalOpenModExpr", tlet), [ e ]))
  | `Let_exc_exp (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "let" *)
      in
      let _v2 = map_exception_definition env v2 in
      let _v3 =
        token env v3
        (* "in" *)
      in
      let v4 = map_sequence_expression env v4 |> seq1 in
      ExprTodo (("LocalExn", v1), [ v4 ])

and map_expression_item (env : env) ((v1, v2) : CST.expression_item) =
  let v1 = map_sequence_expression env v1 |> seq1 in
  let _v2 = List.map (map_item_attribute env) v2 in
  v1

and map_extension (env : env) (x : CST.extension_) : todo_category =
  match x with
  | `Exte (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "[%" *)
      in
      let _v2 = map_attribute_id env v2 in
      let _v3 = map_attribute_payload_opt env v3 in
      let _v4 =
        token env v4
        (* "]" *)
      in
      ("Extension", v1)
  | `Quoted_exte (v1, v2, v3, (v4, v5, v6), v7) ->
      let v1 =
        token env v1
        (* "{%" *)
      in
      let _v2 = map_attribute_id env v2 in
      let _v3 =
        match v3 with
        | Some tok -> Some (token env tok) (* pattern \s+ *)
        | None -> None
      in
      let _v4 =
        token env v4
        (* left_quoted_string_delimiter *)
      in
      let _v5 =
        match v5 with
        | Some x -> Some (map_quoted_string_content env x)
        | None -> None
      in
      let _v6 =
        token env v6
        (* right_quoted_string_delimiter *)
      in
      let _v7 =
        token env v7
        (* "}" *)
      in
      ("Extension", v1)

and map_external_ (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.external_) :
    item =
  let v1 =
    token env v1
    (* "external" *)
  in
  let _v2 = map_attribute_opt env v2 in
  let v3 = map_value_name env v3 in
  let typ = map_polymorphic_typed env v4 |> snd in
  let _v5 =
    token env v5
    (* "=" *)
  in
  let v6 = List.map (map_string_ env) v6 in
  let v7 = List.map (map_item_attribute env) v7 in
  { i = External (v1, v3, typ, v6); iattrs = v7 }

and map_field_binding_pattern (env : env)
    ((v1, v2, v3) : CST.field_binding_pattern) : name * pattern =
  let v1 = map_field_path env v1 in
  let _v2TODO = map_typed_opt env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 =
          token env v1
          (* "=" *)
        in
        let v2 = map_binding_pattern env v2 in
        v2
    | None -> PatVar (snd v1)
  in
  (v1, v3)

and map_field_declaration (env : env) ((v1, v2, v3) : CST.field_declaration) =
  let v1 =
    match v1 with
    | Some tok -> Some (token env tok) (* "mutable" *)
    | None -> None
  in
  let v2 =
    identifier env v2
    (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  in
  let v3 = map_polymorphic_typed env v3 |> snd in
  (v2, v3, v1)

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) =
  let v1 = map_field_path env v1 in
  let _v2TODO = map_typed_opt env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 =
          token env v1
          (* "=" *)
        in
        let v2 = map_expression env v2 in
        v2
    | None -> name_of_id (snd v1)
  in
  (v1, v3)

and map_field_get_expression (env : env)
    ((v1, v2, v3) : CST.field_get_expression) =
  let v1 = map_simple_expression env v1 in
  let v2 =
    token env v2
    (* "." *)
  in
  let v3 = map_field_path env v3 in
  (v1, v2, v3)

(* like map_field_binding_pattern *)
and map_field_pattern (env : env) ((v1, v2, v3) : CST.field_pattern) =
  let v1 = map_field_path env v1 in
  let _v2TODO = map_typed_opt env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 =
          token env v1
          (* "=" *)
        in
        let v2 = map_pattern env v2 in
        v2
    | None -> PatVar (snd v1)
  in
  (v1, v3)

and map_floating_attribute (env : env)
    ((v1, v2, v3, v4) : CST.floating_attribute) : Tok.t =
  let v1 =
    token env v1
    (* "[@@@" *)
  in
  let _v2 = map_attribute_id env v2 in
  let _v3 = map_attribute_payload_opt env v3 in
  let _v4 =
    token env v4
    (* "]" *)
  in
  v1

and map_guard (env : env) ((v1, v2) : CST.guard) =
  let _v1 =
    token env v1
    (* "when" *)
  in
  let v2 = map_sequence_expression env v2 |> seq1 in
  v2

and map_infix_expression (env : env) (x : CST.infix_expression) : expr =
  match x with
  | `Exp_pow_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_pow_operator env v2 in
      let v3 = map_expression env v3 in
      Infix (v1, v2, v3)
  | `Exp_mult_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_mult_operator env v2 in
      let v3 = map_expression env v3 in
      Infix (v1, v2, v3)
  | `Exp_add_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_add_operator env v2 in
      let v3 = map_expression env v3 in
      Infix (v1, v2, v3)
  | `Exp_concat_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        str env v2
        (* concat_operator *)
      in
      let v3 = map_expression env v3 in
      Infix (v1, v2, v3)
  | `Exp_rel_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        str env v2
        (* rel_operator *)
      in
      let v3 = map_expression env v3 in
      Infix (v1, v2, v3)
  | `Exp_and_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_and_operator env v2 in
      let v3 = map_expression env v3 in
      Infix (v1, v2, v3)
  | `Exp_or_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_or_operator env v2 in
      let v3 = map_expression env v3 in
      Infix (v1, v2, v3)
  | `Exp_assign_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let _, t = map_assign_operator env v2 in
      let v3 = map_expression env v3 in
      RefAssign (v1, t, v3)

and map_instance_variable_expression (env : env)
    ((v1, v2) : CST.instance_variable_expression) =
  let id =
    identifier env v1
    (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let _v1 =
          token env v1
          (* "=" *)
        in
        let v2 = map_expression env v2 in
        (id, v2)
    | None -> (id, name_of_id id)
  in
  v2

and map_item_attribute (env : env) ((v1, v2, v3, v4) : CST.item_attribute) :
    attribute =
  let v1 =
    token env v1
    (* "[@@" *)
  in
  let v2 = map_attribute_id env v2 in
  let v3 = map_attribute_payload_opt env v3 in
  let v4 =
    token env v4
    (* "]" *)
  in
  NamedAttr (v1, (v2, v3), v4)

and map_item_extension (env : env) (x : CST.item_extension_) =
  match x with
  | `Item_exte (v1, v2, v3, v4, v5) ->
      let v1 =
        token env v1
        (* "[%%" *)
      in
      let _v2 = map_attribute_id env v2 in
      let _v3 = map_attribute_payload_opt env v3 in
      let _v4 =
        token env v4
        (* "]" *)
      in
      let _v5 = List.map (map_item_attribute env) v5 in
      ("Extension", v1)
  | `Quoted_item_exte (v1, v2, v3, (v4, v5, v6), v7, v8) ->
      let v1 =
        token env v1
        (* "{%%" *)
      in
      let _v2 = map_attribute_id env v2 in
      let _v3 =
        match v3 with
        | Some tok -> Some (token env tok) (* pattern \s+ *)
        | None -> None
      in
      let _v4 =
        token env v4
        (* left_quoted_string_delimiter *)
      in
      let _v5 =
        match v5 with
        | Some x -> map_quoted_string_content env x
        | None -> []
      in
      let _v6 =
        token env v6
        (* right_quoted_string_delimiter *)
      in
      let _v7 =
        token env v7
        (* "}" *)
      in
      let _v8 = List.map (map_item_attribute env) v8 in
      ("Extension", v1)

and map_labeled_argument (env : env) (x : CST.labeled_argument) =
  match x with
  | `Choice_TILDE_lowe_id x -> (
      let x = map_label env x in
      match x with
      | (true, _t), id -> ArgKwd (id, name_of_id id)
      | (false, _t), id -> ArgQuestion (id, name_of_id id))
  | `Choice_TILDE_lowe_id_imm_tok_colon_simple_exp (v0, v1, v2, v3) -> (
      let v1 = map_label env (v0, v1) in
      let _v2 =
        token env v2
        (* ":" *)
      in
      let v3 = map_simple_expression env v3 in
      match v1 with
      | (true, _t), id -> ArgKwd (id, v3)
      | (false, _t), id -> ArgQuestion (id, v3))
  | `Choice_TILDE_LPAR_lowe_id_typed_RPAR (v1, v2, v3, v4, v5) ->
      let _label_kind = map_anon_choice_TILDE_72781e5 env v1 in
      let l = (* "(" *) token env v2 in
      let _lbl_name = (* pattern "[a-z_][a-zA-Z0-9_']*" *) str env v3 in
      let _ty = map_typed env v4 in
      let _r = (* ")" *) token env v5 in
      ArgTodo ("LabelTyped", l)

and map_let_binding (env : env) ((v1, v2, v3) : CST.let_binding) : let_binding =
  let pat = map_binding_pattern env v1 in
  let lattrs = List.map (map_item_attribute env) v3 in
  match v2 with
  | Some (v1, v2, v3, v4, v5) -> (
      let lparams = List.map (map_parameter_ env) v1 in
      let tyopt =
        match v2 with
        | Some x -> Some (map_polymorphic_typed env x)
        | None -> None
      in
      let _tycastopt =
        match v3 with
        | Some (v1, v2) ->
            let _v1 =
              token env v1
              (* ":>" *)
            in
            let v2 = map_type_ env v2 in
            Some v2
        | None -> None
      in
      let teq =
        token env v4
        (* "=" *)
      in
      let lbody = map_sequence_expression env v5 |> seq1 in
      match (pat, lparams, tyopt) with
      | PatVar id, _, _ ->
          LetClassic
            {
              lname = id;
              lparams;
              lrettype = tyopt |> Option.map snd;
              lbody;
              lattrs;
            }
      | pat, [], None -> LetPattern (pat, lbody)
      | pat, [], Some (tcolon, ty) ->
          LetPattern (PatTyped (pat, tcolon, ty), lbody)
      (* TODO: grammar js is wrong there too, this can not happen *)
      | _ -> raise (Parsing_error.Other_error ("Invalid let binding", teq)))
  (* TODO: grammar.js is wrong, this can not happen *)
  | None -> raise Impossible

and map_list_binding_pattern (env : env)
    ((v1, v2, v3) : CST.list_binding_pattern) =
  let v1 =
    token env v1
    (* "[" *)
  in
  let v2 =
    match v2 with
    | Some x -> map_anon_bind_pat_rep_SEMI_bind_pat_opt_SEMI_ddad36f env x
    | None -> []
  in
  let v3 =
    token env v3
    (* "]" *)
  in
  PatList (v1, v2, v3)

and map_list_expression (env : env) ((v1, v2, v3) : CST.list_expression) =
  let v1 =
    token env v1
    (* "[" *)
  in
  let v2 =
    match v2 with
    | Some x -> map_anon_exp_rep_SEMI_exp_opt_SEMI_72c923c env x
    | None -> []
  in
  let v3 =
    token env v3
    (* "]" *)
  in
  List (v1, v2, v3)

and map_list_pattern (env : env) ((v1, v2, v3) : CST.list_pattern) =
  let v1 =
    token env v1
    (* "[" *)
  in
  let v2 =
    match v2 with
    | Some x -> map_anon_pat_rep_SEMI_pat_opt_SEMI_35217e1 env x
    | None -> []
  in
  let v3 =
    token env v3
    (* "]" *)
  in
  PatList (v1, v2, v3)

and map_match_case (env : env) ((v1, v2, v3, v4) : CST.match_case) : match_case
    =
  let v1 = map_pattern env v1 in
  let v2 =
    match v2 with
    | Some x -> Some (map_guard env x)
    | None -> None
  in
  let v3 =
    token env v3
    (* "->" *)
  in
  let v4 =
    match v4 with
    | `Seq_exp x -> map_sequence_expression env x |> seq1
    | `Refu_case tok ->
        let x = token env tok in
        ExprTodo (("RefutationCaseGADT", x), [])
    (* "." *)
  in
  (v1, (v2, v3, v4))

and map_match_cases (env : env) ((v1, v2, v3) : CST.match_cases) =
  let _v1 =
    match v1 with
    | Some tok -> Some (token env tok) (* "|" *)
    | None -> None
  in
  let v2 = map_match_case env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "|" *)
        in
        let v2 = map_match_case env v2 in
        v2)
      v3
  in
  v2 :: v3

and map_module_binding (env : env) ((v1, v2, v3, v4, v5) : CST.module_binding) =
  let v1 = map_anon_choice_module_name_ba4e400 env v1 in
  let _v2TODO = List.map (map_module_parameter env) v2 in
  let _v3TODO =
    match v3 with
    | Some x -> Some (map_module_typed env x)
    | None -> None
  in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let _v1 = map_anon_choice_EQ_4ccabd6 env v1 in
        let v2 = map_module_expression env v2 in
        v2
    | None -> ModuleTodo (("AbstractModule", snd v1), [])
  in
  let _v5 = List.map (map_item_attribute env) v5 in
  { mname = v1; mbody = v4 }

and map_module_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.module_definition) =
  let v1 =
    token env v1
    (* "module" *)
  in
  let _v2 = map_attribute_opt env v2 in
  let _v3 = rec_opt env v3 in
  let v4 = map_module_binding env v4 in
  let v5 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "and" *)
        in
        let v2 = map_module_binding env v2 in
        v2)
      v5
  in
  match v5 with
  | [] -> Module (v1, v4) |> mki
  | _ -> ItemTodo (("Modules", v1), []) |> mki

and map_module_expression (env : env) (x : CST.module_expression) : module_expr
    =
  match x with
  | `Simple_module_exp x -> map_simple_module_expression env x
  | `Module_path x ->
      let x = map_module_path env x in
      ModuleName (qualifier_to_name x)
  | `Stru_ (v1, v2, v3) ->
      let _v1 =
        token env v1
        (* "struct" *)
      in
      let v2 =
        match v2 with
        | Some x -> map_structure env x
        | None -> []
      in
      let _v3 =
        token env v3
        (* "end" *)
      in
      ModuleStruct v2
  | `Func (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "functor" *)
      in
      let _v2 = List.map (map_module_parameter env) v2 in
      let _v3 =
        token env v3
        (* "->" *)
      in
      let v4 = map_module_expression env v4 in
      ModuleTodo (("Functor", v1), [ v4 ])
  | `Module_app (v1, v2) ->
      let v1 = map_module_expression env v1 in
      let v2 =
        match v2 with
        | `Simple_module_exp x -> [ map_simple_module_expression env x ]
        | `LPAR_RPAR (v1, v2) ->
            let _v1 =
              token env v1
              (* "(" *)
            in
            let _v2 =
              token env v2
              (* ")" *)
            in
            []
      in
      ModuleTodo (("App", Tok.unsafe_fake_tok ""), v1 :: v2)

and map_module_parameter (env : env) ((v1, v2, v3) : CST.module_parameter) :
    module_parameter =
  let v1 =
    token env v1
    (* "(" *)
  in
  let param =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_module_name_ba4e400 env v1 in
        let v2 = map_module_typed env v2 in
        ModuleParam (v1, v2)
    | None -> ModuleParamUnit v1
  in
  let _v3 =
    token env v3
    (* ")" *)
  in
  param

and map_module_type (env : env) (x : CST.module_type) : module_type =
  match x with
  | `Module_type_path x ->
      let name = map_module_type_path env x in
      ModuleTyPath name
  | `Sign_ (v1, v2, v3) ->
      let _v1 =
        token env v1
        (* "sig" *)
      in
      let items =
        match v2 with
        | Some x -> map_signature env x
        | None -> []
      in
      let _v3 =
        token env v3
        (* "end" *)
      in
      ModuleTySig items
  | `Module_type_cons (v1, v2, v3, v4) ->
      let base = map_module_type env v1 in
      let _v2 =
        token env v2
        (* "with" *)
      in
      let first = map_anon_choice_cons_type_771aabb env v3 in
      let rest =
        List.concat_map
          (fun (v1, v2) ->
            let _v1 =
              token env v1
              (* "and" *)
            in
            map_anon_choice_cons_type_771aabb env v2)
          v4
      in
      ModuleTyWith (base, first @ rest)
  | `Module_type_of (v1, v2, v3, v4) ->
      let _v1 =
        token env v1
        (* "module" *)
      in
      let _v2 =
        token env v2
        (* "type" *)
      in
      let of_tok =
        token env v3
        (* "of" *)
      in
      let expr = map_module_expression env v4 in
      ModuleTyOf (of_tok, expr)
  | `Func_type (v1, v2, v3) -> (
      let arrow =
        token env v2
        (* "->" *)
      in
      let codomain = map_module_type env v3 in
      match v1 with
      | `Opt_func_rep_module_param (v1, v2) ->
          let _v1 =
            Option.map (token env) v1
            (* "functor" *)
          in
          let params = List.map (map_module_parameter env) v2 in
          ModuleTyFunctor (params, arrow, codomain)
      | `Module_type x ->
          let domain = map_module_type env x in
          ModuleTyArrow (domain, arrow, codomain))
  | `Paren_module_type (v1, v2, v3) ->
      let _v1 =
        token env v1
        (* "(" *)
      in
      let inner = map_module_type env v2 in
      let _v3 =
        token env v3
        (* ")" *)
      in
      inner
  | `Choice_exte x ->
      let t = map_extension env x in
      ModuleTyTodo t

and map_module_type_definition (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.module_type_definition) =
  let v1 =
    token env v1
    (* "module" *)
  in
  let _v2 =
    token env v2
    (* "type" *)
  in
  let _v3 = map_attribute_opt env v3 in
  let v4 = map_module_type_name env v4 in
  let body =
    match v5 with
    | Some (v1, v2) ->
        let _v1 = map_anon_choice_EQ_4ccabd6 env v1 in
        let v2 = map_module_type env v2 in
        Some v2
    | None -> None
  in
  let v6 = List.map (map_item_attribute env) v6 in
  { i = ModuleType (v1, v4, body); iattrs = v6 }

and map_module_typed (env : env) ((v1, v2) : CST.module_typed) =
  let _v1 =
    token env v1
    (* ":" *)
  in
  let v2 = map_module_type env v2 in
  v2

and map_object_copy_expression (env : env)
    ((v1, v2, v3, v4) : CST.object_copy_expression) =
  let v1 =
    token env v1
    (* "{<" *)
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_instance_variable_expression env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 =
                token env v1
                (* ";" *)
              in
              let v2 = map_instance_variable_expression env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 =
    match v3 with
    | Some tok -> Some (token env tok) (* ";" *)
    | None -> None
  in
  let _v4 =
    token env v4
    (* ">}" *)
  in
  ExprTodo (("ObjCopy", v1), v2 |> List.map snd)

and map_object_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.object_expression) : object_ =
  let o_tok =
    token env v1
    (* "object" *)
  in
  let _v2 = map_attribute_opt env v2 in
  let _v3 =
    match v3 with
    | Some (v1, v2, v3, v4) ->
        let _v1 =
          token env v1
          (* "(" *)
        in
        let _v2 = map_pattern env v2 in
        let _v3 = map_typed_opt env v3 in
        let _v4 =
          token env v4
          (* ")" *)
        in
        ()
    | None -> ()
  in
  let o_body =
    List.map
      (fun x ->
        match x with
        | `Class_field x -> map_class_field env x
        | `Floa_attr x ->
            let tk = map_floating_attribute env x in
            CfldTodo ("ClassFloatAttr", tk))
      v4
  in
  let _v5 =
    token env v5
    (* "end" *)
  in
  { o_tok; o_body }

and map_open_module (env : env) ((v1, v2, v3, v4, v5) : CST.open_module) =
  let v1 =
    token env v1
    (* "open" *)
  in
  let _v2 =
    match v2 with
    | Some tok -> Some (token env tok) (* "!" *)
    | None -> None
  in
  let _v3 = map_attribute_opt env v3 in
  let v4 = map_module_expression env v4 in
  let v5 = List.map (map_item_attribute env) v5 in
  match v4 with
  | ModuleName n -> { i = Open (v1, n); iattrs = v5 }
  | _ -> { i = ItemTodo (("OpenModExpr", v1), []); iattrs = v5 }

and map_package_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.package_expression) =
  let v1 =
    token env v1
    (* "(" *)
  in
  let _v2 =
    token env v2
    (* "module" *)
  in
  let _v3 = map_attribute_opt env v3 in
  let _v4 = map_module_expression env v4 in
  let _v5 =
    match v5 with
    | Some x -> Some (map_module_typed env x)
    | None -> None
  in
  let _v6 =
    token env v6
    (* ")" *)
  in
  ExprTodo (("Package", v1), [])

and map_package_pattern (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.package_pattern) =
  let v1 =
    token env v1
    (* "(" *)
  in
  let _v2 =
    token env v2
    (* "module" *)
  in
  let _v3 = map_attribute_opt env v3 in
  let _v4 = map_anon_choice_module_name_ba4e400 env v4 in
  let _v5 =
    match v5 with
    | Some x -> Some (map_module_typed env x)
    | None -> None
  in
  let _v6 =
    token env v6
    (* ")" *)
  in
  PatTodo (("Package", v1), [])

and map_parameter_ (env : env) (x : CST.parameter_) : parameter =
  match x with
  | `Param x -> map_parameter env x
  | `Paren_abst_type x ->
      let l, _, _ = map_parenthesized_abstract_type env x in
      ParamTodo ("Paren", l)

and map_parameter (env : env) (x : CST.parameter) =
  match x with
  | `Simple_pat x ->
      let x = map_simple_pattern env x in
      Param x
  | `Choice_TILDE_simple_value_pat x ->
      let (_bool, t), _id = map_label env x in
      ParamTodo ("Label", t)
  | `Choice_TILDE_lowe_id_imm_tok_colon_simple_pat (v0, v1, v2, v3) ->
      let (_bool, t), _id = map_label env (v0, v1) in
      let _v2 =
        token env v2
        (* ":" *)
      in
      let _v3 = map_simple_pattern env v3 in
      ParamTodo ("Label", t)
  | `Choice_TILDE_LPAR_simple_value_pat_opt_typed_opt_EQ_seq_exp_RPAR
      (v1, v2, v3, v4, v5, v6) ->
      let _bool, t = map_anon_choice_TILDE_72781e5 env v1 in
      let _v2 =
        token env v2
        (* "(" *)
      in
      let _v3 =
        token env v3
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      let _v4 = map_typed_opt env v4 in
      let _v5 =
        match v5 with
        | Some (v1, v2) ->
            let _v1 =
              token env v1
              (* "=" *)
            in
            let v2 = map_sequence_expression env v2 |> seq1 in
            Some v2
        | None -> None
      in
      let _v6 =
        token env v6
        (* ")" *)
      in
      ParamTodo ("Label", t)
  | `Choice_TILDE_lowe_id_imm_tok_colon_LPAR_pat_opt_typed_EQ_seq_exp_RPAR
      (v0, v1, v2, v3, v4, v5, v6, v7, v8) ->
      let (_bool, t), _id = map_label env (v0, v1) in
      let _v2 =
        token env v2
        (* ":" *)
      in
      let _v3 =
        token env v3
        (* "(" *)
      in
      let _v4 = map_pattern env v4 in
      let _v5 = map_typed_opt env v5 in
      let _v6 =
        token env v6
        (* "=" *)
      in
      let _v7 = map_sequence_expression env v7 |> seq1 in
      let _v8 =
        token env v8
        (* ")" *)
      in
      ParamTodo ("Label", t)

and map_parenthesized_expression (env : env) (x : CST.parenthesized_expression)
    =
  match x with
  | `Begin_opt_attr_seq_exp_end (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "begin" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = map_sequence_expression env v3 in
      let v4 =
        token env v4
        (* "end" *)
      in
      Sequence (v1, v3, v4)
  | `LPAR_seq_exp_RPAR (v1, v2, v3) -> (
      let v1 =
        token env v1
        (* "(" *)
      in
      let v2 = map_sequence_expression env v2 in
      let v3 =
        token env v3
        (* ")" *)
      in
      (* like in parser_ml.mly *)
      match v2 with
      | [] -> Sequence (v1, [], v3)
      (* Ocaml_to_generic will do the right thing if x is a tuple or
       * if this expression is part of a Constructor call.
       *)
      | [ x ] -> ParenExpr (v1, x, v3)
      | _ -> Sequence (v1, v2, v3))

and map_parenthesized_type (env : env) ((v1, v2, v3) : CST.parenthesized_type) =
  let _v1 =
    token env v1
    (* "(" *)
  in
  let v2 = map_type_ env v2 in
  let _v3 =
    token env v3
    (* ")" *)
  in
  v2

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)
and map_effect_pattern env (x : CST.effect_pattern) : pattern =
  match x with
  | `Simple_pat x -> map_simple_pattern env x
  | `Cons_pat (v1, v2, v3) -> (
      let v1 = map_constructor_path env v1 in
      let v3 = map_pattern env v3 in
      match v2 with
      | None -> PatConstructor (v1, Some v3)
      | Some x ->
          let _, (tok, names), _ = map_parenthesized_abstract_type env x in
          PatConstructorAbstract (v1, tok, names, v3))
  | `Tag_pat (v1, v2) ->
      let v1 = map_tag env v1 in
      let v2 = map_pattern env v2 in
      PatPolyVariant (v1, Some v2)
  | `Lazy_pat (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "lazy" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = map_pattern env v3 in
      PatTodo (("Lazy", v1), [ v3 ])

and map_pattern (env : env) (x : CST.pattern) : pattern =
  match x with
  | `Effect_pat x -> map_effect_pattern env x
  | `Effect_pat_ (effect_tok, pat, _, continuation) ->
      PatEffect
        ( token env effect_tok,
          map_effect_pattern env pat,
          map_simple_pattern env continuation )
  | `Alias_pat (pat, _, id) ->
      PatAs (map_pattern env pat, map_value_pattern env id)
  | `Or_pat_anon x -> map_or_pattern env x
  | `Tuple_pat x -> PatTuple (Tok.unsafe_fake_bracket (map_tuple_pattern env x))
  | `Cons_pat (left, op, right) ->
      PatConsInfix (map_pattern env left, token env op, map_pattern env right)
  | `Range_pat x -> map_range_pattern env x
  | `Exc_pat (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "exception" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = map_pattern env v3 in
      PatTodo (("Exception", v1), [ v3 ])

and map_or_binding_pattern env (x : CST.or_binding_pattern_anonymous) =
  let rec elements (left, _, right) =
    map_binding_pattern env left
    ::
    (match right with
    | `Bind_pat x -> [ map_binding_pattern env x ]
    | `Or_bind_pat_anon x -> elements x)
  in
  match elements x with
  | first :: rest -> List.fold_left (fun acc p -> PatDisj (acc, p)) first rest
  | [] -> assert false

and map_or_pattern env (x : CST.or_pattern_anonymous) =
  let rec elements (left, _, right) =
    map_pattern env left
    ::
    (match right with
    | `Pat x -> [ map_pattern env x ]
    | `Or_pat_anon x -> elements x)
  in
  match elements x with
  | first :: rest -> List.fold_left (fun acc p -> PatDisj (acc, p)) first rest
  | [] -> assert false

and map_tuple_expression env ((head, _, tail) : CST.tuple_expression) =
  let element = function
    | `Exp x -> map_expression env x
    | `Labe_tuple_elem x -> map_labeled_tuple_element env x
  in
  let head = element head in
  head
  ::
  (match tail with
  | `Tuple_exp x -> map_tuple_expression env x
  | `Exp x -> [ map_expression env x ]
  | `Labe_tuple_elem x -> [ map_labeled_tuple_element env x ])

(* A labeled tuple element is encoded as a one-field record with an internal
 * qualifier, keeping it distinct from an actual record-valued tuple element.
 * The analogous PatRecord encoding keeps binding patterns visible to analysis. *)
and map_labeled_tuple_element env (x : CST.labeled_tuple_element) =
  let label, value =
    match x with
    | `TILDE_lowe_id (_, id) ->
        let id = identifier env id in
        (id, Name ([], id))
    | `TILDE_lowe_id_imm_tok_colon_simple_exp (_, id, _, value) ->
        (identifier env id, map_simple_expression env value)
    | `TILDE_LPAR_lowe_id_typed_RPAR (_, _, id, ty, _) ->
        let id = identifier env id in
        let colon, ty = map_typed env ty in
        (id, TypedExpr (Name ([], id), colon, ty))
  in
  Record
    ( None,
      Tok.unsafe_fake_bracket
        [ (([ ("!tuple-label!", snd label) ], label), value) ] )

and map_tuple_pattern env ((head, _, tail) : CST.tuple_pattern) =
  let element = function
    | `Pat x -> map_pattern env x
    | `Labe_tuple_elem_pat x -> map_labeled_tuple_element_pattern env x
  in
  let head = element head in
  head
  ::
  (match tail with
  | `Tuple_pat x -> map_tuple_pattern env x
  | `Pat x -> [ map_pattern env x ]
  | `Labe_tuple_elem_pat x -> [ map_labeled_tuple_element_pattern env x ])

and map_labeled_tuple_element_pattern env
    (x : CST.labeled_tuple_element_pattern) =
  let label, value =
    match x with
    | `TILDE_lowe_id (_, id) ->
        let id = identifier env id in
        (id, PatVar id)
    | `TILDE_lowe_id_imm_tok_colon_simple_pat (_, id, _, value) ->
        (identifier env id, map_simple_pattern env value)
    | `TILDE_LPAR_lowe_id_typed_RPAR (_, _, id, ty, _) ->
        let id = identifier env id in
        let colon, ty = map_typed env ty in
        (id, PatTyped (PatVar id, colon, ty))
  in
  PatRecord
    (Tok.unsafe_fake_bracket
       [ (([ ("!tuple-label!", snd label) ], label), value) ])

and map_tuple_binding_pattern env ((head, _, tail) : CST.tuple_binding_pattern)
    =
  let element = function
    | `Bind_pat x -> map_binding_pattern env x
    | `Labe_tuple_elem_bind_pat x ->
        map_labeled_tuple_element_binding_pattern env x
  in
  let head = element head in
  head
  ::
  (match tail with
  | `Tuple_bind_pat x -> map_tuple_binding_pattern env x
  | `Bind_pat x -> [ map_binding_pattern env x ]
  | `Labe_tuple_elem_bind_pat x ->
      [ map_labeled_tuple_element_binding_pattern env x ])

and map_labeled_tuple_element_binding_pattern env
    (x : CST.labeled_tuple_element_binding_pattern) =
  let label, value =
    match x with
    | `TILDE_lowe_id (_, id) ->
        let id = identifier env id in
        (id, PatVar id)
    | `TILDE_lowe_id_imm_tok_colon_simple_bind_pat (_, id, _, value) ->
        (identifier env id, map_simple_binding_pattern env value)
    | `TILDE_LPAR_lowe_id_typed_RPAR (_, _, id, ty, _) ->
        let id = identifier env id in
        let colon, ty = map_typed env ty in
        (id, PatTyped (PatVar id, colon, ty))
  in
  PatRecord
    (Tok.unsafe_fake_bracket
       [ (([ ("!tuple-label!", snd label) ], label), value) ])

and map_polymorphic_type (env : env) (x : CST.polymorphic_type) : type_ =
  match x with
  | `Poly_type_ (v1, v2, v3) ->
      let _v1 =
        match v1 with
        | `Rep1_type_var xs ->
            let _xs = List.map (map_type_variable env) xs in
            ()
        | `Abst_type x ->
            let _x = map_abstract_type env x in
            ()
      in
      let v2 =
        token env v2
        (* "." *)
      in
      let v3 = map_type_ env v3 in
      TyTodo (("Forall", v2), [ v3 ])
  | `Type x -> map_type_ env x

and map_polymorphic_typed (env : env) ((v1, v2) : CST.polymorphic_typed) =
  let tcolon =
    token env v1
    (* ":" *)
  in
  let ty = map_polymorphic_type env v2 in
  (tcolon, ty)

and map_record_binding_pattern (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.record_binding_pattern) =
  let v1 =
    token env v1
    (* "{" *)
  in
  let _v2 = map_field_binding_pattern env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* ";" *)
        in
        let v2 = map_field_binding_pattern env v2 in
        v2)
      v3
  in
  let _v4 =
    match v4 with
    | Some (v1, v2) ->
        let _v1 =
          token env v1
          (* ";" *)
        in
        let v2 =
          token env v2
          (* "_" *)
        in
        Some v2
    | None -> None
  in
  let _v5 =
    match v5 with
    | Some tok -> Some (token env tok) (* ";" *)
    | None -> None
  in
  let v6 =
    token env v6
    (* "}" *)
  in
  PatRecord (v1, v3, v6)

and map_record_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.record_declaration) =
  let v1 =
    token env v1
    (* "{" *)
  in
  let v2 = map_field_declaration env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* ";" *)
        in
        let v2 = map_field_declaration env v2 in
        v2)
      v3
  in
  let _v4 =
    match v4 with
    | Some tok -> Some (token env tok) (* ";" *)
    | None -> None
  in
  let v5 =
    token env v5
    (* "}" *)
  in
  (v1, v2 :: v3, v5)

and map_record_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.record_expression) =
  let v1 =
    token env v1
    (* "{" *)
  in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_simple_expression env v1 in
        let _v2 =
          token env v2
          (* "with" *)
        in
        Some v1
    | None -> None
  in
  let v3 = map_field_expression env v3 in
  let v4 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* ";" *)
        in
        let v2 = map_field_expression env v2 in
        v2)
      v4
  in
  let _v5 =
    match v5 with
    | Some tok -> Some (token env tok) (* ";" *)
    | None -> None
  in
  let v6 =
    token env v6
    (* "}" *)
  in
  Record (v2, (v1, v3 :: v4, v6))

and map_record_pattern (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.record_pattern) =
  let v1 =
    token env v1
    (* "{" *)
  in
  let v2 = map_field_pattern env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* ";" *)
        in
        let v2 = map_field_pattern env v2 in
        v2)
      v3
  in
  let _v4 =
    match v4 with
    | Some (v1, v2) ->
        let _v1 =
          token env v1
          (* ";" *)
        in
        let v2 =
          token env v2
          (* "_" *)
        in
        Some v2
    | None -> None
  in
  let _v5 =
    match v5 with
    | Some tok -> Some (token env tok) (* ";" *)
    | None -> None
  in
  let v6 =
    token env v6
    (* "}" *)
  in
  PatRecord (v1, v2 :: v3, v6)

and map_sequence_expression (env : env) (x : CST.sequence_expression) :
    expr list =
  match x with
  | `Exp x -> [ map_expression env x ]
  | `Seq_exp_anon (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let _v2 =
        token env v2
        (* ";" *)
      in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let _v1 = map_attribute_opt env v1 in
            let v2 = map_sequence_expression env v2 in
            v2
        | None -> []
      in
      v1 :: v3

and map_signature (env : env) (x : CST.signature) : item list =
  match x with
  | `Rep1_SEMISEMI xs ->
      let _xs = List.map (token env) (* ";;" *) xs in
      []
  | `Rep1_rep_SEMISEMI_sign_item_rep_SEMISEMI (v1, v2) ->
      let v1 =
        List.map
          (fun (v1, v2) ->
            let _v1 = List.map (token env) (* ";;" *) v1 in
            let v2 = map_signature_item env v2 in
            v2)
          v1
      in
      let _v2 = List.map (token env) (* ";;" *) v2 in
      v1

and map_signature_item (env : env) (x : CST.signature_item) : item =
  match x with
  | `Value_spec (v1, v2, v3, v4, v5) ->
      let v1 =
        token env v1
        (* "val" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = map_value_name env v3 in
      let v4 = map_polymorphic_typed env v4 |> snd in
      let v5 = List.map (map_item_attribute env) v5 in
      { i = Val (v1, v3, v4); iattrs = v5 }
  | `Exte x -> map_external_ env x
  | `Type_defi x -> map_type_definition env x
  | `Exc_defi x -> map_exception_definition env x
  | `Module_defi x -> map_module_definition env x
  | `Module_type_defi x -> map_module_type_definition env x
  | `Open_module x -> map_open_module env x
  | `Incl_module_type (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "include" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let _v3 = map_module_type env v3 in
      let v4 = List.map (map_item_attribute env) v4 in
      { i = ItemTodo (("Include", v1), []); iattrs = v4 }
  | `Class_defi x -> map_class_definition env x
  | `Class_type_defi x -> map_class_type_definition env x
  | `Floa_attr x ->
      let tk = map_floating_attribute env x in
      ItemTodo (("Attr", tk), []) |> mki
  | `Choice_item_exte x ->
      let t = map_item_extension env x in
      ItemTodo (t, []) |> mki

and map_simple_class_expression (env : env) (x : CST.simple_class_expression) =
  match x with
  | `Class_path x ->
      let _, id = map_class_path env x in
      ClTodo (("ClassPath", snd id), [])
  | `Inst_class (v1, v2, v3, v4, v5) ->
      let lb =
        token env v1
        (* "[" *)
      in
      let _v2 = map_type_ env v2 in
      let _v3 =
        List.map
          (fun (v1, v2) ->
            let _v1 =
              token env v1
              (* "," *)
            in
            let v2 = map_type_ env v2 in
            v2)
          v3
      in
      let _rb =
        token env v4
        (* "]" *)
      in
      let _v5 = map_class_path env v5 in
      ClTodo (("InstClass", lb), [])
  | `Obj_exp x ->
      let x = map_object_expression env x in
      ClObj x
  | `Typed_class_exp (v1, v2, v3, v4) ->
      let _lp =
        token env v1
        (* "(" *)
      in
      let e = map_class_expression env v2 in
      let _v3 = map_class_typed env v3 in
      let _rp =
        token env v4
        (* ")" *)
      in
      e
  | `Paren_class_exp (v1, v2, v3) ->
      let _lp =
        token env v1
        (* "(" *)
      in
      let e = map_class_expression env v2 in
      let _rp =
        token env v3
        (* ")" *)
      in
      e
  | `Choice_exte x ->
      let todo = map_extension env x in
      ClTodo (todo, [])

and map_simple_class_type (env : env) (x : CST.simple_class_type) =
  match x with
  | `Class_type_path x ->
      let _x = map_class_type_path env x in
      ()
  | `Inst_class_type (v1, v2, v3, v4, v5) ->
      let _v1 =
        token env v1
        (* "[" *)
      in
      let _v2 = map_type_ env v2 in
      let _v3 =
        List.map
          (fun (v1, v2) ->
            let _v1 =
              token env v1
              (* "," *)
            in
            let v2 = map_type_ env v2 in
            v2)
          v3
      in
      let _v4 =
        token env v4
        (* "]" *)
      in
      let _v5 = map_class_type_path env v5 in
      ()
  | `Class_body_type (v1, v2, v3, v4) ->
      let _v1 =
        token env v1
        (* "object" *)
      in
      let _v2 =
        match v2 with
        | Some x -> Some (map_parenthesized_type env x)
        | None -> None
      in
      let _v3 =
        List.map
          (fun x ->
            match x with
            | `Class_field_spec x ->
                let _ = map_class_field_specification env x in
                ()
            | `Floa_attr x ->
                let _x = map_floating_attribute env x in
                ())
          v3
      in
      let _v4 =
        token env v4
        (* "end" *)
      in
      ()
  | `Let_open_class_type (v1, v2, v3, v4) ->
      let _v1 =
        token env v1
        (* "let" *)
      in
      let _v2 = map_open_module env v2 in
      let _v3 =
        token env v3
        (* "in" *)
      in
      let _v4 = map_simple_class_type env v4 in
      ()
  | `Choice_exte x ->
      let _x = map_extension env x in
      ()

and map_simple_expression (env : env) (x : CST.simple_expression) : expr =
  match x with
  | `Obj_exp x -> Obj (map_object_expression env x)
  | `Value_path x ->
      let x = map_value_path env x in
      Name x
  | `Cst x ->
      let x = map_constant env x in
      L x
  | `Typed_exp (v1, v2, v3, v4) ->
      let _v1 =
        token env v1
        (* "(" *)
      in
      let v2 = map_sequence_expression env v2 |> seq1 in
      let t, v3 = map_typed env v3 in
      let _v4 =
        token env v4
        (* ")" *)
      in
      TypedExpr (v2, t, v3)
  | `Cons_path x ->
      let x = map_constructor_path env x in
      Constructor (x, None)
  | `Tag x ->
      let x = map_tag env x in
      PolyVariant (x, None)
  | `List_exp x -> map_list_expression env x
  | `Array_exp x -> map_array_expression env x
  | `Record_exp x -> map_record_expression env x
  | `Prefix_exp (v1, v2) -> (
      let v1 =
        str env v1
        (* prefix_operator *)
      in
      let v2 = map_simple_expression env v2 in
      match v1 with
      | "!", tk -> RefAccess (tk, v2)
      | _else_ -> Prefix (v1, v2))
  | `Hash_exp (v1, v2, v3) ->
      let v1 = map_simple_expression env v1 in
      let _op, v2 = map_hash_operator env v2 in
      let v3 = map_simple_expression env v3 in
      ExprTodo (("Hash", v2), [ v1; v3 ])
  | `Field_get_exp x ->
      let a, b, c = map_field_get_expression env x in
      FieldAccess (a, b, c)
  | `Array_get_exp x -> map_array_get_expression env x
  | `Str_get_exp x -> map_string_get_expression env x
  | `Biga_get_exp x -> map_bigarray_get_expression env x
  | `Coer_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        token env v1
        (* "(" *)
      in
      let v2 = map_sequence_expression env v2 |> seq1 in
      let _v3 = map_typed_opt env v3 in
      let _v4 =
        token env v4
        (* ":>" *)
      in
      let _v5 = map_type_ env v5 in
      let _v6 =
        token env v6
        (* ")" *)
      in
      ExprTodo (("Coerce", v1), [ v2 ])
  | `Local_open_exp (v1, v2, v3) -> (
      let qu = map_module_path env v1 in
      let n = AST_ocaml.qualifier_to_name qu in
      let tdot =
        token env v2
        (* "." *)
      in
      match v3 with
      | `LPAR_opt_seq_exp_RPAR (v1, v2, v3) ->
          let lp =
            token env v1
            (* "(" *)
          in
          let rp =
            token env v3
            (* ")" *)
          in
          let xs =
            match v2 with
            | Some e -> map_sequence_expression env e
            | None -> []
          in
          let e =
            match xs with
            | [ x ] -> ParenExpr (lp, x, rp)
            | xs -> Sequence (lp, xs, rp)
          in
          LocalOpen (n, tdot, e)
      (* those do not require the extra () as they already use
       * delimiters (e.g., '[]' for map_list_expression)
       *)
      | `List_exp x ->
          let e = map_list_expression env x in
          LocalOpen (n, tdot, e)
      | `Array_exp x ->
          let e = map_array_expression env x in
          LocalOpen (n, tdot, e)
      | `Record_exp x ->
          let e = map_record_expression env x in
          LocalOpen (n, tdot, e)
      | `Obj_copy_exp x ->
          let e = map_object_copy_expression env x in
          LocalOpen (n, tdot, e)
      | `Pack_exp x ->
          let e = map_package_expression env x in
          LocalOpen (n, tdot, e))
  | `Pack_exp x -> map_package_expression env x
  | `New_exp (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "new" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let v3 = map_class_path env v3 in
      New (v1, v3)
  | `Obj_copy_exp x -> map_object_copy_expression env x
  | `Meth_invo (v1, v2, v3) ->
      let v1 = map_simple_expression env v1 in
      let v2 =
        token env v2
        (* "#" *)
      in
      let v3 =
        identifier env v3
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      ObjAccess (v1, v2, v3)
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Ocam_value tok ->
      let t = token env tok in
      ExprTodo (("OCamlYacc", t), [])
  (* pattern \$[0-9]+ *)
  | `Choice_exte x ->
      let t = map_extension env x in
      ExprTodo (t, [])

and map_simple_module_expression (env : env) (x : CST.simple_module_expression)
    =
  match x with
  | `Typed_module_exp (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "(" *)
      in
      let v2 = map_module_expression env v2 in
      let _v3 = map_module_typed env v3 in
      let _v4 =
        token env v4
        (* ")" *)
      in
      ModuleTodo (("ModuleTyped", v1), [ v2 ])
  | `Paren_module_exp (v1, v2, v3) ->
      let _v1 =
        token env v1
        (* "(" *)
      in
      let v2 = map_module_expression env v2 in
      let _v3 =
        token env v3
        (* ")" *)
      in
      v2
  | `Packed_module (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        token env v1
        (* "(" *)
      in
      let _v2 =
        token env v2
        (* "val" *)
      in
      let _v3 = map_expression env v3 in
      let _v4 =
        match v4 with
        | Some x -> Some (map_module_typed env x)
        | None -> None
      in
      let _v5 =
        match v5 with
        | Some (v1, v2) ->
            let _v1 =
              token env v1
              (* ":>" *)
            in
            let v2 = map_module_type env v2 in
            Some v2
        | None -> None
      in
      let _v6 =
        token env v6
        (* ")" *)
      in
      ModuleTodo (("Package", v1), [])
  | `Choice_exte x ->
      let t = map_extension env x in
      ModuleTodo (t, [])

(* diff with map_binding_pattern? no Or, as, here *)

and map_simple_pattern (env : env) (x : CST.simple_pattern) : pattern =
  match x with
  | `Effect tok -> PatVar (str env tok)
  | `Choice_choice_simple_value_pat x -> (
      match x with
      | `Choice_simple_value_pat x -> (
          let x = map_value_pattern env x in
          match x with
          | "_", tk -> PatUnderscore tk
          | _else_ -> PatVar x)
      | `Signed_cst x ->
          let x = map_signed_constant env x in
          PatLiteral x
      | `Typed_pat (v1, v2, v3, v4) ->
          let _v1 =
            token env v1
            (* "(" *)
          in
          let v2 = map_pattern env v2 in
          let t, v3 = map_typed env v3 in
          let _v4 =
            token env v4
            (* ")" *)
          in
          PatTyped (v2, t, v3)
      | `Cons_path x ->
          let x = map_constructor_path env x in
          PatConstructor (x, None)
      | `Tag x ->
          let x = map_tag env x in
          PatPolyVariant (x, None)
      | `Poly_vari_pat x -> map_polymorphic_variant_pattern env x
      | `Record_pat x -> map_record_pattern env x
      | `List_pat x -> map_list_pattern env x
      | `Array_pat x -> map_array_pattern env x
      | `Local_open_pat (v1, v2, v3) ->
          let _v1TODO = map_module_path env v1 in
          let v2 =
            token env v2
            (* "." *)
          in
          let v3 =
            match v3 with
            | `LPAR_opt_pat_RPAR (v1, v2, v3) ->
                let v1 =
                  token env v1
                  (* "(" *)
                in
                let _v3 =
                  token env v3
                  (* ")" *)
                in
                let v2 =
                  match v2 with
                  | Some x -> PatTodo (("LocalOpen", v1), [ map_pattern env x ])
                  | None -> PatTodo (("LocalOpen", v1), [])
                in
                v2
            | `List_pat x -> map_list_pattern env x
            | `Array_pat x -> map_array_pattern env x
            | `Record_pat x -> map_record_pattern env x
          in
          PatTodo (("LocalOpen", v2), [ v3 ])
      | `Pack_pat x -> map_package_pattern env x
      | `Paren_pat (v1, v2, v3) -> (
          let v1 =
            token env v1
            (* "(" *)
          in
          let v2 = map_pattern env v2 in
          let v3 =
            token env v3
            (* ")" *)
          in
          (* putting real tokens on Tuples *)
          match v2 with
          | PatTuple (_, xs, _) -> PatTuple (v1, xs, v3)
          | p -> p)
      | `Choice_exte x -> PatTodo (map_extension env x, []))

and map_simple_type (env : env) (x : CST.simple_type) : type_ =
  match x with
  | `Local_open_type (path, dot, inner) ->
      let path = map_type_open_path env path in
      let ty =
        match inner with
        | `LPAR_type_RPAR x -> map_parenthesized_type env x
        | `Pack_type x -> map_simple_type env (`Pack_type x)
        | `Poly_vari_type x -> map_simple_type env (`Poly_vari_type x)
      in
      TyLocalOpen (path, token env dot, ty)
  | `Type_var x ->
      let x = map_type_variable env x in
      TyVar x
  | `Type_cons_path x ->
      let x = map_type_constructor_path env x in
      TyName x
  | `Cons_type (v1, v2) ->
      let v1 = map_anon_choice_simple_type_9a0619a env v1 in
      let v2 = map_type_constructor_path env v2 in
      TyApp (v1, v2)
  | `Poly_vari_type v1 -> (
      match v1 with
      | `LBRACK_tag_spec_RBRACK (v1, v2, v3) ->
          let v1 =
            token env v1
            (* "[" *)
          in
          let tag = map_tag_specification env v2 in
          let _v3 =
            token env v3
            (* "]" *)
          in
          TyTodo (("Poly", v1), [ tag ])
      | `LBRACK_opt_tag_spec_BAR_tag_spec_rep_BAR_tag_spec_RBRACK
          (v1, v2, v3, v4, v5, v6) ->
          let v1 =
            token env v1
            (* "[" *)
          in
          let leading =
            match v2 with
            | Some x -> [ map_tag_spec env x ]
            | None -> []
          in
          let _v3 =
            token env v3
            (* "|" *)
          in
          let first = map_tag_spec env v4 in
          let rest =
            List.map
              (fun (v1, v2) ->
                let _v1 =
                  token env v1
                  (* "|" *)
                in
                map_tag_spec env v2)
              v5
          in
          let _v6 =
            token env v6
            (* "]" *)
          in
          TyTodo (("Poly", v1), leading @ (first :: rest))
      | `LBRACKGT_opt_BAR_opt_tag_spec_rep_BAR_tag_spec_RBRACK (v1, v2, v3, v4)
        ->
          let v1 =
            token env v1
            (* "[>" *)
          in
          let _v2 =
            match v2 with
            | Some tok -> Some (token env tok) (* "|" *)
            | None -> None
          in
          let tags =
            match v3 with
            | Some (v1, v2) ->
                let v1 = map_tag_spec env v1 in
                let v2 =
                  List.map
                    (fun (v1, v2) ->
                      let _v1 =
                        token env v1
                        (* "|" *)
                      in
                      map_tag_spec env v2)
                    v2
                in
                v1 :: v2
            | None -> []
          in
          let _v4 =
            token env v4
            (* "]" *)
          in
          TyTodo (("Poly", v1), tags)
      | `LBRACKLT_opt_BAR_tag_spec_rep_BAR_tag_spec_opt_GT_rep1_tag_RBRACK
          (v1, v2, v3, v4, v5, v6) ->
          let v1 =
            token env v1
            (* "[<" *)
          in
          let _v2 =
            match v2 with
            | Some tok -> Some (token env tok) (* "|" *)
            | None -> None
          in
          let first = map_tag_spec env v3 in
          let rest =
            List.map
              (fun (v1, v2) ->
                let _v1 =
                  token env v1
                  (* "|" *)
                in
                map_tag_spec env v2)
              v4
          in
          let extra =
            match v5 with
            | Some (v1, v2) ->
                let _v1 =
                  token env v1
                  (* ">" *)
                in
                List.map (fun tag -> type_of_tag (map_tag env tag)) v2
            | None -> []
          in
          let _v6 =
            token env v6
            (* "]" *)
          in
          TyTodo (("Poly", v1), (first :: rest) @ extra))
  | `Pack_type (v1, v2, v3, v4, v5) ->
      let v1 =
        token env v1
        (* "(" *)
      in
      let _v2 =
        token env v2
        (* "module" *)
      in
      let _v3 = map_attribute_opt env v3 in
      let mty = map_module_type env v4 in
      let _v5 =
        token env v5
        (* ")" *)
      in
      TyTodo (("Package", v1), module_type_as_types mty)
  | `Hash_type (v1, v2, v3) ->
      let _v1 =
        match v1 with
        | Some x -> Some (map_anon_choice_simple_type_9a0619a env x)
        | None -> None
      in
      let v2 =
        token env v2
        (* "#" *)
      in
      let _v3 = map_class_type_path env v3 in
      TyTodo (("Hash", v2), [])
  | `Obj_type (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "<" *)
      in
      let _v2 =
        match v2 with
        | Some x -> (
            match x with
            | `Choice_meth_type_rep_SEMI_choice_meth_type_opt_SEMI_opt_DOTDOT
                (v1, v2, v3) ->
                let _v1 = map_anon_choice_meth_type_d8c4bdc env v1 in
                let _v2 =
                  List.map
                    (fun (v1, v2) ->
                      let _v1 =
                        token env v1
                        (* ";" *)
                      in
                      let v2 = map_anon_choice_meth_type_d8c4bdc env v2 in
                      v2)
                    v2
                in
                let _v3 =
                  match v3 with
                  | Some (v1, v2) ->
                      let _v1 =
                        token env v1
                        (* ";" *)
                      in
                      let _v2 =
                        match v2 with
                        | Some tok -> Some (token env tok) (* ".." *)
                        | None -> None
                      in
                      ()
                  | None -> ()
                in
                ()
            | `DOTDOT tok ->
                let _t =
                  token env tok
                  (* ".." *)
                in
                ())
        | None -> ()
      in
      let _v3 =
        token env v3
        (* ">" *)
      in
      TyTodo (("Object", v1), [])
  | `Paren_type x -> map_parenthesized_type env x
  | `Choice_exte x ->
      let t = map_extension env x in
      TyTodo (t, [])

and map_simple_typed (env : env) ((v1, v2) : CST.simple_typed) : type_ =
  let _v1 =
    token env v1
    (* ":" *)
  in
  let v2 = map_simple_type env v2 in
  v2

and map_string_get_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.string_get_expression) =
  let v1 = map_simple_expression env v1 in
  let v2 =
    token env v2
    (* "." *)
  in
  let _v3 =
    match v3 with
    | Some x -> Some (map_indexing_operator_path env x)
    | None -> None
  in
  let _v4 =
    token env v4
    (* "[" *)
  in
  let v5 = map_sequence_expression env v5 |> seq1 in
  let _v6 =
    token env v6
    (* "]" *)
  in
  ExprTodo (("String", v2), [ v1; v5 ])

(*****************************************************************************)
(* Structure *)
(*****************************************************************************)
and map_structure (env : env) (x : CST.structure) : item list =
  match x with
  | `Rep1_SEMISEMI xs ->
      let _xs = List.map (token env) (* ";;" *) xs in
      []
  | `Rep_SEMISEMI_choice_stru_item_rep_choice_rep_SEMISEMI_choice_stru_item_rep_SEMISEMI
      (v1, v2, v3, v4) ->
      let _v1 = List.map (token env) (* ";;" *) v1 in
      let v2 =
        match v2 with
        | `Stru_item x -> map_structure_item env x
        | `Topl_dire x -> map_toplevel_directive env x
        | `Exp_item x ->
            let x = map_expression_item env x in
            TopExpr x |> mki
      in
      let v3 =
        List.map
          (fun x ->
            match x with
            | `Rep_SEMISEMI_choice_stru_item (v1, v2) ->
                let _v1 = List.map (token env) (* ";;" *) v1 in
                let v2 =
                  match v2 with
                  | `Stru_item x -> map_structure_item env x
                  | `Topl_dire x -> map_toplevel_directive env x
                in
                v2
            | `Rep1_SEMISEMI_exp_item (v1, v2) ->
                let _v1 = List.map (token env) (* ";;" *) v1 in
                let v2 = map_expression_item env v2 in
                TopExpr v2 |> mki)
          v3
      in
      let _v4 = List.map (token env) (* ";;" *) v4 in
      v2 :: v3

and map_structure_item (env : env) (x : CST.structure_item) : item =
  match x with
  | `Value_defi x ->
      let a, b, c = map_value_definition env x in
      Let (a, b, c) |> mki
  | `Exte x -> map_external_ env x
  | `Type_defi x -> map_type_definition env x
  | `Exc_defi x -> map_exception_definition env x
  | `Module_defi x -> map_module_definition env x
  | `Module_type_defi x -> map_module_type_definition env x
  | `Open_module x -> map_open_module env x
  | `Incl_module (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "include" *)
      in
      let _v2 = map_attribute_opt env v2 in
      let _v3 = map_module_expression env v3 in
      let v4 = List.map (map_item_attribute env) v4 in
      { i = ItemTodo (("Include", v1), []); iattrs = v4 }
  | `Class_defi x -> map_class_definition env x
  | `Class_type_defi x -> map_class_type_definition env x
  | `Floa_attr x ->
      let tk = map_floating_attribute env x in
      ItemTodo (("Attr", tk), []) |> mki
  | `Choice_item_exte x ->
      let t = map_item_extension env x in
      ItemTodo (t, []) |> mki

and map_tag_spec (env : env) (x : CST.tag_spec) : type_ =
  match x with
  | `Type x -> map_type_ env x
  | `Tag_spec x -> map_tag_specification env x

and map_tag_specification (env : env) ((v1, v2) : CST.tag_specification) : type_
    =
  let tick, id = map_tag env v1 in
  let of_tys =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let _v1 =
          token env v1
          (* "of" *)
        in
        let _v2 =
          match v2 with
          | Some tok -> Some (token env tok) (* "&" *)
          | None -> None
        in
        let first = map_type_ env v3 in
        let rest =
          List.map
            (fun (v1, v2) ->
              let _v1 =
                token env v1
                (* "&" *)
              in
              map_type_ env v2)
            v4
        in
        first :: rest
    | None -> []
  in
  let tag = ("`" ^ fst id, Tok.combine_toks tick [ snd id ]) in
  match of_tys with
  | [] -> TyName ([], tag)
  | tys -> TyApp (Tok.unsafe_fake_bracket tys, ([], tag))

and map_then_clause (env : env) ((v1, v2) : CST.then_clause) =
  let _v1 =
    token env v1
    (* "then" *)
  in
  let v2 = map_expression env v2 in
  v2

and map_tuple_type_rhs (env : env) ((head, tail) : CST.tuple_type_rhs) =
  let head =
    match head with
    | `Simple_type x -> map_simple_type env x
    | `Labe_tuple_elem_type x -> map_labeled_tuple_element_type env x
  in
  head
  ::
  (match tail with
  | None -> []
  | Some (_, `Tuple_type_rhs x) -> map_tuple_type_rhs env x)

and map_labeled_tuple_element_type env
    ((label, _, ty) : CST.labeled_tuple_element_type) =
  TyLabeled (identifier env label, map_simple_type env ty)

and map_proper_tuple_type env ((head, _, tail) : CST.proper_tuple_type) =
  TyTuple (map_simple_type env head :: map_tuple_type_rhs env tail)

and map_argument_type env (x : CST.argument_type) =
  match x with
  | `Simple_type x -> map_simple_type env x
  | `Proper_tuple_type x -> map_proper_tuple_type env x
  | `Labe_arg_type (_, _, colon, ty) ->
      TyTodo (("Label", token env colon), [ map_argument_type env ty ])

and map_type_ (env : env) (x : CST.type_) : type_ =
  match x with
  | `Simple_type x -> map_simple_type env x
  | `Proper_tuple_type x -> map_proper_tuple_type env x
  | `Labe_tuple_type (head, _, tail) ->
      TyTuple
        (map_labeled_tuple_element_type env head :: map_tuple_type_rhs env tail)
  | `Func_type (v1, v2, v3) ->
      let v1 = map_argument_type env v1 in
      let _v2 =
        token env v2
        (* "->" *)
      in
      let v3 = map_type_ env v3 in
      TyFunction (v1, v3)
  | `Alia_type (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 =
        token env v2
        (* "as" *)
      in
      let _v3 = map_type_variable env v3 in
      TyTodo (("Alias", v2), [ v1 ])

and private_opt env v =
  match v with
  | None -> None
  | Some tok -> Some (token env tok)

and map_type_binding (env : env) ((v1, v2, v3) : CST.type_binding) :
    type_declaration =
  let tparams =
    match v1 with
    | Some x -> Some (map_type_params env x)
    | None -> None
  in
  let v2 =
    match v2 with
    | `Lowe_id_opt_type_equa_opt_EQ_opt_priv_choice_vari_decl_rep_type_cons
        (v1, v2, v3, v4) ->
        let id =
          identifier env v1
          (* pattern "[a-z_][a-zA-Z0-9_']*" *)
        in
        (* in OCaml you can now have type x = X.t = ...,
         * but just 'type x = foo' is parsed as a map_type_equation
         *)
        let ty_alias_opt =
          match v2 with
          | Some x -> Some (map_type_equation env x)
          | None -> None
        in
        let ty_kind_opt =
          match v3 with
          | Some (v1, v2, v3) ->
              let _v1 =
                token env v1
                (* "=" *)
              in
              let _v2 = private_opt env v2 in
              let v3 =
                match v3 with
                | `Vari_decl x ->
                    let xs = map_variant_declaration env x in
                    Some (AlgebraicType xs)
                | `Record_decl x ->
                    let xs = map_record_declaration env x in
                    Some (RecordType xs)
                | `DOTDOT tok ->
                    let _x = token env tok in
                    (* ".." ??? TODO *)
                    Some AbstractType
              in
              v3
          | None -> None
        in
        let _v4 = List.map (map_type_constraint env) v4 in
        let tbody =
          match (ty_alias_opt, ty_kind_opt) with
          | Some (_teq, ty), None -> CoreType ty
          | None, Some kind -> kind
          | None, None -> AbstractType
          | Some _equationTODO, Some kind -> kind
        in
        TyDecl { tname = id; tparams; tbody }
    | `Type_cons_path_PLUSEQ_opt_priv_vari_decl (v1, v2, v3, v4) ->
        let _v1 = map_type_constructor_path env v1 in
        let v2 =
          token env v2
          (* "+=" *)
        in
        let _v3 = private_opt env v3 in
        let _v4 = map_variant_declaration env v4 in
        TyDeclTodo ("ExtensionType", v2)
  in
  let _v3 = List.map (map_item_attribute env) v3 in
  v2

and map_type_constraint (env : env) ((v1, v2, v3, v4) : CST.type_constraint) =
  let _v1 =
    token env v1
    (* "constraint" *)
  in
  let _v2 = map_type_ env v2 in
  let _v3 =
    token env v3
    (* "=" *)
  in
  let _v4 = map_type_ env v4 in
  ()

and map_type_definition (env : env) ((v1, v2, v3, v4, v5) : CST.type_definition)
    =
  let v1 =
    token env v1
    (* "type" *)
  in
  let _v2 = map_attribute_opt env v2 in
  let _v3 =
    match v3 with
    | Some tok -> Some (token env tok) (* "nonrec" *)
    | None -> None
  in
  let v4 = map_type_binding env v4 in
  let v5 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          token env v1
          (* "and" *)
        in
        let v2 = map_type_binding env v2 in
        v2)
      v5
  in
  Type (v1, v4 :: v5) |> mki

and map_type_equation (env : env) ((v1, v2, v3) : CST.type_equation) =
  let teq = map_anon_choice_EQ_4ccabd6 env v1 in
  let _v2 = private_opt env v2 in
  let ty = map_type_ env v3 in
  (teq, ty)

and map_type_parameter_constraint (env : env)
    ((v1, v2, v3, v4, v5) : CST.type_parameter_constraint) : Tok.t =
  let tconstraint =
    token env v1
    (* "constraint" *)
  in
  let _v2 = map_type_ env v2 in
  let _v3 =
    token env v3
    (* "=" *)
  in
  let _v4 = map_type_ env v4 in
  let _v5 = List.map (map_item_attribute env) v5 in
  tconstraint

and map_typed (env : env) ((v1, v2) : CST.typed) : Tok.t * type_ =
  let v1 =
    token env v1
    (* ":" *)
  in
  let v2 = map_type_ env v2 in
  (v1, v2)

and map_typed_opt env v : type_ option =
  match v with
  | Some x -> Some (snd (map_typed env x))
  | None -> None

and rec_opt env x =
  match x with
  | None -> None
  | Some tok -> Some (token env tok)

and map_value_definition (env : env) ((v1, v2, v3) : CST.value_definition) :
    Tok.t * rec_opt * let_binding list =
  let t, recopt =
    match v1 with
    | `Let_opt_attr_opt_rec (v1, v2, v3) ->
        let v1 =
          token env v1
          (* "let" *)
        in
        let _v2 = map_attribute_opt env v2 in
        let v3 = rec_opt env v3 in
        (v1, v3)
    | `Let_op tok ->
        let t = token env tok in
        (* let_operator *)
        (t, None)
  in
  let v2 = map_let_binding env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let _v1 =
          match v1 with
          | `And tok -> token env tok (* "and" *)
          | `Let_and_op tok -> token env tok
          (* and_operator *)
        in
        let v2 = map_let_binding env v2 in
        v2)
      v3
  in
  (t, recopt, v2 :: v3)

and map_variant_declaration (env : env) (x : CST.variant_declaration) =
  match x with
  | `BAR_opt_cons_decl_rep_BAR_cons_decl (v1, v2) ->
      let _v1 =
        token env v1
        (* "|" *)
      in
      let v2 =
        match v2 with
        | Some x -> map_anon_cons_decl_rep_BAR_cons_decl_fc0ccc5 env x
        | None -> []
      in
      v2
  | `Cons_decl_rep_BAR_cons_decl x ->
      let x = map_anon_cons_decl_rep_BAR_cons_decl_fc0ccc5 env x in
      x

let map_compilation_unit (env : env) (x : CST.compilation_unit) =
  match x with
  | `Opt_sheb_opt_stru (v1, v2) ->
      let _v1 =
        match v1 with
        | Some tok -> Some (token env tok) (* pattern #!.* *)
        | None -> None
      in
      let v2 =
        match v2 with
        | Some x -> map_structure env x
        | None -> []
      in
      v2
  | `Sign x ->
      let x = map_signature env x in
      x

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_ocaml.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      map_compilation_unit env cst)

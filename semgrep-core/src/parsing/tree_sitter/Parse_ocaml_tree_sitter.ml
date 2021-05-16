(* Yoann Padioleau
 *
 * Copyright (c) 2021 R2C
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
module PI = Parse_info
module CST = Tree_sitter_ocaml.CST
module H = Parse_tree_sitter_helpers
open Ast_ml

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let _token = H.token

let _fake = PI.fake_info

let str = H.str

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-ocaml/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the ocaml CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let todo (env : env) _ = failwith "not implemented"

let _map_capitalized_identifier (env : env) (tok : CST.capitalized_identifier) =
  token env tok

(* pattern "[A-Z][a-zA-Z0-9_']*" *)

let _map_conversion_specification (env : env)
    (tok : CST.conversion_specification) =
  token env tok

(* conversion_specification *)

let map_unit_ (env : env) (x : CST.unit_) : literal =
  match x with
  | `LPAR_RPAR (v1, v2) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = token env v2 (* ")" *) in
      Unit (v1, v2)
  | `Begin_end (v1, v2) ->
      let v1 = token env v1 (* "begin" *) in
      let v2 = token env v2 (* "end" *) in
      Unit (v1, v2)

let map_boolean (env : env) (x : CST.boolean) : literal =
  match x with
  | `True tok -> Bool (true, token env tok) (* "true" *)
  | `False tok -> Bool (false, token env tok)

(* "false" *)

let _map_rel_operator (env : env) (tok : CST.rel_operator) = token env tok

(* rel_operator *)

let map_or_operator (env : env) (x : CST.or_operator) =
  match x with
  | `Or tok -> token env tok (* "or" *)
  | `BARBAR tok -> token env tok

(* "||" *)

let map_and_operator_ (env : env) (x : CST.and_operator_) =
  match x with
  | `AMP tok -> token env tok (* "&" *)
  | `AMPAMP tok -> token env tok

(* "&&" *)

let map_anon_choice_EQ_4ccabd6 (env : env) (x : CST.anon_choice_EQ_4ccabd6) =
  match x with
  | `EQ tok -> token env tok (* "=" *)
  | `COLONEQ tok -> token env tok

(* ":=" *)

let _map_prefix_operator (env : env) (tok : CST.prefix_operator) = token env tok

(* prefix_operator *)

let _map_concat_operator (env : env) (tok : CST.concat_operator) = token env tok

(* concat_operator *)

let _map_match_operator (env : env) (tok : CST.match_operator) = token env tok

(* match_operator *)

let _map_pat_d43393f (env : env) (tok : CST.pat_d43393f) = token env tok

(* pattern "[^\\\\']" *)

let _map_hash_operator (env : env) (tok : CST.hash_operator) = token env tok

(* hash_operator *)

let map_anon_choice_priv_c7cc539 (env : env) (x : CST.anon_choice_priv_c7cc539)
    =
  match x with
  | `Priv tok -> token env tok (* "private" *)
  | `Virt tok -> token env tok

(* "virtual" *)

let _map_pat_60fc52b (env : env) (tok : CST.pat_60fc52b) = token env tok

(* pattern "\\\\[\\\\\"'ntbr ]" *)

let map_anon_choice_TILDE_72781e5 (env : env)
    (x : CST.anon_choice_TILDE_72781e5) =
  match x with
  | `TILDE tok -> token env tok (* "~" *)
  | `QMARK tok -> token env tok

(* "?" *)

let _map_pat_6cdf4be (env : env) (tok : CST.pat_6cdf4be) = token env tok

(* pattern \\u\{[0-9A-Fa-f]+\} *)

let _map_pat_21333c0 (env : env) (tok : CST.pat_21333c0) = token env tok

(* pattern \\o[0-3][0-7][0-7] *)

let _map_let_operator (env : env) (tok : CST.let_operator) = token env tok

(* let_operator *)

let _map_pat_9465c8b (env : env) (tok : CST.pat_9465c8b) = token env tok

(* pattern \\\n[\t ]* *)

let _map_add_operator (env : env) (tok : CST.add_operator) = token env tok

(* add_operator *)

let _map_left_quoted_string_delimiter (env : env)
    (tok : CST.left_quoted_string_delimiter) =
  token env tok

(* left_quoted_string_delimiter *)

let _map_pat_86b875b (env : env) (tok : CST.pat_86b875b) = token env tok

(* pattern \\[0-9][0-9][0-9] *)

let _map_indexing_operator (env : env) (tok : CST.indexing_operator) =
  token env tok

(* indexing_operator *)

let _map_pat_3d340f6 (env : env) (tok : CST.pat_3d340f6) = token env tok

(* pattern \s+ *)

let _map_pat_19aaf34 (env : env) (tok : CST.pat_19aaf34) = token env tok

(* pattern "[^\\\\\"%@]+|%|@" *)

let _map_right_quoted_string_delimiter (env : env)
    (tok : CST.right_quoted_string_delimiter) =
  token env tok

(* right_quoted_string_delimiter *)

let _map_and_operator (env : env) (tok : CST.and_operator) = token env tok

(* and_operator *)

let _map_pat_3bf11d1 (env : env) (tok : CST.pat_3bf11d1) = token env tok

(* pattern \\x[0-9A-Fa-f][0-9A-Fa-f] *)

let _map_null (env : env) (tok : CST.null) = token env tok

(* null *)

let _map_pat_714c625 (env : env) (tok : CST.pat_714c625) = token env tok

(* pattern [^%@|]+|%|@|\| *)

let map_sign_operator (env : env) (x : CST.sign_operator) =
  match x with
  | `PLUS tok -> token env tok (* "+" *)
  | `DASH tok -> token env tok (* "-" *)
  | `PLUSDOT tok -> token env tok (* "+." *)
  | `DASHDOT tok -> token env tok

(* "-." *)

let _map_tok_choice_pat_4349e4b (env : env) (tok : CST.tok_choice_pat_4349e4b) =
  token env tok

(* tok_choice_pat_4349e4b *)

let _map_shebang (env : env) (tok : CST.shebang) = token env tok

(* pattern #!.* *)

let map_anon_choice_muta_d43fe41 (env : env) (x : CST.anon_choice_muta_d43fe41)
    =
  match x with
  | `Muta tok -> token env tok (* "mutable" *)
  | `Virt tok -> token env tok

(* "virtual" *)

let map_assign_operator (env : env) (x : CST.assign_operator) =
  match x with `COLONEQ tok -> token env tok

(* ":=" *)

let _map_identifier (env : env) (tok : CST.identifier) = token env tok

(* pattern "[a-z_][a-zA-Z0-9_']*" *)

let _map_ocamlyacc_value (env : env) (tok : CST.ocamlyacc_value) = token env tok

(* pattern \$[0-9]+ *)

let _map_pretty_printing_indication (env : env)
    (tok : CST.pretty_printing_indication) =
  token env tok

(* pattern @([\[\], ;.{}?]|\\n|<[0-9]+>) *)

let map_anon_choice_PLUS_da42005 (env : env) (x : CST.anon_choice_PLUS_da42005)
    =
  match x with
  | `PLUS tok -> token env tok (* "+" *)
  | `DASH tok -> token env tok

(* "-" *)

let map_mult_operator (env : env) (x : CST.mult_operator) =
  match x with
  | `Tok_pat_058c54c_rep_pat_525fbb4 tok ->
      token env tok (* tok_pat_058c54c_rep_pat_525fbb4 *)
  | `Mod tok -> token env tok (* "mod" *)
  | `Land tok -> token env tok (* "land" *)
  | `Lor tok -> token env tok (* "lor" *)
  | `Lxor tok -> token env tok

(* "lxor" *)

let map_escape_sequence (env : env) (x : CST.escape_sequence) =
  match x with
  | `Pat_60fc52b tok -> token env tok (* pattern "\\\\[\\\\\"'ntbr ]" *)
  | `Pat_86b875b tok -> token env tok (* pattern \\[0-9][0-9][0-9] *)
  | `Pat_3bf11d1 tok -> token env tok (* pattern \\x[0-9A-Fa-f][0-9A-Fa-f] *)
  | `Pat_21333c0 tok -> token env tok

(* pattern \\o[0-3][0-7][0-7] *)

let map_pow_operator (env : env) (x : CST.pow_operator) =
  match x with
  | `Tok_STARSTAR_rep_pat_525fbb4 tok ->
      token env tok (* tok_STARSTAR_rep_pat_525fbb4 *)
  | `Lsl tok -> token env tok (* "lsl" *)
  | `Lsr tok -> token env tok (* "lsr" *)
  | `Asr tok -> token env tok

(* "asr" *)

let map_quoted_string_content (env : env) (xs : CST.quoted_string_content) =
  List.map
    (fun x ->
      match x with
      | `Imm_tok_SPACE tok -> token env tok (* " " *)
      | `Imm_tok_LF tok -> token env tok (* "\n" *)
      | `Imm_tok_HT tok -> token env tok (* "\t" *)
      | `Imm_tok_LBRACKAT tok -> token env tok (* "[@" *)
      | `Imm_tok_LBRACKATAT tok -> token env tok (* "[@@" *)
      | `Imm_tok_LBRACKATATAT tok -> token env tok (* "[@@@" *)
      | `Pat_714c625 tok -> token env tok (* pattern [^%@|]+|%|@|\| *)
      | `Null tok -> token env tok (* null *)
      | `Conv_spec tok -> token env tok (* conversion_specification *)
      | `Pretty_prin_indi tok -> token env tok
      (* pattern @([\[\], ;.{}?]|\\n|<[0-9]+>) *))
    xs

let map_constructor_name (env : env) (x : CST.constructor_name) =
  match x with
  | `Capi_id tok -> token env tok (* pattern "[A-Z][a-zA-Z0-9_']*" *)
  | `LPAR_COLONCOLON_RPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = token env v2 (* "::" *) in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)

let rec map_module_path (env : env) (x : CST.module_path) =
  match x with
  | `Capi_id tok -> token env tok (* pattern "[A-Z][a-zA-Z0-9_']*" *)
  | `Module_path_DOT_capi_id (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* pattern "[A-Z][a-zA-Z0-9_']*" *) in
      todo env (v1, v2, v3)

let map_anon_choice_module_name_7ad5569 (env : env)
    (x : CST.anon_choice_module_name_7ad5569) =
  match x with
  | `Capi_id tok -> token env tok (* pattern "[A-Z][a-zA-Z0-9_']*" *)
  | `X__ tok -> token env tok

(* "_" *)

let rec map_extended_module_path (env : env) (x : CST.extended_module_path) =
  match x with
  | `Choice_capi_id x -> (
      match x with
      | `Capi_id tok -> token env tok (* pattern "[A-Z][a-zA-Z0-9_']*" *)
      | `Exte_module_path_DOT_capi_id (v1, v2, v3) ->
          let v1 = map_extended_module_path env v1 in
          let v2 = token env v2 (* "." *) in
          let v3 = token env v3 (* pattern "[A-Z][a-zA-Z0-9_']*" *) in
          todo env (v1, v2, v3) )
  | `Exte_module_path_LPAR_exte_module_path_RPAR (v1, v2, v3, v4) ->
      let v1 = map_extended_module_path env v1 in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_extended_module_path env v3 in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)

let map_string_content (env : env) (xs : CST.string_content) =
  List.map
    (fun x ->
      match x with
      | `Imm_tok_SPACE tok -> token env tok (* " " *)
      | `Imm_tok_LF tok -> token env tok (* "\n" *)
      | `Imm_tok_HT tok -> token env tok (* "\t" *)
      | `Imm_tok_LBRACKAT tok -> token env tok (* "[@" *)
      | `Imm_tok_LBRACKATAT tok -> token env tok (* "[@@" *)
      | `Imm_tok_LBRACKATATAT tok -> token env tok (* "[@@@" *)
      | `Pat_19aaf34 tok -> token env tok (* pattern "[^\\\\\"%@]+|%|@" *)
      | `Null tok -> token env tok (* null *)
      | `Esc_seq x -> map_escape_sequence env x
      | `Pat_6cdf4be tok -> token env tok (* pattern \\u\{[0-9A-Fa-f]+\} *)
      | `Pat_9465c8b tok -> token env tok (* pattern \\\n[\t ]* *)
      | `Conv_spec tok -> token env tok (* conversion_specification *)
      | `Pretty_prin_indi tok -> token env tok
      (* pattern @([\[\], ;.{}?]|\\n|<[0-9]+>) *))
    xs

let map_character_content (env : env) (x : CST.character_content) =
  match x with
  | `Pat_d43393f tok -> token env tok (* pattern "[^\\\\']" *)
  | `Null tok -> token env tok (* null *)
  | `Esc_seq x -> map_escape_sequence env x

let map_infix_operator (env : env) (x : CST.infix_operator) =
  match x with
  | `Hash_op tok -> token env tok (* hash_operator *)
  | `Pow_op x -> map_pow_operator env x
  | `Mult_op x -> map_mult_operator env x
  | `Add_op tok -> token env tok (* add_operator *)
  | `Concat_op tok -> token env tok (* concat_operator *)
  | `Rel_op tok -> token env tok (* rel_operator *)
  | `And_op_ x -> map_and_operator_ env x
  | `Or_op x -> map_or_operator env x
  | `Assign_op x -> map_assign_operator env x

let map_module_type_name (env : env) (x : CST.module_type_name) =
  match x with
  | `Capi_id tok -> token env tok (* pattern "[A-Z][a-zA-Z0-9_']*" *)
  | `Id tok -> token env tok

(* pattern "[a-z_][a-zA-Z0-9_']*" *)

let map_abstract_type (env : env) ((v1, v2) : CST.abstract_type) =
  let v1 = token env v1 (* "type" *) in
  let v2 = List.map (token env) (* pattern "[a-z_][a-zA-Z0-9_']*" *) v2 in
  todo env (v1, v2)

let map_anon_choice_inst_var_name_cbd841f (env : env)
    (x : CST.anon_choice_inst_var_name_cbd841f) =
  match x with
  | `Id tok -> token env tok (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  | `Capi_id tok -> token env tok

(* pattern "[A-Z][a-zA-Z0-9_']*" *)

let map_label (env : env) ((v1, v2) : CST.label) =
  let v1 = map_anon_choice_TILDE_72781e5 env v1 in
  let v2 = token env v2 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
  todo env (v1, v2)

let map_constructor_path (env : env) (x : CST.constructor_path) =
  match x with
  | `Choice_capi_id x -> map_constructor_name env x
  | `Module_path_DOT_choice_capi_id (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = map_constructor_name env v3 in
      todo env (v1, v2, v3)

let map_indexing_operator_path (env : env) (x : CST.indexing_operator_path) =
  match x with
  | `Inde_op tok -> token env tok (* indexing_operator *)
  | `Module_path_DOT_inde_op (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* indexing_operator *) in
      todo env (v1, v2, v3)

let map_class_path (env : env) (x : CST.class_path) =
  match x with
  | `Id tok -> token env tok (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  | `Module_path_DOT_id (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
      todo env (v1, v2, v3)

let map_field_path (env : env) (x : CST.field_path) =
  match x with
  | `Id tok -> token env tok (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  | `Module_path_DOT_id (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
      todo env (v1, v2, v3)

let map_type_constructor_path (env : env) (x : CST.type_constructor_path) =
  match x with
  | `Id tok -> token env tok (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  | `Exte_module_path_DOT_id (v1, v2, v3) ->
      let v1 = map_extended_module_path env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
      todo env (v1, v2, v3)

let map_class_type_path (env : env) (x : CST.class_type_path) =
  match x with
  | `Id tok -> token env tok (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  | `Exte_module_path_DOT_id (v1, v2, v3) ->
      let v1 = map_extended_module_path env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
      todo env (v1, v2, v3)

let map_string_ (env : env) ((v1, v2, v3) : CST.string_) =
  let v1 = token env v1 (* "\"" *) in
  let v2 =
    match v2 with Some x -> map_string_content env x | None -> todo env ()
  in
  let v3 = token env v3 (* "\"" *) in
  todo env (v1, v2, v3)

let map_parenthesized_operator (env : env)
    ((v1, v2, v3) : CST.parenthesized_operator) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | `Prefix_op tok -> token env tok (* prefix_operator *)
    | `Sign_op x -> map_sign_operator env x
    | `Infix_op x -> map_infix_operator env x
    | `DOT_inde_op_choice_LPAR_opt_SEMI_DOTDOT_RPAR_opt_LTDASH (v1, v2, v3, v4)
      ->
        let v1 = token env v1 (* "." *) in
        let v2 = token env v2 (* indexing_operator *) in
        let v3 =
          match v3 with
          | `LPAR_opt_SEMI_DOTDOT_RPAR (v1, v2, v3) ->
              let v1 = token env v1 (* "(" *) in
              let v2 =
                match v2 with
                | Some (v1, v2) ->
                    let v1 = token env v1 (* ";" *) in
                    let v2 = token env v2 (* ".." *) in
                    todo env (v1, v2)
                | None -> todo env ()
              in
              let v3 = token env v3 (* ")" *) in
              todo env (v1, v2, v3)
          | `LBRACK_opt_SEMI_DOTDOT_RBRACK (v1, v2, v3) ->
              let v1 = token env v1 (* "[" *) in
              let v2 =
                match v2 with
                | Some (v1, v2) ->
                    let v1 = token env v1 (* ";" *) in
                    let v2 = token env v2 (* ".." *) in
                    todo env (v1, v2)
                | None -> todo env ()
              in
              let v3 = token env v3 (* "]" *) in
              todo env (v1, v2, v3)
          | `LCURL_opt_SEMI_DOTDOT_RCURL (v1, v2, v3) ->
              let v1 = token env v1 (* "{" *) in
              let v2 =
                match v2 with
                | Some (v1, v2) ->
                    let v1 = token env v1 (* ";" *) in
                    let v2 = token env v2 (* ".." *) in
                    todo env (v1, v2)
                | None -> todo env ()
              in
              let v3 = token env v3 (* "}" *) in
              todo env (v1, v2, v3)
        in
        let v4 =
          match v4 with
          | Some tok -> token env tok (* "<-" *)
          | None -> todo env ()
        in
        todo env (v1, v2, v3, v4)
    | `Let_op tok -> token env tok (* let_operator *)
    | `And_op tok -> token env tok (* and_operator *)
    | `Match_op tok -> token env tok
    (* match_operator *)
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

let map_module_type_path (env : env) (x : CST.module_type_path) =
  match x with
  | `Choice_capi_id x -> map_module_type_name env x
  | `Exte_module_path_DOT_choice_capi_id (v1, v2, v3) ->
      let v1 = map_extended_module_path env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = map_module_type_name env v3 in
      todo env (v1, v2, v3)

let map_parenthesized_abstract_type (env : env)
    ((v1, v2, v3) : CST.parenthesized_abstract_type) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_abstract_type env v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

let map_attribute_id (env : env) ((v1, v2) : CST.attribute_id) =
  let v1 = map_anon_choice_inst_var_name_cbd841f env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "." *) in
        let v2 = map_anon_choice_inst_var_name_cbd841f env v2 in
        todo env (v1, v2))
      v2
  in
  todo env (v1, v2)

let map_directive (env : env) ((v1, v2) : CST.directive) =
  let v1 = token env v1 (* "#" *) in
  let v2 = map_anon_choice_inst_var_name_cbd841f env v2 in
  todo env (v1, v2)

let map_tag (env : env) ((v1, v2) : CST.tag) =
  let v1 = token env v1 (* "`" *) in
  let v2 = map_anon_choice_inst_var_name_cbd841f env v2 in
  todo env (v1, v2)

let map_type_variable (env : env) ((v1, v2) : CST.type_variable) =
  let v1 = token env v1 (* "'" *) in
  let v2 = map_anon_choice_inst_var_name_cbd841f env v2 in
  todo env (v1, v2)

let map_polymorphic_variant_pattern (env : env)
    ((v1, v2) : CST.polymorphic_variant_pattern) =
  let v1 = token env v1 (* "#" *) in
  let v2 = map_type_constructor_path env v2 in
  todo env (v1, v2)

let map_constant (env : env) (x : CST.constant) : expr =
  match x with
  | `Num tok ->
      let s, t = str env tok (* number *) in
      L
        ( try
            let i = int_of_string s in
            Int (Some i, t)
          with _ ->
            let fopt = float_of_string_opt s in
            Float (fopt, t) )
  | `Char (v1, v2, v3) ->
      let v1 = token env v1 (* "'" *) in
      let v2 = map_character_content env v2 in
      let v3 = token env v3 (* "'" *) in
      todo env (v1, v2, v3)
  | `Str x -> map_string_ env x
  | `Quoted_str (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "{" *) in
      let v2 = token env v2 (* left_quoted_string_delimiter *) in
      let v3 =
        match v3 with
        | Some x -> map_quoted_string_content env x
        | None -> todo env ()
      in
      let v4 = token env v4 (* right_quoted_string_delimiter *) in
      let v5 = token env v5 (* "}" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Bool x -> L (map_boolean env x)
  | `Unit x -> L (map_unit_ env x)

let map_value_pattern (env : env) (x : CST.value_pattern) =
  match x with
  | `Id tok -> token env tok (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  | `Paren_op x -> map_parenthesized_operator env x

let map_value_name (env : env) (x : CST.value_name) =
  match x with
  | `Id tok -> token env tok (* pattern "[a-z_][a-zA-Z0-9_']*" *)
  | `Paren_op x -> map_parenthesized_operator env x

let map_attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = token env v1 (* "%" *) in
  let v2 = map_attribute_id env v2 in
  todo env (v1, v2)

let map_type_param (env : env) ((v1, v2) : CST.type_param) =
  let v1 =
    match v1 with
    | Some x -> (
        match x with
        | `PLUS_opt_BANG (v1, v2) ->
            let v1 = token env v1 (* "+" *) in
            let v2 =
              match v2 with
              | Some tok -> token env tok (* "!" *)
              | None -> todo env ()
            in
            todo env (v1, v2)
        | `DASH_opt_BANG (v1, v2) ->
            let v1 = token env v1 (* "-" *) in
            let v2 =
              match v2 with
              | Some tok -> token env tok (* "!" *)
              | None -> todo env ()
            in
            todo env (v1, v2)
        | `BANG_opt_choice_PLUS (v1, v2) ->
            let v1 = token env v1 (* "!" *) in
            let v2 =
              match v2 with
              | Some x -> map_anon_choice_PLUS_da42005 env x
              | None -> todo env ()
            in
            todo env (v1, v2) )
    | None -> todo env ()
  in
  let v2 =
    match v2 with
    | `Type_var x -> map_type_variable env x
    | `X__ tok -> token env tok
    (* "_" *)
  in
  todo env (v1, v2)

let map_signed_constant (env : env) (x : CST.signed_constant) =
  match x with
  | `Cst x -> map_constant env x
  | `Signed_num (v1, v2) ->
      let v1 = map_anon_choice_PLUS_da42005 env v1 in
      let v2 = token env v2 (* tok_choice_pat_4349e4b *) in
      todo env (v1, v2)

let map_value_path (env : env) (x : CST.value_path) =
  match x with
  | `Value_name x -> map_value_name env x
  | `Module_path_DOT_value_name (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = map_value_name env v3 in
      todo env (v1, v2, v3)

let map_type_params (env : env) (x : CST.type_params) =
  match x with
  | `Type_param x -> map_type_param env x
  | `LPAR_type_param_rep_COMMA_type_param_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_type_param env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_type_param env v2 in
            todo env (v1, v2))
          v3
      in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)

let map_anon_LBRACK_type_param_rep_COMMA_type_param_RBRACK_cea5434 (env : env)
    ((v1, v2, v3, v4) :
      CST.anon_LBRACK_type_param_rep_COMMA_type_param_RBRACK_cea5434) =
  let v1 = token env v1 (* "[" *) in
  let v2 = map_type_param env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 = map_type_param env v2 in
        todo env (v1, v2))
      v3
  in
  let v4 = token env v4 (* "]" *) in
  todo env (v1, v2, v3, v4)

let map_range_pattern (env : env) ((v1, v2, v3) : CST.range_pattern) =
  let v1 = map_signed_constant env v1 in
  let v2 = token env v2 (* ".." *) in
  let v3 = map_signed_constant env v3 in
  todo env (v1, v2, v3)

let map_toplevel_directive (env : env) ((v1, v2) : CST.toplevel_directive) =
  let v1 = map_directive env v1 in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `Cst x -> map_constant env x
        | `Value_path x -> map_value_path env x
        | `Module_path x -> map_module_path env x )
    | None -> todo env ()
  in
  todo env (v1, v2)

let rec map_anon_bind_pat_ext_rep_SEMI_bind_pat_ext_opt_SEMI_38caf30 (env : env)
    ((v1, v2, v3) :
      CST.anon_bind_pat_ext_rep_SEMI_bind_pat_ext_opt_SEMI_38caf30) =
  let v1 = map_binding_pattern_ext env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* ";" *) in
        let v2 = map_binding_pattern_ext env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with Some tok -> token env tok (* ";" *) | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_anon_choice_cons_type_771aabb (env : env)
    (x : CST.anon_choice_cons_type_771aabb) =
  match x with
  | `Cons_type (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "type" *) in
      let v2 =
        match v2 with Some x -> map_type_params env x | None -> todo env ()
      in
      let v3 = map_type_constructor_path env v3 in
      let v4 = map_type_equation env v4 in
      let v5 = List.map (map_type_constraint env) v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Cons_module (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "module" *) in
      let v2 = map_module_path env v2 in
      let v3 = map_anon_choice_EQ_4ccabd6 env v3 in
      let v4 = map_extended_module_path env v4 in
      todo env (v1, v2, v3, v4)
  | `Cons_module_type (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "module" *) in
      let v2 = token env v2 (* "type" *) in
      let v3 = map_module_type_path env v3 in
      let v4 = map_anon_choice_EQ_4ccabd6 env v4 in
      let v5 = map_module_type_ext env v5 in
      todo env (v1, v2, v3, v4, v5)

and map_anon_choice_meth_type_345b567 (env : env)
    (x : CST.anon_choice_meth_type_345b567) =
  match x with
  | `Meth_type (v1, v2) ->
      let v1 = token env v1 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
      let v2 = map_polymorphic_typed env v2 in
      todo env (v1, v2)
  | `Choice_simple_type x -> map_simple_type_ext env x

and map_anon_choice_simple_type_ext_30dd028 (env : env)
    (x : CST.anon_choice_simple_type_ext_30dd028) =
  match x with
  | `Choice_simple_type x -> map_simple_type_ext env x
  | `LPAR_type_ext_rep_COMMA_type_ext_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_type_ext env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_type_ext env v2 in
            todo env (v1, v2))
          v3
      in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)

and map_anon_cons_decl_rep_BAR_cons_decl_fc0ccc5 (env : env)
    ((v1, v2) : CST.anon_cons_decl_rep_BAR_cons_decl_fc0ccc5) =
  let v1 = map_constructor_declaration env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "|" *) in
        let v2 = map_constructor_declaration env v2 in
        todo env (v1, v2))
      v2
  in
  todo env (v1, v2)

and map_anon_exp_ext_rep_SEMI_exp_ext_opt_SEMI_f0de170 (env : env)
    ((v1, v2, v3) : CST.anon_exp_ext_rep_SEMI_exp_ext_opt_SEMI_f0de170) =
  let v1 = map_expression_ext env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* ";" *) in
        let v2 = map_expression_ext env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with Some tok -> token env tok (* ";" *) | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_anon_pat_ext_rep_SEMI_pat_ext_opt_SEMI_3830e8c (env : env)
    ((v1, v2, v3) : CST.anon_pat_ext_rep_SEMI_pat_ext_opt_SEMI_3830e8c) =
  let v1 = map_pattern_ext env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* ";" *) in
        let v2 = map_pattern_ext env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with Some tok -> token env tok (* ";" *) | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_argument (env : env) (x : CST.argument) : argument =
  match x with
  | `Choice_simple_exp x -> map_simple_expression_ext env x
  | `Labe_arg x -> map_labeled_argument env x

and map_array_binding_pattern (env : env)
    ((v1, v2, v3) : CST.array_binding_pattern) =
  let v1 = token env v1 (* "[|" *) in
  let v2 =
    match v2 with
    | Some x ->
        map_anon_bind_pat_ext_rep_SEMI_bind_pat_ext_opt_SEMI_38caf30 env x
    | None -> todo env ()
  in
  let v3 = token env v3 (* "|]" *) in
  todo env (v1, v2, v3)

and map_array_expression (env : env) ((v1, v2, v3) : CST.array_expression) =
  let v1 = token env v1 (* "[|" *) in
  let v2 =
    match v2 with
    | Some x -> map_anon_exp_ext_rep_SEMI_exp_ext_opt_SEMI_f0de170 env x
    | None -> todo env ()
  in
  let v3 = token env v3 (* "|]" *) in
  todo env (v1, v2, v3)

and map_array_get_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.array_get_expression) =
  let v1 = map_simple_expression_ext env v1 in
  let v2 = token env v2 (* "." *) in
  let v3 =
    match v3 with
    | Some x -> map_indexing_operator_path env x
    | None -> todo env ()
  in
  let v4 = token env v4 (* "(" *) in
  let v5 = map_sequence_expression_ext env v5 in
  let v6 = token env v6 (* ")" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_array_pattern (env : env) ((v1, v2, v3) : CST.array_pattern) =
  let v1 = token env v1 (* "[|" *) in
  let v2 =
    match v2 with
    | Some x -> map_anon_pat_ext_rep_SEMI_pat_ext_opt_SEMI_3830e8c env x
    | None -> todo env ()
  in
  let v3 = token env v3 (* "|]" *) in
  todo env (v1, v2, v3)

and map_attribute_payload (env : env) (x : CST.attribute_payload) =
  match x with
  | `Stru x -> map_structure env x
  | `COLON_choice_type_ext (v1, v2) ->
      let v1 = token env v1 (* ":" *) in
      let v2 =
        match v2 with
        | `Type_ext x -> map_type_ext env x
        | `Sign x -> map_signature env x
      in
      todo env (v1, v2)
  | `QMARK_pat_ext_opt_guard (v1, v2, v3) ->
      let v1 = token env v1 (* "?" *) in
      let v2 = map_pattern_ext env v2 in
      let v3 =
        match v3 with Some x -> map_guard env x | None -> todo env ()
      in
      todo env (v1, v2, v3)

and map_bigarray_get_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.bigarray_get_expression) =
  let v1 = map_simple_expression_ext env v1 in
  let v2 = token env v2 (* "." *) in
  let v3 =
    match v3 with
    | Some x -> map_indexing_operator_path env x
    | None -> todo env ()
  in
  let v4 = token env v4 (* "{" *) in
  let v5 = map_sequence_expression_ext env v5 in
  let v6 = token env v6 (* "}" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_binding_pattern (env : env) (x : CST.binding_pattern) =
  match x with
  | `Value_name x -> map_value_name env x
  | `Signed_cst x -> map_signed_constant env x
  | `Typed_bind_pat (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_binding_pattern_ext env v2 in
      let v3 = map_typed env v3 in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Cons_path x -> map_constructor_path env x
  | `Tag x -> map_tag env x
  | `Poly_vari_pat x -> map_polymorphic_variant_pattern env x
  | `Record_bind_pat x -> map_record_binding_pattern env x
  | `List_bind_pat x -> map_list_binding_pattern env x
  | `Array_bind_pat x -> map_array_binding_pattern env x
  | `Local_open_bind_pat (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 =
        match v3 with
        | `LPAR_opt_bind_pat_ext_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 =
              match v2 with
              | Some x -> map_binding_pattern_ext env x
              | None -> todo env ()
            in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        | `List_bind_pat x -> map_list_binding_pattern env x
        | `Array_bind_pat x -> map_array_binding_pattern env x
        | `Record_bind_pat x -> map_record_binding_pattern env x
      in
      todo env (v1, v2, v3)
  | `Pack_pat x -> map_package_pattern env x
  | `Paren_bind_pat (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_binding_pattern_ext env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  | `Alias_bind_pat (v1, v2, v3) ->
      let v1 = map_binding_pattern_ext env v1 in
      let v2 = token env v2 (* "as" *) in
      let v3 = map_value_name env v3 in
      todo env (v1, v2, v3)
  | `Or_bind_pat (v1, v2, v3) ->
      let v1 = map_binding_pattern_ext env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_binding_pattern_ext env v3 in
      todo env (v1, v2, v3)
  | `Cons_bind_pat_1ca6430 (v1, v2) ->
      let v1 = map_constructor_path env v1 in
      let v2 = map_binding_pattern_ext env v2 in
      todo env (v1, v2)
  | `Tag_bind_pat (v1, v2) ->
      let v1 = map_tag env v1 in
      let v2 = map_binding_pattern_ext env v2 in
      todo env (v1, v2)
  | `Tuple_bind_pat (v1, v2, v3) ->
      let v1 = map_binding_pattern_ext env v1 in
      let v2 = token env v2 (* "," *) in
      let v3 = map_binding_pattern_ext env v3 in
      todo env (v1, v2, v3)
  | `Cons_bind_pat_f2d0ae9 (v1, v2, v3) ->
      let v1 = map_binding_pattern_ext env v1 in
      let v2 = token env v2 (* "::" *) in
      let v3 = map_binding_pattern_ext env v3 in
      todo env (v1, v2, v3)
  | `Range_pat x -> map_range_pattern env x
  | `Lazy_bind_pat (v1, v2, v3) ->
      let v1 = token env v1 (* "lazy" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_binding_pattern_ext env v3 in
      todo env (v1, v2, v3)

and map_binding_pattern_ext (env : env) (x : CST.binding_pattern_ext) =
  match x with
  | `Bind_pat x -> map_binding_pattern env x
  | `Exte x ->
      let _ = map_extension env x in
      todo env ()

and map_class_binding (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.class_binding) =
  let v1 =
    match v1 with
    | Some tok -> token env tok (* "virtual" *)
    | None -> todo env ()
  in
  let v2 =
    match v2 with
    | Some x ->
        map_anon_LBRACK_type_param_rep_COMMA_type_param_RBRACK_cea5434 env x
    | None -> todo env ()
  in
  let v3 = token env v3 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
  let v4 = List.map (map_parameter env) v4 in
  let v5 =
    match v5 with Some x -> map_class_typed env x | None -> todo env ()
  in
  let v6 =
    match v6 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_class_expression_ext env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v7 = List.map (map_item_attribute env) v7 in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_class_definition (env : env) ((v1, v2, v3, v4) : CST.class_definition) =
  let v1 = token env v1 (* "class" *) in
  let v2 =
    match v2 with Some x -> map_attribute env x | None -> todo env ()
  in
  let v3 = map_class_binding env v3 in
  let v4 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "and" *) in
        let v2 = map_class_binding env v2 in
        todo env (v1, v2))
      v4
  in
  todo env (v1, v2, v3, v4)

and map_class_expression (env : env) (x : CST.class_expression) =
  match x with
  | `Simple_class_exp x -> map_simple_class_expression env x
  | `Class_func (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "fun" *) in
      let v2 = List.map (map_parameter env) v2 in
      let v3 = token env v3 (* "->" *) in
      let v4 = map_class_expression_ext env v4 in
      todo env (v1, v2, v3, v4)
  | `Class_app (v1, v2) ->
      let v1 = map_simple_class_expression env v1 in
      let v2 = List.map (map_argument env) v2 in
      todo env (v1, v2)
  | `Let_class_exp (v1, v2, v3) ->
      let v1 = map_value_definition env v1 in
      let v2 = token env v2 (* "in" *) in
      let v3 = map_class_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Let_open_class_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "let" *) in
      let v2 = map_open_module env v2 in
      let v3 = token env v3 (* "in" *) in
      let v4 = map_class_expression_ext env v4 in
      todo env (v1, v2, v3, v4)

and map_class_expression_ext (env : env) (x : CST.class_expression_ext) =
  match x with
  | `Class_exp x -> map_class_expression env x
  | `Exte x -> map_extension env x

and map_class_field (env : env) (x : CST.class_field) =
  match x with
  | `Inhe_defi (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "inherit" *) in
      let v2 =
        match v2 with
        | Some tok -> token env tok (* "!" *)
        | None -> todo env ()
      in
      let v3 = map_class_expression_ext env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "as" *) in
            let v2 = map_value_pattern env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v5 = List.map (map_item_attribute env) v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Inst_var_defi (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "val" *) in
      let v2 =
        match v2 with
        | Some tok -> token env tok (* "!" *)
        | None -> todo env ()
      in
      let v3 = List.map (map_anon_choice_muta_d43fe41 env) v3 in
      let v4 = token env v4 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
      let v5 =
        match v5 with Some x -> map_typed env x | None -> todo env ()
      in
      let v6 =
        match v6 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":>" *) in
            let v2 = map_type_ext env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v7 =
        match v7 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_sequence_expression_ext env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v8 = List.map (map_item_attribute env) v8 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Meth_defi (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "method" *) in
      let v2 =
        match v2 with
        | Some tok -> token env tok (* "!" *)
        | None -> todo env ()
      in
      let v3 = List.map (map_anon_choice_priv_c7cc539 env) v3 in
      let v4 = token env v4 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
      let v5 = List.map (map_parameter env) v5 in
      let v6 =
        match v6 with
        | Some x -> map_polymorphic_typed env x
        | None -> todo env ()
      in
      let v7 =
        match v7 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_sequence_expression_ext env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v8 = List.map (map_item_attribute env) v8 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Type_param_cons x -> map_type_parameter_constraint env x
  | `Class_init (v1, v2, v3) ->
      let v1 = token env v1 (* "initializer" *) in
      let v2 = map_sequence_expression_ext env v2 in
      let v3 = List.map (map_item_attribute env) v3 in
      todo env (v1, v2, v3)

and map_class_field_ext (env : env) (x : CST.class_field_ext) =
  match x with
  | `Class_field x -> map_class_field env x
  | `Item_exte x -> map_item_extension env x

and map_class_field_specification (env : env)
    (x : CST.class_field_specification) =
  match x with
  | `Inhe_spec (v1, v2, v3) ->
      let v1 = token env v1 (* "inherit" *) in
      let v2 = map_simple_class_type_ext env v2 in
      let v3 = List.map (map_item_attribute env) v3 in
      todo env (v1, v2, v3)
  | `Inst_var_spec (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "val" *) in
      let v2 = List.map (map_anon_choice_muta_d43fe41 env) v2 in
      let v3 = token env v3 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
      let v4 = map_typed env v4 in
      let v5 = List.map (map_item_attribute env) v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Meth_spec (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "method" *) in
      let v2 = List.map (map_anon_choice_priv_c7cc539 env) v2 in
      let v3 = token env v3 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
      let v4 = map_polymorphic_typed env v4 in
      let v5 = List.map (map_item_attribute env) v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Type_param_cons x -> map_type_parameter_constraint env x

and map_class_field_specification_ext (env : env)
    (x : CST.class_field_specification_ext) =
  match x with
  | `Class_field_spec x -> map_class_field_specification env x
  | `Item_exte x -> map_item_extension env x

and map_class_type (env : env) (x : CST.class_type) =
  match x with
  | `Simple_class_type x -> map_simple_class_type env x
  | `Class_func_type (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some (v1, v2, v3) ->
            let v1 =
              match v1 with
              | Some tok -> token env tok (* "?" *)
              | None -> todo env ()
            in
            let v2 = token env v2 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
            let v3 = token env v3 (* ":" *) in
            todo env (v1, v2, v3)
        | None -> todo env ()
      in
      let v2 = map_tuple_type_ext env v2 in
      let v3 = token env v3 (* "->" *) in
      let v4 = map_class_type_ext env v4 in
      todo env (v1, v2, v3, v4)

and map_class_type_binding (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.class_type_binding) =
  let v1 =
    match v1 with
    | Some tok -> token env tok (* "virtual" *)
    | None -> todo env ()
  in
  let v2 =
    match v2 with
    | Some x ->
        map_anon_LBRACK_type_param_rep_COMMA_type_param_RBRACK_cea5434 env x
    | None -> todo env ()
  in
  let v3 = token env v3 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
  let v4 = token env v4 (* "=" *) in
  let v5 = map_simple_class_type_ext env v5 in
  let v6 = List.map (map_item_attribute env) v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_class_type_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.class_type_definition) =
  let v1 = token env v1 (* "class" *) in
  let v2 = token env v2 (* "type" *) in
  let v3 =
    match v3 with Some x -> map_attribute env x | None -> todo env ()
  in
  let v4 = map_class_type_binding env v4 in
  let v5 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "and" *) in
        let v2 = map_class_type_binding env v2 in
        todo env (v1, v2))
      v5
  in
  todo env (v1, v2, v3, v4, v5)

and map_class_type_ext (env : env) (x : CST.class_type_ext) =
  match x with
  | `Class_type x -> map_class_type env x
  | `Exte x -> map_extension env x

and map_class_typed (env : env) ((v1, v2) : CST.class_typed) =
  let v1 = token env v1 (* ":" *) in
  let v2 = map_class_type_ext env v2 in
  todo env (v1, v2)

and map_constructor_argument (env : env) (x : CST.constructor_argument) =
  match x with
  | `Choice_simple_type_rep_STAR_choice_simple_type (v1, v2) ->
      let v1 = map_simple_type_ext env v1 in
      let v2 =
        List.map
          (fun (v1, v2) ->
            let v1 = token env v1 (* "*" *) in
            let v2 = map_simple_type_ext env v2 in
            todo env (v1, v2))
          v2
      in
      todo env (v1, v2)
  | `Record_decl x -> map_record_declaration env x

and map_constructor_declaration (env : env)
    ((v1, v2) : CST.constructor_declaration) =
  let v1 =
    match v1 with
    | `Choice_capi_id x -> map_constructor_name env x
    | `Choice_LBRACK_RBRACK x -> (
        match x with
        | `LBRACK_RBRACK (v1, v2) ->
            let v1 = token env v1 (* "[" *) in
            let v2 = token env v2 (* "]" *) in
            todo env (v1, v2)
        | `LPAR_RPAR (v1, v2) ->
            let v1 = token env v1 (* "(" *) in
            let v2 = token env v2 (* ")" *) in
            todo env (v1, v2)
        | `True tok -> token env tok (* "true" *)
        | `False tok -> token env tok (* "false" *) )
  in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `Of_cons_arg (v1, v2) ->
            let v1 = token env v1 (* "of" *) in
            let v2 = map_constructor_argument env v2 in
            todo env (v1, v2)
        | `Simple_typed x -> map_simple_typed env x
        | `COLON_cons_arg_DASHGT_choice_simple_type (v1, v2, v3, v4) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = map_constructor_argument env v2 in
            let v3 = token env v3 (* "->" *) in
            let v4 = map_simple_type_ext env v4 in
            todo env (v1, v2, v3, v4)
        | `EQ_cons_path (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_constructor_path env v2 in
            todo env (v1, v2) )
    | None -> todo env ()
  in
  todo env (v1, v2)

and map_do_clause (env : env) ((v1, v2, v3) : CST.do_clause) =
  let v1 = token env v1 (* "do" *) in
  let v2 =
    match v2 with
    | Some x -> map_sequence_expression_ext env x
    | None -> todo env ()
  in
  let v3 = token env v3 (* "done" *) in
  todo env (v1, v2, v3)

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = token env v1 (* "else" *) in
  let v2 = map_expression_ext env v2 in
  todo env (v1, v2)

and map_exception_definition (env : env)
    ((v1, v2, v3, v4) : CST.exception_definition) =
  let v1 = token env v1 (* "exception" *) in
  let v2 =
    match v2 with Some x -> map_attribute env x | None -> todo env ()
  in
  let v3 = map_constructor_declaration env v3 in
  let v4 = List.map (map_item_attribute env) v4 in
  todo env (v1, v2, v3, v4)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Simple_exp x -> map_simple_expression env x
  | `Prod_exp (v1, v2, v3) ->
      let v1 = map_expression_ext env v1 in
      let v2 = token env v2 (* "," *) in
      let v3 = map_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Cons_exp (v1, v2, v3) ->
      let v1 = map_expression_ext env v1 in
      let v2 = token env v2 (* "::" *) in
      let v3 = map_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `App_exp (v1, v2) ->
      let v1 = map_simple_expression_ext env v1 in
      let v2 = List.map (map_argument env) v2 in
      todo env (v1, v2)
  | `Infix_exp x -> map_infix_expression env x
  | `Sign_exp (v1, v2) ->
      let v1 = map_sign_operator env v1 in
      let v2 = map_expression_ext env v2 in
      todo env (v1, v2)
  | `Set_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Field_get_exp x -> map_field_get_expression env x
        | `Array_get_exp x -> map_array_get_expression env x
        | `Str_get_exp x -> map_string_get_expression env x
        | `Biga_get_exp x -> map_bigarray_get_expression env x
        | `Id tok -> token env tok
        (* pattern "[a-z_][a-zA-Z0-9_']*" *)
      in
      let v2 = token env v2 (* "<-" *) in
      let v3 = map_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `If_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "if" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_sequence_expression_ext env v3 in
      let v4 = map_then_clause env v4 in
      let v5 =
        match v5 with Some x -> map_else_clause env x | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5)
  | `While_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "while" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_sequence_expression_ext env v3 in
      let v4 = map_do_clause env v4 in
      todo env (v1, v2, v3, v4)
  | `For_exp (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "for" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_value_pattern env v3 in
      let v4 = token env v4 (* "=" *) in
      let v5 = map_sequence_expression_ext env v5 in
      let v6 =
        match v6 with
        | `To tok -> token env tok (* "to" *)
        | `Downto tok -> token env tok
        (* "downto" *)
      in
      let v7 = map_sequence_expression_ext env v7 in
      let v8 = map_do_clause env v8 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Match_exp (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | `Match_opt_attr (v1, v2) ->
            let v1 = token env v1 (* "match" *) in
            let v2 =
              match v2 with
              | Some x -> map_attribute env x
              | None -> todo env ()
            in
            todo env (v1, v2)
        | `Match_op tok -> token env tok
        (* match_operator *)
      in
      let v2 = map_sequence_expression_ext env v2 in
      let v3 = token env v3 (* "with" *) in
      let v4 = map_match_cases env v4 in
      todo env (v1, v2, v3, v4)
  | `Func_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "function" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_match_cases env v3 in
      todo env (v1, v2, v3)
  | `Fun_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "fun" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = List.map (map_parameter env) v3 in
      let v4 =
        match v4 with Some x -> map_simple_typed env x | None -> todo env ()
      in
      let v5 = token env v5 (* "->" *) in
      let v6 = map_sequence_expression_ext env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Try_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "try" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_sequence_expression_ext env v3 in
      let v4 = token env v4 (* "with" *) in
      let v5 = map_match_cases env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Let_exp (v1, v2, v3) ->
      let v1 = map_value_definition env v1 in
      let v2 = token env v2 (* "in" *) in
      let v3 = map_sequence_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Assert_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "assert" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_simple_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Lazy_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "lazy" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_simple_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Let_module_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "let" *) in
      let v2 = map_module_definition env v2 in
      let v3 = token env v3 (* "in" *) in
      let v4 = map_sequence_expression_ext env v4 in
      todo env (v1, v2, v3, v4)
  | `Let_open_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "let" *) in
      let v2 = map_open_module env v2 in
      let v3 = token env v3 (* "in" *) in
      let v4 = map_sequence_expression_ext env v4 in
      todo env (v1, v2, v3, v4)
  | `Let_exc_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "let" *) in
      let v2 = map_exception_definition env v2 in
      let v3 = token env v3 (* "in" *) in
      let v4 = map_sequence_expression_ext env v4 in
      todo env (v1, v2, v3, v4)
  | `Obj_exp x -> map_object_expression env x

and map_expression_ext (env : env) (x : CST.expression_ext) =
  match x with
  | `Exp x -> map_expression env x
  | `Exte x ->
      let _ = map_extension env x in
      todo env ()

and map_expression_item (env : env) ((v1, v2) : CST.expression_item) =
  let v1 = map_sequence_expression_ext env v1 in
  let v2 = List.map (map_item_attribute env) v2 in
  todo env (v1, v2)

and map_extension (env : env) (x : CST.extension) =
  match x with
  | `Exte_ (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "[%" *) in
      let v2 = map_attribute_id env v2 in
      let v3 =
        match v3 with
        | Some x -> map_attribute_payload env x
        | None -> todo env ()
      in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Quoted_exte (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "{%" *) in
      let v2 = map_attribute_id env v2 in
      let v3 =
        match v3 with
        | Some tok -> token env tok (* pattern \s+ *)
        | None -> todo env ()
      in
      let v4 = token env v4 (* left_quoted_string_delimiter *) in
      let v5 =
        match v5 with
        | Some x -> map_quoted_string_content env x
        | None -> todo env ()
      in
      let v6 = token env v6 (* right_quoted_string_delimiter *) in
      let v7 = token env v7 (* "}" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7)

and map_external_ (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.external_) =
  let v1 = token env v1 (* "external" *) in
  let v2 =
    match v2 with Some x -> map_attribute env x | None -> todo env ()
  in
  let v3 = map_value_name env v3 in
  let v4 = map_typed env v4 in
  let v5 = token env v5 (* "=" *) in
  let v6 = List.map (map_string_ env) v6 in
  let v7 = List.map (map_item_attribute env) v7 in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_field_binding_pattern (env : env)
    ((v1, v2, v3) : CST.field_binding_pattern) =
  let v1 = map_field_path env v1 in
  let v2 = match v2 with Some x -> map_typed env x | None -> todo env () in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_binding_pattern_ext env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_field_declaration (env : env) ((v1, v2, v3) : CST.field_declaration) =
  let v1 =
    match v1 with
    | Some tok -> token env tok (* "mutable" *)
    | None -> todo env ()
  in
  let v2 = token env v2 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
  let v3 = map_polymorphic_typed env v3 in
  todo env (v1, v2, v3)

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) =
  let v1 = map_field_path env v1 in
  let v2 = match v2 with Some x -> map_typed env x | None -> todo env () in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression_ext env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_field_get_expression (env : env)
    ((v1, v2, v3) : CST.field_get_expression) =
  let v1 = map_simple_expression_ext env v1 in
  let v2 = token env v2 (* "." *) in
  let v3 = map_field_path env v3 in
  todo env (v1, v2, v3)

and map_field_pattern (env : env) ((v1, v2, v3) : CST.field_pattern) =
  let v1 = map_field_path env v1 in
  let v2 = match v2 with Some x -> map_typed env x | None -> todo env () in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_pattern_ext env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_floating_attribute (env : env)
    ((v1, v2, v3, v4) : CST.floating_attribute) =
  let v1 = token env v1 (* "[@@@" *) in
  let v2 = map_attribute_id env v2 in
  let v3 =
    match v3 with Some x -> map_attribute_payload env x | None -> todo env ()
  in
  let v4 = token env v4 (* "]" *) in
  todo env (v1, v2, v3, v4)

and map_guard (env : env) ((v1, v2) : CST.guard) =
  let v1 = token env v1 (* "when" *) in
  let v2 = map_sequence_expression_ext env v2 in
  todo env (v1, v2)

and map_infix_expression (env : env) (x : CST.infix_expression) : expr =
  match x with
  | `Choice_exp_pow_op_choice_exp (v1, v2, v3) ->
      let v1 = map_expression_ext env v1 in
      let v2 = map_pow_operator env v2 in
      let v3 = map_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Choice_exp_mult_op_choice_exp (v1, v2, v3) ->
      let v1 = map_expression_ext env v1 in
      let v2 = map_mult_operator env v2 in
      let v3 = map_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Choice_exp_add_op_choice_exp (v1, v2, v3) ->
      let v1 = map_expression_ext env v1 in
      let v2 = token env v2 (* add_operator *) in
      let v3 = map_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Choice_exp_concat_op_choice_exp (v1, v2, v3) ->
      let v1 = map_expression_ext env v1 in
      let v2 = token env v2 (* concat_operator *) in
      let v3 = map_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Choice_exp_rel_op_choice_exp (v1, v2, v3) ->
      let v1 = map_expression_ext env v1 in
      let v2 = token env v2 (* rel_operator *) in
      let v3 = map_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Choice_exp_and_op__choice_exp (v1, v2, v3) ->
      let v1 = map_expression_ext env v1 in
      let v2 = map_and_operator_ env v2 in
      let v3 = map_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Choice_exp_or_op_choice_exp (v1, v2, v3) ->
      let v1 = map_expression_ext env v1 in
      let v2 = map_or_operator env v2 in
      let v3 = map_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Choice_exp_assign_op_choice_exp (v1, v2, v3) ->
      let v1 = map_expression_ext env v1 in
      let v2 = map_assign_operator env v2 in
      let v3 = map_expression_ext env v3 in
      todo env (v1, v2, v3)

and map_instance_variable_expression (env : env)
    ((v1, v2) : CST.instance_variable_expression) =
  let v1 = token env v1 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression_ext env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2)

and map_item_attribute (env : env) ((v1, v2, v3, v4) : CST.item_attribute) =
  let v1 = token env v1 (* "[@@" *) in
  let v2 = map_attribute_id env v2 in
  let v3 =
    match v3 with Some x -> map_attribute_payload env x | None -> todo env ()
  in
  let v4 = token env v4 (* "]" *) in
  todo env (v1, v2, v3, v4)

and map_item_extension (env : env) (x : CST.item_extension) =
  match x with
  | `Item_exte_ (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "[%%" *) in
      let v2 = map_attribute_id env v2 in
      let v3 =
        match v3 with
        | Some x -> map_attribute_payload env x
        | None -> todo env ()
      in
      let v4 = token env v4 (* "]" *) in
      let v5 = List.map (map_item_attribute env) v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Quoted_item_exte (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "{%%" *) in
      let v2 = map_attribute_id env v2 in
      let v3 =
        match v3 with
        | Some tok -> token env tok (* pattern \s+ *)
        | None -> todo env ()
      in
      let v4 = token env v4 (* left_quoted_string_delimiter *) in
      let v5 =
        match v5 with
        | Some x -> map_quoted_string_content env x
        | None -> todo env ()
      in
      let v6 = token env v6 (* right_quoted_string_delimiter *) in
      let v7 = token env v7 (* "}" *) in
      let v8 = List.map (map_item_attribute env) v8 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)

and map_labeled_argument (env : env) (x : CST.labeled_argument) =
  match x with
  | `Label x -> map_label env x
  | `Label_imm_tok_COLON_choice_simple_exp (v1, v2, v3) ->
      let v1 = map_label env v1 in
      let v2 = token env v2 (* ":" *) in
      let v3 = map_simple_expression_ext env v3 in
      todo env (v1, v2, v3)

and map_let_binding (env : env) ((v1, v2, v3) : CST.let_binding) =
  let v1 = map_binding_pattern_ext env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3, v4, v5) ->
        let v1 = List.map (map_parameter env) v1 in
        let v2 =
          match v2 with
          | Some x -> map_polymorphic_typed env x
          | None -> todo env ()
        in
        let v3 =
          match v3 with
          | Some (v1, v2) ->
              let v1 = token env v1 (* ":>" *) in
              let v2 = map_type_ext env v2 in
              todo env (v1, v2)
          | None -> todo env ()
        in
        let v4 = token env v4 (* "=" *) in
        let v5 = map_sequence_expression_ext env v5 in
        todo env (v1, v2, v3, v4, v5)
    | None -> todo env ()
  in
  let v3 = List.map (map_item_attribute env) v3 in
  todo env (v1, v2, v3)

and map_list_binding_pattern (env : env)
    ((v1, v2, v3) : CST.list_binding_pattern) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | Some x ->
        map_anon_bind_pat_ext_rep_SEMI_bind_pat_ext_opt_SEMI_38caf30 env x
    | None -> todo env ()
  in
  let v3 = token env v3 (* "]" *) in
  todo env (v1, v2, v3)

and map_list_expression (env : env) ((v1, v2, v3) : CST.list_expression) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | Some x -> map_anon_exp_ext_rep_SEMI_exp_ext_opt_SEMI_f0de170 env x
    | None -> todo env ()
  in
  let v3 = token env v3 (* "]" *) in
  todo env (v1, v2, v3)

and map_list_pattern (env : env) ((v1, v2, v3) : CST.list_pattern) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | Some x -> map_anon_pat_ext_rep_SEMI_pat_ext_opt_SEMI_3830e8c env x
    | None -> todo env ()
  in
  let v3 = token env v3 (* "]" *) in
  todo env (v1, v2, v3)

and map_match_case (env : env) ((v1, v2, v3, v4) : CST.match_case) =
  let v1 = map_pattern_ext env v1 in
  let v2 = match v2 with Some x -> map_guard env x | None -> todo env () in
  let v3 = token env v3 (* "->" *) in
  let v4 =
    match v4 with
    | `Seq_exp_ext x -> map_sequence_expression_ext env x
    | `Refu_case tok -> token env tok
    (* "." *)
  in
  todo env (v1, v2, v3, v4)

and map_match_cases (env : env) ((v1, v2, v3) : CST.match_cases) =
  let v1 =
    match v1 with Some tok -> token env tok (* "|" *) | None -> todo env ()
  in
  let v2 = map_match_case env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "|" *) in
        let v2 = map_match_case env v2 in
        todo env (v1, v2))
      v3
  in
  todo env (v1, v2, v3)

and map_module_binding (env : env) ((v1, v2, v3, v4, v5) : CST.module_binding) =
  let v1 = map_anon_choice_module_name_7ad5569 env v1 in
  let v2 = List.map (map_module_parameter env) v2 in
  let v3 =
    match v3 with Some x -> map_module_typed env x | None -> todo env ()
  in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_module_expression_ext env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v5 = List.map (map_item_attribute env) v5 in
  todo env (v1, v2, v3, v4, v5)

and map_module_definition (env : env)
    ((v1, v2, v3, v4, v5) : CST.module_definition) =
  let v1 = token env v1 (* "module" *) in
  let v2 =
    match v2 with Some x -> map_attribute env x | None -> todo env ()
  in
  let v3 =
    match v3 with Some tok -> token env tok (* "rec" *) | None -> todo env ()
  in
  let v4 = map_module_binding env v4 in
  let v5 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "and" *) in
        let v2 = map_module_binding env v2 in
        todo env (v1, v2))
      v5
  in
  todo env (v1, v2, v3, v4, v5)

and map_module_expression (env : env) (x : CST.module_expression) =
  match x with
  | `Simple_module_exp x -> map_simple_module_expression env x
  | `Module_path x -> map_module_path env x
  | `Stru_ (v1, v2, v3) ->
      let v1 = token env v1 (* "struct" *) in
      let v2 =
        match v2 with Some x -> map_structure env x | None -> todo env ()
      in
      let v3 = token env v3 (* "end" *) in
      todo env (v1, v2, v3)
  | `Func (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "functor" *) in
      let v2 = List.map (map_module_parameter env) v2 in
      let v3 = token env v3 (* "->" *) in
      let v4 = map_module_expression_ext env v4 in
      todo env (v1, v2, v3, v4)
  | `Module_app (v1, v2) ->
      let v1 = map_module_expression_ext env v1 in
      let v2 =
        match v2 with
        | `Simple_module_exp_ext x -> map_simple_module_expression_ext env x
        | `LPAR_RPAR (v1, v2) ->
            let v1 = token env v1 (* "(" *) in
            let v2 = token env v2 (* ")" *) in
            todo env (v1, v2)
      in
      todo env (v1, v2)

and map_module_expression_ext (env : env) (x : CST.module_expression_ext) =
  match x with
  | `Module_exp x -> map_module_expression env x
  | `Exte x -> map_extension env x

and map_module_parameter (env : env) ((v1, v2, v3) : CST.module_parameter) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_module_name_7ad5569 env v1 in
        let v2 = map_module_typed env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_module_type (env : env) (x : CST.module_type) =
  match x with
  | `Module_type_path x -> map_module_type_path env x
  | `Sign_ (v1, v2, v3) ->
      let v1 = token env v1 (* "sig" *) in
      let v2 =
        match v2 with Some x -> map_signature env x | None -> todo env ()
      in
      let v3 = token env v3 (* "end" *) in
      todo env (v1, v2, v3)
  | `Module_type_cons (v1, v2, v3, v4) ->
      let v1 = map_module_type_ext env v1 in
      let v2 = token env v2 (* "with" *) in
      let v3 = map_anon_choice_cons_type_771aabb env v3 in
      let v4 =
        List.map
          (fun (v1, v2) ->
            let v1 = token env v1 (* "and" *) in
            let v2 = map_anon_choice_cons_type_771aabb env v2 in
            todo env (v1, v2))
          v4
      in
      todo env (v1, v2, v3, v4)
  | `Module_type_of (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "module" *) in
      let v2 = token env v2 (* "type" *) in
      let v3 = token env v3 (* "of" *) in
      let v4 = map_module_expression_ext env v4 in
      todo env (v1, v2, v3, v4)
  | `Func_type (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Func_rep_module_param (v1, v2) ->
            let v1 = token env v1 (* "functor" *) in
            let v2 = List.map (map_module_parameter env) v2 in
            todo env (v1, v2)
        | `Choice_module_type x -> map_module_type_ext env x
      in
      let v2 = token env v2 (* "->" *) in
      let v3 = map_module_type_ext env v3 in
      todo env (v1, v2, v3)
  | `Paren_module_type (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_module_type_ext env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)

and map_module_type_definition (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.module_type_definition) =
  let v1 = token env v1 (* "module" *) in
  let v2 = token env v2 (* "type" *) in
  let v3 =
    match v3 with Some x -> map_attribute env x | None -> todo env ()
  in
  let v4 = map_module_type_name env v4 in
  let v5 =
    match v5 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_EQ_4ccabd6 env v1 in
        let v2 = map_module_type_ext env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v6 = List.map (map_item_attribute env) v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_module_type_ext (env : env) (x : CST.module_type_ext) =
  match x with
  | `Module_type x -> map_module_type env x
  | `Exte x -> map_extension env x

and map_module_typed (env : env) ((v1, v2) : CST.module_typed) =
  let v1 = token env v1 (* ":" *) in
  let v2 = map_module_type_ext env v2 in
  todo env (v1, v2)

and map_object_copy_expression (env : env)
    ((v1, v2, v3, v4) : CST.object_copy_expression) =
  let v1 = token env v1 (* "{<" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_instance_variable_expression env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = token env v1 (* ";" *) in
              let v2 = map_instance_variable_expression env v2 in
              todo env (v1, v2))
            v2
        in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v3 =
    match v3 with Some tok -> token env tok (* ";" *) | None -> todo env ()
  in
  let v4 = token env v4 (* ">}" *) in
  todo env (v1, v2, v3, v4)

and map_object_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.object_expression) =
  let v1 = token env v1 (* "object" *) in
  let v2 =
    match v2 with Some x -> map_attribute env x | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some (v1, v2, v3, v4) ->
        let v1 = token env v1 (* "(" *) in
        let v2 = map_pattern_ext env v2 in
        let v3 =
          match v3 with Some x -> map_typed env x | None -> todo env ()
        in
        let v4 = token env v4 (* ")" *) in
        todo env (v1, v2, v3, v4)
    | None -> todo env ()
  in
  let v4 =
    List.map
      (fun x ->
        match x with
        | `Class_field_ext x -> map_class_field_ext env x
        | `Floa_attr x -> map_floating_attribute env x)
      v4
  in
  let v5 = token env v5 (* "end" *) in
  todo env (v1, v2, v3, v4, v5)

and map_open_module (env : env) ((v1, v2, v3, v4, v5) : CST.open_module) =
  let v1 = token env v1 (* "open" *) in
  let v2 =
    match v2 with Some tok -> token env tok (* "!" *) | None -> todo env ()
  in
  let v3 =
    match v3 with Some x -> map_attribute env x | None -> todo env ()
  in
  let v4 = map_module_expression_ext env v4 in
  let v5 = List.map (map_item_attribute env) v5 in
  todo env (v1, v2, v3, v4, v5)

and map_package_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.package_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = token env v2 (* "module" *) in
  let v3 =
    match v3 with Some x -> map_attribute env x | None -> todo env ()
  in
  let v4 = map_module_expression_ext env v4 in
  let v5 =
    match v5 with Some x -> map_module_typed env x | None -> todo env ()
  in
  let v6 = token env v6 (* ")" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_package_pattern (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.package_pattern) =
  let v1 = token env v1 (* "(" *) in
  let v2 = token env v2 (* "module" *) in
  let v3 =
    match v3 with Some x -> map_attribute env x | None -> todo env ()
  in
  let v4 = map_anon_choice_module_name_7ad5569 env v4 in
  let v5 =
    match v5 with Some x -> map_module_typed env x | None -> todo env ()
  in
  let v6 = token env v6 (* ")" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_parameter (env : env) (x : CST.parameter) =
  match x with
  | `Param_ x -> map_parameter_ env x
  | `Paren_abst_type x -> map_parenthesized_abstract_type env x

and map_parameter_ (env : env) (x : CST.parameter_) =
  match x with
  | `Simple_pat_ext x -> map_simple_pattern_ext env x
  | `Choice_TILDE_id x -> map_label env x
  | `Label_imm_tok_COLON_simple_pat_ext (v1, v2, v3) ->
      let v1 = map_label env v1 in
      let v2 = token env v2 (* ":" *) in
      let v3 = map_simple_pattern_ext env v3 in
      todo env (v1, v2, v3)
  | `Choice_TILDE_LPAR_id_opt_typed_opt_EQ_seq_exp_ext_RPAR
      (v1, v2, v3, v4, v5, v6) ->
      let v1 = map_anon_choice_TILDE_72781e5 env v1 in
      let v2 = token env v2 (* "(" *) in
      let v3 = token env v3 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
      let v4 =
        match v4 with Some x -> map_typed env x | None -> todo env ()
      in
      let v5 =
        match v5 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_sequence_expression_ext env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v6 = token env v6 (* ")" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Label_imm_tok_COLON_LPAR_pat_ext_opt_typed_EQ_seq_exp_ext_RPAR
      (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = map_label env v1 in
      let v2 = token env v2 (* ":" *) in
      let v3 = token env v3 (* "(" *) in
      let v4 = map_pattern_ext env v4 in
      let v5 =
        match v5 with Some x -> map_typed env x | None -> todo env ()
      in
      let v6 = token env v6 (* "=" *) in
      let v7 = map_sequence_expression_ext env v7 in
      let v8 = token env v8 (* ")" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)

and map_parenthesized_expression (env : env) (x : CST.parenthesized_expression)
    =
  match x with
  | `Begin_opt_attr_seq_exp_ext_end (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "begin" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_sequence_expression_ext env v3 in
      let v4 = token env v4 (* "end" *) in
      todo env (v1, v2, v3, v4)
  | `LPAR_seq_exp_ext_RPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_sequence_expression_ext env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)

and map_parenthesized_type (env : env) ((v1, v2, v3) : CST.parenthesized_type) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_type_ext env v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)
and map_pattern (env : env) (x : CST.pattern) : pattern =
  match x with
  | `Simple_pat x -> map_simple_pattern env x
  | `Alias_pat (v1, v2, v3) ->
      let v1 = map_pattern_ext env v1 in
      let v2 = token env v2 (* "as" *) in
      let v3 = map_value_pattern env v3 in
      todo env (v1, v2, v3)
  | `Or_pat (v1, v2, v3) ->
      let v1 = map_pattern_ext env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_pattern_ext env v3 in
      todo env (v1, v2, v3)
  | `Cons_pat_4ec55c1 (v1, v2, v3) ->
      let v1 = map_constructor_path env v1 in
      let v2 =
        match v2 with
        | Some x -> map_parenthesized_abstract_type env x
        | None -> todo env ()
      in
      let v3 = map_pattern_ext env v3 in
      todo env (v1, v2, v3)
  | `Tag_pat (v1, v2) ->
      let v1 = map_tag env v1 in
      let v2 = map_pattern_ext env v2 in
      todo env (v1, v2)
  | `Tuple_pat (v1, v2, v3) ->
      let v1 = map_pattern_ext env v1 in
      let v2 = token env v2 (* "," *) in
      let v3 = map_pattern_ext env v3 in
      todo env (v1, v2, v3)
  | `Cons_pat_9b4e481 (v1, v2, v3) ->
      let v1 = map_pattern_ext env v1 in
      let v2 = token env v2 (* "::" *) in
      let v3 = map_pattern_ext env v3 in
      todo env (v1, v2, v3)
  | `Range_pat x -> map_range_pattern env x
  | `Lazy_pat (v1, v2, v3) ->
      let v1 = token env v1 (* "lazy" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_pattern_ext env v3 in
      todo env (v1, v2, v3)
  | `Exc_pat (v1, v2, v3) ->
      let v1 = token env v1 (* "exception" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_pattern_ext env v3 in
      todo env (v1, v2, v3)

and map_pattern_ext (env : env) (x : CST.pattern_ext) =
  match x with
  | `Pat x -> map_pattern env x
  | `Exte x ->
      let _ = map_extension env x in
      todo env ()

and map_polymorphic_type (env : env) (x : CST.polymorphic_type) =
  match x with
  | `Poly_type_ (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Rep1_type_var xs -> List.map (map_type_variable env) xs
        | `Abst_type x -> map_abstract_type env x
      in
      let v2 = token env v2 (* "." *) in
      let v3 = map_type_ext env v3 in
      todo env (v1, v2, v3)
  | `Type_ext x -> map_type_ext env x

and map_polymorphic_typed (env : env) ((v1, v2) : CST.polymorphic_typed) =
  let v1 = token env v1 (* ":" *) in
  let v2 = map_polymorphic_type env v2 in
  todo env (v1, v2)

and map_record_binding_pattern (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.record_binding_pattern) =
  let v1 = token env v1 (* "{" *) in
  let v2 = map_field_binding_pattern env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* ";" *) in
        let v2 = map_field_binding_pattern env v2 in
        todo env (v1, v2))
      v3
  in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* ";" *) in
        let v2 = token env v2 (* "_" *) in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v5 =
    match v5 with Some tok -> token env tok (* ";" *) | None -> todo env ()
  in
  let v6 = token env v6 (* "}" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_record_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.record_declaration) =
  let v1 = token env v1 (* "{" *) in
  let v2 = map_field_declaration env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* ";" *) in
        let v2 = map_field_declaration env v2 in
        todo env (v1, v2))
      v3
  in
  let v4 =
    match v4 with Some tok -> token env tok (* ";" *) | None -> todo env ()
  in
  let v5 = token env v5 (* "}" *) in
  todo env (v1, v2, v3, v4, v5)

and map_record_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.record_expression) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_simple_expression_ext env v1 in
        let v2 = token env v2 (* "with" *) in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v3 = map_field_expression env v3 in
  let v4 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* ";" *) in
        let v2 = map_field_expression env v2 in
        todo env (v1, v2))
      v4
  in
  let v5 =
    match v5 with Some tok -> token env tok (* ";" *) | None -> todo env ()
  in
  let v6 = token env v6 (* "}" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_record_pattern (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.record_pattern) =
  let v1 = token env v1 (* "{" *) in
  let v2 = map_field_pattern env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* ";" *) in
        let v2 = map_field_pattern env v2 in
        todo env (v1, v2))
      v3
  in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* ";" *) in
        let v2 = token env v2 (* "_" *) in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v5 =
    match v5 with Some tok -> token env tok (* ";" *) | None -> todo env ()
  in
  let v6 = token env v6 (* "}" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_sequence_expression_ (env : env) (x : CST.sequence_expression_) =
  match x with
  | `Exp x -> map_expression env x
  | `Seq_exp (v1, v2, v3) ->
      let v1 = map_expression_ext env v1 in
      let v2 = token env v2 (* ";" *) in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 =
              match v1 with
              | Some x -> map_attribute env x
              | None -> todo env ()
            in
            let v2 = map_sequence_expression_ext env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3)

and map_sequence_expression_ext (env : env) (x : CST.sequence_expression_ext) =
  match x with
  | `Seq_exp_ x -> map_sequence_expression_ env x
  | `Exte x -> map_extension env x

(*****************************************************************************)
(* Signature *)
(*****************************************************************************)
and map_signature (env : env) (x : CST.signature) =
  match x with
  | `Rep1_SEMISEMI xs -> List.map (token env) (* ";;" *) xs
  | `Rep1_rep_SEMISEMI_sign_item_ext_rep_SEMISEMI (v1, v2) ->
      let v1 =
        List.map
          (fun (v1, v2) ->
            let v1 = List.map (token env) (* ";;" *) v1 in
            let v2 = map_signature_item_ext env v2 in
            todo env (v1, v2))
          v1
      in
      let v2 = List.map (token env) (* ";;" *) v2 in
      todo env (v1, v2)

and map_signature_item (env : env) (x : CST.signature_item) =
  match x with
  | `Value_spec (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "val" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_value_name env v3 in
      let v4 = map_typed env v4 in
      let v5 = List.map (map_item_attribute env) v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Exte x -> map_external_ env x
  | `Type_defi x -> map_type_definition env x
  | `Exc_defi x -> map_exception_definition env x
  | `Module_defi x -> map_module_definition env x
  | `Module_type_defi x -> map_module_type_definition env x
  | `Open_module x -> map_open_module env x
  | `Incl_module_type (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "include" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_module_type_ext env v3 in
      let v4 = List.map (map_item_attribute env) v4 in
      todo env (v1, v2, v3, v4)
  | `Class_defi x -> map_class_definition env x
  | `Class_type_defi x -> map_class_type_definition env x
  | `Floa_attr x -> map_floating_attribute env x

and map_signature_item_ext (env : env) (x : CST.signature_item_ext) =
  match x with
  | `Sign_item x -> map_signature_item env x
  | `Item_exte x -> map_item_extension env x

and map_simple_class_expression (env : env) (x : CST.simple_class_expression) =
  match x with
  | `Class_path x -> map_class_path env x
  | `Inst_class (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = map_type_ext env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_type_ext env v2 in
            todo env (v1, v2))
          v3
      in
      let v4 = token env v4 (* "]" *) in
      let v5 = map_class_path env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Obj_exp x -> map_object_expression env x
  | `Typed_class_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_class_expression_ext env v2 in
      let v3 = map_class_typed env v3 in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Paren_class_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_class_expression_ext env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)

and map_simple_class_type (env : env) (x : CST.simple_class_type) =
  match x with
  | `Class_type_path x -> map_class_type_path env x
  | `Inst_class_type (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = map_type_ext env v2 in
      let v3 =
        List.map
          (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_type_ext env v2 in
            todo env (v1, v2))
          v3
      in
      let v4 = token env v4 (* "]" *) in
      let v5 = map_class_type_path env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Class_body_type (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "object" *) in
      let v2 =
        match v2 with
        | Some x -> map_parenthesized_type env x
        | None -> todo env ()
      in
      let v3 =
        List.map
          (fun x ->
            match x with
            | `Class_field_spec_ext x -> map_class_field_specification_ext env x
            | `Floa_attr x -> map_floating_attribute env x)
          v3
      in
      let v4 = token env v4 (* "end" *) in
      todo env (v1, v2, v3, v4)
  | `Let_open_class_type (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "let" *) in
      let v2 = map_open_module env v2 in
      let v3 = token env v3 (* "in" *) in
      let v4 = map_simple_class_type_ext env v4 in
      todo env (v1, v2, v3, v4)

and map_simple_class_type_ext (env : env) (x : CST.simple_class_type_ext) =
  match x with
  | `Simple_class_type x -> map_simple_class_type env x
  | `Exte x -> map_extension env x

and map_simple_expression (env : env) (x : CST.simple_expression) : expr =
  match x with
  | `Value_path x -> map_value_path env x
  | `Cst x -> map_constant env x
  | `Typed_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_sequence_expression_ext env v2 in
      let v3 = map_typed env v3 in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Cons_path x -> map_constructor_path env x
  | `Tag x -> map_tag env x
  | `List_exp x -> map_list_expression env x
  | `Array_exp x -> map_array_expression env x
  | `Record_exp x -> map_record_expression env x
  | `Prefix_exp (v1, v2) ->
      let v1 = token env v1 (* prefix_operator *) in
      let v2 = map_simple_expression_ext env v2 in
      todo env (v1, v2)
  | `Hash_exp (v1, v2, v3) ->
      let v1 = map_simple_expression_ext env v1 in
      let v2 = token env v2 (* hash_operator *) in
      let v3 = map_simple_expression_ext env v3 in
      todo env (v1, v2, v3)
  | `Field_get_exp x -> map_field_get_expression env x
  | `Array_get_exp x -> map_array_get_expression env x
  | `Str_get_exp x -> map_string_get_expression env x
  | `Biga_get_exp x -> map_bigarray_get_expression env x
  | `Coer_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_sequence_expression_ext env v2 in
      let v3 =
        match v3 with Some x -> map_typed env x | None -> todo env ()
      in
      let v4 = token env v4 (* ":>" *) in
      let v5 = map_type_ext env v5 in
      let v6 = token env v6 (* ")" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Local_open_exp (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 =
        match v3 with
        | `LPAR_opt_seq_exp_ext_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 =
              match v2 with
              | Some x -> map_sequence_expression_ext env x
              | None -> todo env ()
            in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        | `List_exp x -> map_list_expression env x
        | `Array_exp x -> map_array_expression env x
        | `Record_exp x -> map_record_expression env x
        | `Obj_copy_exp x -> map_object_copy_expression env x
        | `Pack_exp x -> map_package_expression env x
      in
      todo env (v1, v2, v3)
  | `Pack_exp x -> map_package_expression env x
  | `New_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "new" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_class_path env v3 in
      todo env (v1, v2, v3)
  | `Obj_copy_exp x -> map_object_copy_expression env x
  | `Meth_invo (v1, v2, v3) ->
      let v1 = map_simple_expression_ext env v1 in
      let v2 = token env v2 (* "#" *) in
      let v3 = token env v3 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
      todo env (v1, v2, v3)
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Ocam_value tok -> token env tok

(* pattern \$[0-9]+ *)
and map_simple_expression_ext (env : env) (x : CST.simple_expression_ext) =
  match x with
  | `Simple_exp x ->
      let _ = map_simple_expression env x in
      todo env ()
  | `Exte x ->
      let _ = map_extension env x in
      todo env ()

and map_simple_module_expression (env : env) (x : CST.simple_module_expression)
    =
  match x with
  | `Typed_module_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_module_expression_ext env v2 in
      let v3 = map_module_typed env v3 in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Paren_module_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_module_expression_ext env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  | `Packed_module (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = token env v2 (* "val" *) in
      let v3 = map_expression_ext env v3 in
      let v4 =
        match v4 with Some x -> map_module_typed env x | None -> todo env ()
      in
      let v5 =
        match v5 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":>" *) in
            let v2 = map_module_type_ext env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v6 = token env v6 (* ")" *) in
      todo env (v1, v2, v3, v4, v5, v6)

and map_simple_module_expression_ext (env : env)
    (x : CST.simple_module_expression_ext) =
  match x with
  | `Simple_module_exp x -> map_simple_module_expression env x
  | `Exte x -> map_extension env x

and map_simple_pattern (env : env) (x : CST.simple_pattern) : pattern =
  match x with
  | `Value_pat x ->
      let _ = map_value_pattern env x in
      todo env ()
  | `Signed_cst x ->
      let _ = map_signed_constant env x in
      todo env ()
  | `Typed_pat (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_pattern_ext env v2 in
      let v3 = map_typed env v3 in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Cons_path x -> map_constructor_path env x
  | `Tag x -> map_tag env x
  | `Poly_vari_pat x -> map_polymorphic_variant_pattern env x
  | `Record_pat x -> map_record_pattern env x
  | `List_pat x -> map_list_pattern env x
  | `Array_pat x -> map_array_pattern env x
  | `Local_open_pat (v1, v2, v3) ->
      let v1 = map_module_path env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 =
        match v3 with
        | `LPAR_opt_pat_ext_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 =
              match v2 with
              | Some x -> map_pattern_ext env x
              | None -> todo env ()
            in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        | `List_pat x -> map_list_pattern env x
        | `Array_pat x -> map_array_pattern env x
        | `Record_pat x -> map_record_pattern env x
      in
      todo env (v1, v2, v3)
  | `Pack_pat x ->
      let _ = map_package_pattern env x in
      todo env ()
  | `Paren_pat (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_pattern_ext env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)

and map_simple_pattern_ext (env : env) (x : CST.simple_pattern_ext) =
  match x with
  | `Simple_pat x -> map_simple_pattern env x
  | `Exte x ->
      let _ = map_extension env x in
      todo env ()

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
and map_simple_type (env : env) (x : CST.simple_type) : type_ =
  match x with
  | `Type_var x -> map_type_variable env x
  | `Type_cons_path x -> map_type_constructor_path env x
  | `Cons_type (v1, v2) ->
      let v1 = map_anon_choice_simple_type_ext_30dd028 env v1 in
      let v2 = map_type_constructor_path env v2 in
      todo env (v1, v2)
  | `Poly_vari_type v1 -> (
      match v1 with
      | `LBRACK_tag_spec_RBRACK (v1, v2, v3) ->
          let v1 = token env v1 (* "[" *) in
          let v2 = map_tag_specification env v2 in
          let v3 = token env v3 (* "]" *) in
          todo env (v1, v2, v3)
      | `LBRACK_opt_tag_spec_BAR_tag_spec_rep_BAR_tag_spec_RBRACK
          (v1, v2, v3, v4, v5, v6) ->
          let v1 = token env v1 (* "[" *) in
          let v2 =
            match v2 with Some x -> map_tag_spec env x | None -> todo env ()
          in
          let v3 = token env v3 (* "|" *) in
          let v4 = map_tag_spec env v4 in
          let v5 =
            List.map
              (fun (v1, v2) ->
                let v1 = token env v1 (* "|" *) in
                let v2 = map_tag_spec env v2 in
                todo env (v1, v2))
              v5
          in
          let v6 = token env v6 (* "]" *) in
          todo env (v1, v2, v3, v4, v5, v6)
      | `LBRACKGT_opt_BAR_opt_tag_spec_rep_BAR_tag_spec_RBRACK (v1, v2, v3, v4)
        ->
          let v1 = token env v1 (* "[>" *) in
          let v2 =
            match v2 with
            | Some tok -> token env tok (* "|" *)
            | None -> todo env ()
          in
          let v3 =
            match v3 with
            | Some (v1, v2) ->
                let v1 = map_tag_spec env v1 in
                let v2 =
                  List.map
                    (fun (v1, v2) ->
                      let v1 = token env v1 (* "|" *) in
                      let v2 = map_tag_spec env v2 in
                      todo env (v1, v2))
                    v2
                in
                todo env (v1, v2)
            | None -> todo env ()
          in
          let v4 = token env v4 (* "]" *) in
          todo env (v1, v2, v3, v4)
      | `LBRACKLT_opt_BAR_tag_spec_rep_BAR_tag_spec_opt_GT_rep1_tag_RBRACK
          (v1, v2, v3, v4, v5, v6) ->
          let v1 = token env v1 (* "[<" *) in
          let v2 =
            match v2 with
            | Some tok -> token env tok (* "|" *)
            | None -> todo env ()
          in
          let v3 = map_tag_spec env v3 in
          let v4 =
            List.map
              (fun (v1, v2) ->
                let v1 = token env v1 (* "|" *) in
                let v2 = map_tag_spec env v2 in
                todo env (v1, v2))
              v4
          in
          let v5 =
            match v5 with
            | Some (v1, v2) ->
                let v1 = token env v1 (* ">" *) in
                let v2 = List.map (map_tag env) v2 in
                todo env (v1, v2)
            | None -> todo env ()
          in
          let v6 = token env v6 (* "]" *) in
          todo env (v1, v2, v3, v4, v5, v6) )
  | `Pack_type (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = token env v2 (* "module" *) in
      let v3 =
        match v3 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v4 = map_module_type_ext env v4 in
      let v5 = token env v5 (* ")" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Hash_type (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some x -> map_anon_choice_simple_type_ext_30dd028 env x
        | None -> todo env ()
      in
      let v2 = token env v2 (* "#" *) in
      let v3 = map_class_type_path env v3 in
      todo env (v1, v2, v3)
  | `Obj_type (v1, v2, v3) ->
      let v1 = token env v1 (* "<" *) in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `Choice_meth_type_rep_SEMI_choice_meth_type_opt_SEMI_opt_DOTDOT
                (v1, v2, v3) ->
                let v1 = map_anon_choice_meth_type_345b567 env v1 in
                let v2 =
                  List.map
                    (fun (v1, v2) ->
                      let v1 = token env v1 (* ";" *) in
                      let v2 = map_anon_choice_meth_type_345b567 env v2 in
                      todo env (v1, v2))
                    v2
                in
                let v3 =
                  match v3 with
                  | Some (v1, v2) ->
                      let v1 = token env v1 (* ";" *) in
                      let v2 =
                        match v2 with
                        | Some tok -> token env tok (* ".." *)
                        | None -> todo env ()
                      in
                      todo env (v1, v2)
                  | None -> todo env ()
                in
                todo env (v1, v2, v3)
            | `DOTDOT tok -> token env tok (* ".." *) )
        | None -> todo env ()
      in
      let v3 = token env v3 (* ">" *) in
      todo env (v1, v2, v3)
  | `Paren_type x -> map_parenthesized_type env x

and map_simple_type_ext (env : env) (x : CST.simple_type_ext) =
  match x with
  | `Simple_type x -> map_simple_type env x
  | `Exte x ->
      let _ = map_extension env x in
      todo env ()

and map_simple_typed (env : env) ((v1, v2) : CST.simple_typed) =
  let v1 = token env v1 (* ":" *) in
  let v2 = map_simple_type_ext env v2 in
  todo env (v1, v2)

and map_string_get_expression (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.string_get_expression) =
  let v1 = map_simple_expression_ext env v1 in
  let v2 = token env v2 (* "." *) in
  let v3 =
    match v3 with
    | Some x -> map_indexing_operator_path env x
    | None -> todo env ()
  in
  let v4 = token env v4 (* "[" *) in
  let v5 = map_sequence_expression_ext env v5 in
  let v6 = token env v6 (* "]" *) in
  todo env (v1, v2, v3, v4, v5, v6)

(*****************************************************************************)
(* Structure *)
(*****************************************************************************)
and map_structure (env : env) (x : CST.structure) =
  match x with
  | `Rep1_SEMISEMI xs -> List.map (token env) (* ";;" *) xs
  | `Rep_SEMISEMI_choice_stru_item_ext_rep_choice_rep_SEMISEMI_choice_stru_item_ext_rep_SEMISEMI
      (v1, v2, v3, v4) ->
      let v1 = List.map (token env) (* ";;" *) v1 in
      let v2 =
        match v2 with
        | `Stru_item_ext x -> map_structure_item_ext env x
        | `Topl_dire x -> map_toplevel_directive env x
        | `Exp_item x -> map_expression_item env x
      in
      let v3 =
        List.map
          (fun x ->
            match x with
            | `Rep_SEMISEMI_choice_stru_item_ext (v1, v2) ->
                let v1 = List.map (token env) (* ";;" *) v1 in
                let v2 =
                  match v2 with
                  | `Stru_item_ext x -> map_structure_item_ext env x
                  | `Topl_dire x -> map_toplevel_directive env x
                in
                todo env (v1, v2)
            | `Rep1_SEMISEMI_exp_item (v1, v2) ->
                let v1 = List.map (token env) (* ";;" *) v1 in
                let v2 = map_expression_item env v2 in
                todo env (v1, v2))
          v3
      in
      let v4 = List.map (token env) (* ";;" *) v4 in
      todo env (v1, v2, v3, v4)

and map_structure_item (env : env) (x : CST.structure_item) =
  match x with
  | `Value_defi x -> map_value_definition env x
  | `Exte x -> map_external_ env x
  | `Type_defi x -> map_type_definition env x
  | `Exc_defi x -> map_exception_definition env x
  | `Module_defi x -> map_module_definition env x
  | `Module_type_defi x -> map_module_type_definition env x
  | `Open_module x -> map_open_module env x
  | `Incl_module (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "include" *) in
      let v2 =
        match v2 with Some x -> map_attribute env x | None -> todo env ()
      in
      let v3 = map_module_expression_ext env v3 in
      let v4 = List.map (map_item_attribute env) v4 in
      todo env (v1, v2, v3, v4)
  | `Class_defi x -> map_class_definition env x
  | `Class_type_defi x -> map_class_type_definition env x
  | `Floa_attr x -> map_floating_attribute env x

and map_structure_item_ext (env : env) (x : CST.structure_item_ext) =
  match x with
  | `Stru_item x -> map_structure_item env x
  | `Item_exte x -> map_item_extension env x

and map_tag_spec (env : env) (x : CST.tag_spec) =
  match x with
  | `Type_ext x -> map_type_ext env x
  | `Tag_spec x -> map_tag_specification env x

and map_tag_specification (env : env) ((v1, v2) : CST.tag_specification) =
  let v1 = map_tag env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 = token env v1 (* "of" *) in
        let v2 =
          match v2 with
          | Some tok -> token env tok (* "&" *)
          | None -> todo env ()
        in
        let v3 = map_type_ext env v3 in
        let v4 =
          List.map
            (fun (v1, v2) ->
              let v1 = token env v1 (* "&" *) in
              let v2 = map_type_ext env v2 in
              todo env (v1, v2))
            v4
        in
        todo env (v1, v2, v3, v4)
    | None -> todo env ()
  in
  todo env (v1, v2)

and map_then_clause (env : env) ((v1, v2) : CST.then_clause) =
  let v1 = token env v1 (* "then" *) in
  let v2 = map_expression_ext env v2 in
  todo env (v1, v2)

and map_tuple_type_ (env : env) (x : CST.tuple_type_) =
  match x with
  | `Simple_type x -> map_simple_type env x
  | `Tuple_type (v1, v2, v3) ->
      let v1 = map_tuple_type_ext env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = map_simple_type_ext env v3 in
      todo env (v1, v2, v3)

and map_tuple_type_ext (env : env) (x : CST.tuple_type_ext) =
  match x with
  | `Tuple_type_ x -> map_tuple_type_ env x
  | `Exte x ->
      let _ = map_extension env x in
      todo env ()

and map_type_ (env : env) (x : CST.type_) : type_ =
  match x with
  | `Tuple_type_ x -> map_tuple_type_ env x
  | `Func_type (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Typed_label x -> map_typed_label env x
        | `Type_ext x -> map_type_ext env x
      in
      let v2 = token env v2 (* "->" *) in
      let v3 = map_type_ext env v3 in
      todo env (v1, v2, v3)
  | `Alia_type (v1, v2, v3) ->
      let v1 = map_type_ext env v1 in
      let v2 = token env v2 (* "as" *) in
      let v3 = map_type_variable env v3 in
      todo env (v1, v2, v3)

and map_type_binding (env : env) ((v1, v2, v3) : CST.type_binding) =
  let v1 =
    match v1 with Some x -> map_type_params env x | None -> todo env ()
  in
  let v2 =
    match v2 with
    | `Id_opt_type_equa_opt_EQ_opt_priv_choice_vari_decl_rep_type_cons
        (v1, v2, v3, v4) ->
        let v1 = token env v1 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
        let v2 =
          match v2 with
          | Some x -> map_type_equation env x
          | None -> todo env ()
        in
        let v3 =
          match v3 with
          | Some (v1, v2, v3) ->
              let v1 = token env v1 (* "=" *) in
              let v2 =
                match v2 with
                | Some tok -> token env tok (* "private" *)
                | None -> todo env ()
              in
              let v3 =
                match v3 with
                | `Vari_decl x -> map_variant_declaration env x
                | `Record_decl x -> map_record_declaration env x
                | `DOTDOT tok -> token env tok
                (* ".." *)
              in
              todo env (v1, v2, v3)
          | None -> todo env ()
        in
        let v4 = List.map (map_type_constraint env) v4 in
        todo env (v1, v2, v3, v4)
    | `Type_cons_path_PLUSEQ_opt_priv_vari_decl (v1, v2, v3, v4) ->
        let v1 = map_type_constructor_path env v1 in
        let v2 = token env v2 (* "+=" *) in
        let v3 =
          match v3 with
          | Some tok -> token env tok (* "private" *)
          | None -> todo env ()
        in
        let v4 = map_variant_declaration env v4 in
        todo env (v1, v2, v3, v4)
  in
  let v3 = List.map (map_item_attribute env) v3 in
  todo env (v1, v2, v3)

and map_type_constraint (env : env) ((v1, v2, v3, v4) : CST.type_constraint) =
  let v1 = token env v1 (* "constraint" *) in
  let v2 = map_type_ext env v2 in
  let v3 = token env v3 (* "=" *) in
  let v4 = map_type_ext env v4 in
  todo env (v1, v2, v3, v4)

and map_type_definition (env : env) ((v1, v2, v3, v4, v5) : CST.type_definition)
    =
  let v1 = token env v1 (* "type" *) in
  let v2 =
    match v2 with Some x -> map_attribute env x | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some tok -> token env tok (* "nonrec" *)
    | None -> todo env ()
  in
  let v4 = map_type_binding env v4 in
  let v5 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "and" *) in
        let v2 = map_type_binding env v2 in
        todo env (v1, v2))
      v5
  in
  todo env (v1, v2, v3, v4, v5)

and map_type_equation (env : env) ((v1, v2, v3) : CST.type_equation) =
  let v1 = map_anon_choice_EQ_4ccabd6 env v1 in
  let v2 =
    match v2 with
    | Some tok -> token env tok (* "private" *)
    | None -> todo env ()
  in
  let v3 = map_type_ext env v3 in
  todo env (v1, v2, v3)

and map_type_ext (env : env) (x : CST.type_ext) =
  match x with
  | `Type x ->
      let _ = map_type_ env x in
      todo env ()
  | `Exte x ->
      let _ = map_extension env x in
      todo env ()

and map_type_parameter_constraint (env : env)
    ((v1, v2, v3, v4, v5) : CST.type_parameter_constraint) =
  let v1 = token env v1 (* "constraint" *) in
  let v2 = map_type_ext env v2 in
  let v3 = token env v3 (* "=" *) in
  let v4 = map_type_ext env v4 in
  let v5 = List.map (map_item_attribute env) v5 in
  todo env (v1, v2, v3, v4, v5)

and map_typed (env : env) ((v1, v2) : CST.typed) =
  let v1 = token env v1 (* ":" *) in
  let v2 = map_type_ext env v2 in
  todo env (v1, v2)

and map_typed_label (env : env) ((v1, v2, v3, v4) : CST.typed_label) =
  let v1 =
    match v1 with Some tok -> token env tok (* "?" *) | None -> todo env ()
  in
  let v2 = token env v2 (* pattern "[a-z_][a-zA-Z0-9_']*" *) in
  let v3 = token env v3 (* ":" *) in
  let v4 = map_type_ext env v4 in
  todo env (v1, v2, v3, v4)

and map_value_definition (env : env) ((v1, v2, v3) : CST.value_definition) =
  let v1 =
    match v1 with
    | `Let_opt_attr_opt_rec (v1, v2, v3) ->
        let v1 = token env v1 (* "let" *) in
        let v2 =
          match v2 with Some x -> map_attribute env x | None -> todo env ()
        in
        let v3 =
          match v3 with
          | Some tok -> token env tok (* "rec" *)
          | None -> todo env ()
        in
        todo env (v1, v2, v3)
    | `Let_op tok -> token env tok
    (* let_operator *)
  in
  let v2 = map_let_binding env v2 in
  let v3 =
    List.map
      (fun (v1, v2) ->
        let v1 =
          match v1 with
          | `And tok -> token env tok (* "and" *)
          | `And_op tok -> token env tok
          (* and_operator *)
        in
        let v2 = map_let_binding env v2 in
        todo env (v1, v2))
      v3
  in
  todo env (v1, v2, v3)

and map_variant_declaration (env : env) (x : CST.variant_declaration) =
  match x with
  | `BAR_opt_cons_decl_rep_BAR_cons_decl (v1, v2) ->
      let v1 = token env v1 (* "|" *) in
      let v2 =
        match v2 with
        | Some x -> map_anon_cons_decl_rep_BAR_cons_decl_fc0ccc5 env x
        | None -> todo env ()
      in
      todo env (v1, v2)
  | `Cons_decl_rep_BAR_cons_decl x ->
      map_anon_cons_decl_rep_BAR_cons_decl_fc0ccc5 env x

let map_compilation_unit (env : env) ((v1, v2) : CST.compilation_unit) =
  let v1 =
    match v1 with
    | Some tok -> token env tok (* pattern #!.* *)
    | None -> todo env ()
  in
  let v2 =
    match v2 with Some x -> map_structure env x | None -> todo env ()
  in
  todo env (v1, v2)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_ocaml.Parse.file file ())
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      try map_compilation_unit env cst
      with Failure "not implemented" as exn ->
        let s = Printexc.get_backtrace () in
        pr2 "Some constructs are not handled yet";
        pr2 "CST was:";
        CST.dump_tree cst;
        pr2 "Original backtrace:";
        pr2 s;
        raise exn)

(* Yoann Padioleau
 *
 * Copyright (c) 2023 Semgrep Imc.
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
open Fpath_.Operators
module CST = Tree_sitter_proto.CST
module H = Parse_tree_sitter_helpers
module G = AST_generic
module H2 = AST_generic_helpers
module R = Raw_tree

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Protobuf parser using tree-sitter-lang/semgrep-proto and converting
 * directly to AST_generic
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let _token = H.token
let _str = H.str
let _fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-proto/Boilerplate.ml *)
(**
   Boilerplate to be used as a template when mapping the proto CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

let token (env : env) (tok : Tree_sitter_run.Token.t) = R.Token (H.str env tok)

let _map_float_lit (env : env) (tok : CST.float_lit) =
  (* float_lit *) token env tok

let _map_decimal_lit (env : env) (tok : CST.decimal_lit) =
  (* decimal_lit *) token env tok

let _map_octal_lit (env : env) (tok : CST.octal_lit) =
  (* octal_lit *) token env tok

let map_imm_tok_prec_p1_pat_dc28280 (env : env)
    (tok : CST.imm_tok_prec_p1_pat_dc28280) =
  (* pattern "[^'\\\\]+" *) token env tok

let _map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_key_type (env : env) (x : CST.key_type) =
  match x with
  | `Int32 tok -> R.Case ("Int32", (* "int32" *) token env tok)
  | `Int64 tok -> R.Case ("Int64", (* "int64" *) token env tok)
  | `Uint32 tok -> R.Case ("Uint32", (* "uint32" *) token env tok)
  | `Uint64 tok -> R.Case ("Uint64", (* "uint64" *) token env tok)
  | `Sint32 tok -> R.Case ("Sint32", (* "sint32" *) token env tok)
  | `Sint64 tok -> R.Case ("Sint64", (* "sint64" *) token env tok)
  | `Fixed32 tok -> R.Case ("Fixed32", (* "fixed32" *) token env tok)
  | `Fixed64 tok -> R.Case ("Fixed64", (* "fixed64" *) token env tok)
  | `Sfixed32 tok -> R.Case ("Sfixed32", (* "sfixed32" *) token env tok)
  | `Sfixed64 tok -> R.Case ("Sfixed64", (* "sfixed64" *) token env tok)
  | `Bool tok -> R.Case ("Bool", (* "bool" *) token env tok)
  | `Str tok -> R.Case ("Str", (* "string" *) token env tok)

let _map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_imm_tok_prec_p1_pat_3a2a380 (env : env)
    (tok : CST.imm_tok_prec_p1_pat_3a2a380) =
  (* pattern "[^\"\\\\]+" *) token env tok

let map_anon_choice_DASH_81d4819 (env : env) (x : CST.anon_choice_DASH_81d4819)
    =
  match x with
  | `DASH tok -> R.Case ("DASH", (* "-" *) token env tok)
  | `PLUS tok -> R.Case ("PLUS", (* "+" *) token env tok)

let map_syntax (env : env) ((v1, v2, v3, v4) : CST.syntax) =
  let v1 = (* "syntax" *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = (* "\"proto3\"" *) token env v3 in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [ v1; v2; v3; v4 ]

let _map_hex_lit (env : env) (tok : CST.hex_lit) = (* hex_lit *) token env tok

let map_string_ (env : env) (x : CST.string_) =
  match x with
  | `DQUOT_rep_choice_imm_tok_prec_p1_pat_3a2a380_DQUOT (v1, v2, v3) ->
      R.Case
        ( "DQUOT_rep_choice_imm_tok_prec_p1_pat_3a2a380_DQUOT",
          let v1 = (* "\"" *) token env v1 in
          let v2 =
            R.List
              (List_.map
                 (fun x ->
                   match x with
                   | `Imm_tok_prec_p1_pat_3a2a380 x ->
                       R.Case
                         ( "Imm_tok_prec_p1_pat_3a2a380",
                           map_imm_tok_prec_p1_pat_3a2a380 env x )
                   | `Esc_seq tok ->
                       R.Case ("Esc_seq", (* escape_sequence *) token env tok))
                 v2)
          in
          let v3 = (* "\"" *) token env v3 in
          R.Tuple [ v1; v2; v3 ] )
  | `SQUOT_rep_choice_imm_tok_prec_p1_pat_dc28280_SQUOT (v1, v2, v3) ->
      R.Case
        ( "SQUOT_rep_choice_imm_tok_prec_p1_pat_dc28280_SQUOT",
          let v1 = (* "'" *) token env v1 in
          let v2 =
            R.List
              (List_.map
                 (fun x ->
                   match x with
                   | `Imm_tok_prec_p1_pat_dc28280 x ->
                       R.Case
                         ( "Imm_tok_prec_p1_pat_dc28280",
                           map_imm_tok_prec_p1_pat_dc28280 env x )
                   | `Esc_seq tok ->
                       R.Case ("Esc_seq", (* escape_sequence *) token env tok))
                 v2)
          in
          let v3 = (* "'" *) token env v3 in
          R.Tuple [ v1; v2; v3 ] )

let map_bool_ (env : env) (x : CST.bool_) =
  match x with
  | `True tok -> R.Case ("True", (* "true" *) token env tok)
  | `False tok -> R.Case ("False", (* "false" *) token env tok)

let map_int_lit (env : env) (x : CST.int_lit) =
  match x with
  | `Deci_lit tok -> R.Case ("Deci_lit", (* decimal_lit *) token env tok)
  | `Octal_lit tok -> R.Case ("Octal_lit", (* octal_lit *) token env tok)
  | `Hex_lit tok -> R.Case ("Hex_lit", (* hex_lit *) token env tok)

let map_message_or_enum_type (env : env)
    ((v1, v2, v3) : CST.message_or_enum_type) =
  let v1 =
    match v1 with
    | Some tok -> R.Option (Some ((* "." *) token env tok))
    | None -> R.Option None
  in
  let v2 =
    R.List
      (List_.map
         (fun (v1, v2) ->
           let v1 = (* identifier *) token env v1 in
           let v2 = (* "." *) token env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_field_names (env : env) ((v1, v2) : CST.field_names) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    R.List
      (List_.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = (* identifier *) token env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

let map_full_ident (env : env) ((v1, v2) : CST.full_ident) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    match v2 with
    | Some xs ->
        R.Option
          (Some
             (R.List
                (List_.map
                   (fun (v1, v2) ->
                     let v1 = (* "." *) token env v1 in
                     let v2 = (* identifier *) token env v2 in
                     R.Tuple [ v1; v2 ])
                   xs)))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

let map_import (env : env) ((v1, v2, v3, v4) : CST.import) =
  let v1 = (* "import" *) token env v1 in
  let v2 =
    match v2 with
    | Some x ->
        R.Option
          (Some
             (match x with
             | `Weak tok -> R.Case ("Weak", (* "weak" *) token env tok)
             | `Public tok -> R.Case ("Public", (* "public" *) token env tok)))
    | None -> R.Option None
  in
  let v3 = map_string_ env v3 in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [ v1; v2; v3; v4 ]

let map_field_number (env : env) (x : CST.field_number) = map_int_lit env x

let map_type_ (env : env) (x : CST.type_) =
  match x with
  | `Double tok -> R.Case ("Double", (* "double" *) token env tok)
  | `Float tok -> R.Case ("Float", (* "float" *) token env tok)
  | `Int32 tok -> R.Case ("Int32", (* "int32" *) token env tok)
  | `Int64 tok -> R.Case ("Int64", (* "int64" *) token env tok)
  | `Uint32 tok -> R.Case ("Uint32", (* "uint32" *) token env tok)
  | `Uint64 tok -> R.Case ("Uint64", (* "uint64" *) token env tok)
  | `Sint32 tok -> R.Case ("Sint32", (* "sint32" *) token env tok)
  | `Sint64 tok -> R.Case ("Sint64", (* "sint64" *) token env tok)
  | `Fixed32 tok -> R.Case ("Fixed32", (* "fixed32" *) token env tok)
  | `Fixed64 tok -> R.Case ("Fixed64", (* "fixed64" *) token env tok)
  | `Sfixed32 tok -> R.Case ("Sfixed32", (* "sfixed32" *) token env tok)
  | `Sfixed64 tok -> R.Case ("Sfixed64", (* "sfixed64" *) token env tok)
  | `Bool tok -> R.Case ("Bool", (* "bool" *) token env tok)
  | `Str tok -> R.Case ("Str", (* "string" *) token env tok)
  | `Bytes tok -> R.Case ("Bytes", (* "bytes" *) token env tok)
  | `Mess_or_enum_type x ->
      R.Case ("Mess_or_enum_type", map_message_or_enum_type env x)

let map_package (env : env) ((v1, v2, v3) : CST.package) =
  let v1 = (* "package" *) token env v1 in
  let v2 = map_full_ident env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_option_name (env : env) ((v1, v2) : CST.option_name) =
  let v1 =
    match v1 with
    | `Id tok -> R.Case ("Id", (* identifier *) token env tok)
    | `LPAR_full_id_RPAR (v1, v2, v3) ->
        R.Case
          ( "LPAR_full_id_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = map_full_ident env v2 in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [ v1; v2; v3 ] )
  in
  let v2 =
    R.List
      (List_.map
         (fun (v1, v2) ->
           let v1 = (* "." *) token env v1 in
           let v2 = (* identifier *) token env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

let rec map_constant (env : env) (x : CST.constant) =
  match x with
  | `Full_id x -> R.Case ("Full_id", map_full_ident env x)
  | `Opt_choice_DASH_int_lit (v1, v2) ->
      R.Case
        ( "Opt_choice_DASH_int_lit",
          let v1 =
            match v1 with
            | Some x -> R.Option (Some (map_anon_choice_DASH_81d4819 env x))
            | None -> R.Option None
          in
          let v2 = map_field_number env v2 in
          R.Tuple [ v1; v2 ] )
  | `Opt_choice_DASH_float_lit (v1, v2) ->
      R.Case
        ( "Opt_choice_DASH_float_lit",
          let v1 =
            match v1 with
            | Some x -> R.Option (Some (map_anon_choice_DASH_81d4819 env x))
            | None -> R.Option None
          in
          let v2 = (* float_lit *) token env v2 in
          R.Tuple [ v1; v2 ] )
  | `Str x -> R.Case ("Str", map_string_ env x)
  | `Bool x -> R.Case ("Bool", map_bool_ env x)
  | `Blk_lit (v1, v2, v3) ->
      R.Case
        ( "Blk_lit",
          let v1 = (* "{" *) token env v1 in
          let v2 =
            R.List
              (List_.map
                 (fun (v1, v2, v3, v4) ->
                   let v1 = (* identifier *) token env v1 in
                   let v2 =
                     match v2 with
                     | Some tok -> R.Option (Some ((* ":" *) token env tok))
                     | None -> R.Option None
                   in
                   let v3 =
                     match v3 with
                     | `Cst x -> R.Case ("Cst", map_constant env x)
                     | `LBRACK_cst_rep_COMMA_cst_RBRACK (v1, v2, v3, v4) ->
                         R.Case
                           ( "LBRACK_cst_rep_COMMA_cst_RBRACK",
                             let v1 = (* "[" *) token env v1 in
                             let v2 = map_constant env v2 in
                             let v3 =
                               R.List
                                 (List_.map
                                    (fun (v1, v2) ->
                                      let v1 = (* "," *) token env v1 in
                                      let v2 = map_constant env v2 in
                                      R.Tuple [ v1; v2 ])
                                    v3)
                             in
                             let v4 = (* "]" *) token env v4 in
                             R.Tuple [ v1; v2; v3; v4 ] )
                   in
                   let v4 =
                     match v4 with
                     | Some x ->
                         R.Option
                           (Some
                              (match x with
                              | `COMMA tok ->
                                  R.Case ("COMMA", (* "," *) token env tok)
                              | `SEMI tok ->
                                  R.Case ("SEMI", (* ";" *) token env tok)))
                     | None -> R.Option None
                   in
                   R.Tuple [ v1; v2; v3; v4 ])
                 v2)
          in
          let v3 = (* "}" *) token env v3 in
          R.Tuple [ v1; v2; v3 ] )

let map_range (env : env) ((v1, v2) : CST.range) =
  let v1 = map_field_number env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        R.Option
          (Some
             (let v1 = (* "to" *) token env v1 in
              let v2 =
                match v2 with
                | `Int_lit x -> R.Case ("Int_lit", map_field_number env x)
                | `Max tok -> R.Case ("Max", (* "max" *) token env tok)
              in
              R.Tuple [ v1; v2 ]))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

let map_field_option (env : env) ((v1, v2, v3) : CST.field_option) =
  let v1 = map_option_name env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_constant env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_option_ (env : env) ((v1, v2, v3, v4, v5) : CST.option_) =
  let v1 = (* "option" *) token env v1 in
  let v2 = map_option_name env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_constant env v4 in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_enum_value_option (env : env) ((v1, v2, v3) : CST.enum_value_option) =
  let v1 = map_option_name env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_constant env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_ranges (env : env) ((v1, v2) : CST.ranges) =
  let v1 = map_range env v1 in
  let v2 =
    R.List
      (List_.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_range env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

let map_field_options (env : env) ((v1, v2) : CST.field_options) =
  let v1 = map_field_option env v1 in
  let v2 =
    R.List
      (List_.map
         (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_field_option env v2 in
           R.Tuple [ v1; v2 ])
         v2)
  in
  R.Tuple [ v1; v2 ]

let map_rpc (env : env)
    ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) : CST.rpc) =
  let v1 = (* "rpc" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 = (* "(" *) token env v3 in
  let v4 =
    match v4 with
    | Some tok -> R.Option (Some ((* "stream" *) token env tok))
    | None -> R.Option None
  in
  let v5 = map_message_or_enum_type env v5 in
  let v6 = (* ")" *) token env v6 in
  let v7 = (* "returns" *) token env v7 in
  let v8 = (* "(" *) token env v8 in
  let v9 =
    match v9 with
    | Some tok -> R.Option (Some ((* "stream" *) token env tok))
    | None -> R.Option None
  in
  let v10 = map_message_or_enum_type env v10 in
  let v11 = (* ")" *) token env v11 in
  let v12 =
    match v12 with
    | `LCURL_rep_choice_opt_RCURL (v1, v2, v3) ->
        R.Case
          ( "LCURL_rep_choice_opt_RCURL",
            let v1 = (* "{" *) token env v1 in
            let v2 =
              R.List
                (List_.map
                   (fun x ->
                     match x with
                     | `Opt x -> R.Case ("Opt", map_option_ env x)
                     | `Empty_stmt tok ->
                         R.Case ("Empty_stmt", (* ";" *) token env tok))
                   v2)
            in
            let v3 = (* "}" *) token env v3 in
            R.Tuple [ v1; v2; v3 ] )
    | `SEMI tok -> R.Case ("SEMI", (* ";" *) token env tok)
  in
  R.Tuple [ v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11; v12 ]

let map_enum_field (env : env) ((v1, v2, v3, v4, v5, v6) : CST.enum_field) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 =
    match v3 with
    | Some tok -> R.Option (Some ((* "-" *) token env tok))
    | None -> R.Option None
  in
  let v4 = map_field_number env v4 in
  let v5 =
    match v5 with
    | Some (v1, v2, v3, v4) ->
        R.Option
          (Some
             (let v1 = (* "[" *) token env v1 in
              let v2 = map_enum_value_option env v2 in
              let v3 =
                R.List
                  (List_.map
                     (fun (v1, v2) ->
                       let v1 = (* "," *) token env v1 in
                       let v2 = map_enum_value_option env v2 in
                       R.Tuple [ v1; v2 ])
                     v3)
              in
              let v4 = (* "]" *) token env v4 in
              R.Tuple [ v1; v2; v3; v4 ]))
    | None -> R.Option None
  in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [ v1; v2; v3; v4; v5; v6 ]

let map_reserved (env : env) ((v1, v2, v3) : CST.reserved) =
  let v1 = (* "reserved" *) token env v1 in
  let v2 =
    match v2 with
    | `Ranges x -> R.Case ("Ranges", map_ranges env x)
    | `Field_names x -> R.Case ("Field_names", map_field_names env x)
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_field (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.field) =
  let v1 =
    match v1 with
    | Some tok -> R.Option (Some ((* "optional" *) token env tok))
    | None -> R.Option None
  in
  let v2 =
    match v2 with
    | Some tok -> R.Option (Some ((* "repeated" *) token env tok))
    | None -> R.Option None
  in
  let v3 = map_type_ env v3 in
  let v4 = (* identifier *) token env v4 in
  let v5 = (* "=" *) token env v5 in
  let v6 = map_field_number env v6 in
  let v7 =
    match v7 with
    | Some (v1, v2, v3) ->
        R.Option
          (Some
             (let v1 = (* "[" *) token env v1 in
              let v2 = map_field_options env v2 in
              let v3 = (* "]" *) token env v3 in
              R.Tuple [ v1; v2; v3 ]))
    | None -> R.Option None
  in
  let v8 = (* ";" *) token env v8 in
  R.Tuple [ v1; v2; v3; v4; v5; v6; v7; v8 ]

let map_map_field (env : env)
    ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) : CST.map_field) =
  let v1 = (* "map" *) token env v1 in
  let v2 = (* "<" *) token env v2 in
  let v3 = map_key_type env v3 in
  let v4 = (* "," *) token env v4 in
  let v5 = map_type_ env v5 in
  let v6 = (* ">" *) token env v6 in
  let v7 = (* identifier *) token env v7 in
  let v8 = (* "=" *) token env v8 in
  let v9 = map_field_number env v9 in
  let v10 =
    match v10 with
    | Some (v1, v2, v3) ->
        R.Option
          (Some
             (let v1 = (* "[" *) token env v1 in
              let v2 = map_field_options env v2 in
              let v3 = (* "]" *) token env v3 in
              R.Tuple [ v1; v2; v3 ]))
    | None -> R.Option None
  in
  let v11 = (* ";" *) token env v11 in
  R.Tuple [ v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11 ]

let map_oneof_field (env : env) ((v1, v2, v3, v4, v5) : CST.oneof_field) =
  let v1 = map_type_ env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_field_number env v4 in
  let v5 =
    match v5 with
    | Some (v1, v2, v3) ->
        R.Option
          (Some
             (let v1 = (* "[" *) token env v1 in
              let v2 = map_field_options env v2 in
              let v3 = (* "]" *) token env v3 in
              R.Tuple [ v1; v2; v3 ]))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_service (env : env) ((v1, v2, v3, v4, v5) : CST.service) =
  let v1 = (* "service" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 =
    R.List
      (List_.map
         (fun x ->
           match x with
           | `Opt x -> R.Case ("Opt", map_option_ env x)
           | `Rpc x -> R.Case ("Rpc", map_rpc env x)
           | `Empty_stmt tok -> R.Case ("Empty_stmt", (* ";" *) token env tok))
         v4)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_enum_body (env : env) ((v1, v2, v3) : CST.enum_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List
      (List_.map
         (fun x ->
           match x with
           | `Opt x -> R.Case ("Opt", map_option_ env x)
           | `Enum_field x -> R.Case ("Enum_field", map_enum_field env x)
           | `Empty_stmt tok -> R.Case ("Empty_stmt", (* ";" *) token env tok))
         v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_oneof (env : env) ((v1, v2, v3, v4, v5) : CST.oneof) =
  let v1 = (* "oneof" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 =
    R.List
      (List_.map
         (fun x ->
           match x with
           | `Opt x -> R.Case ("Opt", map_option_ env x)
           | `Oneof_field x -> R.Case ("Oneof_field", map_oneof_field env x)
           | `Empty_stmt tok -> R.Case ("Empty_stmt", (* ";" *) token env tok))
         v4)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [ v1; v2; v3; v4; v5 ]

let map_enum (env : env) ((v1, v2, v3) : CST.enum) =
  let v1 = (* "enum" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 = map_enum_body env v3 in
  R.Tuple [ v1; v2; v3 ]

let rec map_message (env : env) ((v1, v2, v3) : CST.message) =
  let v1 = (* "message" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 = map_message_body env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_message_body (env : env) ((v1, v2, v3) : CST.message_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List
      (List_.map
         (fun x ->
           match x with
           | `Field x -> R.Case ("Field", map_field env x)
           | `Enum x -> R.Case ("Enum", map_enum env x)
           | `Mess x -> R.Case ("Mess", map_message env x)
           | `Opt x -> R.Case ("Opt", map_option_ env x)
           | `Oneof x -> R.Case ("Oneof", map_oneof env x)
           | `Map_field x -> R.Case ("Map_field", map_map_field env x)
           | `Rese x -> R.Case ("Rese", map_reserved env x)
           | `Empty_stmt tok -> R.Case ("Empty_stmt", (* ";" *) token env tok))
         v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

let map_source_file (env : env) ((v1, v2) : CST.source_file) : G.raw_tree =
  let v1 = map_syntax env v1 in
  let v2 =
    match v2 with
    | Some xs ->
        R.Option
          (Some
             (R.List
                (List_.map
                   (fun x ->
                     match x with
                     | `Import x -> R.Case ("Import", map_import env x)
                     | `Pack x -> R.Case ("Pack", map_package env x)
                     | `Opt x -> R.Case ("Opt", map_option_ env x)
                     | `Enum x -> R.Case ("Enum", map_enum env x)
                     | `Mess x -> R.Case ("Mess", map_message env x)
                     | `Serv x -> R.Case ("Serv", map_service env x)
                     | `Empty_stmt tok ->
                         R.Case ("Empty_stmt", (* ";" *) token env tok))
                   xs)))
    | None -> R.Option None
  in
  R.Tuple [ v1; v2 ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_proto.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      let raw = map_source_file env cst in
      let st = G.stmt_of_raw raw in
      [ st ])

(* todo: special mode to convert Ellipsis in the right construct! *)
let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_proto.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      let xs = map_source_file env cst in
      G.Raw xs)

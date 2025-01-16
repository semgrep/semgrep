(* Brandon Wu
 *
 * Copyright (c) 2022 Semgrep Inc.
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
module CST = Tree_sitter_dart.CST
module H = Parse_tree_sitter_helpers
module H2 = AST_generic_helpers
module G = AST_generic
module R = Raw_tree
open AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Dart parser using tree-sitter-lang/semgrep-dart and converting
 * directly to AST_generic.ml
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type context = Program | Pattern
type env = context H.env

let todo (_env : env) _ = failwith "not implemented"
let str = H.str
let token = H.token
let fb = Tok.unsafe_fake_bracket

let _map_trailing_comma env x =
  match x with
  | Some tok -> Some ((* "," *) token env tok)
  | None -> None

let forward_reference_map_template_substitution =
  ref (fun _env _x -> failwith "Foward reference not set.")

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

(* This was started by copying tree-sitter-lang/semgrep-dart/Boilerplate.ml *)

let _map_break_builtin (env : env) (tok : CST.break_builtin) =
  (* break_builtin *) token env tok

let map_anon_choice_async_725f72f (env : env)
    (x : CST.anon_choice_async_725f72f) =
  match x with
  | `Async tok -> KeywordAttr (Async, (* "async" *) token env tok)
  | `Asyn tok -> unhandled_keywordattr ((* "async*" *) str env tok)
  | `Sync tok -> unhandled_keywordattr ((* "sync*" *) str env tok)

let map_bitwise_operator (env : env) (x : CST.bitwise_operator) =
  match x with
  | `AMP tok -> (BitAnd, (* "&" *) token env tok)
  | `HAT tok -> (BitXor, (* "^" *) token env tok)
  | `BAR tok -> (BitOr, (* "|" *) token env tok)

let _map_pat_05bf793 (env : env) (tok : CST.pat_05bf793) =
  (* pattern [^*]*\*+([^/*][^*]*\*+)* *) token env tok

let map_tok_is (env : env) (tok : CST.tok_is) = (* tok_is *) token env tok

let _map_as_operator (env : env) (tok : CST.as_operator) =
  (* as_operator *) token env tok

let _map_template_chars_double_single (env : env)
    (tok : CST.template_chars_double_single) =
  (* template_chars_double_single *) token env tok

let _map_documentation_block_comment (env : env)
    (tok : CST.documentation_block_comment) =
  (* documentation_block_comment *) token env tok

let _map_pat_d6c261f (env : env) (tok : CST.pat_d6c261f) =
  (* pattern ([^/\n].*\
     )? *)
  token env tok

let _map_template_chars_double (env : env) (tok : CST.template_chars_double) =
  (* template_chars_double *) token env tok

let _map_block_comment (env : env) (tok : CST.block_comment) =
  (* block_comment *) token env tok

let _map_pat_4fd4a56 (env : env) (tok : CST.pat_4fd4a56) =
  (* pattern .* *) token env tok

let _map_void_type (env : env) (tok : CST.void_type) =
  (* void_type *) token env tok

let _map_assert_builtin (env : env) (tok : CST.assert_builtin) =
  (* assert_builtin *) token env tok

let map_shift_operator_ (env : env) (x : CST.shift_operator_) =
  match x with
  | `LTLT tok -> (LSL, (* "<<" *) token env tok)
  | `GTGT tok -> (ASR, (* ">>" *) token env tok)
  | `GTGTGT tok -> (LSR, (* ">>>" *) token env tok)

let _map_const_builtin (env : env) (tok : CST.const_builtin) =
  (* const_builtin *) token env tok

let _map_final_builtin (env : env) (tok : CST.final_builtin) =
  (* final_builtin *) token env tok

let map_multiplicative_operator_ (env : env) (x : CST.multiplicative_operator_)
    =
  match x with
  | `STAR tok -> (Mult, (* "*" *) token env tok)
  | `SLASH tok -> (Div, (* "/" *) token env tok)
  | `PERC tok -> (Mod, (* "%" *) token env tok)
  | `TILDESLASH tok -> (FloorDiv, (* "~/" *) token env tok)

let _map_template_chars_raw_slash (env : env)
    (tok : CST.template_chars_raw_slash) =
  (* template_chars_raw_slash *) token env tok

let map_semicolon (env : env) (v1 : CST.semicolon) = (* ";" *) token env v1

let map_relational_operator (env : env) (x : CST.relational_operator) =
  match x with
  | `LT tok -> (Lt, (* "<" *) token env tok)
  | `GT tok -> (Gt, (* ">" *) token env tok)
  | `LTEQ tok -> (LtE, (* "<=" *) token env tok)
  | `GTEQ tok -> (GtE, (* ">=" *) token env tok)

let _map_decimal_floating_point_literal (env : env)
    (tok : CST.decimal_floating_point_literal) =
  (* decimal_floating_point_literal *) token env tok

(* This one we should probably de-inline *)
let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z_$][\w$]* *) str env tok

let _map_unused_escape_sequence (env : env) (tok : CST.unused_escape_sequence) =
  (* unused_escape_sequence *) token env tok

let _map_identifier_dollar_escaped (env : env)
    (tok : CST.identifier_dollar_escaped) =
  (* pattern ([a-zA-Z_]|(\\\$))([\w]|(\\\$))* *) token env tok

let _map_equality_operator (env : env) (tok : CST.equality_operator) =
  (* equality_operator *) token env tok

let _map_hex_integer_literal (env : env) (tok : CST.hex_integer_literal) =
  (* hex_integer_literal *) token env tok

let map_pat_0017fb0 (env : env) (tok : CST.pat_0017fb0) =
  (* pattern .+ *) str env tok

let _map_template_chars_single (env : env) (tok : CST.template_chars_single) =
  (* template_chars_single *) token env tok

let _map_template_chars_single_single (env : env)
    (tok : CST.template_chars_single_single) =
  (* template_chars_single_single *) token env tok

let map_assignment_operator (env : env) (x : CST.assignment_operator) =
  match x with
  | `EQ tok -> (Eq, (* "=" *) token env tok)
  | `PLUSEQ tok -> (Plus, (* "+=" *) token env tok)
  | `DASHEQ tok -> (Minus, (* "-=" *) token env tok)
  | `STAREQ tok -> (Mult, (* "*=" *) token env tok)
  | `SLASHEQ tok -> (Div, (* "/=" *) token env tok)
  | `PERCEQ tok -> (Mod, (* "%=" *) token env tok)
  | `TILDESLASHEQ tok -> (FloorDiv, (* "~/=" *) token env tok)
  | `LTLTEQ tok -> (LSL, (* "<<=" *) token env tok)
  | `GTGTEQ tok -> (ASR, (* ">>=" *) token env tok)
  | `GTGTGTEQ tok -> (LSR, (* ">>>=" *) token env tok)
  | `AMPEQ tok -> (BitAnd, (* "&=" *) token env tok)
  | `HATEQ tok -> (BitXor, (* "^=" *) token env tok)
  | `BAREQ tok -> (BitOr, (* "|=" *) token env tok)
  | `QMARKQMARKEQ tok -> todo env ((), (* "??=" *) token env tok)

let _map_decimal_integer_literal (env : env) (tok : CST.decimal_integer_literal)
    =
  (* decimal_integer_literal *) token env tok

let _map_additive_operator_ (env : env) (tok : CST.additive_operator_) =
  (* additive_operator_ *) token env tok

let _map_case_builtin (env : env) (tok : CST.case_builtin) =
  (* case_builtin *) token env tok

let map_bitwise_operator_ (env : env) (x : CST.bitwise_operator_) =
  map_bitwise_operator env x

let map_shift_operator (env : env) (x : CST.shift_operator) =
  map_shift_operator_ env x

let map_final_or_const (env : env) (x : CST.final_or_const) =
  match x with
  | `Final_buil tok -> KeywordAttr (Final, (* final_builtin *) token env tok)
  | `Const_buil tok -> KeywordAttr (Const, (* const_builtin *) token env tok)

let map_multiplicative_operator (env : env) (x : CST.multiplicative_operator) =
  map_multiplicative_operator_ env x

let map_identifier_list_ (env : env) ((v1, v2) : CST.identifier_list_) :
    ident list =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) str env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
        v2)
      v2
  in
  v1 :: v2

let rec map_ambiguous_name (env : env) (x : CST.ambiguous_name) : G.dotted_ident
    =
  match x with
  | `Id tok ->
      (* pattern [a-zA-Z_$][\w$]* *)
      [ map_identifier env tok ]
  | `Scoped_id (v1, v2, v3) ->
      let v1 = map_ambiguous_name env v1 in
      let _v2 = (* "." *) token env v2 in
      let v3 = (* pattern [a-zA-Z_$][\w$]* *) map_identifier env v3 in
      (* TODO: optimize *)
      v1 @ [ v3 ]

let map_catch_clause (env : env) ((v1, v2, v3, v4, v5) : CST.catch_clause) :
    tok * (ident, G.catch_exn) Either.t =
  let catch_tok = (* "catch" *) token env v1 in
  let _v2 = (* "(" *) token env v2 in
  let v3 = (* pattern [a-zA-Z_$][\w$]* *) str env v3 in
  let _v5 = (* ")" *) token env v5 in
  match v4 with
  | Some (v1, v2) ->
      let _v1 = (* "," *) token env v1 in
      let v2 = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
      ( catch_tok,
        Right (OtherCatch (("DoubleCatch", catch_tok), [ G.I v3; G.I v2 ])) )
  | None -> (catch_tok, Either.Left v3)

let map_identifier_list (env : env) ((v1, v2) : CST.identifier_list) =
  map_identifier_list_ env (v1, v2)

let map_dot_identifier (env : env) ((v1, v2) : CST.dot_identifier) =
  let _v1 = (* "." *) token env v1 in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) token env v2 in
  v2

let map_label (env : env) ((v1, v2) : CST.label) =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) str env v1 in
  let _v2 = (* ":" *) token env v2 in
  v1

let map_type_dot_identifier (env : env) ((v1, v2) : CST.type_dot_identifier) =
  let _v1 = (* "." *) token env v1 in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
  v2

let map_type_dot_identifier_with_dot (env : env)
    ((v1, v2) : CST.type_dot_identifier) =
  let v1 = (* "." *) token env v1 in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
  (v1, v2)

let map_sub_string_test (env : env) ((v1, v2) : CST.sub_string_test) =
  let s1, t1 = (* "$" *) str env v1 in
  let s2, t2 = str env v2 in
  (* Same as Julia. We don't want to parse "$X" as an interpolation when
     it's in a pattern.
     TODO: add test for this
  *)
  match env.extra with
  | Program -> Either_.Middle3 (N (H2.name_of_id (s2, t2)) |> G.e)
  | Pattern -> Either_.Left3 (s1 ^ s2, Tok.combine_toks t1 [ t2 ])

let map_script_tag (env : env) ((v1, v2, v3) : CST.script_tag) : G.stmt =
  let v1 = (* "#!" *) str env v1 in
  let v2 = map_pat_0017fb0 env v2 in
  let _v3 = (* "\n" *) token env v3 in
  G.exprstmt (G.OtherExpr (v1, [ G.I v2 ]) |> G.e)

let map_external_and_static (env : env) ((v1, v2) : CST.external_and_static) =
  let v1 = [ KeywordAttr (Extern, (* "external" *) token env v1) ] in
  match v2 with
  | Some tok -> v1 @ [ KeywordAttr (Static, token env tok) ]
  | None -> v1

let map_is_operator (env : env) ((v1, v2) : CST.is_operator) =
  let v1 = map_tok_is env v1 in
  let v2 =
    match v2 with
    | Some _tok -> (NotIs, v1)
    | None -> (Is, v1)
  in
  v2

let map_prefix_operator (env : env) (x : CST.prefix_operator) =
  match x with
  | `Minus_op tok -> (Minus, (* "-" *) token env tok)
  | `Nega_op tok -> (Not, (* "!" *) token env tok)
  | `Tilde_op tok -> (BitNot, (* "~" *) token env tok)

(* Precondition: Combinators only matter if the import in question is
   a wildcard import (which it is by default).

   We just convert the wildcard into an `ImportFrom` if there are any
   `show`s, and otherwise keep it a wildcard import, for the reason
   given below about `hide`.
*)
let map_combinator (env : env) (x : CST.combinator) =
  match x with
  | `Show_id_list (v1, v2) ->
      let _v1 = (* "show" *) token env v1 in
      let v2 = map_identifier_list env v2 in
      v2
  | `Hide_id_list (v1, v2) ->
      (* everything EXCEPT for something
         https://dart.dev/language/libraries#importing-only-part-of-a-library
         what utter nonsense
         Since this would be really involved to implement, let's just
         assume that any "hide" implies that we remain a wildcard import.
      *)
      let _v1 = (* "hide" *) token env v1 in
      let _v2 = map_identifier_list env v2 in
      []

let map_qualified (env : env) ((v1, v2) : CST.qualified) : dotted_ident =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) str env v1 in
  let v2 =
    match v2 with
    | Some x -> [ map_type_dot_identifier env x ]
    | None -> []
  in
  v1 :: v2

let map_qualified_of_base base (env : env) ((v1, v2) : CST.qualified) : expr =
  List.fold_left
    (fun acc x ->
      DotAccess (acc, fake ".", FN (Id (x, empty_id_info ()))) |> G.e)
    base
    (map_qualified env (v1, v2))

let map_dotted_identifier_list (env : env)
    ((v1, v2) : CST.dotted_identifier_list) : G.dotted_ident =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) str env v1 in
  let v2 = List_.map (map_type_dot_identifier env) v2 in
  v1 :: v2

let map_type_name_name (env : env) ((v1, v2) : CST.type_name) : name =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) str env v1 in
  let v2 =
    match v2 with
    | Some x -> [ v1; map_type_dot_identifier env x ]
    | None -> [ v1 ]
  in
  H2.name_of_ids v2

let map_type_name (env : env) ((v1, v2) : CST.type_name) : type_ =
  TyN (map_type_name_name env (v1, v2)) |> G.t

let map_binary_operator (env : env) (x : CST.binary_operator) =
  match x with
  | `Mult_op x -> map_multiplicative_operator env x
  | `Addi_op tok -> (
      (* Includes minus, according to the Dart grammar.
         https://github.com/UserNobody14/tree-sitter-dart/blob/7e447dc18a2d293498670fb5ea16138648c883e5/grammar.js#L949
      *)
      match str env tok with
      | "-", t -> (Minus, (* additive_operator_ *) t)
      | _, t -> (Plus, (* additive_operator_ *) t))
  | `Shift_op x -> map_shift_operator env x
  | `Rela_op x -> map_relational_operator env x
  | `EQEQ tok -> (Eq, (* "==" *) token env tok)
  | `Bitw_op_ x -> map_bitwise_operator_ env x

(* See "note: string literals". *)

let map_string_token (env : env) (x : Tree_sitter_run.Token.t) =
  Either_.Left3 (str env x)

let map_string_contents (env : env) x =
  match x with
  | `Temp_chars_single tok
  | `Temp_chars_double tok
  | `Temp_chars_single_single tok
  | `Temp_chars_double_single tok ->
      (* template_chars_double_single *) map_string_token env tok
  | `SQUOT tok -> (* "'" *) map_string_token env tok
  | `DQUOT tok -> (* "'" *) map_string_token env tok
  | `Temp_chars_raw_slash tok ->
      (* template_chars_raw_slash *) map_string_token env tok
  | `Esc_seq tok
  | `Unused_esc_seq tok ->
      (* unused_escape_sequence *) map_string_token env tok
  | `Sub_str_test x -> map_sub_string_test env x
  | `Temp_subs x ->
      (* This reference is set because we can't have nice things, and mutually
         recursive functions cannot be polymorphically generalized before they
         are _all_ defined. This means that, even though we _could_ move this
         function down to where the rest of the string parsing functions are, in
         the hopes that subtyping would make this big one applicable to all the
         smaller polymorphic variant types, in reality this confuses
         type-checking and it picks one arbitrarily.

         So to reduce the amount of boilerplate we have to maintain, let's just
         do this forward reference so we don't have to write out all these
         cases.
      *)
      !forward_reference_map_template_substitution env x
  | `DOLLAR tok -> (* "$" *) map_string_token env tok

let map_string_contents_with_delims (env : env) (l, x, r) =
  let v1 = token env l in
  let v2 = List_.map (map_string_contents env) x in
  let v3 = token env r in
  G.interpolated (v1, v2, v3)

let map_additive_operator (env : env) x =
  match str env x with
  | "-", t -> (Minus, t)
  | _, t -> (Plus, t)

let rec map_additive_expression (env : env) (x : CST.additive_expression) =
  match x with
  | `Real_exp_rep1_addi_op_real_exp (v1, v2) ->
      let v1 = map_real_expression env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = map_additive_operator env v1 in
            let v2 = map_real_expression env v2 in
            (v1, v2))
          v2
      in
      List.fold_left
        (fun acc ((op, t), rhs) ->
          Call (IdSpecial (Op op, t) |> G.e, fb [ Arg acc; Arg rhs ]) |> G.e)
        v1 v2
  | `Super_rep1_addi_op_real_exp (v1, v2) ->
      let v1 = (* "super" *) token env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* additive_operator_ *) map_additive_operator env v1 in
            let v2 = map_real_expression env v2 in
            (v1, v2))
          v2
      in
      List.fold_left
        (fun acc ((op, t), rhs) ->
          Call (IdSpecial (Op op, t) |> G.e, fb [ Arg acc; Arg rhs ]) |> G.e)
        (IdSpecial (Super, v1) |> G.e)
        v2

and map_annotation_ (env : env) (x : CST.annotation_) : G.attribute =
  match x with
  | `Marker_anno (v1, v2) ->
      let v1 = (* "@" *) token env v1 in
      let v2 = map_ambiguous_name env v2 in
      NamedAttr (v1, H2.name_of_ids v2, fb [])
  | `Anno (v1, v2, v3) ->
      let v1 = (* "@" *) token env v1 in
      let v2 = map_ambiguous_name env v2 in
      let v3 = map_arguments env v3 in
      NamedAttr (v1, H2.name_of_ids v2, v3)

and map_anon_arg_rep_COMMA_arg_eb223b2 (env : env)
    ((v1, v2) : CST.anon_arg_rep_COMMA_arg_eb223b2) =
  let v1 = map_expression env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_anon_elem_rep_COMMA_elem_opt_COMMA_4ec364f (env : env)
    ((v1, v2, v3) : CST.anon_elem_rep_COMMA_elem_opt_COMMA_4ec364f) : expr list
    =
  let v1 = map_element env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_element env v2 in
        v2)
      v2
  in
  let _v3 =
    match v3 with
    | Some tok -> ignore (Some ((* "," *) token env tok))
    | None -> ()
  in
  v1 :: v2

and map_argument (env : env) (x : CST.argument) = G.Arg (map_expression env x)

and map_argument_list (env : env) (x : CST.argument_list) : G.argument list =
  match x with
  | `Named_arg_rep_COMMA_named_arg (v1, v2) ->
      let v1 = map_named_argument env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_named_argument env v2 in
            v2)
          v2
      in
      v1 :: v2
  | `Arg_rep_COMMA_arg_rep_COMMA_named_arg_rep_COMMA_named_arg (v1, v2, v3) ->
      let v1 = map_argument env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_argument env v2 in
            v2)
          v2
      in
      let v3 =
        List.concat_map
          (fun (v1, v2, v3) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_named_argument env v2 in
            let v3 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_named_argument env v2 in
                  v2)
                v3
            in
            v2 :: v3)
          v3
      in
      (v1 :: v2) @ v3

and map_argument_part (env : env) ((v1, v2) : CST.argument_part) :
    type_arguments * arguments =
  let v1 =
    match v1 with
    | Some x -> map_type_arguments env x
    | None -> fb []
  in
  let v2 = map_arguments env v2 in
  (v1, v2)

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) : G.arguments =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_argument_list env v1 in
        let _v2 =
          match v2 with
          | Some tok -> ignore ((* "," *) token env tok)
          | None -> ()
        in
        v1
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and map_assertion (env : env) ((v1, v2, v3, v4, v5, v6) : CST.assertion) :
    tok * arguments =
  let v1 = (* assert_builtin *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_argument env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_argument env v2 in
        [ v2 ]
    | None -> []
  in
  let _v5 =
    match v5 with
    | Some tok -> ignore (Some ((* "," *) token env tok))
    | None -> ()
  in
  let v6 = (* ")" *) token env v6 in
  (v1, (v2, v3 :: v4, v6))

and map_assignable_expression (env : env) (x : CST.assignable_expression) : expr
    =
  match x with
  | `Prim_assi_sele_part (v1, v2) ->
      let v1 = map_primary env v1 in
      let v2 = map_assignable_selector_part env v2 in
      v2 v1
  | `Super_unco_assi_sele (v1, v2) ->
      let v1 = (* "super" *) token env v1 in
      let expr = IdSpecial (Super, v1) |> G.e in
      let v2 = map_unconditional_assignable_selector env v2 in
      v2 expr
  | `Cons_invo_assi_sele_part (v1, v2) ->
      let v1 = map_constructor_invocation env v1 in
      let v2 = map_assignable_selector_part env v2 in
      v2 v1
  | `Id tok ->
      G.N (Id ((* pattern [a-zA-Z_$][\w$]* *) str env tok, empty_id_info ()))
      |> G.e

and map_assignable_selector (env : env) (x : CST.assignable_selector) :
    expr -> expr =
 fun expr ->
  match x with
  | `Unco_assi_sele x -> map_unconditional_assignable_selector env x expr
  | `Cond_assi_sele (v1, v2) ->
      let v1 = (* "?." *) token env v1 in
      let v2 = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
      DotAccess
        (special (Op Elvis, v1) [ expr ], v1, FN (Id (v2, empty_id_info ())))
      |> G.e

and map_assignable_selector_part (env : env)
    ((v1, v2) : CST.assignable_selector_part) : expr -> expr =
  let v1 = map_selectors env v1 in
  let v2 = map_assignable_selector env v2 in
  fun expr -> v2 (v1 expr)

and map_assignment_expression (env : env)
    ((v1, v2, v3) : CST.assignment_expression) =
  let v1 = map_assignable_expression env v1 in
  let v2 = map_assignment_operator env v2 in
  let v3 = map_expression env v3 in
  match v2 with
  | Eq, tk -> Assign (v1, tk, v3) |> G.e
  | op -> AssignOp (v1, op, v3) |> G.e

and map_bitwise_expression_base (_env : env) op base after =
  List.fold_left
    (fun acc (tk, exp) -> special (Op op, tk) [ acc; exp ])
    base after

and map_bitwise_and_expression (env : env) (x : CST.bitwise_and_expression) :
    expr =
  match x with
  | `Real_exp_rep1_AMP_real_exp (v1, v2) ->
      let v1 = map_real_expression env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "&" *) token env v1 in
            let v2 = map_real_expression env v2 in
            (v1, v2))
          v2
      in
      map_bitwise_expression_base env BitAnd v1 v2
  | `Super_rep1_AMP_real_exp (v1, v2) ->
      let v1 = (* "super" *) token env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "&" *) token env v1 in
            let v2 = map_real_expression env v2 in
            (v1, v2))
          v2
      in
      map_bitwise_expression_base env BitAnd (IdSpecial (Super, v1) |> G.e) v2

and map_bitwise_or_expression (env : env) (x : CST.bitwise_or_expression) =
  match x with
  | `Real_exp_rep1_BAR_real_exp (v1, v2) ->
      let v1 = map_real_expression env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "|" *) token env v1 in
            let v2 = map_real_expression env v2 in
            (v1, v2))
          v2
      in
      map_bitwise_expression_base env BitOr v1 v2
  | `Super_rep1_BAR_real_exp (v1, v2) ->
      let v1 = (* "super" *) token env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "|" *) token env v1 in
            let v2 = map_real_expression env v2 in
            (v1, v2))
          v2
      in
      map_bitwise_expression_base env BitOr (IdSpecial (Super, v1) |> G.e) v2

and map_bitwise_xor_expression (env : env) (x : CST.bitwise_xor_expression) =
  match x with
  | `Real_exp_rep1_HAT_real_exp (v1, v2) ->
      let v1 = map_real_expression env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "|" *) token env v1 in
            let v2 = map_real_expression env v2 in
            (v1, v2))
          v2
      in
      map_bitwise_expression_base env BitXor v1 v2
  | `Super_rep1_HAT_real_exp (v1, v2) ->
      let v1 = (* "super" *) token env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "|" *) token env v1 in
            let v2 = map_real_expression env v2 in
            (v1, v2))
          v2
      in
      map_bitwise_expression_base env BitXor (IdSpecial (Super, v1) |> G.e) v2

and map_block (env : env) ((v1, v2, v3) : CST.block) : stmt list bracket =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.concat_map (map_statement env) v2 in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_cascade_assignment_section (env : env)
    ((v1, v2) : CST.cascade_assignment_section) =
  let v1 = map_assignment_operator env v1 in
  let v2 = map_expression_without_cascade env v2 in
  todo env (v1, v2)

and map_cascade_section (env : env) ((v1, v2, v3, v4, v5) : CST.cascade_section)
    =
  let v1 =
    match v1 with
    | `DOTDOT tok -> (* ".." *) token env tok
    | `QMARKDOTDOT tok -> (* "?.." *) token env tok
  in
  let v2 = map_cascade_selector env v2 in
  let v3 = List_.map (map_argument_part env) v3 in
  let v4 = List_.map (map_cascade_subsection env) v4 in
  let v5 =
    match v5 with
    | Some x -> Some (map_cascade_assignment_section env x)
    | None -> None
  in
  todo env (v1, v2, v3, v4, v5)

and map_cascade_selector (env : env) (x : CST.cascade_selector) =
  match x with
  | `Opt_null_type_LBRACK_exp_RBRACK (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some tok -> Some ((* "?" *) token env tok)
        | None -> None
      in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_argument env v3 in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Id tok -> todo env ((* pattern [a-zA-Z_$][\w$]* *) token env tok)

and map_cascade_subsection (env : env) ((v1, v2) : CST.cascade_subsection) =
  let v1 = map_assignable_selector env v1 in
  let v2 = List_.map (map_argument_part env) v2 in
  todo env (v1, v2)

and map_constructor_invocation (env : env)
    ((v1, v2, v3, v4, v5) : CST.constructor_invocation) : expr =
  let v1 = map_type_name_name env v1 in
  let _, v2, _ = map_type_arguments env v2 in
  let _v3 = (* "." *) token env v3 in
  let v4 = (* pattern [a-zA-Z_$][\w$]* *) str env v4 in
  let l, v5, r = map_arguments env v5 in
  let args_as_exprs =
    List_.filter_map
      (fun arg ->
        try Some (H2.argument_to_expr arg) with
        | H2.NotAnExpr -> None)
      v5
  in
  let constructor =
    Constructor (Id (v4, empty_id_info ()), (l, args_as_exprs, r)) |> G.e
  in
  OtherExpr
    ( ("ConstructorInvocation", fake "ConstructorInvocation"),
      [ G.Name v1; G.Anys (List_.map (fun x -> Ta x) v2); G.E constructor ] )
  |> G.e

and map_constructor_param (env : env)
    ((v1, v2, v3, v4, v5) : CST.constructor_param) : parameter =
  let pattrs, ptype =
    match v1 with
    | Some x -> map_final_const_var_or_type env x
    | None -> ([], None)
  in
  (* Choosing to ignore the `this` for the more useful name of the variable. *)
  let _v2 = (* "this" *) token env v2 in
  let _v3 = (* "." *) token env v3 in
  let v4 = (* pattern [a-zA-Z_$][\w$]* *) str env v4 in
  let _typarams_TODO, _params_TODO =
    match v5 with
    | Some x -> map_formal_parameter_part env x
    | None -> (None, fb [])
  in
  Param (param_of_id ~pattrs ?ptype v4)

and map_declared_identifier (env : env)
    ((v1, v2, v3, v4) : CST.declared_identifier) :
    attribute list * type_ option * ident =
  let v1 =
    match v1 with
    | Some x -> map_metadata env x
    | None -> []
  in
  let v2 =
    match v2 with
    | Some tok -> [ unhandled_keywordattr ((* "covariant" *) str env tok) ]
    | None -> []
  in
  let attrs, tyopt = map_final_const_var_or_type env v3 in
  let v4 = (* pattern [a-zA-Z_$][\w$]* *) str env v4 in
  (v1 @ v2 @ attrs, tyopt, v4)

(* Only allow the default expression if it's a normal Param, and if
    there isn't already one.
*)
and modify_param ?(attrs = []) ?default _env param =
  match param with
  | Param pc ->
      let pdefault =
        match pc.pdefault with
        | None -> default
        | Some expr -> Some expr
      in
      let pattrs = attrs @ pc.pattrs in
      Param { pc with pdefault; pattrs }
  | other -> other

and map_default_formal_parameter (env : env)
    ((v1, v2) : CST.default_formal_parameter) : parameter =
  let param = map_formal_parameter env v1 in
  let default =
    match v2 with
    | Some (v1, v2) ->
        let _v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        Some v2
    | None -> None
  in
  modify_param ?default env param

and map_default_named_parameter (env : env) (x : CST.default_named_parameter) =
  match x with
  | `Opt_requ_formal_param_opt_EQ_exp (v1, v2, v3) ->
      let attrs =
        match v1 with
        | Some tok -> [ unhandled_keywordattr ((* "required" *) str env tok) ]
        | None -> []
      in
      let param = map_formal_parameter env v2 in
      let default =
        match v3 with
        | Some (v1, v2) ->
            let _v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            Some v2
        | None -> None
      in
      modify_param ~attrs ?default env param
  | `Opt_requ_formal_param_opt_COLON_exp (v1, v2, v3) ->
      let attrs =
        match v1 with
        | Some tok -> [ unhandled_keywordattr ((* "required" *) str env tok) ]
        | None -> []
      in
      let param = map_formal_parameter env v2 in
      let default =
        match v3 with
        | Some (v1, v2) ->
            (* This is still just default values.
               https://dart.dev/language/functions#named-parameters
            *)
            let _v1 = (* ":" *) token env v1 in
            let v2 = map_expression env v2 in
            Some v2
        | None -> None
      in
      modify_param ~attrs ?default env param

and map_element (env : env) (x : CST.element) : expr =
  match x with
  | `Exp x -> map_expression env x
  | `Pair (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let _v2 = (* ":" *) token env v2 in
      let v3 = map_expression env v3 in
      Container (Tuple, fb [ v1; v3 ]) |> G.e
  | `Spread_elem (v1, v2, v3) ->
      let v1 = (* "..." *) token env v1 in
      (* I don't know why the question mark should be there. *)
      let _v2_TODO =
        match v2 with
        | Some tok -> Some ((* "?" *) token env tok)
        | None -> None
      in
      let v3 = map_expression env v3 in
      special (Spread, v1) [ v3 ]
  | `If_elem (v1, v2, v3, v4) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_element env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            Some
              (let _v1 = (* "else" *) token env v1 in
               let v2 = map_element env v2 in
               ExprStmt (v2, G.sc) |> G.s)
        | None -> None
      in
      StmtExpr (If (v1, Cond v2, ExprStmt (v3, G.sc) |> G.s, v4) |> G.s) |> G.e
  | `For_elem (v1, v2, v3, v4, v5, v6) ->
      let _v1_TODO =
        match v1 with
        | Some tok -> [ unhandled_keywordattr ((* "await" *) str env tok) ]
        | None -> []
      in
      let v2 = (* "for" *) token env v2 in
      let _v3 = (* "(" *) token env v3 in
      let v4 = map_for_loop_parts env v4 in
      let _v5 = (* ")" *) token env v5 in
      let v6 = map_element env v6 in
      StmtExpr (For (v2, v4, ExprStmt (v6, G.sc) |> G.s) |> G.s) |> G.e

and map_equality_expression (env : env) (x : CST.equality_expression) : expr =
  match x with
  | `Real_exp_equa_op_real_exp (v1, v2, v3) -> (
      let v1 = map_real_expression env v1 in
      let v2 = (* equality_operator *) str env v2 in
      let v3 = map_real_expression env v3 in
      (* An equality operator can be both == or !=.
         https://github.com/UserNobody14/tree-sitter-dart/blob/7e447dc18a2d293498670fb5ea16138648c883e5/grammar.js#L849
      *)
      match v2 with
      | "!=", tk -> special (Op NotEq, tk) [ v1; v3 ]
      | _, tk -> special (Op Eq, tk) [ v1; v3 ])
  | `Super_equa_op_real_exp (v1, v2, v3) -> (
      let v1 = (* "super" *) token env v1 in
      let v1 = IdSpecial (Super, v1) |> G.e in
      let v2 = (* equality_operator *) str env v2 in
      let v3 = map_real_expression env v3 in
      match v2 with
      | "!=", tk -> special (Op NotEq, tk) [ v1; v3 ]
      | _, tk -> special (Op Eq, tk) [ v1; v3 ])

and map_expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Choice_assign_exp x -> (
      match x with
      | `Assign_exp x -> map_assignment_expression env x
      | `Throw_exp x -> map_throw_expression env x
      | `Real_exp_rep_casc_sect (v1, v2) ->
          let v1 = map_real_expression env v1 in
          let _v2 = R.List (List_.map (map_cascade_section env) v2) in
          v1)
  | `Semg_ellips tok -> Ellipsis ((* "..." *) token env tok) |> G.e
  | `Semg_named_ellips tok ->
      N
        (Id
           ((* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) str env tok, empty_id_info ()))
      |> G.e
  | `Deep_ellips (v1, v2, v3) ->
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      DeepEllipsis (v1, v2, v3) |> G.e

and map_expression_statement (env : env) (x : CST.expression_statement) : stmt =
  match x with
  | `Exp_semi (v1, v2) ->
      let e = map_expression env v1 in
      let sc = map_semicolon env v2 in
      ExprStmt (e, sc) |> G.s
  | `Semg_ellips tok ->
      ExprStmt (Ellipsis (* "..." *) (token env tok) |> G.e, G.sc) |> G.s

and map_expression_without_cascade (env : env)
    (x : CST.expression_without_cascade) : expr =
  match x with
  | `Assign_exp_with_casc (v1, v2, v3) -> (
      let v1 = map_assignable_expression env v1 in
      let v2 = map_assignment_operator env v2 in
      let v3 = map_expression_without_cascade env v3 in
      match v2 with
      | Eq, tk -> Assign (v1, tk, v3) |> G.e
      | op -> AssignOp (v1, op, v3) |> G.e)
  | `Real_exp x -> map_real_expression env x
  | `Throw_exp_with_casc (v1, v2) ->
      let v1 = (* "throw" *) token env v1 in
      let v2 = map_expression_without_cascade env v2 in
      StmtExpr (Throw (v1, v2, G.sc) |> G.s) |> G.e

and map_final_const_var_or_type (env : env) (x : CST.final_const_var_or_type) :
    attribute list * type_ option =
  match x with
  | `Opt_late_buil_final_buil_opt_type (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some tok -> [ unhandled_keywordattr ((* "late" *) str env tok) ]
        | None -> []
      in
      let v2 = KeywordAttr (Final, (* final_builtin *) token env v2) in
      let v3 =
        match v3 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      (v1 @ [ v2 ], v3)
  | `Const_buil_opt_type (v1, v2) ->
      let v1 = KeywordAttr (Const, (* const_builtin *) token env v1) in
      let v2 =
        match v2 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      ([ v1 ], v2)
  | `Opt_late_buil_var_or_type (v1, v2) ->
      let v1 =
        match v1 with
        | Some tok -> [ unhandled_keywordattr ((* "late" *) str env tok) ]
        | None -> []
      in
      let v2 = map_var_or_type env v2 in
      (v1, Some v2)

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) : finally =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_block env v2 in
  (v1, Block v2 |> G.s)

and map_for_loop_parts (env : env) (x : CST.for_loop_parts) : for_header =
  match x with
  | `Choice_decl_id_in_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Decl_id x -> (
            let attrs, tyopt, id = map_declared_identifier env x in
            let tyid = PatId (id, G.empty_id_info ()) in
            match tyopt with
            | None -> tyid
            | Some ty -> PatTyped (tyid, { ty with t_attrs = attrs }))
        | `Id tok ->
            let id = (* pattern [a-zA-Z_$][\w$]* *) str env tok in
            PatId (id, G.empty_id_info ())
      in
      let v2 = (* "in" *) token env v2 in
      let v3 = map_expression env v3 in
      ForEach (v1, v2, v3)
  | `Opt_choice_local_var_decl_opt_exp_semi_opt_exp_rep_COMMA_exp
      (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some x -> (
            match x with
            | `Local_var_decl x ->
                List_.map
                  (fun (x, y) -> ForInitVar (x, y))
                  (map_local_variable_declaration_unwrapped env x)
            | `Opt_exp_rep_COMMA_exp_semi (v1, v2) ->
                let v1 =
                  match v1 with
                  | Some x ->
                      List_.map
                        (fun x -> ForInitExpr x)
                        (map_anon_arg_rep_COMMA_arg_eb223b2 env x)
                  | None -> []
                in
                let _sc = map_semicolon env v2 in
                v1)
        | None -> []
      in
      let v2 =
        match v2 with
        | Some x -> Some [ map_expression env x ]
        | None -> None
      in
      let _sc = map_semicolon env v3 in
      let v4 =
        match v4 with
        | Some x -> Some (map_anon_arg_rep_COMMA_arg_eb223b2 env x)
        | None -> None
      in
      let listify es =
        match es with
        | None -> None
        | Some es -> Some (Container (List, fb es) |> G.e)
      in
      (* The Generic AST cannot accommodate lists of expressions for conditionals and nexts in
         a for loop, so let's just put them in a list.
      *)
      G.ForClassic (v1, listify v2, listify v4)

and map_formal_parameter (env : env) (x : CST.formal_parameter) : parameter =
  match x with
  | `Semg_ellips tok -> ParamEllipsis ((* "..." *) token env tok)
  | `Normal_formal_param (v1, v2) -> (
      let v1 =
        match v1 with
        | Some x -> map_metadata env x
        | None -> []
      in
      let v2 =
        match v2 with
        | `Func_formal_param x -> map_function_formal_parameter env x
        | `Simple_formal_param x -> map_simple_formal_parameter env x
        | `Cons_param x -> map_constructor_param env x
        | `Super_formal_param x -> map_super_formal_parameter env x
      in
      match v2 with
      | Param pc -> Param { pc with pattrs = v1 @ pc.pattrs }
      | other -> other)

and map_formal_parameter_list (env : env) (x : CST.formal_parameter_list) :
    parameters =
  map_strict_formal_parameter_list env x

and map_formal_parameter_part (env : env) ((v1, v2) : CST.formal_parameter_part)
    : type_parameters option * parameters =
  let v1 =
    match v1 with
    | Some x -> Some (map_type_parameters env x)
    | None -> None
  in
  let v2 = map_formal_parameter_list env v2 in
  (v1, v2)

and map_function_body (env : env) (x : CST.function_body) :
    attribute list * function_body =
  match x with
  | `Opt_async_EQGT_exp_semi (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some tok -> [ KeywordAttr (Async, (* "async" *) token env tok) ]
        | None -> []
      in
      let _v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      let _sc = map_semicolon env v4 in
      (v1, FBExpr v3)
  | `Opt_choice_async_blk (v1, v2) ->
      let v1 =
        match v1 with
        | Some x -> [ map_anon_choice_async_725f72f env x ]
        | None -> []
      in
      let v2 = map_block env v2 in
      (v1, FBStmt (Block v2 |> G.s))

and map_function_expression_body (env : env) (x : CST.function_expression_body)
    : function_body =
  match x with
  | `Opt_async_EQGT_exp (v1, v2, v3) ->
      let _v1_TODO =
        match v1 with
        | Some tok -> Some ((* "async" *) token env tok)
        | None -> None
      in
      let _v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      FBExpr v3
  | `Opt_choice_async_blk (v1, v2) ->
      let _v1_TODO =
        match v1 with
        | Some x -> Some (map_anon_choice_async_725f72f env x)
        | None -> None
      in
      let v2 = map_block env v2 in
      FBStmt (Block v2 |> G.s)

and map_function_formal_parameter (env : env)
    ((v1, v2, v3, v4, v5) : CST.function_formal_parameter) : parameter =
  let pattrs =
    match v1 with
    | Some tok -> [ unhandled_keywordattr ((* "covariant" *) str env tok) ]
    | None -> []
  in
  let ptype =
    match v2 with
    | Some x -> Some (map_type_ env x)
    | None -> None
  in
  let v3 = (* pattern [a-zA-Z_$][\w$]* *) str env v3 in
  (* TODO: What does it even mean for a parameter to a function to...
     have its own parameters??
  *)
  let _tparams, _params = map_formal_parameter_part env v4 in
  let param = G.param_of_id ~pattrs ?ptype v3 in
  let v5 =
    match v5 with
    | Some tok ->
        OtherParam (("OptParam", token env tok), [ G.Pa (Param param) ])
    | None -> Param param
  in
  v5

and map_function_signature ~attrs (env : env)
    ((v1, v2, v3, v4) : CST.function_signature) =
  let frettype =
    match v1 with
    | Some x -> Some (map_type_ env x)
    | None -> None
  in
  let id =
    match v2 with
    | `Get tok -> (* "get" *) str env tok
    | `Set tok -> (* "set" *) str env tok
    | `Id tok -> (* pattern [a-zA-Z_$][\w$]* *) str env tok
  in
  let tparams, fparams = map_formal_parameter_part env v3 in
  (* what is this? *)
  let _v4_TODO =
    match v4 with
    | Some x -> Some (map_native env x)
    | None -> None
  in
  (* This is actually ambiguous in some cases, because a forward declaration
     might look like
     foo();
     because in Dart, you don't need to specify the return type.

     In this case, when in a pattern, we might actually prefer to say that
     this should be a call to a function named `foo`.
  *)
  fun (fkind, fbody) ->
    match (env.extra, fparams) with
    | Pattern, (_, [], _) ->
        ExprStmt
          (Call (N (Id (id, empty_id_info ())) |> G.e, fb []) |> G.e, G.sc)
        |> G.s
    | _ ->
        DefStmt
          ( basic_entity ~attrs ?tparams id,
            FuncDef { fkind; fparams; frettype; fbody } )
        |> G.s

and map_function_type (env : env) (x : CST.function_type) : type_ =
  match x with
  | `Func_type_tails x ->
      (* This is something where the final type is just a Function, where the return type
         is not specified, like Function(int) Function(string).
         We will just conjure a dummy type for it for now.
      *)
      let ret_ty =
        OtherType (("DummyReturnType", G.fake "DummyReturnType"), []) |> G.t
      in
      map_function_type_tails ~ret_ty env x
  | `Type_not_func_func_type_tails (v1, v2) ->
      let ret_ty = map_type_not_function env v1 in
      let v2 = map_function_type_tails ~ret_ty env v2 in
      v2

and map_function_type_tail ~ret_ty (env : env)
    ((v1, v2, v3, v4, v5) : CST.function_type_tail) : type_ =
  let _v1 = (* "Function" *) token env v1 in
  let v2 = Option.map (map_type_parameters env) v2 in
  (* TODO: The Generic AST cannot currently accommodate type arguments within
   * a function type *)
  let _v3_TODO =
    match v3 with
    | Some tok ->
        Option.map
          (fun (_, xs, _) ->
            xs
            |> List_.map (fun tp ->
                   OtherTypeParam
                     (("OptionalTP", (* "?" *) token env tok), [ G.Tp tp ])))
          v2
    | None -> None
  in
  let v4 =
    match v4 with
    | Some x -> map_parameter_type_list env x
    | None -> []
  in
  let ty = TyFun (v4, ret_ty) |> G.t in
  (* I (brandon) have no idea if this refers to putting the option on the function inputs
     or something else.
     ChatGPT seems to think that it's making the entire function nullable.
  *)
  match v5 with
  | Some tok -> TyQuestion (ty, token env tok) |> G.t
  | None -> ty

and map_function_type_tails ~ret_ty (env : env) (xs : CST.function_type_tails) :
    type_ =
  (* I assume that something like
     int Function(string) Function (bool)
     just means a curried function of type bool -> string -> int
  *)
  List.fold_left
    (fun ret_ty tail -> map_function_type_tail ~ret_ty env tail)
    ret_ty xs

and map_if_null_expression_ (env : env) (xs : CST.if_null_expression_) exp =
  let l =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "??" *) str env v1 in
        let v2 = map_real_expression env v2 in
        (v1, v2))
      xs
  in
  List.fold_left
    (fun acc (tk, e) -> OtherExpr (tk, [ G.E acc; G.E e ]) |> G.e)
    exp l

and map_initialized_identifier (env : env)
    ((v1, v2) : CST.initialized_identifier) : ident * expr option =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) str env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        Some
          (let _v1 = (* "=" *) token env v1 in
           let v2 = map_expression env v2 in
           v2)
    | None -> None
  in
  (v1, v2)

and map_initialized_variable_definition_unwrapped (env : env)
    ((v1, v2, v3) : CST.initialized_variable_definition) :
    (entity * variable_definition) list =
  let attrs, vtype, id = map_declared_identifier env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let _v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        [ (id, Some v2) ]
    | None -> []
  in
  let inits =
    v2
    @ List_.map
        (fun (v1, v2) ->
          let _v1 = (* "," *) token env v1 in
          let v2 = map_initialized_identifier env v2 in
          v2)
        v3
  in
  List_.map
    (fun (id, vinit) ->
      (basic_entity ~attrs id, { vtype; vinit; vtok = G.no_sc }))
    inits

and map_initialized_variable_definition (env : env)
    ((v1, v2, v3) : CST.initialized_variable_definition) : stmt list =
  List_.map
    (fun (ent, vardef) -> DefStmt (ent, VarDef vardef) |> G.s)
    (map_initialized_variable_definition_unwrapped env (v1, v2, v3))

and map_interface_type_list (env : env) ((v1, v2) : CST.interface_type_list) =
  let v1 = map_type_ env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_type_ env v2 in
        v2)
      v2
  in
  v1 :: v2

(* This is called "lambda expression" but it actually doesn't have to be. *)
and map_lambda_expression ~attrs (env : env) ((v1, v2) : CST.lambda_expression)
    =
  let fattrs, fbody = map_function_body env v2 in
  let v1 =
    map_function_signature ~attrs:(attrs @ fattrs) env v1
      ((Function, fake "function"), fbody)
  in
  v1

and map_literal (env : env) (x : CST.literal) =
  match x with
  | `Deci_int_lit tok ->
      let s, t = (* decimal_integer_literal *) str env tok in
      L (Int (Parsed_int.parse (s, t))) |> G.e
  | `Hex_int_lit tok ->
      let s, t = (* hex_integer_literal *) str env tok in
      L (Int (Parsed_int.parse (s, t))) |> G.e
  | `Deci_floa_point_lit tok ->
      let s, t = (* decimal_floating_point_literal *) str env tok in
      L (Float (float_of_string_opt s, t)) |> G.e
  | `True tok -> L (Bool (true, (* "true" *) token env tok)) |> G.e
  | `False tok -> L (Bool (false, (* "false" *) token env tok)) |> G.e
  | `Str_lit x -> map_uri_expr env x
  | `Null_lit tok -> L (Null (* "null" *) (token env tok)) |> G.e
  | `Symb_lit (v1, v2) ->
      (* For referring to identifiers.
         https://dart.dev/language/built-in-types#symbols
      *)
      let v1 = (* "#" *) token env v1 in
      let v2 = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
      OtherExpr (("Symbol", v1), [ G.I v2 ]) |> G.e
  | `List_lit (v1, v2, v3, v4, v5) ->
      let _v1_TODO =
        match v1 with
        | Some tok -> Some ((* const_builtin *) token env tok)
        | None -> None
      in
      let _v2_TODO =
        match v2 with
        | Some x -> Some (map_type_arguments env x)
        | None -> None
      in
      let v3 = (* "[" *) token env v3 in
      let v4 =
        match v4 with
        | Some x -> map_anon_elem_rep_COMMA_elem_opt_COMMA_4ec364f env x
        | None -> []
      in
      let v5 = (* "]" *) token env v5 in
      Container (List, (v3, v4, v5)) |> G.e
  | `Set_or_map_lit (v1, v2, v3, v4, v5) ->
      let _v1_TODO =
        match v1 with
        | Some tok -> Some ((* const_builtin *) token env tok)
        | None -> None
      in
      let _v2_TODO =
        match v2 with
        | Some x -> Some (map_type_arguments env x)
        | None -> None
      in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        match v4 with
        | Some x -> map_anon_elem_rep_COMMA_elem_opt_COMMA_4ec364f env x
        | None -> []
      in
      let v5 = (* "}" *) token env v5 in
      (* Could be either a set or map, depending on the contents of
         the elements.
         Let's just say it's a map.
      *)
      Container (Dict, (v3, v4, v5)) |> G.e

and map_local_variable_declaration (env : env)
    ((v1, v2) : CST.local_variable_declaration) : G.stmt list =
  let v1 = map_initialized_variable_definition env v1 in
  (* TODO: call H2.add_semicolon_to_last_var_def_and_convert_to_stmts *)
  let _sc = map_semicolon env v2 in
  v1

and map_local_variable_declaration_unwrapped (env : env)
    ((v1, v2) : CST.local_variable_declaration) :
    (G.entity * G.variable_definition) list =
  let v1 = map_initialized_variable_definition_unwrapped env v1 in
  (* TODO: call H2.add_semicolon_to_last_var_def_and_convert_to_stmts *)
  let _v2 = map_semicolon env v2 in
  v1

and map_metadata (env : env) (xs : CST.metadata) : G.attribute list =
  List_.map (map_annotation_ env) xs

and map_multiplicative_expression (env : env)
    (x : CST.multiplicative_expression) =
  match x with
  | `Un_exp_rep1_mult_op_un_exp (v1, v2) ->
      let v1 = map_unary_expression env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = map_multiplicative_operator env v1 in
            let v2 = map_unary_expression env v2 in
            (v1, v2))
          v2
      in
      List.fold_left
        (fun acc ((op, t), rhs) ->
          Call (IdSpecial (Op op, t) |> G.e, fb [ Arg acc; Arg rhs ]) |> G.e)
        v1 v2
  | `Super_rep1_mult_op_un_exp (v1, v2) ->
      let v1 = (* "super" *) token env v1 in
      let v1 = IdSpecial (Super, v1) |> G.e in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = map_multiplicative_operator env v1 in
            let v2 = map_unary_expression env v2 in
            (v1, v2))
          v2
      in
      List.fold_left
        (fun acc ((op, t), rhs) ->
          Call (IdSpecial (Op op, t) |> G.e, fb [ Arg acc; Arg rhs ]) |> G.e)
        v1 v2

and map_named_argument (env : env) ((v1, v2) : CST.named_argument) =
  let v1 = map_label env v1 in
  let v2 = map_expression env v2 in
  ArgKwd (v1, v2)

and map_named_parameter_type (env : env)
    ((v1, v2, v3) : CST.named_parameter_type) : parameter =
  let pattrs =
    match v1 with
    | Some x -> map_metadata env x
    | None -> []
  in
  let pattrs =
    match v2 with
    | Some tok -> unhandled_keywordattr ((* "required" *) str env tok) :: pattrs
    | None -> pattrs
  in
  let ty, id = map_typed_identifier env v3 in
  Param
    {
      pname = Some id;
      ptype = Some ty;
      pdefault = None;
      pattrs;
      pinfo = G.empty_id_info ();
    }

and map_native (env : env) ((v1, v2) : CST.native) =
  let v1 = (* "native" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> Some (map_uri env x)
    | None -> None
  in
  todo env (v1, v2)

and map_normal_formal_parameters (env : env)
    ((v1, v2) : CST.normal_formal_parameters) : parameter list =
  let v1 = map_formal_parameter env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_formal_parameter env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_normal_parameter_type (env : env) ((v1, v2) : CST.normal_parameter_type)
    : parameter =
  let pattrs =
    match v1 with
    | Some x -> map_metadata env x
    | None -> []
  in
  let ptype, pname =
    match v2 with
    | `Typed_id x ->
        let ty, id = map_typed_identifier env x in
        (Some ty, Some id)
    | `Type x -> (Some (map_type_ env x), None)
  in
  Param { ptype; pname; pattrs; pdefault = None; pinfo = G.empty_id_info () }

and map_on_part (env : env) (x : CST.on_part) : catch =
  match x with
  | `Catch_clause_blk (v1, v2) ->
      let tok, v1 = map_catch_clause env v1 in
      let v2 = map_block env v2 in
      let catch_exn =
        match v1 with
        | Either.Left id -> CatchPattern (PatId (id, G.empty_id_info ()))
        | Either.Right catch_exn -> catch_exn
      in
      (tok, catch_exn, Block v2 |> G.s)
  | `On_type_not_void_opt_catch_clause_blk (v1, v2, v3, v4) ->
      let v1 = (* "on" *) token env v1 in
      let v2 = map_type_not_void env v2 in
      let v3 =
        match v3 with
        | Some x -> (
            match map_catch_clause env x with
            | _, Either.Left id -> CatchParam (G.param_of_id ~ptype:v2 id)
            (* If we are already set on an exn (the DoubleCatch case)
               then we cannot incorporate the type.
            *)
            | _, Either.Right catch_exn -> catch_exn)
        | None ->
            CatchParam
              {
                ptype = Some v2;
                pname = None;
                pdefault = None;
                pattrs = [];
                pinfo = empty_id_info ();
              }
      in
      let v4 = map_block env v4 in
      (v1, v3, Block v4 |> G.s)

and map_optional_formal_parameters (env : env)
    (x : CST.optional_formal_parameters) : parameter list =
  match x with
  | `Opt_post_formal_params (v1, v2, v3, v4, v5) ->
      let _v1 = (* "[" *) token env v1 in
      let v2 = map_default_formal_parameter env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_default_formal_parameter env v2 in
            v2)
          v3
      in
      let _v4 =
        match v4 with
        | Some tok -> ignore (Some ((* "," *) token env tok))
        | None -> ()
      in
      let _v5 = (* "]" *) token env v5 in
      [
        OtherParam
          ( ("OptionalParams", fake "OptionalParams"),
            List_.map (fun param -> G.Pa param) (v2 :: v3) );
      ]
  | `Named_formal_params (v1, v2, v3, v4, v5) ->
      let _v1 = (* "{" *) token env v1 in
      let v2 = map_default_named_parameter env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_default_named_parameter env v2 in
            v2)
          v3
      in
      let _v4 =
        match v4 with
        | Some tok -> ignore (Some ((* "," *) token env tok))
        | None -> ()
      in
      let _v5 = (* "}" *) token env v5 in
      v2 :: v3

and map_optional_parameter_types (env : env) (x : CST.optional_parameter_types)
    : parameter list =
  match x with
  | `Opt_posi_param_types (v1, v2, v3, v4, v5) ->
      let _v1 = (* "[" *) token env v1 in
      let v2 = map_normal_parameter_type env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_normal_parameter_type env v2 in
            v2)
          v3
      in
      let _v4 =
        match v4 with
        | Some tok -> ignore ((* "," *) token env tok)
        | None -> ()
      in
      let _v5 = (* "]" *) token env v5 in
      v2 :: v3
  | `Named_param_types (v1, v2, v3, v4, v5) ->
      let _v1 = (* "{" *) token env v1 in
      let v2 = map_named_parameter_type env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_named_parameter_type env v2 in
            v2)
          v3
      in
      let _v4 =
        match v4 with
        | Some tok -> ignore ((* "," *) token env tok)
        | None -> ()
      in
      let _v5 = (* "}" *) token env v5 in
      v2 :: v3

and map_parameter_type_list (env : env) ((v1, v2, v3) : CST.parameter_type_list)
    : parameter list =
  let _v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `Normal_param_type_rep_COMMA_normal_param_type_opt_COMMA (v1, v2, v3)
          ->
            let v1 = map_normal_parameter_type env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_normal_parameter_type env v2 in
                  v2)
                v2
            in
            let _v3 =
              match v3 with
              | Some tok -> ignore ((* "," *) token env tok)
              | None -> ()
            in
            v1 :: v2
        | `Normal_param_type_rep_COMMA_normal_param_type_COMMA_opt_param_types
            (v1, v2, v3, v4) ->
            let v1 = map_normal_parameter_type env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_normal_parameter_type env v2 in
                  v2)
                v2
            in
            let _v3 = (* "," *) token env v3 in
            let v4 = map_optional_parameter_types env v4 in
            (* I (brandon) wanted to reflect the separate structure of the optional params
               from the normal parameters.
               This is because we may have a function type like
               int Function(int, string y, [int z, int w])
               where the optional parameters are clearly separated.
            *)
            let opt_args = List_.map (fun param -> G.Pa param) v4 in
            (v1 :: v2)
            @ [ G.OtherParam (("OptParams", G.fake "OptParams"), opt_args) ]
        | `Opt_param_types x ->
            let opt_args =
              map_optional_parameter_types env x
              |> List_.map (fun param -> G.Pa param)
            in
            [ G.OtherParam (("OptParams", G.fake "OptParams"), opt_args) ])
    | None -> []
  in
  let _v3 = (* ")" *) token env v3 in
  v2

and map_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  H2.set_e_range v1 v3 v2;
  v2

and map_postfix_expression (env : env) (x : CST.postfix_expression) =
  match x with
  | `Prim_rep_sele (v1, v2) ->
      let v1 = map_primary env v1 in
      let v2 = map_selectors env v2 in
      v2 v1
  | `Post_exp_ x -> map_postfix_expression_ env x

and map_postfix_expression_ (env : env) (x : CST.postfix_expression_) =
  match x with
  | `Assi_exp_post_op (v1, v2) ->
      let v1 = map_assignable_expression env v1 in
      let s, t = (* increment_operator *) str env v2 in
      let incr_decr =
        match s with
        | "--" -> Decr
        | _ -> Incr
      in
      special (IncrDecr (incr_decr, Postfix), t) [ v1 ]
  | `Cons_invo_rep_sele (v1, v2) ->
      let v1 = map_constructor_invocation env v1 in
      let v2 = map_selectors env v2 in
      v2 v1

and map_primary (env : env) (x : CST.primary) : expr =
  match x with
  | `Lit x -> map_literal env x
  | `Func_exp (v1, v2) ->
      let _tparams, fparams = map_formal_parameter_part env v1 in
      let fbody = map_function_expression_body env v2 in
      Lambda
        { fkind = (LambdaKind, fake "lambda"); fparams; frettype = None; fbody }
      |> G.e
  | `Id tok ->
      N (Id ((* pattern [a-zA-Z_$][\w$]* *) str env tok, empty_id_info ()))
      |> G.e
  | `New_exp (v1, v2, v3, v4) ->
      let v1 = (* "new" *) token env v1 in
      let v2 = map_type_not_void env v2 in
      (* Dunno what this means. *)
      let _v3_TODO =
        match v3 with
        | Some x -> Some (map_dot_identifier env x)
        | None -> None
      in
      let v4 = map_arguments env v4 in
      New (v1, v2, empty_id_info (), v4) |> G.e
  | `Const_obj_exp (v1, v2, v3, v4) ->
      let v1 = (* const_builtin *) token env v1 in
      let v2 = map_type_not_void env v2 in
      let _v3_TODO =
        match v3 with
        | Some x -> Some (map_dot_identifier env x)
        | None -> None
      in
      let v4 = map_arguments env v4 in
      (* Seems to be similar to the above case, but with the
         added stipulation it must be `const`.
      *)
      New (v1, v2, empty_id_info (), v4) |> G.e
  | `LPAR_exp_RPAR x -> map_parenthesized_expression env x
  | `This tok -> IdSpecial (This, (* "this" *) token env tok) |> G.e
  | `Super_unco_assi_sele (v1, v2) ->
      let v1 = (* "super" *) token env v1 in
      let v1 = IdSpecial (Super, v1) |> G.e in
      let v2 = map_unconditional_assignable_selector env v2 in
      v2 v1

and map_real_expression (env : env) (x : CST.real_expression) : expr =
  match x with
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_real_expression env v1 in
      let _v2 = (* "?" *) token env v2 in
      let v3 = map_expression_without_cascade env v3 in
      let _v4 = (* ":" *) token env v4 in
      let v5 = map_expression_without_cascade env v5 in
      Conditional (v1, v3, v5) |> G.e
  | `Logi_or_exp (v1, v2) ->
      let v1 = map_real_expression env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "||" *) token env v1 in
            let v2 = map_real_expression env v2 in
            (v1, v2))
          v2
      in
      List.fold_left (fun acc (tk, e) -> special (Op Or, tk) [ acc; e ]) v1 v2
  | `If_null_exp (v1, v2) ->
      let v1 = map_real_expression env v1 in
      let v2 = map_if_null_expression_ env v2 in
      v2 v1
  | `Addi_exp x -> map_additive_expression env x
  | `Mult_exp x -> map_multiplicative_expression env x
  | `Rela_exp x -> map_relational_expression env x
  | `Equa_exp x -> map_equality_expression env x
  | `Logi_and_exp (v1, v2) ->
      let v1 = map_real_expression env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "&&" *) token env v1 in
            let v2 = map_real_expression env v2 in
            (v1, v2))
          v2
      in
      List.fold_left (fun acc (tk, e) -> special (Op And, tk) [ acc; e ]) v1 v2
  | `Bitw_and_exp x -> map_bitwise_and_expression env x
  | `Bitw_or_exp x -> map_bitwise_or_expression env x
  | `Bitw_xor_exp x -> map_bitwise_xor_expression env x
  | `Shift_exp x -> map_shift_expression env x
  | `Type_cast_exp (v1, v2) ->
      let v1 = map_real_expression env v1 in
      let v2 = map_type_cast env v2 in
      v2 v1
  | `Type_test_exp (v1, v2) ->
      let v1 = map_real_expression env v1 in
      let v2 = map_type_test env v2 in
      v2 v1
  | `Un_exp x -> map_unary_expression env x

and map_relational_expression (env : env) (x : CST.relational_expression) =
  match x with
  | `Real_exp_rela_op_real_exp (v1, v2, v3) ->
      let v1 = map_real_expression env v1 in
      let op, tk = map_relational_operator env v2 in
      let v3 = map_real_expression env v3 in
      special (Op op, tk) [ v1; v3 ]
  | `Super_rela_op_real_exp (v1, v2, v3) ->
      let v1 = (* "super" *) token env v1 in
      let v1 = IdSpecial (Super, v1) |> G.e in
      let op, tk = map_relational_operator env v2 in
      let v3 = map_real_expression env v3 in
      special (Op op, tk) [ v1; v3 ]

and map_selector (env : env) (x : CST.selector) : expr -> expr =
 fun expr ->
  match x with
  (* Seems to be a null-assert pattern.
     https://dart.dev/language/pattern-types#null-assert
  *)
  | `Excl_op tok -> Ref ((* "!" *) token env tok, expr) |> G.e
  | `Assi_sele x -> map_assignable_selector env x expr
  | `Arg_part x ->
      let _tyargs_TODO, args = map_argument_part env x in
      Call (expr, args) |> G.e

and map_selectors (env : env) (x : CST.selector list) : expr -> expr =
  x
  |> List_.map (map_selector env)
  (* turn each selector into a function
      [f1; f2; f3; f4]
  *)
  |> List.rev
  (* reverse to
      [f4; f3; f2; f1]
  *)
  |> List.fold_left Common.compose (fun x -> x)
(* compose from the left
    fun x -> f4 (f3 (f2 (f1 x)))
*)

and map_shift_expression (env : env) (x : CST.shift_expression) =
  match x with
  | `Real_exp_rep1_shift_op_real_exp (v1, v2) ->
      let v1 = map_real_expression env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = map_shift_operator env v1 in
            let v2 = map_real_expression env v2 in
            (v1, v2))
          v2
      in
      List.fold_left
        (fun acc ((op, t), rhs) ->
          Call (IdSpecial (Op op, t) |> G.e, fb [ Arg acc; Arg rhs ]) |> G.e)
        v1 v2
  | `Super_rep1_shift_op_real_exp (v1, v2) ->
      let v1 = (* "super" *) token env v1 in
      let v1 = IdSpecial (Super, v1) |> G.e in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let v1 = map_shift_operator env v1 in
            let v2 = map_real_expression env v2 in
            (v1, v2))
          v2
      in
      List.fold_left
        (fun acc ((op, t), rhs) ->
          Call (IdSpecial (Op op, t) |> G.e, fb [ Arg acc; Arg rhs ]) |> G.e)
        v1 v2

and map_simple_formal_parameter (env : env) (x : CST.simple_formal_parameter) :
    parameter =
  match x with
  | `Decl_id x ->
      let pattrs, ptype, id = map_declared_identifier env x in
      Param (param_of_id ?ptype ~pattrs id)
  | `Opt_cova_id (v1, v2) ->
      let v1 =
        match v1 with
        | Some tok -> [ unhandled_keywordattr ((* "covariant" *) str env tok) ]
        | None -> []
      in
      let v2 = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
      Param (param_of_id ~pattrs:v1 v2)

and map_statement_as_stmt env x =
  match map_statement env x with
  (* No point in re-packing a Block. *)
  | [ ({ s = Block _; _ } as other) ] -> other
  | other -> Block (fb other) |> G.s

and map_statement (env : env) (x : CST.statement) : stmt list =
  match x with
  | `Blk x -> [ Block (map_block env x) |> G.s ]
  | `Local_func_decl (v1, v2) ->
      let attrs =
        match v1 with
        | Some x -> map_metadata env x
        | None -> []
      in
      let v2 = map_lambda_expression ~attrs env v2 in
      [ v2 ]
  | `Local_var_decl x -> map_local_variable_declaration env x
  | `For_stmt (v1, v2, v3, v4, v5, v6) ->
      (* what does it mean to await a for loop? *)
      let _v1_TODO =
        match v1 with
        | Some tok -> [ unhandled_keywordattr ((* "await" *) str env tok) ]
        | None -> []
      in
      let v2 = (* "for" *) token env v2 in
      let _v3 = (* "(" *) token env v3 in
      let v4 = map_for_loop_parts env v4 in
      let _v5 = (* ")" *) token env v5 in
      let v6 = map_statement env v6 in
      [ For (v2, v4, Block (fb v6) |> G.s) |> G.s ]
  | `While_stmt (v1, v2, v3) ->
      let v1 = (* "while" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement_as_stmt env v3 in
      [ While (v1, Cond v2, v3) |> G.s ]
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "do" *) token env v1 in
      let v2 = map_statement_as_stmt env v2 in
      let _v3 = (* "while" *) token env v3 in
      let v4 = map_parenthesized_expression env v4 in
      let _sc = map_semicolon env v5 in
      [ DoWhile (v1, v2, v4) |> G.s ]
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = (* "switch" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_switch_block env v3 in
      [ Switch (v1, Some (Cond v2), v3) |> G.s ]
  | `If_stmt (v1, v2, v3, v4) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement_as_stmt env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            Some
              (let _v1 = (* "else" *) token env v1 in
               let v2 = map_statement_as_stmt env v2 in
               v2)
        | None -> None
      in
      [ If (v1, Cond v2, v3, v4) |> G.s ]
  | `Try_stmt (v1, v2) ->
      let try_tok, stmt = map_try_head env v1 in
      let catches, finally =
        match v2 with
        | `Fina_clause x -> ([], Some (map_finally_clause env x))
        | `Rep1_on_part_opt_fina_clause (v1, v2) ->
            let v1 = List_.map (map_on_part env) v1 in
            let v2 =
              match v2 with
              | Some x -> Some (map_finally_clause env x)
              | None -> None
            in
            (v1, v2)
      in
      [ Try (try_tok, stmt, catches, None, finally) |> G.s ]
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = (* break_builtin *) token env v1 in
      let v2 =
        match v2 with
        | Some tok -> LId ((* pattern [a-zA-Z_$][\w$]* *) str env tok)
        | None -> LNone
      in
      let sc = map_semicolon env v3 in
      [ Break (v1, v2, sc) |> G.s ]
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = (* "continue" *) token env v1 in
      let v2 =
        match v2 with
        | Some tok -> LId ((* pattern [a-zA-Z_$][\w$]* *) str env tok)
        | None -> LNone
      in
      let sc = map_semicolon env v3 in
      [ Continue (v1, v2, sc) |> G.s ]
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = (* "return" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> Some (map_expression env x)
        | None -> None
      in
      let sc = map_semicolon env v3 in
      [ Return (v1, v2, sc) |> G.s ]
  | `Yield_stmt (v1, v2, v3) ->
      let v1 = (* "yield" *) token env v1 in
      let v2 = map_expression env v2 in
      let _sc = map_semicolon env v3 in
      [ G.exprstmt (Yield (v1, Some v2, false) |> G.e) ]
  | `Yield_each_stmt (v1, v2, v3, v4) ->
      (* Dart docs are incredibly unhelpful on what this is.
         https://dart.dev/language/functions#generators
         Let's just treat it the same.
      *)
      let v1 = (* "yield" *) token env v1 in
      let _v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      let _sc = map_semicolon env v4 in
      [ G.exprstmt (Yield (v1, Some v3, false) |> G.e) ]
  | `Exp_stmt x -> [ map_expression_statement env x ]
  | `Assert_stmt (v1, v2) ->
      let tok, args = map_assertion env v1 in
      let sc = (* ";" *) map_semicolon env v2 in
      [ Assert (tok, args, sc) |> G.s ]

and map_strict_formal_parameter_list (env : env)
    (x : CST.strict_formal_parameter_list) : parameters =
  match x with
  | `LPAR_RPAR (v1, v2) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = (* ")" *) token env v2 in
      (v1, [], v2)
  | `LPAR_normal_formal_params_opt_COMMA_RPAR (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_normal_formal_parameters env v2 in
      let _v3 =
        match v3 with
        | Some tok -> ignore ((* "," *) token env tok)
        | None -> ()
      in
      let v4 = (* ")" *) token env v4 in
      (v1, v2, v4)
  | `LPAR_normal_formal_params_COMMA_opt_formal_params_RPAR (v1, v2, v3, v4, v5)
    ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_normal_formal_parameters env v2 in
      let _v3 = (* "," *) token env v3 in
      let v4 = map_optional_formal_parameters env v4 in
      let v5 = (* ")" *) token env v5 in
      (v1, v2 @ v4, v5)
  | `LPAR_opt_formal_params_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_optional_formal_parameters env v2 in
      let v3 = (* ")" *) token env v3 in
      (v1, v2, v3)

(* note: string literals
   There are a bunch of string literals that all contain pretty much the
   same content that can fit into a G.interpolated, but the constructor
   names are not all the same, and the delimiters are different.

   We could just repeat the same boilerplate a bunch of times but this
   is really cumbersome and makes the file really ugly.

   It would be nice to use polymorphism and subtyping to have just one
   single function which can take care of each polymorphic variant, but
   this doesn't play nice with type-checking, as such a function cannot
   be used at a bunch of different types by other functions it is mutually
   recursive with, as it will not have been polymorphically generalized yet.

   So we define that function `map_string_contents` all the way above this
   section. We also needed to use a forward reference to escape the mutual
   recursion.

   This leads to significantly less code, though.
*)

and map_string_literal (env : env) (xs : CST.string_literal) : G.expr =
  G.Call
    ( G.IdSpecial (G.ConcatString G.SequenceConcat, fake "concat") |> G.e,
      fb
        (xs
        |> List_.map (fun x ->
               match x with
               | `Str_lit_double_quotes x ->
                   map_string_literal_double_quotes env x
               | `Str_lit_single_quotes x ->
                   map_string_literal_single_quotes env x
               | `Str_lit_double_quotes_mult x ->
                   map_string_literal_double_quotes_multiple env x
               | `Str_lit_single_quotes_mult x ->
                   map_string_literal_single_quotes_multiple env x
               | `Raw_str_lit_double_quotes x ->
                   map_raw_string_literal_double_quotes env x
               | `Raw_str_lit_single_quotes x ->
                   map_raw_string_literal_single_quotes env x
               | `Raw_str_lit_double_quotes_mult x ->
                   map_raw_string_literal_double_quotes_multiple env x
               | `Raw_str_lit_single_quotes_mult x ->
                   map_raw_string_literal_single_quotes_multiple env x)
        |> List_.map (fun x -> Arg x)) )
  |> G.e

and map_string_literal_to_strings (env : env) (xs : CST.string_literal) :
    G.expr list =
  xs
  |> List_.map (fun x ->
         match x with
         | `Str_lit_double_quotes x -> map_string_literal_double_quotes env x
         | `Str_lit_single_quotes x -> map_string_literal_single_quotes env x
         | `Str_lit_double_quotes_mult x ->
             map_string_literal_double_quotes_multiple env x
         | `Str_lit_single_quotes_mult x ->
             map_string_literal_single_quotes_multiple env x
         | `Raw_str_lit_double_quotes x ->
             map_raw_string_literal_double_quotes env x
         | `Raw_str_lit_single_quotes x ->
             map_raw_string_literal_single_quotes env x
         | `Raw_str_lit_double_quotes_mult x ->
             map_raw_string_literal_double_quotes_multiple env x
         | `Raw_str_lit_single_quotes_mult x ->
             map_raw_string_literal_single_quotes_multiple env x)

(* boilerplate boilerplate na na na na *)

and map_string_literal_double_quotes (env : env)
    ((v1, v2, v3) : CST.string_literal_double_quotes) : G.expr =
  map_string_contents_with_delims env ((* "\"" *) v1, v2, (* "\"" *) v3)

and map_string_literal_double_quotes_multiple (env : env)
    ((v1, v2, v3) : CST.string_literal_double_quotes_multiple) =
  map_string_contents_with_delims env ((* "\"\"\"" *) v1, v2, (* "\"\"\"" *) v3)

and map_string_literal_single_quotes (env : env)
    ((v1, v2, v3) : CST.string_literal_single_quotes) =
  map_string_contents_with_delims env ((* "'" *) v1, v2, (* "'" *) v3)

and map_string_literal_single_quotes_multiple (env : env)
    ((v1, v2, v3) : CST.string_literal_single_quotes_multiple) =
  map_string_contents_with_delims env ((* "'''" *) v1, v2, (* "'''" *) v3)

(* All of the following are "raw strings", which are a little different because
   they interpret backslashes differently. Let's just parse it the same, though.
*)
and map_raw_string_literal_double_quotes (env : env)
    ((v1, v2, v3) : CST.raw_string_literal_double_quotes) =
  map_string_contents_with_delims env ((* "r\"" *) v1, v2, (* "\"" *) v3)

and map_raw_string_literal_single_quotes_multiple (env : env)
    ((v1, v2, v3) : CST.raw_string_literal_single_quotes_multiple) =
  map_string_contents_with_delims env ((* "r'''" *) v1, v2, (* "'''" *) v3)

and map_raw_string_literal_double_quotes_multiple (env : env)
    ((v1, v2, v3) : CST.raw_string_literal_double_quotes_multiple) =
  map_string_contents_with_delims env ((* "r\"\"\"" *) v1, v2, (* "\"\"\"" *) v3)

and map_raw_string_literal_single_quotes (env : env)
    ((v1, v2, v3) : CST.raw_string_literal_single_quotes) =
  map_string_contents_with_delims env ((* "r'" *) v1, v2, (* "'" *) v3)

and map_super_formal_parameter (env : env)
    ((v1, v2, v3, v4, v5) : CST.super_formal_parameter) : parameter =
  let pattrs, ptype =
    match v1 with
    | Some x -> map_final_const_var_or_type env x
    | None -> ([], None)
  in
  (* This has to do with allowing certain parameters to be automatically
     initialized to that of the superclass.
     https://github.com/dart-lang/language/issues/1855
     The important thing is the name of the parameter.
  *)
  let _v2_TODO = (* "super" *) token env v2 in
  let _v3 = (* "." *) token env v3 in
  let v4 = (* pattern [a-zA-Z_$][\w$]* *) str env v4 in
  let _tparams_TODO, _params_TODO =
    match v5 with
    | Some x -> map_formal_parameter_part env x
    | None -> (None, fb [])
  in
  Param (G.param_of_id ~pattrs ?ptype v4)

and map_switch_block (env : env) ((v1, v2, v3) : CST.switch_block) :
    case_and_body list =
  let _v1 = (* "{" *) token env v1 in
  let v2 =
    List_.map
      (fun x ->
        match x with
        | `Switch_label x -> Either.Left (map_switch_label env x)
        | `Stmt x -> Either.Right (map_statement_as_stmt env x))
      v2
  in
  let _v3 = (* "}" *) token env v3 in
  match
    List_.fold_right
      (fun either acc ->
        match (either, acc) with
        | Either.Left _case, (None, acc) ->
            (* this means we saw a case with no stmt below, just skip and move on *)
            (None, acc)
        | Either.Right stmt, (None, acc) -> (Some ([], stmt), acc)
        | Either.Left case, (Some (cases, stmt), acc) ->
            (Some (case :: cases, stmt), acc)
        | Either.Right stmt, (Some (cases, stmt'), acc) ->
            (Some ([], stmt), CasesAndBody (cases, stmt') :: acc))
      v2 (None, [])
  with
  | None, acc -> acc
  | Some (cases, stmt), acc -> CasesAndBody (cases, stmt) :: acc

and map_switch_label (env : env) ((v1, v2) : CST.switch_label) : case =
  (* Not clear to me what these are. *)
  let _v1_TODO = List_.map (map_label env) v1 in
  let v2 =
    match v2 with
    | `Case_buil_exp_COLON (v1, v2, v3) ->
        let v1 = (* case_builtin *) token env v1 in
        let v2 = map_expression env v2 in
        let _v3 = (* ":" *) token env v3 in
        Case (v1, H2.expr_to_pattern v2)
    | `Defa_COLON (v1, v2) ->
        let v1 = (* "default" *) token env v1 in
        let _v2 = (* ":" *) token env v2 in
        Default v1
  in
  v2

and map_template_substitution (env : env) ((v1, v2) : CST.template_substitution)
    =
  let _s1, _t1 = (* "$" *) str env v1 in
  let v2 =
    match v2 with
    | `LCURL_exp_RCURL (v1, v2, v3) ->
        let v1 = (* "{" *) token env v1 in
        let v2 = map_expression env v2 in
        let v3 = (* "}" *) token env v3 in
        Either_.Right3 (v1, Some v2, v3)
    | `Id_dollar_esca tok ->
        let s2, t2 =
          (* pattern ([a-zA-Z_]|(\\\$))([\w]|(\\\$))* *) str env tok
        in
        Left3 (s2, t2)
  in
  v2

and map_throw_expression (env : env) ((v1, v2) : CST.throw_expression) =
  let v1 = (* "throw" *) token env v1 in
  let v2 = map_expression env v2 in
  StmtExpr (Throw (v1, v2, G.sc) |> G.s) |> G.e

and map_try_head (env : env) ((v1, v2) : CST.try_head) =
  let v1 = (* "try" *) token env v1 in
  let v2 = map_block env v2 in
  (v1, Block v2 |> G.s)

and map_type_ (env : env) (x : CST.type_) : type_ =
  match x with
  | `Func_type_opt_null_type (v1, v2) -> (
      let v1 = map_function_type env v1 in
      match v2 with
      | Some tok -> TyQuestion (v1, (* "?" *) token env tok) |> G.t
      | None -> v1)
  | `Type_not_func x -> map_type_not_function env x

and map_type_arguments (env : env) (x : CST.type_arguments) : type_arguments =
  match x with
  | `LT_opt_type_rep_COMMA_type_GT (v1, v2, v3) ->
      let v1 = (* "<" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> List_.map (fun ty -> TA ty) (map_interface_type_list env x)
        | None -> []
      in
      let v3 = (* ">" *) token env v3 in
      (v1, v2, v3)

and map_type_bound (env : env) ((v1, v2) : CST.type_bound) : type_ =
  let _v1 = (* "extends" *) token env v1 in
  let v2 = map_type_not_void env v2 in
  v2

and map_type_cast (env : env) ((v1, v2) : CST.type_cast) exp : expr =
  let v1 = (* as_operator *) token env v1 in
  let v2 = map_type_not_void env v2 in
  Cast (v2, v1, exp) |> G.e

and map_type_not_function (env : env) (x : CST.type_not_function) : type_ =
  match x with
  | `Type_not_void_not_func x -> map_type_not_void_not_function env x
  | `Void_type tok ->
      TyN ((* void_type *) Id (str env tok, G.empty_id_info ())) |> G.t

and map_type_not_void (env : env) (x : CST.type_not_void) : type_ =
  match x with
  | `Func_type_opt_null_type (v1, v2) ->
      let v1 = map_function_type env v1 in
      let v2 =
        match v2 with
        | Some tok -> TyQuestion (v1, (* "?" *) token env tok) |> G.t
        | None -> v1
      in
      v2
  | `Type_not_void_not_func x -> map_type_not_void_not_function env x

and map_type_not_void_not_function (env : env)
    (x : CST.type_not_void_not_function) : type_ =
  match x with
  | `Type_name_opt_type_args_opt_null_type (v1, v2, v3) ->
      let v1 = map_type_name env v1 in
      let v2 =
        match v2 with
        | Some x ->
            let tyargs = map_type_arguments env x in
            TyApply (v1, tyargs) |> G.t
        | None -> v1
      in
      let v3 =
        match v3 with
        | Some tok -> TyQuestion (v2, (* "?" *) token env tok) |> G.t
        | None -> v2
      in
      v3
  | `Func_buil_id_opt_null_type (v1, v2) -> (
      (* This refers to any function type. *)
      let v1 = (* "Function" *) str env v1 in
      let ty = TyN (Id (v1, G.empty_id_info ())) |> G.t in
      match v2 with
      | Some tok -> TyQuestion (ty, (* "?" *) token env tok) |> G.t
      | None -> ty)

and map_type_parameter (env : env) ((v1, v2, v3, v4) : CST.type_parameter) :
    type_parameter =
  let tp_id = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
  let tp_attrs =
    let v1 =
      match v1 with
      | Some x -> map_metadata env x
      | None -> []
    in
    match v3 with
    | Some tok -> KeywordAttr (Optional, (* "?" *) token env tok) :: v1
    | None -> v1
  in
  let tp_bounds =
    match v4 with
    | Some x -> [ map_type_bound env x ]
    | None -> []
  in
  TP { tp_id; tp_attrs; tp_bounds; tp_default = None; tp_variance = None }

and map_type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) :
    type_parameters =
  let lt = (* "<" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_type_parameter env v2 in
        v2)
      v3
  in
  let gt = (* ">" *) token env v4 in
  (lt, v2 :: v3, gt)

and map_type_test (env : env) ((v1, v2) : CST.type_test) exp : expr =
  let op, tk = map_is_operator env v1 in
  let v2 = map_type_not_void env v2 in
  Call (IdSpecial (Op op, tk) |> G.e, fb [ G.Arg exp; G.ArgType v2 ]) |> G.e

and map_typed_identifier (env : env) ((v1, v2) : CST.typed_identifier) :
    type_ * ident =
  let v1 = map_type_ env v1 in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
  (v1, v2)

and map_unary_expression (env : env) (x : CST.unary_expression) =
  match x with
  | `Post_exp x -> map_postfix_expression env x
  | `Un_exp_ x -> map_unary_expression_ env x

and map_unary_expression_ (env : env) (x : CST.unary_expression_) : expr =
  match x with
  | `Prefix_op_un_exp (v1, v2) ->
      let op, tk = map_prefix_operator env v1 in
      let v2 = map_unary_expression env v2 in
      special (Op op, tk) [ v2 ]
  | `Await_exp (v1, v2) ->
      let v1 = (* "await" *) token env v1 in
      let v2 = map_unary_expression env v2 in
      Await (v1, v2) |> G.e
  | `Choice_minus_op_super (v1, v2) ->
      let op, tk =
        match v1 with
        | `Minus_op tok -> (Minus, (* "-" *) token env tok)
        | `Tilde_op tok -> (BitNot, (* "~" *) token env tok)
      in
      let v2 = (* "super" *) token env v2 in
      special (Op op, tk) [ IdSpecial (Super, v2) |> G.e ]
  | `Incr_op_assi_exp (v1, v2) ->
      let s, t = (* increment_operator *) str env v1 in
      let incr_decr =
        match s with
        | "--" -> Decr
        | _ -> Incr
      in
      let v2 = map_assignable_expression env v2 in
      special (IncrDecr (incr_decr, Prefix), t) [ v2 ]

and map_unconditional_assignable_selector (env : env)
    (x : CST.unconditional_assignable_selector) : expr -> expr =
 fun expr ->
  match x with
  | `Opt_null_type_LBRACK_exp_RBRACK (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some tok -> special (Op Elvis, token env tok) [ expr ]
        | None -> expr
      in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_argument env v3 in
      let v4 = (* "]" *) token env v4 in
      Call (v1, (v2, [ v3 ], v4)) |> G.e
  | `DOT_id x ->
      let dot, id = map_type_dot_identifier_with_dot env x in
      DotAccess (expr, dot, FN (Id (id, empty_id_info ()))) |> G.e

and map_uri (env : env) (x : CST.uri) : G.module_name =
  match map_string_literal_to_strings env x with
  | [ { e = G.L (String (_, (s, t), _)); _ } ] ->
      (* It's a pain, but we have to parse the string ourselves.
         It can look like
         dart:<path>
         package:<path>
         for instance, dart:foo/bar

         It's a pain to separate these tokens by ourselves, so let's
         just give them all the location of the originating token.
      *)
      Common.(
        if s =~ "^\\(dart\\):\\(.*\\)$" then
          let prefix, s = Common.matched2 s in
          let dotted =
            List_.map (fun s -> (s, t)) (prefix :: String.split_on_char '/' s)
          in
          DottedName dotted
        else if s =~ "^\\(package\\):\\(.*\\)$" then
          let prefix, s = Common.matched2 s in
          let dotted =
            List_.map (fun s -> (s, t)) (prefix :: String.split_on_char '/' s)
          in
          DottedName dotted
        else DottedName [ (s, t) ])
  | other ->
      (* We have to make it fit in `module_name` somehow, so just drop anything we
         can't understand.
      *)
      DottedName
        (List_.filter_map
           (function
             | { e = G.L (String (_, (s, t), _)); _ } -> Some (s, t)
             | _ -> None)
           other)

and map_uri_expr (env : env) (x : CST.uri) : expr = map_string_literal env x

and map_var_or_type (env : env) (x : CST.var_or_type) : type_ =
  match x with
  | `Type x -> map_type_ env x
  | `Infe_type_opt_type (v1, v2) ->
      let v1 = TyAny ((* "var" *) token env v1) |> G.t in
      let v2 =
        match v2 with
        (* I don't know why you should be able to have both var and a type *)
        | Some x -> todo env (Some (map_type_ env x))
        | None -> v1
      in
      v2

(* "Part directives" allow specifying other files which will contain code which
   is in the same library.
   https://dart.dev/effective-dart/usage#do-use-strings-in-part-of-directives
   https://medium.com/const-final-and-static-in-dart/libraries-packages-and-import-in-dart-how-they-work-and-should-be-used-part-1-ccde79d5ec02
*)
let map_part_directive (env : env) ((v1, v2, v3, v4) : CST.part_directive) =
  let v1 =
    match v1 with
    | Some x -> map_metadata env x
    | None -> []
  in
  let v2 = (* "part" *) str env v2 in
  let v3 = map_uri env v3 in
  let _sc = map_semicolon env v4 in
  let d = OtherDirective (v2, [ G.Modn v3 ]) |> G.d in
  DirectiveStmt { d with d_attrs = v1 } |> G.s

let map_initialized_identifier_list (env : env)
    ((v1, v2) : CST.initialized_identifier_list) =
  let v1 = map_initialized_identifier env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_initialized_identifier env v2 in
        v2)
      v2
  in
  v1 :: v2

let map_type_not_void_list (env : env) ((v1, v2) : CST.type_not_void_list) :
    type_ list =
  let v1 = map_type_not_void env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_type_not_void env v2 in
        v2)
      v2
  in
  v1 :: v2

let map_enum_constant (env : env) ((v1, v2) : CST.enum_constant) : field =
  let v1 =
    match v1 with
    | Some x -> map_metadata env x
    | None -> []
  in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
  G.F
    (DefStmt
       ( basic_entity ~attrs:v1 v2,
         EnumEntryDef { ee_args = None; ee_body = None } )
    |> G.s)

let map_anon_choice_type_be0da33 (env : env) (x : CST.anon_choice_type_be0da33)
    : type_ =
  match x with
  | `Type x -> map_type_ env x
  | `Infe_type tok -> TyAny ((* "var" *) token env tok) |> G.t

let map_setter_signature ~attrs (env : env)
    ((v1, v2, v3, v4, v5) : CST.setter_signature) =
  let frettype =
    match v1 with
    | Some x -> Some (map_type_ env x)
    | None -> None
  in
  let v2 = (* "get" *) token env v2 in
  let v3 = (* pattern [a-zA-Z_$][\w$]* *) str env v3 in
  let tparams, fparams = map_formal_parameter_part env v4 in
  let _v5_TODO =
    match v5 with
    | Some x -> Some (map_native env x)
    | None -> None
  in
  fun fbody ->
    DefStmt
      ( basic_entity ?tparams ~attrs v3,
        FuncDef { fkind = (Function, v2); fparams; frettype; fbody } )
    |> G.s

let map_operator_signature ?(attrs = []) (env : env)
    ((v1, v2, v3, v4, v5) : CST.operator_signature) : stmt =
  let frettype =
    match v1 with
    | Some x -> Some (map_type_ env x)
    | None -> None
  in
  let _v2 = (* "operator" *) token env v2 in
  let v3 =
    match v3 with
    | `TILDE tok -> str env tok
    | `Bin_op x ->
        let _op, t = map_binary_operator env x in
        (Tok.content_of_tok t, t)
    | `LBRACKRBRACK tok -> (* "[]" *) str env tok
    | `LBRACKRBRACKEQ tok -> (* "[]=" *) str env tok
  in
  let fparams = map_formal_parameter_list env v4 in
  let _v5 =
    match v5 with
    | Some x -> Some (map_native env x)
    | None -> None
  in
  DefStmt
    ( basic_entity ~attrs v3,
      FuncDef
        {
          fkind = (Function, fake "function");
          fparams;
          fbody = FBNothing;
          frettype;
        } )
  |> G.s

let map_type_alias ~attrs (env : env) (x : CST.type_alias) : stmt =
  match x with
  | `Type_type_name_opt_type_params_EQ_func_type_SEMI (v1, v2, v3, v4, v5, v6)
    ->
      let _v1 = (* "typedef" *) token env v1 in
      let v2 = map_type_name_name env v2 in
      let tparams = Option.map (map_type_parameters env) v3 in
      let _v4 = (* "=" *) token env v4 in
      let v5 = map_function_type env v5 in
      let _sc = (* ";" *) map_semicolon env v6 in
      DefStmt
        ({ name = EN v2; attrs; tparams }, TypeDef { tbody = AliasType v5 })
      |> G.s
  | `Type_opt_type_type_name_formal_param_part_SEMI (v1, v2, v3, v4, v5) ->
      (* This seems to be the "original" use for typedefs in Dart, which
         is for function types.
         The optional type below has to do with whether or not the return
         type of the function is specified.
         https://stackoverflow.com/questions/12545762/what-are-function-typedefs-function-type-aliases-in-dart
      *)
      let _v1 = (* "typedef" *) token env v1 in
      let ret_ty =
        match v2 with
        | Some x -> map_type_ env x
        | None ->
            OtherType (("DummyReturnType", G.fake "DummyReturnType"), []) |> G.t
      in
      let v3 = map_type_name_name env v3 in
      let tparams, (_, params, _) = map_formal_parameter_part env v4 in
      let _sc = (* ";" *) map_semicolon env v5 in
      let ty = TyFun (params, ret_ty) |> G.t in
      DefStmt
        ({ name = EN v3; attrs; tparams }, TypeDef { tbody = AliasType ty })
      |> G.s

(* THINK: Seems to be like a Java package...
   https://dart.dev/language/libraries
*)
let map_library_name (env : env) ((v1, v2, v3, v4) : CST.library_name) : stmt =
  let v1 =
    match v1 with
    | Some x -> map_metadata env x
    | None -> []
  in
  let v2 = (* "library" *) token env v2 in
  let v3 = map_dotted_identifier_list env v3 in
  let _sc = map_semicolon env v4 in
  let dk = Package (v2, v3) in
  DirectiveStmt { d = dk; d_attrs = v1 } |> G.s

let map_getter_signature ~attrs (env : env)
    ((v1, v2, v3, v4) : CST.getter_signature) =
  let frettype =
    match v1 with
    | Some x -> Some (map_type_ env x)
    | None -> None
  in
  (* THINK: Should this be separated out more than just a DefStmt? OtherStmt? *)
  let _s, t = (* "get" *) str env v2 in
  let v3 = (* pattern [a-zA-Z_$][\w$]* *) str env v3 in
  let _v4_TODO =
    match v4 with
    | Some x -> Some (map_native env x)
    | None -> None
  in
  fun fbody ->
    DefStmt
      ( basic_entity ~attrs v3,
        FuncDef { fkind = (Function, t); fparams = fb []; frettype; fbody } )
    |> G.s

let map_constant_constructor_signature (env : env)
    ((v1, v2, v3) : CST.constant_constructor_signature) :
    attribute * dotted_ident * parameters =
  let v1 = KeywordAttr (Const, (* const_builtin *) token env v1) in
  let v2 = map_qualified env v2 in
  let params = map_formal_parameter_list env v3 in
  (v1, v2, params)

let map_factory_constructor_signature (env : env)
    ((v1, v2, v3, v4) : CST.factory_constructor_signature) :
    attribute * dotted_ident * parameters =
  let v1 = unhandled_keywordattr ((* "factory" *) str env v1) in
  let v2 = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
  let v3 = List_.map (map_type_dot_identifier env) v3 in
  let v4 = map_formal_parameter_list env v4 in
  (v1, v2 :: v3, v4)

(* "Part of directives" are the opposite of part directives, and
   allow specifying what library a given part is a part of.
*)
let map_part_of_directive (env : env)
    ((v1, v2, v3, v4, v5) : CST.part_of_directive) =
  let v1 =
    match v1 with
    | Some x -> map_metadata env x
    | None -> []
  in
  let _v2 = (* "part" *) token env v2 in
  let v3 = (* "of" *) str env v3 in
  (* https://www.youtube.com/watch?v=SXKlJuO07eM *)
  let v4 =
    match v4 with
    | `Dotted_id_list x -> DottedName (map_dotted_identifier_list env x)
    | `Uri x -> map_uri env x
  in
  let _sc = map_semicolon env v5 in
  let d = OtherDirective (v3, [ G.Modn v4 ]) |> G.d in
  DirectiveStmt { d with d_attrs = v1 } |> G.s

let map_static_final_declaration (env : env)
    ((v1, v2, v3) : CST.static_final_declaration) : ident * expr =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) str env v1 in
  let _v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  (v1, v3)

let map_constructor_signature (env : env)
    ((v1, v2, v3) : CST.constructor_signature) : dotted_ident * parameters =
  let v1 = (* pattern [a-zA-Z_$][\w$]* *) str env v1 in
  let v2 =
    match v2 with
    | Some x -> [ v1; map_type_dot_identifier env x ]
    | None -> [ v1 ]
  in
  let v3 = map_formal_parameter_list env v3 in
  (v2, v3)

let map_uri_test (env : env) ((v1, v2) : CST.uri_test) =
  let v1 = map_dotted_identifier_list env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        Some
          (let v1 = (* "==" *) token env v1 in
           let v2 = map_uri env v2 in
           (v1, v2))
    | None -> None
  in
  todo env (v1, v2)

let map_interfaces (env : env) ((v1, v2) : CST.interfaces) =
  let _v1 = (* "implements" *) token env v1 in
  let v2 = map_type_not_void_list env v2 in
  v2

let map_mixins (env : env) ((v1, v2) : CST.mixins) : type_ list =
  let _v1 = (* "with" *) token env v1 in
  let v2 = map_type_not_void_list env v2 in
  v2

let map_enum_body ~attrs ~enum_tok ~enum_id (env : env)
    ((v1, v2, v3, v4, v5) : CST.enum_body) : stmt =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_enum_constant env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_enum_constant env v2 in
        v2)
      v3
  in
  let _v4 =
    match v4 with
    | Some tok -> ignore ((* "," *) token env tok)
    | None -> ()
  in
  let v5 = (* "}" *) token env v5 in
  DefStmt
    ( basic_entity ~attrs enum_id,
      ClassDef
        {
          ckind = (Class, enum_tok);
          cextends = [];
          cimplements = [];
          cmixins = [];
          cparams = fb [];
          cbody = (v1, v2 :: v3, v5);
        } )
  |> G.s

let map_static_final_declaration_list (env : env)
    ((v1, v2) : CST.static_final_declaration_list) =
  let v1 = map_static_final_declaration env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_static_final_declaration env v2 in
        v2)
      v2
  in
  v1 :: v2

let map_initializer_list_entry (env : env) (x : CST.initializer_list_entry) :
    expr =
  match x with
  | `Super_opt_DOT_qual_args (v1, v2, v3) ->
      let v1 = (* "super" *) token env v1 in
      let super = IdSpecial (Super, v1) |> G.e in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let _v1 = (* "." *) token env v1 in
            let v2 = map_qualified_of_base super env v2 in
            v2
        | None -> super
      in
      let v3 = map_arguments env v3 in
      Call (v2, v3) |> G.e
  | `Field_init (v1, v2, v3, v4, v5) ->
      let offset =
        Id ((* pattern [a-zA-Z_$][\w$]* *) str env v2, empty_id_info ())
      in
      let lhs =
        match v1 with
        | Some (v1, v2) ->
            let v1 = (* "this" *) token env v1 in
            let v2 = (* "." *) token env v2 in
            DotAccess (IdSpecial (This, v1) |> G.e, v2, FN offset) |> G.e
        | None -> G.N offset |> G.e
      in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_real_expression env v4 in
      let _v5_TODO = R.List (List_.map (map_cascade_section env) v5) in
      Assign (lhs, v3, v4) |> G.e
  | `Asse x ->
      let assert_tok, args = map_assertion env x in
      StmtExpr (Assert (assert_tok, args, G.fake "") |> G.s) |> G.e

let map_configuration_uri (env : env)
    ((v1, v2, v3, v4, v5) : CST.configuration_uri) =
  let _v1 = (* "if" *) token env v1 in
  let _v2 = (* "(" *) token env v2 in
  let _v3 = map_uri_test env v3 in
  let _v4 = (* ")" *) token env v4 in
  let _v5 = map_uri env v5 in
  (* TODO: What is a configuration URI??? *)
  ()

let map_mixin_application ~class_tok (env : env)
    ((v1, v2, v3) : CST.mixin_application) =
  (* Semantically equivalent to an empty class using a mixin. See
     https://medium.com/flutter-community/dart-what-are-mixins-3a72344011f3
  *)
  let cextends = [ (map_type_not_void env v1, None) ] in
  let cmixins = map_mixins env v2 in
  let cimplements =
    match v3 with
    | Some x -> map_interfaces env x
    | None -> []
  in
  ClassDef
    {
      ckind = (Class, class_tok);
      cextends;
      cmixins;
      cimplements;
      cparams = fb [];
      cbody = fb [];
    }

let map_superclass (env : env) (x : CST.superclass) :
    (type_ * arguments option) list * type_ list =
  match x with
  | `Extends_type_not_void_opt_mixins (v1, v2, v3) ->
      let _v1 = (* "extends" *) token env v1 in
      let v2 = map_type_not_void env v2 in
      (* A mixin is essentially an extension of a signature.
          https://dart.dev/language/mixins
          No real reason to support it right now.
      *)
      let v3 =
        match v3 with
        | Some x -> map_mixins env x
        | None -> []
      in
      ([ (v2, None) ], v3)
  | `Mixins x -> ([], map_mixins env x)

let map_enum_declaration ~attrs (env : env)
    ((v1, v2, v3) : CST.enum_declaration) =
  let enum_tok = (* "enum" *) token env v1 in
  let enum_id = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
  map_enum_body ~attrs ~enum_tok ~enum_id env v3

let map_initializers (env : env) ((v1, v2, v3) : CST.initializers) : expr list =
  let _v1 = (* ":" *) token env v1 in
  let v2 = map_initializer_list_entry env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_initializer_list_entry env v2 in
        v2)
      v3
  in
  v2 :: v3

let map_configurable_uri (env : env) ((v1, v2) : CST.configurable_uri) :
    G.module_name =
  let v1 = map_uri env v1 in
  let _v2 = List_.map (map_configuration_uri env) v2 in
  (* TODO: this is just totally wrong actually lmao
     the string isnt parsed -- it needs to be something like
     "foo/bar/qux"
     which puts separating that on us...
  *)
  v1

let map_mixin_application_class ~attrs ~class_tok (env : env)
    ((v1, v2, v3, v4, v5) : CST.mixin_application_class) =
  let id = (* pattern [a-zA-Z_$][\w$]* *) str env v1 in
  let tparams = Option.map (map_type_parameters env) v2 in
  let _teq = (* "=" *) token env v3 in
  let cdef = map_mixin_application ~class_tok env v4 in
  (* TODO: add sc to cdef *)
  let _sc = map_semicolon env v5 in
  DefStmt (basic_entity id ~attrs ?tparams, cdef) |> G.s

(* For use to augment the body of a function with "initializers", which are
   code that runs prior to the body of the constructor, on invocation.
   https://dart.dev/language/constructors#initializer-list
*)
let augment_body initializers body =
  let initializers =
    List_.map (fun e -> ExprStmt (e, G.sc) |> G.s) initializers
  in
  match (initializers, body) with
  | [], _ -> body
  | _, FBStmt { s = Block (l, stmts, r); _ } ->
      FBStmt (Block (l, initializers @ stmts, r) |> G.s)
  | _, FBStmt s -> FBStmt (Block (fb (initializers @ [ s ])) |> G.s)
  | _, FBExpr e ->
      FBStmt (Block (fb (initializers @ [ ExprStmt (e, G.sc) |> G.s ])) |> G.s)
  | _, FBDecl _
  | _, FBNothing ->
      FBStmt (Block (fb initializers) |> G.s)

let map_method_signature (env : env) (x : CST.method_signature) (attrs, body) =
  match x with
  | `Cons_sign_opt_initis (v1, v2) ->
      let dotted, fparams = map_constructor_signature env v1 in
      let v2 =
        match v2 with
        | Some x -> map_initializers env x
        | None -> []
      in
      let ent = { name = EN (H2.name_of_ids dotted); attrs; tparams = None } in
      let fbody = augment_body v2 body in
      DefStmt
        ( ent,
          FuncDef
            { fkind = (Method, fake "Method"); fparams; frettype = None; fbody }
        )
      |> G.s
  | `Fact_cons_sign x ->
      let attr, dotted, fparams = map_factory_constructor_signature env x in
      let ent =
        {
          name = EN (H2.name_of_ids dotted);
          attrs = [ attr ] @ attrs;
          tparams = None;
        }
      in
      DefStmt
        ( ent,
          FuncDef
            {
              fkind = (Method, fake "Method");
              fparams;
              frettype = None;
              fbody = body;
            } )
      |> G.s
  | `Opt_static_choice_func_sign (v1, v2) ->
      let attrs =
        match v1 with
        | Some tok ->
            [ KeywordAttr (Static, (* "static" *) token env tok) ] @ attrs
        | None -> attrs
      in
      let v2 =
        match v2 with
        | `Func_sign x ->
            map_function_signature ~attrs env x
              ((Function, fake "Function"), FBNothing)
        | `Getter_sign x -> map_getter_signature ~attrs env x FBNothing
        | `Setter_sign x -> map_setter_signature ~attrs env x FBNothing
      in
      v2
  | `Op_sign x -> map_operator_signature ~attrs env x

let map_anon_choice_redi_3f8cf96 (env : env) (x : CST.anon_choice_redi_3f8cf96)
    : expr list =
  (* These are initializers, which may appear after a constructor, but before
     the body, which result in code being run before the body of the constructor.
     https://dart.dev/language/constructors#initializer-list
  *)
  match x with
  | `Redi (v1, v2, v3, v4) ->
      let _v1 = (* ":" *) token env v1 in
      let v2 = (* "this" *) token env v2 in
      let this = IdSpecial (This, v2) |> G.e in
      let v3 =
        match v3 with
        | Some x ->
            DotAccess
              ( this,
                fake "",
                FN (Id (map_type_dot_identifier env x, empty_id_info ())) )
            |> G.e
        | None -> this
      in
      let v4 = map_arguments env v4 in
      [ Call (v3, v4) |> G.e ]
  | `Initis x -> map_initializers env x

(* All imports in Dart are wildcards by default.
   It is only in aliasing the library's name that the library itself
   is brought into scope, as opposed to all of the names within it.

   https://dart.dev/language/libraries#specifying-a-library-prefix
*)

let map_import_specification (env : env) (x : CST.import_specification) =
  match x with
  | `Import_conf_uri_opt_as_id_rep_comb_semi (v1, v2, v3, v4, v5) -> (
      let v1 = (* "import" *) token env v1 in
      let uri = map_configurable_uri env v2 in
      let sc = map_semicolon env v5 in
      (* brandon: I'm not convinced it's possible to have both a package
         alias and to do selective importing.
         If it is, the package alias should be more important, anyways.
      *)
      match v3 with
      | Some (v1, v2) ->
          let v1 = (* "as" *) token env v1 in
          let v2 = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
          ImportAs (v1, uri, Some (v2, G.empty_id_info ()))
      | None -> (
          let v4 = List.concat_map (map_combinator env) v4 in
          match v4 with
          | [] ->
              (* No selective imports, so remain a wildcard import. *)
              ImportAll (v1, uri, sc)
          | _ ->
              (* If there are selective imports, convert to an ImportFrom. *)
              let selected =
                List_.map (fun id -> H2.mk_import_from_kind id None) v4
              in
              ImportFrom (v1, uri, selected)))
  | `Import_uri_defe_as_id_rep_comb_semi (v1, v2, v3, v4, v5, v6, v7) ->
      (* This is a 'lazy import'.
         https://dart.dev/language/libraries#importing-only-part-of-a-library
         We'll not treat it specially for now.
      *)
      let v1 = (* "import" *) token env v1 in
      let uri = map_uri env v2 in
      let _v3_TODO = (* "deferred" *) token env v3 in
      let _v4 = (* "as" *) token env v4 in
      let v5 = (* pattern [a-zA-Z_$][\w$]* *) str env v5 in
      let _v6 = List_.map (map_combinator env) v6 in
      let _sc = map_semicolon env v7 in
      (* For the same reason as above, we prefer the "ImportAs"
         interpretation, in which case the combinators are not relevant.
      *)
      ImportAs (v1, uri, Some (v5, G.empty_id_info ()))

let map_declaration_ ?(attrs = []) (env : env) (x : CST.declaration_) :
    stmt list =
  match x with
  | `Cst_cons_sign_opt_choice_redi (v1, v2) ->
      let attr, dotted, fparams = map_constant_constructor_signature env v1 in
      let initializers =
        match v2 with
        | Some x -> map_anon_choice_redi_3f8cf96 env x
        | None -> []
      in
      let fbody = augment_body initializers FBNothing in
      let ent =
        {
          name = EN (H2.name_of_ids dotted);
          attrs = [ attr ] @ attrs;
          tparams = None;
        }
      in
      [
        DefStmt
          ( ent,
            FuncDef
              {
                fkind = (Function, fake "Function");
                fparams;
                frettype = None;
                fbody;
              } )
        |> G.s;
      ]
  | `Cons_sign_opt_choice_redi (v1, v2) ->
      let dotted, fparams = map_constructor_signature env v1 in
      let initializers =
        match v2 with
        | Some x -> map_anon_choice_redi_3f8cf96 env x
        | None -> []
      in
      let ent = { name = EN (H2.name_of_ids dotted); attrs; tparams = None } in
      let fbody = augment_body initializers FBNothing in
      [
        DefStmt
          ( ent,
            FuncDef
              {
                fkind = (Function, fake "Function");
                fparams;
                frettype = None;
                fbody;
              } )
        |> G.s;
      ]
  | `Exte_opt_const_buil_fact_cons_sign (v1, v2, v3) ->
      let v1 = KeywordAttr (Extern, (* "external" *) token env v1) in
      let v2 =
        match v2 with
        | Some tok -> [ KeywordAttr (Const, (* const_builtin *) token env tok) ]
        | None -> []
      in
      let attr, dotted, fparams = map_factory_constructor_signature env v3 in
      let attrs = [ v1 ] @ v2 @ [ attr ] in
      let ent =
        {
          name = EN (H2.name_of_ids dotted);
          attrs = [ attr ] @ attrs;
          tparams = None;
        }
      in
      [
        DefStmt
          ( ent,
            FuncDef
              {
                fkind = (Function, fake "Function");
                fparams;
                frettype = None;
                fbody = FBNothing;
              } )
        |> G.s;
      ]
  | `Opt_const_buil_fact_cons_sign_native (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some tok -> [ KeywordAttr (Const, (* const_builtin *) token env tok) ]
        | None -> []
      in
      let attr, dotted, fparams = map_factory_constructor_signature env v2 in
      let attrs = v1 @ [ attr ] @ attrs in
      let _v3_TODO = map_native env v3 in
      let ent = { name = EN (H2.name_of_ids dotted); attrs; tparams = None } in
      [
        DefStmt
          ( ent,
            FuncDef
              {
                fkind = (Function, fake "Function");
                fparams;
                frettype = None;
                fbody = FBNothing;
              } )
        |> G.s;
      ]
  | `Exte_cst_cons_sign (v1, v2) ->
      let v1 = KeywordAttr (Extern, (* "external" *) token env v1) in
      let attr, dotted, fparams = map_constant_constructor_signature env v2 in
      let attrs = [ v1; attr ] @ attrs in
      let ent = { name = EN (H2.name_of_ids dotted); attrs; tparams = None } in
      [
        DefStmt
          ( ent,
            FuncDef
              {
                fkind = (Function, fake "Function");
                fparams;
                frettype = None;
                fbody = FBNothing;
              } )
        |> G.s;
      ]
  | `Redi_fact_cons_sign (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let attrs =
        let v1 =
          match v1 with
          | Some tok ->
              [ KeywordAttr (Const, (* const_builtin *) token env tok) ] @ attrs
          | None -> attrs
        in
        let v2 = unhandled_keywordattr ((* "factory" *) str env v2) in
        v1 @ [ v2 ]
      in
      let dotted =
        let v3 = (* pattern [a-zA-Z_$][\w$]* *) str env v3 in
        let v4 = List_.map (map_type_dot_identifier env) v4 in
        v3 :: v4
      in
      let fparams = map_formal_parameter_list env v5 in
      let _v6 = (* "=" *) token env v6 in
      let v7 = map_type_not_void env v7 in
      let v8 =
        match v8 with
        | Some x -> [ G.I (map_type_dot_identifier env x) ]
        | None -> []
      in
      [
        DefStmt
          ( { name = EN (H2.name_of_ids dotted); attrs; tparams = None },
            FuncDef
              {
                fkind = (Method, fake "method");
                fparams;
                frettype = None;
                fbody =
                  FBExpr
                    (OtherExpr (("Redirect", fake "Redirect"), [ G.T v7 ] @ v8)
                    |> G.e);
              } )
        |> G.s;
      ]
  | `Exte_cons_sign (v1, v2) ->
      let v1 = KeywordAttr (Extern, (* "external" *) token env v1) in
      let dotted, fparams = map_constructor_signature env v2 in
      let ent =
        {
          name = EN (H2.name_of_ids dotted);
          attrs = [ v1 ] @ attrs;
          tparams = None;
        }
      in
      [
        DefStmt
          ( ent,
            FuncDef
              {
                fkind = (Function, fake "Function");
                fparams;
                frettype = None;
                fbody = FBNothing;
              } )
        |> G.s;
      ]
  | `Opt_exte_buil_opt_static_getter_sign (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some tok -> [ KeywordAttr (Extern, (* "external" *) token env tok) ]
        | None -> []
      in
      let v2 =
        match v2 with
        | Some tok -> [ KeywordAttr (Static, (* "static" *) token env tok) ]
        | None -> []
      in
      let attrs = v1 @ v2 @ attrs in
      let v3 = map_getter_signature ~attrs env v3 FBNothing in
      [ v3 ]
  | `Opt_exte_and_static_setter_sign (v1, v2) ->
      let attrs =
        match v1 with
        | Some x -> map_external_and_static env x @ attrs
        | None -> attrs
      in
      let v2 = map_setter_signature ~attrs env v2 FBNothing in
      [ v2 ]
  | `Opt_exte_op_sign (v1, v2) ->
      let attrs =
        match v1 with
        | Some tok ->
            [ KeywordAttr (Extern, (* "external" *) token env tok) ] @ attrs
        | None -> attrs
      in
      let v2 = map_operator_signature ~attrs env v2 in
      [ v2 ]
  | `Opt_exte_and_static_func_sign (v1, v2) ->
      let attrs =
        match v1 with
        | Some x -> map_external_and_static env x @ attrs
        | None -> attrs
      in
      let v2 =
        map_function_signature ~attrs env v2 ((Method, fake "method"), FBNothing)
      in
      [ v2 ]
  | `Static_func_sign (v1, v2) ->
      let v1 = KeywordAttr (Static, (* "static" *) token env v1) in
      let v2 =
        map_function_signature ~attrs:([ v1 ] @ attrs) env v2
          ((Method, fake "method"), FBNothing)
      in
      [ v2 ]
  | `Static_choice_final_or_const_opt_type_static_final_decl_list (v1, v2) ->
      let static_attr = KeywordAttr (Static, (* "static" *) token env v1) in
      let new_attrs, vtype, inits =
        match v2 with
        | `Final_or_const_opt_type_static_final_decl_list (v1, v2, v3) ->
            let v1 = map_final_or_const env v1 in
            let vtype =
              match v2 with
              | Some x -> Some (map_type_ env x)
              | None -> None
            in
            let v3 = map_static_final_declaration_list env v3 in
            let attrs = [ v1 ] in
            (attrs, vtype, List_.map (fun (id, e) -> (id, Some e)) v3)
        | `Late_buil_choice_final_buil_opt_type_init_id_list (v1, v2) ->
            let late_attr = unhandled_keywordattr ((* "late" *) str env v1) in
            let v2 =
              match v2 with
              | `Final_buil_opt_type_init_id_list (v1, v2, v3) ->
                  let v1 =
                    KeywordAttr (Final, (* final_builtin *) token env v1)
                  in
                  let vtype =
                    match v2 with
                    | Some x -> Some (map_type_ env x)
                    | None -> None
                  in
                  let attrs = [ late_attr; v1 ] in
                  let v3 = map_initialized_identifier_list env v3 in
                  (attrs, vtype, v3)
              | `Choice_type_init_id_list (v1, v2) ->
                  let v1 = map_anon_choice_type_be0da33 env v1 in
                  let v2 = map_initialized_identifier_list env v2 in
                  let attrs = [ late_attr ] in
                  (attrs, Some v1, v2)
            in
            v2
        | `Choice_type_init_id_list (v1, v2) ->
            let v1 = map_anon_choice_type_be0da33 env v1 in
            let v2 = map_initialized_identifier_list env v2 in
            let attrs = [] in
            (attrs, Some v1, v2)
      in
      let attrs = (static_attr :: new_attrs) @ attrs in
      List_.map
        (fun (id, vinit) ->
          DefStmt
            (basic_entity ~attrs id, VarDef { vinit; vtype; vtok = G.no_sc })
          |> G.s)
        inits
  | `Cova_choice_late_buil_choice_final_buil_opt_type_id_list_ (v1, v2) ->
      let cov_attr = unhandled_keywordattr ((* "covariant" *) str env v1) in
      let new_attrs, vtype, inits =
        match v2 with
        | `Late_buil_choice_final_buil_opt_type_id_list_ (v1, v2) ->
            let late_attr = unhandled_keywordattr ((* "late" *) str env v1) in
            let v2 =
              match v2 with
              | `Final_buil_opt_type_id_list_ (v1, v2, v3) ->
                  let v1 =
                    KeywordAttr (Final, (* final_builtin *) token env v1)
                  in
                  let attrs = [ late_attr; v1 ] in
                  let vtype =
                    match v2 with
                    | Some x -> Some (map_type_ env x)
                    | None -> None
                  in
                  let v3 = map_identifier_list_ env v3 in
                  (attrs, vtype, List_.map (fun id -> (id, None)) v3)
              | `Choice_type_init_id_list (v1, v2) ->
                  let v1 = map_anon_choice_type_be0da33 env v1 in
                  let v2 = map_initialized_identifier_list env v2 in
                  let attrs = [ cov_attr; late_attr ] in
                  (attrs, Some v1, v2)
            in
            v2
        | `Choice_type_init_id_list (v1, v2) ->
            let v1 = map_anon_choice_type_be0da33 env v1 in
            let v2 = map_initialized_identifier_list env v2 in
            let attrs = [] in
            (attrs, Some v1, v2)
      in
      let attrs = (cov_attr :: new_attrs) @ attrs in
      List_.map
        (fun (id, vinit) ->
          DefStmt
            (basic_entity ~attrs id, VarDef { vinit; vtype; vtok = G.no_sc })
          |> G.s)
        inits
  | `Opt_late_buil_final_buil_opt_type_init_id_list (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some tok -> [ unhandled_keywordattr ((* "late" *) str env tok) ]
        | None -> []
      in
      let v2 = KeywordAttr (Final, (* final_builtin *) token env v2) in
      let attrs = v1 @ [ v2 ] @ attrs in
      let vtype =
        match v3 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      let inits = map_initialized_identifier_list env v4 in
      List_.map
        (fun (id, vinit) ->
          DefStmt
            (basic_entity ~attrs id, VarDef { vinit; vtype; vtok = G.no_sc })
          |> G.s)
        inits
  | `Opt_late_buil_var_or_type_init_id_list (v1, v2, v3) ->
      let attrs =
        match v1 with
        | Some tok ->
            [ unhandled_keywordattr ((* "late" *) str env tok) ] @ attrs
        | None -> attrs
      in
      let v2 = map_var_or_type env v2 in
      let inits = map_initialized_identifier_list env v3 in
      List_.map
        (fun (id, vinit) ->
          DefStmt
            ( basic_entity ~attrs id,
              VarDef { vinit; vtype = Some v2; vtok = G.no_sc } )
          |> G.s)
        inits

let map_declaration_as_stmt (env : env) (x : CST.declaration_) : stmt =
  Block (fb (map_declaration_ env x)) |> G.s

let map_class_member_definition ~attrs (env : env)
    (x : CST.class_member_definition) : field =
  match x with
  | `Decl__semi (v1, v2) ->
      let v1 = map_declaration_as_stmt env v1 in
      let _sc = map_semicolon env v2 in
      G.F v1
  | `Meth_sign_func_body (v1, v2) ->
      let v1 = map_method_signature env v1 in
      let fattrs, v2 = map_function_body env v2 in
      G.F (v1 (attrs @ fattrs, v2))

let map_extension_body (env : env) ((v1, v2, v3) : CST.extension_body) :
    stmt list =
  let _v1 = (* "{" *) token env v1 in
  let v2 =
    List.concat_map
      (fun x ->
        match x with
        | `Opt_meta_decl__semi (v1, v2, v3) ->
            let attrs =
              match v1 with
              | Some x -> map_metadata env x
              | None -> []
            in
            let v2 = map_declaration_ ~attrs env v2 in
            let _sc = map_semicolon env v3 in
            v2
        | `Opt_meta_meth_sign_func_body (v1, v2, v3) ->
            let attrs =
              match v1 with
              | Some x -> map_metadata env x
              | None -> []
            in
            let v2 = map_method_signature env v2 in
            let fattrs, v3 = map_function_body env v3 in
            [ v2 (attrs @ fattrs, v3) ])
      v2
  in
  let _v3 = (* "}" *) token env v3 in
  v2

let map_import_or_export (env : env) (x : CST.import_or_export) : stmt =
  let d =
    match x with
    | `Libr_import (v1, v2) ->
        let v1 =
          match v1 with
          | Some x -> map_metadata env x
          | None -> []
        in
        let d = map_import_specification env v2 |> G.d in
        { d with d_attrs = v1 }
    | `Libr_export (v1, v2, v3, v4, v5) ->
        let v1 =
          match v1 with
          | Some x -> map_metadata env x
          | None -> []
        in
        let v2 = (* "export" *) str env v2 in
        let v3 = map_configurable_uri env v3 in
        let v4 = List.concat_map (map_combinator env) v4 in
        let _sc = map_semicolon env v5 in
        let d =
          OtherDirective (v2, G.Modn v3 :: List_.map (fun x -> G.I x) v4) |> G.d
        in
        { d with d_attrs = v1 }
  in
  DirectiveStmt d |> G.s

let map_class_body (env : env) ((v1, v2, v3) : CST.class_body) :
    field list bracket =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let attrs =
          match v1 with
          | Some x -> map_metadata env x
          | None -> []
        in
        let v2 = map_class_member_definition ~attrs env v2 in
        v2)
      v2
  in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

let map_extension_declaration ~attrs (env : env) (x : CST.extension_declaration)
    : stmt =
  match x with
  | `Exte_opt_id_opt_type_params_on_type_exte_body (v1, v2, v3, v4, v5, v6) ->
      let _v1 = (* "extension" *) token env v1 in
      let attrs = [ G.Anys (List_.map (fun attr -> G.At attr) attrs) ] in
      let v2 =
        match v2 with
        | Some tok -> [ G.I ((* pattern [a-zA-Z_$][\w$]* *) str env tok) ]
        | None -> []
      in
      let v3 =
        match v3 with
        | Some x ->
            let _, xs, _ = map_type_parameters env x in
            [ G.Anys (xs |> List_.map (fun tp -> G.Tp tp)) ]
        | None -> []
      in
      let _v4 = (* "on" *) token env v4 in
      let v5 = [ G.T (map_type_ env v5) ] in
      let v6 = [ G.Ss (map_extension_body env v6) ] in
      G.OtherStmt (OS_Extension, attrs @ v2 @ v3 @ v5 @ v6) |> G.s

let map_class_definition ~attrs (env : env) (x : CST.class_definition) : stmt =
  match x with
  | `Opt_abst_class_id_opt_type_params_opt_supe_opt_inters_class_body
      (v1, v2, v3, v4, v5, v6, v7) ->
      let attrs =
        match v1 with
        | Some tok ->
            attrs @ [ KeywordAttr (Abstract, (* "abstract" *) token env tok) ]
        | None -> attrs
      in
      let v2 = (* "class" *) token env v2 in
      let v3 = (* pattern [a-zA-Z_$][\w$]* *) str env v3 in
      let tparams = Option.map (map_type_parameters env) v4 in
      let cextends, cmixins =
        match v5 with
        | Some x -> map_superclass env x
        | None -> ([], [])
      in
      let cimplements =
        match v6 with
        | Some x -> map_interfaces env x
        | None -> []
      in
      let cbody = map_class_body env v7 in
      DefStmt
        ( basic_entity ~attrs ?tparams v3,
          ClassDef
            {
              ckind = (Class, v2);
              cextends;
              cimplements;
              cparams = fb [];
              cbody;
              cmixins;
            } )
      |> G.s
  | `Opt_meta_opt_abst_class_mixin_app_class (v1, v2, v3, v4) ->
      let attrs =
        match v1 with
        | Some x -> attrs @ map_metadata env x
        | None -> attrs
      in
      let attrs =
        match v2 with
        | Some tok ->
            attrs @ [ KeywordAttr (Abstract, (* "abstract" *) token env tok) ]
        | None -> attrs
      in
      let class_tok = (* "class" *) token env v3 in
      map_mixin_application_class ~attrs ~class_tok env v4

let map_top_level_definition ~attrs (env : env) (x : CST.top_level_definition) :
    stmt list =
  match x with
  | `Class_defi x -> [ map_class_definition ~attrs env x ]
  | `Enum_decl x -> [ map_enum_declaration ~attrs env x ]
  | `Exte_decl x -> [ map_extension_declaration ~attrs env x ]
  | `Mixin_decl (v1, v2, v3, v4, v5, v6) ->
      (* A mixin is basically a nominative extension of a class.
         https://dart.dev/language/mixins
      *)
      let v1 = (* "mixin" *) str env v1 in
      let v2 = (* pattern [a-zA-Z_$][\w$]* *) str env v2 in
      let v3 =
        match v3 with
        | Some x ->
            let _, xs, _ = map_type_parameters env x in
            [ G.Anys (xs |> List_.map (fun tp -> G.Tp tp)) ]
        | None -> []
      in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            [
              G.Anys
                (let _v1 = (* "on" *) token env v1 in
                 let v2 = map_type_not_void_list env v2 in
                 List_.map (fun ty -> G.T ty) v2);
            ]
        | None -> []
      in
      let v5 =
        match v5 with
        | Some x ->
            [ G.Anys (List_.map (fun ty -> G.T ty) (map_interfaces env x)) ]
        | None -> []
      in
      let _, v6, _ = map_class_body env v6 in
      let attrs = Anys (attrs |> List_.map (fun x -> G.At x)) in
      [
        OtherStmt
          (OS_Todo, [ attrs; TodoK v1; G.I v2 ] @ v3 @ v4 @ v5 @ [ G.Flds v6 ])
        |> G.s;
      ]
  | `Type_alias x -> [ map_type_alias ~attrs env x ]
  | `Opt_exte_buil_func_sign_semi (v1, v2, v3) ->
      let attrs =
        match v1 with
        | Some tok ->
            attrs @ [ G.unhandled_keywordattr ((* "external" *) str env tok) ]
        | None -> attrs
      in
      let v2 =
        map_function_signature ~attrs env v2
          ((Function, fake "function"), FBStmt (Block (fb []) |> G.s))
      in
      let _sc = map_semicolon env v3 in
      [ v2 ]
  | `Opt_exte_buil_getter_sign_semi (v1, v2, v3) ->
      let attrs =
        match v1 with
        | Some tok ->
            attrs @ [ unhandled_keywordattr ((* "external" *) str env tok) ]
        | None -> attrs
      in
      let v2 =
        map_getter_signature ~attrs env v2 (FBStmt (Block (fb []) |> G.s))
      in
      let _sc = map_semicolon env v3 in
      [ v2 ]
  | `Opt_exte_buil_setter_sign_semi (v1, v2, v3) ->
      let attrs =
        match v1 with
        | Some tok ->
            attrs @ [ unhandled_keywordattr ((* "external" *) str env tok) ]
        | None -> attrs
      in
      let v2 =
        map_setter_signature ~attrs env v2 (FBStmt (Block (fb []) |> G.s))
      in
      let _sc = map_semicolon env v3 in
      [ v2 ]
  | `Func_sign_func_body x -> [ map_lambda_expression ~attrs env x ]
  | `Getter_sign_func_body (v1, v2) ->
      let fattrs, fbody = map_function_body env v2 in
      let v1 = map_getter_signature ~attrs:(attrs @ fattrs) env v1 fbody in
      [ v1 ]
  | `Setter_sign_func_body (v1, v2) ->
      let fattrs, fbody = map_function_body env v2 in
      let v1 = map_setter_signature ~attrs:(attrs @ fattrs) env v1 fbody in
      [ v1 ]
  | `Choice_final_buil_opt_type_static_final_decl_list_semi (v1, v2, v3, v4) ->
      let attrs =
        let v1 = map_final_or_const env v1 in
        attrs @ [ v1 ]
      in
      let vtype =
        match v2 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      let v3 = map_static_final_declaration_list env v3 in
      (* TODO: inject it at least in the last decl in v3? *)
      let _sc = map_semicolon env v4 in
      v3
      |> List_.map (fun (id, expr) ->
             G.DefStmt
               ( basic_entity ~attrs id,
                 VarDef { vinit = Some expr; vtype; vtok = G.no_sc } )
             |> G.s)
  | `Late_buil_final_buil_opt_type_init_id_list_semi (v1, v2, v3, v4, v5) ->
      let attrs =
        let v1 = G.unhandled_keywordattr ((* "late" *) str env v1) in
        let v2 = KeywordAttr (Final, (* final_builtin *) token env v2) in
        attrs @ [ v1; v2 ]
      in
      let vtype =
        match v3 with
        | Some x -> Some (map_type_ env x)
        | None -> None
      in
      let inits = map_initialized_identifier_list env v4 in
      let sc = map_semicolon env v5 in
      inits
      |> List_.map (fun (id, vinit) ->
             (basic_entity ~attrs id, { vinit; vtype; vtok = G.no_sc }))
      |> H2.add_semicolon_to_last_var_def_and_convert_to_stmts sc
  | `Opt_late_buil_choice_type_init_id_list_semi (v1, v2, v3, v4) ->
      let attrs =
        match v1 with
        | Some tok ->
            attrs @ [ G.unhandled_keywordattr ((* "late" *) str env tok) ]
        | None -> attrs
      in
      let vtype = map_var_or_type env v2 in
      let inits = map_initialized_identifier_list env v3 in
      let sc = map_semicolon env v4 in
      inits
      |> List_.map (fun (id, vinit) ->
             ( basic_entity ~attrs id,
               { vinit; vtype = Some vtype; vtok = G.no_sc } ))
      |> H2.add_semicolon_to_last_var_def_and_convert_to_stmts sc

let map_program (env : env) (prog : CST.program) =
  match prog with
  | `Semg_exp (v1, v2) ->
      let _v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
      G.E (map_expression env v2)
  | `Opt_script_tag_opt_libr_name_rep_import_or_export_rep_part_dire_rep_part_of_dire_rep_opt_meta_top_level_defi_rep_stmt
      (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | Some x -> [ map_script_tag env x ]
        | None -> []
      in
      let v2 =
        match v2 with
        | Some x -> [ map_library_name env x ]
        | None -> []
      in
      let v3 = List_.map (map_import_or_export env) v3 in
      let v4 = List_.map (map_part_directive env) v4 in
      let v5 = List_.map (map_part_of_directive env) v5 in
      let v6 =
        List.concat_map
          (fun (v1, v2) ->
            let v1 =
              match v1 with
              | Some x -> map_metadata env x
              | None -> []
            in
            let v2 = map_top_level_definition ~attrs:v1 env v2 in
            v2)
          v6
      in
      let v7 = List.concat_map (map_statement env) v7 in
      G.Pr (v1 @ v2 @ v3 @ v4 @ v5 @ v6 @ v7)

(*****************************************************************************)
(* Set forward references *)
(*****************************************************************************)

let () =
  forward_reference_map_template_substitution := map_template_substitution

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_dart.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Program } in
      let any = map_program env cst in
      match any with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

(* Cribbed from the Cairo parser. *)
let parse_expression_or_source_file str =
  let res = Tree_sitter_dart.Parse.string str in
  match res.errors with
  | [] -> res
  | _ ->
      let expr_str = "__SEMGREP_EXPRESSION " ^ str in
      Tree_sitter_dart.Parse.string expr_str

let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_expression_or_source_file str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern }
      in
      let any = map_program env cst in
      (* this will be simplified i:f needed in Parse_pattern.normalize_any *)
      any)

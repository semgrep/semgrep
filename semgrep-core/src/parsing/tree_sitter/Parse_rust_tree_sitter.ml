(* Ruin0x11
 *
 * Copyright (C) 2021 r2c
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
module CST = Tree_sitter_rust.CST
module H = Parse_tree_sitter_helpers
module H2 = AST_generic_helpers
module PI = Parse_info
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Rust parser using tree-sitter-lang/semgrep-rust and converting
 * directly to pfff/h_program-lang/ast_generic.ml
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token

let str = H.str

let sc = G.sc

let fb = G.fake_bracket

let fake_id s = (s, G.fake s)

let stmt_to_expr stmt = G.OtherExpr (G.OE_StmtExpr, [ G.S stmt ])

(* TODO: delete once return proper AST_generic.name in a few functions *)
let name_of_id id = (id, G.empty_name_info)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-rust/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the rust CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type function_declaration_rs = {
  name : G.name_or_dynamic;
  type_params : G.type_parameter list;
  params : G.parameter list;
  retval : G.type_ option;
}

and trait_bound =
  | TraitBoundType of G.type_
  | TraitBoundLifetime of lifetime
  | TraitBoundHigherRanked of G.type_parameter list * G.type_
  | TraitBoundRemoved of G.type_

and where_clause = where_predicate list

and where_predicate = where_predicate_type * trait_bound list

and where_predicate_type =
  | WherePredLifetime of lifetime
  | WherePredId of G.ident
  | WherePredType of G.type_
  | WherePredHigherRanked of G.type_parameter list * G.type_

and lifetime = G.ident

and rust_macro_definition = rust_macro_rule list G.bracket

and rust_macro_item =
  | Tk of G.tok
  | MacTkTree of rust_macro_item list G.bracket
  | MacTks of rust_macro_item list G.bracket * G.ident option * G.tok

and rust_macro_rule = {
  rules : rust_macro_pattern list;
  body : rust_macro_item list G.bracket;
}

and rust_macro_pattern =
  | RustMacPatTree of rust_macro_pattern list
  | RustMacPatRepetition of
      rust_macro_pattern list G.bracket * G.ident option * G.tok
  | RustMacPatBinding of G.ident * G.tok
  | RustMacPatToken of G.tok

and rust_meta_argument =
  | MetaArgMetaItem of rust_meta_item
  | MetaArgLiteral of G.literal

and rust_meta_item_value =
  | MetaItemLiteral of G.literal
  | MetaItemMetaArgs of rust_meta_argument list

and rust_meta_item = G.dotted_ident * rust_meta_item_value option

and rust_attribute = AttrInner of rust_meta_item | AttrOuter of rust_meta_item

let deoptionalize l =
  let rec deopt acc = function
    | [] -> List.rev acc
    | None :: tl -> deopt acc tl
    | Some x :: tl -> deopt (x :: acc) tl
  in
  deopt [] l

let ident (env : env) (tok : CST.identifier) : G.ident = str env tok

(* pattern [a-zA-Z_]\w* *)

let map_fragment_specifier (env : env) (x : CST.fragment_specifier) =
  match x with
  | `Blk tok -> token env tok (* "block" *)
  | `Expr tok -> token env tok (* "expr" *)
  | `Id tok -> token env tok (* "ident" *)
  | `Item tok -> token env tok (* "item" *)
  | `Life tok -> token env tok (* "lifetime" *)
  | `Lit tok -> token env tok (* "literal" *)
  | `Meta tok -> token env tok (* "meta" *)
  | `Pat tok -> token env tok (* "pat" *)
  | `Path tok -> token env tok (* "path" *)
  | `Stmt tok -> token env tok (* "stmt" *)
  | `Tt tok -> token env tok (* "tt" *)
  | `Ty tok -> token env tok (* "ty" *)
  | `Vis tok -> token env tok

(* "vis" *)

let map_token_quantifier (env : env) (x : CST.anon_choice_PLUS_348fa54) =
  match x with
  | `PLUS tok -> token env tok (* "+" *)
  | `STAR tok -> token env tok (* "*" *)
  | `QMARK tok -> token env tok

(* "?" *)

let map_boolean_literal (env : env) (x : CST.boolean_literal) : G.literal =
  match x with
  | `True tok -> G.Bool (true, token env tok) (* "true" *)
  | `False tok -> G.Bool (false, token env tok)

(* "false" *)

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  match x with
  | `Defa tok -> ident env tok (* "default" *)
  | `Union tok -> ident env tok

(* "union" *)

let map_primitive_type_token (env : env) (x : CST.anon_choice_u8_6dad923) =
  match x with
  | `U8 tok (* "u8" *)
  | `I8 tok (* "i8" *)
  | `U16 tok (* "u16" *)
  | `I16 tok (* "i16" *)
  | `U32 tok (* "u32" *)
  | `I32 tok (* "i32" *)
  | `U64 tok (* "u64" *)
  | `I64 tok (* "i64" *)
  | `U128 tok (* "u128" *)
  | `I128 tok (* "i128" *)
  | `Isize tok (* "isize" *)
  | `Usize tok (* "usize" *)
  | `F32 tok (* "f32" *)
  | `F64 tok (* "f64" *)
  | `Bool tok (* "bool" *)
  | `Str tok (* "str" *)
  | `Char tok (* "char" *) ->
      tok

let map_primitive_type_ident (env : env) (x : CST.anon_choice_u8_6dad923) :
    string * PI.token_mutable =
  str env (map_primitive_type_token env x)

let map_primitive_type (env : env) (x : CST.anon_choice_u8_6dad923) : G.type_ =
  let s, tok = map_primitive_type_ident env x in
  G.TyBuiltin (s, tok)

let map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) :
    G.literal =
  let ldquote = token env v1 (* pattern "b?\"" *) in
  let strs =
    List.map
      (fun x ->
        match x with
        | `Esc_seq tok -> str env tok (* escape_sequence *)
        | `Str_content tok -> str env tok
        (* string_content *))
      v2
  in
  let rdquote = token env v3 (* "\"" *) in
  let str = strs |> List.map fst |> String.concat "" in
  let toks = (strs |> List.map snd) @ [ rdquote ] in
  G.String (str, PI.combine_infos ldquote toks)

let integer_literal env tok =
  let s, t = str env tok in
  (int_of_string_opt s, t)

let float_literal env tok =
  let s, t = str env tok in
  (float_of_string_opt s, t)

let map_literal (env : env) (x : CST.literal) : G.literal =
  match x with
  | `Str_lit x -> map_string_literal env x
  | `Raw_str_lit tok -> G.String (str env tok) (* raw_string_literal *)
  | `Char_lit tok -> G.Char (str env tok) (* char_literal *)
  | `Bool_lit x -> map_boolean_literal env x
  | `Int_lit tok -> G.Int (integer_literal env tok) (* integer_literal *)
  | `Float_lit tok -> G.Float (float_literal env tok)

(* float_literal *)

let map_literal_token (env : env) (x : CST.literal) : PI.token_mutable =
  let lit = map_literal env x in
  match lit with
  | Bool (_, tok)
  | Int (_, tok)
  | Float (_, tok)
  | Char (_, tok)
  | String (_, tok)
  | Unit tok
  | Null tok
  | Undefined tok
  | Imag (_, tok)
  | Ratio (_, tok)
  (* TODO? use PI.combine_info for the other tokens in it ? *)
  | Atom (_, (_, tok))
  | Regexp ((_, (_, tok), _), _) ->
      tok

let map_literal_pattern (env : env) (x : CST.literal_pattern) : G.pattern =
  match x with
  | `Str_lit x -> G.PatLiteral (map_string_literal env x)
  | `Raw_str_lit tok ->
      G.PatLiteral (G.String (str env tok)) (* raw_string_literal *)
  | `Char_lit tok -> G.PatLiteral (G.Char (str env tok)) (* char_literal *)
  | `Bool_lit x -> G.PatLiteral (map_boolean_literal env x)
  | `Int_lit tok ->
      G.PatLiteral (G.Int (integer_literal env tok)) (* integer_literal *)
  | `Float_lit tok ->
      G.PatLiteral (G.Float (float_literal env tok)) (* float_literal *)
  | `Nega_lit (v1, v2) -> (
      let neg = str env v1 (* "-" *) in
      match v2 with
      | `Int_lit tok ->
          let iopt, t = integer_literal env tok in
          (* integer_literal *)
          let iopt = match iopt with Some i -> Some (-i) | None -> None in
          G.PatLiteral (G.Int (iopt, PI.combine_infos (snd neg) [ t ]))
      | `Float_lit tok ->
          let fopt, t = float_literal env tok in
          (* float_literal *)
          let fopt = match fopt with Some f -> Some (-.f) | None -> None in
          G.PatLiteral (G.Float (fopt, PI.combine_infos (snd neg) [ t ])) )

let map_extern_modifier (env : env) ((v1, v2) : CST.extern_modifier) :
    G.attribute list =
  let extern = token env v1 (* "extern" *) in
  let extern_attr = G.KeywordAttr (G.Extern, extern) in

  (* as in 'extern "C"' or 'extern "stdcall"' *)
  let quantifier =
    Option.map
      (fun x ->
        let str = map_string_literal env x in
        G.OtherAttribute (G.OA_Expr, [ G.E (G.L str) ]))
      v2
  in

  deoptionalize [ Some extern_attr; quantifier ]

let rec map_simple_path (env : env) (x : CST.simple_path) : G.dotted_ident =
  match x with
  | `Self tok -> [ ident env tok ] (* "self" *)
  | `Choice_u8 x -> [ map_primitive_type_ident env x ]
  | `Meta tok -> [ ident env tok ] (* pattern \$[a-zA-Z_]\w* *)
  | `Super tok -> [ ident env tok ] (* "super" *)
  | `Crate tok -> [ ident env tok ] (* "crate" *)
  | `Id tok ->
      [ ident env tok ]
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  | `Simple_scoped_id x ->
      let dots, ident = map_simple_scoped_identifier env x in
      ident :: dots

and map_simple_path_ident (env : env) (x : CST.simple_path) :
    G.dotted_ident * G.ident =
  match x with
  | `Self tok -> ([], ident env tok) (* "self" *)
  | `Choice_u8 x -> ([], map_primitive_type_ident env x)
  | `Meta tok -> ([], ident env tok) (* pattern \$[a-zA-Z_]\w* *)
  | `Super tok -> ([], ident env tok) (* "super" *)
  | `Crate tok -> ([], ident env tok) (* "crate" *)
  | `Id tok ->
      ([], ident env tok)
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  | `Simple_scoped_id x -> map_simple_scoped_identifier env x

and map_simple_scoped_identifier (env : env)
    ((v1, v2, v3) : CST.simple_scoped_identifier) : G.dotted_ident * G.ident =
  let path = map_simple_path env v1 in
  let colons = token env v2 (* "::" *) in
  let ident = ident env v3 (* identifier *) in
  (path, ident)

and map_simple_scoped_identifier_name (env : env)
    ((v1, v2, v3) : CST.simple_scoped_identifier) : G.ident * G.name_info =
  let path = map_simple_path env v1 in
  let colons = token env v2 (* "::" *) in
  let ident = ident env v3 (* identifier *) in
  (ident, { G.name_qualifier = Some (G.QDots path); G.name_typeargs = None })

let map_foreign_item_type (env : env) ((v1, v2, v3) : CST.foreign_item_type) :
    G.stmt =
  let type_ = token env v1 (* "type" *) in
  let ident = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let semicolon = token env v3 (* ";" *) in
  let type_def =
    { G.tbody = G.NewType (G.TyN (G.Id (ident, G.empty_id_info ()))) }
  in
  let ent =
    {
      G.name = G.EN (G.Id (ident, G.empty_id_info ()));
      G.attrs = [ G.KeywordAttr (G.Extern, G.fake "extern") ];
      G.tparams = [];
    }
  in
  G.DefStmt (ent, G.TypeDef type_def) |> G.s

let map_lifetime (env : env) ((v1, v2) : CST.lifetime) : lifetime =
  let apostrophe = token env v1 (* "'" *) in
  let id = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  id

let map_loop_label (env : env) ((v1, v2) : CST.loop_label) : G.label_ident =
  let apostophe = token env v1 (* "'" *) in
  let label = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  G.LId label

let map_non_special_token (env : env) (x : CST.non_special_token) :
    PI.token_mutable =
  match x with
  | `Lit x -> map_literal_token env x
  | `Id tok ->
      token env tok
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  | `Meta tok -> token env tok (* pattern \$[a-zA-Z_]\w* *)
  | `Muta_spec tok -> token env tok (* "mut" *)
  | `Self tok -> token env tok (* "self" *)
  | `Super tok -> token env tok (* "super" *)
  | `Crate tok -> token env tok (* "crate" *)
  | `Choice_u8 x ->
      let _, tok = map_primitive_type_ident env x in
      tok
  | `Pat_e14e5d5 tok -> token env tok (*tok*)
  | `SQUOT tok -> token env tok (* "'" *)
  | `As tok -> token env tok (* "as" *)
  | `Async tok -> token env tok (* "async" *)
  | `Await tok -> token env tok (* "await" *)
  | `Brk tok -> token env tok (* "break" *)
  | `Const tok -> token env tok (* "const" *)
  | `Cont tok -> token env tok (* "continue" *)
  | `Defa tok -> token env tok (* "default" *)
  | `Enum tok -> token env tok (* "enum" *)
  | `Fn tok -> token env tok (* "fn" *)
  | `For tok -> token env tok (* "for" *)
  | `If tok -> token env tok (* "if" *)
  | `Impl tok -> token env tok (* "impl" *)
  | `Let tok -> token env tok (* "let" *)
  | `Loop tok -> token env tok (* "loop" *)
  | `Match tok -> token env tok (* "match" *)
  | `Mod tok -> token env tok (* "mod" *)
  | `Pub tok -> token env tok (* "pub" *)
  | `Ret tok -> token env tok (* "return" *)
  | `Static tok -> token env tok (* "static" *)
  | `Struct tok -> token env tok (* "struct" *)
  | `Trait tok -> token env tok (* "trait" *)
  | `Type tok -> token env tok (* "type" *)
  | `Union tok -> token env tok (* "union" *)
  | `Unsafe tok -> token env tok (* "unsafe" *)
  | `Use tok -> token env tok (* "use" *)
  | `Where tok -> token env tok (* "where" *)
  | `While tok -> token env tok

(* "while" *)

let map_function_modifiers (env : env) (xs : CST.function_modifiers) :
    G.attribute list =
  List.map
    (fun x ->
      match x with
      | `Async tok -> [ G.KeywordAttr (G.Async, token env tok) ] (* "async" *)
      | `Defa tok ->
          [ G.KeywordAttr (G.DefaultImpl, token env tok) ] (* "default" *)
      | `Const tok -> [ G.KeywordAttr (G.Const, token env tok) ] (* "const" *)
      | `Unsafe tok ->
          [ G.KeywordAttr (G.Unsafe, token env tok) ] (* "unsafe" *)
      | `Extern_modi x -> map_extern_modifier env x)
    xs
  |> List.flatten

let map_for_lifetimes (env : env) ((v1, v2, v3, v4, v5, v6) : CST.for_lifetimes)
    : lifetime list =
  let for_ = token env v1 (* "for" *) in
  let lthan = token env v2 (* "<" *) in
  let lifetime_first = map_lifetime env v3 in
  let lifetime_rest =
    List.map
      (fun (v1, v2) ->
        let comma = token env v1 (* "," *) in
        let lifetime = map_lifetime env v2 in
        lifetime)
      v4
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) in
  let gthan = token env v6 (* ">" *) in
  lifetime_first :: lifetime_rest

let rec map_token_tree (env : env) (x : CST.token_tree) :
    rust_macro_item list G.bracket =
  match x with
  | `LPAR_rep_choice_tok_tree_RPAR (v1, v2, v3) ->
      let lparen = token env v1 (* "(" *) in
      let tokens = List.map (map_tokens env) v2 in
      let rparen = token env v3 (* ")" *) in
      (lparen, tokens, rparen)
  | `LBRACK_rep_choice_tok_tree_RBRACK (v1, v2, v3) ->
      let lbracket = token env v1 (* "[" *) in
      let tokens = List.map (map_tokens env) v2 in
      let rbracket = token env v3 (* "]" *) in
      (lbracket, tokens, rbracket)
  | `LCURL_rep_choice_tok_tree_RCURL (v1, v2, v3) ->
      let lbrace = token env v1 (* "{" *) in
      let tokens = List.map (map_tokens env) v2 in
      let rbrace = token env v3 (* "}" *) in
      (lbrace, tokens, rbrace)

and map_tokens (env : env) (x : CST.tokens) : rust_macro_item =
  match x with
  | `Tok_tree x -> MacTkTree (map_token_tree env x)
  | `Tok_repe (v1, v2, v3, v4, v5, v6) ->
      let dollar = token env v1 (* "$" *) in
      let lparen = token env v2 (* "(" *) in
      let tokens = List.map (map_tokens env) v3 in
      let rparen = token env v4 (* ")" *) in
      let ident = Option.map (fun tok -> ident env tok) v5 in
      (* pattern [^+*?]+ *)
      let quantifier = map_token_quantifier env v6 in
      MacTks ((lparen, tokens, rparen), ident, quantifier)
  | `Choice_lit x -> Tk (map_non_special_token env x)

let rec map_token_pattern (env : env) (x : CST.token_pattern) :
    rust_macro_pattern =
  match x with
  | `Tok_tree_pat x -> RustMacPatTree (map_token_tree_pattern env x)
  | `Tok_repe_pat (v1, v2, v3, v4, v5, v6) ->
      let dollar = token env v1 (* "$" *) in
      let lparen = token env v2 (* "(" *) in
      let patterns = List.map (map_token_pattern env) v3 in
      let rparen = token env v4 (* ")" *) in
      let ident = Option.map (fun tok -> ident env tok) v5 in
      (* pattern [^+*?]+ *)
      let quantifier = map_token_quantifier env v6 in
      RustMacPatRepetition ((lparen, patterns, rparen), ident, quantifier)
  | `Tok_bind_pat (v1, v2, v3) ->
      let ident = ident env v1 (* pattern \$[a-zA-Z_]\w* *) in
      let colon = token env v2 (* ":" *) in
      let fragment_specifier = map_fragment_specifier env v3 in
      RustMacPatBinding (ident, fragment_specifier)
  | `Choice_lit x -> RustMacPatToken (map_non_special_token env x)

and map_token_tree_pattern (env : env) (x : CST.token_tree_pattern) :
    rust_macro_pattern list =
  match x with
  | `LPAR_rep_tok_pat_RPAR (v1, v2, v3) ->
      let lparen = token env v1 (* "(" *) in
      let patterns = List.map (map_token_pattern env) v2 in
      let rparen = token env v3 (* ")" *) in
      patterns
  | `LBRACK_rep_tok_pat_RBRACK (v1, v2, v3) ->
      let lbracket = token env v1 (* "[" *) in
      let patterns = List.map (map_token_pattern env) v2 in
      let rbracket = token env v3 (* "]" *) in
      patterns
  | `LCURL_rep_tok_pat_RCURL (v1, v2, v3) ->
      let lbrace = token env v1 (* "{" *) in
      let patterns = List.map (map_token_pattern env) v2 in
      let rbrace = token env v3 (* "}" *) in
      patterns

and map_macro_rule (env : env) ((v1, v2, v3) : CST.macro_rule) : rust_macro_rule
    =
  let rules = map_token_tree_pattern env v1 in
  let arrow = token env v2 (* "=>" *) in
  let body = map_token_tree env v3 in
  { rules; body }

let rec map_abstract_type_trait_name (env : env)
    (x : CST.anon_choice_type_id_02b4436) : G.type_ =
  match x with
  | `Id tok ->
      let ident = ident env tok in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      G.TyN (G.Id (ident, G.empty_id_info ()))
  | `Scoped_type_id x -> map_scoped_type_identifier env x
  | `Gene_type x ->
      let name = map_generic_type_name env x in
      G.TyN (G.IdQualified (name, G.empty_id_info ()))
  | `Func_type x -> map_function_type env x

(* TODO: return AST_generic.name *)
and map_struct_name (env : env) (x : CST.anon_choice_type_id_2c46bcf) :
    G.ident * G.name_info =
  match x with
  | `Id tok ->
      name_of_id (ident env tok)
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  | `Scoped_type_id x -> map_scoped_type_identifier_name env x

(* TODO: return AST_generic.name *)
and map_tuple_struct_name (env : env) (x : CST.anon_choice_type_id_f1f5a37) :
    G.ident * G.name_info =
  match x with
  | `Id tok ->
      name_of_id (ident env tok)
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  | `Scoped_id x -> map_scoped_identifier_name env x

and map_struct_pattern_field (env : env) (x : CST.anon_choice_field_pat_8e757e8)
    : G.dotted_ident * G.pattern =
  match x with
  | `Field_pat (v1, v2, v3) -> (
      let ref_ = Option.map (fun tok -> token env tok) in
      (* "ref" *)
      let mutability_attr =
        Option.map
          (fun tok ->
            let t = token env tok in
            (* "mut" *)
            G.KeywordAttr (G.Mutable, t))
          v2
      in
      match v3 with
      | `Id tok ->
          let ident = ident env tok in
          (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
          ([ ident ], G.PatId (ident, G.empty_id_info ()))
      | `Id_COLON_pat (v1, v2, v3) ->
          let ident = ident env v1 in
          (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
          let colon = token env v2 (* ":" *) in
          let pat = map_pattern env v3 in
          ([ ident ], pat) )
  | `Rema_field_pat tok ->
      let ident = ident env tok in
      (* ".." *)
      let name = [ ident ] in
      (name, G.OtherPat (G.OP_Todo, [ G.Tk (token env tok) ]))

and map_type_parameter (env : env) (x : CST.anon_choice_life_859e88f) :
    G.type_parameter =
  match x with
  | `Life x ->
      let lt = map_lifetime env x in
      (lt, [ G.OtherTypeParam (G.OTP_Lifetime, []) ])
  | `Meta tok ->
      let meta = ident env tok in
      (* pattern \$[a-zA-Z_]\w* *)
      (meta, [ G.OtherTypeParam (G.OTP_Ident, []) ])
  | `Id tok ->
      let ident = ident env tok in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      (ident, [ G.OtherTypeParam (G.OTP_Ident, []) ])
  | `Cons_type_param x -> map_constrained_type_parameter env x
  | `Opt_type_param (v1, v2, v3) ->
      let type_param =
        match v1 with
        | `Id tok ->
            let ident = ident env tok in
            (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
            (ident, [ G.OtherTypeParam (G.OTP_Ident, []) ])
        | `Cons_type_param x -> map_constrained_type_parameter env x
      in
      let equal = token env v2 (* "=" *) in
      let default_ty = map_type_ env v3 in
      type_param
  | `Const_param (v1, v2, v3, v4) ->
      let const = token env v1 in
      (* "const" *)
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let colon = token env v3 in
      (* ":" *)
      let ty = map_type_ env v4 in
      (ident, [ G.OtherTypeParam (G.OTP_Const, [ G.T ty ]) ])

and dotted_ident_of_name_ (n : G.ident * G.name_info) : G.dotted_ident =
  match n with (* TODO, look QDots too *)
  | id, _ -> [ id ]

and map_range_pattern_bound (env : env) (x : CST.anon_choice_lit_pat_0884ef0) :
    G.pattern =
  match x with
  | `Lit_pat x -> map_literal_pattern env x
  | `Choice_self x ->
      let name = map_path_name env x in
      let dotted = dotted_ident_of_name_ name in
      G.PatConstructor (dotted, [])

and map_meta_argument (env : env) (x : CST.anon_choice_meta_item_fefa160) :
    rust_meta_argument =
  match x with
  | `Meta_item x -> MetaArgMetaItem (map_meta_item env x)
  | `Lit x -> MetaArgLiteral (map_literal env x)

and map_anon_choice_param_2c23cdc (env : env) outer_attr
    (x : CST.anon_choice_param_2c23cdc) : G.parameter =
  match x with
  | `Param x -> map_parameter env x
  | `Self_param (v1, v2, v3, v4) ->
      let borrow = Option.map (fun tok -> token env tok (* "&" *)) v1 in
      let lifetime = Option.map (fun x -> map_lifetime env x) v2 in
      let mutability =
        Option.map
          (fun tok ->
            let t = token env tok in
            (* "mut" *)
            G.KeywordAttr (G.Mutable, t))
          v3
      in
      let self = ident env v4 (* "self" *) in
      let self_type = G.TyN (G.Id (self, G.empty_id_info ())) in
      let type_ =
        match borrow with
        | Some tok -> G.TyRef (tok, self_type)
        | None -> self_type
      in
      let attrs = deoptionalize [ mutability ] in
      let param =
        {
          G.pname = Some self;
          pdefault = None;
          ptype = Some type_;
          pattrs = attrs;
          pinfo = G.empty_id_info ();
        }
      in
      G.ParamClassic param
  | `Vari_param tok -> G.ParamEllipsis (token env tok) (* "..." *)
  | `X__ tok ->
      (* ellided parameter *)
      G.ParamPattern (G.PatUnderscore (token env tok))
  | `Type x ->
      let ty = map_type_ env x in
      let param =
        {
          G.pname = None;
          G.ptype = Some ty;
          G.pdefault = None;
          G.pattrs = [];
          G.pinfo = G.empty_id_info ();
        }
      in
      G.ParamClassic param

and map_closure_parameter (env : env) (x : CST.anon_choice_pat_4717dcc) :
    G.parameter =
  match x with
  | `Pat x ->
      let pattern = map_pattern env x in
      G.ParamPattern pattern
  | `Param x -> map_parameter env x

and map_field_initializer (env : env)
    (x : CST.anon_choice_shor_field_init_9cb4441) : G.expr =
  match x with
  | `Shor_field_init (v1, v2) ->
      let outer_attrs = List.map (map_outer_attribute_item env) v1 in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let lhs = G.N (G.Id (ident, G.empty_id_info ())) in
      (* bound variable with same ident as field name *)
      let rhs = G.N (G.Id (ident, G.empty_id_info ())) in
      G.Assign (lhs, G.fake ":", rhs)
  | `Field_init (v1, v2, v3, v4) ->
      let outer_attrs = List.map (map_outer_attribute_item env) v1 in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let lhs = G.N (G.Id (ident, G.empty_id_info ())) in
      let colon = token env v3 (* ":" *) in
      let rhs = map_expression env v4 in
      G.Assign (lhs, colon, rhs)
  | `Base_field_init x -> map_base_field_initializer env x

and map_type_argument (env : env) (x : CST.anon_choice_type_39799c3) :
    G.type_argument =
  match x with
  | `Type x -> G.TypeArg (map_type_ env x)
  | `Type_bind (v1, v2, v3) ->
      (* TODO *)
      let ident = ident env v1 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let equals = token env v2 (* "=" *) in
      let ty = map_type_ env v3 in
      G.TypeArg ty
  | `Life x -> G.TypeLifetime (map_lifetime env x)
  | `Lit x ->
      let lit = map_literal env x in
      G.OtherTypeArg (G.OTA_Literal, [ G.E (G.L lit) ])
  | `Blk x ->
      let block_expr = map_block_expr env x in
      G.OtherTypeArg (G.OTA_ConstBlock, [ G.E block_expr ])

and map_tuple_pattern_list (env : env)
    ((v1, v2) : CST.anon_pat_rep_COMMA_pat_2a80f16) : G.pattern list =
  let pattern_first = map_pattern env v1 in
  let pattern_rest =
    List.map
      (fun (v1, v2) ->
        let comma = token env v1 (* "," *) in
        let pattern = map_pattern env v2 in
        pattern)
      v2
  in
  pattern_first :: pattern_rest

and map_arguments (env : env) ((v1, v2, v3, v4) : CST.arguments) :
    G.arguments G.bracket =
  let lparen = token env v1 (* "(" *) in
  let args =
    match v2 with
    | Some (v1, v2, v3) ->
        let outer_attrs = List.map (map_outer_attribute_item env) v1 in
        let expr_first = G.Arg (map_expression env v2) in
        let expr_rest =
          List.map
            (fun (v1, v2, v3) ->
              let comma = token env v1 (* "," *) in
              let outer_attrs = List.map (map_outer_attribute_item env) v2 in
              let expr = map_expression env v3 in
              G.Arg expr)
            v3
        in
        expr_first :: expr_rest
    | None -> []
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) in
  let rparen = token env v4 (* ")" *) in
  (lparen, args, rparen)

and map_associated_type (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.associated_type) : G.stmt =
  let type_ = token env v1 (* "type" *) in
  let ident = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let type_params =
    match v3 with Some x -> map_type_parameters env x | None -> []
  in
  let trait_bounds =
    match v4 with Some x -> map_trait_bounds env x | None -> []
  in
  let ty =
    Option.map
      (fun (v1, v2) ->
        let equals = token env v1 (* "=" *) in
        let ty = map_type_ env v2 in
        ty)
      v5
  in
  let semicolon = token env v6 (* ";" *) in
  let type_def_kind =
    match ty with
    | Some ty -> G.AliasType ty
    | None -> G.OtherTypeKind (G.OTKO_AbstractType, [])
  in
  let type_def = { G.tbody = type_def_kind } in
  let ent =
    {
      G.name = G.EN (G.Id (ident, G.empty_id_info ()));
      G.attrs = [];
      G.tparams = type_params;
    }
  in
  G.DefStmt (ent, G.TypeDef type_def) |> G.s

and map_attribute (env : env) ((v1, v2, v3) : CST.attribute) : rust_meta_item =
  let lbracket = token env v1 (* "[" *) in
  let meta_item = map_meta_item env v2 in
  let rbracket = token env v3 (* "]" *) in
  meta_item

and map_base_field_initializer (env : env)
    ((v1, v2) : CST.base_field_initializer) : G.expr =
  let dots = token env v1 (* ".." *) in
  let lhs = G.IdSpecial (G.Spread, dots) in
  (* Copy remaining struct fields from this existing instance *)
  let rhs = map_expression env v2 in
  G.AssignOp (lhs, (G.Append, dots), rhs)

and map_binary_expression (env : env) (x : CST.binary_expression) : G.expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.And, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Or, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitAnd, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitOr, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitXor, v2), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_choice_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let tok, op =
        match v2 with
        | `EQEQ tok -> (token env tok, G.Eq) (* "==" *)
        | `BANGEQ tok -> (token env tok, G.NotEq) (* "!=" *)
        | `LT tok -> (token env tok, G.Lt) (* "<" *)
        | `LTEQ tok -> (token env tok, G.LtE) (* "<=" *)
        | `GT tok -> (token env tok, G.Gt) (* ">" *)
        | `GTEQ tok -> (token env tok, G.GtE)
        (* ">=" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op op, tok), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_choice_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let tok, op =
        match v2 with
        | `LTLT tok -> (token env tok, G.LSL) (* "<<" *)
        (* According to https://doc.rust-lang.org/reference/expressions/operator-expr.html#arithmetic-and-logical-binary-operators: *)
        (* "Arithmetic right shift on signed integer types, logical right shift on unsigned integer types." *)
        | `GTGT tok -> (token env tok, G.LSR)
        (* ">>" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op op, tok), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let tok, op =
        match v2 with
        | `PLUS tok -> (token env tok, G.Plus) (* "+" *)
        | `DASH tok -> (token env tok, G.Minus)
        (* "-" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op op, tok), fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let tok, op =
        match v2 with
        | `STAR tok -> (token env tok, G.Mult) (* "*" *)
        | `SLASH tok -> (token env tok, G.Div) (* "/" *)
        | `PERC tok -> (token env tok, G.Mod)
        (* "%" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op op, tok), fb [ G.Arg v1; G.Arg v3 ])

and map_block (env : env) ((v1, v2, v3, v4) : CST.block) : G.stmt =
  let lbrace = token env v1 (* "{" *) in
  let stmts = List.map (map_statement env) v2 |> List.flatten in
  let stmts_and_expr =
    match v3 with
    | Some x ->
        let expr = map_expression env x in
        let stmt = G.ExprStmt (expr, sc) |> G.s in
        List.concat [ stmts; [ stmt ] ]
    | None -> stmts
  in
  let rbrace = token env v4 (* "}" *) in
  G.Block (lbrace, stmts, rbrace) |> G.s

and map_block_expr (env : env) ((v1, v2, v3, v4) : CST.block) : G.expr =
  let block = map_block env (v1, v2, v3, v4) in
  stmt_to_expr block

and map_bounded_type (env : env) (x : CST.bounded_type) : G.type_ =
  match x with
  | `Life_PLUS_type (v1, v2, v3) ->
      let lifetime = map_lifetime env v1 in
      let plus = token env v2 (* "+" *) in
      let type_ = map_type_ env v3 in
      G.TyOr (G.OtherType (G.OT_Lifetime, [ G.I lifetime ]), plus, type_)
  | `Type_PLUS_type (v1, v2, v3) ->
      let type_a = map_type_ env v1 in
      let plus = token env v2 (* "+" *) in
      let type_b = map_type_ env v3 in
      G.TyOr (type_a, plus, type_b)
  | `Type_PLUS_life (v1, v2, v3) ->
      let type_ = map_type_ env v1 in
      let plus = token env v2 (* "+" *) in
      let lifetime = map_lifetime env v3 in
      G.TyOr (type_, plus, G.OtherType (G.OT_Lifetime, [ G.I lifetime ]))

and map_bracketed_type (env : env) ((v1, v2, v3) : CST.bracketed_type) =
  let lthan = token env v1 (* "<" *) in
  let ty =
    match v2 with
    | `Type x -> map_type_ env x
    | `Qual_type x -> map_qualified_type env x
  in
  let gthan = token env v3 (* ">" *) in
  (lthan, ty, gthan)

and map_closure_parameters (env : env) ((v1, v2, v3) : CST.closure_parameters) :
    G.parameter list =
  let lpipe = token env v1 (* "|" *) in
  let params =
    match v2 with
    | Some (v1, v2) ->
        let param_first = map_closure_parameter env v1 in
        let param_rest =
          List.map
            (fun (v1, v2) ->
              let comma = token env v1 (* "," *) in
              let param = map_closure_parameter env v2 in
              param)
            v2
        in
        param_first :: param_rest
    | None -> []
  in
  let rpipe = token env v3 (* "|" *) in
  params

and map_const_block (env : env) ((v1, v2) : CST.const_block) : G.expr =
  let const = token env v1 (* "const" *) in
  let block = map_block env v2 in
  let stmt = G.OtherStmtWithStmt (G.OSWS_ConstBlock, None, block) |> G.s in
  stmt_to_expr stmt

and map_const_item (env : env) ((v1, v2, v3, v4, v5, v6) : CST.const_item) :
    G.stmt =
  let const = token env v1 (* "const" *) in
  let attrs = [ G.KeywordAttr (G.Const, const) ] in
  let ident = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let colon = token env v3 (* ":" *) in
  let type_ = map_type_ env v4 in
  let init =
    Option.map
      (fun (v1, v2) ->
        let equals = token env v1 (* "=" *) in
        let expr = map_expression env v2 in
        expr)
      v5
  in
  let semicolon = token env v6 (* ";" *) in
  let var_def = { G.vinit = init; G.vtype = Some type_ } in
  let ent =
    {
      G.name = G.EN (G.Id (ident, G.empty_id_info ()));
      G.attrs;
      G.tparams = [];
    }
  in
  G.DefStmt (ent, G.VarDef var_def) |> G.s

and map_constrained_type_parameter (env : env)
    ((v1, v2) : CST.constrained_type_parameter) : G.type_parameter =
  let ident =
    match v1 with `Life x -> map_lifetime env x | `Id tok -> ident env tok
    (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  in
  let bounds = map_trait_bounds env v2 in
  (ident, [ G.OtherTypeParam (G.OTP_Constrained, []) ])

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) : G.stmt =
  let else_ = token env v1 (* "else" *) in
  match v2 with
  | `Blk x ->
      (* plain else *)
      map_block env x
  | `If_exp x ->
      (* else if *)
      let if_expr = map_if_expression env x in
      G.ExprStmt (if_expr, sc) |> G.s
  | `If_let_exp x ->
      (* else if let Some(...) = x *)
      let if_let_expr = map_if_let_expression env x in
      G.ExprStmt (if_let_expr, sc) |> G.s

and map_enum_variant (env : env) ((v1, v2, v3, v4) : CST.enum_variant) :
    G.or_type_element =
  let visibility =
    match v1 with Some x -> map_visibility_modifier env x | None -> []
  in
  let ident = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let init =
    Option.map
      (fun (v1, v2) ->
        let equals = token env v1 (* "=" *) in
        let expr = map_expression env v2 in
        expr)
      v4
  in
  match v3 with
  | Some x ->
      let types =
        match x with
        | `Field_decl_list x -> map_field_declaration_list_types env x
        | `Orde_field_decl_list x ->
            map_ordered_field_declaration_list_types env x
      in
      G.OrConstructor (ident, types)
  | None -> G.OrEnum (ident, init)

and map_enum_variant_list (env : env) ((v1, v2, v3, v4) : CST.enum_variant_list)
    : G.type_definition_kind =
  let lbrace = token env v1 (* "{" *) in
  let variants =
    match v2 with
    | Some (v1, v2, v3) ->
        let outer_attributes = List.map (map_outer_attribute_item env) v1 in
        let variant_first = map_enum_variant env v2 in
        let variant_rest =
          List.map
            (fun (v1, v2, v3) ->
              let comma = token env v1 (* "," *) in
              let outer_attributes =
                List.map (map_outer_attribute_item env) v2
              in
              let variant = map_enum_variant env v3 in
              variant)
            v3
        in
        variant_first :: variant_rest
    | None -> []
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) v3 in
  let rbrace = token env v4 (* "}" *) in
  G.OrType variants

and map_expression (env : env) (x : CST.expression) =
  match x with
  | `Un_exp (v1, v2) -> (
      let expr = map_expression env v2 in
      match v1 with
      | `DASH tok ->
          let tok = token env tok in
          (* "-" *)
          G.Call (G.IdSpecial (G.Op G.Minus, tok), fb [ G.Arg expr ])
      | `STAR tok ->
          let tok = token env tok in
          (* "*" *)
          G.DeRef (tok, expr)
      | `BANG tok ->
          let tok = token env tok in
          (* "!" *)
          G.Call (G.IdSpecial (G.Op G.Not, tok), fb [ G.Arg expr ]) )
  | `Ref_exp (v1, v2, v3) ->
      let ref_ = token env v1 (* "&" *) in
      let mutability =
        Option.map
          (fun tok ->
            let tok = token env tok (* "mut" *) in
            G.KeywordAttr (G.Mutable, tok))
          v2
      in
      let expr = map_expression env v3 in
      G.Ref (ref_, expr)
  | `Try_exp (v1, v2) ->
      let expr = map_expression env v1 in
      let question = token env v2 (* "?" *) in
      G.Call (G.IdSpecial (G.Op G.Elvis, question), fb [ G.Arg expr ])
  | `Bin_exp x -> map_binary_expression env x
  | `Assign_exp (v1, v2, v3) ->
      let lhs = map_expression env v1 in
      let equals = token env v2 (* "=" *) in
      let rhs = map_expression env v3 in
      G.Assign (lhs, equals, rhs)
  | `Comp_assign_expr (v1, v2, v3) ->
      let lhs = map_expression env v1 in
      let op, tok =
        match v2 with
        | `PLUSEQ tok -> (G.Plus, token env tok) (* "+=" *)
        | `DASHEQ tok -> (G.Minus, token env tok) (* "-=" *)
        | `STAREQ tok -> (G.Mult, token env tok) (* "*=" *)
        | `SLASHEQ tok -> (G.Div, token env tok) (* "/=" *)
        | `PERCEQ tok -> (G.Mod, token env tok) (* "%=" *)
        | `AMPEQ tok -> (G.BitAnd, token env tok) (* "&=" *)
        | `BAREQ tok -> (G.BitOr, token env tok) (* "|=" *)
        | `HATEQ tok -> (G.BitXor, token env tok) (* "^=" *)
        | `LTLTEQ tok -> (G.LSL, token env tok) (* "<<=" *)
        (* According to https://doc.rust-lang.org/reference/expressions/operator-expr.html#arithmetic-and-logical-binary-operators: *)
        (* "Arithmetic right shift on signed integer types, logical right shift on unsigned integer types." *)
        | `GTGTEQ tok -> (G.LSR, token env tok)
        (* ">>=" *)
      in
      let rhs = map_expression env v3 in
      G.AssignOp (lhs, (op, tok), rhs)
  | `Type_cast_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let as_ = token env v2 (* "as" *) in
      let type_ = map_type_ env v3 in
      G.Cast (type_, expr)
  | `Range_exp x -> map_range_expression env x
  | `Call_exp (v1, v2) ->
      let expr = map_expression env v1 in
      let args = map_arguments env v2 in
      G.Call (expr, args)
  | `Ret_exp x -> map_return_expression env x
  | `Lit x -> G.L (map_literal env x)
  | `Id tok ->
      G.N (G.Id (ident env tok, G.empty_id_info ()))
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  | `Choice_u8 x ->
      let tok = map_primitive_type_token env x in
      G.N (G.Id (ident env tok, G.empty_id_info ()))
  | `Choice_defa x ->
      let ident = map_reserved_identifier env x in
      G.N (G.Id (ident, G.empty_id_info ()))
  | `Self tok -> G.IdSpecial (G.Self, token env tok) (* "self" *)
  | `Scoped_id x -> map_scoped_identifier env x
  | `Gene_func (v1, v2, v3) -> (
      let colons = token env v2 (* "::" *) in
      let typeargs = map_type_arguments env v3 in
      match v1 with
      | `Id tok ->
          let ident = ident env tok in
          (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
          let name =
            (ident, { G.name_qualifier = None; G.name_typeargs = Some typeargs })
          in
          G.N (G.IdQualified (name, G.empty_id_info ()))
      | `Scoped_id x ->
          let id_qualified = map_scoped_identifier env x in
          let ident, name_qualifier =
            match id_qualified with
            | G.N
                (G.IdQualified
                  ((ident, { name_qualifier; name_typeargs = None; _ }), _)) ->
                (ident, name_qualifier)
            | _ -> raise Impossible
          in
          (* TODO is this correct? *)
          let name =
            (ident, { G.name_qualifier; G.name_typeargs = Some typeargs })
          in
          G.N (G.IdQualified (name, G.empty_id_info ()))
      | `Field_exp x -> map_field_expression env x (Some typeargs) )
  | `Await_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let dot = token env v2 (* "." *) in
      let await = token env v3 (* "await" *) in
      G.Await (await, expr)
  | `Field_exp x -> map_field_expression env x None
  | `Array_exp (v1, v2, v3, v4) ->
      let lbracket = token env v1 (* "[" *) in
      let outer_attrs = List.map (map_outer_attribute_item env) v2 in
      let exprs =
        match v3 with
        | `Exp_SEMI_exp (v1, v2, v3) ->
            let ty = map_expression env v1 in
            let semicolon = token env v2 (* ";" *) in
            let init = map_expression env v3 in
            [ ty; init ]
            (* TODO? *)
        | `Opt_exp_rep_COMMA_exp_opt_COMMA (v1, v2) ->
            let exprs =
              match v1 with
              | Some (v1, v2) ->
                  let expr_first = map_expression env v1 in
                  let expr_rest =
                    List.map
                      (fun (v1, v2) ->
                        let comma = token env v1 (* "," *) in
                        let expr = map_expression env v2 in
                        expr)
                      v2
                  in
                  expr_first :: expr_rest
              | None -> []
            in
            let comma = Option.map (fun tok -> token env tok (* "," *)) v2 in
            exprs
      in
      let rbracket = token env v4 (* "]" *) in
      G.Container (G.Array, (lbracket, exprs, rbracket))
  | `Tuple_exp (v1, v2, v3, v4, v5, v6, v7) ->
      let lparen = token env v1 (* "(" *) in
      let outer_attrs = List.map (map_outer_attribute_item env) v2 in
      let expr_first = map_expression env v3 in
      let comma = token env v4 (* "," *) in
      let expr_rest =
        List.map
          (fun (v1, v2) ->
            let expr = map_expression env v1 in
            let comma = token env v2 (* "," *) in
            expr)
          v5
      in
      let expr_last =
        match v6 with Some x -> [ map_expression env x ] | None -> []
      in
      let rparen = token env v7 (* ")" *) in
      let exprs = List.concat [ [ expr_first ]; expr_rest; expr_last ] in
      G.Tuple (lparen, exprs, rparen)
  | `Macro_invo x -> map_macro_invocation env x
  | `Unit_exp (v1, v2) ->
      let lparen = token env v1 (* "(" *) in
      let rparen = token env v2 (* ")" *) in
      G.L (G.Unit lparen)
  | `Choice_unsafe_blk x -> map_expression_ending_with_block env x
  | `Brk_exp (v1, v2, v3) ->
      let break = token env v1 (* "break" *) in
      let label =
        match v2 with Some x -> map_loop_label env x | None -> G.LNone
      in
      let expr = Option.map (fun x -> map_expression env x) v3 in
      let break_stmt = G.Break (break, label, sc) |> G.s in
      (* TODO expr *)
      stmt_to_expr break_stmt
  | `Cont_exp (v1, v2) ->
      let continue = token env v1 (* "continue" *) in
      let label =
        match v2 with Some x -> map_loop_label env x | None -> G.LNone
      in
      let continue_stmt = G.Continue (continue, label, sc) |> G.s in
      stmt_to_expr continue_stmt
  | `Index_exp (v1, v2, v3, v4) ->
      let expr = map_expression env v1 in
      let lbracket = token env v2 (* "[" *) in
      let index = map_expression env v3 in
      let rbracket = token env v4 (* "]" *) in
      G.ArrayAccess (expr, (lbracket, index, rbracket))
  | `Meta tok ->
      let meta = ident env tok in
      (* pattern \$[a-zA-Z_]\w* *)
      G.N (G.Id (meta, G.empty_id_info ()))
  | `Clos_exp (v1, v2, v3) ->
      let is_move =
        Option.map
          (fun tok ->
            let tok = token env tok in
            (* "move" *)
            G.KeywordAttr (G.Mutable, tok))
          v1
      in
      let params = map_closure_parameters env v2 in
      let ret_type, body =
        match v3 with
        | `Opt_DASHGT_type_blk (v1, v2) ->
            let ret_type =
              Option.map
                (fun (v1, v2) ->
                  let arrow = token env v1 (* "->" *) in
                  let ty = map_type_ env v2 in
                  ty)
                v1
            in
            let body = map_block env v2 in
            (ret_type, body)
        | `Exp x ->
            let expr = map_expression env x in
            (None, G.ExprStmt (expr, sc) |> G.s)
      in
      let func_def =
        {
          G.fkind = (G.LambdaKind, G.fake "closure");
          G.fparams = params;
          G.frettype = ret_type;
          G.fbody = body;
        }
      in
      G.Lambda func_def
  | `Paren_exp (v1, v2, v3) ->
      let lparen = token env v1 (* "(" *) in
      let expr = map_expression env v2 in
      let rparen = token env v3 (* ")" *) in
      expr
  | `Struct_exp (v1, v2) ->
      let name : G.ident * G.name_info =
        match v1 with
        | `Id tok ->
            let ident = ident env tok in
            (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
            (ident, G.empty_name_info)
        | `Scoped_type_id_in_exp_posi x ->
            map_scoped_type_identifier_in_expression_position env x
        | `Gene_type_with_turb x -> map_generic_type_with_turbofish env x
      in
      let fields = map_field_initializer_list env v2 in
      G.Constructor (dotted_ident_of_name_ name, fields)
  | `Ellips tok -> G.Ellipsis (token env tok) (* "..." *)
  | `Deep_ellips (v1, v2, v3) ->
      let lellips = token env v1 (* "<..." *) in
      let expr = map_expression env v2 in
      let rellips = token env v3 (* "...>" *) in
      G.DeepEllipsis (lellips, expr, rellips)

and map_expression_ending_with_block (env : env)
    (x : CST.expression_ending_with_block) : G.expr =
  let map_loop_label_ (v1, v2) =
    let loop_label = map_loop_label env v1 in
    let colon = token env v2 (* ":" *) in
    loop_label
  in
  match x with
  | `Unsafe_blk (v1, v2) ->
      let unsafe = token env v1 (* "unsafe" *) in
      let block = map_block env v2 in
      let stmt = G.OtherStmtWithStmt (G.OSWS_UnsafeBlock, None, block) |> G.s in
      stmt_to_expr stmt
  | `Async_blk (v1, v2, v3) ->
      let async = token env v1 (* "async" *) in
      let move = Option.map (fun tok -> token env tok (* "move" *)) v2 in
      let block = map_block env v3 in
      let stmt = G.OtherStmtWithStmt (G.OSWS_AsyncBlock, None, block) |> G.s in
      stmt_to_expr stmt
  | `Blk x -> map_block_expr env x
  | `If_exp x -> map_if_expression env x
  | `If_let_exp x -> map_if_let_expression env x
  | `Match_exp (v1, v2, v3) ->
      let match_ = token env v1 (* "match" *) in
      let expr = map_expression env v2 in
      let actions = map_match_block env v3 in
      G.MatchPattern (expr, actions)
  | `While_exp (v1, v2, v3, v4) ->
      let loop_label = Option.map map_loop_label_ v1 in
      let while_ = token env v2 (* "while" *) in
      let cond = map_expression env v3 in
      let body = map_block env v4 in
      let while_stmt = G.While (while_, cond, body) |> G.s in
      stmt_to_expr while_stmt
  | `While_let_exp (v1, v2, v3, v4, v5, v6, v7) ->
      let loop_label = Option.map map_loop_label_ v1 in
      let while_ = token env v2 (* "while" *) in
      let let_ = token env v3 (* "let" *) in
      let pattern = map_pattern env v4 in
      let equals = token env v5 (* "=" *) in
      let cond = map_expression env v6 in
      let body = map_block env v7 in
      let while_stmt = G.While (while_, cond, body) |> G.s in
      let expr = G.OtherExpr (G.OE_StmtExpr, [ G.P pattern; G.S while_stmt ]) in
      G.LetPattern (pattern, expr)
  | `Loop_exp (v1, v2, v3) ->
      let loop_label = Option.map map_loop_label_ v1 in
      let loop = token env v2 (* "loop" *) in
      let cond = G.L (G.Bool (true, G.fake "true")) in
      (* dummy, acts as 'while true' *)
      let body = map_block env v3 in
      let loop_stmt = G.While (loop, cond, body) |> G.s in
      stmt_to_expr loop_stmt
  | `For_exp (v1, v2, v3, v4, v5, v6) ->
      let loop_label = Option.map map_loop_label_ v1 in
      let for_ = token env v2 (* "for" *) in
      let pattern = map_pattern env v3 in
      let in_ = token env v4 (* "in" *) in
      let expr = map_expression env v5 in
      let body = map_block env v6 in
      let for_header = G.ForEach (pattern, in_, expr) in
      let for_stmt = G.For (for_, for_header, body) |> G.s in
      stmt_to_expr for_stmt
  | `Const_blk x -> map_const_block env x

and map_expression_statement (env : env) (x : CST.expression_statement) : G.stmt
    =
  match x with
  | `Choice_exp_SEMI x -> (
      match x with
      | `Exp_SEMI (v1, v2) ->
          let expr = map_expression env v1 in
          let semicolon = token env v2 (* ";" *) in
          G.ExprStmt (expr, semicolon) |> G.s
      | `Choice_unsafe_blk x ->
          let expr = map_expression_ending_with_block env x in
          G.ExprStmt (expr, sc) |> G.s )
  | `Ellips_SEMI (v1, v2) ->
      let ellipsis = token env v1 (* "..." *) in
      let sc = token env v2 (* ";" *) in
      let expr = G.Ellipsis ellipsis in
      G.ExprStmt (expr, sc) |> G.s
  | `Ellips tok ->
      let ellipsis = token env tok in
      (* "..." *)
      let expr = G.Ellipsis ellipsis in
      G.ExprStmt (expr, sc) |> G.s

and map_field_declaration (env : env) ((v1, v2, v3, v4) : CST.field_declaration)
    : G.field =
  let attrs =
    match v1 with Some x -> map_visibility_modifier env x | None -> []
  in
  let ident = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let colon = token env v3 (* ":" *) in
  let ty = map_type_ env v4 in
  let var_def = { G.vinit = None; G.vtype = Some ty } in
  let ent =
    {
      G.name = G.EN (G.Id (ident, G.empty_id_info ()));
      G.attrs;
      G.tparams = [];
    }
  in
  G.FieldStmt (G.DefStmt (ent, G.FieldDefColon var_def) |> G.s)

(* for struct definition *)
and map_field_declaration_list (env : env)
    ((v1, v2, v3, v4) : CST.field_declaration_list) : G.field list G.bracket =
  let lbrace = token env v1 (* "{" *) in
  let fields =
    match v2 with
    | Some (v1, v2, v3) ->
        let outer_attrs = List.map (map_outer_attribute_item env) v1 in
        let field_first = map_field_declaration env v2 in
        let field_rest =
          List.map
            (fun (v1, v2, v3) ->
              let comma = token env v1 (* "," *) in
              let outer_attrs = List.map (map_outer_attribute_item env) v2 in
              let field = map_field_declaration env v3 in
              field)
            v3
        in
        field_first :: field_rest
    | None -> []
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) v3 in
  let rbrace = token env v4 (* "}" *) in
  (lbrace, fields, rbrace)

and map_field_declaration_type (env : env)
    ((v1, v2, v3, v4) : CST.field_declaration) : G.type_ =
  let attrs =
    match v1 with Some x -> map_visibility_modifier env x | None -> []
  in
  let ident = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let colon = token env v3 (* ":" *) in
  let ty = map_type_ env v4 in
  ty

(* for enum definition (OrConstructor) *)
and map_field_declaration_list_types (env : env)
    ((v1, v2, v3, v4) : CST.field_declaration_list) : G.type_ list =
  let lbrace = token env v1 (* "{" *) in
  let types =
    match v2 with
    | Some (v1, v2, v3) ->
        let outer_attrs = List.map (map_outer_attribute_item env) v1 in
        let type_first = map_field_declaration_type env v2 in
        let type_rest =
          List.map
            (fun (v1, v2, v3) ->
              let comma = token env v1 (* "," *) in
              let outer_attrs = List.map (map_outer_attribute_item env) v2 in
              let ty = map_field_declaration_type env v3 in
              ty)
            v3
        in
        type_first :: type_rest
    | None -> []
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) v3 in
  let rbrace = token env v4 (* "}" *) in
  types

and map_field_declaration_union (env : env)
    ((v1, v2, v3, v4) : CST.field_declaration) : G.or_type_element =
  let attrs =
    match v1 with Some x -> map_visibility_modifier env x | None -> []
  in
  let ident = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let colon = token env v3 (* ":" *) in
  let ty = map_type_ env v4 in
  G.OrUnion (ident, ty)

(* for union definition *)
and map_field_declaration_list_union (env : env)
    ((v1, v2, v3, v4) : CST.field_declaration_list) : G.or_type_element list =
  let lbrace = token env v1 (* "{" *) in
  let fields =
    match v2 with
    | Some (v1, v2, v3) ->
        let outer_attrs = List.map (map_outer_attribute_item env) v1 in
        let field_first = map_field_declaration_union env v2 in
        let field_rest =
          List.map
            (fun (v1, v2, v3) ->
              let comma = token env v1 (* "," *) in
              let outer_attrs = List.map (map_outer_attribute_item env) v2 in
              let field = map_field_declaration_union env v3 in
              field)
            v3
        in
        field_first :: field_rest
    | None -> []
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) v3 in
  let rbrace = token env v4 (* "}" *) in
  fields

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression)
    (typeargs : G.type_arguments option) : G.expr =
  let expr = map_expression env v1 in
  let dot = token env v2 (* "." *) in
  let ident_or_dyn =
    match v3 with
    | `Id tok -> (
        let ident = ident env tok in
        (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
        match typeargs with
        | Some tas ->
            let name_ =
              (ident, { G.name_qualifier = None; G.name_typeargs = Some tas })
            in
            G.EN (G.IdQualified (name_, G.empty_id_info ()))
        | None -> G.EN (G.Id (ident, G.empty_id_info ())) )
    | `Int_lit tok -> (
        let literal = G.L (G.Int (integer_literal env tok)) in
        (* integer_literal *)
        match typeargs with
        | Some tas -> raise Impossible
        | None -> G.EDynamic literal )
  in
  G.DotAccess (expr, dot, ident_or_dyn)

and map_field_initializer_list (env : env)
    ((v1, v2, v3, v4) : CST.field_initializer_list) : G.expr list =
  let lbrace = token env v1 (* "{" *) in
  let fields =
    match v2 with
    | Some (v1, v2) ->
        let field_first = map_field_initializer env v1 in
        let field_rest =
          List.map
            (fun (v1, v2) ->
              let comma = token env v1 (* "," *) in
              let field = map_field_initializer env v2 in
              field)
            v2
        in
        field_first :: field_rest
    | None -> []
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) v3 in
  let rbrace = token env v4 (* "}" *) in
  fields

and map_foreign_block_item (env : env) ((v1, v2, v3) : CST.foreign_block_item) :
    G.stmt =
  let outer_attrs = List.map (map_outer_attribute_item env) v1 in
  let visibility =
    match v2 with Some x -> map_visibility_modifier env x | None -> []
  in
  match v3 with
  | `Fore_item_static x -> map_foreign_item_static env x
  | `Func_sign_with_defa_item x ->
      let defn = map_function_signature_with_default_item env x in
      G.DefStmt defn |> G.s
  | `Fore_item_type x -> map_foreign_item_type env x
  | `Macro_invo x ->
      let invo = map_macro_invocation env x in
      G.ExprStmt (invo, sc) |> G.s

and map_foreign_item_static (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.foreign_item_static) : G.stmt =
  let static = token env v1 (* "static" *) in
  let static_attr = G.KeywordAttr (G.Static, static) in
  let mutability =
    Option.map
      (fun tok ->
        let tok = token env tok (* "mut" *) in
        G.KeywordAttr (G.Mutable, tok))
      v2
  in
  let ident = ident env v3 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let colon = token env v4 (* ":" *) in
  let ty = map_type_ env v5 in
  let semicolon = token env v6 (* ";" *) in
  let var_def = { G.vinit = None; G.vtype = Some ty } in
  let ent =
    {
      G.name = G.EN (G.Id (ident, G.empty_id_info ()));
      G.attrs = deoptionalize [ Some static_attr; mutability ];
      G.tparams = [];
    }
  in
  G.DefStmt (ent, G.VarDef var_def) |> G.s

and map_foreign_mod_block (env : env) ((v1, v2, v3, v4) : CST.foreign_mod_block)
    : G.stmt =
  let lbrace = token env v1 (* "{" *) in
  let inner_attrs = List.map (map_inner_attribute_item env) v2 in
  let items = List.map (map_foreign_block_item env) v3 in
  let rbrace = token env v4 (* "}" *) in
  let block = G.Block (lbrace, items, rbrace) |> G.s in
  G.OtherStmtWithStmt (G.OSWS_ForeignBlock, None, block) |> G.s

and map_function_declaration (env : env)
    ((v1, v2, v3, v4, v5) : CST.function_declaration) : function_declaration_rs
    =
  let name : G.name_or_dynamic =
    match v1 with
    | `Id tok ->
        let ident = ident env tok in
        (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
        G.EN (G.Id (ident, G.empty_id_info ()))
    | `Meta tok ->
        let metavar = ident env tok in
        (* pattern \$[a-zA-Z_]\w* *)
        G.EDynamic (G.N (G.Id (metavar, G.empty_id_info ())))
  in
  let type_params =
    match v2 with Some x -> map_type_parameters env x | None -> []
  in
  let params = map_parameters env v3 in
  let retval =
    Option.map
      (fun (v1, v2) ->
        let arrow = token env v1 (* "->" *) in
        map_type_ env v2)
      v4
  in
  let where_clause = Option.map (fun x -> map_where_clause env x) v5 in
  { name; type_params; params; retval }

and map_function_item (env : env) ((v1, v2, v3, v4) : CST.function_item) :
    G.stmt =
  let attrs =
    match v1 with Some x -> map_function_modifiers env x | None -> []
  in
  let id = token env v2 (* "fn" *) in
  let fn_decl = map_function_declaration env v3 in
  let body = map_block env v4 in
  let fn_def =
    {
      G.fparams = fn_decl.params;
      G.frettype = fn_decl.retval;
      G.fkind = (G.Function, id);
      G.fbody = body;
    }
  in
  let ent =
    { G.name = fn_decl.name; G.attrs; G.tparams = fn_decl.type_params }
  in
  G.DefStmt (ent, G.FuncDef fn_def) |> G.s

and map_function_signature_with_default_item (env : env)
    ((v1, v2, v3, v4) : CST.function_signature_with_default_item) : G.definition
    =
  let modifiers = Option.map (fun x -> map_function_modifiers env x) v1 in
  let fn = token env v2 (* "fn" *) in
  let fn_decl = map_function_declaration env v3 in
  let default_impl =
    match v4 with
    | `SEMI tok ->
        let semicolon = token env tok in
        (* ";" *)
        (* No default implementation *)
        G.Block (G.fake_bracket []) |> G.s
    | `Blk x -> map_block env x
  in
  let fn_def =
    {
      G.fkind = (G.Method, fn);
      G.fparams = fn_decl.params;
      G.frettype = fn_decl.retval;
      G.fbody = default_impl;
    }
  in
  let ent =
    { G.name = fn_decl.name; G.attrs = []; G.tparams = fn_decl.type_params }
  in
  (ent, G.FuncDef fn_def)

and map_function_type (env : env) ((v1, v2, v3, v4) : CST.function_type) :
    G.type_ =
  let lifetimes =
    match v1 with Some x -> map_for_lifetimes env x | None -> []
  in
  let trait, modifiers =
    match v2 with
    | `Choice_id x ->
        let trait_name = map_struct_name env x in
        (* FnOnce, FnMut... *)
        (Some trait_name, None)
    | `Opt_func_modifs_fn (v1, v2) ->
        let modifiers = Option.map (fun x -> map_function_modifiers env x) v1 in
        let fn = token env v2 (* "fn" *) in
        (None, modifiers)
  in
  let params = map_parameters env v3 in
  let ret_type =
    match v4 with
    | Some (v1, v2) ->
        let arrow = token env v1 (* "->" *) in
        let ty = map_type_ env v2 in
        ty
    | None -> G.TyBuiltin (fake_id "()")
  in
  G.TyFun (params, ret_type)

(* TODO lifetimes, modifiers, traits *)
and map_generic_type_name (env : env) ((v1, v2) : CST.generic_type) :
    G.ident * G.name_info =
  let ident, info = map_struct_name env v1 in
  let typeargs = map_type_arguments env v2 in
  ( ident,
    { G.name_qualifier = info.name_qualifier; G.name_typeargs = Some typeargs }
  )

and map_generic_type (env : env) ((v1, v2) : CST.generic_type) : G.type_ =
  let name = map_generic_type_name env (v1, v2) in
  G.TyN (G.IdQualified (name, G.empty_id_info ()))

and map_generic_type_with_turbofish (env : env)
    ((v1, v2, v3) : CST.generic_type_with_turbofish) : G.ident * G.name_info =
  let ident, info = map_tuple_struct_name env v1 in
  let colons = token env v2 (* "::" *) in
  let typeargs = map_type_arguments env v3 in
  ( ident,
    { G.name_qualifier = info.name_qualifier; G.name_typeargs = Some typeargs }
  )

and map_generic_type_with_turbofish_type (env : env)
    ((v1, v2, v3) : CST.generic_type_with_turbofish) : G.type_ =
  let name = map_generic_type_with_turbofish env (v1, v2, v3) in
  G.TyN (G.IdQualified (name, G.empty_id_info ()))

and map_if_expression (env : env) ((v1, v2, v3, v4) : CST.if_expression) :
    G.expr =
  let if_ = token env v1 (* "if" *) in
  let cond = map_expression env v2 in
  let body = map_block env v3 in
  let else_ = Option.map (fun x -> map_else_clause env x) v4 in
  let if_stmt = G.If (if_, cond, body, else_) |> G.s in
  stmt_to_expr if_stmt

and map_if_let_expression (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.if_let_expression) : G.expr =
  let if_ = token env v1 (* "if" *) in
  let let_ = token env v2 (* "let" *) in
  let pattern = map_pattern env v3 in
  let equals = token env v4 (* "=" *) in
  let cond = map_expression env v5 in
  let body = map_block env v6 in
  let else_ = Option.map (fun x -> map_else_clause env x) v7 in
  let if_stmt = G.If (if_, cond, body, else_) |> G.s in
  let expr = G.OtherExpr (G.OE_StmtExpr, [ G.P pattern; G.S if_stmt ]) in
  G.LetPattern (pattern, expr)

and map_impl_block (env : env) ((v1, v2, v3, v4) : CST.impl_block) : G.stmt =
  let lbrace = token env v1 (* "{" *) in
  let inner_attrs = List.map (map_inner_attribute_item env) v2 in
  let stmts = List.map (map_impl_block_item env) v3 in
  let rbrace = token env v4 (* "}" *) in
  let block = G.Block (lbrace, stmts, rbrace) |> G.s in
  G.OtherStmtWithStmt (G.OSWS_ImplBlock, None, block) |> G.s

and map_impl_block_item (env : env) ((v1, v2, v3) : CST.impl_block_item) :
    G.stmt =
  let outer_attrs = List.map (map_outer_attribute_item env) v1 in
  let visibility =
    match v2 with Some x -> map_visibility_modifier env x | None -> []
  in
  match v3 with
  | `Impl_blk_item_const x -> map_impl_block_item_const env x
  | `Func_item x -> map_function_item env x
  | `Impl_blk_item_type x -> map_impl_block_item_type env x
  | `Macro_invo x ->
      let invo = map_macro_invocation env x in
      G.ExprStmt (invo, sc) |> G.s

and map_impl_block_item_const (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.impl_block_item_const) : G.stmt =
  let const = token env v1 (* "const" *) in
  let const_attr = G.KeywordAttr (G.Const, const) in
  let ident = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let colon = token env v3 (* ":" *) in
  let ty = map_type_ env v4 in
  let equals = token env v5 (* "=" *) in
  let expr = map_expression env v6 in
  let semicolon = token env v7 (* ";" *) in
  let var_def = { G.vinit = Some expr; G.vtype = Some ty } in
  let ent =
    {
      G.name = G.EN (G.Id (ident, G.empty_id_info ()));
      G.attrs = [ const_attr ];
      G.tparams = [];
    }
  in
  G.DefStmt (ent, G.VarDef var_def) |> G.s

and map_impl_block_item_type (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.impl_block_item_type) : G.stmt =
  let type_ = token env v1 (* "type" *) in
  let ident = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let type_params =
    match v3 with Some x -> map_type_parameters env x | None -> []
  in
  let equals = token env v4 (* "=" *) in
  let ty_ = map_type_ env v5 in
  let semicolon = token env v6 (* ";" *) in
  let type_def = { G.tbody = G.NewType ty_ } in
  let ent =
    {
      G.name = G.EN (G.Id (ident, G.empty_id_info ()));
      G.attrs = [];
      G.tparams = type_params;
    }
  in
  G.DefStmt (ent, G.TypeDef type_def) |> G.s

and map_inner_attribute_item (env : env)
    ((v1, v2, v3) : CST.inner_attribute_item) : rust_attribute =
  let hash = token env v1 (* "#" *) in
  let bang = token env v2 (* "!" *) in
  let meta_item = map_attribute env v3 in
  AttrInner meta_item

and map_last_match_arm (env : env) ((v1, v2, v3, v4, v5) : CST.last_match_arm) :
    G.action =
  let outer_attrs = List.map (map_outer_attribute_item env) v1 in
  let pattern = map_match_pattern env v2 in
  let arrow = token env v3 (* "=>" *) in
  let expr = map_expression env v4 in
  let comma = Option.map (fun tok -> token env tok) v5 in
  (pattern, expr)

and map_macro_invocation (env : env) ((v1, v2, v3) : CST.macro_invocation) :
    G.expr =
  let name =
    match v1 with
    | `Simple_scoped_id x -> map_simple_scoped_identifier_name env x
    | `Id tok -> name_of_id (ident env tok)
    (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  in
  let bang = token env v2 (* "!" *) in
  let tokens = map_token_tree env v3 in
  let di = dotted_ident_of_name_ name in
  G.OtherExpr (G.OE_MacroInvocation, [ G.Di di ])

and map_match_arm (env : env) ((v1, v2, v3, v4) : CST.match_arm) : G.action =
  let outer_attrs = List.map (map_outer_attribute_item env) v1 in
  let pattern =
    match v2 with
    | `Macro_invo x ->
        let invo = map_macro_invocation env x in
        G.OtherPat (G.OP_Todo, [ G.E invo ])
    | `Match_pat x -> map_match_pattern env x
  in
  let arrow = token env v3 (* "=>" *) in
  let expr =
    match v4 with
    | `Exp_COMMA (v1, v2) ->
        let expr = map_expression env v1 in
        let comma = token env v2 (* "," *) in
        expr
    | `Choice_unsafe_blk x -> map_expression_ending_with_block env x
  in
  (pattern, expr)

and map_match_block (env : env) ((v1, v2, v3) : CST.match_block) : G.action list
    =
  let lbrace = token env v1 (* "{" *) in
  let actions =
    match v2 with
    | Some (v1, v2) ->
        let match_arms = List.map (map_match_arm env) v1 in
        let match_arm_last = map_last_match_arm env v2 in
        List.concat [ match_arms; [ match_arm_last ] ]
    | None -> []
  in
  let rbrace = token env v3 (* "}" *) in
  actions

and map_match_pattern (env : env) ((v1, v2) : CST.match_pattern) : G.pattern =
  let pat = map_pattern env v1 in
  match v2 with
  | Some (v1, v2) ->
      let if_ = token env v1 (* "if" *) in
      let expr = map_expression env v2 in
      G.PatWhen (pat, expr)
  | None -> pat

and map_meta_arguments (env : env) ((v1, v2, v3, v4) : CST.meta_arguments) :
    rust_meta_argument list =
  let lparen = token env v1 (* "(" *) in
  let args =
    match v2 with
    | Some (v1, v2) ->
        let arg_first = map_meta_argument env v1 in
        let arg_rest =
          List.map
            (fun (v1, v2) ->
              let comma = token env v1 (* "," *) in
              let arg = map_meta_argument env v2 in
              arg)
            v2
        in
        arg_first :: arg_rest
    | None -> []
  in
  let comma = Option.map (fun tok -> token env tok) v3 in
  let rparen = token env v4 (* ")" *) in
  args

and map_meta_item (env : env) ((v1, v2) : CST.meta_item) =
  let path = map_simple_path env v1 in
  let value =
    Option.map
      (fun x ->
        match x with
        | `EQ_lit (v1, v2) ->
            let equals = token env v1 (* "=" *) in
            let lit = map_literal env v2 in
            MetaItemLiteral lit
        | `Meta_args x -> MetaItemMetaArgs (map_meta_arguments env x))
      v2
  in
  (path, value)

and map_mod_block (env : env) ((v1, v2, v3, v4) : CST.mod_block) :
    G.stmt list G.bracket =
  let lbrace = token env v1 (* "{" *) in
  let inner_attrs = List.map (map_inner_attribute_item env) v2 in
  let stmts = List.map (map_item env) v3 |> List.flatten in
  let rbrace = token env v4 (* "}" *) in
  (lbrace, stmts, rbrace)

and map_ordered_field (env : env) outer_attrs (attrs : G.attribute list)
    (type_ : G.type_) (index : int) : G.field =
  let index_s = string_of_int index in
  let var_def = { G.vinit = None; G.vtype = Some type_ } in
  let ent =
    {
      G.name = G.EDynamic (G.L (G.Int (Some index, G.fake index_s)));
      G.attrs = [];
      G.tparams = [];
    }
  in
  let stmt = G.DefStmt (ent, G.FieldDefColon var_def) |> G.s in
  G.FieldStmt stmt

(* for struct definition *)
and map_ordered_field_declaration_list (env : env)
    ((v1, v2, v3, v4) : CST.ordered_field_declaration_list) :
    G.field list G.bracket =
  let lparen = token env v1 (* "(" *) in
  let fields =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let outer_attrs = List.map (map_outer_attribute_item env) v1 in
        let visibility =
          match v2 with Some x -> map_visibility_modifier env x | None -> []
        in
        let type_first = map_type_ env v3 in
        let field_first =
          map_ordered_field env outer_attrs visibility type_first 0
        in
        let field_rest =
          List.mapi
            (fun index (v1, v2, v3, v4) ->
              let comma = token env v1 (* "," *) in
              let outer_attrs = List.map (map_outer_attribute_item env) v2 in
              let visibility =
                match v3 with
                | Some x -> map_visibility_modifier env x
                | None -> []
              in
              let type_ = map_type_ env v4 in
              let field =
                map_ordered_field env outer_attrs visibility type_first
                  (index + 1)
              in
              field)
            v4
        in
        field_first :: field_rest
    | None -> []
  in
  let comma = Option.map (fun tok -> token env tok) v3 in
  let rparen = token env v4 (* ")" *) in
  (lparen, fields, rparen)

(* for enum definition (OrConstructor) *)
and map_ordered_field_declaration_list_types (env : env)
    ((v1, v2, v3, v4) : CST.ordered_field_declaration_list) : G.type_ list =
  let lparen = token env v1 (* "(" *) in
  let types =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let outer_attrs = List.map (map_outer_attribute_item env) v1 in
        let visibility =
          match v2 with Some x -> map_visibility_modifier env x | None -> []
        in
        let type_first = map_type_ env v3 in
        let type_rest =
          List.mapi
            (fun index (v1, v2, v3, v4) ->
              let comma = token env v1 (* "," *) in
              let outer_attrs = List.map (map_outer_attribute_item env) v2 in
              let visibility =
                match v3 with
                | Some x -> map_visibility_modifier env x
                | None -> []
              in
              let type_ = map_type_ env v4 in
              type_)
            v4
        in
        type_first :: type_rest
    | None -> []
  in
  let comma = Option.map (fun tok -> token env tok) v3 in
  let rparen = token env v4 (* ")" *) in
  types

and map_outer_attribute_item (env : env) ((v1, v2) : CST.outer_attribute_item) :
    rust_attribute =
  let hash = token env v1 (* "#" *) in
  let meta_item = map_attribute env v2 in
  AttrOuter meta_item

and map_parameter (env : env) ((v1, v2, v3, v4) : CST.parameter) : G.parameter =
  let mutability =
    Option.map
      (fun tok ->
        let tok = token env tok in
        (* "mut" *)
        G.KeywordAttr (G.Mutable, tok))
      v1
  in
  let attrs = deoptionalize [ mutability ] in
  let colon = token env v3 (* ":" *) in
  let ty = map_type_ env v4 in
  match v2 with
  | `Pat x ->
      let pattern = map_pattern env x in
      G.OtherParam (G.OPO_Todo, [ G.P pattern; G.T ty ])
  | `Self tok ->
      let ident = ident env tok in
      (* "self" *)
      let param =
        {
          G.pname = Some ident;
          G.ptype = None;
          (* TODO *)
          G.pdefault = None;
          G.pattrs = attrs;
          G.pinfo = G.empty_id_info ();
        }
      in
      G.ParamClassic param
  | `Choice_defa x ->
      let ident = map_reserved_identifier env x in
      let param =
        {
          G.pname = Some ident;
          G.ptype = None;
          G.pdefault = None;
          G.pattrs = attrs;
          G.pinfo = G.empty_id_info ();
        }
      in
      G.ParamClassic param

and map_parameters (env : env) ((v1, v2, v3, v4) : CST.parameters) :
    G.parameter list =
  let lparen = token env v1 (* "(" *) in
  let params =
    match v2 with
    | Some (v1, v2, v3) ->
        let outer_attr =
          Option.map (fun x -> map_outer_attribute_item env x) v1
        in
        let param_first = map_anon_choice_param_2c23cdc env outer_attr v2 in
        let param_rest =
          List.map
            (fun (v1, v2, v3) ->
              let comma = token env v1 (* "," *) in
              let outer_attr =
                Option.map (fun x -> map_outer_attribute_item env x) v2
              in
              map_anon_choice_param_2c23cdc env outer_attr v3)
            v3
        in
        param_first :: param_rest
    | None -> []
  in
  let comma = Option.map (fun tok -> token env tok) v3 in
  let rparen = token env v4 (* ")" *) in
  params

and map_path (env : env) (x : CST.path) : G.expr =
  match x with
  | `Self tok ->
      let self = token env tok in
      (* "self" *)
      G.IdSpecial (G.Self, self)
  | `Choice_u8 x ->
      let ident = map_primitive_type_ident env x in
      G.N (G.Id (ident, G.empty_id_info ()))
  | `Meta tok ->
      let metavar = ident env tok in
      (* pattern \$[a-zA-Z_]\w* *)
      G.N (G.Id (metavar, G.empty_id_info ()))
  | `Super tok ->
      let super = token env tok in
      (* "super" *)
      G.IdSpecial (G.Super, super)
  | `Crate tok ->
      let crate = token env tok in
      (* "crate" *)
      G.IdSpecial (G.Parent, crate)
      (* TODO module instead? *)
  | `Id tok ->
      let ident = ident env tok in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      G.N (G.Id (ident, G.empty_id_info ()))
  | `Scoped_id x -> map_scoped_identifier env x

and map_path_name (env : env) (x : CST.path) : G.ident * G.name_info =
  match x with
  | `Self tok ->
      let self = ident env tok in
      (* "self" *)
      (self, G.empty_name_info)
  | `Choice_u8 x ->
      let ident = map_primitive_type_ident env x in
      (ident, G.empty_name_info)
  | `Meta tok ->
      let metavar = ident env tok in
      (* pattern \$[a-zA-Z_]\w* *)
      (metavar, G.empty_name_info)
  | `Super tok ->
      let super = ident env tok in
      (* "super" *)
      (super, G.empty_name_info)
  | `Crate tok ->
      let crate = ident env tok in
      (* "crate" *)
      (crate, G.empty_name_info)
  | `Id tok ->
      let ident = ident env tok in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      (ident, G.empty_name_info)
  | `Scoped_id x -> map_scoped_identifier_name env x

and map_pattern (env : env) (x : CST.pattern) : G.pattern =
  match x with
  | `Lit_pat x -> map_literal_pattern env x
  | `Choice_u8 x ->
      let ident = map_primitive_type_ident env x in
      G.PatId (ident, G.empty_id_info ())
  | `Id tok ->
      let ident = ident env tok in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      G.PatId (ident, G.empty_id_info ())
  | `Scoped_id x ->
      G.PatConstructor
        (dotted_ident_of_name_ (map_scoped_identifier_name env x), [])
  | `Tuple_pat (v1, v2, v3, v4) ->
      let lparen = token env v1 (* "(" *) in
      let items =
        match v2 with Some x -> map_tuple_pattern_list env x | None -> []
      in
      let comma = Option.map (fun tok -> token env tok) v3 in
      (* "," *)
      let rparen = token env v4 (* ")" *) in
      G.PatTuple (lparen, items, rparen)
  | `Tuple_struct_pat (v1, v2, v3, v4, v5) ->
      let name = map_tuple_struct_name env v1 in
      let lparen = token env v2 (* "(" *) in
      let items =
        match v3 with Some x -> map_tuple_pattern_list env x | None -> []
      in
      let comma = Option.map (fun tok -> token env tok) v4 in
      (* "," *)
      let rparen = token env v5 (* ")" *) in
      G.PatTuple (lparen, items, rparen)
  | `Struct_pat (v1, v2, v3, v4, v5) ->
      let name = map_struct_name env v1 in
      let lbrace = token env v2 (* "{" *) in
      let fields =
        match v3 with
        | Some (v1, v2) ->
            let field_first = map_struct_pattern_field env v1 in
            let field_rest =
              List.map
                (fun (v1, v2) ->
                  let comma = token env v1 (* "," *) in
                  let field = map_struct_pattern_field env v2 in
                  field)
                v2
            in
            field_first :: field_rest
        | None -> []
      in
      let comma = Option.map (fun tok -> token env tok) v4 in
      (* "," *)
      let rbrace = token env v5 (* "}" *) in
      G.PatRecord (lbrace, fields, rbrace)
  | `Ref_pat_a3d7f54 (v1, v2) ->
      let ref_ = token env v1 (* "ref" *) in
      let pattern = map_pattern env v2 in
      pattern
  | `Slice_pat (v1, v2, v3, v4) ->
      let lbracket = token env v1 (* "[" *) in
      let patterns =
        match v2 with Some x -> map_tuple_pattern_list env x | None -> []
      in
      let comma = Option.map (fun tok -> token env tok) v3 in
      (* "," *)
      let rbracket = token env v4 (* "]" *) in
      G.PatTuple (lbracket, patterns, rbracket)
  | `Capt_pat (v1, v2, v3) ->
      let ident = ident env v1 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let at = token env v2 (* "@" *) in
      let pattern = map_pattern env v3 in
      G.PatAs (pattern, (ident, G.empty_id_info ()))
  | `Ref_pat_dbbcf07 (v1, v2, v3) ->
      let and_ = token env v1 (* "&" *) in
      let mutability =
        Option.map
          (fun tok ->
            let tok = token env tok in
            (* "mut" *)
            G.KeywordAttr (G.Mutable, tok))
          v2
      in
      let pattern = map_pattern env v3 in
      let attrs = deoptionalize [ mutability ] in
      pattern
  | `Rema_field_pat tok ->
      G.PatId (ident env tok, G.empty_id_info ()) (* ".." *)
  | `Mut_pat (v1, v2) ->
      let mut = token env v1 (* "mut" *) in
      let mutability = G.KeywordAttr (G.Mutable, mut) in
      let pattern = map_pattern env v2 in
      pattern
  | `Range_pat (v1, v2, v3) ->
      let lbound = map_range_pattern_bound env v1 in
      let op =
        match v2 with
        | `DOTDOTDOT tok ->
            let tok = token env tok in
            (* "..." *)
            G.Range
        | `DOTDOTEQ tok ->
            let tok = token env tok in
            (* "..=" *)
            G.RangeInclusive
      in
      let rbound = map_range_pattern_bound env v3 in
      G.PatDisj (lbound, rbound)
  | `Or_pat (v1, v2, v3) ->
      let pattern_lhs = map_pattern env v1 in
      let or_ = token env v2 (* "|" *) in
      let pattern_rhs = map_pattern env v3 in
      G.DisjPat (pattern_lhs, pattern_rhs)
  | `Const_blk x ->
      let block = map_const_block env x in
      G.OtherPat (G.OP_Expr, [ G.E block ])
  | `X__ tok -> G.PatUnderscore (token env tok)

(* "_" *)
and map_pointer_type (env : env) ((v1, v2, v3) : CST.pointer_type) : G.type_ =
  let star = token env v1 (* "*" *) in
  let attr =
    match v2 with
    | `Const tok -> G.KeywordAttr (G.Const, token env tok) (* "const" *)
    | `Muta_spec tok -> G.KeywordAttr (G.Mutable, token env tok)
    (* "mut" *)
  in
  let type_ = map_type_ env v3 in
  G.TyPointer (star, type_)

(* TODO constness *)
and map_qualified_type (env : env) ((v1, v2, v3) : CST.qualified_type) : G.type_
    =
  let lhs = map_type_ env v1 in
  let as_ = token env v2 (* "as" *) in
  let rhs = map_type_ env v3 in
  G.OtherType (G.OT_Todo, [ G.T lhs; G.Tk as_; G.T rhs ])

and map_range_expression (env : env) (x : CST.range_expression) : G.expr =
  match x with
  | `Exp_choice_DOTDOT_exp (v1, v2, v3) ->
      let lhs = map_expression env v1 in
      let op, tok =
        match v2 with
        | `DOTDOT tok -> (G.Range, token env tok) (* ".." *)
        | `DOTDOTEQ tok -> (G.RangeInclusive, token env tok) (* "..=" *)
        (* Alternate proposed syntax (RFC 1192) *)
        | `DOTDOTDOT tok -> (G.RangeInclusive, token env tok)
        (* "..." *)
      in
      let rhs = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op op, tok), fb [ G.Arg lhs; G.Arg rhs ])
  | `Exp_DOTDOT (v1, v2) ->
      let lhs = map_expression env v1 in
      let dots = token env v2 (* ".." *) in
      G.Call (G.IdSpecial (G.Op G.Range, dots), fb [ G.Arg lhs ])
  | `DOTDOT_exp x -> map_base_field_initializer env x
  | `DOTDOT tok ->
      let dots = token env tok in
      (* ".." *)
      G.Call (G.IdSpecial (G.Op G.Range, dots), fb [])

and map_reference_type (env : env) ((v1, v2, v3, v4) : CST.reference_type) :
    G.type_ =
  let ref_ = token env v1 (* "&" *) in
  let lifetime = Option.map (fun x -> map_lifetime env x) v2 in
  let mutability =
    Option.map
      (fun tok ->
        let tok = token env tok in
        (* "mut" *)
        G.KeywordAttr (G.Mutable, tok))
      v3
  in
  let type_ = map_type_ env v4 in
  (* TODO TyRef with lifetime/mutability *)
  G.TyRef (ref_, type_)

and map_return_expression (env : env) (x : CST.return_expression) : G.expr =
  let return_stmt =
    match x with
    | `Ret_exp (v1, v2) ->
        let return = token env v1 (* "return" *) in
        let expr = map_expression env v2 in
        G.Return (return, Some expr, sc) |> G.s
    | `Ret tok ->
        let return = token env tok in
        (* "return" *)
        G.Return (return, None, sc) |> G.s
  in
  stmt_to_expr return_stmt

and map_scoped_identifier_name (env : env)
    ((v1, v2, v3) : CST.scoped_identifier) : G.ident * G.name_info =
  let qualifier_expr, type_ =
    match v1 with
    | Some x -> (
        match x with
        | `Choice_self x -> (Some (map_path env x), None)
        | `Brac_type x ->
            let _, ty, _ = map_bracketed_type env x in
            (None, Some ty)
        | `Gene_type_with_turb x ->
            let ty = map_generic_type_with_turbofish_type env x in
            (None, Some ty) )
    | None -> (None, None)
  in
  let colons = token env v2 (* "::" *) in
  let ident = ident env v3 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let qualifier = Option.map (fun x -> G.QExpr (x, colons)) qualifier_expr in
  let typeargs = Option.map (fun x -> [ G.TypeArg x ]) type_ in
  (ident, { G.name_qualifier = qualifier; G.name_typeargs = typeargs })

and map_scoped_identifier (env : env) (v1 : CST.scoped_identifier) : G.expr =
  G.N (G.IdQualified (map_scoped_identifier_name env v1, G.empty_id_info ()))

and map_scoped_type_identifier_name (env : env)
    ((v1, v2, v3) : CST.scoped_type_identifier) : G.ident * G.name_info =
  let colons = token env v2 (* "::" *) in
  let qualifier, typeargs =
    match v1 with
    | Some x -> (
        match x with
        | `Choice_self x ->
            let ident = map_path env x in
            (Some (G.QExpr (ident, colons)), None)
        | `Gene_type_with_turb x ->
            let ty = map_generic_type_with_turbofish_type env x in
            (None, Some [ G.TypeArg ty ])
        | `Brac_type x ->
            let _, ty, _ = map_bracketed_type env x in
            (None, Some [ G.TypeArg ty ])
        | `Gene_type x ->
            let ty = map_generic_type env x in
            (None, Some [ G.TypeArg ty ]) )
    | None -> (None, None)
  in
  let ident = ident env v3 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  (ident, { G.name_qualifier = qualifier; G.name_typeargs = typeargs })

and map_scoped_type_identifier (env : env)
    ((v1, v2, v3) : CST.scoped_type_identifier) : G.type_ =
  let name = map_scoped_type_identifier_name env (v1, v2, v3) in
  G.TyN (G.IdQualified (name, G.empty_id_info ()))

and map_scoped_type_identifier_in_expression_position (env : env)
    ((v1, v2, v3) : CST.scoped_type_identifier_in_expression_position) :
    G.ident * G.name_info =
  let colons = token env v2 (* "::" *) in
  let qualifier, typeargs =
    match v1 with
    | Some x -> (
        match x with
        | `Choice_self x ->
            let ident = map_path env x in
            (Some (G.QExpr (ident, colons)), None)
        | `Gene_type_with_turb x ->
            let ty = map_generic_type_with_turbofish_type env x in
            (None, Some [ G.TypeArg ty ]) )
    | None -> (None, None)
  in
  let ident = ident env v3 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  (ident, { G.name_qualifier = qualifier; G.name_typeargs = typeargs })

and map_statement (env : env) (x : CST.statement) : G.stmt list =
  match x with
  | `Exp_stmt x -> [ map_expression_statement env x ]
  | `Let_decl (v1, v2, v3, v4, v5, v6) ->
      let let_ = token env v1 (* "let" *) in
      let mutability =
        Option.map
          (fun tok ->
            let t = token env tok in
            (* "mut" *)
            G.KeywordAttr (G.Mutable, t))
          v2
      in
      let attrs = deoptionalize [ mutability ] in
      let pattern = map_pattern env v3 in
      let type_ =
        Option.map
          (fun (v1, v2) ->
            let colon = token env v1 (* ":" *) in
            let type_ = map_type_ env v2 in
            type_)
          v4
      in
      let expr =
        Option.map
          (fun (v1, v2) ->
            let equals = token env v1 (* "=" *) in
            let expr = map_expression env v2 in
            expr)
          v5
      in
      let semicolon = token env v6 (* ";" *) in
      let var_def = { G.vinit = expr; G.vtype = type_ } in
      let ent =
        {
          (* Patterns are difficult to convert to expressions, so wrap it *)
          G.name = G.EDynamic (G.OtherExpr (G.OE_Todo, [ G.P pattern ]));
          G.attrs;
          G.tparams = [];
        }
      in
      [ G.DefStmt (ent, G.VarDef var_def) |> G.s ]
  | `Item x -> map_item env x

and map_trait_block (env : env) ((v1, v2, v3) : CST.trait_block) :
    G.field list G.bracket =
  let lbrace = token env v1 (* "{" *) in
  let fields = List.map (map_trait_block_item env) v2 in
  let rbrace = token env v3 (* "}" *) in
  (lbrace, fields, rbrace)

and map_trait_block_item (env : env) ((v1, v2) : CST.trait_block_item) : G.field
    =
  let outer_attrs = List.map (map_outer_attribute_item env) v1 in
  match v2 with
  | `Const_item x -> G.FieldStmt (map_const_item env x)
  | `Func_sign_with_defa_item x ->
      let def = map_function_signature_with_default_item env x in
      G.FieldStmt (G.DefStmt def |> G.s)
  | `Asso_type x -> G.FieldStmt (map_associated_type env x)
  | `Macro_invo x ->
      let invo = map_macro_invocation env x in
      G.FieldStmt (G.ExprStmt (invo, sc) |> G.s)

and map_higher_ranked_trait_bound (env : env)
    ((v1, v2, v3) : CST.higher_ranked_trait_bound) :
    G.type_parameter list * G.type_ =
  let for_ = token env v1 (* "for" *) in
  let type_parameters = map_type_parameters env v2 in
  let type_ = map_type_ env v3 in
  (type_parameters, type_)

and map_trait_bound (env : env) (x : CST.anon_choice_type_d689819) : trait_bound
    =
  match x with
  | `Type x ->
      let ty = map_type_ env x in
      TraitBoundType ty
  | `Life x ->
      let lt = map_lifetime env x in
      TraitBoundLifetime lt
  | `Higher_ranked_trait_bound x ->
      let type_params, type_ = map_higher_ranked_trait_bound env x in
      TraitBoundHigherRanked (type_params, type_)
  | `Remo_trait_bound (v1, v2) ->
      let qmark = token env v1 (* "?" *) in
      let ty = map_type_ env v2 in
      TraitBoundRemoved ty

and map_trait_bounds (env : env) ((v1, v2, v3) : CST.trait_bounds) :
    trait_bound list =
  let colon = token env v1 (* ":" *) in
  let trait_bound_first = map_trait_bound env v2 in
  let trait_bound_rest =
    List.map
      (fun (v1, v2) ->
        let plus = token env v1 (* "+" *) in
        let trait_bound = map_trait_bound env v2 in
        trait_bound)
      v3
  in
  trait_bound_first :: trait_bound_rest

and map_tuple_type (env : env) ((v1, v2, v3, v4, v5) : CST.tuple_type) : G.type_
    =
  let lparen = token env v1 (* "(" *) in
  let ty_first = map_type_ env v2 in
  let ty_rest =
    List.map
      (fun (v1, v2) ->
        let comma = token env v1 (* "," *) in
        let ty = map_type_ env v2 in
        ty)
      v3
  in
  let comma = Option.map (fun tok -> token env tok) v4 in
  let rparen = token env v5 (* ")" *) in
  G.TyTuple (lparen, ty_first :: ty_rest, rparen)

and map_type_ (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Abst_type (v1, v2) ->
      let impl = token env v1 (* "impl" *) in
      let trait_type = map_abstract_type_trait_name env v2 in
      trait_type
  | `Ref_type x -> map_reference_type env x
  | `Meta tok ->
      let metavar = ident env tok in
      (* pattern \$[a-zA-Z_]\w* *)
      G.OtherType (G.OT_Expr, [ G.E (G.N (G.Id (metavar, G.empty_id_info ()))) ])
  | `Poin_type x -> map_pointer_type env x
  | `Gene_type x -> map_generic_type env x
  | `Scoped_type_id x -> map_scoped_type_identifier env x
  | `Tuple_type x -> map_tuple_type env x
  | `Unit_type (v1, v2) ->
      let lparen = str env v1 (* "(" *) in
      let rparen = str env v2 (* ")" *) in
      let str = List.map fst [ lparen; rparen ] |> String.concat "" in
      G.TyBuiltin (str, PI.combine_infos (snd lparen) [ snd rparen ])
  | `Array_type (v1, v2, v3, v4) ->
      let lbracket = token env v1 (* "[" *) in
      let ty = map_type_ env v2 in
      let default =
        Option.map
          (fun (v1, v2) ->
            let semicolon = token env v1 (* ";" *) in
            let expr = map_expression env v2 in
            expr)
          v3
      in
      let rbracket = token env v4 (* "]" *) in
      G.TyArray ((lbracket, default, rbracket), ty)
  | `Func_type x -> map_function_type env x
  | `Id tok ->
      let ident = ident env tok in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let id_info = G.empty_id_info () in
      G.TyN (G.Id (ident, id_info))
  | `Macro_invo x ->
      let invo = map_macro_invocation env x in
      G.OtherType (G.OT_Expr, [ G.E invo ])
  | `Empty_type tok ->
      let bang = token env tok in
      (* "!" *)
      G.TyBuiltin ("!", bang)
  | `Dyna_type (v1, v2) ->
      let dyn = token env v1 (* "dyn" *) in
      let ty = map_abstract_type_trait_name env v2 in
      ty
  | `Boun_type x -> map_bounded_type env x
  | `Choice_u8 x -> map_primitive_type env x

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) :
    G.type_arguments =
  let lthan = token env v1 (* tok_LT *) in
  let typearg_first = map_type_argument env v2 in
  let typearg_rest =
    List.map
      (fun (v1, v2) ->
        let comma = token env v1 (* "," *) in
        let typearg = map_type_argument env v2 in
        typearg)
      v3
  in
  let comma = Option.map (fun tok -> token env tok) v4 in
  let gthan = token env v5 (* ">" *) in
  typearg_first :: typearg_rest

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters)
    : G.type_parameter list =
  let lthan = token env v1 (* "<" *) in
  let type_param_first = map_type_parameter env v2 in
  let type_param_rest =
    List.map
      (fun (v1, v2) ->
        let comma = token env v1 (* "," *) in
        let type_param = map_type_parameter env v2 in
        type_param)
      v3
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) v4 in
  let gthan = token env v5 (* ">" *) in
  type_param_first :: type_param_rest

and map_use_clause (env : env) (x : CST.use_clause) use : G.directive list =
  match x with
  | `Choice_self x ->
      let dots, ident = map_simple_path_ident env x in
      let modname = G.DottedName dots in
      [ G.ImportFrom (use, modname, ident, None) ]
  | `Use_as_clause (v1, v2, v3) ->
      let dots, ident_ = map_simple_path_ident env v1 in
      let modname = G.DottedName dots in
      let as_ = token env v2 (* "as" *) in
      let alias = ident env v3 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      [ G.ImportFrom (use, modname, ident_, Some (alias, G.empty_id_info ())) ]
  | `Use_list x -> map_use_list env x use None
  | `Scoped_use_list (v1, v2, v3) ->
      let scope = Option.map (fun x -> map_simple_path env x) v1 in
      let colons = token env v2 (* "::" *) in
      map_use_list env v3 use scope
  | `Use_wild (v1, v2) ->
      let dots =
        match v1 with
        | Some (v1, v2) ->
            let path = map_simple_path env v1 in
            let colons = token env v2 (* "::" *) in
            path
        | None -> []
      in
      let modname = G.DottedName dots in
      let wildcard = token env v2 (* "*" *) in
      [ G.ImportAll (use, modname, wildcard) ]

and prepend_module_name (scope : G.dotted_ident) (modname : G.module_name) :
    G.module_name =
  match modname with
  | DottedName modname -> G.DottedName (modname @ scope)
  | _ -> modname

and prepend_scope (dir : G.directive) (scope : G.dotted_ident option) :
    G.directive =
  match scope with
  | Some scope -> (
      match dir with
      | ImportFrom (tok, modname, id, alias) ->
          let modname = prepend_module_name scope modname in
          G.ImportFrom (tok, modname, id, alias)
      | ImportAs (tok, modname, alias) ->
          let modname = prepend_module_name scope modname in
          G.ImportAs (tok, modname, alias)
      | ImportAll (tok, modname, wildcard) ->
          let modname = prepend_module_name scope modname in
          G.ImportAll (tok, modname, wildcard)
      | _ -> dir )
  | None -> dir

and map_use_list (env : env) ((v1, v2, v3, v4) : CST.use_list)
    (use : PI.token_mutable) (scope : G.dotted_ident option) : G.directive list
    =
  let lbracket = token env v1 (* "{" *) in
  let directives =
    match v2 with
    | Some (v1, v2) ->
        let use_clause_first =
          match v1 with `Use_clause x -> map_use_clause env x use
        in
        let use_clause_rest =
          List.map
            (fun (v1, v2) ->
              let comma = token env v1 (* "," *) in
              let use_clause =
                match v2 with `Use_clause x -> map_use_clause env x use
              in
              use_clause)
            v2
        in
        List.flatten (use_clause_first :: use_clause_rest)
    | None -> []
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) v3 in
  let rbracket = token env v4 (* "}" *) in
  List.map (fun x -> prepend_scope x scope) directives

and map_visibility_quantifier (env : env) (v1, v2, v3) : G.attribute =
  let lparen = token env v1 (* "(" *) in
  let attribute =
    match v2 with
    (* TODO *)
    | `Self tok -> G.KeywordAttr (G.Private, token env tok) (* "self" *)
    | `Super tok -> G.KeywordAttr (G.Private, token env tok) (* "super" *)
    | `Crate tok -> G.KeywordAttr (G.Protected, token env tok) (* "crate" *)
    | `In_choice_self (v1, v2) ->
        let in_ = token env v1 (* "in" *) in
        let path = map_simple_path env v2 in
        G.OtherAttribute (G.OA_Expr, [ G.Di path ])
  in
  let rparen = token env v3 (* ")" *) in
  attribute

and map_visibility_modifier (env : env) (x : CST.visibility_modifier) :
    G.attribute list =
  match x with
  | `Crate tok ->
      let tok = token env tok in
      (* "crate" *)
      (* unstable(crate_visibility_modifier) *)
      (* synonymous with `pub(crate)` *)
      [ G.KeywordAttr (G.Protected, tok) ]
  | `Pub_opt_LPAR_choice_self_RPAR (v1, v2) ->
      let pub = token env v1 (* "pub" *) in
      let pub_attr = G.KeywordAttr (G.Public, pub) in
      let qualifier =
        Option.map (fun x -> map_visibility_quantifier env x) v2
      in
      deoptionalize [ Some pub_attr; qualifier ]

and map_where_clause (env : env) ((v1, v2, v3, v4) : CST.where_clause) :
    where_clause =
  let where = token env v1 (* "where" *) in
  let predicate_first = map_where_predicate env v2 in
  let predicate_rest =
    List.map
      (fun (v1, v2) ->
        let comma = token env v1 (* "," *) in
        let predicate = map_where_predicate env v2 in
        predicate)
      v3
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) v4 in
  predicate_first :: predicate_rest

and map_where_predicate (env : env) ((v1, v2) : CST.where_predicate) :
    where_predicate =
  let where_predicate_type =
    match v1 with
    | `Life x -> WherePredLifetime (map_lifetime env x)
    | `Id tok ->
        WherePredId (ident env tok)
        (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
    | `Scoped_type_id x -> WherePredType (map_scoped_type_identifier env x)
    | `Gene_type x -> WherePredType (map_generic_type env x)
    | `Ref_type x -> WherePredType (map_reference_type env x)
    | `Poin_type x -> WherePredType (map_pointer_type env x)
    | `Tuple_type x -> WherePredType (map_tuple_type env x)
    | `Higher_ranked_trait_bound x ->
        let type_params, ty = map_higher_ranked_trait_bound env x in
        WherePredHigherRanked (type_params, ty)
    | `Choice_u8 x -> WherePredType (map_primitive_type env x)
  in
  let trait_bounds = map_trait_bounds env v2 in
  (where_predicate_type, trait_bounds)

and convert_macro_item (item : rust_macro_item) : G.any list =
  match item with
  | Tk tok -> [ G.Tk tok ]
  | MacTkTree (_, items, _) -> List.flatten (List.map convert_macro_item items)
  | MacTks ((_, items, _), ident, tok) ->
      let items = List.flatten (List.map convert_macro_item items) in
      let ident = match ident with Some i -> [ G.I i ] | None -> [] in
      items @ ident @ [ G.Tk tok ]

and convert_macro_pattern (pattern : rust_macro_pattern) : G.any list =
  match pattern with
  | RustMacPatTree pats -> []
  | RustMacPatRepetition ((_, pats, _), ident, tok) ->
      let pats = List.flatten (List.map convert_macro_pattern pats) in
      let ident = match ident with Some i -> [ G.I i ] | None -> [] in
      pats @ ident @ [ G.Tk tok ]
  | RustMacPatBinding (ident, tok) -> [ G.I ident; G.Tk tok ]
  | RustMacPatToken tok -> [ G.Tk tok ]

and convert_macro_rule (rule : rust_macro_rule) : G.any list =
  let rules = List.flatten (List.map convert_macro_pattern rule.rules) in
  let _, items, _ = rule.body in
  let body = List.flatten (List.map convert_macro_item items) in
  rules @ body

and convert_macro_def (macro_def : rust_macro_definition) : G.macro_definition =
  let _, rules, _ = macro_def in
  let body = List.flatten (List.map convert_macro_rule rules) in
  { G.macroparams = []; G.macrobody = body }

and map_item_kind (env : env) outer_attrs visibility (x : CST.item_kind) :
    G.stmt list =
  match x with
  | `Const_item x -> [ map_const_item env x ]
  | `Macro_invo x ->
      let invo = map_macro_invocation env x in
      [ G.ExprStmt (invo, sc) |> G.s ]
  | `Macro_defi (v1, v2, v3) ->
      let macro_rules = token env v1 (* "macro_rules!" *) in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let macro_def : rust_macro_definition =
        match v3 with
        | `LPAR_rep_macro_rule_SEMI_opt_macro_rule_RPAR_SEMI (v1, v2, v3, v4, v5)
          ->
            let lparen = token env v1 (* "(" *) in
            let rules =
              List.map
                (fun (v1, v2) ->
                  let rule = map_macro_rule env v1 in
                  let semicolon = token env v2 (* ";" *) in
                  rule)
                v2
            in
            let rule_last =
              match v3 with Some x -> [ map_macro_rule env x ] | None -> []
            in
            let rparen = token env v4 (* ")" *) in
            let semicolon = token env v5 (* ";" *) in
            (lparen, List.concat [ rules; rule_last ], rparen)
        | `LCURL_rep_macro_rule_SEMI_opt_macro_rule_RCURL (v1, v2, v3, v4) ->
            let lbrace = token env v1 (* "{" *) in
            let rules =
              List.map
                (fun (v1, v2) ->
                  let rule = map_macro_rule env v1 in
                  let semicolon = token env v2 (* ";" *) in
                  rule)
                v2
            in
            let rule_last =
              match v3 with Some x -> [ map_macro_rule env x ] | None -> []
            in
            let rbrace = token env v4 (* "}" *) in
            (lbrace, List.concat [ rules; rule_last ], rbrace)
      in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs = [];
          G.tparams = [];
        }
      in
      let macro_def = convert_macro_def macro_def in
      [ G.DefStmt (ent, G.MacroDef macro_def) |> G.s ]
  | `Empty_stmt tok ->
      let semicolon = token env tok in
      (* ";" *)
      []
  | `Mod_item (v1, v2, v3) ->
      let mod_ = token env v1 (* "mod" *) in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let block =
        match v3 with
        | `SEMI tok ->
            let semicolon = token env tok in
            (* ";" *)
            []
        | `Mod_blk x ->
            let _, block, _ = map_mod_block env x in
            block
      in
      let mod_def = { G.mbody = G.ModuleStruct (Some [ ident ], block) } in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs = [];
          G.tparams = [];
        }
      in
      [ G.DefStmt (ent, G.ModuleDef mod_def) |> G.s ]
  | `Fore_mod_item (v1, v2) ->
      let extern = map_extern_modifier env v1 in
      let block =
        match v2 with
        | `SEMI tok ->
            let semicolon = token env tok in
            (* ";" *)
            G.Block (G.fake_bracket []) |> G.s
        | `Fore_mod_blk x -> map_foreign_mod_block env x
      in
      [ block ]
  | `Struct_item (v1, v2, v3, v4) ->
      let struct_ = token env v1 (* "struct" *) in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let type_params =
        match v3 with Some x -> map_type_parameters env x | None -> []
      in
      let fields, where_clause =
        match v4 with
        | `Opt_where_clause_field_decl_list (v1, v2) ->
            let where_clause =
              Option.map (fun x -> map_where_clause env x) v1
            in
            let fields = map_field_declaration_list env v2 in
            (fields, where_clause)
        | `Orde_field_decl_list_opt_where_clause_SEMI (v1, v2, v3) ->
            (* Unit structs *)
            let fields = map_ordered_field_declaration_list env v1 in
            let where_clause =
              Option.map (fun x -> map_where_clause env x) v2
            in
            let semicolon = token env v3 (* ";" *) in
            (fields, where_clause)
        | `SEMI tok ->
            let semicolon = token env tok in
            (* ";" *)
            let fields = (G.fake "{", [], G.fake "}") in
            (fields, None)
      in
      let class_def =
        {
          G.ckind = (G.Class, struct_);
          G.cextends = [];
          G.cimplements = [];
          G.cmixins = [];
          G.cparams = [];
          G.cbody = fields;
        }
      in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          attrs = [];
          tparams = type_params;
        }
      in
      [ G.DefStmt (ent, G.ClassDef class_def) |> G.s ]
  | `Union_item (v1, v2, v3, v4, v5) ->
      let union = token env v1 (* "union" *) in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let type_params =
        match v3 with Some x -> map_type_parameters env x | None -> []
      in
      let where_clause =
        match v4 with Some x -> map_where_clause env x | None -> []
      in
      let variants = map_field_declaration_list_union env v5 in
      let type_def = { G.tbody = G.OrType variants } in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs = [];
          G.tparams = type_params;
        }
      in
      [ G.DefStmt (ent, G.TypeDef type_def) |> G.s ]
  | `Enum_item (v1, v2, v3, v4, v5) ->
      let enum = token env v1 (* "enum" *) in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let type_params =
        match v3 with Some x -> map_type_parameters env x | None -> []
      in
      let where_clause = Option.map (fun x -> map_where_clause env x) v4 in
      let body = map_enum_variant_list env v5 in
      let type_def = { G.tbody = body } in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs = [];
          G.tparams = type_params;
        }
      in
      [ G.DefStmt (ent, G.TypeDef type_def) |> G.s ]
  | `Type_item (v1, v2, v3, v4, v5, v6) ->
      let type_ = token env v1 (* "type" *) in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let type_params =
        match v3 with Some x -> map_type_parameters env x | None -> []
      in
      let equals = token env v4 (* "=" *) in
      let ty = map_type_ env v5 in
      let semicolon = token env v6 (* ";" *) in
      let type_def =
        { G.tbody = G.NewType (G.TyN (G.Id (ident, G.empty_id_info ()))) }
      in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs = [];
          G.tparams = type_params;
        }
      in
      [ G.DefStmt (ent, G.TypeDef type_def) |> G.s ]
  | `Func_item x -> [ map_function_item env x ]
  | `Func_sign_item (v1, v2, v3, v4) ->
      let attrs =
        match v1 with Some x -> map_function_modifiers env x | None -> []
      in
      let fn = token env v2 (* "fn" *) in
      let fn_decl = map_function_declaration env v3 in
      let semicolon = token env v4 (* ";" *) in
      let empty_block = G.Block (G.fake "{", [], G.fake "}") |> G.s in
      let fn_def =
        {
          G.fparams = fn_decl.params;
          G.frettype = fn_decl.retval;
          G.fkind = (G.Function, fn);
          (* no body defined *)
          G.fbody = empty_block;
        }
      in
      let ent =
        { G.name = fn_decl.name; G.attrs; G.tparams = fn_decl.type_params }
      in
      [ G.DefStmt (ent, G.FuncDef fn_def) |> G.s ]
  | `Impl_item (v1, v2, v3, v4, v5, v6, v7) ->
      let unsafe_attr =
        Option.map
          (fun tok ->
            let tok = token env tok in
            (* "unsafe" *)
            G.KeywordAttr (G.Unsafe, tok))
          v1
      in
      let attrs = deoptionalize [ unsafe_attr ] in
      let impl = token env v2 (* "impl" *) in
      let type_params =
        match v3 with Some x -> map_type_parameters env x | None -> []
      in
      let trait_type =
        Option.map
          (fun (v1, v2) ->
            let ty =
              match v1 with
              | `Id tok ->
                  let ident = ident env tok in
                  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
                  G.TyN (G.Id (ident, G.empty_id_info ()))
              | `Scoped_type_id x -> map_scoped_type_identifier env x
              | `Gene_type x -> map_generic_type env x
            in
            let for_ = token env v2 (* "for" *) in
            ty)
          v4
      in
      let ty = map_type_ env v5 in
      let where_clause = Option.map (fun x -> map_where_clause env x) v6 in
      [ map_impl_block env v7 ]
  | `Trait_item (v1, v2, v3, v4, v5, v6, v7) ->
      let unsafe_attr =
        Option.map
          (fun tok ->
            let tok = token env tok in
            (* "unsafe" *)
            G.KeywordAttr (G.Unsafe, tok))
          v1
      in
      let attrs = deoptionalize [ unsafe_attr ] in
      let trait = token env v2 (* "trait" *) in
      let ident = ident env v3 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let type_params =
        match v4 with Some x -> map_type_parameters env x | None -> []
      in
      let trait_bounds =
        match v5 with Some x -> map_trait_bounds env x | None -> []
      in
      let where_clause = Option.map (fun x -> map_where_clause env x) v6 in
      let fields = map_trait_block env v7 in
      let class_def =
        {
          G.ckind = (G.Trait, trait);
          G.cextends = [];
          G.cimplements = [];
          G.cmixins = [];
          G.cparams = [];
          G.cbody = fields;
        }
      in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs;
          G.tparams = [];
        }
      in
      [ G.DefStmt (ent, G.ClassDef class_def) |> G.s ]
  | `Use_decl (v1, v2, v3) ->
      let use = token env v1 (* "use" *) in
      let use_clauses = map_use_clause env v2 use in
      let semicolon = token env v3 (* ";" *) in
      List.map (fun x -> G.DirectiveStmt x |> G.s) use_clauses
  | `Extern_crate_decl (v1, v2, v3, v4, v5) ->
      let extern = token env v1 (* "extern" *) in
      let crate = token env v2 (* "crate" *) in
      let ident_ = ident env v3 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let alias =
        Option.map
          (fun (v1, v2) ->
            let as_ = token env v1 (* "as" *) in
            let alias = ident env v2 in
            (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
            alias)
          v4
      in
      let semicolon = token env v5 (* ";" *) in
      let any =
        match alias with
        | Some x -> [ G.I ident_; G.I x ]
        | None -> [ G.I ident_ ]
      in
      let directive = G.OtherDirective (G.OI_Extern, any) in
      [ G.DirectiveStmt directive |> G.s ]
  | `Static_item (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let static = token env v1 (* "static" *) in
      let ref_ = Option.map (fun tok -> token env tok (* "ref" *)) v2 in
      let mutability_attr =
        Option.map
          (fun tok ->
            let t = token env tok in
            (* "mut" *)
            G.KeywordAttr (G.Mutable, t))
          v3
      in
      let attrs =
        deoptionalize
          [ Some (G.KeywordAttr (G.Static, static)); mutability_attr ]
      in
      let ident = ident env v4 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let colon = token env v5 (* ":" *) in
      let type_ = map_type_ env v6 in
      let init =
        Option.map
          (fun (v1, v2) ->
            let equals = token env v1 (* "=" *) in
            let expr = map_expression env v2 in
            expr)
          v7
      in
      let semicolon = token env v8 (* ";" *) in
      let var_def = { G.vinit = init; G.vtype = Some type_ } in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs;
          G.tparams = [];
        }
      in
      [ G.DefStmt (ent, G.VarDef var_def) |> G.s ]

and map_item (env : env) ((v1, v2, v3) : CST.item) : G.stmt list =
  let outer_attrs = List.map (map_outer_attribute_item env) v1 in
  let visibility = Option.map (fun x -> map_visibility_modifier env x) v2 in
  map_item_kind env outer_attrs visibility v3

let map_source_file (env : env) (x : CST.source_file) : G.any =
  match x with
  | `Rep_inner_attr_item_rep_item (v1, v2) ->
      let inner_attrs = List.map (map_inner_attribute_item env) v1 in
      let items = List.map (map_item env) v2 |> List.flatten in
      G.Pr items
  | `Semg_exp (v1, v2) ->
      let header = token env v1 (* "__SEMGREP_EXPRESSION" *) in
      let expr = map_expression env v2 in
      G.E expr
  | `Semg_stmt (v1, v2) ->
      let header = token env v1 (* "__SEMGREP_STATEMENT" *) in
      let stmts = List.map (map_statement env) v2 |> List.flatten in
      G.Ss stmts

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_rust.Parse.file file ())
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match map_source_file env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_expression_or_source_file str =
  let res = Tree_sitter_rust.Parse.string str in
  match res.errors with
  | [] -> res
  | _ -> (
      let expr_str = "__SEMGREP_EXPRESSION " ^ str in
      let expr_res = Tree_sitter_rust.Parse.string expr_str in
      match expr_res.errors with
      | [] -> expr_res
      | _ ->
          let stmt_str = "__SEMGREP_STATEMENT " ^ str in
          Tree_sitter_rust.Parse.string stmt_str )

(* todo: special mode to convert Ellipsis in the right construct! *)
let parse_pattern str =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke parse_expression_or_source_file str ())
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      match map_source_file env cst with
      | G.Pr [ x ] -> G.S x
      | G.Pr xs -> G.Ss xs
      | x -> x)

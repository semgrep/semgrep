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

[@@@warning "-32"]

module CST = Tree_sitter_rust.CST
module H = Parse_tree_sitter_helpers
module PI = Parse_info
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Rust parser using ocaml-tree-sitter-lang/rust and converting
 * directly to pfff/h_program-lang/ast_generic.ml
 *
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env
let token = H.token
let str = H.str
let sc = PI.fake_info ";"
let fb = G.fake_bracket
let fake_id s = (s, G.fake s)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying ocaml-tree-sitter-lang/rust/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the rust CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type function_declaration_rs = {
  name: G.ident_or_dynamic;
  type_params: G.type_parameter list option;
  params: G.parameter list;
  retval: G.type_ option;
  where_clause: G.where_clause option;
}

let deoptionalize l =
  let rec deopt acc = function
    | [] -> List.rev acc
    | None::tl -> deopt acc tl
    | Some x::tl -> deopt (x::acc) tl
  in
  deopt [] l

let todo (env : env) _ =
   failwith "not implemented"

let ident (env : env) (tok : CST.identifier): G.ident =
  str env tok (* pattern [a-zA-Z_]\w* *)

let map_integer_literal (env : env) (tok : CST.integer_literal) =
  token env tok (* integer_literal *)

let map_fragment_specifier (env : env) (x : CST.fragment_specifier) =
  (match x with
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
  | `Vis tok -> token env tok (* "vis" *)
  )

let map_token_quantifier (env : env) (x : CST.anon_choice_PLUS_348fa54) =
  (match x with
  | `PLUS tok -> token env tok (* "+" *)
  | `STAR tok -> token env tok (* "*" *)
  | `QMARK tok -> token env tok (* "?" *)
  )

let map_boolean_literal (env : env) (x : CST.boolean_literal): G.literal =
  (match x with
  | `True tok -> G.Bool (true, token env tok) (* "true" *)
  | `False tok -> G.Bool (false, token env tok) (* "false" *)
  )

let map_char_literal (env : env) (tok : CST.char_literal) =
  token env tok (* char_literal *)

let map_metavariable (env : env) (tok : CST.metavariable) =
  token env tok (* pattern \$[a-zA-Z_]\w* *)

let map_block_comment (env : env) (tok : CST.block_comment) =
  token env tok (* block_comment *)

let map_line_comment (env : env) (tok : CST.line_comment) =
  token env tok (* line_comment *)

let map_raw_string_literal (env : env) (tok : CST.raw_string_literal) =
  token env tok (* raw_string_literal *)

let map_pat_785a82e (env : env) (tok : CST.pat_785a82e) =
  token env tok (* pattern [/_\-=->,;:::!=?.@*=/=&=#%=^=+<>|~]+ *)

let map_float_literal (env : env) (tok : CST.float_literal) =
  token env tok (* float_literal *)

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  token env tok (* escape_sequence *)

let map_tok_LT (env : env) (tok : CST.tok_LT) =
  token env tok (* tok_LT *)

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  (match x with
  | `Defa tok -> ident env tok (* "default" *)
  | `Union tok -> ident env tok (* "union" *)
  )

let map_pat_1e84e62 (env : env) (tok : CST.pat_1e84e62) =
  token env tok (* pattern [^+*?]+ *)

let map_pat_36c5a8e (env : env) (tok : CST.pat_36c5a8e) =
  token env tok (* pattern "b?\"" *)

let map_string_content (env : env) (tok : CST.string_content) =
  token env tok (* string_content *)

let map_primitive_type_token (env : env) (x : CST.anon_choice_u8_6dad923) =
  (match x with
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
  | `Char tok (* "char" *)
      -> tok
  )

let map_primitive_type_ident (env : env) (x : CST.anon_choice_u8_6dad923): (string * PI.token_mutable) =
  str env (map_primitive_type_token env x)

let map_primitive_type (env : env) (x : CST.anon_choice_u8_6dad923): G.type_ =
  let (s, tok) = map_primitive_type_ident env x in
  G.TyBuiltin (s, tok)

let map_identifier (env : env) (tok : CST.identifier) =
  token env tok (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)

let map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal): G.literal =
  let ldquote = token env v1 (* pattern "b?\"" *) in
  let strs =
    List.map (fun x ->
      (match x with
      | `Esc_seq tok -> let (s, _) = str env tok in s (* escape_sequence *)
      | `Str_content tok -> let (s, _) = str env tok in s (* string_content *)
      )
    ) v2 in
  let rdquote = token env v3 (* "\"" *) in
  G.String ((String.concat "" strs), ldquote)

let map_literal (env : env) (x : CST.literal): G.literal =
  (match x with
  | `Str_lit x -> map_string_literal env x
  | `Raw_str_lit tok -> G.String (str env tok) (* raw_string_literal *)
  | `Char_lit tok -> G.String (str env tok) (* char_literal *)
  | `Bool_lit x -> map_boolean_literal env x
  | `Int_lit tok -> G.Int (str env tok) (* integer_literal *)
  | `Float_lit tok -> G.Float (str env tok) (* float_literal *)
  )

let map_literal_token (env : env) (x : CST.literal): PI.token_mutable =
  let lit = map_literal env x in
  (match lit with
  | Bool (_, tok)
  | Int (_, tok)
  | Float (_, tok)
  | Char (_, tok)
  | String (_, tok)
  | Regexp (_, tok)
  | Unit tok
  | Null tok
  | Undefined tok
  | Imag (_, tok)
  | Ratio (_, tok)
  | Atom (_, tok)
   -> tok
  )

let map_literal_pattern (env : env) (x : CST.literal_pattern): G.pattern =
  (match x with
  | `Str_lit x -> G.PatLiteral (map_string_literal env x)
  | `Raw_str_lit tok -> G.PatLiteral (G.String (str env tok)) (* raw_string_literal *)
  | `Char_lit tok -> G.PatLiteral (G.Char (str env tok)) (* char_literal *)
  | `Bool_lit x -> G.PatLiteral (map_boolean_literal env x)
  | `Int_lit tok -> G.PatLiteral (G.Int (str env tok)) (* integer_literal *)
  | `Float_lit tok -> G.PatLiteral (G.Float (str env tok)) (* float_literal *)
  | `Nega_lit (v1, v2) ->
      let (neg, _) = str env v1 (* "-" *) in
      (match v2 with
        | `Int_lit tok -> let (int, tok) = str env tok in (* integer_literal *)
            G.PatLiteral (G.Int ((String.concat "" [neg; int]), tok))
        | `Float_lit tok -> let (float, tok) = str env tok in (* float_literal *)
            G.PatLiteral (G.Float (String.concat "" [neg; float], tok))
      )
  )

let map_extern_modifier (env : env) ((v1, v2) : CST.extern_modifier) =
  let v1 = token env v1 (* "extern" *) in
  let v2 =
    (match v2 with
    | Some x -> map_string_literal env x
    | None -> todo env ())
  in
  todo env (v1, v2)

let map_foreign_item_type (env : env) ((v1, v2, v3) : CST.foreign_item_type) =
  let v1 = token env v1 (* "type" *) in
  let v2 =
    token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  in
  let v3 = token env v3 (* ";" *) in
  todo env (v1, v2, v3)

let map_lifetime (env : env) ((v1, v2) : CST.lifetime): G.lifetime =
  let apostrophe = token env v1 (* "'" *) in
  let ident = ident env v2 in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  ident

let map_loop_label (env : env) ((v1, v2) : CST.loop_label) =
  let v1 = token env v1 (* "'" *) in
  let v2 =
    token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  in
  todo env (v1, v2)

let map_non_special_token (env : env) (x : CST.non_special_token): PI.token_mutable =
  (match x with
  | `Lit x -> map_literal_token env x
  | `Id tok -> token env tok (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  | `Meta tok -> token env tok (* pattern \$[a-zA-Z_]\w* *)
  | `Muta_spec tok -> token env tok (* "mut" *)
  | `Self tok -> token env tok (* "self" *)
  | `Super tok -> token env tok (* "super" *)
  | `Crate tok -> token env tok (* "crate" *)
  | `Choice_u8 x -> let (_, tok) = map_primitive_type_ident env x in tok
  | `Pat_785a82e tok -> token env tok (* pattern [/_\-=->,;:::!=?.@*=/=&=#%=^=+<>|~]+ *)
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
  | `While tok -> token env tok (* "while" *)
  )

let map_function_modifiers (env : env) (xs : CST.function_modifiers) =
  List.map (fun x ->
    (match x with
    | `Async tok -> token env tok (* "async" *)
    | `Defa tok -> token env tok (* "default" *)
    | `Const tok -> token env tok (* "const" *)
    | `Unsafe tok -> token env tok (* "unsafe" *)
    | `Extern_modi x -> map_extern_modifier env x
    )
  ) xs

let map_for_lifetimes (env : env) ((v1, v2, v3, v4, v5, v6) : CST.for_lifetimes) =
  let v1 = token env v1 (* "for" *) in
  let v2 = token env v2 (* "<" *) in
  let v3 = map_lifetime env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_lifetime env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 =
    (match v5 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v6 = token env v6 (* ">" *) in
  todo env (v1, v2, v3, v4, v5, v6)

let rec map_token_tree (env : env) (x : CST.token_tree): G.any list list G.bracket =
  (match x with
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
  )

and map_tokens (env : env) (x : CST.tokens): G.any list =
  (match x with
  | `Tok_tree x -> [G.MacTkTree (map_token_tree env x)]
  | `Tok_repe (v1, v2, v3, v4, v5, v6) ->
      let dollar = token env v1 (* "$" *) in
      let lparen = token env v2 (* "(" *) in
      let tokens = List.map (map_tokens env) v3 in
      let rparen = token env v4 (* ")" *) in
      let ident = Option.map (fun tok -> ident env tok) v5 in (* pattern [^+*?]+ *)
      let quantifier = map_token_quantifier env v6 in
      [G.MacTks ((lparen, tokens, rparen), ident, quantifier)]
  | `Choice_lit x -> [G.Tk (map_non_special_token env x)]
  )

let rec map_token_pattern (env : env) (x : CST.token_pattern): G.rust_macro_pattern =
  (match x with
  | `Tok_tree_pat x -> G.RustMacPatTree (map_token_tree_pattern env x)
  | `Tok_repe_pat (v1, v2, v3, v4, v5, v6) ->
      let dollar = token env v1 (* "$" *) in
      let lparen = token env v2 (* "(" *) in
      let patterns = List.map (map_token_pattern env) v3 in
      let rparen = token env v4 (* ")" *) in
      let ident = Option.map (fun tok -> ident env tok) v5 in (* pattern [^+*?]+ *)
      let quantifier = map_token_quantifier env v6 in
      G.RustMacPatRepetition ((lparen, patterns, rparen), ident, quantifier)
  | `Tok_bind_pat (v1, v2, v3) ->
      let ident = ident env v1 (* pattern \$[a-zA-Z_]\w* *) in
      let colon = token env v2 (* ":" *) in
      let fragment_specifier = map_fragment_specifier env v3 in
      G.RustMacPatBinding (ident, fragment_specifier)
  | `Choice_lit x -> G.RustMacPatToken (map_non_special_token env x)
  )

and map_token_tree_pattern (env : env) (x : CST.token_tree_pattern): G.rust_macro_pattern list =
  (match x with
  | `LPAR_rep_tok_pat_RPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = List.map (map_token_pattern env) v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  | `LBRACK_rep_tok_pat_RBRACK (v1, v2, v3) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = List.map (map_token_pattern env) v2 in
      let v3 = token env v3 (* "]" *) in
      todo env (v1, v2, v3)
  | `LCURL_rep_tok_pat_RCURL (v1, v2, v3) ->
      let v1 = token env v1 (* "{" *) in
      let v2 = List.map (map_token_pattern env) v2 in
      let v3 = token env v3 (* "}" *) in
      todo env (v1, v2, v3)
  )

and map_macro_rule (env : env) ((v1, v2, v3) : CST.macro_rule) =
  let pattern = map_token_tree_pattern env v1 in
  let arrow = token env v2 (* "=>" *) in
  let body = map_token_tree env v3 in
  todo env (v1, v2, v3)

let rec map_abstract_type_trait_name (env : env) (x : CST.anon_choice_field_id_02b4436): G.type_ =
  (match x with
  | `Id tok -> let ident = ident env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      G.TyId (ident, G.empty_id_info ())
  | `Scoped_type_id x -> map_scoped_type_identifier env x
  | `Gene_type x -> let name = map_generic_type env x in
      G.TyIdQualified (name, G.empty_id_info ())
  | `Func_type x -> map_function_type env x
  )

and map_struct_name (env : env) (x : CST.anon_choice_field_id_2c46bcf): G.name =
  (match x with
  | `Id tok -> let ident = ident env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      (ident, { G.name_qualifier = None; G.name_typeargs = None })
  | `Scoped_type_id x -> map_scoped_type_identifier_name env x
  )

and map_tuple_struct_name (env : env) (x : CST.anon_choice_field_id_f1f5a37): G.name =
  (match x with
  | `Id tok -> let ident = ident env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      (ident, { G.name_qualifier = None; G.name_typeargs = None })
  | `Scoped_id x -> map_scoped_identifier_name env x
  )

and map_struct_pattern_field (env : env) (x : CST.anon_choice_field_pat_8e757e8): (G.name * G.pattern option) =
  (match x with
  | `Field_pat (v1, v2, v3) ->
      let reference =
        (match v1 with
        | Some tok -> token env tok (* "ref" *)
        | None -> todo env ())
      in
      let mutability =
        (match v2 with
        | Some tok -> token env tok (* "mut" *)
        | None -> todo env ())
      in
      (match v3 with
        | `Id tok -> let ident = ident env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
            let name = (ident, G.empty_name_info) in
            (name, None)
        | `Id_COLON_pat (v1, v2, v3) ->
            let ident = ident env v1 in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
            let colon = token env v2 (* ":" *) in
            let pat = map_pattern env v3 in
            let name = (ident, G.empty_name_info) in
            (name, Some pat))
  | `Rema_field_pat tok -> let ident = ident env tok in (* ".." *)
      let name = (ident, G.empty_name_info) in
      (name, Some (G.PatRemaining (token env tok)))
  )

and map_type_parameter (env : env) (x : CST.anon_choice_life_859e88f): G.type_parameter =
  (match x with
   | `Life x -> let lt = map_lifetime env x in
       (lt, [G.TyParamLifetime])
  | `Meta tok -> let ident = ident env tok in (* pattern \$[a-zA-Z_]\w* *)
      (ident, [G.TyParamMetavar])
  | `Id tok -> let ident = ident env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      (ident, [])
  | `Cons_type_param x -> map_constrained_type_parameter env x
  | `Opt_type_param (v1, v2, v3) ->
      let (ident, _ ) as type_param =
        (match v1 with
        | `Id tok -> let ident = ident env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
            (ident, [])
        | `Cons_type_param x -> map_constrained_type_parameter env x
        )
      in
      let equal = token env v2 (* "=" *) in
      let default_ty = map_type_ env v3 in
      (ident, [G.TyParamOptional (type_param, default_ty)])
  | `Const_param (v1, v2, v3, v4) ->
      let const = token env v1 in (* "const" *)
      let ident = ident env v2 in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let colon = token env v3 in (* ":" *)
      let ty = map_type_ env v4 in
      (ident, [G.TyParamConst ty])
  )

and map_range_pattern_bound (env : env) (x : CST.anon_choice_lit_pat_0884ef0): G.pattern =
  (match x with
  | `Lit_pat x -> map_literal_pattern env x
  | `Choice_self x -> let expr = map_path env x in
      G.PatName expr
  )

and map_anon_choice_meta_item_fefa160 (env : env) (x : CST.anon_choice_meta_item_fefa160) =
  (match x with
  | `Meta_item x -> map_meta_item env x
  | `Lit x -> map_literal env x
  )

and map_anon_choice_param_2c23cdc (env : env) outer_attr (x : CST.anon_choice_param_2c23cdc): G.parameter =
  (match x with
  | `Param x -> map_parameter env x
  | `Self_param (v1, v2, v3, v4) ->
      let borrow = Option.map (fun tok ->
        let t = token env tok in (* "&" *)
        G.KeywordAttr (G.Borrowed, t)
      )
      v1 in
      let lifetime = Option.map (fun x -> map_lifetime env x) v2 in
      let mutability = Option.map (fun tok ->
        let t = token env tok in (* "mut" *)
        G.KeywordAttr (G.Mutable, t)
      )
      v3 in
      let attrs = deoptionalize [borrow; mutability] in
      let self = ident env v4 (* "self" *) in
      let param = {
        G.pname = Some self;
        pdefault = None;
        ptype = None;
        pattrs = attrs;
        pinfo = G.empty_id_info ();
      } in
      G.ParamClassic param
  | `Vari_param tok -> let ellipsis = token env tok in (* "..." *)
      G.ParamEllipsis ellipsis
  | `X__ tok -> let underscore = token env tok in (* "_" *)
      G.ParamEllided underscore
  | `Type x -> let ty = map_type_ env x in
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
  )

and map_closure_parameter (env : env) (x : CST.anon_choice_pat_4717dcc): G.parameter =
  (match x with
   | `Pat x -> let pattern = map_pattern env x in
       G.ParamPattern pattern
  | `Param x -> map_parameter env x
  )

and map_field_initializer (env : env) (x : CST.anon_choice_shor_field_init_9cb4441): G.expr =
  (match x with
  | `Shor_field_init (v1, v2) ->
      let outer_attrs = List.map (map_outer_attribute_item env) v1 in
      let ident = ident env v2 in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let lhs = G.Id (ident, G.empty_id_info ()) in
      (* bound variable with same ident as field name *)
      let rhs = G.Id (ident, G.empty_id_info ()) in
      G.Assign (lhs, G.fake ":", rhs)
  | `Field_init (v1, v2, v3, v4) ->
      let outer_attrs = List.map (map_outer_attribute_item env) v1 in
      let ident = ident env v2 in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let lhs = G.Id (ident, G.empty_id_info ()) in
      let colon = token env v3 (* ":" *) in
      let rhs = map_expression env v4 in
      G.Assign (lhs, colon, rhs)
  | `Base_field_init x -> map_base_field_initializer env x
  )

and map_type_argument (env : env) (x : CST.anon_choice_type_39799c3): G.type_argument =
  (match x with
  | `Type x -> let ty = map_type_ env x in
      G.TypeArg ty
  | `Type_bind (v1, v2, v3) ->
      let ident = ident env v1 in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let equals = token env v2 (* "=" *) in
      let ty = map_type_ env v3 in
      G.TypeBinding (ident, ty)
  | `Life x -> let lifetime = map_lifetime env x in
      G.TypeLifetime lifetime
  | `Lit x -> let literal = map_literal env x in
      G.TypeLiteral literal
  | `Blk x -> let block_expr = map_block_expr env x in
      G.TypeBlock block_expr
  )

and map_trait_bound (env : env) (x : CST.anon_choice_type_d689819): G.trait_bound =
  (match x with
   | `Type x -> let ty = map_type_ env x in
       G.TraitBoundType ty
   | `Life x -> let lt = map_lifetime env x in
       G.TraitBoundLifetime lt
   | `Higher_ranked_trait_bound x ->
       let (type_params, type_) = map_higher_ranked_trait_bound env x in
       G.TraitBoundHigherRanked (type_params, type_)
  | `Remo_trait_bound (v1, v2) ->
      let qmark = token env v1 (* "?" *) in
      let ty = map_type_ env v2 in
      G.TraitBoundRemoved ty
  )

and map_tuple_pattern_list (env : env) ((v1, v2) : CST.anon_pat_rep_COMMA_pat_2a80f16): G.pattern list =
  let pattern_first = map_pattern env v1 in
  let pattern_rest =
    List.map (fun (v1, v2) ->
      let comma = token env v1 (* "," *) in
      let pattern = map_pattern env v2 in
      pattern
    ) v2
  in
  (pattern_first::pattern_rest)

and map_arguments (env : env) ((v1, v2, v3, v4) : CST.arguments) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = List.map (map_outer_attribute_item env) v1 in
        let v2 = map_expression env v2 in
        let v3 =
          List.map (fun (v1, v2, v3) ->
            let v1 = token env v1 (* "," *) in
            let v2 = List.map (map_outer_attribute_item env) v2 in
            let v3 = map_expression env v3 in
            todo env (v1, v2, v3)
          ) v3
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v4 = token env v4 (* ")" *) in
  todo env (v1, v2, v3, v4)

and map_associated_type (env : env) ((v1, v2, v3, v4, v5, v6) : CST.associated_type) =
  let v1 = token env v1 (* "type" *) in
  let v2 =
    token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  in
  let v3 =
    (match v3 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x -> map_trait_bounds env x
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v6 = token env v6 (* ";" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_attribute (env : env) ((v1, v2, v3) : CST.attribute) =
  let v1 = token env v1 (* "[" *) in
  let v2 = map_meta_item env v2 in
  let v3 = token env v3 (* "]" *) in
  todo env (v1, v2, v3)

and map_base_field_initializer (env : env) ((v1, v2) : CST.base_field_initializer) =
  let dots = token env v1 (* ".." *) in
  let lhs = G.IdSpecial (G.Spread, dots) in
  (* Copy remaining struct fields from this existing instance *)
  let rhs = map_expression env v2 in
  G.AssignOp (lhs, (G.Append, dots), rhs)

and map_binary_expression (env : env) (x : CST.binary_expression): G.expr =
  (match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.And, v2), fb [G.Arg v1; G.Arg v3])
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Or, v2), fb [G.Arg v1; G.Arg v3])
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitAnd, v2), fb [G.Arg v1; G.Arg v3])
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitOr, v2), fb [G.Arg v1; G.Arg v3])
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitXor, v2), fb [G.Arg v1; G.Arg v3])
  | `Exp_choice_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let (tok, op) =
        (match v2 with
        | `EQEQ tok -> (token env tok, G.Eq) (* "==" *)
        | `BANGEQ tok -> (token env tok, G.NotEq) (* "!=" *)
        | `LT tok -> (token env tok, G.Lt) (* "<" *)
        | `LTEQ tok -> (token env tok, G.LtE) (* "<=" *)
        | `GT tok -> (token env tok, G.Gt) (* ">" *)
        | `GTEQ tok -> (token env tok, G.GtE) (* ">=" *)
        )
      in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op op, tok), fb [G.Arg v1; G.Arg v3])
  | `Exp_choice_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let (tok, op) =
        (match v2 with
        | `LTLT tok -> (token env tok, G.LSL) (* "<<" *)
        (* According to https://doc.rust-lang.org/reference/expressions/operator-expr.html#arithmetic-and-logical-binary-operators: *)
        (* "Arithmetic right shift on signed integer types, logical right shift on unsigned integer types." *)
        | `GTGT tok -> (token env tok, G.LSR) (* ">>" *)
        )
      in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op op, tok), fb [G.Arg v1; G.Arg v3])
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let (tok, op) =
        (match v2 with
        | `PLUS tok -> (token env tok, G.Plus) (* "+" *)
        | `DASH tok -> (token env tok, G.Minus) (* "-" *)
        )
      in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op op, tok), fb [G.Arg v1; G.Arg v3])
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let (tok, op) =
        (match v2 with
        | `STAR tok -> (token env tok, G.Mult) (* "*" *)
        | `SLASH tok -> (token env tok, G.Div) (* "/" *)
        | `PERC tok -> (token env tok, G.Mod) (* "%" *)
        )
      in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op op, tok), fb [G.Arg v1; G.Arg v3])
  )

and map_block (env : env) ((v1, v2, v3, v4) : CST.block): G.stmt =
  let lbrace = token env v1 (* "{" *) in
  let stmts = List.map (map_statement env) v2 in
  let stmts_and_expr =
    (match v3 with
     | Some x -> let expr = map_expression env x in
         let stmt = G.ExprStmt (expr, sc) |> G.s in
         List.concat [stmts; [stmt]]
    | None -> stmts)
  in
  let rbrace = token env v4 (* "}" *) in
  G.Block (lbrace, stmts, rbrace) |> G.s

and map_block_expr (env : env) ((v1, v2, v3, v4) : CST.block): G.expr =
  let block = map_block env (v1, v2, v3, v4) in
  G.OtherExpr (G.OE_StmtExpr, [G.S block])

and map_bounded_type (env : env) (x : CST.bounded_type) =
  (match x with
  | `Life_PLUS_type (v1, v2, v3) ->
      let v1 = map_lifetime env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Type_PLUS_type (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Type_PLUS_life (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = map_lifetime env v3 in
      todo env (v1, v2, v3)
  )

and map_bracketed_type (env : env) ((v1, v2, v3) : CST.bracketed_type) =
  let lthan = token env v1 (* "<" *) in
  let ty =
    (match v2 with
    | `Type x -> map_type_ env x
    | `Qual_type x -> map_qualified_type env x)
  in
  let gthan = token env v3 (* ">" *) in
  (lthan, ty, gthan)

and map_closure_parameters (env : env) ((v1, v2, v3) : CST.closure_parameters) =
  let v1 = token env v1 (* "|" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_closure_parameter env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_closure_parameter env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "|" *) in
  todo env (v1, v2, v3)

and map_const_block (env : env) ((v1, v2) : CST.const_block): G.expr =
  let const = token env v1 (* "const" *) in
  let block = map_block env v2 in
  G.OtherExpr (G.OE_StmtExpr, [G.S block]) (* TODO make ConstBlock? *)

and map_const_item (env : env) ((v1, v2, v3, v4, v5, v6) : CST.const_item) =
  let v1 = token env v1 (* "const" *) in
  let v2 =
    token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  in
  let v3 = token env v3 (* ":" *) in
  let v4 = map_type_ env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v6 = token env v6 (* ";" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_constrained_type_parameter (env : env) ((v1, v2) : CST.constrained_type_parameter) =
  let v1 =
    (match v1 with
    | `Life x -> map_lifetime env x
    | `Id tok -> ident env tok (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
    )
  in
  let v2 = map_trait_bounds env v2 in
  todo env (v1, v2)

and map_else_clause (env : env) ((v1, v2) : CST.else_clause): G.stmt =
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

and map_enum_variant (env : env) ((v1, v2, v3, v4) : CST.enum_variant) =
  let v1 =
    (match v1 with
    | Some x -> map_visibility_modifier env x
    | None -> todo env ())
  in
  let v2 =
    token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  in
  let v3 =
    (match v3 with
    | Some x ->
        (match x with
        | `Field_decl_list x -> map_field_declaration_list env x
        | `Orde_field_decl_list x ->
            map_ordered_field_declaration_list env x
        )
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_enum_variant_list (env : env) ((v1, v2, v3, v4) : CST.enum_variant_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = List.map (map_outer_attribute_item env) v1 in
        let v2 = map_enum_variant env v2 in
        let v3 =
          List.map (fun (v1, v2, v3) ->
            let v1 = token env v1 (* "," *) in
            let v2 = List.map (map_outer_attribute_item env) v2 in
            let v3 = map_enum_variant env v3 in
            todo env (v1, v2, v3)
          ) v3
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

and map_expression (env : env) (x : CST.expression): G.expr =
  (match x with
  | `Un_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `DASH tok -> token env tok (* "-" *)
        | `STAR tok -> token env tok (* "*" *)
        | `BANG tok -> token env tok (* "!" *)
        )
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Ref_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "&" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* "mut" *)
        | None -> todo env ())
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Try_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "?" *) in
      todo env (v1, v2)
  | `Bin_exp x -> map_binary_expression env x
  | `Assign_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Comp_assign_expr (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUSEQ tok -> token env tok (* "+=" *)
        | `DASHEQ tok -> token env tok (* "-=" *)
        | `STAREQ tok -> token env tok (* "*=" *)
        | `SLASHEQ tok -> token env tok (* "/=" *)
        | `PERCEQ tok -> token env tok (* "%=" *)
        | `AMPEQ tok -> token env tok (* "&=" *)
        | `BAREQ tok -> token env tok (* "|=" *)
        | `HATEQ tok -> token env tok (* "^=" *)
        | `LTLTEQ tok -> token env tok (* "<<=" *)
        | `GTGTEQ tok -> token env tok (* ">>=" *)
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Type_cast_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "as" *) in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Range_exp x -> map_range_expression env x
  | `Call_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_arguments env v2 in
      todo env (v1, v2)
  | `Ret_exp x -> map_return_expression env x
  | `Lit x -> G.L (map_literal env x)
  | `Id tok -> G.Id (ident env tok, G.empty_id_info ()) (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  | `Choice_u8 x -> let tok = map_primitive_type_token env x in
      G.Id (ident env tok, G.empty_id_info ())
  | `Choice_defa x -> let ident = map_reserved_identifier env x in
      G.Id (ident, G.empty_id_info ())
  | `Self tok -> G.IdSpecial (G.Self, token env tok) (* "self" *)
  | `Scoped_id x -> map_scoped_identifier env x
  | `Gene_func (v1, v2, v3) ->
      let colons = token env v2 (* "::" *) in
      let typeargs = map_type_arguments env v3 in
      (match v1 with
       | `Id tok ->
           let ident = ident env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
           let name = (ident, { G.name_qualifier = None; G.name_typeargs = Some typeargs }) in
           G.IdQualified (name, G.empty_id_info ())
       | `Scoped_id x -> let id_qualified = map_scoped_identifier env x in
           let (ident, name_qualifier) = (match id_qualified with
             | G.IdQualified ((ident, { name_qualifier; name_typeargs = None; _ }), _) -> (ident, name_qualifier)
             | _ -> raise Impossible
           ) in
           (* TODO is this correct? *)
           let name = (ident, { G.name_qualifier = name_qualifier; G.name_typeargs = Some typeargs }) in
           G.IdQualified (name, G.empty_id_info ())
       | `Field_exp x -> map_field_expression env x (Some typeargs))

  | `Await_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* "await" *) in
      todo env (v1, v2, v3)
  | `Field_exp x -> map_field_expression env x None
  | `Array_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = List.map (map_outer_attribute_item env) v2 in
      let v3 =
        (match v3 with
        | `Exp_SEMI_exp (v1, v2, v3) ->
            let v1 = map_expression env v1 in
            let v2 = token env v2 (* ";" *) in
            let v3 = map_expression env v3 in
            todo env (v1, v2, v3)
        | `Opt_exp_rep_COMMA_exp_opt_COMMA (v1, v2) ->
            let v1 =
              (match v1 with
              | Some (v1, v2) ->
                  let v1 = map_expression env v1 in
                  let v2 =
                    List.map (fun (v1, v2) ->
                      let v1 = token env v1 (* "," *) in
                      let v2 = map_expression env v2 in
                      todo env (v1, v2)
                    ) v2
                  in
                  todo env (v1, v2)
              | None -> todo env ())
            in
            let v2 =
              (match v2 with
              | Some tok -> token env tok (* "," *)
              | None -> todo env ())
            in
            todo env (v1, v2)
        )
      in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Tuple_exp (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = List.map (map_outer_attribute_item env) v2 in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* "," *) in
      let v5 =
        List.map (fun (v1, v2) ->
          let v1 = map_expression env v1 in
          let v2 = token env v2 (* "," *) in
          todo env (v1, v2)
        ) v5
      in
      let v6 =
        (match v6 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v7 = token env v7 (* ")" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Macro_invo x -> map_macro_invocation env x
  | `Unit_exp (v1, v2) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = token env v2 (* ")" *) in
      todo env (v1, v2)
  | `Choice_unsafe_blk x -> map_expression_ending_with_block env x
  | `Brk_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "break" *) in
      let v2 =
        (match v2 with
        | Some x -> map_loop_label env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Cont_exp (v1, v2) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 =
        (match v2 with
        | Some x -> map_loop_label env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Index_exp (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Meta tok -> let tok = token env tok in (* pattern \$[a-zA-Z_]\w* *)
      G.Metavar tok
  | `Clos_exp (v1, v2, v3) ->
      let is_move = Option.map (fun tok ->
        let tok = token env tok in (* "move" *)
        G.KeywordAttr (G.Move, tok)
      ) v1 in
      let params = map_closure_parameters env v2 in
      let (ret_type, body) =
        (match v3 with
        | `Opt_DASHGT_type_blk (v1, v2) ->
            let ret_type = Option.map (fun (v1, v2) ->
              let arrow = token env v1 (* "->" *) in
              let ty = map_type_ env v2 in
              ty
            ) v1 in
            let body = map_block env v2 in
            (ret_type, body)
        | `Exp x -> let expr = map_expression env x in
            (None, G.ExprStmt (expr, sc) |> G.s)
        )
      in
      let func_def = {
        G.fkind = (G.LambdaKind, G.fake "closure");
        G.fparams = params;
        G.frettype = ret_type;
        G.fbody = body;
      } in
      G.Lambda func_def
  | `Paren_exp (v1, v2, v3) ->
      let lparen = token env v1 (* "(" *) in
      let expr = map_expression env v2 in
      let rparen = token env v3 (* ")" *) in
      expr
  | `Struct_exp (v1, v2) ->
      let name: G.name =
        (match v1 with
        | `Id tok -> let ident = ident env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
            (ident, G.empty_name_info)
        | `Scoped_type_id_in_exp_posi x ->
            map_scoped_type_identifier_in_expression_position env x
        | `Gene_type_with_turb x -> map_generic_type_with_turbofish env x
        )
      in
      let fields = map_field_initializer_list env v2 in
      G.Constructor (name, fields)
  )

and map_expression_ending_with_block (env : env) (x : CST.expression_ending_with_block): G.expr =
  let map_loop_label_ (v1, v2) =
    let loop_label = map_loop_label env v1 in
    let colon = token env v2 (* ":" *) in
    loop_label
  in
  (match x with
   | `Unsafe_blk (v1, v2) ->
       let unsafe = token env v1 (* "unsafe" *) in
       let block = map_block_expr env v2 in
       block (* TODO UnsafeBlock? *)
   | `Async_blk (v1, v2, v3) ->
       let async = token env v1 (* "async" *) in
       let move = Option.map (fun tok -> token env tok (* "move" *)) v2 in
       let block = map_block_expr env v3 in
       block (* TODO AsyncBlock? *)
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
       let while_stmt = G.While(while_, cond, body) |> G.s in
       G.OtherExpr (G.OE_StmtExpr, [G.S while_stmt])
   | `While_let_exp (v1, v2, v3, v4, v5, v6, v7) ->
       let loop_label = Option.map map_loop_label_ v1 in
       let while_ = token env v2 (* "while" *) in
       let let_ = token env v3 (* "let" *) in
       let pattern = map_pattern env v4 in
       let equals = token env v5 (* "=" *) in
       let cond = map_expression env v6 in
       let body = map_block env v7 in
       let while_let = G.WhileLet (while_, pattern, cond, body) |> G.s in
       G.OtherExpr (G.OE_StmtExpr, [G.S while_let])
   | `Loop_exp (v1, v2, v3) ->
       let loop_label = Option.map map_loop_label_ v1 in
       let loop = token env v2 (* "loop" *) in
       let body = map_block env v3 in
       let loop_stmt = G.LoopStmt (loop, body) |> G.s in
       G.OtherExpr (G.OE_StmtExpr, [G.S loop_stmt])
   | `For_exp (v1, v2, v3, v4, v5, v6) ->
       let loop_label = Option.map map_loop_label_ v1 in
       let for_ = token env v2 (* "for" *) in
       let pattern = map_pattern env v3 in
       let in_ = token env v4 (* "in" *) in
       let expr = map_expression env v5 in
       let body = map_block env v6 in
       let for_header = G.ForEach (pattern, in_, expr) in
       let for_stmt = G.For (for_, for_header, body) |> G.s in
       G.OtherExpr (G.OE_StmtExpr, [G.S for_stmt])
   | `Const_blk x -> map_const_block env x
  )

and map_expression_statement (env : env) (x : CST.expression_statement) =
  (match x with
  | `Exp_SEMI (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ";" *) in
      todo env (v1, v2)
  | `Choice_unsafe_blk x ->
      map_expression_ending_with_block env x
  )

and map_field_declaration (env : env) ((v1, v2, v3, v4) : CST.field_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_visibility_modifier env x
    | None -> todo env ())
  in
  let v2 =
    token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  in
  let v3 = token env v3 (* ":" *) in
  let v4 = map_type_ env v4 in
  todo env (v1, v2, v3, v4)

and map_field_declaration_list (env : env) ((v1, v2, v3, v4) : CST.field_declaration_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = List.map (map_outer_attribute_item env) v1 in
        let v2 = map_field_declaration env v2 in
        let v3 =
          List.map (fun (v1, v2, v3) ->
            let v1 = token env v1 (* "," *) in
            let v2 = List.map (map_outer_attribute_item env) v2 in
            let v3 = map_field_declaration env v3 in
            todo env (v1, v2, v3)
          ) v3
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) (typeargs : G.type_arguments option): G.expr =
  let expr = map_expression env v1 in
  let dot = token env v2 (* "." *) in
  let ident_or_dyn = (match v3 with
   | `Id tok -> let ident = ident env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
       (match typeargs with
        | Some tas ->
            let name = (ident, { G.name_qualifier = None; G.name_typeargs = Some tas }) in
            G.EName name
        | None -> G.EId (ident, G.empty_id_info ()))

   | `Int_lit tok -> let literal = G.L (G.Int (str env tok)) in (* integer_literal *)
       (match typeargs with
        | Some tas -> raise Impossible
        | None -> G.EDynamic literal))
  in
  G.DotAccess (expr, dot, ident_or_dyn)

and map_field_initializer_list (env : env) ((v1, v2, v3, v4) : CST.field_initializer_list): G.expr list =
  let lbrace = token env v1 (* "{" *) in
  let fields = (match v2 with
    | Some (v1, v2) ->
        let field_first = map_field_initializer env v1 in
        let field_rest =
          List.map (fun (v1, v2) ->
            let comma = token env v1 (* "," *) in
            let field = map_field_initializer env v2 in
            field
          ) v2 in
        (field_first::field_rest)
    | None -> [])
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) in
  let rbrace = token env v4 (* "}" *) in
  fields

and map_foreign_block_item (env : env) ((v1, v2, v3) : CST.foreign_block_item) =
  let v1 = List.map (map_outer_attribute_item env) v1 in
  let v2 =
    (match v2 with
    | Some x -> map_visibility_modifier env x
    | None -> todo env ())
  in
  let stmt =
    (match v3 with
    | `Fore_item_static x -> map_foreign_item_static env x
    | `Func_sign_with_defa_item x ->
        let defn = map_function_signature_with_default_item env x in
        G.DefStmt defn |> G.s
    | `Fore_item_type x -> map_foreign_item_type env x
    | `Macro_invo x -> let invo = map_macro_invocation env x in
        G.ExprStmt (invo, sc) |> G.s
    )
  in
  todo env (v1, v2, v3)

and map_foreign_item_static (env : env) ((v1, v2, v3, v4, v5, v6) : CST.foreign_item_static): G.stmt =
  let static = token env v1 (* "static" *) in
  let static_attr = G.KeywordAttr (G.Static, static) in
  let mutability = Option.map (fun tok ->
    let tok = token env tok (* "mut" *) in
    G.KeywordAttr (G.Mutable, tok)
  ) v2 in
  let ident = ident env v3 in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let colon = token env v4 (* ":" *) in
  let ty = map_type_ env v5 in
  let semicolon = token env v6 (* ";" *) in
  let var_def = {
    G.vinit = None;
    G.vtype = Some ty;
  } in
  let ent = {
    G.name = G.EId (ident, G.empty_id_info ());
    G.attrs = deoptionalize [Some static_attr; mutability];
    G.tparams = []
  } in
  G.DefStmt (ent, (G.VarDef var_def)) |> G.s

and map_foreign_mod_block (env : env) ((v1, v2, v3, v4) : CST.foreign_mod_block) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (map_inner_attribute_item env) v2 in
  let v3 = List.map (map_foreign_block_item env) v3 in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

and map_function_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.function_declaration): function_declaration_rs =
  let name: G.ident_or_dynamic =
    (match v1 with
    | `Id tok ->
        let ident = ident env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
        G.EName (ident, {G.name_qualifier = None; G.name_typeargs = None})
    | `Meta tok ->
        let metavar = token env tok in (* pattern \$[a-zA-Z_]\w* *)
        G.EDynamic (G.Metavar metavar)
    )
  in
  let type_params = Option.map (fun x -> map_type_parameters env x) v2 in
  let params = map_parameters env v3 in
  let retval = Option.map (fun (v1, v2) ->
    let arrow = token env v1 (* "->" *) in
    map_type_ env v2)
  v4 in
  let where_clause = Option.map (fun x -> map_where_clause env x) v5 in
  { name; type_params; params; retval; where_clause }

and map_function_item (env : env) ((v1, v2, v3, v4) : CST.function_item): G.stmt =
  let fn_modifiers = Option.map (fun x -> map_function_modifiers env x) v1 in
  let id = token env v2 (* "fn" *) in
  let fn_decl = map_function_declaration env v3 in
  let body = map_block env v4 in
  let ent = { G.name = fn_decl.name; G.attrs = []; G.tparams = [] } in
  let fn_def = {
    G.fparams = fn_decl.params;
    G.frettype = None;
    G.fkind = (G.Function, id);
    G.fbody = body
  } in
  G.DefStmt (ent, G.FuncDef fn_def) |> G.s

and map_function_signature_with_default_item (env : env) ((v1, v2, v3, v4) : CST.function_signature_with_default_item): G.definition =
  let modifiers = Option.map (fun x -> map_function_modifiers env x) v1 in
  let fn = token env v2 (* "fn" *) in
  let fn_decl = map_function_declaration env v3 in
  let default_impl =
    (match v4 with
    | `SEMI tok -> let semicolon = token env tok in (* ";" *)
        (* No default implementation *)
        G.OtherStmt (G.OS_NoDefaultImpl, []) |> G.s
    | `Blk x -> map_block env x
    )
  in
  let fn_def = {
    G.fkind = (G.Method, fn);
    G.fparams = _decl.params;
    G.frettype = fn_decl.retval;
    G.fbody = default_impl
  } in
  let ent = {
    G.name = fn_decl.name;
    G.attrs = [];
    G.tparams = fn_decl.type_params;
  } in
  (ent, G.FuncDef fn_def)

and map_function_type (env : env) ((v1, v2, v3, v4) : CST.function_type): G.type_ =
  let lifetimes =
    (match v1 with
    | Some x -> map_for_lifetimes env x
    | None -> [])
  in
  let (trait, modifiers) =
    (match v2 with
    | `Choice_id x -> let trait_name = map_struct_name env x in (* FnOnce, FnMut... *)
        (Some trait_name, None)
    | `Opt_func_modifs_fn (v1, v2) ->
        let modifiers = Option.map (fun x -> map_function_modifiers env x) v1 in
        let fn = token env v2 (* "fn" *) in
        (None, modifiers)
    )
  in
  let params = map_parameters env v3 in
  let ret_type =
    (match v4 with
    | Some (v1, v2) ->
        let arrow = token env v1 (* "->" *) in
        let ty = map_type_ env v2 in
        ty
    | None -> G.TyBuiltin (fake_id "()"))
  in
  G.TyFun (params, ret_type) (* TODO lifetimes, modifiers, traits *)

and map_generic_type (env : env) ((v1, v2) : CST.generic_type): G.name =
  let (ident, info) = map_struct_name env v1 in
  let typeargs = map_type_arguments env v2 in
  (ident, { G.name_qualifier = info.name_qualifier; G.name_typeargs = Some typeargs })

and map_generic_type_with_turbofish (env : env) ((v1, v2, v3) : CST.generic_type_with_turbofish): G.name =
  let (ident, info) = map_tuple_struct_name env v1 in
  let colons = token env v2 (* "::" *) in
  let typeargs = map_type_arguments env v3 in
  (ident, { G.name_qualifier = info.name_qualifier; G.name_typeargs = Some typeargs })

and map_higher_ranked_trait_bound (env : env) ((v1, v2, v3) : CST.higher_ranked_trait_bound): (G.type_parameter list * G.type_) =
  let for_ = token env v1 (* "for" *) in
  let type_parameters = map_type_parameters env v2 in
  let type_ = map_type_ env v3 in
  (type_parameters, type_)

and map_if_expression (env : env) ((v1, v2, v3, v4) : CST.if_expression): G.expr =
  let if_ = token env v1 (* "if" *) in
  let cond = map_expression env v2 in
  let body = map_block env v3 in
  let else_ = Option.map (fun x -> map_else_clause env x) v4 in
  let if_stmt = G.If (if_, cond, body, else_) |> G.s in
  G.OtherExpr (G.OE_StmtExpr, [G.S if_stmt])

and map_if_let_expression (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.if_let_expression): G.expr =
  let if_ = token env v1 (* "if" *) in
  let let_ = token env v2 (* "let" *) in
  let pattern = map_pattern env v3 in
  let equals = token env v4 (* "=" *) in
  let cond = map_expression env v5 in
  let body = map_block env v6 in
  let else_ = Option.map (fun x -> map_else_clause env x) v7 in
  let if_let = G.IfLet (if_, pattern, cond, body, else_) |> G.s in
  G.OtherExpr (G.OE_StmtExpr, [G.S if_let])

and map_impl_block (env : env) ((v1, v2, v3, v4) : CST.impl_block) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (map_inner_attribute_item env) v2 in
  let v3 = List.map (map_impl_block_item env) v3 in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

and map_impl_block_item (env : env) ((v1, v2, v3) : CST.impl_block_item): G.stmt =
  let outer_attrs = List.map (map_outer_attribute_item env) v1 in
  let visibility =
    (match v2 with
    | Some x -> map_visibility_modifier env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | `Impl_blk_item_const x -> map_impl_block_item_const env x
    | `Func_item x -> map_function_item env x
    | `Impl_blk_item_type x -> map_impl_block_item_type env x
    | `Macro_invo x -> map_macro_invocation env x
    )
  in
  todo env (v1, v2, v3)

and map_impl_block_item_const (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.impl_block_item_const) =
  let v1 = token env v1 (* "const" *) in
  let v2 =
    token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  in
  let v3 = token env v3 (* ":" *) in
  let v4 = map_type_ env v4 in
  let v5 = token env v5 (* "=" *) in
  let v6 = map_expression env v6 in
  let v7 = token env v7 (* ";" *) in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_impl_block_item_type (env : env) ((v1, v2, v3, v4, v5, v6) : CST.impl_block_item_type) =
  let v1 = token env v1 (* "type" *) in
  let v2 =
    token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  in
  let v3 =
    (match v3 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v4 = token env v4 (* "=" *) in
  let v5 = map_type_ env v5 in
  let v6 = token env v6 (* ";" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_inner_attribute_item (env : env) ((v1, v2, v3) : CST.inner_attribute_item) =
  let v1 = token env v1 (* "#" *) in
  let v2 = token env v2 (* "!" *) in
  let v3 = map_attribute env v3 in
  todo env (v1, v2, v3)

and map_last_match_arm (env : env) ((v1, v2, v3, v4, v5) : CST.last_match_arm) =
  let v1 = List.map (map_outer_attribute_item env) v1 in
  let v2 = map_match_pattern env v2 in
  let v3 = token env v3 (* "=>" *) in
  let v4 = map_expression env v4 in
  let v5 =
    (match v5 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

and map_macro_invocation (env : env) ((v1, v2, v3) : CST.macro_invocation): G.expr =
  let name =
    (match v1 with
    | `Scoped_id x -> map_scoped_identifier env x
    | `Id tok -> let ident = token env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
        G.EName (ident, {G.name_qualifier = None; G.name_typeargs = None})
    )
  in
  let v2 = token env v2 (* "!" *) in
  let v3 = map_token_tree env v3 in
  todo env (v1, v2, v3)

and map_match_arm (env : env) ((v1, v2, v3, v4) : CST.match_arm): G.action =
  let outer_attrs = List.map (map_outer_attribute_item env) v1 in
  let pattern =
    (match v2 with
     | `Macro_invo x -> let invo = map_macro_invocation env x in
         G.OtherPat (G.OP_MacroInvocation, [G.E invo])
    | `Match_pat x -> map_match_pattern env x
    )
  in
  let arrow = token env v3 (* "=>" *) in
  let expr =
    (match v4 with
    | `Exp_COMMA (v1, v2) ->
        let expr = map_expression env v1 in
        let comma = token env v2 (* "," *) in
        expr
    | `Choice_unsafe_blk x ->
        map_expression_ending_with_block env x
    )
  in
  (pattern, expr)

and map_match_block (env : env) ((v1, v2, v3) : CST.match_block): G.action list =
  let lbrace = token env v1 (* "{" *) in
  let actions =
    (match v2 with
    | Some (v1, v2) ->
        let match_arms = List.map (map_match_arm env) v1 in
        let match_arm_last = map_last_match_arm env v2 in
        List.concat [match_arms; [match_arm_last]]
    | None -> [])
  in
  let rbrace = token env v3 (* "}" *) in
  actions

and map_match_pattern (env : env) ((v1, v2) : CST.match_pattern) =
  let v1 = map_pattern env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "if" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_meta_arguments (env : env) ((v1, v2, v3, v4) : CST.meta_arguments) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_meta_item_fefa160 env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_anon_choice_meta_item_fefa160 env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v4 = token env v4 (* ")" *) in
  todo env (v1, v2, v3, v4)

and map_meta_item (env : env) ((v1, v2) : CST.meta_item) =
  let v1 = map_path env v1 in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `EQ_lit (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_literal env v2 in
            todo env (v1, v2)
        | `Meta_args x -> map_meta_arguments env x
        )
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_mod_block (env : env) ((v1, v2, v3, v4) : CST.mod_block) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (map_inner_attribute_item env) v2 in
  let v3 = List.map (map_item env) v3 in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

and map_ordered_field_declaration_list (env : env) ((v1, v2, v3, v4) : CST.ordered_field_declaration_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 = List.map (map_outer_attribute_item env) v1 in
        let v2 =
          (match v2 with
          | Some x -> map_visibility_modifier env x
          | None -> todo env ())
        in
        let v3 = map_type_ env v3 in
        let v4 =
          List.map (fun (v1, v2, v3, v4) ->
            let v1 = token env v1 (* "," *) in
            let v2 = List.map (map_outer_attribute_item env) v2 in
            let v3 =
              (match v3 with
              | Some x -> map_visibility_modifier env x
              | None -> todo env ())
            in
            let v4 = map_type_ env v4 in
            todo env (v1, v2, v3, v4)
          ) v4
        in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v4 = token env v4 (* ")" *) in
  todo env (v1, v2, v3, v4)

and map_outer_attribute_item (env : env) ((v1, v2) : CST.outer_attribute_item) =
  let v1 = token env v1 (* "#" *) in
  let v2 = map_attribute env v2 in
  todo env (v1, v2)

and map_parameter (env : env) ((v1, v2, v3, v4) : CST.parameter) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* "mut" *)
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Pat x -> map_pattern env x
    | `Self tok -> token env tok (* "self" *)
    | `Choice_defa x -> map_reserved_identifier env x
    )
  in
  let v3 = token env v3 (* ":" *) in
  let v4 = map_type_ env v4 in
  todo env (v1, v2, v3, v4)

and map_parameters (env : env) ((v1, v2, v3, v4) : CST.parameters): G.parameter list =
  let lparen = token env v1 (* "(" *) in
  let params =
    (match v2 with
    | Some (v1, v2, v3) ->
        let outer_attr = Option.map (fun x -> map_outer_attribute_item env x) v1 in
        let param_first = map_anon_choice_param_2c23cdc env outer_attr v2 in
        let param_rest =
          List.map (fun (v1, v2, v3) ->
            let comma = token env v1 (* "," *) in
            let outer_attr = Option.map (fun x -> map_outer_attribute_item env x) v2 in
            map_anon_choice_param_2c23cdc env outer_attr v3
          ) v3
        in
        (param_first::param_rest)
    | None -> [])
  in
  let comma = Option.map (fun tok -> token env tok) v3 in
  let rparen = token env v4 (* ")" *) in
  params

and map_path (env : env) (x : CST.path): G.expr =
  (match x with
  | `Self tok -> let self = token env tok in (* "self" *)
      G.IdSpecial (G.Self, self)
  | `Choice_u8 x -> let (_, ident) = map_primitive_type_ident env x in
      G.Id (ident, G.empty_id_info ())
  | `Meta tok -> let metavar = token env tok in (* pattern \$[a-zA-Z_]\w* *)
      G.Metavar metavar
  | `Super tok -> let super = token env tok in (* "super" *)
      G.IdSpecial (G.Super, super)
  | `Crate tok -> let crate = token env tok in (* "crate" *)
      G.IdSpecial (G.Parent, crate) (* TODO module instead? *)
  | `Id tok -> let ident = token env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      G.Id (ident, G.empty_id_info ())
  | `Scoped_id x -> map_scoped_identifier env x
  )

and map_pattern (env : env) (x : CST.pattern): G.pattern =
  (match x with
  | `Lit_pat x -> map_literal_pattern env x
  | `Choice_u8 x -> map_primitive_type_ident env x
  | `Id tok ->
      token env tok (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  | `Scoped_id x -> let ident = map_scoped_identifier env x in
      G.PatName ident
  | `Tuple_pat (v1, v2, v3, v4) ->
      let lparen = token env v1 (* "(" *) in
      let items =
        (match v2 with
        | Some x -> map_tuple_pattern_list env x
        | None -> [])
      in
      let comma = Option.map (fun tok -> token env tok) v3 in (* "," *)
      let rparen = token env v4 (* ")" *) in
      G.PatTuple (lparen, items, rparen)
  | `Tuple_struct_pat (v1, v2, v3, v4, v5) ->
      let name = map_tuple_struct_name env v1 in
      let lparen = token env v2 (* "(" *) in
      let items =
        (match v3 with
        | Some x -> map_tuple_pattern_list env x
        | None -> [])
      in
      let comma = Option.map (fun tok -> token env tok) v4 in (* "," *)
      let rparen = token env v5 (* ")" *) in
      G.PatTupleStruct (name, (lparen, items, rparen))
  | `Struct_pat (v1, v2, v3, v4, v5) ->
      let name = map_struct_name env v1 in
      let lbrace = token env v2 (* "{" *) in
      let fields =
        (match v3 with
        | Some (v1, v2) ->
            let field_first = map_struct_pattern_field env v1 in
            let field_rest =
              List.map (fun (v1, v2) ->
                let comma = token env v1 (* "," *) in
                let field = map_struct_pattern_field env v2 in
                field
              ) v2
            in
            (field_first::field_rest)
        | None -> [])
      in
      let comma = Option.map (fun tok -> token env tok) v4 in (* "," *)
      let rbrace = token env v5 (* "}" *) in
      G.PatStruct (name, (lbrace, fields, rbrace))
  | `Ref_pat_a3d7f54 (v1, v2) ->
      let ref_ = token env v1 (* "ref" *) in
      let pattern = map_pattern env v2 in
      G.PatRef (ref_, pattern)
  | `Slice_pat (v1, v2, v3, v4) ->
      let lbracket = token env v1 (* "[" *) in
      let patterns =
        (match v2 with
        | Some x -> map_tuple_pattern_list env x
        | None -> [])
      in
      let comma = Option.map (fun tok -> token env tok) v3 in (* "," *)
      let rbracket = token env v4 (* "]" *) in
      G.PatSlice (lbracket, patterns, rbracket)
  | `Capt_pat (v1, v2, v3) ->
      let ident = token env v1 in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let at = token env v2 (* "@" *) in
      let pattern = map_pattern env v3 in
      G.PatCapture (ident, pattern)
  | `Ref_pat_dbbcf07 (v1, v2, v3) ->
      let and_ = token env v1 (* "&" *) in
      let borrow = G.KeywordAttr (G.Borrowed, and_) in
      let mutability = Option.map (fun tok ->
        let tok = token env tok in (* "mut" *)
        G.KeywordAttr (G.Mutable, tok)
      ) v2 in
      let pattern = map_pattern env v3 in
      let attrs = deoptionalize [Some borrow; mutability] in
      G.PatBorrow (attrs, pattern)
  | `Rema_field_pat tok -> let tok = token env tok in (* ".." *)
      G.PatRemaining tok
  | `Mut_pat (v1, v2) ->
      let mut = token env v1 (* "mut" *) in
      let mutability = G.KeywordAttr (G.Mutable, mut) in
      let pattern = map_pattern env v2 in
      G.PatMutable ([mutability], pattern)
  | `Range_pat (v1, v2, v3) ->
      let lbound = map_range_pattern_bound env v1 in
      let op =
        (match v2 with
         | `DOTDOTDOT tok -> let tok = token env tok in (* "..." *)
             G.Range
         | `DOTDOTEQ tok -> let tok = token env tok in (* "..=" *)
             G.RangeInclusive
        )
      in
      let rbound = map_range_pattern_bound env v3 in
      G.PatRange (lbound, op, rbound)
  | `Or_pat (v1, v2, v3) ->
      let pattern_lhs = map_pattern env v1 in
      let or_ = token env v2 (* "|" *) in
      let pattern_rhs = map_pattern env v3 in
      G.DisjPat (pattern_lhs, pattern_rhs)
  | `Const_blk x -> let block = map_const_block env x in
      G.PatConstBlock block
  | `X__ tok -> let tok = token env tok in (* "_" *)
      G.PatUnderscore tok
  )

and map_pointer_type (env : env) ((v1, v2, v3) : CST.pointer_type) =
  let v1 = token env v1 (* "*" *) in
  let v2 =
    (match v2 with
    | `Const tok -> token env tok (* "const" *)
    | `Muta_spec tok -> token env tok (* "mut" *)
    )
  in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_qualified_type (env : env) ((v1, v2, v3) : CST.qualified_type) =
  let v1 = map_type_ env v1 in
  let v2 = token env v2 (* "as" *) in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_range_expression (env : env) (x : CST.range_expression) =
  (match x with
  | `Exp_choice_DOTDOT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `DOTDOT tok -> token env tok (* ".." *)
        | `DOTDOTDOT tok -> token env tok (* "..." *)
        | `DOTDOTEQ tok -> token env tok (* "..=" *)
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DOTDOT (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ".." *) in
      todo env (v1, v2)
  | `DOTDOT_exp x -> map_base_field_initializer env x
  | `DOTDOT tok -> token env tok (* ".." *)
  )

and map_reference_type (env : env) ((v1, v2, v3, v4) : CST.reference_type) =
  let v1 = token env v1 (* "&" *) in
  let v2 =
    (match v2 with
    | Some x -> map_lifetime env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "mut" *)
    | None -> todo env ())
  in
  let v4 = map_type_ env v4 in
  todo env (v1, v2, v3, v4)

and map_return_expression (env : env) (x : CST.return_expression) =
  (match x with
  | `Ret_exp (v1, v2) ->
      let v1 = token env v1 (* "return" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Ret tok -> token env tok (* "return" *)
  )

and map_scoped_identifier_name (env : env) ((v1, v2, v3) : CST.scoped_identifier): G.name =
  let (qualifier_expr, type_) = (match v1 with
    | Some x ->
        (match x with
        | `Choice_self x -> (Some (map_path env x), None)
        | `Brac_type x -> let (_, ty, _) = map_bracketed_type env x in
            (None, Some ty)
        | `Gene_type_with_turb x ->
            (None, Some (map_generic_type_with_turbofish env x)))
    | None -> (None, None)
  ) in
  let colons = token env v2 (* "::" *) in
  let ident = token env v3 in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let qualifier = Option.map (fun x -> G.QExpr (x, colons)) qualifier_expr in
  let typeargs = Option.map (fun x -> [G.TypeArg x]) type_ in
  (ident, { G.name_qualifier = qualifier; G.name_typeargs = typeargs })

and map_scoped_identifier (env : env) (v1 : CST.scoped_identifier): G.expr =
  G.IdQualified (map_scoped_identifier_name env v1, G.empty_id_info ())

and map_scoped_type_identifier_name (env : env) ((v1, v2, v3) : CST.scoped_type_identifier): G.name =
  let colons = token env v2 (* "::" *) in
  let qualifier = (match v1 with
    | Some x ->
        (match x with
        | `Choice_self x -> let ident = map_path env x in
            Some (G.QExpr (ident, colons))
        | `Gene_type_with_turb x -> let ty = map_generic_type_with_turbofish env x in
            Some (G.QType ty)
        | `Brac_type x -> let (_, ty, _) = map_bracketed_type env x in
            Some (G.QType ty)
        | `Gene_type x -> let ty = map_generic_type env x in
            Some (G.QType ty)
    )
    | None -> None
  ) in
  let ident = token env v3 in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  (ident, { G.name_qualifier = qualifier; G.name_typeargs = None })

and map_scoped_type_identifier (env : env) ((v1, v2, v3) : CST.scoped_type_identifier): G.type_ =
  let name = map_scoped_type_identifier_name env (v1, v2, v3) in
  G.TyIdQualified (name, G.empty_id_info ())

and map_scoped_type_identifier_in_expression_position (env : env) ((v1, v2, v3) : CST.scoped_type_identifier_in_expression_position): G.name =
  let colons = token env v2 (* "::" *) in
  let qualifier = (match v1 with
    | Some x ->
        (match x with
         | `Choice_self x -> let ident = map_path env x in
             Some (G.QExpr (ident, colons))
         | `Gene_type_with_turb x -> let ty = map_generic_type_with_turbofish env x in
             Some (G.QType ty)
        )
    | None -> None)
  in
  let ident = token env v3 in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  (ident, { G.name_qualifier = qualifier; G.name_typeargs = None })

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Exp_stmt x -> map_expression_statement env x
  | `Let_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "let" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* "mut" *)
        | None -> todo env ())
      in
      let v3 = map_pattern env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v6 = token env v6 (* ";" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Item x -> map_item env x
  )

and map_trait_block (env : env) ((v1, v2, v3) : CST.trait_block) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (map_trait_block_item env) v2 in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_trait_block_item (env : env) ((v1, v2) : CST.trait_block_item) =
  let v1 = List.map (map_outer_attribute_item env) v1 in
  let v2 =
    (match v2 with
    | `Const_item x -> map_const_item env x
    | `Func_sign_with_defa_item x ->
        map_function_signature_with_default_item env x
    | `Asso_type x -> map_associated_type env x
    | `Macro_invo x -> map_macro_invocation env x
    )
  in
  todo env (v1, v2)

and map_trait_bounds (env : env) ((v1, v2, v3) : CST.trait_bounds): G.trait_bound list =
  let colon = token env v1 (* ":" *) in
  let trait_bound_first = map_trait_bound env v2 in
  let trait_bound_rest =
    List.map (fun (v1, v2) ->
      let plus = token env v1 (* "+" *) in
      let trait_bound = map_trait_bound env v2 in
      trait_bound
    ) v3
  in
  (trait_bound_first::trait_bound_rest)

and map_tuple_type (env : env) ((v1, v2, v3, v4, v5) : CST.tuple_type) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v5 = token env v5 (* ")" *) in
  todo env (v1, v2, v3, v4, v5)

and map_type_ (env : env) (x : CST.type_): G.type_ =
  (match x with
  | `Abst_type (v1, v2) ->
      let impl = token env v1 (* "impl" *) in
      let trait_id = map_abstract_type_trait_name env v2 in
      todo env (v1, v2)
  | `Ref_type x -> map_reference_type env x
  | `Meta tok -> let metavar = token env tok in (* pattern \$[a-zA-Z_]\w* *)
      G.OtherType (G.OT_Expr, [G.E (G.Metavar metavar)])
  | `Poin_type x -> map_pointer_type env x
  | `Gene_type x -> map_generic_type env x
  | `Scoped_type_id x -> let name = map_scoped_type_identifier env x in
      G.TyIdQualified (name, G.empty_id_info ())
  | `Tuple_type x -> map_tuple_type env x
  | `Unit_type (v1, v2) ->
      let lparen = token env v1 (* "(" *) in
      let rparen = token env v2 (* ")" *) in
      G.TyBuiltin ("()", lparen)
  | `Array_type (v1, v2, v3, v4) ->
      let lbracket = token env v1 (* "[" *) in
      let ty = map_type_ env v2 in
      let default = Option.map (fun (v1, v2) ->
            let semicolon = token env v1 (* ";" *) in
            let expr = map_expression env v2 in
            expr
      ) v3 in
      let rbracket = token env v4 (* "]" *) in
      G.TyArray ((lbracket, default, rbracket), ty)
  | `Func_type x -> map_function_type env x
  | `Id tok ->
      let ident = token env tok in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let id_info = G.empty_id_info () in
      G.TyId (ident, id_info)
  | `Macro_invo x -> let invo = map_macro_invocation env x in
      G.OtherType (G.OT_Expr, [invo])
  | `Empty_type tok -> let bang = token env tok in (* "!" *)
      G.TyBuiltin("!", bang)
  | `Dyna_type (v1, v2) ->
      let dyn = token env v1 (* "dyn" *) in
      let dyn_id = map_abstract_type_trait_name env v2 in
      todo env (v1, v2)
  | `Boun_type x -> map_bounded_type env x
  | `Choice_u8 x -> map_primitive_type_ident env x
  )

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments): G.type_arguments =
  let lthan = token env v1 (* tok_LT *) in
  let typearg_first = map_type_argument env v2 in
  let typearg_rest =
    List.map (fun (v1, v2) ->
      let comma = token env v1 (* "," *) in
      let typearg = map_type_argument env v2 in
      typearg
    ) v3
  in
  let comma = Option.map (fun tok -> token env tok) v4 in
  let gthan = token env v5 (* ">" *) in
  (typearg_first::typearg_rest)

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters): G.type_parameter list =
  let lthan = token env v1 (* "<" *) in
  let type_param_first = map_type_parameter env v2 in
  let type_param_rest =
    List.map (fun (v1, v2) ->
      let comma = token env v1 (* "," *) in
      let type_param = map_type_parameter env v2 in
      type_param
    ) v3
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) v4 in
  let gthan = token env v5 (* ">" *) in
  todo env (v1, v2, v3, v4, v5)

and map_use_clause (env : env) (x : CST.use_clause) =
  (match x with
  | `Choice_self x -> map_path env x
  | `Use_as_clause (v1, v2, v3) ->
      let v1 = map_path env v1 in
      let v2 = token env v2 (* "as" *) in
      let v3 =
        token env v3 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      in
      todo env (v1, v2, v3)
  | `Use_list x -> map_use_list env x
  | `Scoped_use_list (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_path env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "::" *) in
      let v3 = map_use_list env v3 in
      todo env (v1, v2, v3)
  | `Use_wild (v1, v2) ->
      let v1 =
        (match v1 with
        | Some (v1, v2) ->
            let v1 = map_path env v1 in
            let v2 = token env v2 (* "::" *) in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v2 = token env v2 (* "*" *) in
      todo env (v1, v2)
  )

and map_use_list (env : env) ((v1, v2, v3, v4) : CST.use_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | `Use_clause x -> map_use_clause env x
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | `Use_clause x -> map_use_clause env x
              )
            in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

and map_visibility_quantifier (env: env) (v1, v2, v3): G.attribute =
  let lparen = token env v1 (* "(" *) in
  let attribute =
    (match v2 with
     | `Self tok -> G.KeywordAttr (G.Protected, token env tok) (* "self" *)
     | `Super tok -> G.KeywordAttr (G.VisSuper, token env tok) (* "super" *)
     | `Crate tok -> G.KeywordAttr (G.Module, token env tok) (* "crate" *)
     | `In_choice_self (v1, v2) ->
         let in_ = token env v1 (* "in" *) in
         let path = map_path env v2 in
         G.OtherAttribute (G.OA_Expr, [G.E path])
    )
  in
  let rparen = token env v3 (* ")" *) in
  attribute

and map_visibility_modifier (env : env) (x : CST.visibility_modifier): G.attribute list =
  (match x with
  | `Crate tok -> let tok = token env tok in (* "crate" *)
      (* unstable(crate_visibility_modifier) *)
      (* synonymous with `pub(crate)` *)
      G.KeywordAttr (G.Module, tok)
  | `Pub_opt_LPAR_choice_self_RPAR (v1, v2) ->
      let pub = token env v1 (* "pub" *) in
      let pub_attr = G.KeywordAttr (G.Public, pub) in
      let qualifier = Option.map (fun x -> map_visibility_quantifier env x) v2 in
      deoptionalize [Some pub_attr; qualifier]
  )

and map_where_clause (env : env) ((v1, v2, v3, v4) : CST.where_clause): G.where_clause =
  let where = token env v1 (* "where" *) in
  let predicate_first = map_where_predicate env v2 in
  let predicate_rest =
    List.map (fun (v1, v2) ->
      let comma = token env v1 (* "," *) in
      let predicate = map_where_predicate env v2 in
      predicate
    ) v3
  in
  let comma = Option.map (fun tok -> token env tok (* "," *)) v4 in
  (predicate_first::predicate_rest)

and map_where_predicate (env : env) ((v1, v2) : CST.where_predicate): G.where_predicate =
  let where_predicate_type =
    (match v1 with
     | `Life x -> G.WherePredLifetime (map_lifetime env x)
     | `Id tok -> G.WherePredId (token env tok) (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
     | `Scoped_type_id x -> G.WherePredType (map_scoped_type_identifier env x)
     | `Gene_type x -> G.WherePredType (map_generic_type env x)
     | `Ref_type x -> G.WherePredType (map_reference_type env x)
     | `Poin_type x -> G.WherePredType (map_pointer_type env x)
     | `Tuple_type x -> G.WherePredType (map_tuple_type env x)
     | `Higher_ranked_trait_bound x -> let (type_params, ty) = map_higher_ranked_trait_bound env x in
         G.WherePredHigherRanked (type_params, ty)
     | `Choice_u8 x -> map_primitive_type_ident env x
    )
  in
  let trait_bounds = map_trait_bounds env v2 in
  (where_predicate_type, trait_bounds)

and map_item_kind (env : env) outer_attrs visibility (x : CST.item_kind): G.stmt list =
  (match x with
  | `Const_item x -> map_const_item env x
  | `Macro_invo x -> map_macro_invocation env x
  | `Macro_defi (v1, v2, v3) ->
      let v1 = token env v1 (* "macro_rules!" *) in
      let v2 =
        token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      in
      let v3 =
        (match v3 with
        | `LPAR_rep_macro_rule_SEMI_opt_macro_rule_RPAR_SEMI (v1, v2, v3, v4, v5) ->
            let v1 = token env v1 (* "(" *) in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = map_macro_rule env v1 in
                let v2 = token env v2 (* ";" *) in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some x -> map_macro_rule env x
              | None -> todo env ())
            in
            let v4 = token env v4 (* ")" *) in
            let v5 = token env v5 (* ";" *) in
            todo env (v1, v2, v3, v4, v5)
        | `LCURL_rep_macro_rule_SEMI_opt_macro_rule_RCURL (v1, v2, v3, v4) ->
            let v1 = token env v1 (* "{" *) in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = map_macro_rule env v1 in
                let v2 = token env v2 (* ";" *) in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some x -> map_macro_rule env x
              | None -> todo env ())
            in
            let v4 = token env v4 (* "}" *) in
            todo env (v1, v2, v3, v4)
        )
      in
      todo env (v1, v2, v3)
  | `Empty_stmt tok -> token env tok (* ";" *)
  | `Mod_item (v1, v2, v3) ->
      let v1 = token env v1 (* "mod" *) in
      let v2 =
        token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      in
      let v3 =
        (match v3 with
        | `SEMI tok -> token env tok (* ";" *)
        | `Mod_blk x -> map_mod_block env x
        )
      in
      todo env (v1, v2, v3)
  | `Fore_mod_item (v1, v2) ->
      let v1 = map_extern_modifier env v1 in
      let v2 =
        (match v2 with
        | `SEMI tok -> token env tok (* ";" *)
        | `Fore_mod_blk x -> map_foreign_mod_block env x
        )
      in
      todo env (v1, v2)
  | `Struct_item (v1, v2, v3, v4) ->
      let struct_ = token env v1 (* "struct" *) in
      let ident = token env v2 in (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let type_params = match v3 with
        | Some x -> map_type_parameters env x
        | None -> []
      in
      let (fields, where_clause) =
        (match v4 with
        | `Opt_where_clause_field_decl_list (v1, v2) ->
            let where_clause = Option.map (fun x -> map_where_clause env x) v1 in
            let fields = map_field_declaration_list env v2 in
            (fields, where_clause)
        | `Orde_field_decl_list_opt_where_clause_SEMI (v1, v2, v3) ->
            (* Unit structs *)
            let fields = map_ordered_field_declaration_list env v1 in
            let where_clause = Option.map (fun x -> map_where_clause env x) v2 in
            let semicolon = token env v3 (* ";" *) in
            (fields, where_clause)
        | `SEMI tok -> token env tok (* ";" *)
        )
      in
      let class_def = {
        G.ckind = (G.Class, struct_);
        G.cextends = [];
        G.cimplements = [];
        G.cmixins = [];
        G.cbody = fields;
      } in
      let ent = { G.name = ident; attrs = []; tparams = type_params } in
      [G.DefStmt (ent, G.ClassDef class_def) |> G.s]
  | `Union_item (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "union" *) in
      let v2 =
        token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v5 = map_field_declaration_list env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Enum_item (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "enum" *) in
      let v2 =
        token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v5 = map_enum_variant_list env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Type_item (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "type" *) in
      let v2 =
        token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "=" *) in
      let v5 = map_type_ env v5 in
      let v6 = token env v6 (* ";" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Func_item x -> [map_function_item env x]
  | `Func_sign_item (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some x -> map_function_modifiers env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "fn" *) in
      let v3 = map_function_declaration env v3 in
      let v4 = token env v4 (* ";" *) in
      todo env (v1, v2, v3, v4)
  | `Impl_item (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "unsafe" *)
        | None -> todo env ())
      in
      let v2 = token env v2 (* "impl" *) in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 =
              (match v1 with
              | `Id tok ->
                  token env tok (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
              | `Scoped_type_id x -> map_scoped_type_identifier env x
              | `Gene_type x -> map_generic_type env x
              )
            in
            let v2 = token env v2 (* "for" *) in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 = map_type_ env v5 in
      let v6 =
        (match v6 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v7 = map_impl_block env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Trait_item (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "unsafe" *)
        | None -> todo env ())
      in
      let v2 = token env v2 (* "trait" *) in
      let v3 =
        token env v3 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      in
      let v4 =
        (match v4 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_trait_bounds env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v7 = map_trait_block env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Use_decl (v1, v2, v3) ->
      let v1 = token env v1 (* "use" *) in
      let v2 = map_use_clause env v2 in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Extern_crate_decl (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "extern" *) in
      let v2 = token env v2 (* "crate" *) in
      let v3 =
        token env v3 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "as" *) in
            let v2 =
              token env v2 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 = token env v5 (* ";" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Static_item (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "static" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* "ref" *)
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some tok -> token env tok (* "mut" *)
        | None -> todo env ())
      in
      let v4 =
        token env v4 (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      in
      let v5 = token env v5 (* ":" *) in
      let v6 = map_type_ env v6 in
      let v7 =
        (match v7 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v8 = token env v8 (* ";" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  )

and map_item (env : env) ((v1, v2, v3) : CST.item) =
  let outer_attrs = List.map (map_outer_attribute_item env) v1 in
  let visibility = Option.map (fun x -> map_visibility_modifier env x) v2 in
  map_item_kind env outer_attrs visibility v3

let map_source_file (env : env) ((v1, v2) : CST.source_file): G.program =
  let v1 = List.map (map_inner_attribute_item env) v1 in
  let v2 = List.map (map_item env) v2 in
  []


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () ->
       Parallel.backtrace_when_exn := false;
       Parallel.invoke Tree_sitter_rust.Parse.file file ()
    )
    (fun cst ->
       let env = { H.file; conv = H.line_col_to_pos file; extra = () } in

       try
         map_source_file env cst
       with
         (Failure "not implemented") as exn ->
           let s = Printexc.get_backtrace () in
           pr2 "Some constructs are not handled yet";
           pr2 "CST was:";
           CST.dump_tree cst;
           pr2 "Original backtrace:";
           pr2 s;
           raise exn
    )

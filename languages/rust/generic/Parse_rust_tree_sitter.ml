(* Ruin0x11
 *
 * Copyright (C) 2021 Semgrep Inc.
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
open Either_
module CST = Tree_sitter_rust.CST
module H = Parse_tree_sitter_helpers
module H2 = AST_generic_helpers
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Rust parser using tree-sitter-lang/semgrep-rust and converting
 * directly to AST_generic.ml
 *
 * Some comments are tagged with ruin: to indicate code ruin0x11 wrote
 * in a fork of tree-sitter-rust that didn't get merge in the official
 * tree-sitter-rust (but we might want to merge at some point)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type mode = Pattern | Target
type env = mode H.env

let token = H.token
let str = H.str
let sc = G.sc
let fb = Tok.unsafe_fake_bracket
let fake_id s = (s, G.fake s)

let deoptionalize l =
  let rec deopt acc = function
    | [] -> List.rev acc
    | None :: tl -> deopt acc tl
    | Some x :: tl -> deopt (x :: acc) tl
  in
  deopt [] l

let in_pattern env =
  match env.H.extra with
  | Target -> false
  | Pattern -> true

(*****************************************************************************)
(* Intermediate AST-like types *)
(*****************************************************************************)

type function_declaration_rs = {
  name : G.entity_name;
  type_params : G.type_parameters option;
  params : G.parameters;
  retval : G.type_ option;
}

type lifetime = G.ident

type trait_bound =
  | TraitBoundType of G.type_
  | TraitBoundLifetime of lifetime
  | TraitBoundHigherRanked of G.type_parameters * G.type_
  | TraitBoundRemoved of G.type_

type where_predicate_type =
  | WherePredLifetime of lifetime
  | WherePredId of G.ident
  | WherePredType of G.type_
  | WherePredHigherRanked of G.type_parameters * G.type_

type where_predicate = where_predicate_type * trait_bound list
type where_clause = where_predicate list

type rust_macro_item =
  | MacAny of G.any
  | MacTree of rust_macro_item list G.bracket
  | MacTreeBis of rust_macro_item list G.bracket * G.ident option * G.tok

let rec macro_items_to_anys (xs : rust_macro_item list) : G.any list =
  (* A change to how tree sitter parses macro items has led to some nesting in
   * MacAny macro items. Flatten these before pattern matching below. *)
  let xs =
    xs
    |> List.concat_map (function
         | MacAny (G.Anys anys) -> anys |> List_.map (fun any -> MacAny any)
         | other -> [ other ])
  in
  (* Note that the commas are considered like any other tokens in a Rust macro;
     * they are not separators between rust_macro_items.
  *)
  (* Thus, we need this machinery in order to parse a list of macro items
     to see if the macro invocation looks like a normal expression. For instance,
     if a sequence of macro items looks like
     <expr> , <expr> , <expr>
     then we assume that this is something which looks like a series of
     arguments, so we produce a single `Any`, which is `Args`.
     Note that <expr> can also occur once with no commas, or zero times.
  *)
  let macro_item_to_expr = function
    | MacAny (G.E e) -> Some e
    | MacAny (G.I e) -> Some (G.N (G.Id (e, G.empty_id_info ())) |> G.e)
    (* probably unreachable *)
    | MacAny (G.Ar (Arg e)) -> Some e
    | MacAny _
    | MacTreeBis _
    | MacTree _ ->
        None
  in
  (* try_as_normal_exprs just tries to parse all of the arguments to a
     macro as expressions. In anticipation of dealing with the case
     where such an argument is either followed by a `.` or prefixed by
     an operator like `&` and `*`, we carry an accumulator argument and
     straightforwardly recurse upon the list.
  *)
  let rec try_as_normal_exprs acc macros =
    match (acc, macros) with
    (* If we end with a comma, that's pretty weird and probably wrong. *)
    | _, [ MacAny (G.Tk (Tok.OriginTok { str = ","; _ })) ] -> None
    | Some e, [] -> Some [ e ]
    | None, [] -> Some []
    (* For now, we just directly case on the token to see if its a comma.
       This is a fragile approach, because I'm a little suspicious and I
       don't fully trust pattern matching on the string inside of the token,
       but there's little opportunities for the string to have changed by
       this point, so this should work.
       It's also a lot more work to bring this information over from
       when we first parse the token.
    *)
    | None, MacAny (G.Tk (Tok.OriginTok { str = ","; _ })) :: _ -> None
    | Some e, MacAny (G.Tk (Tok.OriginTok { str = ","; _ })) :: rest ->
        let* args = try_as_normal_exprs None rest in
        Some (e :: args)
    (* For the dot case, we want to only handle this once we've already seen and
       are currently parsing an entry. Hence, we case on the "Some".
    *)
    | ( Some e,
        MacAny (G.Tk (Tok.OriginTok { str = "."; _ } as tk))
        :: MacAny (G.I id)
        :: rest ) ->
        try_as_normal_exprs
          (Some (G.DotAccess (e, tk, G.FN (Id (id, G.empty_id_info ()))) |> G.e))
          rest
    (* For the prefix case, however, we must only handle this if we haven't
       seen an entry, because this should start off the prefix.
    *)
    | None, MacAny (G.Tk (Tok.OriginTok { str; _ } as tk)) :: rest -> (
        (* NOTE: We only deal with the case where there is one on the front,
           because as it turns out, the Rust tree-sitter parser will parse
           something like

           &*x
           as
           &* x

           as in, with &* as a single token. So let's just not deal with
           that for now.
        *)
        (* We need to do the rest of it first, so we can ensure that the
           prefix operator happens last.
        *)
        let* args = try_as_normal_exprs None rest in
        match args with
        | [] -> None
        | e :: es ->
            let* e =
              match str with
              | "&" -> Some (Ref (tk, e) |> G.e)
              | "*" -> Some (DeRef (tk, e) |> G.e)
              | _ -> None
            in
            Some (e :: es))
    | _, mac :: rest ->
        let* expr = macro_item_to_expr mac in
        let* args = try_as_normal_exprs (Some expr) rest in
        Some args
  in
  match try_as_normal_exprs None xs with
  | None -> xs |> List_.map macro_item_to_any
  | Some res -> [ G.Args (List_.map (fun e -> G.Arg e) res) ]

and macro_item_to_any = function
  | MacAny x -> x
  | MacTree (l, xs, r) ->
      G.Anys ([ G.Tk l ] @ macro_items_to_anys xs @ [ G.Tk r ])
  | MacTreeBis ((l, xs, r), idopt, t) ->
      G.Anys
        ([ G.Tk l ] @ macro_items_to_anys xs @ [ G.Tk r ]
        @ (match idopt with
          | None -> []
          | Some id -> [ G.I id ])
        @ [ G.Tk t ])

(* TODO: factorize with macro_items_to_anys above *)
let rec convert_macro_item (item : rust_macro_item) : G.any list =
  match item with
  | MacAny any -> [ any ]
  | MacTree (_, items, _) -> List.concat_map convert_macro_item items
  | MacTreeBis ((_, items, _), ident, tok) ->
      let items = List.concat_map convert_macro_item items in
      let ident =
        match ident with
        | Some i -> [ G.I i ]
        | None -> []
      in
      items @ ident @ [ G.Tk tok ]

type rust_macro_pattern =
  | RustMacPatTree of rust_macro_pattern list
  | RustMacPatRepetition of
      rust_macro_pattern list G.bracket * G.ident option * G.tok
  | RustMacPatBinding of G.ident * G.tok
  | RustMacPatToken of G.any

type rust_macro_definition = rust_macro_rule list G.bracket

and rust_macro_rule = {
  rules : rust_macro_pattern list;
  body : rust_macro_item list G.bracket;
}

let rec convert_macro_pattern (pattern : rust_macro_pattern) : G.any list =
  match pattern with
  | RustMacPatTree _patsTODO -> []
  | RustMacPatRepetition ((_, pats, _), ident, tok) ->
      let pats = List.concat_map convert_macro_pattern pats in
      let ident =
        match ident with
        | Some i -> [ G.I i ]
        | None -> []
      in
      pats @ ident @ [ G.Tk tok ]
  | RustMacPatBinding (ident, tok) -> [ G.I ident; G.Tk tok ]
  | RustMacPatToken any -> [ any ]

and convert_macro_rule (rule : rust_macro_rule) : G.any list =
  let rules = List.concat_map convert_macro_pattern rule.rules in
  let _, items, _ = rule.body in
  let body = List.concat_map convert_macro_item items in
  rules @ body

and convert_macro_def (macro_def : rust_macro_definition) : G.macro_definition =
  let _, rules, _ = macro_def in
  let body = List.concat_map convert_macro_rule rules in
  { G.macroparams = []; G.macrobody = body }

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-rust/Boilerplate.ml *)

let ident (env : env) (tok : CST.identifier) : G.ident =
  (* pattern [a-zA-Z_]\w* *)
  str env tok

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

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) : G.ident
    =
  match x with
  | `Defa tok -> ident env tok (* "default" *)
  | `Union tok -> ident env tok

(* "union" *)

let map_primitive_type_token (_env : env) (x : CST.anon_choice_u8_6dad923) =
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
    string * Tok.t =
  str env (map_primitive_type_token env x)

let map_primitive_type (env : env) (x : CST.anon_choice_u8_6dad923) : G.type_ =
  let s, tok = map_primitive_type_ident env x in
  G.ty_builtin (s, tok)

let map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) :
    G.literal =
  let ldquote = token env v1 (* pattern "b?\"" *) in
  let strs =
    List_.map
      (fun x ->
        match x with
        | `Esc_seq tok -> str env tok (* escape_sequence *)
        | `Str_content tok -> str env tok
        (* string_content *))
      v2
  in
  let rdquote = token env v3 in
  G.String (G.string_ (ldquote, strs, rdquote))

let integer_literal env tok =
  let s, t = str env tok in
  Parsed_int.parse (s, t)

let float_literal env tok =
  let s, t = str env tok in
  (float_of_string_opt s, t)

let map_literal (env : env) (x : CST.literal) : G.literal =
  match x with
  | `Str_lit x -> map_string_literal env x
  | `Raw_str_lit tok -> G.String (fb (str env tok)) (* raw_string_literal *)
  | `Char_lit tok -> G.Char (str env tok) (* char_literal *)
  | `Bool_lit x -> map_boolean_literal env x
  | `Int_lit tok -> G.Int (integer_literal env tok) (* integer_literal *)
  | `Float_lit tok -> G.Float (float_literal env tok)

(* float_literal *)

let map_literal_pattern (env : env) (x : CST.literal_pattern) : G.pattern =
  match x with
  | `Str_lit x -> G.PatLiteral (map_string_literal env x)
  | `Raw_str_lit tok ->
      G.PatLiteral (G.String (fb (str env tok))) (* raw_string_literal *)
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
          let pi = integer_literal env tok |> Parsed_int.neg in
          (* integer_literal *)
          G.PatLiteral
            (G.Int
               (pi
               |> Parsed_int.map_tok (fun t -> Tok.combine_toks (snd neg) [ t ])
               ))
      | `Float_lit tok ->
          let fopt, t = float_literal env tok in
          (* float_literal *)
          let fopt =
            match fopt with
            | Some f -> Some (-.f)
            | None -> None
          in
          G.PatLiteral (G.Float (fopt, Tok.combine_toks (snd neg) [ t ])))

let map_extern_modifier (env : env) ((v1, v2) : CST.extern_modifier) :
    G.attribute list =
  let extern = token env v1 (* "extern" *) in
  let extern_attr = G.KeywordAttr (G.Extern, extern) in

  (* as in 'extern "C"' or 'extern "stdcall"' *)
  let quantifier =
    Option.map
      (fun x ->
        let str = map_string_literal env x in
        G.OtherAttribute (("Extern", extern), [ G.E (G.L str |> G.e) ]))
      v2
  in

  deoptionalize [ Some extern_attr; quantifier ]

(* ruin:
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
         let path, ident = map_simple_scoped_identifier env x in
         (* TODO: bug, path @ [ident] *)
         ident :: path

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

   let rec map_simple_scoped_identifier (env : env)
       ((v1, v2, v3) : CST.simple_scoped_identifier) : G.dotted_ident * G.ident =
     let path = map_simple_path env v1 in
     let _colons = token env v2 (* "::" *) in
     let ident = ident env v3 (* identifier *) in
     (path, ident)

   and map_simple_scoped_identifier_name (env : env)
       ((v1, v2, v3) : CST.simple_scoped_identifier) : G.name =
     let path = map_simple_path env v1 in
     let _colons = token env v2 (* "::" *) in
     let ident = ident env v3 (* identifier *) in
     H2.name_of_ids (path @ [ ident ])

   let map_foreign_item_type (env : env) ((v1, v2, v3) : CST.foreign_item_type) :
       G.stmt =
     let _type_TODO = token env v1 (* "type" *) in
     let ident = ident env v2 in
     (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
     let _semicolon = token env v3 (* ";" *) in
     let type_def = { G.tbody = G.NewType (G.TyN (H2.name_of_id ident) |> G.t) } in
     let ent =
       {
         G.name = G.EN (H2.name_of_id ident);
         G.attrs = [ G.KeywordAttr (G.Extern, G.fake "extern") ];
         G.tparams = [];
       }
     in
     G.DefStmt (ent, G.TypeDef type_def) |> G.s
*)

let map_lifetime (env : env) ((v1, v2) : CST.lifetime) : lifetime =
  let _tTODO = token env v1 (* "'" *) in
  let id = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  id

let map_label (env : env) ((v1, v2) : CST.label) : G.label =
  let _apostopheTODO = token env v1 (* "'" *) in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  ident env v2

let map_non_special_token (env : env) (x : CST.non_special_token) =
  match x with
  | `Choice_lit x -> (
      match x with
      | `Lit x ->
          let lit = map_literal env x in
          G.E (G.L lit |> G.e)
      | `Id tok -> G.I (str env tok)
      | `Muta_spec tok -> G.I ((* "mut" *) str env tok)
      | `Self tok -> G.I ((* "self" *) str env tok)
      | `Super tok -> G.I ((* "super" *) str env tok)
      | `Crate tok -> G.I ((* "crate" *) str env tok)
      | `Choice_u8 x -> G.I (map_primitive_type_ident env x)
      | `Rep1_choice_SLASH xs ->
          G.Anys
            (List_.map
               (fun x ->
                 match x with
                 | `SLASH tok -> G.Tk ((* "/" *) token env tok)
                 | `X__ tok -> G.Tk ((* "_" *) token env tok)
                 | `BSLASH tok -> G.Tk ((* "\\" *) token env tok)
                 | `DASH tok -> G.Tk ((* "-" *) token env tok)
                 | `EQ tok -> G.Tk ((* "=" *) token env tok)
                 | `DASHGT tok -> G.Tk ((* "->" *) token env tok)
                 | `COMMA tok -> G.Tk ((* "," *) token env tok)
                 | `SEMI tok -> G.Tk ((* ";" *) token env tok)
                 | `COLON tok -> G.Tk ((* ":" *) token env tok)
                 | `COLONCOLON tok -> G.Tk ((* "::" *) token env tok)
                 | `BANG tok -> G.Tk ((* "!" *) token env tok)
                 | `QMARK tok -> G.Tk ((* "?" *) token env tok)
                 | `DOT tok -> G.Tk ((* "." *) token env tok)
                 | `AT tok -> G.Tk ((* "@" *) token env tok)
                 | `STAR tok -> G.Tk ((* "*" *) token env tok)
                 | `AMP tok -> G.Tk ((* "&" *) token env tok)
                 | `HASH tok -> G.Tk ((* "#" *) token env tok)
                 | `PERC tok -> G.Tk ((* "%" *) token env tok)
                 | `HAT tok -> G.Tk ((* "^" *) token env tok)
                 | `PLUS tok -> G.Tk ((* "+" *) token env tok)
                 | `LT tok -> G.Tk ((* "<" *) token env tok)
                 | `GT tok -> G.Tk ((* ">" *) token env tok)
                 | `BAR tok -> G.Tk ((* "|" *) token env tok)
                 | `TILDE tok -> G.Tk ((* "~" *) token env tok))
               xs)
      | `SQUOT tok -> G.I (str env tok) (* "'" *)
      | `As tok -> G.I (str env tok) (* "as" *)
      | `Async tok -> G.I (str env tok) (* "async" *)
      | `Await tok -> G.I (str env tok) (* "await" *)
      | `Brk tok -> G.I (str env tok) (* "break" *)
      | `Const tok -> G.I (str env tok) (* "const" *)
      | `Cont tok -> G.I (str env tok) (* "continue" *)
      | `Defa tok -> G.I (str env tok) (* "default" *)
      | `Enum tok -> G.I (str env tok) (* "enum" *)
      | `Fn tok -> G.I (str env tok) (* "fn" *)
      | `For tok -> G.I (str env tok) (* "for" *)
      | `If tok -> G.I (str env tok) (* "if" *)
      | `Impl tok -> G.I (str env tok) (* "impl" *)
      | `Let tok -> G.I (str env tok) (* "let" *)
      | `Loop tok -> G.I (str env tok) (* "loop" *)
      | `Match tok -> G.I (str env tok) (* "match" *)
      | `Mod tok -> G.I (str env tok) (* "mod" *)
      | `Pub tok -> G.I (str env tok) (* "pub" *)
      | `Ret tok -> G.I (str env tok) (* "return" *)
      | `Static tok -> G.I (str env tok) (* "static" *)
      | `Struct tok -> G.I (str env tok) (* "struct" *)
      | `Trait tok -> G.I (str env tok) (* "trait" *)
      | `Type tok -> G.I (str env tok) (* "type" *)
      | `Union tok -> G.I (str env tok) (* "union" *)
      | `Unsafe tok -> G.I (str env tok) (* "unsafe" *)
      | `Use tok -> G.I (str env tok) (* "use" *)
      | `Where tok -> G.I (str env tok) (* "where" *)
      | `While tok -> G.I (str env tok))
  | `Ellips tok ->
      let s, t = (* "..." *) str env tok in
      if s = "..." && env.extra =*= Pattern then G.E (G.Ellipsis t |> G.e)
      else G.Tk t

let map_function_modifiers (env : env) (xs : CST.function_modifiers) :
    G.attribute list =
  List.concat_map
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

let map_for_lifetimes (env : env)
    ((v1, v2, v3, v4, _v5TODO, v6) : CST.for_lifetimes) : lifetime list =
  let _for_ = token env v1 (* "for" *) in
  let _lthan = token env v2 (* "<" *) in
  let lifetime_first = map_lifetime env v3 in
  let lifetime_rest =
    List_.map
      (fun (v1, v2) ->
        let _comma = token env v1 (* "," *) in
        let lifetime = map_lifetime env v2 in
        lifetime)
      v4
  in
  let _comma = Option.map (fun tok -> token env tok (* "," *)) in
  let _gthan = token env v6 (* ">" *) in
  lifetime_first :: lifetime_rest

let rec map_token_tree (env : env) (x : CST.token_tree) :
    rust_macro_item list G.bracket =
  match x with
  | `LPAR_rep_choice_tok_tree_RPAR (v1, v2, v3) ->
      let lparen = token env v1 (* "(" *) in
      let tokens = List_.map (map_tokens env) v2 in
      let rparen = token env v3 (* ")" *) in
      (lparen, tokens, rparen)
  | `LBRACK_rep_choice_tok_tree_RBRACK (v1, v2, v3) ->
      let lbracket = token env v1 (* "[" *) in
      let tokens = List_.map (map_tokens env) v2 in
      let rbracket = token env v3 (* "]" *) in
      (lbracket, tokens, rbracket)
  | `LCURL_rep_choice_tok_tree_RCURL (v1, v2, v3) ->
      let lbrace = token env v1 (* "{" *) in
      let tokens = List_.map (map_tokens env) v2 in
      let rbrace = token env v3 (* "}" *) in
      (lbrace, tokens, rbrace)

and map_tokens (env : env) (x : CST.tokens) : rust_macro_item =
  match x with
  | `Tok_tree x -> MacTree (map_token_tree env x)
  | `Tok_repe (v1, v2, v3, v4, v5, v6) ->
      let _dollarTODO = token env v1 (* "$" *) in
      let lparen = token env v2 (* "(" *) in
      let tokens = List_.map (map_tokens env) v3 in
      let rparen = token env v4 (* ")" *) in
      let ident = Option.map (fun tok -> ident env tok) v5 in
      (* pattern [^+*?]+ *)
      let quantifier = map_token_quantifier env v6 in
      MacTreeBis ((lparen, tokens, rparen), ident, quantifier)
  | `Meta tok ->
      let tok = (* pattern \$[a-zA-Z_]\w* *) str env tok in
      MacAny (G.I tok)
  | `Choice_choice_lit x -> MacAny (map_non_special_token env x)

let map_non_delim_token (env : env) (x : CST.non_delim_token) =
  match x with
  | `Choice_choice_lit x -> MacAny (map_non_special_token env x)
  | `DOLLAR tok -> MacAny (G.Tk ((* "$" *) token env tok))

let rec map_delim_token_tree (env : env) (x : CST.delim_token_tree) :
    rust_macro_item list G.bracket =
  match x with
  | `LPAR_rep_delim_tokens_RPAR (v1, v2, v3) ->
      let lparen = token env v1 (* "(" *) in
      let tokens = List_.map (map_delim_tokens env) v2 in
      let rparen = token env v3 (* ")" *) in
      (lparen, tokens, rparen)
  | `LBRACK_rep_delim_tokens_RBRACK (v1, v2, v3) ->
      let lbracket = token env v1 (* "[" *) in
      let tokens = List_.map (map_delim_tokens env) v2 in
      let rbracket = token env v3 (* "]" *) in
      (lbracket, tokens, rbracket)
  | `LCURL_rep_delim_tokens_RCURL (v1, v2, v3) ->
      let lbrace = token env v1 (* "{" *) in
      let tokens = List_.map (map_delim_tokens env) v2 in
      let rbrace = token env v3 (* "}" *) in
      (lbrace, tokens, rbrace)

and map_delim_tokens (env : env) (x : CST.delim_tokens) =
  match x with
  | `Non_delim_tok x -> map_non_delim_token env x
  | `Delim_tok_tree x -> MacTree (map_delim_token_tree env x)

let rec map_token_pattern (env : env) (x : CST.token_pattern) :
    rust_macro_pattern =
  match x with
  | `Tok_tree_pat x -> RustMacPatTree (map_token_tree_pattern env x)
  | `Tok_repe_pat (v1, v2, v3, v4, v5, v6) ->
      let _dollarTODO = token env v1 (* "$" *) in
      let lparen = token env v2 (* "(" *) in
      let patterns = List_.map (map_token_pattern env) v3 in
      let rparen = token env v4 (* ")" *) in
      let ident = Option.map (fun tok -> ident env tok) v5 in
      (* pattern [^+*?]+ *)
      let quantifier = map_token_quantifier env v6 in
      RustMacPatRepetition ((lparen, patterns, rparen), ident, quantifier)
  | `Tok_bind_pat (v1, v2, v3) ->
      let ident = ident env v1 (* pattern \$[a-zA-Z_]\w* *) in
      let _colon = token env v2 (* ":" *) in
      let fragment_specifier = map_fragment_specifier env v3 in
      RustMacPatBinding (ident, fragment_specifier)
  | `Meta tok ->
      let tok = (* pattern \$[a-zA-Z_]\w* *) str env tok in
      RustMacPatToken (G.I tok)
  | `Choice_choice_lit x -> RustMacPatToken (map_non_special_token env x)

and map_token_tree_pattern (env : env) (x : CST.token_tree_pattern) :
    rust_macro_pattern list =
  match x with
  | `LPAR_rep_tok_pat_RPAR (v1, v2, v3) ->
      let _lparen = token env v1 (* "(" *) in
      let patterns = List_.map (map_token_pattern env) v2 in
      let _rparen = token env v3 (* ")" *) in
      patterns
  | `LBRACK_rep_tok_pat_RBRACK (v1, v2, v3) ->
      let _lbracket = token env v1 (* "[" *) in
      let patterns = List_.map (map_token_pattern env) v2 in
      let _rbracket = token env v3 (* "]" *) in
      patterns
  | `LCURL_rep_tok_pat_RCURL (v1, v2, v3) ->
      let _lbrace = token env v1 (* "{" *) in
      let patterns = List_.map (map_token_pattern env) v2 in
      let _rbrace = token env v3 (* "}" *) in
      patterns

and map_macro_rule (env : env) ((v1, v2, v3) : CST.macro_rule) : rust_macro_rule
    =
  let rules = map_token_tree_pattern env v1 in
  let _arrow = token env v2 (* "=>" *) in
  let body = map_token_tree env v3 in
  { rules; body }

let rec map_abstract_type_trait_name (env : env) x : G.type_ =
  match x with
  | `Id tok ->
      let ident = ident env tok in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      G.TyN (H2.name_of_id ident) |> G.t
  | `Scoped_type_id x ->
      let n = map_scoped_type_identifier_name env x in
      G.TyN n |> G.t
  | `Gene_type x ->
      let name = map_generic_type_name env x in
      G.TyN name |> G.t
  | `Func_type x -> map_function_type env x

and map_struct_name (env : env) x : G.name =
  match x with
  | `Id tok -> H2.name_of_id (ident env tok)
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  | `Scoped_type_id x -> map_scoped_type_identifier_name env x

and map_tuple_struct_name (env : env) x : G.name =
  match x with
  | `Id tok -> H2.name_of_id (ident env tok)
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  | `Scoped_id x -> map_scoped_identifier_name env x

and map_struct_pattern_field (env : env) (x : CST.anon_choice_field_pat_8e757e8)
    : G.dotted_ident * G.pattern =
  match x with
  | `Field_pat (_v1, v2, v3) -> (
      let _ref_ = Option.map (fun tok -> token env tok) in
      (* "ref" *)
      let _mutability_attr =
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
          let _colon = token env v2 (* ":" *) in
          let pat = map_pattern env v3 in
          ([ ident ], pat))
  | `Rema_field_pat tok ->
      let ident = ident env tok in
      (* ".." *)
      let name = [ ident ] in
      (name, G.OtherPat (("..", token env tok), []))

and map_type_parameter (env : env) (x : CST.anon_choice_life_859e88f) :
    G.type_parameter =
  match x with
  | `Life x ->
      let id = map_lifetime env x in
      G.OtherTypeParam (("LifeTime", snd id), [ G.I id ])
  | `Meta tok ->
      let meta = ident env tok in
      (* pattern \$[a-zA-Z_]\w* *)
      G.tparam_of_id meta
  | `Id tok ->
      let ident = ident env tok in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      G.tparam_of_id ident
  | `Cons_type_param x -> map_constrained_type_parameter env x
  | `Opt_type_param (v1, v2, v3) -> (
      let type_param =
        match v1 with
        | `Id tok ->
            let ident = ident env tok in
            (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
            G.tparam_of_id ident
        | `Cons_type_param x -> map_constrained_type_parameter env x
      in
      let _equal = token env v2 (* "=" *) in
      let default = map_type_ env v3 in
      match type_param with
      | TParamEllipsis _ -> raise Impossible
      | TP x -> TP { x with G.tp_default = Some default }
      | OtherTypeParam (x, anys) -> OtherTypeParam (x, G.T default :: anys))
  | `Const_param (v1, v2, v3, v4) ->
      let const = token env v1 in
      (* "const" *)
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let _colon = token env v3 in
      (* ":" *)
      let ty = map_type_ env v4 in
      G.tparam_of_id ident
        ~tp_attrs:[ G.KeywordAttr (G.Const, const) ]
        ~tp_bounds:[ ty ]

and map_range_pattern_bound (env : env) (x : CST.anon_choice_lit_pat_0884ef0) :
    G.pattern =
  match x with
  | `Lit_pat x -> map_literal_pattern env x
  | `Choice_self x ->
      let name = map_path_name env x in
      G.PatConstructor (name, [])

and map_anon_choice_param_2c23cdc (env : env) _outer_attrTODO
    (x : CST.anon_choice_param_2c23cdc) : G.parameter =
  match x with
  | `Param x -> map_parameter env x
  | `Self_param (v1, v2, v3, v4) ->
      let borrow = Option.map (fun tok -> token env tok (* "&" *)) v1 in
      let _lifetime = Option.map (fun x -> map_lifetime env x) v2 in
      let mutability =
        Option.map
          (fun tok ->
            let t = token env tok in
            (* "mut" *)
            G.KeywordAttr (G.Mutable, t))
          v3
      in
      let self = ident env v4 (* "self" *) in
      let self_type = G.TyN (H2.name_of_id self) |> G.t in
      let type_ =
        match borrow with
        | Some tok -> G.TyRef (tok, self_type) |> G.t
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
      G.Param param
  | `Vari_param tok -> G.ParamEllipsis (token env tok) (* "..." *)
  | `X__ tok ->
      (* ellided parameter *)
      G.ParamPattern (G.PatWildcard (token env tok))
  | `Type x -> (
      let ty = map_type_ env x in
      match ty.t with
      (* If this type is a singular identifier that is a metavariable,
       * then the user probably meant to write a metavariable parameter.
       * So let's translate it to one.
       *)
      | G.TyN (Id (((s, _) as id), _))
        when AST_generic.is_metavar_name s && in_pattern env ->
          let param =
            {
              G.pname = Some id;
              G.ptype = None;
              G.pdefault = None;
              G.pattrs = [];
              G.pinfo = G.empty_id_info ();
            }
          in
          G.Param param
      | _ ->
          let param =
            {
              G.pname = None;
              G.ptype = Some ty;
              G.pdefault = None;
              G.pattrs = [];
              G.pinfo = G.empty_id_info ();
            }
          in
          G.Param param)

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
      let _outer_attrs = List_.map (map_outer_attribute_item env) v1 in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let lhs = G.N (G.Id (ident, G.empty_id_info ())) |> G.e in
      (* bound variable with same ident as field name *)
      let rhs = G.N (G.Id (ident, G.empty_id_info ())) |> G.e in
      G.Assign (lhs, G.fake ":", rhs) |> G.e
  | `Field_init (v1, v2, v3, v4) ->
      let _outer_attrs = List_.map (map_outer_attribute_item env) v1 in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let lhs = G.N (G.Id (ident, G.empty_id_info ())) |> G.e in
      let colon = token env v3 (* ":" *) in
      let rhs = map_expression env v4 in
      G.Assign (lhs, colon, rhs) |> G.e
  | `Base_field_init x -> map_base_field_initializer env x

and map_type_argument (env : env) (x : CST.anon_choice_type_39799c3) :
    G.type_argument =
  match x with
  | `Type x -> G.TA (map_type_ env x)
  | `Type_bind (v1, v2, v3, v4) ->
      let _identTODO = ident env v1 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let _tyargs = Option.map (map_type_arguments env) v2 in
      let equals = token env v3 (* "=" *) in
      let ty = map_type_ env v4 in
      G.OtherTypeArg (("TypeBind", equals), [ T ty ])
  | `Life x -> G.OtherTypeArg (map_lifetime env x, [])
  | `Lit x ->
      let lit = map_literal env x in
      G.TAExpr (G.L lit |> G.e)
  | `Blk x ->
      let block_expr = map_block_expr env x in
      G.TAExpr block_expr

and map_tuple_pattern_list (env : env)
    ((v1, v2) : CST.anon_pat_rep_COMMA_pat_2a80f16) : G.pattern list =
  let pattern_first = map_pattern env v1 in
  let pattern_rest =
    List_.map
      (fun (v1, v2) ->
        let _comma = token env v1 (* "," *) in
        let pattern = map_pattern env v2 in
        pattern)
      v2
  in
  pattern_first :: pattern_rest

and map_tuple_pattern_or_expr_list (env : env) (v1, v2) : G.pattern list =
  let pattern_first = map_pattern_or_expr env v1 in
  let pattern_rest =
    List_.map
      (fun (v1, v2) ->
        let _comma = token env v1 (* "," *) in
        let pattern = map_pattern_or_expr env v2 in
        pattern)
      v2
  in
  pattern_first :: pattern_rest

and map_arguments (env : env) ((v1, v2, _v3TODO, v4) : CST.arguments) :
    G.arguments =
  let lparen = token env v1 (* "(" *) in
  let args =
    match v2 with
    | Some (v1, v2, v3) ->
        let _outer_attrs = List_.map (map_outer_attribute_item env) v1 in
        let expr_first = G.Arg (map_expression env v2) in
        let expr_rest =
          List_.map
            (fun (v1, v2, v3) ->
              let _comma = token env v1 (* "," *) in
              let _outer_attrs = List_.map (map_outer_attribute_item env) v2 in
              let expr = map_expression env v3 in
              G.Arg expr)
            v3
        in
        expr_first :: expr_rest
    | None -> []
  in
  let _comma = Option.map (fun tok -> token env tok (* "," *)) in
  let rparen = token env v4 (* ")" *) in
  (lparen, args, rparen)

(* was restricted to appear only in trait_impl_block by ruin *)
and map_associated_type (env : env) ((v1, v2, v3, v4, v5) : CST.associated_type)
    : G.stmt =
  let _type_TODO = token env v1 (* "type" *) in
  let ident = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let type_params = Option.map (map_type_parameters env) v3 in
  let _trait_bounds =
    match v4 with
    | Some x -> map_trait_bounds env x
    | None -> []
  in
  (* ruin:
     let ty =
       Option.map
         (fun (v1, v2) ->
           let _equals = token env v1 (* "=" *) in
           let ty = map_type_ env v2 in
           ty)
         v5
     in
  *)
  let semicolon = token env v5 (* ";" *) in
  let type_def_kind =
    G.AbstractType semicolon
    (* ruin:
       match ty with
       | Some ty -> G.AliasType ty
       | None -> G.AbstractType semicolon
    *)
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

and map_attribute (env : env) tok ((v1, v2) : CST.attribute) : G.attribute =
  let name = map_path_name env v1 in
  match v2 with
  | None -> NamedAttr (tok, name, fb [])
  | Some (`Delim_tok_tree x) -> (
      let l, macro_items, r = map_delim_token_tree env x in
      match macro_items_to_anys macro_items with
      | [ G.Args args ] -> NamedAttr (tok, name, (l, args, r))
      | anys ->
          (* TODO Should these each be an individual arg? *)
          let arg = G.OtherArg (("AttrMisc", tok), anys) in
          NamedAttr (tok, name, (l, [ arg ], r)))
  | Some (`EQ_exp (v1, v2)) ->
      let _equals = token env v1 (* "=" *) in
      let expr = map_expression env v2 in
      OtherAttribute (("AttrAssign", tok), G.[ Name name; E expr ])

and map_base_field_initializer (env : env)
    ((v1, v2) : CST.base_field_initializer) : G.expr =
  let dots = token env v1 (* ".." *) in
  let lhs = G.IdSpecial (G.Spread, dots) |> G.e in
  (* Copy remaining struct fields from this existing instance *)
  let rhs = map_expression env v2 in
  G.AssignOp (lhs, (G.Append, dots), rhs) |> G.e

and map_binary_expression (env : env) (x : CST.binary_expression) : G.expr_kind
    =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.And, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.Or, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitAnd, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitOr, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op G.BitXor, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
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
      G.Call (G.IdSpecial (G.Op op, tok) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
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
      G.Call (G.IdSpecial (G.Op op, tok) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let tok, op =
        match v2 with
        | `PLUS tok -> (token env tok, G.Plus) (* "+" *)
        | `DASH tok -> (token env tok, G.Minus)
        (* "-" *)
      in
      let v3 = map_expression env v3 in
      G.Call (G.IdSpecial (G.Op op, tok) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
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
      G.Call (G.IdSpecial (G.Op op, tok) |> G.e, fb [ G.Arg v1; G.Arg v3 ])

and map_block (env : env) ((v1, v2, v3, v4, v5) : CST.block) : G.stmt =
  let label =
    match v1 with
    | Some (v1, v2) ->
        let v1 = map_label env v1 in
        let _v2 = (* ":" *) token env v2 in
        Some v1
    | None -> None
  in
  let lbrace = token env v2 (* "{" *) in
  let stmts = map_statements_list env v3 in
  let final_expr =
    match v4 with
    | Some x ->
        let expr = map_expression env x in
        let stmt = G.ExprStmt (expr, sc) |> G.s in
        [ stmt ]
    | None -> []
  in
  let rbrace = token env v5 (* "}" *) in
  let block = G.Block (lbrace, stmts @ final_expr, rbrace) |> G.s in
  match label with
  | Some l -> G.Label (l, block) |> G.s
  | None -> block

and map_block_expr (env : env) (block : CST.block) : G.expr =
  let block = map_block env block in
  G.stmt_to_expr block

and map_bounded_type (env : env) (x : CST.bounded_type) : G.type_ =
  match x with
  | `Life_PLUS_type (v1, v2, v3) ->
      let lifetime = map_lifetime env v1 in
      let plus = token env v2 (* "+" *) in
      let type_ = map_type_ env v3 in
      G.TyOr
        (G.OtherType (("Lifetime", plus), [ G.I lifetime ]) |> G.t, plus, type_)
      |> G.t
  | `Type_PLUS_type (v1, v2, v3) ->
      let type_a = map_type_ env v1 in
      let plus = token env v2 (* "+" *) in
      let type_b = map_type_ env v3 in
      G.TyOr (type_a, plus, type_b) |> G.t
  | `Type_PLUS_life (v1, v2, v3) ->
      let type_ = map_type_ env v1 in
      let plus = token env v2 (* "+" *) in
      let lifetime = map_lifetime env v3 in
      G.TyOr
        (type_, plus, G.OtherType (("Lifetime", plus), [ G.I lifetime ]) |> G.t)
      |> G.t

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
    G.parameters =
  let lpipe = token env v1 (* "|" *) in
  let params =
    match v2 with
    | Some (v1, v2) ->
        let param_first = map_closure_parameter env v1 in
        let param_rest =
          List_.map
            (fun (v1, v2) ->
              let _comma = token env v1 (* "," *) in
              let param = map_closure_parameter env v2 in
              param)
            v2
        in
        param_first :: param_rest
    | None -> []
  in
  let rpipe = token env v3 (* "|" *) in
  (lpipe, params, rpipe)

and map_const_block (env : env) ((v1, v2) : CST.const_block) : G.expr =
  let tconst = token env v1 (* "const" *) in
  let block = map_block env v2 in
  let stmt =
    G.OtherStmtWithStmt (G.OSWS_Block ("Const", tconst), [], block) |> G.s
  in
  G.stmt_to_expr stmt

and map_const_item (env : env) outer_attrs
    ((_v0TODO, v1, v2, v3, v4, v5, v6) : CST.const_item) : G.stmt =
  (* TODO v0 optional visibility modif *)
  let tconst = token env v1 (* "const" *) in
  let attrs = G.KeywordAttr (G.Const, tconst) :: outer_attrs in
  let id = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let _tcolon = token env v3 (* ":" *) in
  let type_ = map_type_ env v4 in
  let init =
    Option.map
      (fun (v1, v2) ->
        let _equals = token env v1 (* "=" *) in
        let expr = map_expression env v2 in
        expr)
      v5
  in
  let sc = token env v6 (* ";" *) in
  let var_def = { G.vinit = init; G.vtype = Some type_; vtok = Some sc } in
  let ent =
    { G.name = G.EN (G.Id (id, G.empty_id_info ())); G.attrs; G.tparams = None }
  in
  G.DefStmt (ent, G.VarDef var_def) |> G.s

and map_constrained_type_parameter (env : env)
    ((v1, v2) : CST.constrained_type_parameter) : G.type_parameter =
  let ident =
    match v1 with
    | `Life x -> map_lifetime env x
    | `Id tok -> ident env tok
    (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  in
  let _boundsTODO = map_trait_bounds env v2 in
  G.tparam_of_id ident

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) : G.stmt =
  let _else_ = token env v1 (* "else" *) in
  match v2 with
  | `Blk x ->
      (* plain else *)
      map_block env x
  | `If_exp x ->
      (* else if *)
      let if_expr = map_if_expression env x in
      G.ExprStmt (if_expr, sc) |> G.s

and map_enum_variant (env : env) ((v1, v2, v3, v4) : CST.enum_variant) :
    G.or_type_element =
  let _visibilityTODO =
    match v1 with
    | Some x -> map_visibility_modifier env x
    | None -> []
  in
  let ident = ident env v2 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  let init =
    Option.map
      (fun (v1, v2) ->
        let _equals = token env v1 (* "=" *) in
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
  let _lbrace = token env v1 (* "{" *) in
  let variants =
    match v2 with
    | Some (v1, v2, v3) ->
        let _outer_attributes = List_.map (map_outer_attribute_item env) v1 in
        let variant_first = map_enum_variant env v2 in
        let variant_rest =
          List_.map
            (fun (v1, v2, v3) ->
              let _comma = token env v1 (* "," *) in
              let _outer_attributes =
                List_.map (map_outer_attribute_item env) v2
              in
              let variant = map_enum_variant env v3 in
              variant)
            v3
        in
        variant_first :: variant_rest
    | None -> []
  in
  let _comma = Option.map (fun tok -> token env tok (* "," *)) v3 in
  let _rbrace = token env v4 (* "}" *) in
  G.OrType variants

and map_expression (env : env) (x : CST.expression) =
  match x with
  | `Exp_except_range x -> map_expression_except_range env x
  | `Range_exp x -> map_range_expression env x
  | `Ellips tok -> G.Ellipsis (token env tok) (* "..." *) |> G.e
  | `Deep_ellips (v1, v2, v3) ->
      let lellips = token env v1 (* "<..." *) in
      let expr = map_expression env v2 in
      let rellips = token env v3 (* "...>" *) in
      G.DeepEllipsis (lellips, expr, rellips) |> G.e
  | `Member_access_ellips_exp (e, _, dots) ->
      G.DotAccessEllipsis (map_expression env e, token env dots) |> G.e

and map_expression_except_range (env : env) (x : CST.expression_except_range) =
  (match x with
  | `Un_exp (v1, v2) -> (
      let expr = map_expression env v2 in
      match v1 with
      | `DASH tok ->
          let tok = token env tok in
          (* "-" *)
          G.Call (G.IdSpecial (G.Op G.Minus, tok) |> G.e, fb [ G.Arg expr ])
      | `STAR tok ->
          let tok = token env tok in
          (* "*" *)
          G.DeRef (tok, expr)
      | `BANG tok ->
          let tok = token env tok in
          (* "!" *)
          G.Call (G.IdSpecial (G.Op G.Not, tok) |> G.e, fb [ G.Arg expr ]))
  | `Ref_exp (v1, v2, v3) ->
      let ref_ = token env v1 (* "&" *) in
      let _mutabilityTODO =
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
      G.Call (G.IdSpecial (G.Op G.Elvis, question) |> G.e, fb [ G.Arg expr ])
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
      G.Cast (type_, as_, expr)
  | `Call_exp (v1, v2) ->
      let expr = map_expression_except_range env v1 in
      let args = map_arguments env v2 in
      G.Call (expr, args)
  | `Ret_exp x ->
      let x = map_return_expression env x in
      x.G.e
  | `Yield_exp x -> map_yield_expression env x
  | `Lit x -> G.L (map_literal env x)
  | `Id tok -> G.N (H2.name_of_id (ident env tok))
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  | `Choice_u8 x ->
      let tok = map_primitive_type_token env x in
      G.N (H2.name_of_id (ident env tok))
  | `Choice_defa x ->
      let ident = map_reserved_identifier env x in
      G.N (H2.name_of_id ident)
  | `Self tok -> G.IdSpecial (G.Self, token env tok) (* "self" *)
  | `Scoped_id x ->
      let x = map_scoped_identifier_name env x in
      G.N x
  | `Gene_func (v1, v2, v3) -> (
      (* TODO: QTop *)
      let _colons = token env v2 (* "::" *) in
      let typeargs = map_type_arguments env v3 in
      match v1 with
      | `Id tok ->
          let ident = ident env tok in
          (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
          G.N (H2.add_type_args_to_name (H2.name_of_id ident) typeargs)
      | `Scoped_id x ->
          let n = map_scoped_identifier_name env x in
          G.N (H2.add_type_args_to_name n typeargs)
      | `Field_exp x ->
          let x = map_field_expression env x (Some typeargs) in
          x.G.e)
  | `Await_exp (v1, v2, v3) ->
      let expr = map_expression env v1 in
      let _dot = token env v2 (* "." *) in
      let await = token env v3 (* "await" *) in
      G.Await (await, expr)
  | `Field_exp x ->
      let x = map_field_expression env x None in
      x.G.e
  | `Array_exp (v1, v2, v3, v4) ->
      let lbracket = token env v1 (* "[" *) in
      let _outer_attrs = List_.map (map_outer_attribute_item env) v2 in
      let exprs =
        match v3 with
        | `Exp_SEMI_exp (v1, v2, v3) ->
            let ty = map_expression env v1 in
            let _semicolon = token env v2 (* ";" *) in
            let init = map_expression env v3 in
            [ ty; init ]
        | `Opt_rep_attr_item_exp_rep_COMMA_rep_attr_item_exp_opt_COMMA (v1, v2)
          ->
            let exprs_with_attrs = map_expressions_with_attributes env v1 in
            (* TODO attrs *)
            let exprs = List_.map snd exprs_with_attrs in
            let _comma = Option.map (fun tok -> token env tok (* "," *)) v2 in
            exprs
      in
      let rbracket = token env v4 (* "]" *) in
      G.Container (G.Array, (lbracket, exprs, rbracket))
  | `Tuple_exp (v1, v2, v3, v4, v5, v6, v7) ->
      let lparen = token env v1 (* "(" *) in
      let _outer_attrs = List_.map (map_outer_attribute_item env) v2 in
      let expr_first = map_expression env v3 in
      let _comma = token env v4 (* "," *) in
      let expr_rest =
        List_.map
          (fun (v1, v2) ->
            let expr = map_expression env v1 in
            let _comma = token env v2 (* "," *) in
            expr)
          v5
      in
      let expr_last =
        match v6 with
        | Some x -> [ map_expression env x ]
        | None -> []
      in
      let rparen = token env v7 (* ")" *) in
      let exprs = List_.flatten [ [ expr_first ]; expr_rest; expr_last ] in
      G.Container (G.Tuple, (lparen, exprs, rparen))
  | `Macro_invo x ->
      let x = map_macro_invocation env x in
      x.G.e
  | `Unit_exp (v1, v2) ->
      let lparen = token env v1 (* "(" *) in
      let _rparen = token env v2 (* ")" *) in
      G.L (G.Unit lparen)
  | `Choice_unsafe_blk x ->
      let x = map_expression_ending_with_block env x in
      x.G.e
  | `Brk_exp (v1, v2, v3) ->
      let break = token env v1 (* "break" *) in
      let label =
        match v2 with
        | Some x -> G.LId (map_label env x)
        | None -> G.LNone
      in
      let _exprTODO = Option.map (fun x -> map_expression env x) v3 in
      let break_stmt = G.Break (break, label, sc) |> G.s in
      (* TODO expr *)
      let x = G.stmt_to_expr break_stmt in
      x.G.e
  | `Cont_exp (v1, v2) ->
      let continue = token env v1 (* "continue" *) in
      let label =
        match v2 with
        | Some x -> G.LId (map_label env x)
        | None -> G.LNone
      in
      let continue_stmt = G.Continue (continue, label, sc) |> G.s in
      let x = G.stmt_to_expr continue_stmt in
      x.G.e
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
  | `Clos_exp x -> map_closure_expression env x
  | `Paren_exp (_v1, v2, _v3) -> (
      match v2 with
      | `Exp e ->
          let x = map_expression env e in
          x.G.e
      | `Semg_typed_meta (v, c, t) ->
          let metavar = ident env v in
          if AST_generic.is_metavar_name (fst metavar) && in_pattern env then
            let colon = token env c in
            let type_ = map_type_ env t in
            G.TypedMetavar (metavar, colon, type_)
          else raise Impossible)
  | `Struct_exp (v1, v2) ->
      let name : G.name =
        match v1 with
        | `Id tok ->
            let ident = ident env tok in
            (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
            H2.name_of_id ident
        | `Scoped_type_id_in_exp_posi x ->
            map_scoped_type_identifier_in_expression_position env x
        | `Gene_type_with_turb x -> map_generic_type_with_turbofish env x
      in
      let l, fields, r = map_field_initializer_list env v2 in
      G.Constructor (name, (l, fields, r)))
  |> G.e

and map_expressions_with_attributes (env : env)
    (opt : CST.anon_opt_rep_attr_item_exp_rep_COMMA_rep_attr_item_exp_3d9e0d4) =
  match opt with
  | Some (v1, v2, v3) ->
      let attrs_first = List_.map (map_outer_attribute_item env) v1 in
      let expr_first = map_expression env v2 in
      let expr_rest =
        List_.map
          (fun (v1, v2, v3) ->
            let _comma = token env v1 (* "," *) in
            let attrs = List_.map (map_outer_attribute_item env) v2 in
            let expr = map_expression env v3 in
            (attrs, expr))
          v3
      in
      (attrs_first, expr_first) :: expr_rest
  | None -> []

and map_closure_expression (env : env)
    ((v1, v2, v3, v4) : CST.closure_expression) =
  let _is_staticTODO =
    Option.map
      (fun tok ->
        let tok = token env tok in
        (* "static" *)
        G.KeywordAttr (G.Static, tok))
      v1
  in
  let _is_moveTODO =
    Option.map
      (fun tok ->
        let tok = token env tok in
        (* "move" *)
        G.KeywordAttr (G.Mutable, tok))
      v2
  in
  let params = map_closure_parameters env v3 in
  let ret_type, body =
    match v4 with
    | `Opt_DASHGT_type_blk (v1, v2) ->
        let ret_type =
          Option.map
            (fun (v1, v2) ->
              let _arrow = token env v1 (* "->" *) in
              let ty = map_type_ env v2 in
              ty)
            v1
        in
        let body = map_block env v2 in
        (ret_type, G.FBStmt body)
    | `Choice_exp x -> (
        match x with
        | `Exp x ->
            let expr = map_expression env x in
            (None, G.FBExpr expr)
        | `X__ _tok -> (* TODO what is this? *) (None, G.FBNothing))
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

and map_yield_expression (env : env) (x : CST.yield_expression) =
  match x with
  | `Yield_exp (v1, v2) ->
      let v1 = (* "yield" *) token env v1 in
      let v2 = map_expression env v2 in
      G.Yield (v1, Some v2, false)
  | `Yield tok ->
      let tok = (* "yield" *) token env tok in
      G.Yield (tok, None, false)

and map_expression_ending_with_block (env : env)
    (x : CST.expression_ending_with_block) : G.expr =
  let map_loop_label_ (v1, v2) =
    let loop_label = G.LId (map_label env v1) in
    let _colon = token env v2 (* ":" *) in
    loop_label
  in
  match x with
  | `Unsafe_blk (v1, v2) ->
      let tunsafe = token env v1 (* "unsafe" *) in
      let block = map_block env v2 in
      let stmt =
        G.OtherStmtWithStmt (G.OSWS_Block ("Unsafe", tunsafe), [], block) |> G.s
      in
      G.stmt_to_expr stmt
  | `Async_blk (v1, v2, v3) ->
      let tasync = token env v1 (* "async" *) in
      let _moveTODO = Option.map (fun tok -> token env tok (* "move" *)) v2 in
      let block = map_block env v3 in
      let stmt =
        G.OtherStmtWithStmt (G.OSWS_Block ("Async", tasync), [], block) |> G.s
      in
      G.stmt_to_expr stmt
  | `Try_blk (v1, v2) ->
      let v1 = (* "try" *) token env v1 in
      let v2 = map_block env v2 in
      let stmt = G.Try (v1, v2, [], None, None) |> G.s in
      G.stmt_to_expr stmt
  | `Blk x -> map_block_expr env x
  | `If_exp x -> map_if_expression env x
  | `Match_exp (v1, v2, v3) ->
      let t = token env v1 (* "match" *) in
      let expr = map_expression env v2 in
      let actions =
        map_match_block env v3 |> List_.map G.case_of_pat_and_expr
      in
      let st = G.Switch (t, Some (G.Cond expr), actions) |> G.s in
      G.stmt_to_expr st
  | `While_exp (v1, v2, v3, v4) ->
      let _loop_labelTODO = Option.map map_loop_label_ v1 in
      let while_ = token env v2 (* "while" *) in
      let cond = map_condition env v3 in
      let body = map_block env v4 in
      let while_stmt = G.While (while_, cond, body) |> G.s in
      G.stmt_to_expr while_stmt
  | `Loop_exp (v1, v2, v3) ->
      let _loop_labelTODO = Option.map map_loop_label_ v1 in
      let loop = token env v2 (* "loop" *) in
      let cond = G.L (G.Bool (true, G.fake "true")) |> G.e in
      (* dummy, acts as 'while true' *)
      let body = map_block env v3 in
      let loop_stmt = G.While (loop, G.Cond cond, body) |> G.s in
      G.stmt_to_expr loop_stmt
  | `For_exp (v1, v2, v3, v4, v5, v6) ->
      let _loop_labelTODO = Option.map map_loop_label_ v1 in
      let for_ = token env v2 (* "for" *) in
      let pattern = map_pattern env v3 in
      let in_ = token env v4 (* "in" *) in
      let expr = map_expression env v5 in
      let body = map_block env v6 in
      let for_header = G.ForEach (pattern, in_, expr) in
      let for_stmt = G.For (for_, for_header, body) |> G.s in
      G.stmt_to_expr for_stmt
  | `Const_blk x -> map_const_block env x

and map_condition (env : env) (x : CST.condition) =
  match x with
  | `Exp x -> G.Cond (map_expression env x)
  | `Let_cond x ->
      let tok, pat, expr = map_let_condition env x in
      G.OtherCond (("LetCond", tok), [ G.P pat; G.E expr ])
  | `Let_chain x ->
      let anys = map_let_chain env x in
      G.OtherCond (("LetChain", G.fake ""), anys)

and map_let_condition (env : env) ((v1, v2, v3, v4) : CST.let_condition) =
  let v1 = (* "let" *) token env v1 in
  let v2 = map_pattern env v2 in
  let _v3 = (* "=" *) token env v3 in
  let v4 = map_expression env v4 in
  (v1, v2, v4)

and map_let_chain (env : env) (x : CST.let_chain) : G.any list =
  (* TODO Construct a semantically reasonable AST *)
  match x with
  | `Let_chain_AMPAMP_let_cond (v1, v2, v3) ->
      let v1 = map_let_chain env v1 in
      let _v2 = (* "&&" *) token env v2 in
      let _tok, pat, expr = map_let_condition env v3 in
      [ G.Anys v1; G.P pat; G.E expr ]
  | `Let_chain_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_let_chain env v1 in
      let _v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      [ G.Anys v1; G.E v3 ]
  | `Let_cond_AMPAMP_exp (v1, v2, v3) ->
      let _tok, pat, expr = map_let_condition env v1 in
      let _v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      [ G.P pat; G.E expr; G.E v3 ]
  | `Let_cond_AMPAMP_let_cond (v1, v2, v3) ->
      let _tok1, pat1, expr1 = map_let_condition env v1 in
      let _v2 = (* "&&" *) token env v2 in
      let _tok3, pat3, expr3 = map_let_condition env v3 in
      [ G.P pat1; G.E expr1; G.P pat3; G.E expr3 ]
  | `Exp_AMPAMP_let_cond (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let _v2 = (* "&&" *) token env v2 in
      let _tok, pat, expr = map_let_condition env v3 in
      [ G.E v1; G.P pat; G.E expr ]

and map_expression_statement (env : env) (x : CST.expression_statement) : G.stmt
    =
  (* We don't want to get ExprStmt of StmtExpr, if possible. *)
  let simplify sc expr =
    match expr with
    | { G.e = G.StmtExpr stmt; _ } -> stmt
    | expr -> G.ExprStmt (expr, sc) |> G.s
  in
  match x with
  | `Choice_exp_SEMI x -> (
      match x with
      | `Exp_SEMI (v1, v2) ->
          let expr = map_expression env v1 in
          let sc = token env v2 (* ";" *) in
          simplify sc expr
      | `Choice_unsafe_blk x ->
          let expr = map_expression_ending_with_block env x in
          simplify sc expr)
  | `Ellips_SEMI (v1, v2) ->
      let ellipsis = token env v1 (* "..." *) in
      let sc = token env v2 (* ";" *) in
      let expr = G.Ellipsis ellipsis |> G.e in
      G.ExprStmt (expr, sc) |> G.s
  | `Ellips tok ->
      let ellipsis = token env tok in
      (* "..." *)
      let expr = G.Ellipsis ellipsis |> G.e in
      G.ExprStmt (expr, sc) |> G.s

and map_field_declaration (env : env) (x : CST.field_declaration) : G.field =
  match x with
  | `Opt_visi_modi_id_COLON_type (v1, v2, v3, v4) ->
      let attrs =
        match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> []
      in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let _colon = token env v3 (* ":" *) in
      let ty = map_type_ env v4 in
      let var_def = { G.vinit = None; G.vtype = Some ty; vtok = G.no_sc } in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs;
          G.tparams = None;
        }
      in
      G.fld (ent, G.FieldDefColon var_def)
  | `Ellips v1 ->
      let t = token env v1 in
      G.field_ellipsis t

(* for struct definition *)
and map_field_declaration_list (env : env)
    ((v1, v2, v3, v4) : CST.field_declaration_list) : G.field list G.bracket =
  let lbrace = token env v1 (* "{" *) in
  let fields =
    match v2 with
    | Some (v1, v2, v3) ->
        let _outer_attrs = List_.map (map_outer_attribute_item env) v1 in
        let field_first = map_field_declaration env v2 in
        let field_rest =
          List_.map
            (fun (v1, v2, v3) ->
              let _comma = token env v1 (* "," *) in
              let _outer_attrs = List_.map (map_outer_attribute_item env) v2 in
              let field = map_field_declaration env v3 in
              field)
            v3
        in
        field_first :: field_rest
    | None -> []
  in
  let _comma = Option.map (fun tok -> token env tok (* "," *)) v3 in
  let rbrace = token env v4 (* "}" *) in
  (lbrace, fields, rbrace)

and map_field_declaration_type (env : env) (x : CST.field_declaration) : G.type_
    =
  match x with
  | `Opt_visi_modi_id_COLON_type (v1, v2, v3, v4) ->
      let _attrsTODO =
        match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> []
      in
      let _identTODO = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let _colon = token env v3 (* ":" *) in
      let ty = map_type_ env v4 in
      ty
  | `Ellips v1 ->
      let t = token env v1 in
      G.TyEllipsis t |> G.t

(* for enum definition (OrConstructor) *)
and map_field_declaration_list_types (env : env)
    ((v1, v2, v3, v4) : CST.field_declaration_list) : G.type_ list =
  let _lbrace = token env v1 (* "{" *) in
  let types =
    match v2 with
    | Some (v1, v2, v3) ->
        let _outer_attrs = List_.map (map_outer_attribute_item env) v1 in
        let type_first = map_field_declaration_type env v2 in
        let type_rest =
          List_.map
            (fun (v1, v2, v3) ->
              let _comma = token env v1 (* "," *) in
              let _outer_attrs = List_.map (map_outer_attribute_item env) v2 in
              let ty = map_field_declaration_type env v3 in
              ty)
            v3
        in
        type_first :: type_rest
    | None -> []
  in
  let _comma = Option.map (fun tok -> token env tok (* "," *)) v3 in
  let _rbrace = token env v4 (* "}" *) in
  types

and map_field_declaration_union (env : env) (x : CST.field_declaration) :
    G.or_type_element =
  match x with
  | `Opt_visi_modi_id_COLON_type (v1, v2, v3, v4) ->
      let _attrsTODO =
        match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> []
      in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let _colon = token env v3 (* ":" *) in
      let ty = map_type_ env v4 in
      G.OrUnion (ident, ty)
  | `Ellips v1 ->
      let t = token env v1 in
      (* TODO *)
      let ty = G.TyEllipsis t |> G.t in
      G.OrUnion (("...", t), ty)

(* for union definition *)
and map_field_declaration_list_union (env : env)
    ((v1, v2, v3, v4) : CST.field_declaration_list) : G.or_type_element list =
  let _lbrace = token env v1 (* "{" *) in
  let fields =
    match v2 with
    | Some (v1, v2, v3) ->
        let _outer_attrs = List_.map (map_outer_attribute_item env) v1 in
        let field_first = map_field_declaration_union env v2 in
        let field_rest =
          List_.map
            (fun (v1, v2, v3) ->
              let _comma = token env v1 (* "," *) in
              let _outer_attrs = List_.map (map_outer_attribute_item env) v2 in
              let field = map_field_declaration_union env v3 in
              field)
            v3
        in
        field_first :: field_rest
    | None -> []
  in
  let _comma = Option.map (fun tok -> token env tok (* "," *)) v3 in
  let _rbrace = token env v4 (* "}" *) in
  fields

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression)
    (typeargs : G.type_arguments option) : G.expr =
  let expr = map_expression env v1 in
  let dot = token env v2 (* "." *) in
  let ident_or_dyn =
    match v3 with
    | `Id tok ->
        let ident = ident env tok in
        (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
        let n = H2.name_of_id ident in
        let n = H2.add_type_args_opt_to_name n typeargs in
        G.FN n
    | `Int_lit tok -> (
        let literal = G.L (G.Int (integer_literal env tok)) |> G.e in
        (* integer_literal *)
        match typeargs with
        | Some _tas -> raise Impossible
        | None -> G.FDynamic literal)
  in
  G.DotAccess (expr, dot, ident_or_dyn) |> G.e

and map_field_initializer_list (env : env)
    ((v1, v2, v3, v4) : CST.field_initializer_list) : G.expr list G.bracket =
  let lbrace = token env v1 (* "{" *) in
  let fields =
    match v2 with
    | Some (v1, v2) ->
        let field_first = map_field_initializer env v1 in
        let field_rest =
          List_.map
            (fun (v1, v2) ->
              let _comma = token env v1 (* "," *) in
              let field = map_field_initializer env v2 in
              field)
            v2
        in
        field_first :: field_rest
    | None -> []
  in
  let _comma = Option.map (fun tok -> token env tok (* "," *)) v3 in
  let rbrace = token env v4 (* "}" *) in
  (lbrace, fields, rbrace)

(* ruin:
   and map_foreign_block_item (env : env) ((v1, v2, v3) : CST.foreign_block_item) :
       G.stmt =
     let _outer_attrs = List.map (map_outer_attribute_item env) v1 in
     let _visibilityTODO =
       match v2 with
       | Some x -> map_visibility_modifier env x
       | None -> []
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
     let _colon = token env v4 (* ":" *) in
     let ty = map_type_ env v5 in
     let _semicolon = token env v6 (* ";" *) in
     let var_def = { G.vinit = None; G.vtype = Some ty } in
     let ent =
       {
         G.name = G.EN (G.Id (ident, G.empty_id_info ()));
         G.attrs = deoptionalize [ Some static_attr; mutability ];
         G.tparams = None;
       }
     in
     G.DefStmt (ent, G.VarDef var_def) |> G.s

   and map_foreign_mod_block (env : env) ((v1, v2, v3, v4) : CST.foreign_mod_block)
       : G.stmt =
     let lbrace = token env v1 (* "{" *) in
     let _inner_attrsTODO = List.map (map_inner_attribute_item env) v2 in
     let items = List.map (map_foreign_block_item env) v3 in
     let rbrace = token env v4 (* "}" *) in
     let block = G.Block (lbrace, items, rbrace) |> G.s in
     G.OtherStmtWithStmt (G.OSWS_ForeignBlock, [], block) |> G.s
*)
and map_function_declaration (env : env) (v1, v2, v3, v4, v5) :
    function_declaration_rs =
  let name : G.entity_name =
    match v1 with
    | `Id tok ->
        let ident = ident env tok in
        (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
        G.EN (G.Id (ident, G.empty_id_info ()))
    | `Meta tok ->
        let metavar = ident env tok in
        (* pattern \$[a-zA-Z_]\w* *)
        G.EDynamic (G.N (G.Id (metavar, G.empty_id_info ())) |> G.e)
  in
  let type_params = Option.map (map_type_parameters env) v2 in
  let params = map_parameters env v3 in
  let retval =
    Option.map
      (fun (v1, v2) ->
        let _arrow = token env v1 (* "->" *) in
        map_type_ env v2)
      v4
  in
  let _where_clauseTODO = Option.map (fun x -> map_where_clause env x) v5 in
  { name; type_params; params; retval }

and map_function_item (env : env) outer_attrs
    ((_v0TODO, v1, v2, v3, v4, v5, v6, v7, v8) : CST.function_item) : G.stmt =
  (* TODO v0 visi modifier *)
  let attrs =
    match v1 with
    | Some x -> map_function_modifiers env x @ outer_attrs
    | None -> outer_attrs
  in
  let id = token env v2 (* "fn" *) in
  let fn_decl = map_function_declaration env (v3, v4, v5, v6, v7) in
  let body = map_block env v8 in
  let fn_def =
    {
      G.fparams = fn_decl.params;
      G.frettype = fn_decl.retval;
      G.fkind = (G.Function, id);
      G.fbody = G.FBStmt body;
    }
  in
  let ent =
    { G.name = fn_decl.name; G.attrs; G.tparams = fn_decl.type_params }
  in
  G.DefStmt (ent, G.FuncDef fn_def) |> G.s

(* ruin:
   and map_function_signature_with_default_item (env : env)
       ((v1, v2, v3, v4)) : G.definition
       =
     let _modifiersTODO = Option.map (fun x -> map_function_modifiers env x) v1 in
     let fn = token env v2 (* "fn" *) in
     let fn_decl = map_function_declaration env v3 in
     let default_impl =
       match v4 with
       | `SEMI tok ->
           let t = token env tok in
           (* ";" *)
           (* No default implementation *)
           G.FBDecl t
       | `Blk x -> G.FBStmt (map_block env x)
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
*)
and map_function_type (env : env) ((v1, v2, v3, v4) : CST.function_type) :
    G.type_ =
  let _lifetimesTODO =
    match v1 with
    | Some x -> map_for_lifetimes env x
    | None -> []
  in
  let _traitTODO, _modifiersTODO =
    match v2 with
    | `Choice_id x ->
        let trait_name = map_struct_name env x in
        (* FnOnce, FnMut... *)
        (Some trait_name, None)
    | `Opt_func_modifs_fn (v1, v2) ->
        let modifiers = Option.map (fun x -> map_function_modifiers env x) v1 in
        let _fnTODO = token env v2 (* "fn" *) in
        (None, modifiers)
  in
  let _, params, _ = map_parameters env v3 in
  let ret_type =
    match v4 with
    | Some (v1, v2) ->
        let _arrow = token env v1 (* "->" *) in
        let ty = map_type_ env v2 in
        ty
    | None -> G.ty_builtin (fake_id "()")
  in
  G.TyFun (params, ret_type) |> G.t

(* TODO lifetimes, modifiers, traits *)
and map_generic_type_name (env : env) ((v1, v2) : CST.generic_type) : G.name =
  let name =
    match v1 with
    | `Choice_defa x -> map_reserved_identifier env x |> H2.name_of_id
    | (`Id _ | `Scoped_type_id _) as x -> map_struct_name env x
  in
  let typeargs = map_type_arguments env v2 in
  H2.add_type_args_to_name name typeargs

and map_generic_type_with_turbofish (env : env)
    ((v1, v2, v3) : CST.generic_type_with_turbofish) : G.name =
  let name = map_tuple_struct_name env v1 in
  let _colons = token env v2 (* "::" *) in
  let typeargs = map_type_arguments env v3 in
  H2.add_type_args_to_name name typeargs

and map_if_expression (env : env) ((v1, v2, v3, v4) : CST.if_expression) :
    G.expr =
  let if_ = token env v1 (* "if" *) in
  let cond = map_condition env v2 in
  let body = map_block env v3 in
  let else_ = Option.map (fun x -> map_else_clause env x) v4 in
  let if_stmt = G.If (if_, cond, body, else_) |> G.s in
  G.stmt_to_expr if_stmt

(* ruin:
   and map_impl_block (env : env) ((v1, v2, v3, v4) : CST.impl_block) : G.stmt =
     let lbrace = token env v1 (* "{" *) in
     let _inner_attrs = List.map (map_inner_attribute_item env) v2 in
     let stmts = List.map (map_impl_block_item env) v3 in
     let rbrace = token env v4 (* "}" *) in
     let block = G.Block (lbrace, stmts, rbrace) |> G.s in
     G.OtherStmtWithStmt (G.OSWS_ImplBlock, [], block) |> G.s

   and map_impl_block_item (env : env) ((v1, v2, v3) : CST.impl_block_item) :
       G.stmt =
     let _outer_attrs = List.map (map_outer_attribute_item env) v1 in
     let _visibility =
       match v2 with
       | Some x -> map_visibility_modifier env x
       | None -> []
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
     let _colon = token env v3 (* ":" *) in
     let ty = map_type_ env v4 in
     let _equals = token env v5 (* "=" *) in
     let expr = map_expression env v6 in
     let _semicolon = token env v7 (* ";" *) in
     let var_def = { G.vinit = Some expr; G.vtype = Some ty } in
     let ent =
       {
         G.name = G.EN (G.Id (ident, G.empty_id_info ()));
         G.attrs = [ const_attr ];
         G.tparams = None;
       }
     in
     G.DefStmt (ent, G.VarDef var_def) |> G.s

   and map_impl_block_item_type (env : env)
       ((v1, v2, v3, v4, v5, v6) : CST.impl_block_item_type) : G.stmt =
     let _type_TODO = token env v1 (* "type" *) in
     let ident = ident env v2 in
     (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
     let type_params =
       match v3 with
       | Some x -> map_type_parameters env x
       | None -> []
     in
     let _equals = token env v4 (* "=" *) in
     let ty_ = map_type_ env v5 in
     let _semicolon = token env v6 (* ";" *) in
     let type_def = { G.tbody = G.NewType ty_ } in
     let ent =
       {
         G.name = G.EN (G.Id (ident, G.empty_id_info ()));
         G.attrs = [];
         G.tparams = type_params;
       }
     in
     G.DefStmt (ent, G.TypeDef type_def) |> G.s
*)
and map_inner_attribute_item (env : env)
    ((v1, v2, v3, v4, v5) : CST.inner_attribute_item) : G.attribute =
  let hash = token env v1 (* "#" *) in
  let _bang = token env v2 (* "!" *) in
  let _lbracket = token env v3 (* "[" *) in
  let attr = map_attribute env hash v4 in
  let _rbracket = token env v5 (* "]" *) in
  attr

and map_last_match_arm (env : env) ((v1, v2, v3, v4, v5) : CST.last_match_arm) :
    G.pattern * G.expr =
  let _outer_attrs = List_.map (map_outer_attribute_item env) v1 in
  let pattern = map_match_pattern env v2 in
  let _arrow = token env v3 (* "=>" *) in
  let expr = map_expression env v4 in
  let _comma = Option.map (fun tok -> token env tok) v5 in
  (pattern, expr)

and map_macro_invocation (env : env) ((v1, v2, v3) : CST.macro_invocation) :
    G.expr =
  let name =
    match v1 with
    (* ruin:    | `Simple_scoped_id x -> map_simple_scoped_identifier_name env x  *)
    | `Scoped_id x -> map_scoped_identifier_name env x
    | `Id tok -> H2.name_of_id (ident env tok)
    | `Choice_defa x -> H2.name_of_id (map_reserved_identifier env x)
    (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  in
  let bang = token env v2 (* "!" *) in
  let name =
    match name with
    | G.Id ((s, i1), info) ->
        G.Id ((s ^ "!", Tok.combine_toks i1 [ bang ]), info)
    | G.IdQualified ({ name_last = (s, i1), topt; _ } as qualified_info) ->
        let s, t = (s ^ "!", Tok.combine_toks i1 [ bang ]) in
        G.IdQualified { qualified_info with name_last = ((s, t), topt) }
  in
  let l, xs, r = map_delim_token_tree env v3 in
  let anys = macro_items_to_anys xs in
  let args =
    match anys with
    (* look like a regular function call, just use Arg then *)
    | [ G.E e ] -> [ G.Arg e ]
    (* coupling: see `macro_items_to_anys` above *)
    | [ G.Args args ] -> args
    | xs -> [ G.OtherArg (("ArgMacro", G.fake ""), xs) ]
  in
  G.Call (G.N name |> G.e, (l, args, r)) |> G.e

and map_match_arm (env : env) ((v1, v2, v3, v4) : CST.match_arm) :
    G.pattern * G.expr =
  let _outer_attrs = List_.map (map_outer_attribute_item env) v1 in
  let pattern = map_match_pattern env v2 in
  let _arrow = token env v3 (* "=>" *) in
  let expr =
    match v4 with
    | `Exp_COMMA (v1, v2) ->
        let expr = map_expression env v1 in
        let _comma = token env v2 (* "," *) in
        expr
    | `Choice_unsafe_blk x -> map_expression_ending_with_block env x
  in
  (pattern, expr)

and map_match_block (env : env) ((v1, v2, v3) : CST.match_block) :
    (G.pattern * G.expr) list =
  let _lbrace = token env v1 (* "{" *) in
  let actions =
    match v2 with
    | Some (v1, v2) ->
        let match_arms = List_.map (map_match_arm env) v1 in
        let match_arm_last = map_last_match_arm env v2 in
        List_.flatten [ match_arms; [ match_arm_last ] ]
    | None -> []
  in
  let _rbrace = token env v3 (* "}" *) in
  actions

and map_match_pattern (env : env) ((v1, v2) : CST.match_pattern) : G.pattern =
  let pat = map_pattern_or_expr env v1 in
  match v2 with
  | Some (v1, v2) -> (
      let _if_TODO = token env v1 (* "if" *) in
      match map_condition env v2 with
      | G.Cond expr -> G.PatWhen (pat, expr)
      | G.OtherCond (kind, anys) -> G.OtherPat (kind, G.P pat :: anys))
  | None -> pat

and map_pattern_or_expr (env : env) (x : CST.anon_choice_pat_17a3e23) =
  match x with
  | `Pat x -> map_pattern env x
  | `Clos_exp x ->
      let expr = map_closure_expression env x |> G.e in
      G.OtherPat (("PatExpr", G.fake ""), [ G.E expr ])

(* ruin:
   and map_mod_block (env : env) ((v1, v2, v3, v4) : CST.mod_block) :
       G.stmt list G.bracket =
     let lbrace = token env v1 (* "{" *) in
     let _inner_attrs = List.map (map_inner_attribute_item env) v2 in
     let stmts = List.map (map_item env) v3 |> List_.flatten in
     let rbrace = token env v4 (* "}" *) in
     (lbrace, stmts, rbrace)
*)
and map_declaration_list env (v1, v2, v3) : G.stmt list G.bracket =
  let lbrace = token env v1 (* "{" *) in
  (* TODO: Factor so that the attributes are not droped on the floor here *)
  let stmts = List.concat_map (map_declaration_statement env []) v2 in
  let rbrace = token env v3 (* "}" *) in
  (lbrace, stmts, rbrace)

and map_ordered_field (_env : env) _outer_attrsTODO
    (_attrsTODO : G.attribute list) (type_ : G.type_) (index : int) : G.field =
  let var_def = { G.vinit = None; G.vtype = Some type_; vtok = G.no_sc } in
  let ent =
    {
      G.name = G.EDynamic (G.L (G.Int (Parsed_int.of_int index)) |> G.e);
      G.attrs = [];
      G.tparams = None;
    }
  in
  G.fld (ent, G.FieldDefColon var_def)

(* for struct definition *)
and map_ordered_field_declaration_list (env : env)
    ((v1, v2, v3, v4) : CST.ordered_field_declaration_list) :
    G.field list G.bracket =
  let lparen = token env v1 (* "(" *) in
  let fields =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let outer_attrs = List_.map (map_outer_attribute_item env) v1 in
        let visibility =
          match v2 with
          | Some x -> map_visibility_modifier env x
          | None -> []
        in
        let type_first = map_type_ env v3 in
        let field_first =
          map_ordered_field env outer_attrs visibility type_first 0
        in
        let field_rest =
          List_.mapi
            (fun index (v1, v2, v3, v4) ->
              let _comma = token env v1 (* "," *) in
              let outer_attrs = List_.map (map_outer_attribute_item env) v2 in
              let visibility =
                match v3 with
                | Some x -> map_visibility_modifier env x
                | None -> []
              in
              let _type_TODO = map_type_ env v4 in
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
  let _comma = Option.map (fun tok -> token env tok) v3 in
  let rparen = token env v4 (* ")" *) in
  (lparen, fields, rparen)

(* for enum definition (OrConstructor) *)
and map_ordered_field_declaration_list_types (env : env)
    ((v1, v2, v3, v4) : CST.ordered_field_declaration_list) : G.type_ list =
  let _lparen = token env v1 (* "(" *) in
  let types =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let _outer_attrs = List_.map (map_outer_attribute_item env) v1 in
        let _visibility =
          match v2 with
          | Some x -> map_visibility_modifier env x
          | None -> []
        in
        let type_first = map_type_ env v3 in
        let type_rest =
          List_.mapi
            (fun _index (v1, v2, v3, v4) ->
              let _comma = token env v1 (* "," *) in
              let _outer_attrs = List_.map (map_outer_attribute_item env) v2 in
              let _visibility =
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
  let _comma = Option.map (fun tok -> token env tok) v3 in
  let _rparen = token env v4 (* ")" *) in
  types

(* was attribute_item before ruin *)
and map_outer_attribute_item (env : env) (v1, v2, v3, v4) : G.attribute =
  let hash = token env v1 (* "#" *) in
  let _lbracket = token env v2 (* "[" *) in
  let attr = map_attribute env hash v3 in
  let _rbracket = token env v4 (* "]" *) in
  attr

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
  let _colon = token env v3 (* ":" *) in
  let ty = map_type_ env v4 in
  match v2 with
  | `Pat x ->
      let pattern = map_pattern env x in
      let pat = G.PatTyped (pattern, ty) in
      G.ParamPattern pat
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
      G.Param param

and map_parameters (env : env) ((v1, v2, v3, v4) : CST.parameters) :
    G.parameters =
  let lparen = token env v1 (* "(" *) in
  let params =
    match v2 with
    | Some (v1, v2, v3) ->
        let outer_attr =
          Option.map (fun x -> map_outer_attribute_item env x) v1
        in
        let param_first = map_anon_choice_param_2c23cdc env outer_attr v2 in
        let param_rest =
          List_.map
            (fun (v1, v2, v3) ->
              let _comma = token env v1 (* "," *) in
              let outer_attr =
                Option.map (fun x -> map_outer_attribute_item env x) v2
              in
              map_anon_choice_param_2c23cdc env outer_attr v3)
            v3
        in
        param_first :: param_rest
    | None -> []
  in
  let _comma = Option.map (fun tok -> token env tok) v3 in
  let rparen = token env v4 (* ")" *) in
  (lparen, params, rparen)

and map_path_name (env : env) (x : CST.path) : G.name =
  match x with
  (* TODO: add Self and Super in Crate in qualifier? *)
  | `Self tok ->
      let self = ident env tok in
      (* "self" *)
      H2.name_of_id self
  | `Choice_u8 x ->
      let ident = map_primitive_type_ident env x in
      H2.name_of_id ident
  | `Meta tok ->
      let metavar = ident env tok in
      (* pattern \$[a-zA-Z_]\w* *)
      H2.name_of_id metavar
  | `Super tok ->
      let super = ident env tok in
      (* "super" *)
      H2.name_of_id super
  | `Crate tok ->
      let crate = ident env tok in
      (* "crate" *)
      H2.name_of_id crate
  | `Id tok ->
      let ident = ident env tok in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      H2.name_of_id ident
  | `Scoped_id x -> map_scoped_identifier_name env x
  | `Choice_defa x -> H2.name_of_id (map_reserved_identifier env x)

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
  | `Choice_defa x ->
      let ident = map_reserved_identifier env x in
      G.PatId (ident, G.empty_id_info ())
  | `Scoped_id x -> G.PatConstructor (map_scoped_identifier_name env x, [])
  | `Tuple_pat (v1, v2, v3, v4) ->
      let lparen = token env v1 (* "(" *) in
      let items =
        match v2 with
        | Some x -> map_tuple_pattern_or_expr_list env x
        | None -> []
      in
      let _comma = Option.map (fun tok -> token env tok) v3 in
      (* "," *)
      let rparen = token env v4 (* ")" *) in
      G.PatTuple (lparen, items, rparen)
  (* stuff like Ok(_) *)
  | `Tuple_struct_pat (v1, v2, v3, v4, v5) ->
      let name = map_tuple_struct_name env v1 in
      let _lparen = token env v2 (* "(" *) in
      let items =
        match v3 with
        | Some x -> map_tuple_pattern_list env x
        | None -> []
      in
      let _comma = Option.map (fun tok -> token env tok) v4 in
      (* "," *)
      let _rparen = token env v5 (* ")" *) in
      G.PatConstructor (name, items)
  | `Struct_pat (v1, v2, v3, v4, v5) ->
      let _nameTODO = map_struct_name env v1 in
      let lbrace = token env v2 (* "{" *) in
      let fields =
        match v3 with
        | Some (v1, v2) ->
            let field_first = map_struct_pattern_field env v1 in
            let field_rest =
              List_.map
                (fun (v1, v2) ->
                  let _comma = token env v1 (* "," *) in
                  let field = map_struct_pattern_field env v2 in
                  field)
                v2
            in
            field_first :: field_rest
        | None -> []
      in
      let _comma = Option.map (fun tok -> token env tok) v4 in
      (* "," *)
      let rbrace = token env v5 (* "}" *) in
      G.PatRecord (lbrace, fields, rbrace)
  | `Ref_pat_a3d7f54 (v1, v2) ->
      let _ref_TODO = token env v1 (* "ref" *) in
      let pattern = map_pattern env v2 in
      pattern
  | `Slice_pat (v1, v2, v3, v4) ->
      let lbracket = token env v1 (* "[" *) in
      let patterns =
        match v2 with
        | Some x -> map_tuple_pattern_list env x
        | None -> []
      in
      let _comma = Option.map (fun tok -> token env tok) v3 in
      (* "," *)
      let rbracket = token env v4 (* "]" *) in
      G.PatTuple (lbracket, patterns, rbracket)
  | `Capt_pat (v1, v2, v3) ->
      let ident = ident env v1 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let _at = token env v2 (* "@" *) in
      let pattern = map_pattern env v3 in
      G.PatAs (pattern, (ident, G.empty_id_info ()))
  | `Ref_pat_dbbcf07 (v1, v2, v3) ->
      let _and_TODO = token env v1 (* "&" *) in
      let mutability =
        Option.map
          (fun tok ->
            let tok = token env tok in
            (* "mut" *)
            G.KeywordAttr (G.Mutable, tok))
          v2
      in
      let pattern = map_pattern env v3 in
      let _attrs = deoptionalize [ mutability ] in
      pattern
  | `Rema_field_pat tok ->
      G.PatId (ident env tok, G.empty_id_info ()) (* ".." *)
  | `Mut_pat (v1, v2) ->
      let mut = token env v1 (* "mut" *) in
      let _mutability = G.KeywordAttr (G.Mutable, mut) in
      let pattern = map_pattern env v2 in
      pattern
  | `Range_pat (v1, v2, v3) ->
      let lbound = map_range_pattern_bound env v1 in
      let _opTODO =
        match v2 with
        | `DOTDOTDOT tok ->
            let _tok = token env tok in
            (* "..." *)
            G.Range
        | `DOTDOTEQ tok ->
            let _tok = token env tok in
            (* "..=" *)
            G.RangeInclusive
      in
      let rbound = map_range_pattern_bound env v3 in
      G.PatDisj (lbound, rbound)
  | `Or_pat (v1, v2, v3) ->
      let pattern_lhs = map_pattern env v1 in
      let _or_TODO = token env v2 (* "|" *) in
      let pattern_rhs = map_pattern env v3 in
      G.DisjPat (pattern_lhs, pattern_rhs)
  | `Const_blk x ->
      let block = map_const_block env x in
      G.OtherPat (("ConstBlock", G.fake ""), [ G.E block ])
  | `Macro_invo x ->
      let x = map_macro_invocation env x in
      G.OtherPat (("MacroPat", G.fake ""), [ G.E x ])
  | `X__ tok -> G.PatWildcard (token env tok)

(* "_" *)
and map_pointer_type (env : env) ((v1, v2, v3) : CST.pointer_type) : G.type_ =
  let star = token env v1 (* "*" *) in
  let _attr =
    match v2 with
    | `Const tok -> G.KeywordAttr (G.Const, token env tok) (* "const" *)
    | `Muta_spec tok -> G.KeywordAttr (G.Mutable, token env tok)
    (* "mut" *)
  in
  let type_ = map_type_ env v3 in
  G.TyPointer (star, type_) |> G.t

(* TODO svalue *)
and map_qualified_type (env : env) ((v1, v2, v3) : CST.qualified_type) : G.type_
    =
  let lhs = map_type_ env v1 in
  let as_ = token env v2 (* "as" *) in
  let rhs = map_type_ env v3 in
  G.OtherType (("As", as_), [ G.T lhs; G.T rhs ]) |> G.t

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
      G.Call (G.IdSpecial (G.Op op, tok) |> G.e, fb [ G.Arg lhs; G.Arg rhs ])
      |> G.e
  | `Exp_DOTDOT (v1, v2) ->
      let lhs = map_expression env v1 in
      let dots = token env v2 (* ".." *) in
      G.Call (G.IdSpecial (G.Op G.Range, dots) |> G.e, fb [ G.Arg lhs ]) |> G.e
  | `DOTDOT_exp x -> map_base_field_initializer env x
  | `DOTDOT tok ->
      let dots = token env tok in
      (* ".." *)
      G.Call (G.IdSpecial (G.Op G.Range, dots) |> G.e, fb []) |> G.e

and map_reference_type (env : env) ((v1, v2, v3, v4) : CST.reference_type) :
    G.type_ =
  let ref_ = token env v1 (* "&" *) in
  let _lifetime = Option.map (fun x -> map_lifetime env x) v2 in
  let mutability =
    Option.map
      (fun tok ->
        let tok = token env tok in
        (* "mut" *)
        G.KeywordAttr (G.Mutable, tok))
      v3
  in
  let type_ = map_type_ env v4 in
  (* TODO TyRef with lifetime *)
  { t = G.TyRef (ref_, type_); t_attrs = mutability |> Option.to_list }

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
  G.stmt_to_expr return_stmt

and map_scoped_identifier_name (env : env)
    ((v1, v2, v3) : CST.scoped_identifier) : G.name =
  let prefix_info =
    match v1 with
    | Some x -> (
        match x with
        | `Choice_self x -> Some (Left (map_path_name env x))
        | `Brac_type x ->
            let l, ty, r = map_bracketed_type env x in
            Some (Right (l, G.TA ty, r))
        | `Gene_type_with_turb x ->
            let n = map_generic_type_with_turbofish env x in
            Some (Left n))
    | None -> None
  in
  (* TODO: QTop *)
  let _colons = token env v2 (* "::" *) in
  let last_id =
    match v3 with
    | `Id tok
    | `Super tok ->
        ident env tok
  in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  (* TODO: use either_opt *)
  match prefix_info with
  | Some (Left name) -> H2.add_suffix_to_name last_id name
  | Some (Right (l, ty, r)) ->
      H2.add_type_args_to_name (H2.name_of_id last_id) (l, [ ty ], r)
  | None -> H2.name_of_id last_id

and map_scoped_type_identifier_name (env : env)
    ((v1, v2, v3) : CST.scoped_type_identifier) : G.name =
  (* TODO: QTop *)
  let _colons = token env v2 (* "::" *) in
  let _either_optTODO =
    match v1 with
    | Some x -> (
        match x with
        | `Choice_self x -> Some (Left (map_path_name env x))
        | `Gene_type_with_turb x ->
            Some (Left (map_generic_type_with_turbofish env x))
        | `Brac_type x ->
            let l, ty, r = map_bracketed_type env x in
            Some (Right (l, [ G.TA ty ], r))
        | `Gene_type x -> Some (Left (map_generic_type_name env x)))
    | None -> None
  in
  let ident = ident env v3 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  H2.name_of_id ident

and map_scoped_type_identifier_in_expression_position (env : env)
    ((v1, v2, v3) : CST.scoped_type_identifier_in_expression_position) : G.name
    =
  let opt =
    match v1 with
    | Some x -> (
        match x with
        | `Choice_self x -> Some (map_path_name env x)
        | `Gene_type_with_turb x -> Some (map_generic_type_with_turbofish env x)
        )
    | None -> None
  in
  let colons = token env v2 (* "::" *) in
  let ident = ident env v3 in
  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
  match opt with
  | None ->
      G.IdQualified
        {
          name_last = (ident, None);
          name_middle = None;
          name_top = Some colons;
          name_info = G.empty_id_info ();
        }
  | Some name -> H2.add_id_opt_type_args_to_name name (ident, None)

and map_statement (env : env) (attrs : G.attribute list) (x : CST.statement) :
    G.stmt list =
  match x with
  | `Exp_stmt x -> [ map_expression_statement env x ]
  | `Choice_choice_const_item x -> map_declaration_statement env attrs x

(* ruin:
   | `Item x -> map_item env x
*)

(* ruin:
   and map_trait_block (env : env) ((v1, v2, v3) : CST.trait_block) :
       G.field list G.bracket =
     let lbrace = token env v1 (* "{" *) in
     let fields = List.map (map_trait_block_item env) v2 in
     let rbrace = token env v3 (* "}" *) in
     (lbrace, fields, rbrace)

   and map_trait_block_item (env : env) ((v1, v2) : CST.trait_block_item) : G.field
       =
     let _outer_attrs = List.map (map_outer_attribute_item env) v1 in
     match v2 with
     | `Const_item x -> G.F (map_const_item env x)
     | `Func_sign_with_defa_item x ->
         let def = map_function_signature_with_default_item env x in
         G.fld def
     | `Asso_type x -> G.F (map_associated_type env x)
     | `Macro_invo x ->
         let invo = map_macro_invocation env x in
         G.F (G.ExprStmt (invo, sc) |> G.s)
*)
and map_higher_ranked_trait_bound (env : env)
    ((v1, v2, v3) : CST.higher_ranked_trait_bound) : G.type_parameters * G.type_
    =
  let _for_TODO = token env v1 (* "for" *) in
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
      let _qmark = token env v1 (* "?" *) in
      let ty = map_type_ env v2 in
      TraitBoundRemoved ty

and map_trait_bounds (env : env) ((v1, v2, v3) : CST.trait_bounds) :
    trait_bound list =
  let _colon = token env v1 (* ":" *) in
  let trait_bound_first = map_trait_bound env v2 in
  let trait_bound_rest =
    List_.map
      (fun (v1, v2) ->
        let _plus = token env v1 (* "+" *) in
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
    List_.map
      (fun (v1, v2) ->
        let _comma = token env v1 (* "," *) in
        let ty = map_type_ env v2 in
        ty)
      v3
  in
  let _comma = Option.map (fun tok -> token env tok) v4 in
  let rparen = token env v5 (* ")" *) in
  G.TyTuple (lparen, ty_first :: ty_rest, rparen) |> G.t

and map_array_type (env : env) ((v1, v2, v3, v4) : CST.array_type) =
  let lbracket = token env v1 (* "[" *) in
  let ty = map_type_ env v2 in
  let default =
    Option.map
      (fun (v1, v2) ->
        let _semicolon = token env v1 (* ";" *) in
        let expr = map_expression env v2 in
        expr)
      v3
  in
  let rbracket = token env v4 (* "]" *) in
  G.TyArray ((lbracket, default, rbracket), ty) |> G.t

and map_type_ (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Abst_type (v1, v2, v3) ->
      let _implTODO = token env v1 (* "impl" *) in
      let _v2 =
        match v2 with
        | Some (v1, v2) ->
            let _v1 = (* "for" *) token env v1 in
            let _v2 = map_type_parameters env v2 in
            (* TODO *)
            ()
        | None -> (* TODO *) ()
      in
      let trait_type = map_abstract_type_trait_name env v3 in
      trait_type
  | `Ref_type x -> map_reference_type env x
  | `Meta tok ->
      let metavar = ident env tok in
      (* pattern \$[a-zA-Z_]\w* *)
      let n = H2.name_of_id metavar in
      (* TODO: why not TyN? *)
      H2.expr_to_type (G.N n |> G.e)
  | `Poin_type x -> map_pointer_type env x
  | `Gene_type x ->
      let name = map_generic_type_name env x in
      G.TyN name |> G.t
  | `Scoped_type_id x ->
      let n = map_scoped_type_identifier_name env x in
      G.TyN n |> G.t
  | `Tuple_type x -> map_tuple_type env x
  | `Unit_type (v1, v2) ->
      let lparen = str env v1 (* "(" *) in
      let rparen = str env v2 (* ")" *) in
      let str = List_.map fst [ lparen; rparen ] |> String.concat "" in
      G.ty_builtin (str, Tok.combine_toks (snd lparen) [ snd rparen ])
  | `Array_type x -> map_array_type env x
  | `Func_type x -> map_function_type env x
  | `Id tok ->
      let ident = ident env tok in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      G.TyN (H2.name_of_id ident) |> G.t
  | `Macro_invo x ->
      let invo = map_macro_invocation env x in
      H2.expr_to_type invo
  | `Empty_type tok ->
      let bang = token env tok in
      (* "!" *)
      G.ty_builtin ("!", bang)
  | `Dyna_type (v1, v2) ->
      let _dynTODO = token env v1 (* "dyn" *) in
      let ty = map_abstract_type_trait_name env v2 in
      ty
  | `Boun_type x -> map_bounded_type env x
  | `Choice_u8 x -> map_primitive_type env x

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) :
    G.type_arguments =
  let lthan = token env v1 (* tok_LT *) in
  let typearg_first = map_type_argument env v2 in
  let typearg_rest =
    List_.map
      (fun (v1, v2) ->
        let _comma = token env v1 (* "," *) in
        let typearg = map_type_argument env v2 in
        typearg)
      v3
  in
  let _comma = Option.map (fun tok -> token env tok) v4 in
  let gthan = token env v5 (* ">" *) in
  (lthan, typearg_first :: typearg_rest, gthan)

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters)
    : G.type_parameters =
  let lt = token env v1 (* "<" *) in
  let type_param_first = map_type_parameter env v2 in
  let type_param_rest =
    List_.map
      (fun (v1, v2) ->
        let _comma = token env v1 (* "," *) in
        let type_param = map_type_parameter env v2 in
        type_param)
      v3
  in
  let _comma = Option.map (fun tok -> token env tok (* "," *)) v4 in
  let gt = token env v5 (* ">" *) in
  (lt, type_param_first :: type_param_rest, gt)

(* was map_simple_path_ident *)
and map_path_ident env x =
  let name = map_path_name env x in
  match List.rev (H2.dotted_ident_of_name name) with
  | [] -> raise Impossible
  | x :: xs -> (List.rev xs, x)

and map_use_clause (env : env) (x : CST.use_clause) use : G.directive list =
  match x with
  | `Choice_self x ->
      let dots, ident = map_path_ident env x in
      let modname = G.DottedName dots in
      [
        G.ImportFrom (use, modname, [ H2.mk_import_from_kind ident None ])
        |> G.d;
      ]
  | `Use_as_clause (v1, v2, v3) ->
      let dots, ident_ = map_path_ident env v1 in
      let modname = G.DottedName dots in
      let _as_ = token env v2 (* "as" *) in
      let alias = ident env v3 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      [
        G.ImportFrom
          (use, modname, [ H2.mk_import_from_kind ident_ (Some alias) ])
        |> G.d;
      ]
  | `Use_list x -> map_use_list env x use None
  | `Scoped_use_list (v1, v2, v3) ->
      let scope =
        Option.map (fun x -> map_path_name env x |> H2.dotted_ident_of_name) v1
      in
      let _colons = token env v2 (* "::" *) in
      map_use_list env v3 use scope
  | `Use_wild (v1, v2) ->
      let dots =
        match v1 with
        | Some (v1, v2) ->
            let path = map_path_name env v1 |> H2.dotted_ident_of_name in
            let _colons = token env v2 (* "::" *) in
            path
        | None -> []
      in
      let modname = G.DottedName dots in
      let wildcard = token env v2 (* "*" *) in
      [ G.ImportAll (use, modname, wildcard) |> G.d ]

and prepend_module_name (scope : G.dotted_ident) (modname : G.module_name) :
    G.module_name =
  match modname with
  | DottedName modname -> G.DottedName (modname @ scope)
  | _ -> modname

and prepend_scope (dir : G.directive) (scope : G.dotted_ident option) :
    G.directive =
  match scope with
  | Some scope -> (
      match dir.d with
      | ImportFrom (tok, modname, imported_names) ->
          let modname = prepend_module_name scope modname in
          { dir with d = G.ImportFrom (tok, modname, imported_names) }
      | ImportAs (tok, modname, alias) ->
          let modname = prepend_module_name scope modname in
          { dir with d = G.ImportAs (tok, modname, alias) }
      | ImportAll (tok, modname, wildcard) ->
          let modname = prepend_module_name scope modname in
          { dir with d = G.ImportAll (tok, modname, wildcard) }
      | _ -> dir)
  | None -> dir

and map_use_list (env : env) ((v1, v2, v3, v4) : CST.use_list) (use : Tok.t)
    (scope : G.dotted_ident option) : G.directive list =
  let _lbracket = token env v1 (* "{" *) in
  let directives =
    match v2 with
    | Some (v1, v2) ->
        let use_clause_first =
          match v1 with
          | `Use_clause x -> map_use_clause env x use
        in
        let use_clause_rest =
          List_.map
            (fun (v1, v2) ->
              let _comma = token env v1 (* "," *) in
              let use_clause =
                match v2 with
                | `Use_clause x -> map_use_clause env x use
              in
              use_clause)
            v2
        in
        List_.flatten (use_clause_first :: use_clause_rest)
    | None -> []
  in
  let _comma = Option.map (fun tok -> token env tok (* "," *)) v3 in
  let _rbracket = token env v4 (* "}" *) in
  List_.map (fun x -> prepend_scope x scope) directives

and map_visibility_quantifier (env : env) (v1, v2, v3) : G.attribute =
  let _lparen = token env v1 (* "(" *) in
  let attribute =
    match v2 with
    (* TODO *)
    | `Self tok -> G.KeywordAttr (G.Private, token env tok) (* "self" *)
    | `Super tok -> G.KeywordAttr (G.Private, token env tok) (* "super" *)
    | `Crate tok -> G.KeywordAttr (G.Protected, token env tok) (* "crate" *)
    | `In_choice_self (v1, v2) ->
        let in_ = token env v1 (* "in" *) in
        let path = map_path_name env v2 |> H2.dotted_ident_of_name in
        G.OtherAttribute (("In", in_), [ G.Di path ])
  in
  let _rparen = token env v3 (* ")" *) in
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
  let _whereTODO = token env v1 (* "where" *) in
  let predicate_first = map_where_predicate env v2 in
  let predicate_rest =
    List_.map
      (fun (v1, v2) ->
        let _comma = token env v1 (* "," *) in
        let predicate = map_where_predicate env v2 in
        predicate)
      v3
  in
  let _comma = Option.map (fun tok -> token env tok (* "," *)) v4 in
  predicate_first :: predicate_rest

and map_where_predicate (env : env) ((v1, v2) : CST.where_predicate) :
    where_predicate =
  let where_predicate_type =
    match v1 with
    | `Life x -> WherePredLifetime (map_lifetime env x)
    | `Id tok -> WherePredId (ident env tok)
    (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
    | `Scoped_type_id x ->
        let n = map_scoped_type_identifier_name env x in
        let t = G.TyN n |> G.t in
        WherePredType t
    | `Gene_type x ->
        let n = map_generic_type_name env x in
        let t = G.TyN n |> G.t in
        WherePredType t
    | `Ref_type x -> WherePredType (map_reference_type env x)
    | `Poin_type x -> WherePredType (map_pointer_type env x)
    | `Tuple_type x -> WherePredType (map_tuple_type env x)
    | `Array_type x -> WherePredType (map_array_type env x)
    | `Higher_ranked_trait_bound x ->
        let type_params, ty = map_higher_ranked_trait_bound env x in
        WherePredHigherRanked (type_params, ty)
    | `Choice_u8 x -> WherePredType (map_primitive_type env x)
  in
  let trait_bounds = map_trait_bounds env v2 in
  (where_predicate_type, trait_bounds)

and map_decls_or_semi env v1 =
  match v1 with
  | `SEMI tok ->
      let _semicolon = token env tok in
      (* ";" *)
      []
  | `Decl_list x ->
      let _, block, _ = map_declaration_list env x in
      block

and map_declaration_statement (env : env) (attrs : G.attribute list) x :
    G.stmt list =
  match x with
  | `Choice_const_item x -> map_declaration_statement_bis env attrs x
  | `Ellips v1 ->
      let t = token env v1 in
      [ G.Ellipsis t |> G.e |> G.exprstmt ]

(* was called map_item_kind by ruin *)
and map_declaration_statement_bis (env : env) outer_attrs (*_visibility*) x :
    G.stmt list =
  match x with
  (* was moved out of declaration_statement in source_file or item by ruin *)
  | `Inner_attr_item v1 ->
      (* TODO: Somehow map this to the containing StmtDef *)
      let _xTODO = map_inner_attribute_item env v1 in
      []
  | `Attr_item v1 ->
      (* TODO: Should be unreachable now. *)
      let _xTODO = map_outer_attribute_item env v1 in
      []
  (* was only in traits with ruin *)
  | `Asso_type v1 -> [ map_associated_type env v1 ]
  (* was moved in _statement instead of declaration_statement by ruin *)
  | `Let_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let _let_ = token env v1 (* "let" *) in
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
            let _colon = token env v1 (* ":" *) in
            let type_ = map_type_ env v2 in
            type_)
          v4
      in
      let expr =
        Option.map
          (fun (v1, v2) ->
            let _equals = token env v1 (* "=" *) in
            let expr = map_expression env v2 in
            expr)
          v5
      in
      (* TODO Include else block somehow *)
      let _else =
        match v6 with
        | Some (v1, v2) ->
            let _v1 = (* "else" *) token env v1 in
            let _v2 = map_block env v2 in
            ()
        | None -> ()
      in
      let sc = token env v7 (* ";" *) in
      let var_def = { G.vinit = expr; G.vtype = type_; vtok = Some sc } in
      let ent =
        {
          (* Patterns are difficult to convert to expressions, so wrap it *)
          G.name = G.EPattern pattern;
          G.attrs = attrs @ outer_attrs;
          G.tparams = None;
        }
      in
      [ G.DefStmt (ent, G.VarDef var_def) |> G.s ]
  | `Const_item x -> [ map_const_item env outer_attrs x ]
  | `Macro_invo x ->
      let invo = map_macro_invocation env x in
      [ G.ExprStmt (invo, sc) |> G.s ]
  | `Macro_defi (v1, v2, v3) ->
      let _macro_rulesTODO = token env v1 (* "macro_rules!" *) in
      let ident =
        match v2 with
        | `Id x -> ident env x
        | `Choice_defa x -> map_reserved_identifier env x
      in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let macro_def : rust_macro_definition =
        match v3 with
        | `LPAR_rep_macro_rule_SEMI_opt_macro_rule_RPAR_SEMI (v1, v2, v3, v4, v5)
          ->
            let lparen = token env v1 (* "(" *) in
            let rules =
              List_.map
                (fun (v1, v2) ->
                  let rule = map_macro_rule env v1 in
                  let _semicolon = token env v2 (* ";" *) in
                  rule)
                v2
            in
            let rule_last =
              match v3 with
              | Some x -> [ map_macro_rule env x ]
              | None -> []
            in
            let rparen = token env v4 (* ")" *) in
            let _semicolon = token env v5 (* ";" *) in
            (lparen, List_.flatten [ rules; rule_last ], rparen)
        | `LCURL_rep_macro_rule_SEMI_opt_macro_rule_RCURL (v1, v2, v3, v4) ->
            let lbrace = token env v1 (* "{" *) in
            let rules =
              List_.map
                (fun (v1, v2) ->
                  let rule = map_macro_rule env v1 in
                  let _semicolon = token env v2 (* ";" *) in
                  rule)
                v2
            in
            let rule_last =
              match v3 with
              | Some x -> [ map_macro_rule env x ]
              | None -> []
            in
            let rbrace = token env v4 (* "}" *) in
            (lbrace, List_.flatten [ rules; rule_last ], rbrace)
      in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs = outer_attrs;
          G.tparams = None;
        }
      in
      let macro_def = convert_macro_def macro_def in
      [ G.DefStmt (ent, G.MacroDef macro_def) |> G.s ]
  | `Empty_stmt tok ->
      let _semicolon = token env tok in
      (* ";" *)
      []
  | `Mod_item (_v0TODO, v1, v2, v3) ->
      let _mod_TODO = token env v1 (* "mod" *) in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let block = map_decls_or_semi env v3 in
      let mod_def = { G.mbody = G.ModuleStruct (Some [ ident ], block) } in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs = outer_attrs;
          G.tparams = None;
        }
      in
      [ G.DefStmt (ent, G.ModuleDef mod_def) |> G.s ]
  | `Fore_mod_item (_v0TODO, v1, v2) ->
      let _externTODO = map_extern_modifier env v1 in
      let blocks = map_decls_or_semi env v2 in
      blocks
  | `Struct_item (_v0TODO, v1, v2, v3, v4) ->
      let struct_ = token env v1 (* "struct" *) in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let type_params = Option.map (map_type_parameters env) v3 in
      let fields, _where_clauseTODO =
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
            let _semicolon = token env v3 (* ";" *) in
            (fields, where_clause)
        | `SEMI tok ->
            let _semicolon = token env tok in
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
          G.cparams = fb [];
          G.cbody = fields;
        }
      in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          attrs = outer_attrs;
          tparams = type_params;
        }
      in
      [ G.DefStmt (ent, G.ClassDef class_def) |> G.s ]
  | `Union_item (_v0TODO, v1, v2, v3, v4, v5) ->
      let _unionTODO = token env v1 (* "union" *) in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let type_params = Option.map (map_type_parameters env) v3 in
      let _where_clauseTODO =
        match v4 with
        | Some x -> map_where_clause env x
        | None -> []
      in
      let variants = map_field_declaration_list_union env v5 in
      let type_def = { G.tbody = G.OrType variants } in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs = outer_attrs;
          G.tparams = type_params;
        }
      in
      [ G.DefStmt (ent, G.TypeDef type_def) |> G.s ]
  | `Enum_item (_v0TODO, v1, v2, v3, v4, v5) ->
      let _enumTODO = token env v1 (* "enum" *) in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let type_params = Option.map (map_type_parameters env) v3 in
      let _where_clause = Option.map (fun x -> map_where_clause env x) v4 in
      let body = map_enum_variant_list env v5 in
      let type_def = { G.tbody = body } in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs = outer_attrs;
          G.tparams = type_params;
        }
      in
      [ G.DefStmt (ent, G.TypeDef type_def) |> G.s ]
  | `Type_item (_v0TODO, v1, v2, v3, v4, v5, v6, v7) ->
      let _type_TODO = token env v1 (* "type" *) in
      let ident = ident env v2 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let type_params = Option.map (map_type_parameters env) v3 in
      let _equals = token env v4 (* "=" *) in
      let _tyTODO = map_type_ env v5 in
      let _where_clauseTODO = Option.map (fun x -> map_where_clause env x) v6 in
      let _semicolon = token env v7 (* ";" *) in
      let type_def =
        { G.tbody = G.NewType (G.TyN (H2.name_of_id ident) |> G.t) }
      in
      let ent =
        {
          G.name = G.EN (H2.name_of_id ident);
          G.attrs = outer_attrs;
          G.tparams = type_params;
        }
      in
      [ G.DefStmt (ent, G.TypeDef type_def) |> G.s ]
  | `Func_item x -> [ map_function_item env outer_attrs x ]
  | `Func_sign_item (_v0TODO, v1, v2, v3, v4, v5, v6, v7, v8) ->
      let attrs =
        match v1 with
        | Some x -> map_function_modifiers env x
        | None -> []
      in
      let fn = token env v2 (* "fn" *) in
      let fn_decl = map_function_declaration env (v3, v4, v5, v6, v7) in
      let t = token env v8 (* ";" *) in
      let fn_def =
        {
          G.fparams = fn_decl.params;
          G.frettype = fn_decl.retval;
          G.fkind = (G.Function, fn);
          (* no body defined *)
          G.fbody = G.FBDecl t;
        }
      in
      let ent =
        {
          G.name = fn_decl.name;
          G.attrs = attrs @ outer_attrs;
          G.tparams = fn_decl.type_params;
        }
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
      let attrs = deoptionalize [ unsafe_attr ] @ outer_attrs in
      let impl = ident env v2 (* "impl" *) in
      let tparams = Option.map (map_type_parameters env) v3 in
      let _trait_typeTODO =
        Option.map
          (fun (v1, v2, v3) ->
            let _v1 =
              match v1 with
              | Some tok -> Some ((* "!" *) token env tok)
              | None -> None
            in
            let ty =
              match v2 with
              | `Id tok ->
                  let ident = ident env tok in
                  (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
                  G.TyN (H2.name_of_id ident) |> G.t
              | `Scoped_type_id x ->
                  let n = map_scoped_type_identifier_name env x in
                  G.TyN n |> G.t
              | `Gene_type x ->
                  let n = map_generic_type_name env x in
                  G.TyN n |> G.t
            in
            let _for_TODO = token env v3 (* "for" *) in
            ty)
          v4
      in
      let ty = map_type_ env v5 in
      let _where_clauseTODO = Option.map (fun x -> map_where_clause env x) v6 in
      let body = map_decls_or_semi env v7 in
      (* TODO not sure what to put for the name *)
      let ent = G.basic_entity ~attrs ?tparams impl in
      let def = G.OtherDef (("Impl", snd impl), [ G.T ty; G.Ss body ]) in
      [ G.DefStmt (ent, def) |> G.s ]
  | `Trait_item (_v0TODO, v1, v2, v3, v4, v5, v6, v7) ->
      let unsafe_attr =
        Option.map
          (fun tok ->
            let tok = token env tok in
            (* "unsafe" *)
            G.KeywordAttr (G.Unsafe, tok))
          v1
      in
      let attrs = deoptionalize [ unsafe_attr ] @ outer_attrs in
      let trait = token env v2 (* "trait" *) in
      let ident = ident env v3 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let _type_paramsTODO = Option.map (map_type_parameters env) v4 in
      let _trait_boundsTODO =
        match v5 with
        | Some x -> map_trait_bounds env x
        | None -> []
      in
      let _where_clauseTODO = Option.map (fun x -> map_where_clause env x) v6 in
      let l, fields, r = map_declaration_list env v7 in
      let class_def =
        {
          G.ckind = (G.Trait, trait);
          G.cextends = [];
          G.cimplements = [];
          G.cmixins = [];
          G.cparams = fb [];
          G.cbody = (l, fields |> List_.map (fun x -> G.F x), r);
        }
      in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs;
          G.tparams = None;
        }
      in
      [ G.DefStmt (ent, G.ClassDef class_def) |> G.s ]
  | `Use_decl (_v0TODO, v1, v2, v3) ->
      let use = token env v1 (* "use" *) in
      let use_clauses = map_use_clause env v2 use in
      let _semicolon = token env v3 (* ";" *) in
      List_.map (fun x -> G.DirectiveStmt x |> G.s) use_clauses
  | `Extern_crate_decl (_v0TODO, v1, v2, v3, v4, v5) ->
      let extern = token env v1 (* "extern" *) in
      let _crate = token env v2 (* "crate" *) in
      let ident_ = ident env v3 in
      (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
      let alias =
        Option.map
          (fun (v1, v2) ->
            let _as_ = token env v1 (* "as" *) in
            let alias = ident env v2 in
            (* pattern (r#)?[a-zA-Zα-ωΑ-Ωµ_][a-zA-Zα-ωΑ-Ωµ\d_]* *)
            (alias, G.empty_id_info ()))
          v4
      in
      let _semicolon = token env v5 (* ";" *) in
      let dir = G.ImportAs (extern, G.DottedName [ ident_ ], alias) |> G.d in
      [ G.DirectiveStmt dir |> G.s ]
  | `Static_item (_v0TODO, v1, v2, v3, v4, v5, v6, v7, v8) ->
      let static = token env v1 (* "static" *) in
      let _ref_ = Option.map (fun tok -> token env tok (* "ref" *)) v2 in
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
      let _colon = token env v5 (* ":" *) in
      let type_ = map_type_ env v6 in
      let init =
        Option.map
          (fun (v1, v2) ->
            let _equals = token env v1 (* "=" *) in
            let expr = map_expression env v2 in
            expr)
          v7
      in
      let sc = token env v8 (* ";" *) in
      let var_def = { G.vinit = init; G.vtype = Some type_; vtok = Some sc } in
      let ent =
        {
          G.name = G.EN (G.Id (ident, G.empty_id_info ()));
          G.attrs = attrs @ outer_attrs;
          G.tparams = None;
        }
      in
      [ G.DefStmt (ent, G.VarDef var_def) |> G.s ]

(* ruin:
   and map_item (env : env) ((v1, v2, v3) : CST.item) : G.stmt list =
     let outer_attrs = List.map (map_outer_attribute_item env) v1 in
     let visibility = Option.map (fun x -> map_visibility_modifier env x) v2 in
     map_item_kind env outer_attrs visibility v3
*)

(* This function is needed to collect outer attributes and pass them
   down into the defintion statements they apply to. TODO: In the long
   run this should achieved by modifying the parser to more accurately
   reflect the grammar *)
and map_statements_list (env : env) (x : CST.statement list) : G.stmt list =
  let accumulate_outer_attr_map_stmt
      ((outer_attrs_rev, res_stmts_rev) : G.attribute list * G.stmt list) =
    function
    | `Choice_choice_const_item (`Choice_const_item (`Attr_item attr)) ->
        (map_outer_attribute_item env attr :: outer_attrs_rev, res_stmts_rev)
    | stmt ->
        let mapped_stmts = map_statement env (List.rev outer_attrs_rev) stmt in
        (* Reversing stmts here isn't absolutely necesarry because right
           now we are only returning 0 or 1 item lists, but if someone
           doesn't know this and returns a multi-element list this code
           would be incorrect because after the fold_left is over we
           reverse the list so that it was equivalent to append_map modulo
           this version gathering up outer_attributes *)
        let mapped_stmts_rev = List.rev mapped_stmts in
        ([], mapped_stmts_rev @ res_stmts_rev)
  in
  (* TODO: Trailing outer_attrs are still being drop via this second *)
  List.fold_left accumulate_outer_attr_map_stmt ([], []) x |> snd |> List.rev

let map_source_file (env : env) (x : CST.source_file) : G.any =
  match x with
  (* ruin:
     | `Rep_inner_attr_item_rep_item (v1, v2) ->
         let _inner_attrs = List.map (map_inner_attribute_item env) v1 in
         let items = List.map (map_item env) v2 |> List_.flatten in
         G.Pr items
  *)
  | `Opt_sheb_rep_stmt (v1, v2) ->
      let _v1 =
        match v1 with
        | Some tok ->
            let _tok = (* pattern #!.* *) token env tok in
            ()
        | None -> ()
      in
      let items = map_statements_list env v2 in
      G.Pr items
  | `Semg_exp (v1, v2) ->
      let _header = token env v1 (* "__SEMGREP_EXPRESSION" *) in
      let expr = map_expression env v2 in
      G.E expr
  | `Semg_stmt (v1, v2) ->
      let _header = token env v1 (* "__SEMGREP_STATEMENT" *) in
      let stmts = map_statements_list env v2 in
      G.Ss stmts

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_rust.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Target } in
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
          Tree_sitter_rust.Parse.string stmt_str)

(* todo: special mode to convert Ellipsis in the right construct! *)
let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_expression_or_source_file str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern }
      in
      map_source_file env cst)

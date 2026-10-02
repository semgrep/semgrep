(* Corey Gagnon
 * Copyright (c) 2026 Meta Platforms, Inc. and affiliates.
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
module CST = Tree_sitter_objc.CST
module H = Parse_tree_sitter_helpers
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Objective-C parser using tree-sitter-lang/semgrep-objc and converting
 * directly to AST_generic.
 *
 * This covers the *minimal security subset* only: the constructs that
 * iOS/ObjC security rules care about (message sends, C calls, field/subscript
 * access, assignments, string/number literals, method/class definitions and
 * their bodies, ObjC block literals). Everything else falls back to
 * `G.OtherExpr`/`G.OtherStmt` so partial coverage still parses.
 *
 * The destructuring below matches the *generated* `Tree_sitter_objc.CST`
 * (positional tuples + ocaml-tree-sitter's abbreviated polymorphic-variant
 * constructor names), NOT node-types.json. The `expression` supertype is
 * two-level (`expression` -> `expression_not_binary` -> an inner
 * `Choice_cond_exp` choice); message sends are the inline `Mess_exp` variant
 * with a keyword-selector list and have no separate CST node.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type context = Program | Pattern
type env = context H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket
let fake_dot () = G.fake "."
let fake_sc () = G.fake ";"

(* Generic "we didn't model this node" fallbacks. Keeping partial coverage
 * parseable is more valuable than failing on unhandled constructs. *)
let todo_expr (_env : env) (tok : Tok.t) : G.expr =
  G.OtherExpr (("ObjcTodo", tok), []) |> G.e

let todo_stmt (_env : env) (tok : Tok.t) : G.stmt =
  G.OtherStmt (G.OS_Todo, [ G.Tk tok ]) |> G.s

let id_expr (id : G.ident) : G.expr = G.N (H2.name_of_id id) |> G.e

(*****************************************************************************)
(* Expressions / Statements / Declarations (one recursive nest) *)
(*****************************************************************************)

let rec map_expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Choice_choice_choice_cond_exp (`Choice_choice_cond_exp x) ->
      map_expression_not_binary env x
  | `Choice_choice_choice_cond_exp (`Bin_exp x) -> map_binary_expression env x
  | `Semg_ellips t -> G.Ellipsis (token env t) |> G.e
  | `Deep_ellips (l, e, r) ->
      G.DeepEllipsis (token env l, map_expression env e, token env r) |> G.e
  | `Semg_named_ellips t -> id_expr (str env t)

and map_expression_not_binary (env : env) (x : CST.expression_not_binary) :
    G.expr =
  match x with
  | `Choice_cond_exp x -> (
      match x with
      | `Cond_exp x -> map_conditional_expression env x
      | `Assign_exp x -> map_assignment_expression env x
      | `Un_exp x -> map_unary_expression env x
      | `Poin_exp x -> map_pointer_expression env x
      | `Subs_exp x -> map_subscript_expression env x
      | `Call_exp x -> map_call_expression env x
      | `Field_exp x -> map_field_expression env x
      | `Id t -> id_expr (str env t)
      | `Num_lit t -> map_number_literal env t
      | `Str x -> map_string_ env x
      | `True t -> G.L (G.Bool (true, token env t)) |> G.e
      | `False t -> G.L (G.Bool (false, token env t)) |> G.e
      | `Null n -> (
          match n with
          | `NULL t
          | `Null t ->
              G.L (G.Null (token env t)) |> G.e)
      | `Paren_exp x -> map_parenthesized_expression env x
      | `Char_lit (open_, parts, _r) ->
          let ltok =
            match open_ with
            | `LSQUOT t
            | `USQUOT_d861d39 t
            | `USQUOT_2701bdc t
            | `U8SQUOT t
            | `SQUOT t ->
                token env t
          in
          let s =
            String.concat ""
              (List.map
                 (fun p ->
                   match p with
                   | `Esc_seq t -> fst (str env t)
                   | `Imm_tok_pat_36637e2 t -> fst (str env t))
                 parts)
          in
          G.L (G.Char (s, ltok)) |> G.e
      | `Cast_exp x -> (
          match x with
          | `LPAR_choice_type_desc_RPAR_exp (l, _ty, _r, e) ->
              (* C-style cast: keep a real token + the inner expr so metavar
                 bindings on casted arguments have positions and carry
                 taint. *)
              G.OtherExpr
                (("ObjcCast", token env l), [ G.E (map_expression env e) ])
              |> G.e
          | `Choice___real_exp (kw, e) ->
              let t =
                match kw with
                | `X___real t
                | `X___imag t ->
                    token env t
              in
              G.OtherExpr (("ObjcCast", t), [ G.E (map_expression env e) ])
              |> G.e)
      | `Update_exp u -> (
          match u with
          | `Choice_DASHDASH_exp (op, e)
          | `Exp_choice_DASHDASH (e, op) -> (
              let e = map_expression env e in
              match op with
              | `DASHDASH t
              | `PLUSPLUS t ->
                  G.OtherExpr (("ObjcIncrDecr", token env t), [ G.E e ]) |> G.e)
          )
      | `Sizeof_exp (tsizeof, arg) -> (
          match arg with
          | `Exp e ->
              G.OtherExpr
                ( ("ObjcSizeof", token env tsizeof),
                  [ G.E (map_expression env e) ] )
              |> G.e
          | `LPAR_type_desc_RPAR _ -> todo_expr env (token env tsizeof))
      (* Enumerated (not wildcarded) so a grammar regeneration that adds
         variants fails to compile instead of silently degrading. *)
      | `Alig_exp _
      | `Offs_exp _
      | `Gene_exp _
      | `Comp_lit_exp _
      | `Gnu_asm_exp _
      | `Exte_exp _ ->
          todo_expr env (fake_dot ()))
  | `Mess_exp x -> map_message_expression env x
  | `At_exp (_at, e) -> map_expression env e
  | `Blk_lit (hat, _, _, _, _, _, body) ->
      (* ObjC block `^{...}` -> lambda, so the body stays analyzable. *)
      let fbody = G.FBStmt (map_compound_statement env body) in
      let fdef =
        {
          G.fkind = (G.LambdaKind, token env hat);
          fparams = fb [];
          frettype = None;
          fbody;
        }
      in
      G.Lambda fdef |> G.e
  | `Choice_id x -> map_keyword_identifier env x
  | `Sele_exp (t, _, _, _) -> todo_expr env (token env t)
  | `Range_exp (e1, t, e2) ->
      G.OtherExpr
        ( ("ObjcRange", token env t),
          [ G.E (map_expression env e1); G.E (map_expression env e2) ] )
      |> G.e
  | `Array_lit (_at, l, items_opt, r) ->
      let elems =
        match items_opt with
        | Some (e0, rest, _trailing_comma) ->
            map_expression env e0
            :: List.map (fun (_comma, e) -> map_expression env e) rest
        | None -> []
      in
      G.Container (G.List, (token env l, elems, token env r)) |> G.e
  | `Dict_lit (_at, l, pairs_opt, r) ->
      let pair ((k, tcolon, v) : CST.dictionary_pair) =
        G.keyval (map_expression env k) (token env tcolon)
          (map_expression env v)
      in
      let elems =
        match pairs_opt with
        | Some (p0, rest, _trailing_comma) ->
            pair p0 :: List.map (fun (_comma, p) -> pair p) rest
        | None -> []
      in
      G.Container (G.Dict, (token env l, elems, token env r)) |> G.e
  | `Avai_exp (kw, _, _, _, _) ->
      let t =
        match kw with
        | `ATav t
        | `X___buil_avai t ->
            token env t
      in
      todo_expr env t
  | `Encode_exp (t, _, _, _) -> todo_expr env (token env t)
  | `Va_arg_exp (t, _, _, _, _, _) -> todo_expr env (token env t)

(* THE important one for security rules.
 * `[receiver a:x b:y]` is modeled as a `G.Call` where the function is
 * `receiver.<method>` (DotAccess). The method name is the FIRST keyword
 * segment only, colon included when it takes arguments ("a:"); subsequent
 * keyword segments become keyword arguments (`G.ArgKwd (b, y)`). A `...`
 * in keyword position becomes a positional `G.Arg (G.Ellipsis)`.
 *
 * Rationale (vs. joining the whole selector into one identifier): the
 * joined name ("a:b:") never appears contiguously in target source, which
 * both defeats rule prefiltering and makes "this selector plus any further
 * keyword args" ([recv a:$X ...]) inexpressible, since ellipsis cannot
 * match inside an identifier. First-segment naming keeps exact-selector
 * precision (a pattern without ellipsis only matches calls with exactly
 * the keywords it spells out, because argument lists must match) while
 * letting `...` work with standard argument-list semantics. This mirrors
 * how the Swift frontend models labeled arguments. *)
and map_message_expression (env : env) (x : CST.message_expression) : G.expr =
  let l, recv, items, r = x in
  let receiver =
    match recv with
    | `Exp e -> map_expression env e
    | `Gene_spec (id, _) -> id_expr (str env id)
  in
  (* One keyword's colon-groups, canonicalized into tagged values.
     `sel:a, b :c` -> [Val a; Val b; Val c] (multiple colon-groups on one
     identifier are ObjC's empty-keyword `sel:a :c` form).
     Canonicalization: in `sel:$X ... kw:$Y` the C range_expression
     greedily swallows `... kw`, parsing group 1 as Range($X, kw) and
     group 2 as $Y. A group whose expression is Range(e, <bare ident>)
     followed by another colon-group therefore really means: e, a
     keyword-position `...`, then kw:<next group>. *)
  let bare_ident (e : CST.expression) =
    match e with
    | `Choice_choice_choice_cond_exp
        (`Choice_choice_cond_exp (`Choice_cond_exp (`Id t))) ->
        Some (str env t)
    | _ -> None
  in
  (* Range(e, <bare ident>) followed by another colon-group is really
     `e`, a keyword-position `...`, then kw:<next group> (see above). *)
  let range_split (e : CST.expression) =
    match e with
    | `Choice_choice_choice_cond_exp
        (`Choice_choice_cond_exp (`Range_exp (e1, tdots, e2))) ->
        Option.map (fun kw -> (e1, tdots, kw)) (bare_ident e2)
    | _ -> None
  in
  let vals_of_extra extra =
    List.map (fun (_comma, x) -> `Val (map_expression env x)) extra
  in
  let rec group_vals groups =
    match groups with
    | (_colon, e, extra) :: ((_colon2, v, extra2) :: rest2 as rest) -> (
        match range_split e with
        | Some (e1, tdots, kw) ->
            (`Val (map_expression env e1) :: vals_of_extra extra)
            @ [ `Ellip (token env tdots); `Kw (kw, map_expression env v) ]
            @ vals_of_extra extra2 @ group_vals rest2
        | None ->
            `Val (map_expression env e)
            :: (vals_of_extra extra @ group_vals rest))
    | [ (_colon, e, extra) ] ->
        `Val (map_expression env e) :: vals_of_extra extra
    | [] -> []
  in
  (* Later keyword segments become an ordered pair of positional
     arguments — a keyword marker (an identifier like "options:", which can
     never collide with a real ObjC identifier since ':' is illegal in one)
     followed by the value. G.ArgKwd is deliberately NOT used: the generic
     matcher treats keyword arguments as unordered, but in Objective-C the
     keyword order IS the selector, so `openURL:options:completionHandler:`
     must not match `openURL:completionHandler:options:`. Positional
     lowering keeps matching order-sensitive; `...` still absorbs whole
     marker+value runs. *)
  let kw_marker (kw : G.ident) = G.Arg (id_expr (fst kw ^ ":", snd kw)) in
  let args_of_val v =
    match v with
    | `Val e -> [ G.Arg e ]
    | `Ellip t -> [ G.Arg (G.Ellipsis t |> G.e) ]
    | `Kw (kw, e) -> [ kw_marker kw; G.Arg e ]
  in
  let name, name_tok, first_args, rest =
    match items with
    | `Id_rep_COLON_exp_rep_COMMA_exp (kw_ident, groups) :: rest ->
        let name, tok = str env kw_ident in
        let name =
          match groups with
          | [] -> name
          | _ -> name ^ ":"
        in
        (name, tok, List.concat_map args_of_val (group_vals groups), rest)
    | `Semg_ellips tok :: rest ->
        (* `[recv ...]` — no selector to name; degrade to a `...` method
           name so `[$RECV ...]`-style inventory patterns still parse.
           Matching support for this form is not claimed. *)
        ("...", token env tok, [], rest)
    | [] -> ("", token env l, [], [])
  in
  let rest_args =
    List.concat_map
      (fun item ->
        match item with
        | `Id_rep_COLON_exp_rep_COMMA_exp (kw_ident, groups) -> (
            let kw = str env kw_ident in
            match group_vals groups with
            | [] ->
                (* bare trailing identifier: `[obj foo bar]` *)
                [ G.Arg (id_expr kw) ]
            | vals -> kw_marker kw :: List.concat_map args_of_val vals)
        | `Semg_ellips tok -> [ G.Arg (G.Ellipsis (token env tok) |> G.e) ])
      rest
  in
  let args = first_args @ rest_args in
  let fn =
    G.DotAccess (receiver, fake_dot (), G.FN (H2.name_of_id (name, name_tok)))
    |> G.e
  in
  G.Call (fn, (token env l, args, token env r)) |> G.e

and map_call_expression (env : env) (x : CST.call_expression) : G.expr =
  let fn, args = x in
  G.Call (map_expression env fn, map_argument_list env args) |> G.e

and map_argument_list (env : env) (x : CST.argument_list) : G.arguments =
  let l, mid, r = x in
  let args =
    match mid with
    | `Opt_choice_opt_type_qual_choice_exp_rep_COMMA_choice_opt_type_qual_choice_exp
        opt -> (
        match opt with
        | None -> []
        | Some (first, rest) ->
            (* Unmodeled argument forms become placeholder args rather than
               disappearing, so call arity is preserved. *)
            let one a =
              match a with
              | `Opt_type_qual_choice_exp (_q, `Exp e) ->
                  G.Arg (map_expression env e)
              | `Opt_type_qual_choice_exp (_, `Typeof_spec _)
              | `Comp_stmt _ ->
                  G.Arg (todo_expr env (token env l))
            in
            one first :: List.map (fun (_comma, a) -> one a) rest)
    | `Id_imm_tok_lt_type_name_rep_COMMA_type_name_GT _ -> []
    | `Objc_bridge _ -> []
    | `Avai _ -> []
  in
  (token env l, args, token env r)

and map_field_expression (env : env) (x : CST.field_expression) : G.expr =
  let e, op, field = x in
  let e = map_expression env e in
  let optok =
    match op with
    | `DOT t -> token env t
    | `DASHGT t -> token env t
  in
  match field with
  | `Id id -> G.DotAccess (e, optok, G.FN (H2.name_of_id (str env id))) |> G.e
  | `Semg_ellips t ->
      G.DotAccess (e, optok, G.FDynamic (G.Ellipsis (token env t) |> G.e))
      |> G.e

and map_subscript_expression (env : env) (x : CST.subscript_expression) : G.expr
    =
  let e, l, idx, r = x in
  G.ArrayAccess
    (map_expression env e, (token env l, map_expression env idx, token env r))
  |> G.e

and map_assignment_left_expression (env : env)
    (x : CST.assignment_left_expression) : G.expr =
  match x with
  | `Id id -> id_expr (str env id)
  | `Call_exp x -> map_call_expression env x
  | `Field_exp x -> map_field_expression env x
  | `Poin_exp x -> map_pointer_expression env x
  | `Subs_exp x -> map_subscript_expression env x
  | `Paren_exp x -> map_parenthesized_expression env x

and map_assignment_expression (env : env) (x : CST.assignment_expression) :
    G.expr =
  let lhs, op, rhs = x in
  let lhs = map_assignment_left_expression env lhs in
  let rhs = map_expression env rhs in
  let aop g t = G.AssignOp (lhs, (g, token env t), rhs) |> G.e in
  match op with
  | `EQ t -> G.Assign (lhs, token env t, rhs) |> G.e
  | `STAREQ t -> aop G.Mult t
  | `SLASHEQ t -> aop G.Div t
  | `PERCEQ t -> aop G.Mod t
  | `PLUSEQ t -> aop G.Plus t
  | `DASHEQ t -> aop G.Minus t
  | `LTLTEQ t -> aop G.LSL t
  | `GTGTEQ t -> aop G.ASR t
  | `AMPEQ t -> aop G.BitAnd t
  | `HATEQ t -> aop G.BitXor t
  | `BAREQ t -> aop G.BitOr t

and map_binary_expression (env : env) (x : CST.binary_expression) : G.expr =
  let mk a t g b =
    G.opcall (g, token env t) [ map_expression env a; map_expression env b ]
  in
  match x with
  | `Exp_PLUS_exp (a, t, b) -> mk a t G.Plus b
  | `Exp_DASH_exp (a, t, b) -> mk a t G.Minus b
  | `Exp_STAR_exp (a, t, b) -> mk a t G.Mult b
  | `Exp_SLASH_exp (a, t, b) -> mk a t G.Div b
  | `Exp_PERC_exp (a, t, b) -> mk a t G.Mod b
  | `Exp_BARBAR_exp (a, t, b) -> mk a t G.Or b
  | `Exp_AMPAMP_exp (a, t, b) -> mk a t G.And b
  | `Exp_BAR_exp (a, t, b) -> mk a t G.BitOr b
  | `Exp_HAT_exp (a, t, b) -> mk a t G.BitXor b
  | `Exp_AMP_exp (a, t, b) -> mk a t G.BitAnd b
  | `Exp_EQEQ_exp (a, t, b) -> mk a t G.Eq b
  | `Exp_BANGEQ_exp (a, t, b) -> mk a t G.NotEq b
  | `Exp_GT_exp (a, t, b) -> mk a t G.Gt b
  | `Exp_GTEQ_exp (a, t, b) -> mk a t G.GtE b
  | `Exp_LTEQ_exp (a, t, b) -> mk a t G.LtE b
  | `Exp_LT_exp (a, t, b) -> mk a t G.Lt b
  | `Exp_LTLT_exp (a, t, b) -> mk a t G.LSL b
  | `Exp_GTGT_exp (a, t, b) -> mk a t G.ASR b

and map_unary_expression (env : env) (x : CST.unary_expression) : G.expr =
  let op, e = x in
  let e = map_expression env e in
  match op with
  | `BANG t -> G.opcall (G.Not, token env t) [ e ]
  | `TILDE t -> G.opcall (G.BitNot, token env t) [ e ]
  | `DASH t -> G.opcall (G.Minus, token env t) [ e ]
  | `PLUS t -> G.opcall (G.Plus, token env t) [ e ]

and map_pointer_expression (env : env) (x : CST.pointer_expression) : G.expr =
  let op, e = x in
  let e = map_expression env e in
  match op with
  | `STAR t -> G.DeRef (token env t, e) |> G.e
  | `AMP t -> G.Ref (token env t, e) |> G.e

and map_conditional_expression (env : env) (x : CST.conditional_expression) :
    G.expr =
  let cond, _q, then_opt, _c, else_ = x in
  let cond = map_expression env cond in
  let else_ = map_expression env else_ in
  let then_ =
    match then_opt with
    | Some x -> map_anon_choice_exp_55b4dba env x
    | None -> cond
  in
  G.Conditional (cond, then_, else_) |> G.e

and map_parenthesized_expression (env : env) (x : CST.parenthesized_expression)
    : G.expr =
  match x with
  | `LPAR_choice_exp_RPAR (_l, inner, _r) -> (
      match inner with
      | `Exp e -> map_expression env e
      | `Comma_exp x -> map_comma_expression env x
      | `Comp_stmt (_arp, l, _items, _r) -> todo_expr env (token env l))
  (* `(int $X)` currently degrades to a bare metavariable: this converter
     does not yet produce `G.type_` on the target side (casts and
     declarations are lowered without types), so a real `G.TypedMetavar`
     could never match anything. Wire both sides together when types
     land. *)
  | `LPAR_semg_typed_meta_RPAR (_l, (_td, mv), _r) -> id_expr (str env mv)

and map_anon_choice_exp_55b4dba (env : env) (x : CST.anon_choice_exp_55b4dba) :
    G.expr =
  match x with
  | `Exp e -> map_expression env e
  | `Comma_exp x -> map_comma_expression env x

and map_comma_expression (env : env) (x : CST.comma_expression) : G.expr =
  let e1, t, rest = x in
  let e1 = map_expression env e1 in
  let e2 = map_anon_choice_exp_55b4dba env rest in
  G.OtherExpr (("ObjcComma", token env t), [ G.E e1; G.E e2 ]) |> G.e

and map_keyword_identifier (env : env) (x : CST.keyword_identifier) : G.expr =
  match x with
  | `Id t
  | `In t
  | `Struct t
  | `Const t ->
      id_expr (str env t)

and map_string_ (env : env) (x : CST.string_) : G.expr =
  match x with
  | `Str_lit x -> map_string_literal env x
  | `Conc_str (hd, _rest) ->
      (* Adjacent-literal concatenation; not modeled as a single string, but
         anchored on a real token so match ranges stay meaningful. *)
      let anchor =
        match hd with
        | `Id_str_lit (id, _) -> token env id
        | `Str_lit_str_lit ((open_, _, _), _)
        | `Str_lit_id ((open_, _, _), _) -> (
            match open_ with
            | `AT_DQUOT (at, _dq) -> token env at
            | `LDQUOT t
            | `UDQUOT_c163aae t
            | `UDQUOT_df3447d t
            | `U8DQUOT t
            | `DQUOT t ->
                token env t)
      in
      todo_expr env anchor

and map_string_literal (env : env) (x : CST.string_literal) : G.expr =
  let open_, parts, r = x in
  let ltok =
    match open_ with
    | `AT_DQUOT (at, _dq) -> token env at
    | `LDQUOT t
    | `UDQUOT_c163aae t
    | `UDQUOT_df3447d t
    | `U8DQUOT t
    | `DQUOT t ->
        token env t
  in
  let strs =
    List.map
      (fun part ->
        match part with
        | `Imm_tok_prec_p1_pat_c7f65b4 t -> str env t
        | `Esc_seq t -> str env t)
      parts
  in
  let s = String.concat "" (List.map fst strs) in
  let content_tok =
    match strs with
    | (_, t) :: _ -> t
    | [] -> ltok
  in
  G.L (G.String (ltok, (s, content_tok), token env r)) |> G.e

and map_number_literal (env : env) (tok : CST.number_literal) : G.expr =
  let s, t = str env tok in
  let is_hex =
    String.length s >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X')
  in
  if
    (not is_hex)
    && (String.contains s '.' || String.contains s 'e' || String.contains s 'E')
  then G.L (G.Float (float_of_string_opt s, t)) |> G.e
  else G.L (G.Int (Parsed_int.parse (s, t))) |> G.e

and map_statement (env : env) (x : CST.statement) : G.stmt =
  match x with
  | `Case_stmt x -> map_case_statement env x
  | `Choice_attr_stmt x -> map_non_case_statement env x

(* The case label itself is not modeled (no Switch node is produced, see
   map_non_case_statement); the body statements must stay analyzable. *)
and map_case_statement (env : env)
    ((label, _tcolon, items) : CST.case_statement) : G.stmt =
  let tok0 =
    match label with
    | `Case_exp (t, _) -> token env t
    | `Defa t -> token env t
  in
  let stmts =
    List.concat_map
      (fun item ->
        match item with
        | `Choice_attr_stmt st -> [ map_non_case_statement env st ]
        | `Decl d -> [ map_declaration env d ]
        | `Type_defi _ -> [])
      items
  in
  G.OtherStmtWithStmt (G.OSWS_Todo, [ G.Tk tok0 ], G.Block (fb stmts) |> G.s)
  |> G.s

and map_non_case_statement (env : env) (x : CST.non_case_statement) : G.stmt =
  match x with
  | `Comp_stmt x -> map_compound_statement env x
  | `Exp_stmt x -> map_expression_statement env x
  | `Ret_stmt x -> map_return_statement env x
  | `Attr_stmt (_attrs, st) -> map_statement env st
  | `If_stmt (tif, cond, then_, else_opt) ->
      G.If
        ( token env tif,
          G.Cond (map_parenthesized_expression env cond),
          map_statement env then_,
          Option.map (fun (_telse, s) -> map_statement env s) else_opt )
      |> G.s
  | `While_stmt (twhile, cond, body) ->
      G.While
        ( token env twhile,
          G.Cond (map_parenthesized_expression env cond),
          map_statement env body )
      |> G.s
  | `Do_stmt (tdo, body, _twhile, cond, _sc) ->
      G.DoWhile
        ( token env tdo,
          map_statement env body,
          map_parenthesized_expression env cond )
      |> G.s
  | `For_stmt x -> map_for_statement env x
  | `Switch_stmt (tswitch, _cond, body) ->
      (* No Switch node yet: case labels are not modeled (see
         map_case_statement), but the body statements stay analyzable. *)
      G.OtherStmtWithStmt
        ( G.OSWS_Todo,
          [ G.Tk (token env tswitch) ],
          map_compound_statement env body )
      |> G.s
  | `Labe_stmt (id, _tcolon, body) ->
      let st =
        match body with
        | `Decl d -> map_declaration env d
        | `Stmt s -> map_statement env s
      in
      G.Label (str env id, st) |> G.s
  | `Brk_stmt (tbreak, sc) ->
      G.Break (token env tbreak, G.LNone, token env sc) |> G.s
  | `Cont_stmt (tcont, sc) ->
      G.Continue (token env tcont, G.LNone, token env sc) |> G.s
  | `Goto_stmt (tgoto, label, sc) ->
      G.Goto (token env tgoto, str env label, token env sc) |> G.s
  | `Try_stmt (ttry, body, handlers) ->
      let ttry =
        match ttry with
        | `ATtry t
        | `X___try t ->
            token env t
      in
      let body = map_compound_statement env body in
      let catches, finally_opt =
        match handlers with
        | `Rep1_catch_clause_opt_fina_clause (clauses, fin_opt) ->
            ( List.map
                (fun ((_kw, _param, cbody) : CST.catch_clause) ->
                  ( ttry,
                    G.CatchPattern (G.PatWildcard ttry),
                    map_compound_statement env cbody ))
                clauses,
              fin_opt )
        | `Fina_clause fin -> ([], Some fin)
      in
      let finally =
        Option.map
          (fun ((kw, fbody) : CST.finally_clause) ->
            let tfin =
              match kw with
              | `ATfi t
              | `X___fina t ->
                  token env t
            in
            (tfin, map_compound_statement env fbody))
          finally_opt
      in
      G.Try (ttry, body, catches, None, finally) |> G.s
  | `Throw_stmt (tthrow, eopt, sc) -> (
      match eopt with
      | Some e ->
          G.Throw (token env tthrow, map_expression env e, token env sc) |> G.s
      (* bare `@throw;` rethrows the in-flight exception *)
      | None -> G.OtherStmt (G.OS_Todo, [ G.Tk (token env tthrow) ]) |> G.s)
  | `Sync_stmt (tsync, _l, _e, _rest, _r, body) ->
      G.OtherStmtWithStmt
        ( G.OSWS_Todo,
          [ G.Tk (token env tsync) ],
          map_compound_statement env body )
      |> G.s
  | `Ms_asm_blk (tasm, _l, _pat, _r) ->
      G.OtherStmt (G.OS_Todo, [ G.Tk (token env tasm) ]) |> G.s

and map_for_statement (env : env) (x : CST.for_statement) : G.stmt =
  match x with
  | `For_LPAR_for_stmt_body_RPAR_stmt (tfor, _l, forbody, _r, body) ->
      let init, cond_opt, _sc, update_opt = forbody in
      (* Initializers surface as ForInitExpr so taint sees their
         assignments; declaration forms reuse the Assign lowering. *)
      let init =
        match init with
        | `Opt_choice_exp_SEMI (Some e, _) ->
            [ G.ForInitExpr (map_anon_choice_exp_55b4dba env e) ]
        | `Opt_choice_exp_SEMI (None, _) -> []
        | `Decl d -> (
            match (map_declaration env d).G.s with
            | G.ExprStmt (e, _) -> [ G.ForInitExpr e ]
            | _ -> [])
      in
      let cond = Option.map (map_anon_choice_exp_55b4dba env) cond_opt in
      let update = Option.map (map_anon_choice_exp_55b4dba env) update_opt in
      G.For
        ( token env tfor,
          G.ForClassic (init, cond, update),
          map_statement env body )
      |> G.s
  | `For_LPAR_choice_decl_specis_decl_in_exp_RPAR_choice_attr_stmt
      (tfor, _l, lhs, tin, e, _r, body) ->
      let pat =
        match lhs with
        | `Id id -> G.PatId (str env id, G.empty_id_info ())
        | `Decl_specis_decl (_specs, declr) -> (
            match name_of_declarator env declr with
            | Some id -> G.PatId (id, G.empty_id_info ())
            | None -> G.PatWildcard (token env tin))
      in
      G.For
        ( token env tfor,
          G.ForEach (pat, token env tin, map_expression env e),
          map_non_case_statement env body )
      |> G.s

and map_expression_statement (env : env) (x : CST.expression_statement) : G.stmt
    =
  let eopt, sc = x in
  let sc = token env sc in
  match eopt with
  | Some e -> G.ExprStmt (map_anon_choice_exp_55b4dba env e, sc) |> G.s
  | None -> G.Block (fb []) |> G.s

and map_return_statement (env : env) (x : CST.return_statement) : G.stmt =
  let tret, eopt, sc = x in
  let eopt = Option.map (map_anon_choice_exp_55b4dba env) eopt in
  G.Return (token env tret, eopt, token env sc) |> G.s

and map_compound_statement (env : env) (x : CST.compound_statement) : G.stmt =
  let _arp, l, items, r = x in
  let stmts = List.concat_map (map_block_item env) items in
  G.Block (token env l, stmts, token env r) |> G.s

and map_block_item (env : env) (x : CST.block_item) : G.stmt list =
  match x with
  | `Semg_ellips t ->
      [ G.ExprStmt (G.Ellipsis (token env t) |> G.e, fake_sc ()) |> G.s ]
  | `Choice_choice_func_defi x -> (
      match x with
      | `Choice_func_defi x -> (
          match x with
          | `Func_defi x -> [ map_function_definition env x ]
          | `Decl x -> [ map_declaration env x ]
          | `Stmt x -> [ map_statement env x ]
          | `Prep_if x -> map_preproc_if env x
          | `Prep_ifdef x -> map_preproc_ifdef env x
          | _ -> [ todo_stmt env (fake_sc ()) ])
      | `Class_inte x -> [ map_class_interface env x ]
      | `Class_impl x -> [ map_class_implementation env x ]
      | _ -> [ todo_stmt env (fake_sc ()) ])

(* Preprocessor conditionals: every branch is flattened into the statement
   stream, ignoring the condition — a rule should match code in any
   `#if`/`#elif`/`#else` arm, since any of them may be compiled. *)
and map_anon_choice_blk_item (env : env) (x : CST.anon_choice_blk_item_e6161e0)
    : G.stmt list =
  match x with
  | `Blk_item bi -> map_block_item env bi
  | `Attr_spec _
  | `Prop_impl _ ->
      []

and map_preproc_alternative (env : env) (x : CST.anon_choice_prep_else_8b52b0f)
    : G.stmt list =
  match x with
  | `Prep_else (_p, items) ->
      List.concat_map (map_anon_choice_blk_item env) items
  | `Prep_elif (_p, _cond, _nl, items, rest) -> (
      List.concat_map (map_anon_choice_blk_item env) items
      @
      match rest with
      | Some r -> map_preproc_alternative env r
      | None -> [])

and map_preproc_if (env : env)
    ((_p, _cond, _nl, items, else_opt, _endif) : CST.preproc_if) : G.stmt list =
  List.concat_map (map_anon_choice_blk_item env) items
  @
  match else_opt with
  | Some r -> map_preproc_alternative env r
  | None -> []

and map_preproc_ifdef (env : env)
    ((_kw, _id, items, else_opt, _endif) : CST.preproc_ifdef) : G.stmt list =
  List.concat_map (map_anon_choice_blk_item env) items
  @
  match else_opt with
  | Some (`Choice_prep_else r) -> map_preproc_alternative env r
  | Some (`Prep_elif (_kw2, _id2, items2, rest)) -> (
      List.concat_map (map_anon_choice_blk_item env) items2
      @
      match rest with
      | Some r -> map_preproc_alternative env r
      | None -> [])
  | None -> []

and map_declaration (env : env) (x : CST.declaration) : G.stmt =
  let _specs, first, rest, _mods, sc = x in
  let sctok = token env sc in
  (* Model `T x = e;` as an assignment so taint flows from initializer to the
   * variable, without depending on the exact VarDef record shape. *)
  let one d =
    match d with
    | `Init_decl (declr, _attr, eqtok, `Exp e) -> (
        let rhs = map_expression env e in
        match name_of_declarator env declr with
        | Some id ->
            Some
              (G.ExprStmt
                 (G.Assign (id_expr id, token env eqtok, rhs) |> G.e, sctok)
              |> G.s)
        | None -> Some (G.ExprStmt (rhs, sctok) |> G.s))
    | `Init_decl (_declr, _attr, _eqtok, `Init_list _) -> None
    | `Decl_opt_gnu_asm_exp _ -> None
    | `Type_qual_id _ -> None
  in
  let stmts = List.filter_map one (first :: List.map snd rest) in
  match stmts with
  | [] -> todo_stmt env sctok
  | [ s ] -> s
  | ss -> G.Block (fb ss) |> G.s

and name_of_declarator (env : env) (d : CST.declarator) : G.ident option =
  match d with
  | `Id id -> Some (str env id)
  | `Poin_decl (_, _, _, _, d) -> name_of_declarator env d
  | `Func_decl (d, _, _, _) -> name_of_declarator env d
  | `Array_decl (d, _, _, _, _) -> name_of_declarator env d
  | `Paren_decl (_, _, d, _) -> name_of_declarator env d
  | `Blk_poin_decl (_, _, d) -> name_of_declarator env d

and map_function_definition (env : env) (x : CST.function_definition) : G.stmt =
  let _ms, _specs, declr, body = x in
  let name =
    match name_of_declarator env declr with
    | Some id -> id
    | None -> ("func", G.fake "func")
  in
  let ent = G.basic_entity name in
  let fbody = G.FBStmt (map_compound_statement env body) in
  let fdef =
    {
      G.fkind = (G.Function, snd name);
      fparams = fb [];
      frettype = None;
      fbody;
    }
  in
  G.DefStmt (ent, G.FuncDef fdef) |> G.s

and map_method_type (_env : env) (_x : CST.method_type) : G.type_ =
  G.OtherType (("ObjcMethodType", fake_dot ()), []) |> G.t

(* First keyword segment only ("greet:"), matching the naming used for
   message sends in map_message_expression, so a method definition and its
   call sites share a name. *)
and name_of_keyword_selector (env : env) (kwsel : CST.keyword_selector) :
    G.ident =
  let labels =
    List.filter_map
      (fun (lbl_opt, _colon, _mt, _param) -> Option.map (str env) lbl_opt)
      kwsel
  in
  match labels with
  | [] -> ("", G.fake "")
  | (s, t0) :: _ -> (s ^ ":", t0)

and name_of_method_selector_no_list (env : env)
    (x : CST.method_selector_no_list) : G.ident =
  match x with
  | `Id id -> str env id
  | `Rep1_kw_decl kwsel -> name_of_keyword_selector env kwsel
  | `Rep1_kw_decl_COMMA_DOTDOTDOT (kwsel, _, _) ->
      name_of_keyword_selector env kwsel

and name_of_method_selector (env : env) (x : CST.method_selector) : G.ident =
  match x with
  | `Choice_id x -> name_of_method_selector_no_list env x
  | `Rep1_kw_decl_COMMA (kwsel, _) -> name_of_keyword_selector env kwsel

and map_interface_declaration (env : env) (x : CST.interface_declaration) :
    G.field list =
  match x with
  | `Decl x -> [ G.F (map_declaration env x) ]
  | `Func_defi x -> [ G.F (map_function_definition env x) ]
  | `Meth_decl (sign, rettype, _attr, parts, _vararg, _mods, _semis) ->
      let names =
        List.filter_map
          (fun part ->
            match part with
            | `Choice_choice_id_opt_attr_spec_opt_meth_param (ms, _, _) ->
                Some (name_of_method_selector env ms)
            | `Meth_param _ -> None)
          parts
      in
      let name =
        match names with
        | [] -> ("method", G.fake "method")
        | (s0, t0) :: rest -> (String.concat "" (s0 :: List.map fst rest), t0)
      in
      let signtok =
        match sign with
        | `PLUS t -> token env t
        | `DASH t -> token env t
      in
      let fdef =
        {
          G.fkind = (G.Method, signtok);
          fparams = fb [];
          frettype = Option.map (map_method_type env) rettype;
          fbody = G.FBDecl (fake_sc ());
        }
      in
      [ G.F (G.DefStmt (G.basic_entity name, G.FuncDef fdef) |> G.s) ]
  (* Conditional members: flatten every branch into the class body, like
     the statement-level map_preproc_if. tree-sitter gives `#if` a
     class-context type with interface_declaration children, but `#ifdef`
     reuses the generic preproc_ifdef whose children are block items. *)
  | `Prep_if_in_inte_decl (_p, _cond, _nl, decls, else_opt, _endif) -> (
      List.concat_map (map_interface_declaration env) decls
      @
      match else_opt with
      | Some r -> map_preproc_alt_in_interface env r
      | None -> [])
  | `Prep_ifdef x -> List.map (fun st -> G.F st) (map_preproc_ifdef env x)
  | _ -> []

and map_preproc_alt_in_interface (env : env)
    (x : CST.anon_choice_prep_else_in_inte_decl_eefbd14) : G.field list =
  match x with
  | `Prep_else_in_inte_decl (_p, decls) ->
      List.concat_map (map_interface_declaration env) decls
  | `Prep_elif_in_inte_decl (_p, _cond, _nl, decls, rest) -> (
      List.concat_map (map_interface_declaration env) decls
      @
      match rest with
      | Some r -> map_preproc_alt_in_interface env r
      | None -> [])

and map_implementation_definition (env : env)
    (x : CST.implementation_definition) : G.field list =
  match x with
  | `Func_defi x -> [ G.F (map_function_definition env x) ]
  | `Decl x -> [ G.F (map_declaration env x) ]
  | `Meth_defi
      (sign, rettype, _attr, sel, _vararg, _decls, _mods, _semi1, body, _semi2)
    ->
      let name =
        match sel with
        | `Choice_id_opt_meth_param_rep_opt_choice_choice_id_meth_param (msnl, _)
          ->
            name_of_method_selector_no_list env msnl
        | `Meth_param_rep_opt_choice_choice_id_meth_param _ ->
            ("method", G.fake "method")
      in
      let signtok =
        match sign with
        | `PLUS t -> token env t
        | `DASH t -> token env t
      in
      let fdef =
        {
          G.fkind = (G.Method, signtok);
          fparams = fb [];
          frettype = Option.map (map_method_type env) rettype;
          fbody = G.FBStmt (map_compound_statement env body);
        }
      in
      [ G.F (G.DefStmt (G.basic_entity name, G.FuncDef fdef) |> G.s) ]
  | `Prep_if_in_impl_defi (_p, _cond, _nl, defs, else_opt, _endif) -> (
      List.concat_map (map_implementation_definition env) defs
      @
      match else_opt with
      | Some r -> map_preproc_alt_in_implementation env r
      | None -> [])
  | `Prep_ifdef x -> List.map (fun st -> G.F st) (map_preproc_ifdef env x)
  | _ -> []

and map_preproc_alt_in_implementation (env : env)
    (x : CST.anon_choice_prep_else_in_impl_defi_a30bcf7) : G.field list =
  match x with
  | `Prep_else_in_impl_defi (_p, defs) ->
      List.concat_map (map_implementation_definition env) defs
  | `Prep_elif_in_impl_defi (_p, _cond, _nl, defs, rest) -> (
      List.concat_map (map_implementation_definition env) defs
      @
      match rest with
      | Some r -> map_preproc_alt_in_implementation env r
      | None -> [])

and map_class_interface (env : env) (x : CST.class_interface) : G.stmt =
  let header, _tp, inheritance, _pa, _iv, decls, _tend = x in
  let _mods, _at, name, _semi = header in
  let ent = G.basic_entity (str env name) in
  let cextends =
    match inheritance with
    | Some (`COLON_id_opt_para_args (_c, super, _)) ->
        [ (G.TyN (H2.name_of_id (str env super)) |> G.t, None) ]
    | Some (`LPAR_opt_id_RPAR _)
    | None ->
        []
  in
  let fields = List.concat_map (map_interface_declaration env) decls in
  let cdef =
    {
      G.ckind = (G.Class, token env name);
      cextends;
      cimplements = [];
      cmixins = [];
      cparams = fb [];
      cbody = fb fields;
    }
  in
  G.DefStmt (ent, G.ClassDef cdef) |> G.s

and map_class_implementation (env : env) (x : CST.class_implementation) : G.stmt
    =
  let header, _tp, inheritance, _iv, defs, _tend = x in
  let _mods, _at, name, _semi = header in
  let ent = G.basic_entity (str env name) in
  let cextends =
    match inheritance with
    | Some (`COLON_id (_c, super)) ->
        [ (G.TyN (H2.name_of_id (str env super)) |> G.t, None) ]
    | Some (`LPAR_id_RPAR _)
    | None ->
        []
  in
  let fields = List.concat_map (map_implementation_definition env) defs in
  let cdef =
    {
      G.ckind = (G.Class, token env name);
      cextends;
      cimplements = [];
      cmixins = [];
      cparams = fb [];
      cbody = fb fields;
    }
  in
  G.DefStmt (ent, G.ClassDef cdef) |> G.s

and map_top_level_statement (env : env) (x : CST.top_level_statement) :
    G.stmt list =
  match x with
  | `Semg_ellips t ->
      [ G.ExprStmt (G.Ellipsis (token env t) |> G.e, fake_sc ()) |> G.s ]
  | `Choice_case_stmt x -> (
      match x with
      | `Comp_stmt x -> [ map_compound_statement env x ]
      | `Top_level_exp_stmt (eopt, sc) -> (
          match eopt with
          | Some enb ->
              [
                G.ExprStmt (map_expression_not_binary env enb, token env sc)
                |> G.s;
              ]
          | None -> [])
      | `Ret_stmt x -> [ map_return_statement env x ]
      | _ -> [ todo_stmt env (fake_sc ()) ])

and map_top_level_item (env : env) (x : CST.top_level_item) : G.stmt list =
  match x with
  | `Choice_func_defi x -> (
      match x with
      | `Func_defi x -> [ map_function_definition env x ]
      | `Decl x -> [ map_declaration env x ]
      | `Top_level_stmt x -> map_top_level_statement env x
      | `Prep_if x -> map_preproc_if env x
      | `Prep_ifdef x -> map_preproc_ifdef env x
      | _ -> [ todo_stmt env (fake_sc ()) ])
  | `Class_inte x -> [ map_class_interface env x ]
  | `Class_impl x -> [ map_class_implementation env x ]
  | _ -> [ todo_stmt env (fake_sc ()) ]

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)

let map_translation_unit (env : env) (x : CST.translation_unit) : G.program =
  match x with
  | `Rep_top_level_item items -> List.concat_map (map_top_level_item env) items
  | `Semg_exp (_, e) -> [ G.ExprStmt (map_expression env e, fake_sc ()) |> G.s ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_objc.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Program } in
      map_translation_unit env cst)

(* A bare expression pattern such as `[req url]` or `foo(...)` is not a valid
   top-level item on its own (a top-level expression statement needs a trailing
   `;`, and error recovery over a semicolon-less unary message send fails). So
   first try parsing the pattern as-is, and on any error retry through the
   grammar's dedicated `semgrep_expression` entry point by prefixing the
   `__SEMGREP_EXPRESSION` sentinel, which forces "parse a standalone
   expression" mode. Mirrors the semgrep-cpp overlay. *)
let parse_expression_or_source_file str =
  let res = Tree_sitter_objc.Parse.string str in
  match res.errors with
  | [] -> res
  | _ ->
      let expr_str = "__SEMGREP_EXPRESSION " ^ str in
      Tree_sitter_objc.Parse.string expr_str

let parse_pattern str_ =
  H.wrap_parser
    (fun () -> parse_expression_or_source_file str_)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern str_; extra = Pattern }
      in
      match cst with
      | `Semg_exp (_, e) -> G.E (map_expression env e)
      | `Rep_top_level_item _ -> (
          match map_translation_unit env cst with
          | [ s ] -> (
              match s.G.s with
              | G.ExprStmt (e, _) -> G.E e
              | _ -> G.S s)
          | stmts -> G.Ss stmts))

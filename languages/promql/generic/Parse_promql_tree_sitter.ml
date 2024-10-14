(* Michael Hoffmann
 *
 * Copyright (c) 2022 R2C
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
module CST = Tree_sitter_promql.CST
module H = Parse_tree_sitter_helpers
module H2 = AST_generic_helpers
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Promql parser using tree-sitter-lang/semgrep-promql and converting
 * directly to AST_generic.ml
 *
 * TODO:
 * - binary operators groupings are ignored for now
 * - support offset and @ modifier for subqueries and selectors
 * - code cleanup; but this seems to work somewhat so good enough for now
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let str = H.str
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

let map_quoted_string (env : env) (x : CST.quoted_string) =
  let map_single_quoted_string (env : env) (x : CST.single_quoted_string) =
    (* We have to remove the quotes so that differently quoted strings match *)
    let s, t = str env x in
    let s' = String.sub s 1 (String.length s - 2) in
    G.L (G.String (fb (s', t))) |> G.e
  in
  match x with
  | `Single_quoted_str tok -> map_single_quoted_string env tok
  | `Double_quoted_str tok -> map_single_quoted_string env tok
  | `Back_quoted_str tok -> map_single_quoted_string env tok

let map_string_literal (env : env) (x : CST.string_literal) =
  match x with
  | `Semg_meta tok -> G.N (H2.name_of_id (str env tok)) |> G.e
  | `Quoted_str tok -> map_quoted_string env tok

let map_float_literal (env : env) (x : CST.float_literal) =
  match x with
  | `Semg_meta tok -> G.N (H2.name_of_id (str env tok)) |> G.e
  | `Pat_db4e4e9 x ->
      let s, tok = str env x in
      G.L (G.Float (float_of_string_opt s, tok)) |> G.e

let map_literal_expression (env : env) (x : CST.literal_expression) =
  match x with
  | `Float_lit tok -> map_float_literal env tok
  | `Str_lit tok -> map_string_literal env tok

let map_duration (env : env) (x : CST.duration) =
  match x with
  | `Semg_meta tok -> G.N (H2.name_of_id (str env tok)) |> G.e
  | `Rep1_pat_780550e_choice_ms x ->
      let dur =
        List_.map
          (fun (f, d) ->
            let si, ti = str env f in
            let pi = Parsed_int.parse (si, ti) in
            let sd, td =
              match d with
              | `Ms tok -> str env tok
              | `S tok -> str env tok
              | `M tok -> str env tok
              | `H tok -> str env tok
              | `D tok -> str env tok
              | `W tok -> str env tok
              | `Y tok -> str env tok
            in
            G.Container
              ( G.Tuple,
                fb
                  [ G.L (G.Int pi) |> G.e; G.L (G.String (fb (sd, td))) |> G.e ]
              )
            |> G.e)
          x
      in
      G.Container (G.List, fb dur) |> G.e

let map_label_name (env : env) (x : CST.label_name) =
  match x with
  | `Semg_meta tok -> G.N (H2.name_of_id (str env tok))
  | `Id tok -> G.N (H2.name_of_id (str env tok))

let map_label_value (env : env) (x : CST.label_value) =
  match x with
  | `Semg_meta tok -> G.N (H2.name_of_id (str env tok)) |> G.e
  | `Quoted_str tok -> map_quoted_string env tok

let map_label_matcher (env : env) ((v1, v2, v3) : CST.label_matcher) =
  let s, st =
    match v1 with
    | `Semg_meta tok -> str env tok
    | `Id tok -> str env tok
  in
  let e, et =
    match v2 with
    | `EQ tok -> str env tok
    | `BANGEQ tok -> str env tok
    | `EQTILDE tok -> str env tok
    | `BANGTILDE tok -> str env tok
  in
  let n = G.N (G.Id ((s, st), G.empty_id_info ())) |> G.e in
  let r = G.N (G.Id ((e, et), G.empty_id_info ())) |> G.e in
  let v3 = map_label_value env v3 in
  G.Container (G.Tuple, fb [ n; r; v3 ]) |> G.e

let map_series_matcher (env : env) (x : CST.series_matcher) =
  (*
      http_requests_total{a="b"} -> {(__name__, "=", http_requests_total), (a ,"=", b)})
  *)
  let map_label_selectors_to_list (env : env) ((_, v1, _) : CST.label_selectors)
      =
    let map_choice_semg_ellips env x =
      match x with
      | `Semg_ellips tok ->
          let _, t = str env tok in
          G.Ellipsis t |> G.e
      | `Label_matc x -> map_label_matcher env x
    in
    match v1 with
    | Some (h, t, _) ->
        let h = map_choice_semg_ellips env h in
        let t = List_.map (fun (_, x) -> map_choice_semg_ellips env x) t in
        [ h ] @ t
    | None -> []
  in

  let map_name_to_name_label_matcher env v1 =
    (* "http_requests_total" is equivalent to {__name__="http_requests_total"} *)
    let n, tok =
      match v1 with
      | `Semg_meta tok ->
          let s, t = str env tok in
          (G.N (H2.name_of_id (s, t)) |> G.e, t)
      | `Id tok ->
          let s, t = str env tok in
          (G.L (G.String (fb (s, t))) |> G.e, t)
    in
    G.Container
      ( G.Tuple,
        fb
          [
            G.N (G.Id (("__name__", tok), G.empty_id_info ~hidden:true ()))
            |> G.e;
            G.N (G.Id (("=", tok), G.empty_id_info ~hidden:true ())) |> G.e;
            n;
          ] )
    |> G.e
  in

  match x with
  | `Metric_name v1 ->
      let name_matcher = map_name_to_name_label_matcher env v1 in
      G.Container (G.Set, fb [ name_matcher ]) |> G.e
  | `Label_selecs v1 ->
      let label_matchers = map_label_selectors_to_list env v1 in
      G.Container (G.Set, fb label_matchers) |> G.e
  | `Metric_name_label_selecs (v1, v2) ->
      let name_matcher = map_name_to_name_label_matcher env v1 in
      let label_matchers = map_label_selectors_to_list env v2 in
      G.Container (G.Set, fb ([ name_matcher ] @ label_matchers)) |> G.e

let map_instant_vector_selector (env : env)
    ((v1, _) : CST.instant_vector_selector) =
  (* TODO: modifier and offset *)
  let v1 = map_series_matcher env v1 in
  G.Container (G.Tuple, fb [ v1 ]) |> G.e

let map_range_vector_selector (env : env)
    ((v1, v2, _) : CST.range_vector_selector) =
  (* TODO: modifier and offset *)
  let v1 = map_series_matcher env v1 in
  let _, v2, _ = v2 in
  let range = map_duration env v2 in
  G.Container (G.Tuple, fb [ v1; range ]) |> G.e

let map_selector_expression (env : env) (x : CST.selector_expression) =
  match x with
  | `Inst_vec_sele tok -> map_instant_vector_selector env tok
  | `Range_vec_sele tok -> map_range_vector_selector env tok

let map_function_grouping (env : env) ((v1, t1, v2, t2) : CST.grouping) =
  let map_choice_semg_ellpis env x =
    match x with
    | `Semg_ellips tok ->
        let _, t = str env tok in
        G.Ellipsis t
    | `Label_name x -> map_label_name env x
  in
  let _, t1 = str env t1 in
  let _, t2 = str env t2 in
  let k =
    match v1 with
    | `Pat_by tok -> str env tok
    | `Pat_with tok -> str env tok
  in
  let v =
    match v2 with
    | Some (v1, v2, _) ->
        let h = map_choice_semg_ellpis env v1 |> G.e in
        let t =
          List_.map (fun (_, lbl) -> map_choice_semg_ellpis env lbl |> G.e) v2
        in
        G.Container (G.Set, (t1, [ h ] @ t, t2)) |> G.e
    | None -> G.Container (G.Set, (t1, [], t2)) |> G.e
  in
  [ G.ArgKwdOptional (k, v) ]

let rec map_function_args (env : env) ((_, v1, _) : CST.function_args) =
  match v1 with
  | Some (v1, v2, _) ->
      let v1 = map_query env v1 in
      let v2 =
        List_.map
          (fun (_, v1) ->
            let v1 = map_query env v1 in
            G.Arg v1)
          v2
      in
      [ G.Arg v1 ] @ v2
  | None -> []

and map_function_expression (env : env) (x : CST.function_call) =
  (*
      sum /by|without/ (a, b, c) (X) -> sum(X, /by|without/=(a,b,c))
  *)
  let v1, v2, v3 =
    match x with
    | `Func_name_func_args (v1, v2) -> (v1, v2, None)
    | `Func_name_grou_func_args (v1, v2, v3) -> (v1, v3, Some v2)
    | `Func_name_func_args_grou (v1, v2, v3) -> (v1, v2, Some v3)
  in
  let f =
    match v1 with
    | `Semg_meta tok -> str env tok
    | `Id tok -> str env tok
  in
  let n = G.N (H2.name_of_id f) |> G.e in
  let args = map_function_args env v2 in
  match v3 with
  | Some v3 ->
      let kwargs = map_function_grouping env v3 in
      G.Call (n, fb (args @ kwargs)) |> G.e
  | None -> G.Call (n, fb args) |> G.e

and map_subquery_expression (env : env) ((v1, v2, _) : CST.subquery) =
  (*
    (X)[10m:30s] => ((X), 10m, 30s)

     TODO: modifier and offset
  *)
  let q = G.Container (G.Tuple, fb [ map_query env v1 ]) |> G.e in
  let _, range, _, mbstep, _ = v2 in
  let range = map_duration env range in
  match mbstep with
  | Some r ->
      let step = map_duration env r in
      G.Container (G.Tuple, fb [ q; range; step ]) |> G.e
  | None -> G.Container (G.Tuple, fb [ q; range ]) |> G.e

and map_operator_expression (env : env) (x : CST.operator_expression) =
  (*
     TODO: groupings
  *)
  let v1, (op, tok), _, v4 =
    match x with
    | `Query_choice_HAT_opt_bin_grou_query (v1, v2, v3, v4) ->
        let op, tok =
          match v2 with
          | `HAT tok ->
              let _, tok = str env tok in
              (G.Pow, tok)
        in
        (v1, (op, tok), v3, v4)
    | `Query_choice_STAR_opt_bin_grou_query (v1, v2, v3, v4) ->
        let op, tok =
          match v2 with
          | `PERC tok ->
              let _, tok = str env tok in
              (G.Mod, tok)
          | `SLASH tok ->
              let _, tok = str env tok in
              (G.Div, tok)
          | `STAR tok ->
              let _, tok = str env tok in
              (G.Mult, tok)
        in
        (v1, (op, tok), v3, v4)
    | `Query_choice_PLUS_opt_bin_grou_query (v1, v2, v3, v4) ->
        let op, tok =
          match v2 with
          | `PLUS tok ->
              let _, tok = str env tok in
              (G.Plus, tok)
          | `DASH tok ->
              let _, tok = str env tok in
              (G.Minus, tok)
        in
        (v1, (op, tok), v3, v4)
    | `Query_choice_EQEQ_opt_pat_bool_opt_bin_grou_query (v1, v2, _, v3, v4) ->
        let op, tok =
          match v2 with
          | `BANGEQ tok ->
              let _, tok = str env tok in
              (G.NotEq, tok)
          | `EQEQ tok ->
              let _, tok = str env tok in
              (G.Eq, tok)
          | `GT tok ->
              let _, tok = str env tok in
              (G.Gt, tok)
          | `GTEQ tok ->
              let _, tok = str env tok in
              (G.GtE, tok)
          | `LT tok ->
              let _, tok = str env tok in
              (G.Lt, tok)
          | `LTEQ tok ->
              let _, tok = str env tok in
              (G.LtE, tok)
        in
        (v1, (op, tok), v3, v4)
    | `Query_choice_pat_and_opt_bin_grou_query (v1, v2, v3, v4) ->
        let op, tok =
          match v2 with
          | `Pat_and tok ->
              let _, tok = str env tok in
              (G.And, tok)
          | `Pat_or tok ->
              let _, tok = str env tok in
              (G.Or, tok)
          | `Pat_unless tok ->
              let _, tok = str env tok in
              (G.Xor, tok)
        in
        (v1, (op, tok), v3, v4)
    | `Query_choice_pat_atan2_opt_bin_grou_query (v1, v2, v3, v4) ->
        let op, tok =
          match v2 with
          | `Pat_atan2 tok ->
              (* This does not map cleanly to an operator but lets abuse a special one *)
              let _, tok = str env tok in
              (G.Elvis, tok)
        in
        (v1, (op, tok), v3, v4)
  in
  let l = map_query env v1 in
  let r = map_query env v4 in
  G.Call (G.IdSpecial (G.Op op, tok) |> G.e, fb [ G.arg l; G.arg r ]) |> G.e

and map_query_expression (env : env) (x : CST.query_expression) =
  match x with
  | `Semg_ellips tok ->
      let _, t = str env tok in
      G.Ellipsis t |> G.e
  | `Choice_lit_exp x -> (
      match x with
      | `Lit_exp x -> map_literal_expression env x
      | `Call_exp x -> map_function_expression env x
      | `Sele_exp x -> map_selector_expression env x
      | `Subq_exp x -> map_subquery_expression env x
      | `Op_exp x -> map_operator_expression env x)

and map_query (env : env) (x : CST.query) =
  match x with
  | `Query_exp x -> map_query_expression env x
  | `LPAR_query_exp_RPAR (_, x, _) -> map_query_expression env x

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_promql.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      let x = map_query env cst in
      [ x |> G.exprstmt ])

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_promql.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      let e = map_query env cst in
      G.E e)

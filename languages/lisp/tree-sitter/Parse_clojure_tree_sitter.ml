(* Yoann Padioleau
 *
 * Copyright (C) 2023 r2c
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
module CST = Tree_sitter_clojure.CST
module R = Raw_tree
open AST_generic
module G = AST_generic
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Clojure parser using tree-sitter-lang/semgrep-clojure and converting
 * directly to AST_generic.ml
 * (we could convert to ../ast/ast_lisp.ml but seems simpler to
 * go directly to the generic AST given ast_lisp.ml is really small).
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let _token = H.token
let _str = H.str
let _todo (_env : env) _ = failwith "not implemented lol"
let token (env : env) (tok : Tree_sitter_run.Token.t) = R.Token (H.str env tok)

let name (env : env) (tok : Tree_sitter_run.Token.t) =
  let s, t = H.str env tok in
  (* alt: detect / and . in tree-sitter-clojure instead *)
  (* TODO: also split for '.' *)
  if s =~ "^\\(.*\\)/\\(.*\\)$" then
    let before, after = Common.matched2 s in
    let t1, t2 = Tok.split_tok_at_bytepos (String.length before) t in
    let id1 = R.Token (before, t1) in
    let id2 = R.Token (after, t2) in
    R.List [ id1; id2 ]
  else R.Token (s, t)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-clojure/.../Boilerplate.ml *)

let map_tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot
    (env : env)
    (tok :
      CST.tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot)
    =
  (* tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot *)
  token env tok

let rec map_anon_choice_read_cond_lit_137feb9 (env : env)
    (x : CST.anon_choice_read_cond_lit_137feb9) =
  match x with
  | `Read_cond_lit x -> R.Case ("Read_cond_lit", map_read_cond_lit env x)
  | `Map_lit x -> R.Case ("Map_lit", map_map_lit env x)
  | `Str_lit tok -> R.Case ("Str_lit", (* str_lit *) token env tok)
  | `Kwd_lit tok -> R.Case ("Kwd_lit", (* kwd_lit *) token env tok)
  | `Sym_lit x -> R.Case ("Sym_lit", map_sym_lit env x)

and map_bare_list_lit (env : env) ((v1, v2, v3) : CST.bare_list_lit) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_bare_map_lit (env : env) ((v1, v2, v3) : CST.bare_map_lit) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_bare_set_lit (env : env) ((v1, v2, v3, v4) : CST.bare_set_lit) =
  let v1 = (* "#" *) token env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 = map_source env v3 in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [ v1; v2; v3; v4 ]

and map_bare_vec_lit (env : env) ((v1, v2, v3) : CST.bare_vec_lit) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [ v1; v2; v3 ]

and map_form (env : env) (x : CST.form) =
  match x with
  | `Num_lit tok -> R.Case ("Num_lit", (* num_lit *) token env tok)
  | `Kwd_lit tok -> R.Case ("Kwd_lit", (* kwd_lit *) token env tok)
  | `Str_lit tok -> R.Case ("Str_lit", (* str_lit *) token env tok)
  | `Char_lit tok -> R.Case ("Char_lit", (* char_lit *) token env tok)
  | `Nil_lit tok -> R.Case ("Nil_lit", (* nil_lit *) token env tok)
  | `Bool_lit tok -> R.Case ("Bool_lit", (* bool_lit *) token env tok)
  | `Sym_lit x -> R.Case ("Sym_lit", map_sym_lit env x)
  | `List_lit x -> R.Case ("List_lit", map_list_lit env x)
  | `Map_lit x -> R.Case ("Map_lit", map_map_lit env x)
  | `Vec_lit (v1, v2) ->
      R.Case
        ( "Vec_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = map_bare_vec_lit env v2 in
          v2 )
  | `Set_lit (v1, v2) ->
      R.Case
        ( "Set_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = map_bare_set_lit env v2 in
          v2 )
  | `Anon_fn_lit (v1, v2, v3) ->
      R.Case
        ( "Anon_fn_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "#" *) token env v2 in
          let v3 = map_bare_list_lit env v3 in
          R.Tuple [ v2; v3 ] )
  | `Regex_lit (v1, v2) ->
      R.Case
        ( "Regex_lit",
          let v1 = (* "#" *) token env v1 in
          let v2 =
            map_tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot
              env v2
          in
          R.Tuple [ v1; v2 ] )
  | `Read_cond_lit x -> R.Case ("Read_cond_lit", map_read_cond_lit env x)
  | `Spli_read_cond_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Spli_read_cond_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "#?@" *) token env v2 in
          let v3 = R.List (List_.map (token env (* ws *)) v3) in
          let v4 = map_bare_list_lit env v4 in
          R.Tuple [ v2; v3; v4 ] )
  | `Ns_map_lit (v1, v2, v3, v4, v5) ->
      R.Case
        ( "Ns_map_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "#" *) token env v2 in
          let v3 =
            match v3 with
            | `Auto_res_mark tok ->
                R.Case ("Auto_res_mark", (* auto_res_mark *) token env tok)
            | `Kwd_lit tok -> R.Case ("Kwd_lit", (* kwd_lit *) token env tok)
          in
          let _v4 = List_.map (map_gap env) v4 in
          let v5 = map_bare_map_lit env v5 in
          R.Tuple [ v2; v3; v5 ] )
  | `Var_quot_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Var_quot_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "#'" *) token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_form env v4 in
          R.Tuple [ v2; v4 ] )
  | `Sym_val_lit (v1, v2, v3) ->
      R.Case
        ( "Sym_val_lit",
          let v1 = (* "##" *) token env v1 in
          let _v2 = List_.map (map_gap env) v2 in
          let v3 = map_sym_lit env v3 in
          R.Tuple [ v1; v3 ] )
  | `Eval_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Eval_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "#=" *) token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 =
            match v4 with
            | `List_lit x -> R.Case ("List_lit", map_list_lit env x)
            | `Read_cond_lit x ->
                R.Case ("Read_cond_lit", map_read_cond_lit env x)
            | `Sym_lit x -> R.Case ("Sym_lit", map_sym_lit env x)
          in
          R.Tuple [ v2; v4 ] )
  | `Tagged_or_ctor_lit (v1, v2, v3, v4, v5, v6) ->
      R.Case
        ( "Tagged_or_ctor_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "#" *) token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_sym_lit env v4 in
          let _v5 = List_.map (map_gap env) v5 in
          let v6 = map_form env v6 in
          R.Tuple [ v2; v4; v6 ] )
  | `Dere_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Dere_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "@" *) token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_form env v4 in
          R.Tuple [ v2; v4 ] )
  | `Quot_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Quot_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "'" *) token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_form env v4 in
          R.Tuple [ v2; v4 ] )
  | `Syn_quot_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Syn_quot_lit",
          let _v1TODO = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "`" *) token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_form env v4 in
          R.Tuple [ v2; v4 ] )
  | `Unqu_spli_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Unqu_spli_lit",
          let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "~@" *) token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_form env v4 in
          R.Tuple [ v2; v4 ] )
  | `Unqu_lit (v1, v2, v3, v4) ->
      R.Case
        ( "Unqu_lit",
          let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
          let v2 = (* "~" *) token env v2 in
          let _v3 = List_.map (map_gap env) v3 in
          let v4 = map_form env v4 in
          R.Tuple [ v2; v4 ] )

(* whitespace/comment *)
and map_gap (_env : env) (x : CST.gap) : unit =
  match x with
  | `Ws _tok -> ()
  | `Comm _tok -> ()
  (* ?? *)
  | `Dis_expr (_v1, _v2, _v3formTODO) -> ()

and map_list_lit (env : env) ((v1, v2) : CST.list_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let v2 = map_bare_list_lit env v2 in
  v2

and map_map_lit (env : env) ((v1, v2) : CST.map_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let v2 = map_bare_map_lit env v2 in
  v2

and map_meta_lit (env : env) ((v1, v2, v3) : CST.meta_lit) =
  let v1 = (* "^" *) token env v1 in
  let _v2 = List_.map (map_gap env) v2 in
  let v3 = map_anon_choice_read_cond_lit_137feb9 env v3 in
  R.Tuple [ v1; v3 ]

and map_metadata_lit (env : env) ((v1, v2) : CST.metadata_lit) =
  let v1 =
    match v1 with
    | `Meta_lit x -> R.Case ("Meta_lit", map_meta_lit env x)
    | `Old_meta_lit x -> R.Case ("Old_meta_lit", map_old_meta_lit env x)
  in
  let _v2 =
    match v2 with
    | Some xs ->
        let _ = List_.map (map_gap env) xs in
        ()
    | None -> ()
  in
  R.Tuple [ v1 ]

and map_old_meta_lit (env : env) ((v1, v2, v3) : CST.old_meta_lit) =
  let v1 = (* "#^" *) token env v1 in
  let _v2 = List_.map (map_gap env) v2 in
  let v3 = map_anon_choice_read_cond_lit_137feb9 env v3 in
  R.Tuple [ v1; v3 ]

and map_read_cond_lit (env : env) ((v1, v2, v3, v4) : CST.read_cond_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let v2 = (* "#?" *) token env v2 in
  let v3 = R.List (List_.map (token env (* ws *)) v3) in
  let v4 = map_bare_list_lit env v4 in
  R.Tuple [ v2; v3; v4 ]

and map_source (env : env) (xs : CST.source) =
  R.List
    (List_.filter_map
       (fun x ->
         match x with
         | `Form x ->
             let x = map_form env x in
             Some (R.Case ("Form", x))
         | `Gap _x -> None)
       xs)

and map_sym_lit (env : env) ((v1, v2) : CST.sym_lit) =
  let _v1 = R.List (List_.map (map_metadata_lit env) v1) in
  let v2 = name env v2 in
  v2

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_clojure.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      let x = map_source env cst in
      [ x |> G.stmt_of_raw ])

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_clojure.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      let e = map_source env cst in
      (* this will be simplified if needed in Parse_pattern.normalize_any *)
      Raw e)

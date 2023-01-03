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
module CST = Tree_sitter_clojure.CST
module PI = Parse_info
open AST_generic
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Clojure parser using tree-sitter-lang/semgrep-clojure and converting
 * directly to AST_generic.ml
 * (we could convert to pfff/lang_lisp/ast_lisp.ml but seems simpler to
 * go directly to the generic AST given ast_lisp.ml is really small).
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let _str = H.str
let todo (_env : env) _ = failwith "not implemented"

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-clojure/.../Boilerplate.ml *)

let rec map_anon_choice_read_cond_lit_137feb9 (env : env)
    (x : CST.anon_choice_read_cond_lit_137feb9) =
  match x with
  | `Read_cond_lit x -> map_read_cond_lit env x
  | `Map_lit x -> map_map_lit env x
  | `Str_lit tok -> (* str_lit *) token env tok
  | `Kwd_lit tok -> (* kwd_lit *) token env tok
  | `Sym_lit x -> map_sym_lit env x

and map_bare_list_lit (env : env) ((v1, v2, v3) : CST.bare_list_lit) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_bare_map_lit (env : env) ((v1, v2, v3) : CST.bare_map_lit) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_bare_set_lit (env : env) ((v1, v2, v3, v4) : CST.bare_set_lit) =
  let v1 = (* "#" *) token env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 = map_source env v3 in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_bare_vec_lit (env : env) ((v1, v2, v3) : CST.bare_vec_lit) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* "]" *) token env v3 in
  todo env (v1, v2, v3)

and map_form (env : env) (x : CST.form) : expr =
  match x with
  | `Num_lit tok ->
      let x = (* num_lit *) token env tok in
      todo env x
  | `Kwd_lit tok ->
      let x = (* kwd_lit *) token env tok in
      todo env x
  | `Str_lit tok ->
      let x = (* str_lit *) token env tok in
      todo env x
  | `Char_lit tok ->
      let x = (* char_lit *) token env tok in
      todo env x
  | `Nil_lit tok ->
      let x = (* nil_lit *) token env tok in
      todo env x
  | `Bool_lit tok ->
      let x = (* bool_lit *) token env tok in
      todo env x
  | `Sym_lit x ->
      let x = map_sym_lit env x in
      todo env x
  | `List_lit x ->
      let x = map_list_lit env x in
      todo env x
  | `Map_lit x ->
      let x = map_map_lit env x in
      todo env x
  | `Vec_lit (v1, v2) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = map_bare_vec_lit env v2 in
      todo env (v1, v2)
  | `Set_lit (v1, v2) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = map_bare_set_lit env v2 in
      todo env (v1, v2)
  | `Anon_fn_lit (v1, v2, v3) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = (* "#" *) token env v2 in
      let v3 = map_bare_list_lit env v3 in
      todo env (v1, v2, v3)
  | `Regex_lit (v1, v2) ->
      let v1 = (* "#" *) token env v1 in
      let v2 =
        (* tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot *)
        token env v2
      in
      todo env (v1, v2)
  | `Read_cond_lit x ->
      let x = map_read_cond_lit env x in
      todo env x
  | `Spli_read_cond_lit (v1, v2, v3, v4) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = (* "#?@" *) token env v2 in
      let v3 = Common.map (token env (* ws *)) v3 in
      let v4 = map_bare_list_lit env v4 in
      todo env (v1, v2, v3, v4)
  | `Ns_map_lit (v1, v2, v3, v4, v5) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = (* "#" *) token env v2 in
      let v3 =
        match v3 with
        | `Auto_res_mark tok -> (* auto_res_mark *) token env tok
        | `Kwd_lit tok -> (* kwd_lit *) token env tok
      in
      let v4 = Common.map (map_gap env) v4 in
      let v5 = map_bare_map_lit env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Var_quot_lit (v1, v2, v3, v4) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = (* "#'" *) token env v2 in
      let v3 = Common.map (map_gap env) v3 in
      let v4 = map_form env v4 in
      todo env (v1, v2, v3, v4)
  | `Sym_val_lit (v1, v2, v3) ->
      let v1 = (* "##" *) token env v1 in
      let v2 = Common.map (map_gap env) v2 in
      let v3 = map_sym_lit env v3 in
      todo env (v1, v2, v3)
  | `Eval_lit (v1, v2, v3, v4) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = (* "#=" *) token env v2 in
      let v3 = Common.map (map_gap env) v3 in
      let v4 =
        match v4 with
        | `List_lit x -> map_list_lit env x
        | `Read_cond_lit x -> map_read_cond_lit env x
        | `Sym_lit x -> map_sym_lit env x
      in
      todo env (v1, v2, v3, v4)
  | `Tagged_or_ctor_lit (v1, v2, v3, v4, v5, v6) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = (* "#" *) token env v2 in
      let v3 = Common.map (map_gap env) v3 in
      let v4 = map_sym_lit env v4 in
      let v5 = Common.map (map_gap env) v5 in
      let v6 = map_form env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Dere_lit (v1, v2, v3, v4) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = (* "@" *) token env v2 in
      let v3 = Common.map (map_gap env) v3 in
      let v4 = map_form env v4 in
      todo env (v1, v2, v3, v4)
  | `Quot_lit (v1, v2, v3, v4) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = (* "'" *) token env v2 in
      let v3 = Common.map (map_gap env) v3 in
      let v4 = map_form env v4 in
      todo env (v1, v2, v3, v4)
  | `Syn_quot_lit (v1, v2, v3, v4) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = (* "`" *) token env v2 in
      let v3 = Common.map (map_gap env) v3 in
      let v4 = map_form env v4 in
      todo env (v1, v2, v3, v4)
  | `Unqu_spli_lit (v1, v2, v3, v4) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = (* "~@" *) token env v2 in
      let v3 = Common.map (map_gap env) v3 in
      let v4 = map_form env v4 in
      todo env (v1, v2, v3, v4)
  | `Unqu_lit (v1, v2, v3, v4) ->
      let v1 = Common.map (map_metadata_lit env) v1 in
      let v2 = (* "~" *) token env v2 in
      let v3 = Common.map (map_gap env) v3 in
      let v4 = map_form env v4 in
      todo env (v1, v2, v3, v4)

and map_gap (env : env) (x : CST.gap) =
  match x with
  | `Ws tok -> (* ws *) token env tok
  | `Comm tok -> (* comment *) token env tok
  | `Dis_expr (v1, v2, v3) ->
      let v1 = (* "#_" *) token env v1 in
      let v2 = Common.map (map_gap env) v2 in
      let v3 = map_form env v3 in
      todo env (v1, v2, v3)

and map_list_lit (env : env) ((v1, v2) : CST.list_lit) =
  let v1 = Common.map (map_metadata_lit env) v1 in
  let v2 = map_bare_list_lit env v2 in
  todo env (v1, v2)

and map_map_lit (env : env) ((v1, v2) : CST.map_lit) =
  let v1 = Common.map (map_metadata_lit env) v1 in
  let v2 = map_bare_map_lit env v2 in
  todo env (v1, v2)

and map_meta_lit (env : env) ((v1, v2, v3) : CST.meta_lit) =
  let v1 = (* "^" *) token env v1 in
  let v2 = Common.map (map_gap env) v2 in
  let v3 = map_anon_choice_read_cond_lit_137feb9 env v3 in
  todo env (v1, v2, v3)

and map_metadata_lit (env : env) ((v1, v2) : CST.metadata_lit) =
  let v1 =
    match v1 with
    | `Meta_lit x -> map_meta_lit env x
    | `Old_meta_lit x -> map_old_meta_lit env x
  in
  let v2 =
    match v2 with
    | Some xs -> Common.map (map_gap env) xs
    | None -> todo env ()
  in
  todo env (v1, v2)

and map_old_meta_lit (env : env) ((v1, v2, v3) : CST.old_meta_lit) =
  let v1 = (* "#^" *) token env v1 in
  let v2 = Common.map (map_gap env) v2 in
  let v3 = map_anon_choice_read_cond_lit_137feb9 env v3 in
  todo env (v1, v2, v3)

and map_read_cond_lit (env : env) ((v1, v2, v3, v4) : CST.read_cond_lit) =
  let v1 = Common.map (map_metadata_lit env) v1 in
  let v2 = (* "#?" *) token env v2 in
  let v3 = Common.map (token env (* ws *)) v3 in
  let v4 = map_bare_list_lit env v4 in
  todo env (v1, v2, v3, v4)

and map_source (env : env) (xs : CST.source) : program =
  Common.map
    (fun x ->
      match x with
      | `Form x ->
          let x = map_form env x in
          todo env x
      | `Gap x ->
          let x = map_gap env x in
          todo env x)
    xs

and map_sym_lit (env : env) ((v1, v2) : CST.sym_lit) =
  let v1 = Common.map (map_metadata_lit env) v1 in
  let v2 = (* tok_pat_0a702c4_rep_choice_pat_0a702c4 *) token env v2 in
  todo env (v1, v2)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_clojure.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      let x = map_source env cst in
      x)

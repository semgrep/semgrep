(**
   Boilerplate to be used as a template when mapping the clojure CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_ws (env : env) (tok : CST.ws) =
  (* ws *) token env tok

let map_auto_res_mark (env : env) (tok : CST.auto_res_mark) =
  (* auto_res_mark *) token env tok

let map_char_lit (env : env) (tok : CST.char_lit) =
  (* char_lit *) token env tok

let map_tok_pat_0a702c4_rep_choice_pat_0a702c4 (env : env) (tok : CST.tok_pat_0a702c4_rep_choice_pat_0a702c4) =
  (* tok_pat_0a702c4_rep_choice_pat_0a702c4 *) token env tok

let map_str_lit (env : env) (tok : CST.str_lit) =
  (* str_lit *) token env tok

let map_nil_lit (env : env) (tok : CST.nil_lit) =
  (* nil_lit *) token env tok

let map_kwd_lit (env : env) (tok : CST.kwd_lit) =
  (* kwd_lit *) token env tok

let map_tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot (env : env) (tok : CST.tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot) =
  (* tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot *) token env tok

let map_num_lit (env : env) (tok : CST.num_lit) =
  (* num_lit *) token env tok

let map_bool_lit (env : env) (tok : CST.bool_lit) =
  (* bool_lit *) token env tok

let rec map_anon_choice_read_cond_lit_137feb9 (env : env) (x : CST.anon_choice_read_cond_lit_137feb9) =
  (match x with
  | `Read_cond_lit x -> R.Case ("Read_cond_lit",
      map_read_cond_lit env x
    )
  | `Map_lit x -> R.Case ("Map_lit",
      map_map_lit env x
    )
  | `Str_lit tok -> R.Case ("Str_lit",
      (* str_lit *) token env tok
    )
  | `Kwd_lit tok -> R.Case ("Kwd_lit",
      (* kwd_lit *) token env tok
    )
  | `Sym_lit x -> R.Case ("Sym_lit",
      map_sym_lit env x
    )
  )

and map_bare_list_lit (env : env) ((v1, v2, v3) : CST.bare_list_lit) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_bare_map_lit (env : env) ((v1, v2, v3) : CST.bare_map_lit) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_bare_set_lit (env : env) ((v1, v2, v3, v4) : CST.bare_set_lit) =
  let v1 = (* "#" *) token env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 = map_source env v3 in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_bare_vec_lit (env : env) ((v1, v2, v3) : CST.bare_vec_lit) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_source env v2 in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_form (env : env) (x : CST.form) =
  (match x with
  | `Num_lit tok -> R.Case ("Num_lit",
      (* num_lit *) token env tok
    )
  | `Kwd_lit tok -> R.Case ("Kwd_lit",
      (* kwd_lit *) token env tok
    )
  | `Str_lit tok -> R.Case ("Str_lit",
      (* str_lit *) token env tok
    )
  | `Char_lit tok -> R.Case ("Char_lit",
      (* char_lit *) token env tok
    )
  | `Nil_lit tok -> R.Case ("Nil_lit",
      (* nil_lit *) token env tok
    )
  | `Bool_lit tok -> R.Case ("Bool_lit",
      (* bool_lit *) token env tok
    )
  | `Sym_lit x -> R.Case ("Sym_lit",
      map_sym_lit env x
    )
  | `List_lit x -> R.Case ("List_lit",
      map_list_lit env x
    )
  | `Map_lit x -> R.Case ("Map_lit",
      map_map_lit env x
    )
  | `Vec_lit (v1, v2) -> R.Case ("Vec_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = map_bare_vec_lit env v2 in
      R.Tuple [v1; v2]
    )
  | `Set_lit (v1, v2) -> R.Case ("Set_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = map_bare_set_lit env v2 in
      R.Tuple [v1; v2]
    )
  | `Anon_fn_lit (v1, v2, v3) -> R.Case ("Anon_fn_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = (* "#" *) token env v2 in
      let v3 = map_bare_list_lit env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Regex_lit (v1, v2) -> R.Case ("Regex_lit",
      let v1 = (* "#" *) token env v1 in
      let v2 =
        map_tok_dquot_rep_pat_0d044a8_rep_bslash_pat_5058f1a_rep_pat_0d044a8_dquot env v2
      in
      R.Tuple [v1; v2]
    )
  | `Read_cond_lit x -> R.Case ("Read_cond_lit",
      map_read_cond_lit env x
    )
  | `Spli_read_cond_lit (v1, v2, v3, v4) -> R.Case ("Spli_read_cond_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = (* "#?@" *) token env v2 in
      let v3 = R.List (List.map (token env (* ws *)) v3) in
      let v4 = map_bare_list_lit env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Ns_map_lit (v1, v2, v3, v4, v5) -> R.Case ("Ns_map_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = (* "#" *) token env v2 in
      let v3 =
        (match v3 with
        | `Auto_res_mark tok -> R.Case ("Auto_res_mark",
            (* auto_res_mark *) token env tok
          )
        | `Kwd_lit tok -> R.Case ("Kwd_lit",
            (* kwd_lit *) token env tok
          )
        )
      in
      let v4 = R.List (List.map (map_gap env) v4) in
      let v5 = map_bare_map_lit env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Var_quot_lit (v1, v2, v3, v4) -> R.Case ("Var_quot_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = (* "#'" *) token env v2 in
      let v3 = R.List (List.map (map_gap env) v3) in
      let v4 = map_form env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Sym_val_lit (v1, v2, v3) -> R.Case ("Sym_val_lit",
      let v1 = (* "##" *) token env v1 in
      let v2 = R.List (List.map (map_gap env) v2) in
      let v3 = map_sym_lit env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Eval_lit (v1, v2, v3, v4) -> R.Case ("Eval_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = (* "#=" *) token env v2 in
      let v3 = R.List (List.map (map_gap env) v3) in
      let v4 =
        (match v4 with
        | `List_lit x -> R.Case ("List_lit",
            map_list_lit env x
          )
        | `Read_cond_lit x -> R.Case ("Read_cond_lit",
            map_read_cond_lit env x
          )
        | `Sym_lit x -> R.Case ("Sym_lit",
            map_sym_lit env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Tagged_or_ctor_lit (v1, v2, v3, v4, v5, v6) -> R.Case ("Tagged_or_ctor_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = (* "#" *) token env v2 in
      let v3 = R.List (List.map (map_gap env) v3) in
      let v4 = map_sym_lit env v4 in
      let v5 = R.List (List.map (map_gap env) v5) in
      let v6 = map_form env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Dere_lit (v1, v2, v3, v4) -> R.Case ("Dere_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = (* "@" *) token env v2 in
      let v3 = R.List (List.map (map_gap env) v3) in
      let v4 = map_form env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Quot_lit (v1, v2, v3, v4) -> R.Case ("Quot_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = (* "'" *) token env v2 in
      let v3 = R.List (List.map (map_gap env) v3) in
      let v4 = map_form env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Syn_quot_lit (v1, v2, v3, v4) -> R.Case ("Syn_quot_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = (* "`" *) token env v2 in
      let v3 = R.List (List.map (map_gap env) v3) in
      let v4 = map_form env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Unqu_spli_lit (v1, v2, v3, v4) -> R.Case ("Unqu_spli_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = (* "~@" *) token env v2 in
      let v3 = R.List (List.map (map_gap env) v3) in
      let v4 = map_form env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Unqu_lit (v1, v2, v3, v4) -> R.Case ("Unqu_lit",
      let v1 = R.List (List.map (map_metadata_lit env) v1) in
      let v2 = (* "~" *) token env v2 in
      let v3 = R.List (List.map (map_gap env) v3) in
      let v4 = map_form env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_gap (env : env) (x : CST.gap) =
  (match x with
  | `Ws tok -> R.Case ("Ws",
      (* ws *) token env tok
    )
  | `Comm tok -> R.Case ("Comm",
      (* comment *) token env tok
    )
  | `Dis_expr (v1, v2, v3) -> R.Case ("Dis_expr",
      let v1 = (* "#_" *) token env v1 in
      let v2 = R.List (List.map (map_gap env) v2) in
      let v3 = map_form env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_list_lit (env : env) ((v1, v2) : CST.list_lit) =
  let v1 = R.List (List.map (map_metadata_lit env) v1) in
  let v2 = map_bare_list_lit env v2 in
  R.Tuple [v1; v2]

and map_map_lit (env : env) ((v1, v2) : CST.map_lit) =
  let v1 = R.List (List.map (map_metadata_lit env) v1) in
  let v2 = map_bare_map_lit env v2 in
  R.Tuple [v1; v2]

and map_meta_lit (env : env) ((v1, v2, v3) : CST.meta_lit) =
  let v1 = (* "^" *) token env v1 in
  let v2 = R.List (List.map (map_gap env) v2) in
  let v3 = map_anon_choice_read_cond_lit_137feb9 env v3 in
  R.Tuple [v1; v2; v3]

and map_metadata_lit (env : env) ((v1, v2) : CST.metadata_lit) =
  let v1 =
    (match v1 with
    | `Meta_lit x -> R.Case ("Meta_lit",
        map_meta_lit env x
      )
    | `Old_meta_lit x -> R.Case ("Old_meta_lit",
        map_old_meta_lit env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some xs -> R.Option (Some (
        R.List (List.map (map_gap env) xs)
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_old_meta_lit (env : env) ((v1, v2, v3) : CST.old_meta_lit) =
  let v1 = (* "#^" *) token env v1 in
  let v2 = R.List (List.map (map_gap env) v2) in
  let v3 = map_anon_choice_read_cond_lit_137feb9 env v3 in
  R.Tuple [v1; v2; v3]

and map_read_cond_lit (env : env) ((v1, v2, v3, v4) : CST.read_cond_lit) =
  let v1 = R.List (List.map (map_metadata_lit env) v1) in
  let v2 = (* "#?" *) token env v2 in
  let v3 = R.List (List.map (token env (* ws *)) v3) in
  let v4 = map_bare_list_lit env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_source (env : env) (xs : CST.source) =
  R.List (List.map (fun x ->
    (match x with
    | `Form x -> R.Case ("Form",
        map_form env x
      )
    | `Gap x -> R.Case ("Gap",
        map_gap env x
      )
    )
  ) xs)

and map_sym_lit (env : env) ((v1, v2) : CST.sym_lit) =
  let v1 = R.List (List.map (map_metadata_lit env) v1) in
  let v2 =
    map_tok_pat_0a702c4_rep_choice_pat_0a702c4 env v2
  in
  R.Tuple [v1; v2]

let dump_tree root =
  map_source () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout
let dump_extras (extras : CST.extras) = ()

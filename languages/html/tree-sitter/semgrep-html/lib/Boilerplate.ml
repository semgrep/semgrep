(**
   Boilerplate to be used as a template when mapping the html CST
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

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* semgrep_metavariable *) token env tok

let map_implicit_end_tag (env : env) (tok : CST.implicit_end_tag) =
  (* implicit_end_tag *) token env tok

let map_pat_03aa317 (env : env) (tok : CST.pat_03aa317) =
  (* pattern [^>]+ *) token env tok

let map_style_start_tag_name (env : env) (tok : CST.style_start_tag_name) =
  (* style_start_tag_name *) token env tok

let map_attribute_value (env : env) (tok : CST.attribute_value) =
  (* pattern "[^<>\"'=\\s]+" *) token env tok

let map_pat_58fbb2e (env : env) (tok : CST.pat_58fbb2e) =
  (* pattern "[^']+" *) token env tok

let map_entity (env : env) (tok : CST.entity) =
  (* pattern &(#([xX][0-9a-fA-F]{1,6}|[0-9]{1,5})|[A-Za-z]{1,30});? *) token env tok

let map_erroneous_end_tag_name (env : env) (tok : CST.erroneous_end_tag_name) =
  (* erroneous_end_tag_name *) token env tok

let map_raw_text (env : env) (tok : CST.raw_text) =
  (* raw_text *) token env tok

let map_text (env : env) (tok : CST.text) =
  (* pattern [^<>&\s]([^<>&]*[^<>&\s])? *) token env tok

let map_pat_98d585a (env : env) (tok : CST.pat_98d585a) =
  (* pattern "[^\"]+" *) token env tok

let map_script_start_tag_name (env : env) (tok : CST.script_start_tag_name) =
  (* script_start_tag_name *) token env tok

let map_start_tag_name (env : env) (tok : CST.start_tag_name) =
  (* start_tag_name *) token env tok

let map_doctype (env : env) (tok : CST.doctype) =
  (* pattern [Dd][Oo][Cc][Tt][Yy][Pp][Ee] *) token env tok

let map_end_tag_name (env : env) (tok : CST.end_tag_name) =
  (* end_tag_name *) token env tok

let map_attribute_name (env : env) (tok : CST.attribute_name) =
  (* pattern "[^<>\"'/=\\s]+" *) token env tok

let map_erroneous_end_tag (env : env) ((v1, v2, v3) : CST.erroneous_end_tag) =
  let v1 = (* "</" *) token env v1 in
  let v2 = (* erroneous_end_tag_name *) token env v2 in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_quoted_attribute_value (env : env) (x : CST.quoted_attribute_value) =
  (match x with
  | `SQUOT_opt_pat_58fbb2e_SQUOT (v1, v2, v3) -> R.Case ("SQUOT_opt_pat_58fbb2e_SQUOT",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_pat_58fbb2e env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `DQUOT_opt_pat_98d585a_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_opt_pat_98d585a_DQUOT",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_pat_98d585a env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_doctype_ (env : env) ((v1, v2, v3, v4) : CST.doctype_) =
  let v1 = (* "<!" *) token env v1 in
  let v2 =
    (* pattern [Dd][Oo][Cc][Tt][Yy][Pp][Ee] *) token env v2
  in
  let v3 = map_pat_03aa317 env v3 in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_end_tag (env : env) (x : CST.end_tag) =
  (match x with
  | `Semg_end_tag (v1, v2, v3) -> R.Case ("Semg_end_tag",
      let v1 = (* "</" *) token env v1 in
      let v2 = (* semgrep_metavariable *) token env v2 in
      let v3 = (* ">" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LTSLASH_end_tag_name_GT (v1, v2, v3) -> R.Case ("LTSLASH_end_tag_name_GT",
      let v1 = (* "</" *) token env v1 in
      let v2 = (* end_tag_name *) token env v2 in
      let v3 = (* ">" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_anon_choice_attr_value_5986531 (env : env) (x : CST.anon_choice_attr_value_5986531) =
  (match x with
  | `Attr_value tok -> R.Case ("Attr_value",
      (* pattern "[^<>\"'=\\s]+" *) token env tok
    )
  | `Quoted_attr_value x -> R.Case ("Quoted_attr_value",
      map_quoted_attribute_value env x
    )
  )

let map_attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = (* pattern "[^<>\"'/=\\s]+" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_anon_choice_attr_value_5986531 env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_style_start_tag (env : env) ((v1, v2, v3, v4) : CST.style_start_tag) =
  let v1 = (* "<" *) token env v1 in
  let v2 = (* style_start_tag_name *) token env v2 in
  let v3 = R.List (List.map (map_attribute env) v3) in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_script_start_tag (env : env) ((v1, v2, v3, v4) : CST.script_start_tag) =
  let v1 = (* "<" *) token env v1 in
  let v2 = (* script_start_tag_name *) token env v2 in
  let v3 = R.List (List.map (map_attribute env) v3) in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_style_element (env : env) ((v1, v2, v3) : CST.style_element) =
  let v1 = map_style_start_tag env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* raw_text *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_end_tag env v3 in
  R.Tuple [v1; v2; v3]

let map_script_element (env : env) ((v1, v2, v3) : CST.script_element) =
  let v1 = map_script_start_tag env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* raw_text *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_end_tag env v3 in
  R.Tuple [v1; v2; v3]

let map_start_tag (env : env) (x : CST.start_tag) =
  (match x with
  | `Semg_start_tag (v1, v2, v3, v4) -> R.Case ("Semg_start_tag",
      let v1 = (* "<" *) token env v1 in
      let v2 = (* semgrep_metavariable *) token env v2 in
      let v3 = R.List (List.map (map_attribute env) v3) in
      let v4 = (* ">" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `LT_start_tag_name_rep_attr_GT (v1, v2, v3, v4) -> R.Case ("LT_start_tag_name_rep_attr_GT",
      let v1 = (* "<" *) token env v1 in
      let v2 = (* start_tag_name *) token env v2 in
      let v3 = R.List (List.map (map_attribute env) v3) in
      let v4 = (* ">" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let rec map_element (env : env) (x : CST.element) =
  (match x with
  | `Start_tag_rep_node_choice_end_tag (v1, v2, v3) -> R.Case ("Start_tag_rep_node_choice_end_tag",
      let v1 = map_start_tag env v1 in
      let v2 = R.List (List.map (map_node env) v2) in
      let v3 =
        (match v3 with
        | `End_tag x -> R.Case ("End_tag",
            map_end_tag env x
          )
        | `Impl_end_tag tok -> R.Case ("Impl_end_tag",
            (* implicit_end_tag *) token env tok
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Self_clos_tag (v1, v2, v3, v4) -> R.Case ("Self_clos_tag",
      let v1 = (* "<" *) token env v1 in
      let v2 = (* start_tag_name *) token env v2 in
      let v3 = R.List (List.map (map_attribute env) v3) in
      let v4 = (* "/>" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_node (env : env) (x : CST.node) =
  (match x with
  | `Doct_ x -> R.Case ("Doct_",
      map_doctype_ env x
    )
  | `Entity tok -> R.Case ("Entity",
      (* pattern &(#([xX][0-9a-fA-F]{1,6}|[0-9]{1,5})|[A-Za-z]{1,30});? *) token env tok
    )
  | `Text tok -> R.Case ("Text",
      (* pattern [^<>&\s]([^<>&]*[^<>&\s])? *) token env tok
    )
  | `Elem x -> R.Case ("Elem",
      map_element env x
    )
  | `Script_elem x -> R.Case ("Script_elem",
      map_script_element env x
    )
  | `Style_elem x -> R.Case ("Style_elem",
      map_style_element env x
    )
  | `Errons_end_tag x -> R.Case ("Errons_end_tag",
      map_erroneous_end_tag env x
    )
  )

let map_toplevel_node (env : env) (x : CST.toplevel_node) =
  (match x with
  | `Doct_ x -> R.Case ("Doct_",
      map_doctype_ env x
    )
  | `Elem x -> R.Case ("Elem",
      map_element env x
    )
  | `Script_elem x -> R.Case ("Script_elem",
      map_script_element env x
    )
  | `Style_elem x -> R.Case ("Style_elem",
      map_style_element env x
    )
  | `Errons_end_tag x -> R.Case ("Errons_end_tag",
      map_erroneous_end_tag env x
    )
  | `Xmld (v1, v2, v3) -> R.Case ("Xmld",
      let v1 = (* "<?xml" *) token env v1 in
      let v2 = R.List (List.map (map_attribute env) v2) in
      let v3 = (* "?>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_document (env : env) (x : CST.document) =
  (match x with
  | `Rep_topl_node xs -> R.Case ("Rep_topl_node",
      R.List (List.map (map_toplevel_node env) xs)
    )
  | `Topl_attr (v1, v2, v3) -> R.Case ("Topl_attr",
      let v1 = (* pattern "[^<>\"'/=\\s]+" *) token env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_anon_choice_attr_value_5986531 env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_document () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras

module Out = Semgrep_output_v1_j
module R = Range
module Set = Set_
module J = JSON
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO I experimented with using Graph_code to generate caller functions
      of the one found in the diff. Leaving this in if we later want to go
      in this direction

   (*****************************************************************************)
   (* Helpers *)
   (*****************************************************************************)

      (* less: could generalize again to other languages at some point *)
      let parse_program lang file =
        (* for codegraph purpose, we need the toplevel Assign turned into VarDef *)
        let assign_to_vardef = true in
        let ast = Parse_python.parse_program file in
        let ast = Python_to_generic.program ~assign_to_vardef ast in
        Naming_AST.resolve lang ast;
        ast

      let get_root_dir filename =
        let subdirs = String.split_on_char '/' filename in
        let res =
          match subdirs with
          | [] -> ""
          | [ x ] -> x
          | x :: y :: _ -> x ^ "/" ^ y
        in
        res

   (*****************************************************************************
   (* Build the graphs *)
   (*****************************************************************************)

      let build_graphs root lang files =
        logger#info "building the graphs";
        let xs = files |> List.map (fun file -> (file, parse_program lang file)) in
        let hooks = Graph_code_AST.default_hooks in
        let graph, _stats = Graph_code_AST.build ~root ~hooks lang xs in

        graph

   (*****************************************************************************)
   (* Check the graphs *)
   (*****************************************************************************)

      (* Similar to graph_code_checker.ml, but tuned for Python and our codebase *)
      let rec get_call_graph g callers fs =
        let get_parents f callers =
          let pred = G.mk_eff_use_pred g in
          let users = pred f in
          List.iter (fun (user, _) -> Common.pr2 user) users;
          let users =
            List.filter
              (fun x ->
                match x with
                | _, E.Function -> true
                | _ -> false)
              users
          in
          Set.union callers (Set.of_list users)
        in
        let callers' : G.node Set.t = List.fold_right get_parents fs callers in
        let set_diff = Set.diff callers callers' in
        if Set.is_empty set_diff then callers
        else get_call_graph g callers (Set.fold (fun e prev -> e :: prev) set_diff [])

   (*****************************************************************************)
   (* Entry point *)
   (*****************************************************************************)
      let get_caller_functions dir fs =
        (* less: we could generalize to other languages! like typescript *)
        let lang = Lang.Python in
        let files, _skipped =
          Find_target.files_of_dirs_or_files (Some lang) [ dir ]
        in
        let files, _skipped =
          Skip_code.filter_files_if_skip_list ~root:[ dir ] files
        in
        List.iter (fun file -> Common.pr2 file) files;
        logger#info "processing %d files" (List.length files);

        let g = build_graphs dir lang files in
        let f_nodes = List.map (fun f -> (f, E.Function)) fs in
        let caller_functions = get_call_graph g (Set.of_list f_nodes) f_nodes in
        Set.fold (fun (e, _) prev -> e :: prev) caller_functions [] *)

   ******************************************************************************)

(*****************************************************************************)
(* Generate pattern *)
(*****************************************************************************)

let range_of_ast ast = R.range_of_tokens (AST_generic_helpers.ii_of_any ast)

let pattern_from_diff (f : Out.diff_file) : JSON.t =
  let file = f.filename in
  let function_from_range file_ast range =
    let r = R.range_of_line_spec range file in
    let func = Range_to_AST.function_at_range r file_ast in
    match func with
    | None -> None
    | Some func ->
        let func_r =
          let r2_opt = range_of_ast func in
          match r2_opt with
          (* NoTokenLocation issue for the expression, should fix! *)
          | None -> failwith "No range found"
          | Some r2 -> r2
        in
        let func_str = R.content_at_range file func_r in
        Some func_str
  in
  let functions =
    try
      let file_ast = Parse_target.parse_program file in
      List_.filter_map (function_from_range file_ast) f.diffs
    with
    | _ -> []
  in
  (* old: we used to have this specified in semgrep_output_v1.atd
   * in a cve_result type, but better to keep semgrep_output_v1.atd
   * clean
   * { Out.url = f.In.url; filename = f.In.filename; funcnames = functions }
   *)
  J.Object
    [
      ("url", J.String f.url);
      ("filename", J.String !!(f.filename));
      ("funcnames", J.Array (functions |> List_.map (fun f -> J.String f)));
    ]

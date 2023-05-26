open Common
open File.Operators
module G = AST_generic
module RM = Range_with_metavars

module DataflowX = Dataflow_core.Make (struct
  type node = IL.node
  type edge = IL.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.IL.n
end)

let pr2_ranges file rwms =
  rwms
  |> List.iter (fun rwm ->
         let code_text = Range.content_at_range file rwm.RM.r in
         let line_str =
           let pm = rwm.RM.origin in
           let loc1, _ = pm.Pattern_match.range_loc in
           string_of_int loc1.Tok.pos.line
         in
         Common.pr2 (code_text ^ " @l." ^ line_str))

let test_tainting lang file options config def =
  Common.pr2 "\nDataflow";
  Common.pr2 "--------";
  let flow, mapping =
    Match_tainting_mode.check_fundef lang options config None def
  in
  let taint_to_str taint =
    let show_taint t =
      match t.Taint.orig with
      | Taint.Src src ->
          let tok1, tok2 = (fst (Taint.pm_of_trace src.call_trace)).range_loc in
          let r = Range.range_of_token_locations tok1 tok2 in
          Range.content_at_range file r
      | Taint.Arg arg -> Taint._show_arg arg
    in
    taint |> Taint.Taint_set.elements |> Common.map show_taint
    |> String.concat ", "
    |> fun str -> "{ " ^ str ^ " }"
  in
  DataflowX.display_mapping flow mapping (Taint_lval_env.to_string taint_to_str)

let test_dfg_tainting rules_file file =
  let rules_file = Fpath.v rules_file in
  let file = Fpath.v file in
  let lang = Lang.lang_of_filename_exn file in
  let rules =
    try Parse_rule.parse rules_file with
    | exn ->
        failwith
          (spf "fail to parse tainting rules %s (exn = %s)" !!rules_file
             (Common.exn_to_s exn))
  in
  let ast =
    try Parse_target.parse_and_resolve_name_warn_if_partial lang !!file with
    | exn ->
        failwith
          (spf "fail to parse %s (exn = %s)" !!file (Common.exn_to_s exn))
  in
  let rules =
    rules
    |> List.filter (fun r ->
           match r.Rule.languages with
           | Xlang.L (x, xs) -> List.mem lang (x :: xs)
           | _ -> false)
  in
  let _search_rules, taint_rules, _extract_rules, _join_rules =
    Rule.partition_rules rules
  in
  let rule = Common.hd_exn "unexpected empty list" taint_rules in
  pr2 "Tainting";
  pr2 "========";
  let handle_findings _ _ _ = () in
  let xconf = Match_env.default_xconfig in
  let xconf = Match_env.adjust_xconfig_with_rule_options xconf rule.options in
  (* this won't cache anything. but that's fine, we don't need it
     for test purposes.
  *)
  let tbl = Match_tainting_mode.mk_specialized_formula_cache [] in
  let config, debug_taint, _exps =
    Match_tainting_mode.taint_config_of_rule ~per_file_formula_cache:tbl xconf
      !!file (ast, []) rule handle_findings
  in
  Common.pr2 "\nSources";
  Common.pr2 "-------";
  pr2_ranges !!file (debug_taint.sources |> Common.map fst);
  Common.pr2 "\nSanitizers";
  Common.pr2 "----------";
  pr2_ranges !!file debug_taint.sanitizers;
  Common.pr2 "\nSinks";
  Common.pr2 "-----";
  pr2_ranges !!file (debug_taint.sinks |> Common.map fst);
  let v =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_function_definition env def =
        test_tainting lang !!file xconf.config config def;
        (* go into nested functions *)
        super#visit_function_definition env def
    end
  in
  (* Check each function definition. *)
  v#visit_program () ast

let actions () =
  [
    ( "-dfg_tainting",
      "<rules> <target>",
      Arg_helpers.mk_action_2_arg test_dfg_tainting );
  ]

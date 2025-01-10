open Common
open Fpath_.Operators
module G = AST_generic
module RM = Range_with_metavars

module DataflowX = Dataflow_core.Make (struct
  type node = IL.node
  type edge = IL.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.IL.n
end)

let pr2_ranges (file : Fpath.t) (rwms : RM.t list) : unit =
  rwms
  |> List.iter (fun rwm ->
         let code_text = Range.content_at_range file rwm.RM.r in
         let line_str =
           let pm = rwm.RM.origin in
           let loc1, _ = pm.range_loc in
           string_of_int loc1.Tok.pos.line
         in
         UCommon.pr2 (code_text ^ " @l." ^ line_str))

let test_tainting taint_inst def =
  UCommon.pr2 "\nDataflow";
  UCommon.pr2 "--------";
  let fcfg, _effects_IGNORED, mapping =
    Match_tainting_mode.check_fundef taint_inst None AST_to_IL.empty_ctx def
  in
  DataflowX.display_mapping fcfg.cfg mapping Taint_lval_env.to_string

let test_dfg_tainting rules_file file =
  let rules_file = Fpath.v rules_file in
  let file = Fpath.v file in
  let lang = Lang.lang_of_filename_exn file in
  let rules =
    match Parse_rule.parse rules_file with
    | Ok rules -> rules
    | Error e ->
        failwith
          (spf "fail to parse tainting rules in %s (error: %s)" !!rules_file
             (Rule_error.string_of_error e))
  in
  let ast =
    try Parse_target.parse_and_resolve_name_warn_if_partial lang file with
    | exn ->
        failwith
          (spf "fail to parse %s (exn = %s)" !!file (Common.exn_to_s exn))
  in
  let rules =
    rules
    |> List.filter (fun r ->
           match r.Rule.target_analyzer with
           | Analyzer.L (x, xs) -> List.mem lang (x :: xs)
           | _ -> false)
  in
  let _search_rules, taint_rules, _extract_rules, _join_rules =
    Rule.partition_rules rules
  in
  let rule = List_.hd_exn "unexpected empty list" taint_rules in
  UCommon.pr2 "Tainting";
  UCommon.pr2 "========";
  let xconf = Match_env.default_xconfig in
  let xconf = Match_env.adjust_xconfig_with_rule_options xconf rule.options in
  (* this won't cache anything. but that's fine, we don't need it
     for test purposes.
  *)
  let tbl = Formula_cache.mk_specialized_formula_cache [] in
  let taint_inst, spec_matches, _exps =
    Match_taint_spec.taint_config_of_rule ~per_file_formula_cache:tbl xconf lang
      file (ast, []) rule
  in
  UCommon.pr2 "\nSources";
  UCommon.pr2 "-------";
  pr2_ranges file (spec_matches.sources |> List_.map fst);
  UCommon.pr2 "\nSanitizers";
  UCommon.pr2 "----------";
  pr2_ranges file (spec_matches.sanitizers |> List_.map fst);
  UCommon.pr2 "\nSinks";
  UCommon.pr2 "-----";
  pr2_ranges file (spec_matches.sinks |> List_.map fst);
  let v =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_function_definition env def =
        test_tainting taint_inst def;
        (* go into nested functions *)
        super#visit_function_definition env def
    end
  in
  (* Check each function definition. *)
  v#visit_program () ast

let actions () =
  [
    ("-dfg_tainting", "<rules> <target>", Arg_.mk_action_2_arg test_dfg_tainting);
  ]

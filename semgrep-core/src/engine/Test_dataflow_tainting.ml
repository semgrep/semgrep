open Common
open AST_generic
module H = AST_generic_helpers
module G = AST_generic
module V = Visitor_AST
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
           string_of_int loc1.Parse_info.line
         in
         Common.pr2 (code_text ^ " @l." ^ line_str))

let test_tainting lang file config def =
  let xs = AST_to_IL.stmt lang (H.funcbody_to_stmt def.fbody) in
  let flow = CFG_build.cfg_of_stmts lang xs in

  Common.pr2 "\nDataflow";
  Common.pr2 "--------";
  let mapping = Dataflow_tainting.fixpoint config flow in
  DataflowX.display_mapping flow mapping (fun taint ->
      let show_taint t =
        match t.Taint.orig with
        | Taint.Src src ->
            let tok1, tok2 = (Taint.pm_of_trace src).range_loc in
            let r = Range.range_of_token_locations tok1 tok2 in
            Range.content_at_range file r
        | Taint.Arg i -> spf "arg %d" i
      in
      taint |> Taint.Taint_set.elements |> Common.map show_taint
      |> String.concat ", "
      |> fun str -> "{ " ^ str ^ " }")

let test_dfg_tainting rules_file file =
  let lang = List.hd (Lang.langs_of_filename file) in
  let rules =
    try Parse_rule.parse rules_file with
    | exn ->
        failwith
          (spf "fail to parse tainting rules %s (exn = %s)" rules_file
             (Common.exn_to_s exn))
  in
  let ast =
    try Parse_target.parse_and_resolve_name_warn_if_partial lang file with
    | exn ->
        failwith (spf "fail to parse %s (exn = %s)" file (Common.exn_to_s exn))
  in
  let rules =
    rules
    |> List.filter (fun r ->
           match r.Rule.languages with
           | Xlang.L (x, xs) -> List.mem lang (x :: xs)
           | _ -> false)
  in
  let _search_rules, taint_rules, _extract_rules = Rule.partition_rules rules in
  let rule = List.hd taint_rules in
  pr2 "Tainting";
  pr2 "========";
  let handle_findings _ _ _ = () in
  let config, debug_taint =
    Match_tainting_mode.taint_config_of_rule Config_semgrep.default_config []
      file (ast, []) rule handle_findings
  in
  Common.pr2 "\nSources";
  Common.pr2 "-------";
  pr2_ranges file debug_taint.sources;
  Common.pr2 "\nSanitizers";
  Common.pr2 "----------";
  pr2_ranges file debug_taint.sanitizers;
  Common.pr2 "\nSinks";
  Common.pr2 "-----";
  pr2_ranges file debug_taint.sinks;
  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (k, _v) def ->
            test_tainting lang file config def;
            (* go into nested functions *)
            k def);
      }
  in
  (* Check each function definition. *)
  v (AST_generic.Pr ast)

let actions () =
  [
    ( "-dfg_tainting",
      "<rules> <target>",
      Common.mk_action_2_arg test_dfg_tainting );
  ]

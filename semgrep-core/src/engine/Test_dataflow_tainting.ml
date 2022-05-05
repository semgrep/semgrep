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

let test_tainting lang file ast rule taint_spec def =
  let xs = AST_to_IL.stmt lang (H.funcbody_to_stmt def.fbody) in
  let flow = CFG_build.cfg_of_stmts xs in
  pr2 "Tainting";
  pr2 "========";
  let handle_findings _ _ _ = () in
  let config, debug_taint =
    Match_tainting_rules.taint_config_of_rule Config_semgrep.default_config []
      file (ast, []) rule taint_spec handle_findings
  in
  Common.pr2 "\nSources";
  Common.pr2 "-------";
  debug_taint.sources
  |> List.iter (fun rwm -> Common.pr2 (Range.content_at_range file rwm.RM.r));
  Common.pr2 "\nSanitizers";
  Common.pr2 "----------";
  debug_taint.sanitizers
  |> List.iter (fun rwm -> Common.pr2 (Range.content_at_range file rwm.RM.r));
  Common.pr2 "\nSinks";
  Common.pr2 "-----";
  debug_taint.sinks
  |> List.iter (fun rwm -> Common.pr2 (Range.content_at_range file rwm.RM.r));
  Common.pr2 "\nDataflow";
  Common.pr2 "--------";
  let mapping = Dataflow_tainting.fixpoint config flow in
  DataflowX.display_mapping flow mapping (fun taint ->
      let open Dataflow_tainting in
      let show_taint t =
        match t.orig with
        | Src src ->
            let tok1, tok2 = (pm_of_dm src).range_loc in
            let r = Range.range_of_token_locations tok1 tok2 in
            Range.content_at_range file r
        | Arg i -> spf "arg %d" i
      in
      taint |> Taint.elements |> Common.map show_taint |> String.concat ", "
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
    try
      let { Parse_target.ast; errors; _ } =
        Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file
      in
      if errors <> [] then pr2 (spf "WARNING: fail to fully parse %s" file);
      ast
    with
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
  let _search_rules, taint_rules = Rule.partition_rules rules in
  let rule, taint_spec = List.hd taint_rules in
  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (k, _v) def ->
            test_tainting lang file ast rule taint_spec def;
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

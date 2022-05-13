open Common
open AST_generic
module H = AST_generic_helpers

module DataflowX = Dataflow_core.Make (struct
  type node = IL.node
  type edge = IL.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.IL.n
end)

let test_dfg_tainting file =
  (* TODO: Run an actual taint rule! *)
  let ast = Parse_target.parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;
  ast
  |> List.iter (fun item ->
         match item.s with
         | DefStmt (_ent, FuncDef def) ->
             let xs = AST_to_IL.stmt lang (H.funcbody_to_stmt def.fbody) in
             let flow = CFG_build.cfg_of_stmts xs in
             pr2 "Tainting";
             let config =
               {
                 Dataflow_tainting.filepath = file;
                 rule_id = "test_dfg_tainting";
                 is_source = (fun _ -> []);
                 is_propa_from = (fun _ -> []);
                 is_propa_to = (fun _ -> []);
                 is_sink = (fun _ -> []);
                 is_sanitizer = (fun _ -> []);
                 unify_mvars = false;
                 handle_findings = (fun _ _ _ -> ());
               }
             in
             let mapping = Dataflow_tainting.fixpoint config flow in
             DataflowX.display_mapping flow mapping (fun _ ->
                 "<pattern matches>")
         | _ -> ())

let actions () =
  [ ("-dfg_tainting", " <file>", Common.mk_action_1_arg test_dfg_tainting) ]

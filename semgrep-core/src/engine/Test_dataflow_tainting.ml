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
  let ast = Parse_target.parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;
  let fun_env = Hashtbl.create 8 in
  ast
  |> List.iter (fun item ->
         match item.s with
         | DefStmt (ent, FuncDef def) ->
             let xs = AST_to_IL.stmt lang (H.funcbody_to_stmt def.fbody) in
             let flow = CFG_build.cfg_of_stmts xs in
             pr2 "Tainting";
             let config =
               {
                 Dataflow_tainting.is_source = (fun _ -> []);
                 is_sink = (fun _ -> []);
                 is_sanitizer = (fun _ -> []);
                 found_tainted_sink = (fun _ _ -> ());
               }
             in
             let opt_name = AST_to_IL.name_of_entity ent in
             let mapping =
               Dataflow_tainting.fixpoint config fun_env opt_name flow
             in
             DataflowX.display_mapping flow mapping (fun _ ->
                 "<pattern matches>")
         | _ -> ())

let actions () =
  [ ("-dfg_tainting", " <file>", Common.mk_action_1_arg test_dfg_tainting) ]

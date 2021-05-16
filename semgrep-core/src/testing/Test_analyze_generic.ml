(*s: pfff/lang_GENERIC/analyze/Test_analyze_generic.ml *)
open Common
open AST_generic
module V = Visitor_AST

let test_typing_generic file =
  let ast = Parse_target.parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;

  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (_k, _) def ->
            let s = AST_generic.show_any (S def.fbody) in
            pr2 s;
            pr2 "==>";

            let xs = AST_to_IL.stmt def.fbody in
            let s = IL.show_any (IL.Ss xs) in
            pr2 s);
      }
  in
  v (Pr ast)

(*s: function [[Test_analyze_generic.test_cfg_generic]] *)
let test_cfg_generic file =
  let ast = Parse_target.parse_program file in
  ast
  |> List.iter (fun item ->
         match item.s with
         | DefStmt (_ent, FuncDef def) -> (
             try
               let flow = Controlflow_build.cfg_of_func def in
               Controlflow.display_flow flow
             with Controlflow_build.Error err ->
               Controlflow_build.report_error err )
         | _ -> ())

(*e: function [[Test_analyze_generic.test_cfg_generic]] *)

module F = Controlflow

module DataflowX = Dataflow.Make (struct
  type node = F.node

  type edge = F.edge

  type flow = (node, edge) Ograph_extended.ograph_mutable

  let short_string_of_node = F.short_string_of_node
end)

(*s: function [[Test_analyze_generic.test_dfg_generic]] *)
let test_dfg_generic file =
  let ast = Parse_target.parse_program file in
  ast
  |> List.iter (fun item ->
         match item.s with
         | DefStmt (_ent, FuncDef def) ->
             let flow = Controlflow_build.cfg_of_func def in
             pr2 "Reaching definitions";
             let mapping = Dataflow_reaching.fixpoint flow in
             DataflowX.display_mapping flow mapping Dataflow.ns_to_str;
             pr2 "Liveness";
             let mapping = Dataflow_liveness.fixpoint flow in
             DataflowX.display_mapping flow mapping (fun () -> "()")
         | _ -> ())

(*e: function [[Test_analyze_generic.test_dfg_generic]] *)

(*s: function [[Test_analyze_generic.test_naming_generic]] *)
let test_naming_generic file =
  let ast = Parse_target.parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;
  let s = AST_generic.show_any (AST_generic.Pr ast) in
  pr2 s

(*e: function [[Test_analyze_generic.test_naming_generic]] *)

let test_constant_propagation file =
  let ast = Parse_target.parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate_basic lang ast;
  let s = AST_generic.show_any (AST_generic.Pr ast) in
  pr2 s

(*s: function [[Test_analyze_generic.test_il_generic]] *)
let test_il_generic file =
  let ast = Parse_target.parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;

  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (_k, _) def ->
            let s = AST_generic.show_any (S def.fbody) in
            pr2 s;
            pr2 "==>";

            let xs = AST_to_IL.stmt def.fbody in
            let s = IL.show_any (IL.Ss xs) in
            pr2 s);
      }
  in
  v (Pr ast)

(*e: function [[Test_analyze_generic.test_il_generic]] *)

(*s: function [[Test_analyze_generic.test_cfg_il]] *)
let test_cfg_il file =
  let ast = Parse_target.parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;

  ast
  |> List.iter (fun item ->
         match item.s with
         | DefStmt (_ent, FuncDef def) ->
             let xs = AST_to_IL.stmt def.fbody in
             let cfg = CFG_build.cfg_of_stmts xs in
             Display_IL.display_cfg cfg
         | _ -> ())

(*e: function [[Test_analyze_generic.test_cfg_il]] *)

module F2 = IL

module DataflowY = Dataflow.Make (struct
  type node = F2.node

  type edge = F2.edge

  type flow = (node, edge) Ograph_extended.ograph_mutable

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F2.n
end)

(*s: function [[Test_analyze_generic.test_dfg_tainting]] *)
let test_dfg_tainting file =
  let ast = Parse_target.parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;
  let fun_env = Hashtbl.create 8 in
  ast
  |> List.iter (fun item ->
         match item.s with
         | DefStmt (ent, FuncDef def) ->
             let xs = AST_to_IL.stmt def.fbody in
             let flow = CFG_build.cfg_of_stmts xs in
             pr2 "Tainting";
             let config =
               {
                 Dataflow_tainting.is_source = (fun _ -> false);
                 is_source_exp = (fun _ -> false);
                 is_sink = (fun _ -> false);
                 is_sanitizer = (fun _ -> false);
                 found_tainted_sink = (fun _ _ -> ());
               }
             in
             let opt_name = AST_to_IL.name_of_entity ent in
             let mapping =
               Dataflow_tainting.fixpoint config fun_env opt_name flow
             in
             DataflowY.display_mapping flow mapping (fun () -> "()")
         | _ -> ())

(*e: function [[Test_analyze_generic.test_dfg_tainting]] *)

let test_dfg_constness file =
  let ast = Parse_target.parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;
  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (_k, _) def ->
            let inputs, xs = AST_to_IL.function_definition def in
            let flow = CFG_build.cfg_of_stmts xs in
            pr2 "Constness";
            let mapping = Dataflow_constness.fixpoint inputs flow in
            Dataflow_constness.update_constness flow mapping;
            DataflowY.display_mapping flow mapping
              Dataflow_constness.string_of_constness;
            let s = AST_generic.show_any (S def.fbody) in
            pr2 s);
      }
  in
  v (Pr ast)

(*s: function [[Test_analyze_generic.actions]] *)
let actions () =
  [
    ("-typing_generic", " <file>", Common.mk_action_1_arg test_typing_generic);
    ("-cfg_generic", " <file>", Common.mk_action_1_arg test_cfg_generic);
    ("-dfg_generic", " <file>", Common.mk_action_1_arg test_dfg_generic);
    ("-naming_generic", " <file>", Common.mk_action_1_arg test_naming_generic);
    ( "-constant_propagation",
      " <file>",
      Common.mk_action_1_arg test_constant_propagation );
    ("-il_generic", " <file>", Common.mk_action_1_arg test_il_generic);
    ("-cfg_il", " <file>", Common.mk_action_1_arg test_cfg_il);
    ("-dfg_tainting", " <file>", Common.mk_action_1_arg test_dfg_tainting);
    ("-dfg_constness", " <file>", Common.mk_action_1_arg test_dfg_constness);
  ]

(*e: function [[Test_analyze_generic.actions]] *)
(*e: pfff/lang_GENERIC/analyze/Test_analyze_generic.ml *)

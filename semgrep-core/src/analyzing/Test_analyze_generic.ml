open Common
open AST_generic
module H = AST_generic_helpers
module V = Visitor_AST

let test_typing_generic ~parse_program file =
  let ast = parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;

  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (_k, _) def ->
            let body = H.funcbody_to_stmt def.fbody in
            let s = AST_generic.show_any (S body) in
            pr2 s;
            pr2 "==>";
            let xs = AST_to_IL.stmt lang body in
            let s = IL.show_any (IL.Ss xs) in
            pr2 s);
      }
  in
  v (Pr ast)

let test_constant_propagation ~parse_program file =
  let ast = parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate_basic lang ast;
  let s = AST_generic.show_any (AST_generic.Pr ast) in
  pr2 s

let test_il_generic ~parse_program file =
  let ast = parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;

  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (_k, _) def ->
            let body = H.funcbody_to_stmt def.fbody in
            let s = AST_generic.show_any (S body) in
            pr2 s;
            pr2 "==>";

            let xs = AST_to_IL.stmt lang body in
            let s = IL.show_any (IL.Ss xs) in
            pr2 s);
      }
  in
  v (Pr ast)

let test_cfg_il ~parse_program file =
  let ast = parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;

  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (_k, _) def ->
            let _, xs = AST_to_IL.function_definition lang def in
            let cfg = CFG_build.cfg_of_stmts lang xs in
            Display_IL.display_cfg cfg);
      }
  in
  v (Pr ast)

module F2 = IL

module DataflowY = Dataflow_core.Make (struct
  type node = F2.node
  type edge = F2.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F2.n
end)

let test_dfg_svalue ~parse_program file =
  let ast = parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;
  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kfunction_definition =
          (fun (_k, _) def ->
            let inputs, xs = AST_to_IL.function_definition lang def in
            let flow = CFG_build.cfg_of_stmts lang xs in
            pr2 "Constness";
            let mapping = Dataflow_svalue.fixpoint lang inputs flow in
            Dataflow_svalue.update_svalue flow mapping;
            DataflowY.display_mapping flow mapping
              (Pretty_print_AST.svalue_to_string lang);
            let s = AST_generic.show_any (S (H.funcbody_to_stmt def.fbody)) in
            pr2 s);
      }
  in
  v (Pr ast)

let actions ~parse_program =
  [
    ( "-typing_generic",
      " <file>",
      Common.mk_action_1_arg (test_typing_generic ~parse_program) );
    ( "-constant_propagation",
      " <file>",
      Common.mk_action_1_arg (test_constant_propagation ~parse_program) );
    ( "-il_generic",
      " <file>",
      Common.mk_action_1_arg (test_il_generic ~parse_program) );
    ("-cfg_il", " <file>", Common.mk_action_1_arg (test_cfg_il ~parse_program));
    ( "-dfg_svalue",
      " <file>",
      Common.mk_action_1_arg (test_dfg_svalue ~parse_program) );
  ]

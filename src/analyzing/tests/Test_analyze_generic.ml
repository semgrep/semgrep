open AST_generic
open Fpath_.Operators
module H = AST_generic_helpers

let test_typing_generic ~parse_program file =
  let file = Fpath.v file in
  let ast = parse_program !!file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;

  let v =
    object
      inherit [_] AST_generic.iter_no_id_info

      method! visit_function_definition _ def =
        let body = H.funcbody_to_stmt def.fbody in
        let s = AST_generic.show_any (S body) in
        UCommon.pr2 s;
        UCommon.pr2 "==>";
        let xs = AST_to_IL.stmt lang body in
        let s = IL.show_any (IL.Ss xs) in
        UCommon.pr2 s
    end
  in
  v#visit_program () ast

let test_constant_propagation ~parse_program file =
  let file = Fpath.v file in
  let ast = parse_program !!file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate_basic lang ast;
  let s = AST_generic.show_any (AST_generic.Pr ast) in
  UCommon.pr2 s

let test_il_generic ~parse_program file =
  let file = Fpath.v file in
  let ast = parse_program !!file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;

  let v =
    object
      inherit [_] AST_generic.iter_no_id_info

      method! visit_function_definition _ def =
        let body = H.funcbody_to_stmt def.fbody in
        let s = AST_generic.show_any (S body) in
        UCommon.pr2 s;
        UCommon.pr2 "==>";

        let xs = AST_to_IL.stmt lang body in
        let s = IL.show_any (IL.Ss xs) in
        UCommon.pr2 s
    end
  in
  v#visit_program () ast

let test_cfg_il ~parse_program file =
  let file = Fpath.v file in
  let ast = parse_program !!file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  Visit_function_defs.visit
    (fun _ fdef ->
      let CFG_build.{ fparams = _; fcfg } = CFG_build.cfg_of_fdef lang fdef in
      Display_IL.display_cfg fcfg)
    ast

module F2 = IL

module DataflowY = Dataflow_core.Make (struct
  type node = F2.node
  type edge = F2.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F2.n
end)

let test_dfg_svalue ~parse_program file =
  let file = Fpath.v file in
  let ast = parse_program !!file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  let v =
    object
      inherit [_] AST_generic.iter_no_id_info

      method! visit_function_definition _ def =
        let CFG_build.{ fparams = inputs; fcfg = flow } =
          CFG_build.cfg_of_fdef lang def
        in
        UCommon.pr2 "Constness";
        let mapping = Dataflow_svalue.fixpoint lang inputs flow in
        Dataflow_svalue.update_svalue flow mapping;
        DataflowY.display_mapping flow mapping
          (Dataflow_var_env.env_to_str (Pretty_print_AST.svalue_to_string lang));
        let s = AST_generic.show_any (S (H.funcbody_to_stmt def.fbody)) in
        UCommon.pr2 s
    end
  in
  v#visit_program () ast

let actions ~parse_program =
  [
    ( "-typing_generic",
      " <file>",
      Arg_.mk_action_1_arg (test_typing_generic ~parse_program) );
    ( "-constant_propagation",
      " <file>",
      Arg_.mk_action_1_arg (test_constant_propagation ~parse_program) );
    ( "-il_generic",
      " <file>",
      Arg_.mk_action_1_arg (test_il_generic ~parse_program) );
    ("-cfg_il", " <file>", Arg_.mk_action_1_arg (test_cfg_il ~parse_program));
    ( "-dfg_svalue",
      " <file>",
      Arg_.mk_action_1_arg (test_dfg_svalue ~parse_program) );
  ]

(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open AST_generic
module H = AST_generic_helpers

let test_typing_generic ~parse_program file =
  let ast = parse_program file in
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
  let ast = parse_program file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate_basic lang ast;
  let s = AST_generic.show_any (AST_generic.Pr ast) in
  UCommon.pr2 s

let test_il_generic ~parse_program file =
  let ast = parse_program file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  Implicit_return.mark_implicit_return lang ast;

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
  let ast = parse_program file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  Implicit_return.mark_implicit_return lang ast;
  let i = ref 0 in
  Visit_function_defs.visit
    (fun ent fdef ->
      let Fun_CFG.{ params = _; fdef = _; cfg; lambdas = _ } =
        CFG_build.cfg_of_gfdef lang fdef
      in
      Display_IL.display_cfg
        ~title:
          (match ent with
          | Some { name = EN (Id ((name, _), _)); _ }
          | Some { name = EN (IdQualified { name_last = (name, _), _; _ }); _ }
            ->
              name
          | _ ->
              let name = "func" ^ Int.to_string !i in
              incr i;
              name)
        cfg)
    ast

module F2 = IL

module DataflowY = Dataflow_core.Make (struct
  type node = F2.node
  type edge = F2.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node n
end)

let test_dfg_svalue ~parse_program file =
  let ast = parse_program file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  let v =
    object
      inherit [_] AST_generic.iter_no_id_info

      method! visit_function_definition _ def =
        let fun_cfg = CFG_build.cfg_of_gfdef lang def in
        UCommon.pr2 "Constness";
        let mapping = Dataflow_svalue.fixpoint lang fun_cfg in
        Dataflow_svalue.update_svalue fun_cfg.cfg mapping;
        DataflowY.display_mapping fun_cfg.cfg mapping
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
      Arg_.mk_action_1_conv Fpath.v (test_typing_generic ~parse_program) );
    ( "-constant_propagation",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v (test_constant_propagation ~parse_program)
    );
    ( "-il_generic",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v (test_il_generic ~parse_program) );
    ( "-cfg_il",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v (test_cfg_il ~parse_program) );
    ( "-dfg_svalue",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v (test_dfg_svalue ~parse_program) );
  ]

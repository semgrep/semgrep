open Common
module V = Visitor_AST
module G = AST_generic

let actions () =
  [
    ( "-dump_bbs",
      "dump bbs",
      Common.mk_action_1_arg (fun file ->
          let ast = Parse_target.parse_program file in
          let lang = List.hd (Lang.langs_of_filename file) in
          Naming_AST.resolve lang ast;
          let v =
            V.mk_visitor
              {
                V.default_visitor with
                V.kdef =
                  (fun (k, _) ((entity, kind) as def) ->
                    match kind with
                    | FuncDef fndef ->
                        let _, xs = AST_to_IL.function_definition lang fndef in
                        let cfg = CFG_build.cfg_of_stmts xs in
                        let bbs = Suggest.Util.cfg_to_bb cfg in
                        spf "Function: %s" (G.show_entity_name entity.name)
                        |> pr;
                        List.iter
                          (fun bb ->
                            print_string "bb:";
                            List.iter
                              (fun instr ->
                                spf " %s |"
                                  (Display_IL.short_string_of_node_kind
                                     (NInstr instr))
                                |> print_string)
                              bb;
                            print_endline "")
                          bbs;
                        k def
                        (*[%show: IL.instr list list] bbs |> pr*)
                    | _ -> k def);
              }
          in
          v (Pr ast)) );
    ( "-suggest",
      "suggest",
      Common.mk_action_n_arg (fun files -> Suggest.suggest files) );
  ]

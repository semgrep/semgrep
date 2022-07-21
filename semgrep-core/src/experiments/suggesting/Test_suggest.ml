open Common

let actions () =
  [
    ( "-dump_bbs",
      "dump bbs",
      Common.mk_action_1_arg (fun file ->
          let ast = Parse_target.parse_program file in
          let lang = List.hd (Lang.langs_of_filename file) in
          Naming_AST.resolve lang ast;
          let v =
            Visitor_AST.mk_visitor
              {
                Visitor_AST.default_visitor with
                Visitor_AST.kfunction_definition =
                  (fun (_k, _) def ->
                    let _, xs = AST_to_IL.function_definition lang def in
                    let cfg = CFG_build.cfg_of_stmts xs in
                    let bbs = Suggest.Util.cfg_to_bb cfg in
                    pr "Function:";
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
                      bbs
                    (*[%show: IL.instr list list] bbs |> pr*));
              }
          in
          v (Pr ast)) );
    ( "-suggest",
      "suggest",
      Common.mk_action_n_arg (fun files -> Suggest.suggest files) );
  ]

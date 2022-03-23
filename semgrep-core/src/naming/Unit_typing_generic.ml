open Common
module V = Visitor_AST
module G = AST_generic

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from _build/default/tests/ hence the '..'s below *)
let tests_path = "../../../tests"
let tests_path_typing = Filename.concat tests_path "OTHER/typing"

let tests parse_program parse_pattern =
  Testutil.pack_tests "typing_tests"
    [
      ( "test basic variable definitions java",
        fun () ->
          let file = Filename.concat tests_path_typing "VarDef.java" in
          try
            let ast = parse_program file in
            let lang = List.hd (Lang.langs_of_filename file) in
            Naming_AST.resolve lang ast;

            let v =
              V.mk_visitor
                {
                  V.default_visitor with
                  V.kexpr =
                    (fun (_k, _) exp ->
                      match exp.G.e with
                      | G.N (G.Id (_, { G.id_type; _ })) -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("String", _), _)); _ } ->
                              ()
                          | _ ->
                              Alcotest.fail
                                "Variable referenced did not have expected \
                                 type String")
                      | _ -> ());
                }
            in
            v (G.Pr ast)
          with
          | Parse_info.Parsing_error _ ->
              Alcotest.failf "it should correctly parse %s" file );
      ( "test multiple variable definitions java",
        fun () ->
          let file = Filename.concat tests_path_typing "EqVarCmp.java" in
          try
            let ast = parse_program file in
            let lang = List.hd (Lang.langs_of_filename file) in
            Naming_AST.resolve lang ast;

            let v =
              V.mk_visitor
                {
                  V.default_visitor with
                  V.kexpr =
                    (fun (_k, _) exp ->
                      match exp.G.e with
                      | G.Call (_, (_, [ x; y ], _)) -> (
                          (match x with
                          | G.Arg { e = G.N (G.Id (_, { G.id_type; _ })); _ }
                            -> (
                              match !id_type with
                              | Some { t = G.TyN (G.Id (("String", _), _)); _ }
                                ->
                                  ()
                              | _ ->
                                  Alcotest.fail
                                    "Variable 1 referenced did not have \
                                     expected type String")
                          | _ -> ());
                          match y with
                          | G.Arg { e = G.N (G.Id (_, { G.id_type; _ })); _ }
                            -> (
                              match !id_type with
                              | Some { t = G.TyN (Id (("int", _), _)); _ } -> ()
                              | _ ->
                                  Alcotest.fail
                                    "Variable 2 referenced did not have \
                                     expected type int")
                          | _ -> ())
                      | G.Assign
                          ({ e = G.N (G.Id (_, { G.id_type; _ })); _ }, _, _)
                        -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("String", _), _)); _ } ->
                              ()
                          | _ ->
                              Alcotest.fail
                                "Variable 1 referenced did not have expected \
                                 type String")
                      | _ -> ());
                }
            in
            v (G.Pr ast)
          with
          | Parse_info.Parsing_error _ ->
              Alcotest.failf "it should correctly parse %s" file );
      ( "test basic params java",
        fun () ->
          let file = Filename.concat tests_path_typing "BasicParam.java" in
          try
            let ast = parse_program file in
            let lang = List.hd (Lang.langs_of_filename file) in
            Naming_AST.resolve lang ast;

            let v =
              V.mk_visitor
                {
                  V.default_visitor with
                  V.kexpr =
                    (fun (_k, _) exp ->
                      match exp.G.e with
                      | G.Call (_, (_, [ x; y ], _)) -> (
                          (match x with
                          | G.Arg { e = G.N (G.Id (_, { G.id_type; _ })); _ }
                            -> (
                              match !id_type with
                              | Some { t = G.TyN (G.Id (("int", _), _)); _ } ->
                                  ()
                              | _ ->
                                  Alcotest.fail
                                    "Variable 1 referenced did not have \
                                     expected type String")
                          | _ -> ());
                          match y with
                          | G.Arg { e = G.N (G.Id (_, { G.id_type; _ })); _ }
                            -> (
                              match !id_type with
                              | Some { t = G.TyN (G.Id (("boolean", _), _)); _ }
                                ->
                                  ()
                              | _ ->
                                  Alcotest.fail
                                    "Variable 2 referenced did not have \
                                     expected type int")
                          | _ -> ())
                      | _ -> ());
                }
            in
            v (G.Pr ast)
          with
          | Parse_info.Parsing_error _ ->
              Alcotest.failf "it should correctly parse %s" file );
      ( "test class field types",
        fun () ->
          let file = Filename.concat tests_path_typing "ClassFields.java" in
          try
            let ast = parse_program file in
            let lang = List.hd (Lang.langs_of_filename file) in
            Naming_AST.resolve lang ast;

            let v =
              V.mk_visitor
                {
                  V.default_visitor with
                  V.kexpr =
                    (fun (_k, _) exp ->
                      match exp.G.e with
                      | G.N (G.Id (("age", _), { G.id_type; _ })) -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("int", _), _)); _ } -> ()
                          | _ ->
                              Alcotest.fail
                                "Variable referenced did not have expected \
                                 type int")
                      | G.N (G.Id (("default_age", _), { G.id_type; _ })) -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("int", _), _)); _ } -> ()
                          | _ ->
                              Alcotest.fail
                                "Variable referenced did not have expected \
                                 type int")
                      | _ -> ());
                }
            in
            v (G.Pr ast)
          with
          | Parse_info.Parsing_error _ ->
              Alcotest.failf "it should correctly parse %s" file );
      ( "java_pattern_files",
        fun () ->
          let dir = Filename.concat tests_path "java/semgrep" in
          let files = Common2.glob (spf "%s/*.sgrep" dir) in
          files
          |> List.iter (fun file ->
                 try
                   let _ = parse_pattern Lang.Java (Common.read_file file) in
                   ()
                 with
                 | Parse_info.Parsing_error _ ->
                     Alcotest.failf "it should correctly parse %s" file) );
      ( "go_pattern_files",
        fun () ->
          let dir = Filename.concat tests_path "go/semgrep" in
          let files = Common2.glob (spf "%s/*.sgrep" dir) in
          files
          |> List.iter (fun file ->
                 try
                   let _ = parse_pattern Lang.Go (Common.read_file file) in
                   ()
                 with
                 | Parse_info.Parsing_error _ ->
                     Alcotest.failf "it should correctly parse %s" file) );
      ( "test basic variable definitions go",
        fun () ->
          let file = Filename.concat tests_path_typing "StaticVarDef.go" in
          try
            let ast = parse_program file in
            let lang = List.hd (Lang.langs_of_filename file) in
            Naming_AST.resolve lang ast;

            let v =
              V.mk_visitor
                {
                  V.default_visitor with
                  V.kexpr =
                    (fun (_k, _) exp ->
                      match exp.G.e with
                      | G.N (G.Id (_, { G.id_type; _ })) -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("int", _), _)); _ } -> ()
                          | _ ->
                              Alcotest.fail
                                "Variable referenced did not have expected \
                                 type int")
                      | _ -> ());
                }
            in
            v (G.Pr ast)
          with
          | Parse_info.Parsing_error _ ->
              Alcotest.failf "it should correctly parse %s" file );
      ( "test basic function call go",
        fun () ->
          let file = Filename.concat tests_path_typing "FuncParam.go" in
          try
            let ast = parse_program file in
            let lang = List.hd (Lang.langs_of_filename file) in
            Naming_AST.resolve lang ast;

            let v =
              V.mk_visitor
                {
                  V.default_visitor with
                  V.kexpr =
                    (fun (_k, _) exp ->
                      match exp.G.e with
                      | G.Call (_, (_, [ x; y ], _)) -> (
                          (match x with
                          | G.Arg
                              { e = G.N (G.Id (("a", _), { G.id_type; _ })); _ }
                            -> (
                              match !id_type with
                              | Some { t = G.TyN (G.Id (("int", _), _)); _ } ->
                                  ()
                              | _ ->
                                  Alcotest.fail
                                    "Variable referenced did not have expected \
                                     type int")
                          | _ ->
                              Alcotest.fail
                                "Expected function call to be with int a as \
                                 first argument");
                          match y with
                          | G.Arg
                              { e = G.N (G.Id (("c", _), { G.id_type; _ })); _ }
                            -> (
                              match !id_type with
                              | Some { t = G.TyN (G.Id (("bool", _), _)); _ } ->
                                  ()
                              | _ ->
                                  Alcotest.fail
                                    "Variable referenced did not have expected \
                                     type bool")
                          | _ ->
                              Alcotest.fail
                                "Epected function call to have bool c as \
                                 second argument")
                      | _ -> ());
                }
            in
            v (G.Pr ast)
          with
          | Parse_info.Parsing_error _ ->
              Alcotest.failf "it should correctly parse %s" file );
      ( "test inferred variable definitions go",
        fun () ->
          let file = Filename.concat tests_path_typing "PropVarDef.go" in
          try
            let ast = parse_program file in
            let lang = List.hd (Lang.langs_of_filename file) in
            Naming_AST.resolve lang ast;

            let v =
              V.mk_visitor
                {
                  V.default_visitor with
                  V.kexpr =
                    (fun (_k, _) exp ->
                      match exp.G.e with
                      | G.N (G.Id (("a", _), { G.id_type; _ })) -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("char", _), _)); _ } -> ()
                          | _ ->
                              Alcotest.fail
                                "Variable referenced did not have expected \
                                 type char")
                      | G.N (G.Id (("b", _), { G.id_type; _ })) -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("int", _), _)); _ } -> ()
                          | _ ->
                              Alcotest.fail
                                "Variable referenced did not have expected \
                                 type int")
                      | G.N (G.Id (("c", _), { G.id_type; _ })) -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("char", _), _)); _ } -> ()
                          | _ ->
                              Alcotest.fail
                                "Variable referenced did not have expected \
                                 type char")
                      | _ -> ());
                }
            in
            v (G.Pr ast)
          with
          | Parse_info.Parsing_error _ ->
              Alcotest.failf "it should correctly parse %s" file );
    ]

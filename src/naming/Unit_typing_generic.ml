open Common
open Fpath_.Operators
module G = AST_generic

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"
let tests_path_typing = tests_path / "typing"

let tests parse_program parse_pattern =
  Testo.categorize "typing_tests"
    [
      t "test basic variable definitions java" (fun () ->
          let file = tests_path_typing / "VarDef.java" in
          try
            let ast = parse_program file in
            let lang = Lang.lang_of_filename_exn file in
            Naming_AST.resolve lang ast;

            let v =
              object (_self : 'self)
                inherit [_] AST_generic.iter_no_id_info

                method! visit_expr _ exp =
                  match exp.G.e with
                  | G.N (G.Id (_, { G.id_type; _ })) -> (
                      match !id_type with
                      | Some { t = G.TyN (G.Id (("String", _), _)); _ } -> ()
                      | _ ->
                          Alcotest.fail
                            "Variable referenced did not have expected type \
                             String")
                  | _ -> ()
              end
            in
            v#visit_program () ast
          with
          | Parsing_error.Syntax_error _ ->
              Alcotest.failf "it should correctly parse %s" !!file);
      t "test multiple variable definitions java" (fun () ->
          let file = tests_path_typing / "EqVarCmp.java" in
          try
            let ast = parse_program file in
            let lang = Lang.lang_of_filename_exn file in
            Naming_AST.resolve lang ast;

            let v =
              object (_self : 'self)
                inherit [_] AST_generic.iter_no_id_info

                method! visit_expr _ exp =
                  match exp.G.e with
                  | G.Call (_, (_, [ x; y ], _)) -> (
                      (match x with
                      | G.Arg { e = G.N (G.Id (_, { G.id_type; _ })); _ } -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("String", _), _)); _ } ->
                              ()
                          | _ ->
                              Alcotest.fail
                                "Variable 1 referenced did not have expected \
                                 type String")
                      | _ -> ());
                      match y with
                      | G.Arg { e = G.N (G.Id (_, { G.id_type; _ })); _ } -> (
                          match !id_type with
                          | Some { t = G.TyN (Id (("int", _), _)); _ } -> ()
                          | _ ->
                              Alcotest.fail
                                "Variable 2 referenced did not have expected \
                                 type int")
                      | _ -> ())
                  | G.Assign ({ e = G.N (G.Id (_, { G.id_type; _ })); _ }, _, _)
                    -> (
                      match !id_type with
                      | Some { t = G.TyN (G.Id (("String", _), _)); _ } -> ()
                      | _ ->
                          Alcotest.fail
                            "Variable 1 referenced did not have expected type \
                             String")
                  | _ -> ()
              end
            in
            v#visit_program () ast
          with
          | Parsing_error.Syntax_error _ ->
              Alcotest.failf "it should correctly parse %s" !!file);
      t "test basic params java" (fun () ->
          let file = tests_path_typing / "BasicParam.java" in
          try
            let ast = parse_program file in
            let lang = Lang.lang_of_filename_exn file in
            Naming_AST.resolve lang ast;

            let v =
              object (_self : 'self)
                inherit [_] AST_generic.iter_no_id_info

                method! visit_expr _ exp =
                  match exp.G.e with
                  | G.Call (_, (_, [ x; y ], _)) -> (
                      (match x with
                      | G.Arg { e = G.N (G.Id (_, { G.id_type; _ })); _ } -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("int", _), _)); _ } -> ()
                          | _ ->
                              Alcotest.fail
                                "Variable 1 referenced did not have expected \
                                 type String")
                      | _ -> ());
                      match y with
                      | G.Arg { e = G.N (G.Id (_, { G.id_type; _ })); _ } -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("boolean", _), _)); _ } ->
                              ()
                          | _ ->
                              Alcotest.fail
                                "Variable 2 referenced did not have expected \
                                 type int")
                      | _ -> ())
                  | _ -> ()
              end
            in
            v#visit_program () ast
          with
          | Parsing_error.Syntax_error _ ->
              Alcotest.failf "it should correctly parse %s" !!file);
      t "test class field types" (fun () ->
          let file = tests_path_typing / "ClassFields.java" in
          try
            let ast = parse_program file in
            let lang = Lang.lang_of_filename_exn file in
            Naming_AST.resolve lang ast;

            let v =
              object (_self : 'self)
                inherit [_] AST_generic.iter_no_id_info

                method! visit_expr _ exp =
                  match exp.G.e with
                  | G.N (G.Id (("age", _), { G.id_type; _ })) -> (
                      match !id_type with
                      | Some { t = G.TyN (G.Id (("int", _), _)); _ } -> ()
                      | _ ->
                          Alcotest.fail
                            "Variable referenced did not have expected type int"
                      )
                  | G.N (G.Id (("default_age", _), { G.id_type; _ })) -> (
                      match !id_type with
                      | Some { t = G.TyN (G.Id (("int", _), _)); _ } -> ()
                      | _ ->
                          Alcotest.fail
                            "Variable referenced did not have expected type int"
                      )
                  | _ -> ()
              end
            in
            v#visit_program () ast
          with
          | Parsing_error.Syntax_error _ ->
              Alcotest.failf "it should correctly parse %s" !!file);
      (* TODO?? why this is here? should be in Unit_parsing. ml *)
      t "java_pattern_files" (fun () ->
          let dir = tests_path / "parsing_patterns" / "java" in
          let files = Common2.glob (spf "%s/*.sgrep" !!dir) in
          files
          |> List.iter (fun file ->
                 try
                   let _ =
                     parse_pattern Lang.Java (UFile.Legacy.read_file file)
                   in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %s" file));
      t "go_pattern_files" (fun () ->
          let dir = tests_path / "parsing_patterns" / "go" in
          let files = Common2.glob (spf "%s/*.sgrep" !!dir) in
          files
          |> List.iter (fun file ->
                 try
                   let _ =
                     parse_pattern Lang.Go (UFile.Legacy.read_file file)
                   in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %s" file));
      t "test basic variable definitions go" (fun () ->
          let file = tests_path_typing / "StaticVarDef.go" in
          try
            let ast = parse_program file in
            let lang = Lang.lang_of_filename_exn file in
            Naming_AST.resolve lang ast;

            let v =
              object (_self : 'self)
                inherit [_] AST_generic.iter_no_id_info

                method! visit_expr _ exp =
                  match exp.G.e with
                  | G.N (G.Id (_, { G.id_type; _ })) -> (
                      match !id_type with
                      | Some { t = G.TyN (G.Id (("int", _), _)); _ } -> ()
                      | _ ->
                          Alcotest.fail
                            "Variable referenced did not have expected type int"
                      )
                  | _ -> ()
              end
            in
            v#visit_program () ast
          with
          | Parsing_error.Syntax_error _ ->
              Alcotest.failf "it should correctly parse %s" !!file);
      t "test basic function call go" (fun () ->
          let file = tests_path_typing / "FuncParam.go" in
          try
            let ast = parse_program file in
            let lang = Lang.lang_of_filename_exn file in
            Naming_AST.resolve lang ast;

            let v =
              object (_self : 'self)
                inherit [_] AST_generic.iter_no_id_info

                method! visit_expr _ exp =
                  match exp.G.e with
                  | G.Call (_, (_, [ x; y ], _)) -> (
                      (match x with
                      | G.Arg { e = G.N (G.Id (("a", _), { G.id_type; _ })); _ }
                        -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("int", _), _)); _ } -> ()
                          | _ ->
                              Alcotest.fail
                                "Variable referenced did not have expected \
                                 type int")
                      | _ ->
                          Alcotest.fail
                            "Expected function call to be with int a as first \
                             argument");
                      match y with
                      | G.Arg { e = G.N (G.Id (("c", _), { G.id_type; _ })); _ }
                        -> (
                          match !id_type with
                          | Some { t = G.TyN (G.Id (("bool", _), _)); _ } -> ()
                          | _ ->
                              Alcotest.fail
                                "Variable referenced did not have expected \
                                 type bool")
                      | _ ->
                          Alcotest.fail
                            "Epected function call to have bool c as second \
                             argument")
                  | _ -> ()
              end
            in
            v#visit_program () ast
          with
          | Parsing_error.Syntax_error _ ->
              Alcotest.failf "it should correctly parse %s" !!file);
      t "test inferred variable definitions go" (fun () ->
          let file = tests_path_typing / "PropVarDef.go" in
          try
            let ast = parse_program file in
            let lang = Lang.lang_of_filename_exn file in
            Naming_AST.resolve lang ast;

            let v =
              object (_self : 'self)
                inherit [_] AST_generic.iter_no_id_info

                method! visit_expr _ exp =
                  match exp.G.e with
                  | G.N (G.Id (("a", _), { G.id_type; _ })) -> (
                      match !id_type with
                      | Some { t = G.TyN (G.Id (("char", _), _)); _ } -> ()
                      | _ ->
                          Alcotest.fail
                            "Variable referenced did not have expected type \
                             char")
                  | G.N (G.Id (("b", _), { G.id_type; _ })) -> (
                      match !id_type with
                      | Some { t = G.TyN (G.Id (("int", _), _)); _ } -> ()
                      | _ ->
                          Alcotest.fail
                            "Variable referenced did not have expected type int"
                      )
                  | G.N (G.Id (("c", _), { G.id_type; _ })) -> (
                      match !id_type with
                      | Some { t = G.TyN (G.Id (("char", _), _)); _ } -> ()
                      | _ ->
                          Alcotest.fail
                            "Variable referenced did not have expected type \
                             char")
                  | _ -> ()
              end
            in
            v#visit_program () ast
          with
          | Parsing_error.Syntax_error _ ->
              Alcotest.failf "it should correctly parse %s" !!file);
    ]

open Common
open OUnit
module V = Visitor_AST
module A = AST_generic

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "typing_tests" >::: [

    "test basic variable definitions java" >:: (fun () ->
      let file = Config_pfff.tests_path "GENERIC/typing/VarDef.java" in
      try
        let ast = Parse_generic.parse_program file in
        let lang = List.hd (Lang.langs_of_filename file) in
        Naming_AST.resolve lang ast;

        let v = V.mk_visitor { V.default_visitor with
                               V.kexpr = (fun (_k, _) exp ->
                                 match exp with
                                 | A.Id(_, {A.id_type=id_type; _}) -> (
                                     match !id_type with
                                     | Some(A.TyId(("String", _), _)) -> ()
                                     | _ -> assert_failure("Variable referenced did not have expected type String"))
                                 | _ -> ()
                               );
                             } in
        v (A.Pr ast)
      with Parse_info.Parsing_error _ ->
        assert_failure (spf "it should correctly parse %s" file)
    );

    "test multiple variable definitions java" >:: (fun () ->
      let file = Config_pfff.tests_path "GENERIC/typing/EqVarCmp.java" in
      try
        let ast = Parse_generic.parse_program file in
        let lang = List.hd (Lang.langs_of_filename file) in
        Naming_AST.resolve lang ast;

        let v = V.mk_visitor { V.default_visitor with
                               V.kexpr = (fun (_k, _) exp ->
                                 match exp with
                                 | A.Call(_, (_,[x;y],_)) ->
                                     ((match x with
                                        | A.Arg(A.Id(_, {A.id_type=id_type; _})) -> (
                                            match !id_type with
                                            | Some(A.TyId(("String", _), _)) -> ()
                                            | _ -> assert_failure("Variable 1 referenced did not have expected type String"))
                                        | _ -> ());
                                      (match y with
                                       | A.Arg(A.Id(_, {A.id_type=id_type; _})) -> (
                                           match !id_type with
                                           | Some(A.TyBuiltin("int", _)) -> ()
                                           | _ -> assert_failure("Variable 2 referenced did not have expected type int"))
                                       | _ -> ()))
                                 | A.Assign(A.Id(_, {A.id_type=id_type; _}), _, _) -> (
                                     match !id_type with
                                     | Some(A.TyId(("String", _), _)) -> ()
                                     | _ -> assert_failure("Variable 1 referenced did not have expected type String"))
                                 | _ -> ()
                               );
                             } in
        v (A.Pr ast)
      with Parse_info.Parsing_error _ ->
        assert_failure (spf "it should correctly parse %s" file)
    );

    "test basic params java" >:: (fun () ->
      let file = Config_pfff.tests_path "GENERIC/typing/BasicParam.java" in
      try
        let ast = Parse_generic.parse_program file in
        let lang = List.hd (Lang.langs_of_filename file) in
        Naming_AST.resolve lang ast;

        let v = V.mk_visitor { V.default_visitor with
                               V.kexpr = (fun (_k, _) exp ->
                                 match exp with
                                 | A.Call(_, (_,[x;y],_)) ->
                                     ((match x with
                                        | A.Arg(A.Id(_, {A.id_type=id_type; _})) -> (
                                            match !id_type with
                                            | Some(A.TyBuiltin("int", _)) -> ()
                                            | _ -> assert_failure("Variable 1 referenced did not have expected type String"))
                                        | _ -> ());
                                      (match y with
                                       | A.Arg(A.Id(_, {A.id_type=id_type; _})) -> (
                                           match !id_type with
                                           | Some(A.TyBuiltin("boolean", _)) -> ()
                                           | _ -> assert_failure("Variable 2 referenced did not have expected type int"))
                                       | _ -> ()))
                                 | _ -> ()
                               );
                             } in
        v (A.Pr ast)
      with Parse_info.Parsing_error _ ->
        assert_failure (spf "it should correctly parse %s" file)
    );

    "test class field types" >:: (fun () ->
      let file = Config_pfff.tests_path "GENERIC/typing/ClassFields.java" in
      try
        let ast = Parse_generic.parse_program file in
        let lang = List.hd (Lang.langs_of_filename file) in
        Naming_AST.resolve lang ast;

        let v = V.mk_visitor { V.default_visitor with
                               V.kexpr = (fun (_k, _) exp ->
                                 match exp with
                                 | A.Id(("age", _), {A.id_type=id_type; _}) -> (
                                     match !id_type with
                                     | Some(A.TyBuiltin("int", _)) -> ()
                                     | _ -> assert_failure("Variable referenced did not have expected type int"))
                                 | A.Id(("default_age", _), {A.id_type=id_type; _}) -> (
                                     match !id_type with
                                     | Some(A.TyBuiltin("int", _)) -> ()
                                     | _ -> assert_failure("Variable referenced did not have expected type int"))
                                 | _ -> ()
                               );
                             } in
        v (A.Pr ast)
      with Parse_info.Parsing_error _ ->
        assert_failure (spf "it should correctly parse %s" file)
    );

    "java_pattern_files" >:: (fun () ->
      let dir = Config_pfff.tests_path "java/semgrep" in
      let files = Common2.glob (spf "%s/*.sgrep" dir) in
      files |> List.iter (fun file ->
        try
          let _ = Parse_generic.parse_pattern Lang.Java (Common.read_file file) in
          ()
        with Parse_info.Parsing_error _ ->
          assert_failure (spf "it should correctly parse %s" file)
      )
    );

    "go_pattern_files" >:: (fun () ->
      let dir = Config_pfff.tests_path "go/semgrep" in
      let files = Common2.glob (spf "%s/*.sgrep" dir) in
      files |> List.iter (fun file ->
        try
          let _ = Parse_generic.parse_pattern Lang.Go (Common.read_file file) in
          ()
        with Parse_info.Parsing_error _ ->
          assert_failure (spf "it should correctly parse %s" file)
      )
    );

    "test basic variable definitions go" >:: (fun () ->
      let file = Config_pfff.tests_path "GENERIC/typing/StaticVarDef.go" in
      try
        let ast = Parse_generic.parse_program file in
        let lang = List.hd (Lang.langs_of_filename file) in
        Naming_AST.resolve lang ast;

        let v = V.mk_visitor { V.default_visitor with
                               V.kexpr = (fun (_k, _) exp ->
                                 match exp with
                                 | A.Id(_, {A.id_type=id_type; _}) -> (
                                     match !id_type with
                                     | Some(A.TyId(("int", _), _)) -> ()
                                     | _ -> assert_failure("Variable referenced did not have expected type int"))
                                 | _ -> ()
                               );
                             } in
        v (A.Pr ast)
      with Parse_info.Parsing_error _ ->
        assert_failure (spf "it should correctly parse %s" file)
    );

    "test basic function call go" >:: (fun () ->
      let file = Config_pfff.tests_path "GENERIC/typing/FuncParam.go" in
      try
        let ast = Parse_generic.parse_program file in
        let lang = List.hd (Lang.langs_of_filename file) in
        Naming_AST.resolve lang ast;

        let v = V.mk_visitor { V.default_visitor with
                               V.kexpr = (fun (_k, _) exp ->
                                 match exp with
                                 | A.Call(_, (_,[x;y],_)) ->
                                     ((match x with
                                        | A.Arg(A.Id(("a", _), {A.id_type=id_type; _})) -> (
                                            match !id_type with
                                            | Some(A.TyId(("int", _), _)) -> ()
                                            | _ -> assert_failure("Variable referenced did not have expected type int"))
                                        | _ -> assert_failure("Expected function call to be with int a as first argument"));
                                      (match y with
                                       | A.Arg(A.Id(("c", _), {A.id_type=id_type; _})) -> (
                                           match !id_type with
                                           | Some(A.TyId(("bool", _), _)) -> ()
                                           | _ -> assert_failure("Variable referenced did not have expected type bool"))
                                       | _ -> assert_failure("Epected function call to have bool c as second argument")))
                                 | _ -> ()
                               );
                             } in
        v (A.Pr ast)
      with Parse_info.Parsing_error _ ->
        assert_failure (spf "it should correctly parse %s" file)
    );

    "test inferred variable definitions go" >:: (fun () ->
      let file = Config_pfff.tests_path "GENERIC/typing/PropVarDef.go" in
      try
        let ast = Parse_generic.parse_program file in
        let lang = List.hd (Lang.langs_of_filename file) in
        Naming_AST.resolve lang ast;

        let v = V.mk_visitor { V.default_visitor with
                               V.kexpr = (fun (_k, _) exp ->
                                 match exp with
                                 | A.Id(("a", _), {A.id_type=id_type; _}) -> (
                                     match !id_type with
                                     | Some(A.TyId(("char", _), _)) -> ()
                                     | _ -> assert_failure("Variable referenced did not have expected type char"))
                                 | A.Id(("b", _), {A.id_type=id_type; _}) -> (
                                     match !id_type with
                                     | Some(A.TyId(("int", _), _)) -> ()
                                     | _ -> assert_failure("Variable referenced did not have expected type int"))
                                 | A.Id(("c", _), {A.id_type=id_type; _}) -> (
                                     match !id_type with
                                     | Some(A.TyId(("char", _), _)) -> ()
                                     | _ -> assert_failure("Variable referenced did not have expected type char"))
                                 | _ -> ()
                               );
                             } in
        v (A.Pr ast)
      with Parse_info.Parsing_error _ ->
        assert_failure (spf "it should correctly parse %s" file)
    );
  ]

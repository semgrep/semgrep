open Common

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = "tests"

let tests caps =
  Testo.categorize "parsing_js"
    [
      t "regression files" (fun () ->
          let dir = Filename.concat tests_path "js/parsing" in
          let files =
            Common2.glob (spf "%s/*.js" dir)
            @ Common2.glob (spf "%s/jsx/*.js" dir)
            @ []
          in
          files
          |> List.iter (fun file ->
                 try
                   let _ = Parse_js.parse_program (Fpath.v file) in
                   ()
                 with
                 | Parsing_error.Syntax_error _
                 | Common.Todo ->
                     Alcotest.failf "it should correctly parse %s" file));
      t "regression files typescript" (fun () ->
          let dir = Filename.concat tests_path "typescript/parsing" in
          let files =
            Common2.glob (spf "%s/*.js" dir)
            @ Common2.glob (spf "%s/*.ts" dir)
            @ []
          in
          files
          |> List.iter (fun file ->
                 try
                   let _ = Parse_js.parse_program (Fpath.v file) in
                   ()
                 with
                 | Parsing_error.Syntax_error _
                 | Common.Todo ->
                     Alcotest.failf "it should correctly parse %s" file));
      t "rejecting bad code" (fun () ->
          try
            Common.save_excursion Flag_parsing.show_parsing_error false
              (fun () ->
                let _ = Parse_js.program_of_string caps "echo 1+" in
                Alcotest.fail "it should have thrown a Parse_error exception")
          with
          | Parsing_error.Syntax_error _ -> ())
      (*
    "the javascript AST mapper", (fun () ->
      let js_ex = "foo(42, 101);" in
      Common2.with_tmp_file ~str:js_ex ~ext:".js" (fun file ->
        let prog = Parse_js.parse_program file in
        let map_visitor = M.mk_visitor { M.default_visitor with
          M.kexpr = (fun (k, _) x ->
            match x with
            | L (Num (s, tok)) ->
                let i = s_to_i s in
                L (Num (i_to_s (i+1), Ast.fakeInfo()))
            | _ -> k x
          );
        }
        in
        let transformed_ast = map_visitor (Program prog) in

        let integers =
          V.do_visit_with_ref (fun aref -> { V.default_visitor with
            V.kexpr = (fun (k, _) x ->
              match x with
              | L (Num (s, tok)) ->
                  Common.push2 (s_to_i s) aref
              | _ -> k x
            );
          }) transformed_ast in
        assert_equal
          ~msg:"it should increment all the integers in the program"
          [43; 102] integers;
      )
    );
*);
    ]

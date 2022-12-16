open Common

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let tests =
  Testutil.pack_tests "parsing_ml" [

    "regression files", (fun () ->
      let dir = Config_pfff.tests_path "ml/parsing" in
      let files = Common2.glob (spf "%s/*.ml" dir) in
      files |> List.iter (fun file ->
        try
          let _ = Parse_ml.parse_program file in
          ()
        with Parse_info.Parsing_error _ ->
          Alcotest.failf "it should correctly parse %s" file
      )
    );

    (* Check that the visitor implementation correctly visit all AST
     * subelements, even when they are deep inside the AST tree (e.g.
     * sub-sub expressions inside parenthesis).
    *)
    "visitor", (fun () ->
      Common2.with_tmp_file ~ext:".ml" ~str:
        "open Foo1
module A = Foo2
"      (fun file ->
         let _ast = Parse_ml.parse_program file in
         let _cnt = ref 0 in
         (* TODO use Visitor_AST of ml_to_generic
                  let visitor = V.mk_visitor { V.default_visitor with
                    V.kmodule_expr = (fun (k, _) x ->
                      (match x with
                      | ModuleName (_qu, _name) ->
                          incr cnt
                      | _ -> ()
                      );
                      k x
                    );
                    V.kitem = (fun (k, _) x ->
                      (match x with
                      | Open (_tok, (_qu, _name)) ->
                          incr cnt;
                      | _ -> ()
                      );
                      k x
                    );
                    V.kqualifier = (fun (_k, _) xs ->
                      (match xs with
                      | [Name (s, _), _tok] ->
                          pr2 s;
                          incr cnt;
                      | _ ->
                          ()
                      );
                    );
                  }
                  in
                  visitor (Program ast);
                  assert_equal 2 !cnt;
         *)
         ()
       )
    );
  ]

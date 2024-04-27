open Common

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = "tests"

let tests (caps : < Cap.tmp >) =
  Testo.categorize "parsing_ml"
    [
      t "regression files" (fun () ->
          let dir = Filename.concat tests_path "ml/parsing" in
          let files = Common2.glob (spf "%s/*.ml" dir) in
          files
          |> List.iter (fun file ->
                 try
                   let _ = Parse_ml.parse_program (Fpath.v file) in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %s" file));
      (* Check that the visitor implementation correctly visit all AST
       * subelements, even when they are deep inside the AST tree (e.g.
       * sub-sub expressions inside parenthesis).
       *)
      t "visitor" (fun () ->
          CapTmp.with_temp_file caps#tmp ~suffix:".ml"
            ~contents:"open Foo1\nmodule A = Foo2\n" (fun file ->
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
              ()));
    ]

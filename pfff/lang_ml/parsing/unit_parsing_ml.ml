open Common
open OUnit

open Cst_ml
module V = Visitor_ml

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "parsing_ml" >::: [

    "regression files" >:: (fun () ->
      let dir = Filename.concat Config_pfff.path "/tests/ml/parsing" in
      let files = Common2.glob (spf "%s/*.ml" dir) in
      files |> List.iter (fun file ->
        try
          let _ = Parse_ml.parse_program file in
          ()
        with Parse_info.Parsing_error _ ->
          assert_failure (spf "it should correctly parse %s" file)
      )
    );

    (* Check that the visitor implementation correctly visit all AST 
     * subelements, even when they are deep inside the AST tree (e.g. 
     * sub-sub expressions inside parenthesis).
     *)
    "visitor" >:: (fun () ->
      Common2.with_tmp_file ~ext:".ml" ~str:
"open Foo1
module A = Foo2
"      (fun file ->
         let ast = Parse_ml.parse_program file in
         let cnt = ref 0 in
         let visitor = V.mk_visitor { V.default_visitor with
           V.kmodule_expr = (fun (k, _) x ->
             (match x with
             | ModuleName ((_qu, _name)) ->
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
         ()
      )
    );
  ]

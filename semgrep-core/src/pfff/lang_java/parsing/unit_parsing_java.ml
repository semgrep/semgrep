open Common

open Ast_java
module Vis = Visitor_java


(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let tests =
  Testutil.pack_tests "parsing_java" [

    "regression files", (fun () ->
      let dir = Config_pfff.tests_path "java/parsing" in
      let files = Common2.glob (spf "%s/*.java" dir) in
      files |> List.iter (fun file ->
        Testutil.run file (fun () -> Parse_java.parse file |> ignore)
      )
    );

    "basic_expr_visitor", (fun () ->
      let count = ref 0 in

      let vis = Vis.mk_visitor
          { Vis.default_visitor with
            Vis.kexpr = (fun (k,_) e -> match e with
              | NewClass _ -> incr count; k e
              | _ -> k e)
          } in

      let thecode = "
        class TestClass {

          public static final void main(string[] args) {
            Object x = new Runnable() {
                public void run() {
                  Object y = new Runnable() {
                      public void run() {
                        System.out.println(\"magic\");
                      }
                    };
                }
              };
          }

        }"
      in

      let ast = Common2.with_tmp_file
          ~str:thecode
          ~ext:"java"
          Parse_java.parse_program in

      vis (AProgram ast);

      Alcotest.(check int) "Expecting two anonymous class definitions"
        2 !count;

    );
  ]

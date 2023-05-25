open Common
open Ast_java
module Vis = Visitor_java

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = "tests"

let tests =
  Testutil.pack_tests "parsing_java"
    [
      ( "regression files",
        fun () ->
          let dir = Filename.concat tests_path "java/parsing" in
          let files = Common2.glob (spf "%s/*.java" dir) in
          files
          |> List.iter (fun file ->
                 Testutil.run file (fun () -> Parse_java.parse file |> ignore))
      );
      ( "basic_expr_visitor",
        fun () ->
          let count = ref 0 in

          let vis =
            Vis.mk_visitor
              {
                Vis.default_visitor with
                Vis.kexpr =
                  (fun (k, _) e ->
                    match e with
                    | NewClass _ ->
                        incr count;
                        k e
                    | _ -> k e);
              }
          in

          let thecode =
            "\n\
            \        class TestClass {\n\n\
            \          public static final void main(string[] args) {\n\
            \            Object x = new Runnable() {\n\
            \                public void run() {\n\
            \                  Object y = new Runnable() {\n\
            \                      public void run() {\n\
            \                        System.out.println(\"magic\");\n\
            \                      }\n\
            \                    };\n\
            \                }\n\
            \              };\n\
            \          }\n\n\
            \        }"
          in

          let ast =
            Common2.with_tmp_file ~str:thecode ~ext:"java"
              Parse_java.parse_program
          in

          vis (AProgram ast);

          Alcotest.(check int)
            "Expecting two anonymous class definitions" 2 !count );
    ]

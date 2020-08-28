open Common

open Ast_java
module Vis = Visitor_java

open OUnit

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "parsing_java" >::: [

    "regression files" >:: (fun () ->
      let dir = Filename.concat Config_pfff.path "/tests/java/parsing" in
      let files = Common2.glob (spf "%s/*.java" dir) in
      files |> List.iter (fun file ->
        try
          let _ = Parse_java.parse file in
          ()
        with Parse_info.Parsing_error _ ->
          assert_failure (spf "it should correctly parse %s" file)
      )
    );

    "basic_expr_visitor" >:: (fun () ->
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

      OUnit.assert_equal ~msg:"Expecting two anonymous class definitions"
        2 !count;

    );
  ]


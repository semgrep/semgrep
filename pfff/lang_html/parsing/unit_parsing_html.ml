open Common

open Ast_html
open OUnit

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "parsing_html" >::: [

    "lexing regular code" >:: (fun () ->
      ()
    );

    "parsing regular code" >:: (fun () ->
      ()
    );

    "html tree correctness" >:: (fun () ->

      let s = "<div>a</div><div>b</div>" in
      let ast = Parse_html.html_tree_of_string s in
      match ast with
      | Element (
          (Tag ("__root__", _)), [], 
          [
            Element (
              (Tag ("div", _)), [],
              _
            );
            Element (
              (Tag ("div", _)), [],
              _
            );
          ]
        ) -> ()
      | _ ->
          assert_failure (spf "wrong ast for %s, got %s"
                          s 
                          ((*Export_html.ml_pattern_string_of_html_tree*)
                            (Common.dump ast)))
    );
  ]

    


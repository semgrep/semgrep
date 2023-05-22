open Common

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from _build/default/tests/ hence the '..'s below *)
let tests_path = "../../../../tests"

let tests =
  Testutil.pack_tests "parsing_json"
    [
      ( "regression files",
        fun () ->
          let dir = Filename.concat tests_path "json/parsing" in
          let files = Common2.glob (spf "%s/*.json" dir) in
          files
          |> List.iter (fun file ->
                 try
                   let _ = Parse_json.parse_program file in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %s" file) );
    ]

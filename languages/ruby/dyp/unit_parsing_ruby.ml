open Common

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = "tests"

let tests =
  Testutil.pack_tests "parsing_ruby"
    [
      ( "regression files",
        fun () ->
          let dir = Filename.concat tests_path "ruby/parsing" in
          let files = Common2.glob (spf "%s/*.rb" dir) in
          files
          |> List.iter (fun file ->
                 try
                   let _ = Parse_ruby.parse_program file in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %s" file) );
    ]

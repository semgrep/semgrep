open Common

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = "tests"

let tests =
  Testo.categorize "parsing_json"
    [
      t "regression files" (fun () ->
          let dir = Filename.concat tests_path "json/parsing" in
          let files = Common2.glob (spf "%s/*.json" dir) in
          files
          |> List.iter (fun file ->
                 try
                   let _ = Parse_json.parse_program (Fpath.v file) in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %s" file));
    ]

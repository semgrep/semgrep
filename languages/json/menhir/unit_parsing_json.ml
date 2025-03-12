open Fpath_.Operators

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"

let tests =
  Testo.categorize "parsing_json"
    [
      t "regression files" (fun () ->
          let dir = tests_path / "json" / "parsing" in
          let files = Common2.glob (dir / "*.json") in
          files
          |> List.iter (fun file ->
                 try
                   let _ = Parse_json.parse_program file in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %a" Fpath.pp file));
    ]

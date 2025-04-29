open Fpath_.Operators

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"

let tests =
  Testo.categorize "parsing_go"
    [
      t "regression files" (fun () ->
          let dir = tests_path / "go" / "parsing" in
          let files = Common2.glob (dir / "*.go") in
          files
          |> List.iter (fun file ->
                 Testutil.run (Fpath.to_string file) (fun () ->
                     Parse_go.parse_program file |> ignore)));
    ]

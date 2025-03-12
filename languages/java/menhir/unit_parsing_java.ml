open Fpath_.Operators

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"

let tests =
  Testo.categorize "parsing_java"
    [
      t "regression files" (fun () ->
          let dir = tests_path / "java" / "parsing" in
          let files = Common2.glob (dir / "*.java") in
          files
          |> List.iter (fun file ->
                 Testutil.run (!!file) (fun () ->
                     Parse_java.parse file |> ignore)));
    ]

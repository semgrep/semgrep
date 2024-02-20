open Common

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = "tests"

let tests =
  Testo.categorize "parsing_java"
    [
      t "regression files" (fun () ->
          let dir = Filename.concat tests_path "java/parsing" in
          let files = Common2.glob (spf "%s/*.java" dir) in
          files
          |> List.iter (fun file ->
                 Testutil.run file (fun () ->
                     Parse_java.parse (Fpath.v file) |> ignore)));
    ]

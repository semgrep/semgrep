open Common

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
    ]

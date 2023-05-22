open Common

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from _build/default/tests/ hence the '..'s below *)
let tests_path = "../../../../tests"

let tests =
  Testutil.pack_tests "parsing_python"
    [
      ( "regression files",
        fun () ->
          let dir = Filename.concat tests_path "python/parsing" in
          let files = Common2.glob (spf "%s/*.py" dir) in
          files
          |> List.iter (fun file ->
                 Testutil.run file (fun () ->
                     Parse_python.parse_program file |> ignore)) );
    ]

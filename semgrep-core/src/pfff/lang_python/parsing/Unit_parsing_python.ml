open Common

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let tests =
  Testutil.pack_tests "parsing_python" [

    "regression files", (fun () ->
      let dir = Config_pfff.tests_path "python/parsing" in
      let files = Common2.glob (spf "%s/*.py" dir)in
      files |> List.iter (fun file ->
        Testutil.run file (fun () -> Parse_python.parse_program file |> ignore)
      )
    );
  ]

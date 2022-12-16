open Common

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let tests =
  Testutil.pack_tests "parsing_go" [

    "regression files", (fun () ->
      let dir = Config_pfff.tests_path "go/parsing" in
      let files = Common2.glob (spf "%s/*.go" dir)in
      files |> List.iter (fun file ->
        Testutil.run file (fun () -> Parse_go.parse_program file |> ignore)
      )
    );
  ]

open Common
open OUnit

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "parsing_json" >::: [

    "regression files" >:: (fun () ->
      let dir = Filename.concat Config_pfff.path "/tests/json/parsing" in
      let files = 
        Common2.glob (spf "%s/*.json" dir)
      in
      files |> List.iter (fun file ->
        try
          let _ = Parse_json.parse_program file in
          ()
        with Parse_info.Parsing_error _ ->
          assert_failure (spf "it should correctly parse %s" file)
      )
    );
]

open Common
open File.Operators
module Y = Yojson.Basic

let dir_pass =
  [ "tests/jsonnet/eval_pass"; "tests/jsonnet/tutorial_tests/pass" ]
  |> File.Path.of_strings

let related_file_of_target ~ext ~file =
  let dirname, basename, _e = Common2.dbe_of_filename !!file in
  let path = Common2.filename_of_dbe (dirname, basename, ext) in
  if Sys.file_exists path then Ok (Fpath.v path)
  else
    let msg =
      spf "could not find %s file for test '%s' in %s" ext basename dirname
    in
    Error msg
(* 
let test_maker dirs pass_or_fail =
  dirs
  |> Common.map (fun dir ->
         Common2.glob (spf "%s/*%s" !!dir "jsonnet")
         |> File.Path.of_strings
         |> Common.map (fun file ->
                ( Fpath.basename file,
                  fun () ->
                    let comparison_file_path =
                      match related_file_of_target ~ext:"json" ~file with
                      | Ok json_file -> json_file
                      | Error msg -> failwith msg
                    in
                    let correct =
                      Y.from_string (File.read_file comparison_file_path)
                    in

                    let ast = Parse_jsonnet.parse_program file in
                    let core = Desugar_jsonnet.desugar_program file ast in
                    (* Currently slightly hacky, since we later may want to test for errors thrown *)
                    try
                      let value_ = Eval_jsonnet_subst.eval_expr core in
                      let json =
                        JSON.to_yojson
                          (Eval_jsonnet_subst.manifest_value value_)
                      in
                      let fmt = format_of_string "expected %s \n but got %s" in
                      let result =
                        Printf.sprintf fmt (Y.to_string correct)
                          (Y.to_string json)
                      in

                      Alcotest.(check bool)
                        result pass_or_fail (Y.equal json correct)
                    with
                    | Eval_jsonnet_subst.Error _ ->
                        Alcotest.(check bool)
                          "this threw an error" (not pass_or_fail) true )))
  |> Common.flatten

let tests () = test_maker dir_pass true *)

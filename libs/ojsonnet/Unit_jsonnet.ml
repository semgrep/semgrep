open Common
open File.Operators
module Y = Yojson.Basic

let dir = Fpath.v "tests/jsonnet/eval"

let related_file_of_target ~ext ~file =
  let dirname, basename, _e = Common2.dbe_of_filename !!file in
  let path = Common2.filename_of_dbe (dirname, basename, ext) in
  if Sys.file_exists path then Ok (Fpath.v path)
  else
    let msg =
      spf "could not find %s file for test '%s' in %s" ext basename dirname
    in
    Error msg

let test_maker () =
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
             let value_ =
               Eval_jsonnet.eval_program core Value_jsonnet.empty_env
             in
             let json =
               JSON.to_yojson (Manifest_jsonnet.manifest_value value_)
             in
             let fmt = format_of_string "expected %s \n but got %s" in
             let result =
               Printf.sprintf fmt (Y.to_string correct) (Y.to_string json)
             in
             Alcotest.(check bool) result true (Y.equal json correct) ))

let tests () = test_maker ()

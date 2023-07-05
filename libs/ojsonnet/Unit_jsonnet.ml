open Common
open File.Operators

(*open Testutil*)
module Y = Yojson.Basic

let related_file_of_target ~ext ~file =
  let dirname, basename, _e = Common2.dbe_of_filename !!file in
  let path = Common2.filename_of_dbe (dirname, basename, ext) in
  if Sys.file_exists path then Ok (Fpath.v path)
  else
    let msg =
      spf "could not find %s file for test '%s' in %s" ext basename dirname
    in
    Error msg

(* Very hard coded for now, need to generalize*)
let files_list =
  print_string (Sys.getcwd ());
  Common2.glob "tests/jsonnet/eval"

(* CURRENTLY VERY BAD TESTING, need to be able to print out output
   *  rather than expected true/false, leaving this as is for now to just set up
   * tests to run, then will change*)
let test_maker files =
  files
  |> Common.map (fun file ->
         let file = Fpath.v file in
         ( Fpath.basename file,
           fun () ->
             let comparison_file_path =
               match related_file_of_target ~ext:"json" ~file with
               | Ok file -> file
               | Error msg -> failwith msg
             in
             let res = Y.from_string (File.read_file comparison_file_path) in

             let ast = Parse_jsonnet.parse_program file in
             let core = Desugar_jsonnet.desugar_program file ast in
             let value_ = Eval_jsonnet.eval_program core in
             let json =
               JSON.to_yojson (Manifest_jsonnet.manifest_value value_)
             in
             Alcotest.(check bool)
               "these should've been equal" (Y.equal json res) true ))

let tests () = test_maker files_list

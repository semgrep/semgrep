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

(* TODO not quite right, need to have a unit to unit not a unit to bool*)
let tests files =
  files
  |> Common.map (fun file ->
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
             Y.equal json res ))

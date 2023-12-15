open Common
open Fpath_.Operators
module Y = Yojson.Basic

let dir_pass = Fpath.v "tests/jsonnet/pass"
let dir_pass_tutorial = Fpath.v "tests/jsonnet/tutorial/pass"
let _dir_fail_tutorial = Fpath.v "tests/jsonnet/tutorial/fail"
let _dir_fail = Fpath.v "tests/jsonnet/fail"
let dir_error = Fpath.v "tests/jsonnet/errors"
let dir_error_tutorial = Fpath.v "tests/jsonnet/tutorial/errors"

let related_file_of_target ~ext ~file =
  let dirname, basename, _e = Filename_.dbe_of_filename !!file in
  let path = Filename_.filename_of_dbe (dirname, basename, ext) in
  if Sys.file_exists path then Ok (Fpath.v path)
  else
    let msg =
      spf "could not find %s file for test '%s' in %s" ext basename dirname
    in
    Error msg

let test_maker_err dir : Alcotest_ext.test list =
  Common2.glob (spf "%s/*%s" !!dir "jsonnet")
  |> Fpath_.of_strings
  |> List_.map (fun file ->
         ( Fpath.basename file,
           fun () ->
             let ast = Parse_jsonnet.parse_program file in
             let core = Desugar_jsonnet.desugar_program file ast in
             try
               let value_ = Eval_jsonnet.eval_program core in
               let _ = Eval_jsonnet.manifest_value value_ in
               Alcotest.(fail "this should have raised an error")
             with
             | Eval_jsonnet_common.Error _ ->
                 Alcotest.(check bool) "this raised an error" true true ))
  |> Alcotest_ext.pack_tests !!dir

let test_maker_pass_fail dir pass_or_fail strategy : Alcotest_ext.test list =
  Common2.glob (spf "%s/*%s" !!dir "jsonnet")
  |> Fpath_.of_strings
  |> List_.map (fun file ->
         ( Fpath.basename file,
           fun () ->
             let comparison_file_path =
               match related_file_of_target ~ext:"json" ~file with
               | Ok json_file -> json_file
               | Error msg -> failwith msg
             in
             let correct =
               Y.from_string (UFile.read_file comparison_file_path)
             in

             let ast = Parse_jsonnet.parse_program file in
             let core = Desugar_jsonnet.desugar_program file ast in
             (* Currently slightly hacky, since we later may want to test for errors thrown *)
             try
               let json =
                 Common.save_excursion Conf_ojsonnet.eval_strategy strategy
                   (fun () ->
                     let value_ = Eval_jsonnet.eval_program core in
                     JSON.to_yojson (Eval_jsonnet.manifest_value value_))
               in
               let fmt = format_of_string "expected %s \n but got %s" in
               let result =
                 Printf.sprintf fmt (Y.to_string correct) (Y.to_string json)
               in

               Alcotest.(check bool) result pass_or_fail (Y.equal json correct)
             with
             | Eval_jsonnet_common.Error _ ->
                 Alcotest.(check bool)
                   "this threw an error" (not pass_or_fail) true ))
  |> Alcotest_ext.pack_tests
       (spf "%s (%s)" !!dir (Conf_ojsonnet.show_eval_strategy strategy))

let tests () : Alcotest_ext.test list =
  Alcotest_ext.pack_suites "ojsonnet"
    [
      test_maker_pass_fail dir_pass true Conf_ojsonnet.EvalSubst;
      test_maker_pass_fail dir_pass_tutorial true Conf_ojsonnet.EvalSubst;
      (* TODO
           test_maker_pass_fail dir_fail false;
           test_maker_pass_fail dir_fail_tutorial false;
      *)
      test_maker_err dir_error;
      test_maker_err dir_error_tutorial;
    ]

open Common
open Fpath_.Operators
module Conf = Conf_ojsonnet
module Y = Yojson.Basic

let t = Testo.create
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

let test_maker_err dir : Testo.t list =
  Common2.glob (spf "%s/*%s" !!dir "jsonnet")
  |> Fpath_.of_strings
  |> List_.map (fun file ->
         t ~category:[ !!dir ] (Fpath.basename file) (fun () ->
             let ast = Parse_jsonnet.parse_program file in
             let core = Desugar_jsonnet.desugar_program file ast in
             try
               let value_ = Eval_jsonnet.eval_program core in
               let _ = Eval_jsonnet.manifest_value value_ in
               Alcotest.(fail "this should have raised an error")
             with
             | Eval_jsonnet_common.Error _ ->
                 Alcotest.(check bool) "this raised an error" true true))

let mk_tests (caps : < Cap.time_limit >) (subdir : string)
    (strategys : Conf.eval_strategy list) : Testo.t list =
  Common2.glob (spf "tests/jsonnet/%s/*.jsonnet" subdir)
  |> Fpath_.of_strings
  |> List_.map (fun file ->
         t
           ~category:[ spf "tests/jsonnet/%s" subdir ]
           (Fpath.basename file)
           (fun () ->
             let comparison_file_path =
               match related_file_of_target ~ext:"json" ~file with
               | Ok json_file -> json_file
               | Error msg -> failwith msg
             in
             let expected =
               Y.from_string (UFile.read_file comparison_file_path)
             in

             let ast = Parse_jsonnet.parse_program file in
             let core = Desugar_jsonnet.desugar_program file ast in
             strategys
             |> List.iter (fun strategy ->
                    let str_strategy = Conf.show_eval_strategy strategy in
                    try
                      (* TODO: make timeouts proportional to local host
                         capabilities or make them very high.
                         This used to run under 0.5s and suddenly started
                         to take over 1s on my machine without me touching
                         ojsonnet's code. *)
                      let timeout = 2.0 in
                      let json_opt =
                        Common.save_excursion Conf.eval_strategy strategy
                          (fun () ->
                            Time_limit.set_timeout caps
                              ~name:("ojsonnet-" ^ str_strategy) timeout
                              (fun () ->
                                let value_ = Eval_jsonnet.eval_program core in
                                JSON.to_yojson
                                  (Eval_jsonnet.manifest_value value_)))
                      in
                      match json_opt with
                      | None ->
                          failwith
                            (spf "%gs timeout on %s with %s" timeout !!file
                               str_strategy)
                      | Some json ->
                          if not (Y.equal json expected) then
                            failwith
                              (spf
                                 "mismatch for %s with strategy %s\n\
                                 \ expected %s but got %s" !!file str_strategy
                                 (Y.to_string expected) (Y.to_string json))
                    with
                    | Eval_jsonnet_common.Error _ ->
                        failwith
                          (spf "this threw an error with %s" str_strategy))))

let tests (caps : < Cap.time_limit >) : Testo.t list =
  Testo.categorize_suites "ojsonnet"
    [
      mk_tests caps "pass/" [ Conf.EvalSubst; Conf.EvalEnvir ];
      mk_tests caps "only_subst/" [ Conf.EvalSubst ];
      mk_tests caps "only_envir/" [ Conf.EvalEnvir ];
      mk_tests caps "tutorial/pass/" [ Conf.EvalSubst; Conf.EvalEnvir ];
      mk_tests caps "tutorial/only_subst/" [ Conf.EvalSubst ];
      mk_tests caps "tutorial/only_envir/" [ Conf.EvalEnvir ];
      (* TODO
           test_maker_pass_fail dir_fail false;
           test_maker_pass_fail dir_fail_tutorial false;
      *)
      test_maker_err dir_error;
      test_maker_err dir_error_tutorial;
    ]

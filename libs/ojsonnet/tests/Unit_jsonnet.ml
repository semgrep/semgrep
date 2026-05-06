(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open Common
open Fpath_.Operators
module Conf = Conf_ojsonnet
module Y = Yojson.Basic

let t = Testo.create
let _dir_fail_tutorial = Fpath.v "tests/jsonnet/tutorial/fail"
let _dir_fail = Fpath.v "tests/jsonnet/fail"
let dir_error = Fpath.v "tests/jsonnet/errors"
let dir_error_tutorial = Fpath.v "tests/jsonnet/tutorial/errors"
let dir_sandbox = Fpath.v "tests/jsonnet/sandbox"
let dir_dos = Fpath.v "tests/jsonnet/dos"

let related_file_of_target ~ext ~file =
  let dirname, basename, _e = Filename_.dbe_of_filename !!file in
  let path = Filename_.filename_of_dbe (dirname, basename, ext) in
  if Sys_.file_exists path then Ok (Fpath.v path)
  else
    let msg =
      spf "could not find %s file for test '%s' in %s" ext basename dirname
    in
    Error msg

let test_maker_err dir : Testo.t list =
  Common2.glob (dir / "*jsonnet")
  |> List.map (fun file ->
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

(* Each fixture in [dir] tries to read a file outside
   its own directory via [import]/[importstr]. Desugar must reject all of them
   with [Desugar_jsonnet.Error] before any file content is loaded.
 *)
let test_maker_sandbox dir : Testo.t list =
  Common2.glob (dir / "*jsonnet")
  |> List.map (fun file ->
      t ~category:[ !!dir ] (Fpath.basename file) (fun () ->
          let ast = Parse_jsonnet.parse_program file in
          try
            let _ = Desugar_jsonnet.desugar_program file ast in
            Alcotest.(fail "sandbox traversal should have been rejected")
          with
          | Desugar_jsonnet.Error _ -> ()))

(* Mirrors the production [Rule_fetching.mk_import_callback] yaml branch:
   given an import like 'foo.yml', sandbox-validate the resolved path before
   using it. Used to verify that yaml-handling callbacks can no longer skip
   the import-root check. *)
let yaml_handling_callback ~sandbox base str =
  if str =~ ".*\\.y[a]?ml$" then
    let final_path = sandbox (Fpath.v base // Fpath.v str) in
    let _ : Fpath.t = final_path in
    Some (AST_jsonnet.L (AST_jsonnet.Null (Tok.unsafe_fake_tok "")))
  else None

let test_yaml_callback_sandbox () =
  let file = Fpath.v "tests/jsonnet/sandbox/yaml_import_traversal.jsonnet" in
  let ast = Parse_jsonnet.parse_program file in
  try
    let _ =
      Desugar_jsonnet.desugar_program ~import_callback:yaml_handling_callback
        file ast
    in
    Alcotest.(fail "yaml import traversal should have been rejected")
  with
  | Desugar_jsonnet.Error _ -> ()

(* Each fixture in [dir] is a malicious jsonnet program designed to hang
   semgrep via unbounded recursion (either runtime function calls caught
   by the eval-depth limit, or circular imports caught by the desugar
   cycle detector). Either failure path is acceptable; the test fails
   only if neither check fires.  ref: ENGINE-2727.
 *)
let test_maker_dos dir : Testo.t list =
  Common2.glob (dir / "*jsonnet")
  |> List.map (fun file ->
      t ~category:[ !!dir ] (Fpath.basename file) (fun () ->
          let ast = Parse_jsonnet.parse_program file in
          try
            let core = Desugar_jsonnet.desugar_program file ast in
            let _ = Eval_jsonnet.eval_program core in
            Alcotest.(fail "DoS fixture should have hit a recursion limit")
          with
          | Desugar_jsonnet.Error _
          | Eval_jsonnet_common.Error _ ->
              ()))

let mk_tests (subdir : Fpath.t) (strategys : Conf.eval_strategy list) :
    Testo.t list =
  Common2.glob Fpath.(v "tests" / "jsonnet" // subdir / "*.jsonnet")
  |> List.map (fun file ->
      t
        ~category:[ !!(Fpath.v "tests/jsonnet" // subdir) ]
        (Fpath.basename file)
        (fun () ->
          let comparison_file_path =
            match related_file_of_target ~ext:"json" ~file with
            | Ok json_file -> json_file
            | Error msg -> failwith msg
          in
          let expected = Y.from_string (UFile.read_file comparison_file_path) in

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
                let timeout = 5.0 in
                let t1 = Unix.gettimeofday () in
                let json_opt =
                  Common.save_excursion Conf.eval_strategy strategy (fun () ->
                      Time_limit.set_timeout ~name:("ojsonnet-" ^ str_strategy)
                        timeout (fun () ->
                          let value_ = Eval_jsonnet.eval_program core in
                          JSON.to_yojson (Eval_jsonnet.manifest_value value_)))
                in
                match json_opt with
                | None ->
                    let t2 = Unix.gettimeofday () in
                    let dt = t2 -. t1 in
                    failwith
                      (spf
                         "%.3fs (%gs) timeout on %s with %s - sometimes \
                          happens when running the tests with excessive \
                          parallelism"
                         dt timeout !!file str_strategy)
                | Some json ->
                    if not (Y.equal json expected) then
                      failwith
                        (spf
                           "mismatch for %s with strategy %s\n\
                           \ expected %s but got %s"
                           !!file str_strategy (Y.to_string expected)
                           (Y.to_string json))
              with
              | Eval_jsonnet_common.Error _ ->
                  failwith (spf "this threw an error with %s" str_strategy))))

let tests : Testo.t list =
  Testo.categorize_suites "ojsonnet"
    [
      mk_tests (Fpath.v "pass/") [ Conf.EvalSubst; Conf.EvalEnvir ];
      mk_tests (Fpath.v "only_subst/") [ Conf.EvalSubst ];
      mk_tests (Fpath.v "only_envir/") [ Conf.EvalEnvir ];
      mk_tests (Fpath.v "tutorial/pass/") [ Conf.EvalSubst; Conf.EvalEnvir ];
      mk_tests (Fpath.v "tutorial/only_subst/") [ Conf.EvalSubst ];
      mk_tests (Fpath.v "tutorial/only_envir/") [ Conf.EvalEnvir ];
      (* TODO
           test_maker_pass_fail dir_fail false;
           test_maker_pass_fail dir_fail_tutorial false;
      *)
      test_maker_err dir_error;
      test_maker_err dir_error_tutorial;
      test_maker_sandbox dir_sandbox;
      [
        t
          ~category:[ "tests/jsonnet/sandbox" ]
          "yaml_callback_sandbox" test_yaml_callback_sandbox;
      ];
      test_maker_dos dir_dos;
    ]

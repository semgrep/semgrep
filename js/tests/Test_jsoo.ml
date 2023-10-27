(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
open Js_of_ocaml

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* skipped_tests is a list of test names and and optional indicies to skip *)
(* for example: *)
(* ("foo", []) will skip all tests with "foo" in the name *)
(* ("foo", [1; 3]) will skip test with "fool" in the name AND whose index are 1 or 3 *)
let skipped_tests =
  [
    (* TODO: investigate C++ test issues *)
    ("Cpp", []);
    ("C++", []);
    (* TODO: re-enable once we fix Julia build slowness *)
    ("Julia", []);
    (* TODO: re-enable once we fix Ruby build slowness *)
    ("Ruby", []);
    (* TODO: re-enable this when we fix the jsoo int overflow bug *)
    ("Go", [ 24 ]);
    (* TODO: investigate c_array_inits pattern parse error*)
    ("C", [ 0 ]);
  ]

(* Filter to skip tests *)
let test_filter ~name ~index =
  if
    List.filter
      (fun (language, indexes) ->
        Common.contains
          (String.lowercase_ascii name)
          (String.lowercase_ascii language)
        && (indexes = [] || List.exists (fun n2 -> n2 = index) indexes))
      skipped_tests
    <> []
  then `Skip
  else `Run

(* Stolen from Logs' logs_browser.ml *)
let _ =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter { Logs.report = Semgrep_node_js_shared.console_report };
  Js.export_all
    (object%js
       method init = Semgrep_js_shared.init_jsoo
       method getMountpoints = Semgrep_js_shared.get_jsoo_mountpoint ()
       method setParsePattern = Semgrep_js_shared.setParsePattern
       method setJustParseWithLang = Semgrep_js_shared.setJustParseWithLang
       method setJsonnetParser = Semgrep_js_shared.setJsonnetParser

       method run filter =
         let argv = [| "" |] in
         let argv =
           if filter <> "" then Array.append argv [| "-e"; filter |] else argv
         in
         let tests = [ Unit_LS.tests ] |> List.flatten in
         let lwt_tests = [ Test_LS_e2e.lwt_tests ] |> List.flatten in
         let _tests =
           Common.map
             (fun (name, f) ->
               let f () =
                 Semgrep_js_shared.wrap_with_js_error
                   ~hook:
                     (Some (fun () -> Firebug.console##log (Js.string name)))
                   f
               in
               (name, f))
             tests
         in
         let lwt_tests =
           Common.map
             (fun (name, f) ->
               let f () =
                 Semgrep_js_shared.wrap_with_js_error
                   ~hook:
                     (Some (fun () -> Firebug.console##log (Js.string name)))
                   f
               in
               (name, f))
             lwt_tests
         in
         (* let run () =
              Alcotest.run "semgrep-js"
                (Testutil.to_alcotest tests)
                ~and_exit:false ~argv ~filter:test_filter
            in *)
         let run_lwt () : unit Lwt.t =
           Alcotest_lwt.run "semgrep-js"
             (Testutil.to_alcotest_lwt lwt_tests)
             ~and_exit:false ~argv ~filter:test_filter
         in
         (* Semgrep_js_shared.wrap_with_js_error run; *)
         Semgrep_node_js_shared.promise_of_lwt
           (Semgrep_js_shared.wrap_with_js_error (fun () -> run_lwt))
    end)

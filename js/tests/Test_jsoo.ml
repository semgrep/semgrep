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
open Semgrep_js_shared

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* These tests are skipped as they are currently incredibly slow *)
(* Once LLVM merges a PR we will re-enable these *)
let filtered = [ "Cpp"; "C++"; "Julia"; "Ruby" ]

(* Filter to skip tests *)
let test_filter ~name ~index =
  ignore index;
  if List.filter (fun n -> Common.contains name n) filtered <> [] then `Skip
  else `Run (* Cpp has a weird error, and @brandon is still working on it soo *)

let _ =
  Js.export_all
    (object%js
       method init = init_jsoo
       method getMountpoints = get_jsoo_mountpoint ()
       method setParsePattern = setParsePattern
       method setJustParseWithLang = setJustParseWithLang
       method setJsonnetParser = setJsonnetParser

       method run filter =
         let argv = [| ""; "--verbose" |] in
         let argv =
           if filter <> "" then Array.append argv [| "-e"; filter |] else argv
         in
         let tests = [ Unit_parsing.tests () ] |> List.flatten in
         let errors = ref [] in
         let tests =
           Common.map
             (fun (name, f) ->
               let f () =
                 try f () with
                 | e ->
                     let e = Js_error.attach_js_backtrace e ~force:false in
                     let error = Option.get (Js_error.of_exn e) in
                     errors := error :: !errors;
                     raise e
               in
               (name, f))
             tests
         in
         (try
            Alcotest.run "semgrep-js"
              (Testutil.to_alcotest tests)
              ~and_exit:false ~argv ~filter:test_filter
          with
         | Alcotest.Test_error ->
             Printf.printf "Some tests failed, displaying:\n");
         !errors |> Array.of_list |> Js.array
    end)

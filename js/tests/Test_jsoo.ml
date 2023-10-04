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
  if
    List.filter
      (fun n ->
        Common.contains (String.lowercase_ascii name) (String.lowercase_ascii n))
      filtered
    <> []
  then `Skip
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
         let argv = [| "" |] in
         let argv =
           if filter <> "" then Array.append argv [| "-e"; filter |] else argv
         in
         let tests =
           [ Unit_parsing.tests (); Unit_engine.tests () ] |> List.flatten
         in
         let tests =
           Common.map
             (fun (name, f) ->
               let f () =
                 wrap_with_js_error
                   ~hook:
                     (Some (fun () -> Firebug.console##log (Js.string name)))
                   f
               in
               (name, f))
             tests
         in
         let run () =
           Alcotest.run "semgrep-js"
             (Testutil.to_alcotest tests)
             ~and_exit:false ~argv ~filter:test_filter
         in
         wrap_with_js_error run
    end)

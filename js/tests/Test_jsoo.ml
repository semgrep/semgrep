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

let filtered =
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
        && (indexes == [] || List.exists (fun n2 -> n2 == index) indexes))
      filtered
    <> []
  then `Skip
  else `Run

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
           [ Unit_parsing.tests (); Unit_engine.tests (); Unit_entropy.tests ]
           |> List.flatten
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

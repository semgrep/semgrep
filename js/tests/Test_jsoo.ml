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

(* Filter to skip tests *)
let test_filter ~name ~index =
  ignore index;
  if Common.contains name "Cpp" then `Skip
  else `Run (* Cpp has a weird error, and @brandon is still working on it soo *)

let _ =
  enable_debug_logging ();
  Js.export_all
    (object%js
       method init = init_jsoo
       method getMountpoints = get_jsoo_mountpoint ()

       method setParsePattern (func : jbool -> jstring -> jstring -> 'a) =
         Parse_pattern.parse_pattern_ref :=
           fun print_error lang pattern ->
             match lang with
             (* The Yaml and JSON parsers are embedded in the engine because it's a
                core component needed to parse rules *)
             | Lang.Yaml -> Yaml_to_generic.any pattern
             | _ ->
                 func (Js.bool print_error)
                   (Js.string (Lang.to_lowercase_alnum lang))
                   (Js.string pattern)

       method setJustParseWithLang
           (func : jstring -> jstring -> Parsing_result2.t) =
         Parse_target.just_parse_with_lang_ref :=
           fun lang filename ->
             match lang with
             (* The Yaml and JSON parsers are embedded in the engine because it's a
                core component needed to parse rules *)
             | Lang.Yaml ->
                 {
                   ast = Yaml_to_generic.program filename;
                   errors = [];
                   skipped_tokens = [];
                   inserted_tokens = [];
                   stat = Parsing_stat.default_stat filename;
                 }
             | _ ->
                 func
                   (Js.string (Lang.to_lowercase_alnum lang))
                   (Js.string filename)

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

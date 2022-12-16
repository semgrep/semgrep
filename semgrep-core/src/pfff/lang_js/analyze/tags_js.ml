(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2012 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)
open Common

module E = Entity_code
module Tags = Tags_file
module Annot = Annotation_js

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* quite similar to tags_php.ml and tags_ml.ml *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let tags_of_files_or_dirs ?(verbose=false) xs =
  let files = Lib_parsing_js.find_source_files_of_dir_or_files xs in

  files |> Console.progress ~show:verbose (fun k ->
    List.map (fun file ->
      k();

      let res =
        Common.save_excursion Flag_parsing.show_parsing_error false (fun ()->
          Common.save_excursion Flag_parsing.exn_when_lexical_error false(fun()->
            Common.save_excursion Flag_parsing.error_recovery true (fun ()->
              Parse_js.parse file
            )))
      in
      let filelines = Common2.cat_array file in

      (* many class idioms are recognized in Class_js *)
      let hcomplete_name_of_info =
        failwith "Class_pre_es6 in TODO_more"
        (* Class_pre_es6.extract_complete_name_of_info ast *)  in

      let tags_classes =
        hcomplete_name_of_info
        |> Common.hash_to_list
        |> List.map (fun (info, (entity_kind, str)) ->
          let str' =
            (* we standardize static vs member methods in class_js
             * for the light_db database building, but
             * for the TAGS file we need the actual javascript
             * way to call a method, that is Class.method,
             * not Class::method or Class->method as in the pfff standard
            *)
            match str with
            | _ when str =~ "\\(.*\\)::\\(.*\\)" ->
                let (c,m) = Common.matched2 str in
                spf "%s.%s" c m
            | _ when str =~ "\\(.*\\)->\\(.*\\)" ->
                let (c,m) = Common.matched2 str in
                spf "%s.%s" c m
            | _ -> str
          in
          let info' = Parse_info.rewrap_str str' info in
          Tags.tag_of_info filelines info' entity_kind
        )
      in

      (* the module idioms are contained in annotations *)
      let annots =
        Annotation_js.annotations_of_program_with_comments res in
      let tags_modules =
        annots |> Common.map_filter (function
          | (Annot.ProvidesModule m | Annot.ProvidesLegacy m), info ->
              let info' = Parse_info.rewrap_str m info in
              Some (Tags.tag_of_info filelines info' (E.Module))
          | _ -> None
        )
      in
      file, tags_classes @ tags_modules
    ))

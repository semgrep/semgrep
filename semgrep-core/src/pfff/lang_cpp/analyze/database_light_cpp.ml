(* Yoann Padioleau
 *
 * Copyright (C) 2010-2011 Facebook
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

module Db = Database_code
module HC = Highlight_code
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * We abuse the code highlighter to also compute the light database.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* pre: the info corresponds to a originTok, otherwise
 * file_of_info could give the location of the macro file
 * and filename_without_leading_path will not like it
*)
let mk_entity ~root ~hcomplete_name_of_info info categ =

  let s = PI.str_of_info info in
  (*pr2 (spf "mk_entity %s" s);*)

  let l = PI.line_of_info info in
  let c = PI.col_of_info info in

  let name = s in
  let fullname =
    try Hashtbl.find hcomplete_name_of_info info |> snd
    with Not_found -> ""
  in

  { Database_code.
    e_name = name;
    e_fullname = if fullname <> name then fullname else "";
    e_file = PI.file_of_info info |> Common.readable ~root;
    e_pos = { Common2.l = l; c };
    e_kind =
      Common2.some (Database_code.entity_kind_of_highlight_category_def categ);

    (* filled in step 2 *)
    e_number_external_users = 0;
    (* TODO *)
    e_good_examples_of_use = [];
    (* TODO *)
    e_properties = [];
  }


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let compute_database ?(verbose=false) files_or_dirs =

  (* when we want to merge this database with the db of another language
   * like PHP, the other database may use realpath for the path of the files
   * so we want to behave the same.
  *)
  let files_or_dirs = files_or_dirs |> List.map Common.fullpath in
  let root = Common2.common_prefix_of_files_or_dirs files_or_dirs in
  pr2 (spf "generating C/C++ db_light with root = %s" root);

  let files = Lib_parsing_cpp.find_source_files_of_dir_or_files files_or_dirs in
  let dirs = files |> List.map Filename.dirname |> Common2.uniq_eff in

  (* step1: collecting definitions *)
  if verbose then pr2 "phase 1: collecting definitions";

  let (hdefs: (string, Db.entity) Hashtbl.t) = Hashtbl.create 1001 in

  let (hdefs_pos: (Parse_info.t, bool) Hashtbl.t) = Hashtbl.create 1001 in

  files |> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k ();
      let res = Parse_cpp.parse file in

      let hcomplete_name_of_info =
        (*Class_js.extract_complete_name_of_info ast *)
        Hashtbl.create 101
      in

      res |> (fun {PI. ast; tokens = toks; _} ->
        let prefs = Highlight_code.default_highlighter_preferences in

        Highlight_cpp.visit_toplevel ~tag_hook:(fun info categ ->

          (* todo? could look at the info of the origintok of the expanded? *)
          if not (PI.is_origintok info) then ()
          else
            (* todo: use is_entity_def_category ? *)
            match categ with
            | HC.Entity (_, (HC.Def2 _)) ->
                Hashtbl.add hdefs_pos info true;
                let e = mk_entity ~root ~hcomplete_name_of_info
                    info categ
                in
                Hashtbl.add hdefs e.Db.e_name e;
            | _ -> ()
        )
          prefs
          (ast, toks)
        ;
      );
    ));

  (* step2: collecting uses *)
  if verbose then pr2 "\nphase 2: collecting uses";

  files |> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let { PI. ast; tokens = toks; stat = _ } = Parse_cpp.parse file in

      (* work by side effect on ast2 too *)
      Check_variables_cpp.check_and_annotate_program
        ast;

      (ast, toks) |> (fun (ast, toks) ->
        let prefs = Highlight_code.default_highlighter_preferences in

        Highlight_cpp.visit_toplevel ~tag_hook:(fun info categ ->
          if not (PI.is_origintok info) then ()
          else
            match categ with
            | HC.Entity (_, (HC.Use2 _))
            | HC.StructName (HC.Use)
              (*| HC.Method (HC.Use2 _) *)
              ->
                let s = PI.str_of_info info in
                Hashtbl.find_all hdefs s |> List.iter (fun entity ->
                  let file_entity = entity.Db.e_file in

                  (* todo: check corresponding entity_kind ? *)
                  if file_entity <> file &&
                     Db.entity_and_highlight_category_correpondance entity categ
                  then begin
                    entity.Db.e_number_external_users <-
                      entity.Db.e_number_external_users + 1;
                  end
                );

            | _ -> ()
        )
          prefs
          (ast, toks)
        ;
      );
      ()
    ));

  (* step3: adding cross reference information *)
  if verbose then pr2 "\nphase 3: last fixes";

  let entities_arr = Common.hash_to_list hdefs |> List.map snd |> Array.of_list
  in
  Db.adjust_method_or_field_external_users ~verbose entities_arr;

  let dirs = dirs |> List.map (fun s -> Common.readable ~root s) in
  let dirs = Db.alldirs_and_parent_dirs_of_relative_dirs dirs in

  { Db.
    root = root;
    dirs = dirs |> List.map (fun d -> d, 0); (* TODO *)
    files = files |> List.map (fun f -> Common.readable ~root f, 0); (* TODO *)
    entities = entities_arr;
  }

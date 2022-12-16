(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

module Flag = Flag_parsing
module Db = Database_code

module T = Parser_ml

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Obsolete file: see the -db_of_graph_code option of codegraph and
 * graph_code_cmt.ml for a more complete implementation.
 *
 * Light database building for OCaml code (mainly used by the codemap
 * semantic code visualizer). We currently abuse the code highlighter
 * to extract the entity defintions, and for the uses we are mainly using
 * the list of tokens ... not the AST.
 *
 * We build the full database in multiple steps as some
 * operations need the information computed globally by the
 * previous step:
 *
 * - collect all definitions and their files
 * - collect all uses, updating the count number of the
 *   corresponding entity (if it's used in a different file)
 *   as well as the entity->test_files_using_it hash.
 *
 * Currently many analysis are just lexical-based (yes I know, I am
 * ridiculous) so there is some ambiguity when we find a use such
 * as a function call. We don't always know to which precise entity
 * it corresponds to.To be precise would require to resolve module name.
 * Fortunately in my code I don't use 'open' that much and only use the
 * simple alias-module idiom which makes it tractable to
 * identify in practice to which entity a qualified function call refers to.
 *
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* poor's man id for now. It's quite close to the fullid we have in
 * database_php.ml.
*)
type entity_poor_id =
    Id of (Common.filename * Common2.filepos)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let is_pleac_file file =
  let file = String.lowercase_ascii file in
  file =~ ".*pleac*"

(* todo? quite pad specific ...
 * try detect when use OUnit ?
*)
let is_test_file file =
  let file = String.lowercase_ascii file in
  (file =~ ".*/test_" || file =~ ".*/unit_")

let is_test_or_pleac_file file =
  is_test_file file || is_pleac_file file



let entity_poor_id_of_entity e =
  Id (e.Db.e_file, e.Db.e_pos)

(* give a score per id and then sort and return top k *)
let rank_and_filter_examples_of_use ~root ids entities_arr =
  ids |> List.map (fun id ->
    let file = entities_arr.(id).Db.e_file in
    let file = Filename.concat root file in
    let size = Common2.filesize file in

    (* Low means better; so prefer small size and pleac files *)
    let score =
      size / (if is_pleac_file file then 4 else 1) in
    score, id
  )
  |> Common.sort_by_key_lowfirst
  |> List.map snd

let parse file =
  Common.save_excursion Flag.error_recovery true (fun () ->
    Common.save_excursion Flag.show_parsing_error false (fun () ->
      Parse_ml.parse file
    ))

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let compute_database ?(verbose=false) files_or_dirs =

  let root = Common2.common_prefix_of_files_or_dirs files_or_dirs in
  let root = Common2.chop_dirsymbol root in
  if verbose then pr2 (spf "generating ML db_light with root = %s" root);

  let files = Lib_parsing_ml.find_source_files_of_dir_or_files files_or_dirs in
  let dirs = files |> List.map Filename.dirname |> Common2.uniq_eff in

  (* PHASE 1: collecting definitions *)
  if verbose then pr2 (spf "PHASE 1: collecting definitions");

  let (hdefs: (string, Db.entity) Hashtbl.t) = Hashtbl.create 1001 in

  (* This is used later when one wants to get the first id of a file.
   *
   * This is just because in step2 when we are collecting uses we
   * don't know in which entity we currently are but we know in
   * which file we are and for the good_examples_of_use we really
   * just need to give one of the id in the (supposidely small) test_or_pleac
   * file.
   *
   * todo: once we have a real callgraph we will not need this anymore.
  *)
  let (hfile_to_entities: (filename, entity_poor_id) Hashtbl.t) =
    Hashtbl.create 1001 in

  files |> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let { Parse_info. ast = _; tokens = _toks; _ } =
        parse file
      in
      ()
    ));

  (* TODO highlighter no more in pfff
        (* this is quite similar to what we do in tags_ml.ml *)
        let prefs = Highlight_code.default_highlighter_preferences in

        Highlight_ml.visit_program
          ~lexer_based_tagger:true (* !! *)
          ~tag_hook:(fun info categ ->
            (* todo: use is_entity_def_category ? *)
            match categ with
            | HC.Entity (_, HC.Def2 _)
            | HC.FunctionDecl _
              ->
                let s = PI.str_of_info info in
                let l = PI.line_of_info info in
                let c = PI.col_of_info info in

                let file = Parse_info.file_of_info info |> Common.readable ~root
                in

                let module_name = Module_ml.module_name_of_filename file in

                let fullpath = Parse_info.file_of_info info in

                (* stuff in mli is ok only where there is no .ml, like
                 * for the externals/core/ stuff
                *)
                let (d,b,e) = Common2.dbe_of_filename fullpath in
                if e = "ml" ||
                   (e = "mli" && not (Sys.file_exists
                                        (Common2.filename_of_dbe (d,b, "ml"))))
                then begin

                  let entity = { Database_code.
                                 e_name = s;
                                 e_fullname = spf "%s.%s" module_name s;
                                 e_file = file;
                                 e_pos = { Common2.l = l; c };
                                 e_kind = Common2.some
                                     (Db.entity_kind_of_highlight_category_def categ);
                                 (* filled in step 2 *)
                                 e_number_external_users = 0;
                                 e_good_examples_of_use = [];

                                 (* TODO once we have a real parser, can at least
                                  * set the UseGlobal property.
                                 *)
                                 e_properties = [];
                               }
                  in
                  (* todo? could be more precise and add the Modulename.s
                   * in the hash so that we don't need to call
                   * Hashtbl.find_all but just Hashtbl.find later ?
                  *)
                  Hashtbl.add hdefs s entity;

                  Hashtbl.add hfile_to_entities file
                    (entity_poor_id_of_entity entity);
                end;

            | _ -> ()
          )
          prefs
          file
          (ast, toks)
      )
  *)

  (* PHASE 2: collecting uses *)
  if verbose then pr2 (spf "PHASE 2: collecting uses");

  let entities_arr =
    Common.hash_to_list hdefs |> List.map snd |> Array.of_list
  in

  (* this is useful when we want to add cross-references in the entities
   * such as the good_examples_of_use that reference another Db.entity_id.
  *)
  let (h_id_mldb_to_id_db: (entity_poor_id, Db.entity_id) Hashtbl.t) =
    Hashtbl.create 1001 in

  entities_arr |> Array.iteri (fun id_db e ->
    let id_mldb = entity_poor_id_of_entity e in
    Hashtbl.add h_id_mldb_to_id_db id_mldb id_db;
  );

  (* todo: could rank later.
   *  so would need a first phase where we collect with
   *   let (hentity_to_test_files_using_it:
   *    (entity_poor_id, Common.filename) Hashtbl.t) =
   *    Hashtbl.create 101 in
   *  ?
   *
   * For now the granularity of the goto_example is entity ->
   * test_files_using_it instead of test_functions_that_use_it
   * because we don't have the full callgraph and different
   * entities id as in database_php.ml. We could try to identify
   * in which entity a function call is by reusing the highlight/visitor
   * above and tracking the tokens and what was the last entity
   * encountered.
  *)
  let add_good_example_of_use test_file entity =
    let poor_id_opt = Common2.hfind_option test_file hfile_to_entities in
    (match poor_id_opt with
     | None -> pr2 (spf "WEIRD, could not find an entity in %s" test_file)
     | Some poor_id_user ->
         let id_user = Hashtbl.find h_id_mldb_to_id_db poor_id_user in
         (* could do a take_safe 3 but for ocaml I don't think we have
          * any scaling issues
         *)
         entity.Db.e_good_examples_of_use <-
           (id_user :: entity.Db.e_good_examples_of_use);
    )
  in


  files |> Console.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k ();

      if file =~ ".*external/" &&
         (* I don't really want pleac files to participate in the
          * e_number_external_users statistics but I want pleac files
          * to participate in the e_good_examples_of_use so have
          * to special case it here. Could introduce a step3 phase ...
         *)
         not (file =~ ".*pleac/")
      then pr2 (spf "skipping external file: %s" file)
      else begin

        let { Parse_info. tokens = toks; _} = parse file in

        let file = Common.readable ~root file in

        (* try to resolve function use more precisely instead of incrementing
         * all entities that have xxx as a name. Look if the module name
         * match the basename of the file defining the entity.
         * But have to remember the module X = XXX aliases.
        *)
        let hmodule_aliases = Hashtbl.create 11 in

        let toks = toks |> Common.exclude (function
          | T.TCommentSpace _ -> true
          | _ -> false
        )
        in

        (* Only consider Module.xxx. Otherwise names such as 'x', or 'yylex'
         * which are variables or internal functions are considered
         * as having a huge count.
         *
        *)
        let rec aux_toks toks =
          match toks with
          | T.Tmodule _
            ::T.TUpperIdent(s, _ii)
            ::T.TEq _
            ::T.TUpperIdent(s2, _ii2)::xs
            ->
              (* we want to transform every occurence of s  into s2,
               * to remove the alias sugar
              *)
              Hashtbl.add hmodule_aliases s s2;
              aux_toks xs

          | T.TUpperIdent(s, _ii)::T.TDot _ii2::T.TLowerIdent(s2, _ii3)::xs ->

              Hashtbl.find_all hdefs s2 |> List.iter (fun entity ->
                let file_entity = entity.Db.e_file in

                let final_module_name =
                  if Hashtbl.mem hmodule_aliases s
                  then Hashtbl.find hmodule_aliases s
                  else s
                in
                let module_entity =
                  let (_d,b,_e) = Common2.dbe_of_filename file_entity in
                  String.capitalize_ascii b
                in

                if file_entity <> file && final_module_name = module_entity
                then begin
                  entity.Db.e_number_external_users <-
                    entity.Db.e_number_external_users + 1;

                  if is_test_or_pleac_file file
                  then
                    add_good_example_of_use file entity;
                end
              );
              aux_toks xs

          | [] -> ()
          | _x::xs ->
              aux_toks xs
        in
        aux_toks toks;
      end
    )
  );

  (* PHASE 3: adjusting entities *)
  if verbose then pr2 (spf "PHASE 3: adjusting entities");

  entities_arr |> Array.iter (fun e ->
    let ids = e.Db.e_good_examples_of_use in
    e.Db.e_good_examples_of_use <-
      rank_and_filter_examples_of_use ~root ids entities_arr;
  );

  let dirs = dirs |> List.map (fun s -> Common.readable ~root s) in
  let dirs = Db.alldirs_and_parent_dirs_of_relative_dirs dirs in

  { Db.
    root = root;
    dirs = dirs |> List.map (fun d -> d, 0); (* TODO *)
    files = files |> List.map (fun f -> Common.readable ~root f, 0); (* TODO *)
    entities = entities_arr;
  }

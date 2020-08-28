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


(*
module Ast = Ast_php
module Db = Database_code
module E = Entity_php
module DbPHP = Database_php
module HC = Highlight_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* Light database building for PHP code (mainly used by the codemap
 * semantic code visualizer).
 * 
 * todo? get rid of database_php.ml too? use prolog instead as the starting
 * point?
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let mk_entity ~root id nb_users good_example_ids properties db =
  let l = DbPHP.line_of_id id db in
  let c = DbPHP.col_of_id id db in
  let name = DbPHP.name_of_id id db in
  let file = DbPHP.filename_of_id id db in
  let kind = DbPHP.kind_of_id id db in
  
  (* so will see in completion popup the possible method
   * for a class when start typing the class
   *)
  let fullname = DbPHP.complete_name_of_id id db in

  { Database_code.
    e_name = name;
    e_fullname = 
      if fullname <> name then fullname else "";

    e_file = Common.filename_without_leading_path root file;
    e_pos = { Common2.l = l; c };
    e_kind = kind;
    e_number_external_users = nb_users;
    e_good_examples_of_use = good_example_ids; 
    e_properties = properties;
  }      

let exclude_ids_same_file ids idfile db =
  ids +> Common.exclude (fun id2 ->
    let idfile2 = DbPHP.filename_of_id id2 db in
    (* less: could filter when only called from another dir ? *)
    idfile = idfile2
  )


let is_pleac_file file = 
  let file = Common2.lowercase file in
  file =~ ".*pleac*"

(* todo? should perhaps be a property in database_php.ml, so   (fun xs -> Common_extra.execute_and_show_progress2 
     ~show_progress:verbose (List.length xs) 
    (fun k -> 

 * don't have to put facebook specific stuff here ?
 *)
let is_test_file file =
  let file = Common2.lowercase file in
  (file =~ ".*test.php$") ||
  (file =~ ".*__tests__.*") ||
  (file =~ ".*tests/push-blocking/") ||
  (file =~ ".*tests/monitoring/") ||
  (file =~ ".*scripts/unittest/tests") ||
  false


let is_test_or_pleac_file file = 
  is_test_file file || is_pleac_file file


(* coupling: with phase 1 where we collect entities *)
let is_id_with_entity id db =
  match DbPHP.kind_of_id id db with
  | Db.Function 
  (* TODO  | Db.Method _ once leverage pathup? *)
  | Db.Class _ 
    -> true
  | _ -> false

(* the number of callers in the "example_of_use" should
 * be small to be a good example of use
 * notes: those are phpdb ids.
 *)
let rank_and_filter_examples_of_use ids db =
  ids +> List.map (fun id ->
    let file = DbPHP.filename_of_id id db in
    
    let nb_callees = List.length (DbPHP.callees_of_id id db) in

    (* Low means better; so prefer small size and pleac files.
     * TODO: when have test files from PHP manual, score ?
     *  could do: pleac first, then php manual example, then flib test,
     *  then other test files
     *)
    let score = 
      nb_callees / (if is_pleac_file file then 4 else 1) in
    score, id
  )
  +> Common.sort_by_key_lowfirst 
  +> List.map snd
  
let good_examples_of_use external_callers db = 
  let candidates = 
    external_callers +> List.filter (fun id ->
      let file = DbPHP.filename_of_id id db in
      is_test_or_pleac_file file &&
      (* can have toplevel statements as callers which later
       * will not have a id_db that we can refer to.
       *)
      is_id_with_entity id db 
    )
  in
  let candidates = rank_and_filter_examples_of_use candidates db in

  (* don't want to increase the size of the db so just take a few one *)
  Common.take_safe 3 candidates +> List.map (fun (Entity_php.Id i) -> i)
  
(*****************************************************************************)
(* Properties *)
(*****************************************************************************)

(* For function can look in AST if contains dynamic calls.
 * Also look for parameters passed by ref.
 *)
let properties_of_function_or_method id db =
  let id_ast = DbPHP.ast_of_id id db in

  let ps = ref [] in

  let params, body = 
    match id_ast with
    | Ast_php.FunctionE def | Ast_php.MethodE def ->
        (* TODO *)
        def.Ast.f_params +> Ast.unparen +> Ast.uncomma_dots, 
        def.Ast.f_body +> Ast.unbrace
    | _ -> 
        failwith "was expecting a Function or Method entity"
  in
  (* passed by ref *)
  params +> Common.index_list_0 +> List.iter (fun (p, i) ->
    if p.Ast.p_ref <> None
    then Common.push2 (Db.TakeArgNByRef i) ps;
  );

  (* call dynamic stuff 
   * todo: should be done via bottomup analysis and using dataflow information
   *  so even if people define lots of wrappers around the builtin
   *  dynamic function, then it does not matter.
   *)
  let calls = Lib_parsing_php.get_funcalls_any (Ast.StmtAndDefs body) in
  let dyncalls = 
    (* todo: what about call_user_func ? should be
     * transformed at parsing time into a FunCallVar ? use
     * a bottom-up analysis to find all those wrappers?
     * 
     * old: Lib_parsing_php.get_funcvars_any (Ast.StmtAndDefs body) 
     *)
     []
  in

  if not (null dyncalls) ||
     calls +> List.exists (fun s -> 
       Hashtbl.mem Env_php.hdynamic_call_wrappers s)
  then Common.push2 (Db.ContainDynamicCall) ps;

  (* dead function *)

  (* todo: reflection (also recursive), use global (also recursive),
   *  dead statements (may need to know exit-like functions),
   *  code coverage.
   *)

  !ps

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let database_code_from_php_database ?(verbose=false) db =

  let root = DbPHP.path_of_project_in_database db in
  if verbose then pr2 (spf "generating PHP db_light with root = %s" root);

  let files = DbPHP.all_files db in
  let dirs = files +> List.map Filename.dirname +> Common2.uniq_eff in

  (* phase 1: collecting entities and basic information *)
  if verbose then pr2 "phase 1: collecting entities";

  let entities = db.DbPHP.defs.DbPHP.id_kind#tolist in
  let entities =
    entities +> Common_extra.progress ~show:verbose (fun k ->
     Common.map_filter (fun (id, id_kind) ->
      k();

      (* coupling: with is_id_with_entity above *)
      match id_kind with
      | Db.Function (* | Db.Method _  TODO leverage pathup? *) ->
          let callers = DbPHP.callers_of_id id db
            +> List.map Callgraph_php.id_of_callerinfo in
          let idfile = DbPHP.filename_of_id id db in
          let external_callers = exclude_ids_same_file callers idfile db in

          (* note that this will return DbPHP ids, not Db ids. Later
           * once we got the full array, we will translate those DbPHP ids
           * into Db ids
           * 
           * Quite simple algorithm to find good examples of use:
           *  we just look at callers, filter the one in test file, then
           *  for each caller, look at number of callees, and take
           *  the one with best ratio.
           * 
           * coupling: note that the e_number_external_users is not that 
           * good for methods because of the approximation we currently do
           * in our class/method analysis. That's why we have another
           * phase later to adjust the method callers count.
           *)
          let good_ex_ids = good_examples_of_use external_callers db in
          let properties = properties_of_function_or_method id db in

          Some (id, mk_entity 
                   ~root id 
                   (List.length external_callers) good_ex_ids properties
                   db)

      | Db.Class _ -> 
          let users = DbPHP.class_users_of_id id db in
          let extenders = DbPHP.class_extenders_of_id id db in

(* TODO: interface? traits?
          let users = DbPHP.class_implementers_of_id id db in
*)
          let idfile = DbPHP.filename_of_id id db in
          let external_users = 
            exclude_ids_same_file (users ++ extenders) idfile db in

          let good_ex_ids = good_examples_of_use external_users db in
          let properties = [] in (* TODO *)

          Some (id, mk_entity 
                   ~root id 
                   (List.length external_users) good_ex_ids properties
                   db)
      | _ -> None
    ))
  in
  (* phase 2: adding the correct cross reference information *)
  if verbose then pr2 "\nphase 2: adding crossref information";

  let entities_arr = Array.of_list entities in

  let h_id_phpdb_to_id_db = Hashtbl.create 101 in
  entities_arr +> Array.iteri (fun id_db (id_phpdb, e) ->
    Hashtbl.add h_id_phpdb_to_id_db id_phpdb id_db;
  );
  let entities_arr =  entities_arr +> Array.map snd in
  let entities_arr =
    entities_arr +> Array.map (fun e ->
      let ids_phpdb = e.Db.e_good_examples_of_use in
      e.Db.e_good_examples_of_use <- 
        ids_phpdb +> List.map (fun id_phpdb ->
          try 
            Hashtbl.find h_id_phpdb_to_id_db (Entity_php.Id id_phpdb)
          with Not_found ->
            raise Not_found
        );
      e
    )
  in
  if verbose then pr2 "phase 3: last fixes";

  (* our current method/field analysis is imprecise; need to compensate back *)
  Db.adjust_method_or_field_external_users ~verbose entities_arr;

  let dirs = dirs +> List.map (fun s -> 
    Common.filename_without_leading_path root s) in
  let dirs = Db.alldirs_and_parent_dirs_of_relative_dirs dirs in

  (* Note that Database_code requires readable paths, hence the
   * filename_without_leading_path below
   *)
  { Database_code.
    root = root;

    dirs = dirs +> List.map (fun d -> 
      d
      , 0); (* TODO *)
    files = files +> List.map (fun f -> 
      Common.filename_without_leading_path root f
      , 0); (* TODO *)
    
    entities = entities_arr;
  }
*)

(*s: test_analyze_php.ml *)
open Common

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Simple AST *)
(*****************************************************************************)
(* mostly a copy paste of Test_parsing_php.parse_php *)
let test_parse_simple xs  =
  let xs = List.map Common.fullpath xs in
  let fullxs, _skipped_paths =
    Lib_parsing_php.find_source_files_of_dir_or_files xs
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let stat_list = ref [] in
  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k ();
    let { Parse_info. stat; ast = cst; _ } =
      Common.save_excursion Flag_parsing.error_recovery true (fun () ->
        Parse_php.parse file
      )
    in
    Common.push stat stat_list;
    if stat.Parse_info.error_line_count = 0
    then
      Error_code.try_with_print_exn_and_reraise file (fun () ->
        let _ast = Ast_php_build.program cst in
        ()
      )
  ));
  Parse_info.print_parsing_stat_list !stat_list;
  ()


let test_dump_simple file =
  Error_code.try_with_print_exn_and_reraise file (fun () ->
    let ast = Parse_php.parse_program file in
    let ast = Ast_php_build.program ast in
    let s = Ast_php.show_program ast in
    pr s
  )

(*
let test_pp_simple file =
  let cst = Parse_php.parse_program file in
  let ast = Ast_php_build.program cst in
  let s = Pretty_print_php_simple.string_of_program ast in
  pr s
*)

(*****************************************************************************)
(* Scope annotations *)
(*****************************************************************************)

(* Will annotate with Local or Param or Global the Var AST elements.
 * See also codemap which does the same and use different colors for
 * different scopes.
*)
(*
let test_scope_php file =
  let ast = Parse_php.parse_program file in
  (* Annotating variables requires a code database because
   * functions can take variables by reference without any
   * annotation at the call site (ugly language). Here
   * we pass an empty code database though.
   *)
  let entity_finder = None in
  Check_variables_php.check_and_annotate_program entity_finder ast;
  raise Todo
*)

(*****************************************************************************)
(* Typing *)
(*****************************************************************************)

(* bin/iphp *)
(*
let test_type_php file =
  let ast =
    Parse_php.parse_program file |> Ast_php_build.program
  in
  let env = { (Env_typing_php.make_env ()) with Env_typing_php.
    verbose = true;
    strict = true;
  } in
  Builtins_typed_php.make env;
  Typing_php.add_defs_code_database_and_update_dependencies env ast;

  Typing_php.infer_using_topological_sort_dependencies env;
  Typing_helpers_php.Print2.penv env;
  ()
*)

(*****************************************************************************)
(* CFG *)
(*****************************************************************************)

(*s: test_cfg_php *)
(*
let test_cfg_php file =
  let ast = Parse_php.parse_program file in
  ast |> List.iter (function
  | Cst_php.FuncDef def ->
      (try
        let flow = Controlflow_build_php.cfg_of_func def in
        Controlflow_php.display_flow flow;
      with Controlflow_build_php.Error err ->
        Controlflow_build_php.report_error err
      )
  | _ -> ()
  )
*)
(*e: test_cfg_php *)
(*
let test_dflow_php file =
  let dflow_of_func_def def =
    (try
       let flow = Controlflow_build_php.cfg_of_func def in
       let mapping = Dataflow_php.reaching_fixpoint flow in
       Dataflow_php.display_reaching_dflow flow mapping;
     with
     | Controlflow_build_php.Error err ->
       Controlflow_build_php.report_error err
     | Todo -> pr "skip: todo"
    )
  in
  let ast = Parse_php.parse_program file in
  ast |> List.iter (function
  | Cst_php.FuncDef def ->
    dflow_of_func_def def
  | Cst_php.ClassDef def ->
    Cst_php.unbrace def.Cst_php.c_body |> List.iter
      (function
      | Cst_php.Method def -> dflow_of_func_def def
      | _ -> ())
  | _ -> ()
  )
*)



(*s: test_cyclomatic_php *)
(*
let test_cyclomatic_php _file =
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in
  ast +> List.iter (function
  | Cst_php.FuncDef def ->
      let name = Cst_php.str_of_ident def.Cst_php.f_name in
      let n = Cyclomatic_php.cyclomatic_complexity_func ~verbose:true def in
      pr2 (spf "cyclomatic complexity for function %s is %d" name n);
  | Cst_php.ClassDef def ->
      let class_stmts = Cst_php.unbrace def.Cst_php.c_body in
      let class_name = Cst_php.str_of_ident def.Cst_php.c_name in
      class_stmts +> List.iter (function
      | Cst_php.Method def ->
          let name = Cst_php.str_of_ident def.Cst_php.f_name in
          let n = Cyclomatic_php.cyclomatic_complexity_func ~verbose:true def in
          pr2 (spf "cyclomatic complexity for method %s::%s is %d"
                  class_name name n);
      | Cst_php.ClassConstants _ | Cst_php.ClassVariables _ ->
          ()
      | Cst_php.XhpDecl _ | Cst_php.UseTrait _ ->
          ()
      )
  | _ -> ()
  )
*)
(*e: test_cyclomatic_php *)

(*****************************************************************************)
(* Abstract interpreter *)
(*****************************************************************************)
(*
module Interp = Abstract_interpreter_php.Interp (Tainting_fake_php.Taint)

(* bin/aphp *)
let test_abstract_interpreter file depth =
  let ast =
    Ast_php_build.program (Parse_php.parse_program file) in
  let jujudb =
    Database_juju_php.juju_db_of_files ~show_progress:false [file] in
  let db =
    Database_juju_php.code_database_of_juju_db jujudb in
  let env =
    Env_interpreter_php.empty_env db file in

  Tracing_php.tracing := true;
  Abstract_interpreter_php.extract_paths := false;

  Abstract_interpreter_php.strict := true;
  Abstract_interpreter_php.max_depth := depth;
  Abstract_interpreter_php.show_vardump := true;

  let _heap =
    Interp.program env Env_interpreter_php.empty_heap ast in
  ()
*)

(*****************************************************************************)
(* Callgraph *)
(*****************************************************************************)
(*
module Db = Database_juju_php
module CG = Callgraph_php2

let test_callgraph_php file =
  let db =
    Db.code_database_of_juju_db  (Db.juju_db_of_files [file]) in
  let g = Callgraph_php_build.create_graph
    ~show_progress:false ~strict:true
    [file] db
  in
  (* todo: could also show a dot? *)
  g |> Map_.iter (fun n1 v ->
    v |> Set_.iter (fun n2 ->
      pr (spf "%s --> %s"
             (CG.string_of_node n1) (CG.string_of_node n2));
    )
  )
*)

(*****************************************************************************)
(* Includes *)
(*****************************************************************************)
(*
(* printing not static include *)
let test_include_require file =
  let ast = Parse_php.parse_program file in

  let increqs = Include_require_php.top_increq_of_program ast in
  increqs |> List.iter (fun (_inckind, _tok, incexpr) ->
    match incexpr with
    | Include_require_php.SimpleVar _
    | Include_require_php.Other _ ->
        (* Matching_report.print_match [tok] *)
      failwith "Matching_report.print_match dependency to fix"
    | _ -> ()
  );
  ()
*)
(*****************************************************************************)
(* Prolog *)
(*****************************************************************************)
(* bin/pphp *)
(*
let test_prolog_php file query =
  let file = Common.fullpath file in
  let xs =
    Database_prolog_php.prolog_query ~verbose:true ~source_file:file ~query in
  pr2_gen xs
*)

(*****************************************************************************)
(* Stat *)
(*****************************************************************************)
(*
let test_stat_php xs =

  let files = Lib_parsing_php.find_source_files_of_dir_or_files xs in
  let h = Common2.hash_with_default (fun () -> 0) in

  files |> Console.progress (fun k -> List.iter (fun file ->
    k();
    try
      let ast = Parse_php.parse_program file in
      h#update "parsing correct" (fun x -> x + 1);
      Statistics_php.stat_of_program h file ast;
      ()

    with Parsing.Parse_error ->
      h#update "parsing error" (fun x -> x + 1);
  ));
  (* old:
   * let stat = Statistics_php.stat_of_program ast in
   * let str = Statistics_php.string_of_stat stat in
   * pr2 str
   *)
  Common2.pr2_xxxxxxxxxxxxxxxxx();
  h#to_list |> List.iter (fun (s, i) -> pr2 (spf "%-30s: %d" s i));
  ()
*)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(*
let test_unsugar_php file =
  let ast = Parse_php.parse_program file in
  let _ast = Unsugar_php.unsugar_self_parent_program ast in
  let s = raise Todo
   (*  Export_ast_php.ml_pattern_string_of_program ast  *)
  in
  pr2 s
*)

(*****************************************************************************)
(* External tools cooperation *)
(*****************************************************************************)

(*
let test_xdebug_dumpfile file =
  file |> Xdebug.iter_dumpfile (fun acall ->
    pr2_gen acall;
  )

let test_php_xdebug file =
  let trace_file = Common.new_temp_file "xdebug" ".xt" in
  let php = Xdebug.php_cmd_with_xdebug_on ~trace_file () in
  let cmd = spf "%s %s" php file in
  pr2 (spf "executing: %s" cmd);
  Common.command2 cmd;
  trace_file |> Xdebug.iter_dumpfile ~show_progress:false (fun call ->
    let caller = call.Xdebug.f_call in
    let str = Xdebug.s_of_kind_call caller in
    let file = call.Xdebug.f_file in
    let line = call.Xdebug.f_line in
    pr (spf "%s:%d: %s" file line str);
  )

let test_type_xdebug_php _file =
  raise Todo
*)
(*
  let (d,b,e) = Common.dbe_of_filename file in
  assert(e = "php");
  let trace_file = Common.filename_of_dbe (d,b,"xt") in
  (* todo? remove pre-existing trace file ? because xdebug by default appends *)
  pr2 (spf "xdebug trace file in %s" trace_file);
  let cmd = Xdebug.php_cmd_with_xdebug_on ~trace_file () in
  let cmd = spf "%s %s" cmd file in
  pr2 (spf "executing: %s" cmd);
  Common.command2 cmd;

  let h = Hashtbl.create 101 in

  trace_file +> Xdebug.iter_dumpfile ~show_progress:true (fun call ->
    (* quite close to Database_php_build.index_db_xdebug *)

    let caller = call.Xdebug.f_call in
    let params = call.Xdebug.f_params in
    let ret = call.Xdebug.f_return in

    let str = Callgraph_php.s_of_kind_call caller in

    let tparams =
      params +> List.map Typing_trivial_php.type_of_expr in
    let tret =
      match ret with
      | None -> [Type_php.Unknown]
      | Some e -> Typing_trivial_php.type_of_expr e
    in
    let ft = [Type_php.Function (tparams +> List.map(fun t -> Some t), tret)] in

    h +> Common.hupdate_default str
      ~update:(fun old -> Typing_trivial_php.union_type old ft)
      ~default:(fun () -> ft);
  );
  h +> Common.hash_to_list +> List.iter (fun (s, t) ->
    pr2 (spf "%s -> %s" s (Type_php.string_of_phptype t));
  );
  ()
*)

(*
let test_parse_phpunit_json file =
  let json = Json_in.load_json file in
  let tr = Phpunit.test_results_of_json json in
  Phpunit.final_report tr
*)
(*
let test_phpdoc dir =
  let files = Phpmanual_xml.find_functions_reference_of_dir dir in
  files +> List.iter (fun file ->
    let _func = Phpmanual_xml.function_name_of_xml_filename file in
    (* pr2 (spf "%s\n %s" func file); *)
    try
      let _xml = Phpmanual_xml.parse_xml file in
      ()
    with _exn ->
      pr2 (spf "PB in %s" file);
  )
*)

(*
let test_php_serialize file =
  let s = Common.read_file file in
  let php = Php_serialize.parse_string s in
  let v = Php_serialize.vof_php php in
  let s = OCaml.string_of_v v in
  pr2 s
*)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

(* Note that other files in this directory define some cmdline actions:
 *  - database_php_build.ml
*)
let actions () = [
  "-parse_php_simple", "   <files or dirs>",
  Common.mk_action_n_arg test_parse_simple;
  "-dump_php_simple", "   <file>",
  Common.mk_action_1_arg test_dump_simple;
(*
  "-pp_php_simple", "   <file>",
  Common.mk_action_1_arg test_pp_simple;
*)
(*
  "-scope_php", " <file>",
  Common.mk_action_1_arg test_scope_php;
  "-type_php", " <file>",
  Common.mk_action_1_arg test_type_php;
*)
  (*s: test_analyze_php actions *)
(*
    "-cfg_php",  " <file>",
    Common.mk_action_1_arg test_cfg_php;

    "-dflow_php",  " <file>",
    Common.mk_action_1_arg test_dflow_php;
  (*x: test_analyze_php actions *)
    "-cyclomatic_php", " <file>",
    Common.mk_action_1_arg test_cyclomatic_php;
  (*e: test_analyze_php actions *)
  "-callgraph_php", "   <file>",
  Common.mk_action_1_arg test_callgraph_php;
*)
(*
  "-dfg_php",  " <file>",
    Common.mk_action_1_arg test_dfg_php;
    "-test_pil",  " <file>",
    Common.mk_action_1_arg test_pil;
    "-test_pretty_print_pil", " <file>",
    Common.mk_action_1_arg test_pretty_print_pil;
    "-cfg_pil",  " <file>",
    Common.mk_action_1_arg test_cfg_pil;
    "-dataflow_pil", " <file",
    Common.mk_action_1_arg test_dataflow_pil;
    "-visitor_pil", " <file",
    Common.mk_action_1_arg test_visitor_pil;
*)
(*
  "-ia_php", " <file> <depth>",
  Common.mk_action_1_arg (fun file ->
    test_abstract_interpreter file 6
  );
  "-ia_php_depth", " <file> <depth>",
  Common.mk_action_2_arg (fun file n ->
    test_abstract_interpreter file (int_of_string n)
  );

  "-prolog_php", " <file> <query>",
  Common.mk_action_2_arg (fun file query ->
    test_prolog_php file query;
  );

  "-stat_php", " <files_or_dirs>",
  Common.mk_action_n_arg test_stat_php;

  "-include_require_static", " <file>",
  Common.mk_action_1_arg test_include_require;
  "-unsugar_php", " <file>",
  Common.mk_action_1_arg test_unsugar_php;

  "-php_xdebug", " <file>",
  Common.mk_action_1_arg test_php_xdebug;
  "-type_xdebug_php", " <file>",
  Common.mk_action_1_arg test_type_xdebug_php;
  "-parse_xdebug_dumpfile", " <dumpfile>",
  Common.mk_action_1_arg test_xdebug_dumpfile;
  "-parse_phpunit_json", " <jsonfile>",
  Common.mk_action_1_arg test_parse_phpunit_json;
*)
(*
  "-test_phpdoc", " <dir>",
  Common.mk_action_1_arg test_phpdoc;
*)
(*
  "-test_php_serialize", " <file>",
  Common.mk_action_1_arg test_php_serialize;
*)
]

(*e: test_analyze_php.ml *)

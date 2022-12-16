(*
 * Please imagine a long and boring gnu-style copyright notice
 * appearing just here.
 *)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(*
 * An interactive tool a la SQL to query information about the structure
 * of a codebase (the inheritance tree, the call graph, the data graph),
 * for instance "What are all the children of class Foo?".
 * The data is the code. The query language is
 * Prolog (http://en.wikipedia.org/wiki/Prolog), a logic-based
 * programming language used mainly in AI but also popular in database
 * (http://en.wikipedia.org/wiki/Datalog).
 *
 * See h_program-lang/prolog_code.pl for more information
 *
 * related work:
 *  - http://jquery.cs.ubc.ca/, the original inspiration for codequery
 *  - QL by semmle
 *  - http://www.ndepend.com/Features.aspx#CQL
 *  - http://golang.org/s/oracle-design
 *  - http://llvm.org/devmtg/2014-10/Slides/Hawes-Frappe.pdf
 *
 * notes: pieter started to implement something similar using neo4j/cypher
 * instead of prolog for the query engine. Example of query:
 *   MATCH (n {vmname: "com/facebook/inject/AbstractProvider"})<-[:EXTENDS]-(m)
               *   RETURN m.vmname
               *   LIMIT 500
               * The main advantage is that if you have your linter already written in
               * Java, then neo4j APIs are easily accessible from the linter to get
               * access to global information. The equivalent in pfff would be to use
               * the graph_code OCaml API from your ocaml linter.
               *
              *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

(* exciting! *)
let datalog = ref false

let lang = ref "php"

(* todo: swipl (SWI-Prolog) is not in PATH by default on our machines *)
let swipl_fb = "/home/pad/packages/Linux/bin/swipl"
let swipl =
  if Sys.file_exists swipl_fb
  then swipl_fb
  else "swipl"

let predicates_file =
  Filename.concat Config_pfff.path_pfff_home "h_program-lang/prolog_code.pl"
let logicrules_file =
  Filename.concat Config_pfff.path_pfff_home "h_program-lang/datalog_code.dtl"
let bddbddb_jar_file =
  Filename.concat Config_pfff.path_pfff_home "external/bddbddb/bddbddb-full.jar"

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let pr2_dbg s =
  if !verbose then pr2 s

(*****************************************************************************)
(* Datalog *)
(*****************************************************************************)

let exec cmd =
  pr2_dbg cmd;
  Common.command2 cmd

(* from hw6/run_bddbddb.py
   -mx600m -Dlearnbestorder=n -Dsingleignore=yes -Dbasedir=./results/ -Dbddcache=1500000 -Dbddnodes=40000000 -Dnumberingtype=%s -Dpa.clinit=no -Dpa.filternull=yes -Dpa.unknowntypes=no net.sf.bddbddb.Solver
*)
let java_options = [
  "-mx2000m";
(*
  "-Dlearnbestorder=n";
  "-Dsingleignore=yes";
  "-Dbddcache=1500000";
  "-Dbddnodes=40000000";
*)
] |> Common.join " "

let run_datalog root facts =
  (* facts +> List.iter pr2; *)
  let datalog_file = Filename.concat root "facts.dl" in
  Common.with_open_outfile datalog_file (fun (pr_no_nl, _chan) ->
    let pr s = pr_no_nl (s ^ ".\n") in
    facts |> List.iter (fun fact ->
      pr (Datalog_code.string_of_fact fact));
  );
  pr2 (spf "Your datalog facts are in %s" datalog_file);
  (*
    when using lua-datalog
    let final_file = "/tmp/datalog.dl" in
  let cmd = spf "cat %s %s > %s"
    datalog_file logicrules_file final_file in
  Common.command2 cmd;
  let cmd = spf "datalog %s | sort" final_file in
  (* Common.command2 cmd; *)
  pr2 (spf "RUN %s" cmd);
  *)
  (* bddbddb special stuff *)
  (* old: *)
  let datadir = "/home/pad/local/datalog/bddbddb/examples/pfff/data" in
  Datalog_code.bddbddb_of_facts facts datadir;


  Common2.with_tmp_dir (fun dir ->
    let datadir = Filename.concat dir "/data" in
    exec (spf "mkdir %s" datadir);
    exec (spf "cp %s %s" logicrules_file dir);
    Datalog_code.bddbddb_of_facts facts datadir;
    let cmd = spf "cd %s; java %s -jar %s %s > %s/X.log"
        dir java_options bddbddb_jar_file
        (Filename.basename logicrules_file) dir in
    exec cmd;
    pr2 ("Done with bddbddb, generating .explain files now");
    let pointing_file =
      Datalog_code.bddbddb_explain_tuples
        (Filename.concat datadir "/PointingData.tuples") in
    let calling_file =
      Datalog_code.bddbddb_explain_tuples
        (Filename.concat datadir "/CallingData.tuples") in
    exec (spf "cp %s %s" pointing_file root);
    exec (spf "cp %s %s" calling_file root);

    calling_file |> Common.cat |> Common.take_safe 10 |> List.iter pr;
    exec (spf "rm %s/*" datadir);
    exec (spf "rmdir %s" datadir);
  );
  (* don''t care about the remaining prolog stuff so exit earlier *)
  raise (UnixExit 0)


(*****************************************************************************)
(* Language specific, building the prolog db *)
(*****************************************************************************)
let build_prolog_db lang root xs =
  let root = Common.fullpath root |> Common2.chop_dirsymbol in
  let files = Find_source.files_of_dir_or_files ~lang xs in
  match lang with
  | "php" ->
      (*
       * todo:
       * - do things in parallel, pack things.
       * => should significantly reduce the time to produce the
       * prolog facts. It currently takes 41min on www and I hope
       * we can reduce that to a few minutes.
       *)
      (* so many errors that is better to hide them for now *)
      Flag_analyze_php.show_errors := false;

      let facts_pl_file = "facts.pl" in
      let prolog_compiled_db = "facts.db" in

      let file = Filename.concat root facts_pl_file in
      pr2 (spf "generating prolog facts in %s" file);
      let facts =
        Database_prolog_php.build ~show_progress:!verbose root files in
      Common.with_open_outfile file (fun (pr_no_nl, _chan) ->
        let pr s = pr_no_nl (s ^ "\n") in
        facts |> List.iter (fun fact ->
          pr (Prolog_code.string_of_fact fact);
        )
      );

      pr2 (spf "compiling prolog facts with swipl in %s/%s"
             root prolog_compiled_db);
      Common.command2 (spf "%s -c %s/%s %s"
                         swipl root facts_pl_file predicates_file);
      Common.command2 (spf "mv a.out %s/%s" root prolog_compiled_db);

      Filename.concat root prolog_compiled_db

  | "cmt" | "bytecode" | "clang2" | "c" ->

      let g =
        match lang with

        #if FEATURE_CMT
        | "cmt" ->
            let ml_files = Find_source.files_of_root ~lang:"ml" root in
            let cmt_files = files in
            Graph_code_cmt.build ~verbose:!verbose ~root ~cmt_files ~ml_files
                                                                     #endif

                                                                     #if FEATURE_BYTECODE
        | "bytecode" ->
            let graph_code_java =
              (*           Some (Graph_code_java.build ~verbose:!verbose ~only_defs:true
                                  root skip_list)
              *)
              None
            in
            Graph_code_bytecode.build ~verbose:!verbose ~graph_code_java
              root files
                   #endif
(*
        | "clang2" ->
          Graph_code_clang.hook_use_edge :=
            Graph_code_prolog.hook_use_edge_for_prolog;
          Graph_code_clang.build ~verbose:!verbose root files
*)
        | "c" ->
            Graph_code_c.hook_use_edge :=
              Graph_code_prolog.hook_use_edge_for_prolog;

            if !datalog
            then Graph_code_c.facts := Some (ref []);

            let g = Graph_code_c.build ~verbose:!verbose root files in

            if !datalog
            then begin
              let facts = List.rev !(Common2.some (!Graph_code_c.facts)) in
              run_datalog root facts
            end;
            g
        | _ -> raise Impossible
      in
      let facts = Graph_code_prolog.build g in
      let facts_pl_file = Filename.concat root "facts.pl" in
      Common.with_open_outfile facts_pl_file (fun (pr_no_nl, _chan) ->
        let pr s = pr_no_nl (s ^ "\n") in
        facts |> List.iter (fun x -> pr (Prolog_code.string_of_fact x))
      );
      let prolog_compiled_db = Filename.concat root "facts.db" in
      Common.command2 (spf "%s -c %s %s" swipl facts_pl_file predicates_file);
      Common.command2 (spf "mv a.out %s" prolog_compiled_db);
      pr2 (spf "Your compiled prolog DB is ready. Run %s" prolog_compiled_db);
      prolog_compiled_db

  | _ -> failwith ("language not yet supported: " ^ lang)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs =
  Logger.log Config_pfff.logger "codequery" None;
  let root = Common2.common_prefix_of_files_or_dirs xs in
  let compiled = build_prolog_db !lang root xs in
  Common.command2 compiled

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)


let test_explain_bddbddb_tuples file =
  let dst = Datalog_code.bddbddb_explain_tuples file in
  dst |> Common.cat |> List.iter pr

(*---------------------------------------------------------------------------*)
(* regression testing *)
(*---------------------------------------------------------------------------*)
let test () =
  let suite = Unit_prolog_php.unittest in
  OUnit.run_test_tt suite |> ignore;
  ()

(* ---------------------------------------------------------------------- *)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let extra_actions () = [
  "-build", " <dirs> source code to analyze",
  Common.mk_action_n_arg (fun xs ->
    let root = Common2.common_prefix_of_files_or_dirs xs in
    let file = build_prolog_db !lang root xs in
    pr2 "";
    pr2 (spf "Your compiled prolog DB is ready. Run %s" file);

  );
  "-test", " run regression tests",
  Common.mk_action_0_arg test;

  "-explain_tuples", " ",
  Common.mk_action_1_arg test_explain_bddbddb_tuples;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
  extra_actions () @
  []

let options () = [
  "-lang", Arg.Set_string lang,
  (spf " <str> choose language (default = %s)" !lang);
  "-datalog", Arg.Set datalog,
  " experimental datalog generation";
  "-verbose", Arg.Unit (fun () ->
    verbose := true;
    Flag_analyze_php.verbose_database := true;
  ), " ";
] @
  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  [
    "-version",   Arg.Unit (fun () ->
      pr2 (spf "CodeQuery version: %s" Config_pfff.version);
      exit 0;
    ),
    "  guess what";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =
  Gc.set {(Gc.get ()) with Gc.stack_limit = 200 * 1024 * 1024};
  Flag_analyze_php.verbose_database := false;

  let usage_msg =
    spf "Usage: %s [options] <dir> \nDoc: %s\nOptions:"
      (Filename.basename Sys.argv.(0))
      "https://github.com/returntocorp/pfff/wiki/Codequery"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () ->
    (match args with
     (* --------------------------------------------------------- *)
     (* actions, useful to debug subpart *)
     (* --------------------------------------------------------- *)
     | xs when List.mem !action (Common.action_list (all_actions())) ->
         Common.do_action !action xs (all_actions())

     | _ when not (Common.null_string !action) ->
         failwith ("unrecognized action or wrong params: " ^ !action)

     (* --------------------------------------------------------- *)
     (* main entry *)
     (* --------------------------------------------------------- *)
     | (x::xs) ->
         main_action (x::xs)

     (* --------------------------------------------------------- *)
     (* empty entry *)
     (* --------------------------------------------------------- *)
     | _ ->
         Common.usage usage_msg (options());
         failwith "too few or too many arguments"
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () ->
    main ();
  )

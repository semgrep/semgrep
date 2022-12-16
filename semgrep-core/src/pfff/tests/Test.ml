(*
 * Please imagine a long and boring GNU-style copyright notice
 * appearing just here.
 *)
open Common

module J = JSON

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Unit tests runner (and a few adhoc actions) *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in the "the options" section below), this
 * program also depends on external files ?
*)
let verbose = ref false

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let graph_of_string str =
  let tmpdir = Filename.get_temp_dir_name () in
  let tmpfile = Parse_php.tmp_php_file_from_string str in
  let (g, _stat) = Graph_code_php.build
      ~verbose:false ~logfile:"/dev/null" tmpdir [tmpfile] in
  g

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* regression testing *)
(*---------------------------------------------------------------------------*)

let tests = List.flatten [

  (* general tests *)
  Unit_commons.tests;
  Unit_commons_core.tests;
  Unit_graph_code.tests ~graph_of_string;

  (* PHP related tests *)
  Unit_parsing_php.tests;
  (* TODO dune .opam file
        Unit_pretty_print_php.unittest;
  *)
(*
   Unit_foundation_php.unittest;
   Unit_static_analysis_php.unittest;
   Unit_typeinfer_php.unittest;
*)
  (*      Unit_analyze_db_php.unittest; *)
  (*      Unit_static_analysis_simple_php.unittest;*)
  (* TODO need extra deps to swipl
        Unit_prolog_php.unittest;
  *)
  (*      Unit_checker_php.unittest; should be in check_generic/ now *)
  (* this one needs xdebug to work *)
  (* TODO dune .opam file
        Unit_coverage_php.unittest;
  *)

  (* non PHP related tests *)

  Unit_parsing_ml.tests;
  Unit_parsing_scala.tests;
  (* TODO path issue when run outside of pfff (e.g., in semgrep-core)
        Unit_analyze_ml.unittest;
  *)
  Unit_parsing_java.tests;
  Unit_analyze_java.tests;
  (* TODO dune
     #if FEATURE_BYTECODE
        Unit_analyze_bytecode.unittest;
     #endif
  *)
  Unit_parsing_js.tests;
  Unit_analyze_js.tests;
  Unit_parsing_json.tests;
  Unit_parsing_python.tests;
  Unit_parsing_ruby.tests;
  (* TODO dune .opan files
        Unit_parsing_html.unittest;
  *)
  Unit_parsing_cpp.tests;
  Unit_parsing_go.tests;
  Pfff_lang_regexp.Unit_parsing.tests;

  (* generic AST tests *)
  (* Unit_naming_generic.unittest;
     Unit_typing_generic.unittest;
     Unit_dataflow.unittest;
  *)
]

(*
   Run the alcotest tests while ignoring command-line options which would
   otherwise be interpreted by alcotest.

   The default alcotest CLI offers a way to filter tests but we're not
   using it, so we specify our own filter with the 'filter' option instead.
*)
let run_alcotest_tests ?pcre_filter () =
  let tests = Testutil.filter ?pcre:pcre_filter tests in
  let alcotest_tests = Testutil.to_alcotest tests in
  Alcotest.run ~argv:[| "<dummy>" |] "pfff" alcotest_tests

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)
type x = Foo of int

let action1 () =
  let x = Foo 1 in
  pr2_gen x;
  let x = 1 in
  pr2_gen x;
  let round x y = (x + (y - 1)) land (lnot (y - 1)) in
  let howmany x y = (x + (y - 1)) / y in
  let roundup x y = howmany x y * y in
  pr2_gen (round 1 1024);
  pr2_gen (howmany 1 1024);
  pr2_gen (roundup 1 1024);
  ()


let test_json_pretty_printer file =
  let json = J.load_json file in
  let s = J.string_of_json json in
  pr s

let test_json_bench file =
  Common.profile_code "json_bench" (fun () ->
    pr2 (Common2.memory_stat ());
    let _json = J.load_json file in
    pr2 (Common2.memory_stat ());
  )

(* ---------------------------------------------------------------------- *)
let pfff_extra_actions () = [
  "-json_pp", " <file>",
  Common.mk_action_1_arg test_json_pretty_printer;
  "-json_bench", " <file>",
  Common.mk_action_1_arg test_json_bench;

  "-check_overlay", " <dir_orig> <dir_overlay>",
  Common.mk_action_2_arg (fun dir_orig dir_overlay ->
    Overlay_code.check_overlay ~dir_orig ~dir_overlay;
  );
  "-gen_overlay", " <dir_orig> <dir_overlay> <output>",
  Common.mk_action_3_arg (fun dir_orig dir_overlay output ->
    Overlay_code.gen_overlay ~dir_orig ~dir_overlay ~output;
  );
  "-adapt_layers_overlay", " <overlay> <dir_layers> <dir_ayers_overlay>",
  Common.mk_action_3_arg
    (fun overlay dir_layers_orig dir_layers_overlay ->
       Overlay_code.adapt_layers
         ~overlay:(Overlay_code.load_overlay overlay)
         ~dir_layers_orig
         ~dir_layers_overlay
       ;
    );
  "-adapt_database_overlay", "<overlay> <file> <output>",
  Common.mk_action_3_arg (fun overlay orig output ->
    let db =
      Database_code.load_database orig in
    let db2 =
      Overlay_code.adapt_database db (Overlay_code.load_overlay overlay) in
    Database_code.save_database db2 output
  );
  "-action1", "", Common.mk_action_0_arg action1;

  (* TODO: move this outside pfff :) *)
  "-action2", "<files>", Common.mk_action_n_arg (fun xs ->
    xs |> List.iter (fun file ->
      let (_d,b,_e) = Common2.dbe_of_filename file in
      let (d,b,e) = "/home/pad/plan9/sys/src/include", b, "h.clang2" in
      let file = Common2.filename_of_dbe (d,b,e) in
      if Sys.file_exists file
      then ()
      else (* pr2 (spf "file %s not there" file); *)
        print_string (spf "%s " b)
    );
    print_string "\n";
  );
  "-mv_kernel", "<file1> <dirdst>", Common.mk_action_2_arg (fun file dirdst ->
    let file = Common.fullpath file in
    let kerneldir = "/home/pad/plan9/sys/src/9" in
    let candidates = Common.cmd_to_list (spf "find %s -type l" kerneldir) in
    let fcandidates =
      Graph_code.basename_to_readable_disambiguator ~root:"" candidates in
    match fcandidates (Filename.basename file) with
    | [] -> failwith "No candidate found"
    | [x] ->
        let x = "/" ^ x in
        let cmd s = Sys.command s |> ignore in
        cmd (spf "cp %s /tmp" x);
        cmd (spf "rm -f %s" x);
        cmd (spf "mv %s %s" file dirdst);
        let file = Filename.concat dirdst (Filename.basename file) in
        let file = Common.fullpath file in
        cmd (spf "ln -s %s %s" file x);
        pr2 (spf "ln -s %s %s" file x);
    | _ -> failwith "too many candidates"
  );

  "-relativize", "<dir>", Common.mk_action_1_arg (fun dir ->
    let dir = Common.fullpath dir in
    let candidates = Common.cmd_to_list (spf "find %s -type l" dir) in
    let root = "/Users/yoann.padioleau/github/xix/xix-plan9/" in
    candidates |> List.iter (fun link ->
      let dst = Unix.readlink link in
      if not (Filename.is_relative dst)
      then begin
        let readable = Common.readable ~root dst in
        pr2 (spf "%s -> %s" link readable);
        let newdst = "../../../../" ^ readable in
        let cmd = spf "rm %s; ln -s %s %s" link newdst link in
        Common.command2 cmd;
      end;
    );
  );

  "-luisa", "<files>", Common.mk_action_4_arg (fun t1 t2 t3 s6 ->
    let f file =
      Common.cat file

      |> List.map (fun s ->
        if s =~ "^\\([A-Za-z_0-9][A-Za-z_0-9][A-Za-z_0-9][A-Za-z_0-9]\\)"
        then Common.matched1 s
        else s
      )

      |> Common2.set
    in
    let s1 = f t1 in
    let s2 = f t2 in
    let s3 = f t3 in

    let s6 = f s6 in
    let ($*$) = Common2.($*$) in

    pr2 "------- s6 * t1 * t2 * t3 -------";
    (s6 $*$  s1 $*$ s2 $*$ s3) |> List.iter pr;
    pr2 "------- s6 * t1 -------";
    (s6 $*$ s1) |> List.iter pr;
    pr2 "------- s6 * t2 -------";
    (s6 $*$ s2) |> List.iter pr;
    pr2 "------- s6 * t3 -------";
    (s6 $*$ s3) |> List.iter pr;
  );
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
  pfff_extra_actions() @
  (* TODO .opam dune file
     Test_analyze_php.actions()@
  *)
  Test_analyze_js.actions()@
  Test_analyze_ruby.actions()@
  Test_analyze_ml.actions()@
  Test_analyze_cpp.actions()@
  Test_analyze_c.actions()@
  Test_program_lang.actions()@
(*
 Builtins_php.actions()@
*)
  []

let options () = [
  "-verbose", Arg.Set verbose,
  " ";
] @
  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  Common2.cmdline_flags_other () @
  [
    "-version",   Arg.Unit (fun () ->
      pr2 (spf "pfff (test) version: %s" Config_pfff.version);
      exit 0;
    ),
    "  guess what";
    (* this can not be factorized in Common *)
    "-date",   Arg.Unit (fun () ->
      pr2 "version: $Date: 2008/10/26 00:44:57 $";
      raise (Common.UnixExit 0)
    ),
    "   guess what";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let run_cli () =

  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};

  let usage_msg =
    "Usage: " ^ Common2.basename Sys.argv.(0) ^
    " [options] <file or dir> " ^ "\n" ^ "Options are:"
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
     | [x] ->
         run_alcotest_tests ~pcre_filter:x ()

     (* --------------------------------------------------------- *)
     (* empty entry *)
     (* --------------------------------------------------------- *)
     | [] -> run_alcotest_tests ()
     | _ ->
         Common.usage usage_msg (options());
         failwith "too few or too many arguments"
    )
  )

(*****************************************************************************)
let () =
  Common.main_boilerplate (fun () ->
    run_cli ();
  )

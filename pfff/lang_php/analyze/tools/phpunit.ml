(*s: phpunit.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009, 2010, 2011 Facebook
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
(*e: Facebook copyright *)

open Common

open Cst_php

module Ast = Cst_php
module J = Json_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This file provides some support to "understand" PHPUnit, and 
 * especially understand PHPUnit tests results, that is
 * a poor's man test results database (e.g test files 
 * that pass, fail, are slow, use huge memory, etc).
 * For now it's doing so by parsing different PHPUnit text runners output. 
 * 
 * Why ? With such information accessible easily from OCaml, it's then 
 * possible to cross-check for instance test results (failures) with other 
 * static or dynamic analysis (e.g. whether failing tests have a high cycomatic
 * complexity).
 * 
 * --------------------------------------------------------------------------
 * 
 * Why parsing PHPUnit output to provide such support ? Why not 
 * extending/instrumenting the current text runners ? Why not using 
 * the -to-json flag of phpunit ? Why not analyzing the MySQL DB containing
 * the test results ? 
 * Well ... I tried but for the kind of things I wanted (run specific 
 * tests on the command line), it was easier to write yet another frontend
 * on top of the existing frontend  (see facebook/fb_phpunit), and parse
 * their output. Both phpunit text runner and our facebook text 
 * runners have pbs.
 *
 * PHPUnit seems to have pbs (at least I had pbs when using it). 
 * It's not easy to run the text UI runner and follows what is described
 * in the manual. Here are the problems I have encountered:
 * 
 * - After I installed PHPUnit (via pear channel and pear install)
 *   on my linux machine, when I run /usr/bin/phpunit I got this message:
 * 
 *     exception 'PHPUnit_Framework_Exception' with message '/usr/bin/phpunit
 *     does not exist' in /usr/share/pear/PHPUnit/Util/Filter.php:147
 * 
 *   If I copy /usr/bin/phpunit to ~/myphpunit and run this one then it works ...
 *   Why ?
 * 
 * - The manual says the phpunit text runner can take a directory
 *   as a parameter but it does not work.
 * 
 * - The manual says it can take an XML configuration file to 
 *   specify the set of files to test, but again, the one shown in the
 *   manual does not work. Had to tweak it.
 * 
 * - Then even after those tweaks, the XML config can still not take
 *   the <directory>dir</directory> command. So have to auto generate
 *   this XML file based on the set of files I want to test. Basically
 *   you need to do your own test runner on top of the phpunit runner ...
 * 
 * - It needs tweak to handle our facebook tests which implicitly load
 *   the framework or implicitely use the flib modules. So it's somehow
 *   simpler to use our facebook text runners.
 * 
 * Note that our own facebook UI text runner(s) is not really good either:
 * 
 *  - It can not print global stat when applied on a directory.
 *    It just calls phpunit on each dir. So there is no incentive
 *    given to people to run our tests (but as our tests are also
 *    kind of flaky and slow, it's maybe better they don't run all
 *    the tests ...)
 * 
 *  - lots of noise in the output
 * 
 *  - It does not provide state for easy regression reports 
 *    (like my simple Common regression testing sexp-based infrastructure).
 *    You use the MySQL database, but it's arguably more heavyweight.
 * 
 *  - It does not report JSON output or txt output that makes it easy
 *    to do diff between run and do  custom made local regressions reports.
 *    To do that you need to either:
 *         * to look at the unittest dashboard, 
 *         * to do some MySQL to extract those info from the unittest db.
 * 
 *  - It has 4 different interfaces, buried in the hierarchy, that is 
 *     * scripts/unittests/bin/unittest.php (for single file test)
 *     * scripts/unittests/bin/unittest-controler.php (for multi files
 *        or dirs tests but requires to be run from certain paths and with
 *        certain arguments)
 *     * scripts/unittests/bin/run-all-unittests
 *     * flib/_bin/testModule (which can not handle recursive testing)
 *  
 *  - Running the tests in a file, or dir, or multiple dirs, or flib needs
 *    a separate set of commands.
 * 
 *  - It does not have flags to avoid the very slow or flaky past tests
 * 
 * It has however nice features:
 * 
 *  - log to a database with full history for all test (and a good
 *    database model)
 * 
 *  - a web interface dashboard
 * 
 *  - an email notification system (with different levels of notifications)
 * 
 *  - a mocking system for our database query (sqlshim) to improve
 *    the stability of our tests
 * 
 *  - a bisect tool (but is said to be too slow for now)
 * 
 *  - a web-based text runner interface (which (ab)use the APC cache 
 *    to accelerate our tests)
 * 
 *  - an infrastructure that makes it possible to parallelize our tests
 *    (and also to be fault tolerant. For instance a syntax error in 
 *    a test file will not abord the full testsuite)
 * 
 * So if you want advanced services, you got it, but if you want 
 * the simple things, we don't have it, or it's awkward to do simple
 * things. Hence this module ...
 * 
 * --------------------------------------------------------------------------
 * 
 * A few notes:
 * 
 * PHPUnit itself proposes 3 ways to run tests:
 *  - via the phpunit runner (when it works ...), which is supposed
 *    to automatically find the test files
 *  - via the phpunit runner and an XML config file which is supposed 
 *    to specify a set of test files
 *  - programmatically by using the PHPUnit library and calling 
 *    addTest() methods.
 * 
 * In facebook There are 3 ways to write tests ...:
 * 
 *  - using the PHPUnit way, by require_once 'PHPUnit/Framework.php';
 *  - using the facebook traditional way, by assuming this required_once is 
 *    already done, but other require has to be written
 *  - using the flib way, by assuming this required_once is done as well
 *    as the require_module of the current module the __tests__ resides in
 *    ...
 *  which means when you do a phpunit test runner, you need to accomodate
 *  this 3 ways ...
 *  
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type test_status = 
  | Pass of int (* nb tests *) * int (* skipped *)
  | Fail of int (* fail *) * int (* pass *)
  | Fatal of string
 (* with tarzan *)

(* 
 * A poor's man test data model. sgrimm's db is far more complete of course.
 * But as said above, it's convenient to have a simple 
 * command line tool that already offers some good services.
 * 
 * We just remember here the number of ok/fails per files (or modules when
 * it's flib tests). No need to remember testCases that fails. The log
 * contain this information. Here we want to look at a higher level 
 * (global stats on our tests). Can then do regression on top of that.
 * If you want fine grained, then sgrimm controller.
 * 
 * less: t_method: string option;
 * 
 *)
type test_result = {
  t_file: Common.filename;

  t_status: test_status;

  t_time: int; (* seconds *)
  t_memory: float; (* Mb *)
  t_trace_nb_lines: int;

  (* facebook specific *)
  t_shimmed: int; (* # of "Creating new database shim" messages *)

  (* TODO? add cyclomatic complexity score ? *)
}
 (* with tarzan *)

 (* with tarzan *)


(* for perf regression testing we just want to know if we switch from
 * big delta, hence this coarse-grained discretization of time and space
 *)
type test_speed = 
  | Fast
  | NormalSpeed
  | Slow
let threshold_slow_seconds = ref 10 
let threshold_normal_seconds = ref 2

type test_space = 
  | SmallMem
  | NormalMem
  | BigMem
let threshold_big_mem = ref 100.0
let threshold_normal_mem = ref 20.0

let threshold_long_trace = ref 50

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let phpunit_testcase_classnames = [
  "PHPUnit_Framework_TestCase";
  "BaseFacebookTestCase"; (* facebook specific ... *)
]

(*****************************************************************************)
(* String of *)
(*****************************************************************************)
let s_of_test_status = function
  | Pass (i, skip) -> spf "Pass: %d (skip = %d)" i skip
  | Fail (fail, pass) -> spf "Fail: %d (and %d ok)" fail pass
  | Fatal s -> spf "Fatal: %s" s

(*****************************************************************************)
(* is_phpunit_class *)
(*****************************************************************************)

(* 
 * In certain analysis we need to know if a file is a PHPUnit test file,
 * for instance when we check that all our tests have appropriate 
 * @emails annotations. Multiple heuristics are possible:
 * 
 *  - look if derived directly from known classes
 *  - look if it contains some testXxx method (looking for 
 *    setup/tearDown/... is unsufficient as those are optional methods)
 *  - look if derived from xxxCase classes
 *  
 * Sgrimm suggested we could also do first a global analysis and 
 * generate a simple text file containing a whitelist of all PHPUnit
 * derived classes.
 * 
 * See is_phpunit_derived_class below for the
 * a more precise (but more heavy) analysis. 
 *)

let is_phpunit_derived_class_heuristics def =
  match def.c_extends with 
  | None -> false
  | Some (_tok, extend_classname) ->
      let s = Ast.str_of_class_name extend_classname in

      List.mem s phpunit_testcase_classnames ||
      (* todo? should maybe print a warning like 
       * "Inferred phpunit testcase class, maybe wrong"
       * for the following cases.
       *)
      s =~ ".*TestCase" ||
      (Ast.unbrace def.c_body |> List.exists (fun class_stmt ->
        match class_stmt with
        | Method def -> 
            let s = Ast.str_of_ident def.f_name in 
            s =~ "^test[A-Za-z_]+"
        | _ -> false
      ))

(*
let _ = example 
  (is_phpunit_derived_class_heuristics
    (Parse_php.class_def_of_string 
        "class a extends MyTestCase { }"))
let _ = example 
  (is_phpunit_derived_class_heuristics
      (Parse_php.class_def_of_string 
 "class a extends OtherCase { 
    function testFoo() {
     }
 }"))
let _ = example 
  (not (is_phpunit_derived_class_heuristics
      (Parse_php.class_def_of_string 
          "class a extends foo { }")))

*)

(* 
 * If you have a full code database, you can perform global analysis
 * and statically find all the derived classes chain of the PHPunit 
 * base class. It's more precise than the previous function which is 
 * a hack, but it requires some global analysis which makes it 
 * less convenient to use sometimes.
 * 
 *)
let is_phpunit_derived_class _def (*db*) =
  raise Todo


let (find_testcase_class_if_any: 
      is_phpunit_base_class_name:(string -> bool) ->
      Cst_php.toplevel list -> Cst_php.class_def option) = 
 fun ~is_phpunit_base_class_name asts ->
  try 
    let x = asts |> Common.find_some (fun ast_toplevel -> 
       
       (* Do we need to visit, that is search deep inside nested classes ?
        * Nested classes are normally frowned upon by our PHP checkers 
        * and especially for our test code. So probably ok to just
        * restrict to pattern match at the toplevel.
        *)
       match ast_toplevel with
       | ClassDef def ->
           let classname = Ast.str_of_ident def.c_name in

           let is_abstract = 
             match def.c_type with
             | ClassAbstract _ -> true
             | _ -> false
           in

           (match def.c_extends with 
           | None -> None
           | Some (_tok, extend_classname) ->

               let extendname = Ast.str_of_class_name extend_classname in
           
              (* our list of base classs may not be up to date
               * so better to keep the old heuristic.
               *)
               if (is_phpunit_derived_class_heuristics def &&
                  not (is_abstract) && 
                  not (is_phpunit_base_class_name classname)) 
                 ||
                 (is_phpunit_base_class_name extendname && 
                  not (is_phpunit_base_class_name classname))
               then Some def
               else None
           )
               

       | StmtList _ | FuncDef _ | ConstantDef _ | TypeDef _ -> None
       | FinalDef _ -> None
       | NotParsedCorrectly infos ->
           let info = List.hd infos in
           let file = Parse_info.file_of_info info in
           pr2 (spf "WARNING: parsing problems in %s" file);
           None
       | NamespaceDef _ | NamespaceBracketDef _ | NamespaceUse _ ->
         failwith "no support for namespace yet"
    ) in
    Some x
  with Not_found -> None


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let test_space_of_float x = 
  match () with
  | _ when x <= !threshold_normal_mem -> SmallMem
  | _ when x <= !threshold_big_mem -> NormalMem
  | _ when x > !threshold_big_mem -> BigMem
  | _ -> raise Impossible

let test_speed_of_int x = 
  match () with
  | _ when x <= !threshold_normal_seconds -> Fast
  | _ when x <= !threshold_slow_seconds -> NormalSpeed
  | _ when x > !threshold_slow_seconds -> Slow
  | _ -> raise Impossible


(*****************************************************************************)
(* Parsing traces *)
(*****************************************************************************)

(* ugly ... *)
let parse_one_trace2 file xs = 

  let statuses = 
    xs |> Common.map_filter (fun s ->
      match s with
      | s when s =~ "^OK (\\([0-9]+\\) tests?, \\([0-9]+\\) assertions?)" ->
          let (nb_tests, _nb_asserts) = 
            Common.matched2 s |> Common2.pair s_to_i 
          in
          Some (Pass (nb_tests, 0))

       
      (* old: | s when s =~ "There were \\([0-9]+\\) failures:" -> *)
      | s when s =~ "Tests?: \\([0-9]+\\), Assertions?: \\([0-9]+\\), Failures?: \\([0-9]+\\)" ->
          let (nb_tests, _nb_asserts, nb_fail) = 
            Common.matched3 s |> Common2.triple s_to_i
          in

          (* todo? parse the failure message, file pos/method, 
           * corresponding asserts.
           *)
          Some (Fail (nb_fail, nb_tests - nb_fail))

      (* TODO: diff between fail and errors ? *)
      | s when s =~ "Tests?: \\([0-9]+\\), Assertions?: \\([0-9]+\\), Errors?: \\([0-9]+\\)" ->
          let (nb_tests, _nb_asserts, nb_fail) = 
            Common.matched3 s |> Common2.triple s_to_i
          in
          Some (Fail (nb_fail, nb_tests - nb_fail))


      | s when s =~ "Tests?: \\([0-9]+\\), Assertions?: \\([0-9]+\\), Skipped?: \\([0-9]+\\)" ->
          let (nb_tests, _nb_asserts, nb_skip) = 
            Common.matched3 s |> Common2.triple s_to_i
          in
          Some (Pass (nb_tests - nb_skip, nb_skip))

      (* maybe facebook specific, with our FacebookTestCase wrapper *)
      | s when s =~ "Tests?: \\([0-9]+\\), Assertions?: \\([0-9]+\\), Incompletes?: \\([0-9]+\\)" ->
          let (nb_tests, _nb_asserts, nb_skip) = 
            Common.matched3 s |> Common2.triple s_to_i
          in
          Some (Pass (nb_tests - nb_skip, nb_skip))


      | s when s =~ "PHP Fatal error:" ->
          Some (Fatal s)

      | s when s =~ ".*Fatal PHP Exception:" ->
          Some (Fatal s)

      | s when s =~ "^PHP Catchable fatal error:" ->
          Some (Fatal s)

      (* flib specific *)
      | s when s =~ "^Error: No test sources found in" ->
          Some (Fatal s)


      | s when s =~ "^DISABLING" ->
          Some (Fatal s)

      | _ -> None
    )
  in
  (*pr2_gen statuses;*)
  let status = 
    match statuses with
    | [x] -> x
    | [] -> 
        let msg = ("parse error: could not find status in trace for: " ^ file) in
        pr2 msg;
        (Fatal msg)
    | x::y::xs ->
        if (x::y::xs) |> List.for_all (function Fatal _ -> true | _ -> false)
        then x
        else begin
          pr2 ("parse error: multiple statues in trace for: " ^ file);
          x
        end
  in
  
  let times_and_mem = 
    xs |> Common.map_filter (fun s ->
      match s with

      | s when s =~ "^Time: \\(.*\\), Memory: \\(.*\\)" ->

          let (tm, mem) = Common.matched2 s in
          
          (* ex: 
           * Time: 18 seconds, Memory: 52.00Mb
           * Time: 01:02, Memory: 64.25Mb
           *)
          
          let sec = 
            match () with
            | _ when tm =~ "\\([0-9]+\\) seconds?" ->
                s_to_i (Common.matched1 tm)
            | _ when tm =~ "\\([0-9]+\\):\\([0-9]+\\)" ->
                let (min, sec) = 
                  Common.matched2 tm |> Common2.pair s_to_i in
                min * 60 + sec
            | _ ->
                failwith ("wrong time format: " ^ tm)
          in
          let mem = 
            match () with
            | _ when mem =~ "\\([0-9]+\\)\\.\\([0-9]+\\)Mb" ->
                let (x1, x2) = 
                  Common.matched2 mem |> Common2.pair s_to_i in
                (* could also call float_of_string directly on mem ... *)
                float_of_string (spf "%d.%d" x1 x2)
            | _ ->
                failwith ("wrong memory format: " ^ mem)
          in        
          Some (sec, mem)
          
      (* in olf version of our test runner, we only reported time *)
      | s when s =~ "^Time: \\(.*\\)" ->
          pr2 ("Old PHPUnit format not supported: " ^ s);
          (* some fake values *)
          Some (1, 50.0)

      | _ -> 
          None
    )
  in
  (*pr2_gen times;*)

  let time, memory = 
    match times_and_mem, status with
    | [x], _ -> x
    | _, Fatal _ -> 
        0, 0.0

    | [], _ -> 
        failwith ("parse error: could not find time in trace for: " ^ file)
    | _x::_y::_xs, _ ->
        failwith ("parse error: multiple time entries in trace for: " ^ file)
  in

  let nb_shimmed = 
    xs |> List.filter (fun s -> s =~ "Creating new database shim:") 
    |> List.length
  in
  let nb_lines = List.length xs in

  { 
    t_file = file;
    t_status = status;
    t_time = time;
    t_memory = memory;
    t_shimmed = nb_shimmed;
    t_trace_nb_lines = nb_lines;
  }

let parse_one_trace a b = 
  Common.profile_code "Phpunit.parse_one_trace" (fun () -> 
    parse_one_trace2 a b)


(*****************************************************************************)
(* Final report *)
(*****************************************************************************)

let final_report ?(report_also_pass=false) tr = 
  
  Common2.pr2_xxxxxxxxxxxxxxxxx();
  pr2 "Report";
  Common2.pr2_xxxxxxxxxxxxxxxxx();

  tr |> List.iter (fun t ->
    match t.t_status with
    | Fail (fail, _pass) ->
        pr2 (spf "FAIL: %3d,  in %s" fail t.t_file)
    | Fatal s ->
        pr2 (spf "FATAL: in %s, %s" t.t_file s)
    | Pass _ -> 
        if report_also_pass then begin 
        if t.t_time > !threshold_slow_seconds
        then
          pr2 (spf "SLOW: %3ds, in %s" t.t_time t.t_file);

        if t.t_memory > !threshold_big_mem
        then
          pr2 (spf "BIG: %02.2fMb, in %s" t.t_memory t.t_file);

        if t.t_trace_nb_lines > !threshold_long_trace
        then
          pr2 (spf "ERRORS: %d error lines, in %s" t.t_trace_nb_lines t.t_file);
        end
        
  );


  let total_fail = tr |> Common.map_filter (fun t ->
    match t.t_status with Fail (i, _) -> Some i | _ -> None) |> 
    Common2.sum_int in
  let total_pass = tr |> Common.map_filter (fun t ->
    match t.t_status with Pass (i,_) | Fail(_,i) -> Some i | _ -> None) |> 
    Common2.sum_int in
  let total_fatal = tr |> Common.map_filter (fun t ->
    match t.t_status with Fatal _-> Some 1 | _ -> None) |> 
    Common2.sum_int in

  let total_files_with_pbs = tr |> List.map (fun t ->
    match t.t_status with Fatal _ | Fail(_) -> 1 | _ -> 0) |> Common2.sum_int in
  let total_files = List.length tr in

  let total_shimmed = tr |> List.map (fun t -> t.t_shimmed) 
    |> Common2.sum_int in
    
  pr2 (spf "total shimmed: %d" total_shimmed);

  let total = total_fail + total_pass + total_fatal in
  pr2 (spf "pass  = %02.2f%% (%d)" 
          (Common2.pourcent_float total_pass total) total_pass);
  pr2 (spf "fail  = %02.2f%% (%d)" 
          (Common2.pourcent_float total_fail total) total_fail);
  pr2 (spf "fatal = %02.2f%% (%d)" 
          (Common2.pourcent_float total_fatal total) total_fatal);

  pr2 (spf "files with pbs = %02.2f%% (%d)" 
          (Common2.pourcent_float total_files_with_pbs total_files)
          total_files_with_pbs);

  ()

(*****************************************************************************)
(* JSON output *)
(*****************************************************************************)

(* auto generated by 'ocamltarzan -choice json_of phpunit.ml *)
let json_of_test_status =
  function
  | Pass (v1, v2) ->
      let v1 = J.Int v1  and v2 = J.Int v2 in
      let args = [ v1; v2 ] in J.Array ((J.String "__Pass")::args)
  | Fail ((v1, v2)) ->
      let v1 = J.Int v1 and v2 = J.Int v2 in
      let args = [ v1; v2 ] in J.Array ((J.String "__Fail")::args)
  | Fatal v1 ->
      let v1 = J.String v1 in
      let args = [ v1 ] in J.Array ((J.String "__Fatal")::args)

let json_of_test_result {
                          t_file = v_t_file;
                          t_status = v_t_status;
                          t_time = v_t_time;
                          t_memory = v_t_memory;
                          t_trace_nb_lines = v_t_trace_nb_lines;
                          t_shimmed = v_t_shimmed
                        } =
  let bnds = [] in
  let arg = J.Int v_t_shimmed in
  let bnd = ("t_shimmed", arg) in
  let bnds = bnd :: bnds in
  let arg = J.Int v_t_trace_nb_lines in
  let bnd = ("t_trace_nb_lines", arg) in
  let bnds = bnd :: bnds in
  let arg = J.Float v_t_memory in
  let bnd = ("t_memory", arg) in
  let bnds = bnd :: bnds in
  let arg = J.Int v_t_time in
  let bnd = ("t_time", arg) in
  let bnds = bnd :: bnds in
  let arg = json_of_test_status v_t_status in
  let bnd = ("t_status", arg) in
  let bnds = bnd :: bnds in
  let arg = J.String v_t_file in
  let bnd = ("t_file", arg) in let bnds = bnd :: bnds in J.Object bnds

let json_of_list of_a xs = J.Array(List.map of_a xs)

let json_of_test_results v = json_of_list json_of_test_result v

(*****************************************************************************)
(* JSON input *)
(*****************************************************************************)

module Ocamlx = struct
open OCaml

let stag_incorrect_n_args _loc tag _v = 
  failwith ("stag_incorrect_n_args on: " ^ tag)

let unexpected_stag _loc _v = 
  failwith ("unexpected_stag:")

let record_duplicate_fields _loc _dup_flds _v = 
  failwith ("record_duplicate_fields:")

let record_extra_fields _loc _flds _v =
  failwith ("record_extra_fields:")

let record_undefined_elements _loc _v _xs = 
  failwith ("record_undefined_elements:")

(*
let record_list_instead_atom _loc _v = 
  failwith ("record_list_instead_atom:")
*)

(*
let tuple_of_size_n_expected  _loc n v = 
  failwith (spf "tuple_of_size_n_expected: %d, got %s" n (Common2.dump v))
*)

(* 
 * Assumes the json was generated via 'ocamltarzan -choice json_of', which
 * have certain conventions on how to encode variants for instance.
 *)
let rec (v_of_json: Json_type.json_type -> v) = fun j ->
  match j with
  | J.String s -> VString s
  | J.Int i -> VInt i
  | J.Float f -> VFloat f
  | J.Bool b -> VBool b
  | J.Null -> raise Todo

  (* Arrays are used for represent constructors or regular list. Have to 
   * go sligtly deeper to disambiguate.
   *)
  | J.Array xs ->
      (match xs with
      (* VERY VERY UGLY. It is legitimate to have for instance tuples
       * of strings where the first element is a string that happen to
       * look like a constructor. With this ugly code we currently
       * not handle that :(
       * 
       * update: in the layer json file, one can have a filename
       * like Makefile and we don't want it to be a constructor ...
       * so for now I just generate constructors strings like
       * __Pass so we know it comes from an ocaml constructor.
       *)
       | (J.String s)::xs when s =~ "^__\\([A-Z][A-Za-z_]*\\)$" ->
           let constructor = Common.matched1 s in
           VSum (constructor, List.map v_of_json  xs)
      | ys ->
          VList (ys |> List.map v_of_json)
      )
  | J.Object flds ->
      VDict (flds |> List.map (fun (s, fld) ->
        s, v_of_json fld
      ))

end

(* I have not yet an ocamltarzan script for the of_json ... but I have one
 * for of_v, so have to pass through OCaml.v ... ugly
 *)

(* auto generated by 'ocamltarzan -choice ofv phpunit.ml *)
let test_status_ofv =
  let _loc = "Xxx.test_status"
  in
    function
    | (OCaml.VSum (((("Pass" as tag)), sexp_args)) as sexp) ->
        (match sexp_args with
         | [ v1; v2 ] ->
             let v1 = OCaml.int_ofv v1
             and v2 = OCaml.int_ofv v2
             in Pass ((v1, v2))
         | _ -> Ocamlx.stag_incorrect_n_args _loc tag sexp)

    | (OCaml.VSum (((("Fail" as tag)), sexp_args)) as sexp) ->
        (match sexp_args with
         | [ v1; v2 ] ->
             let v1 = OCaml.int_ofv v1
             and v2 = OCaml.int_ofv v2
             in Fail ((v1, v2))
         | _ -> Ocamlx.stag_incorrect_n_args _loc tag sexp)
    | (OCaml.VSum (((("Fatal" as tag)), sexp_args)) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = OCaml.string_ofv v1 in Fatal v1
         | _ -> Ocamlx.stag_incorrect_n_args _loc tag sexp)
    | sexp -> Ocamlx.unexpected_stag _loc sexp

let test_result_ofv =
  let _loc = "Xxx.test_result"
  in
    function
    | (OCaml.VDict field_sexps as sexp) ->
        let t_file_field = ref None and t_status_field = ref None
        and t_time_field = ref None and t_memory_field = ref None
        and t_trace_nb_lines_field = ref None and t_shimmed_field = ref None
        and duplicates = ref [] and extra = ref [] in
        let rec iter =
          (function
           | (field_name, field_sexp) :: tail ->
               ((match field_name with
                 | "t_file" ->
                     (match !t_file_field with
                      | None ->
                          let fvalue = OCaml.string_ofv field_sexp
                          in t_file_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "t_status" ->
                     (match !t_status_field with
                      | None ->
                          let fvalue = test_status_ofv field_sexp
                          in t_status_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "t_time" ->
                     (match !t_time_field with
                      | None ->
                          let fvalue = OCaml.int_ofv field_sexp
                          in t_time_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "t_memory" ->
                     (match !t_memory_field with
                      | None ->
                          let fvalue = OCaml.float_ofv field_sexp
                          in t_memory_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "t_trace_nb_lines" ->
                     (match !t_trace_nb_lines_field with
                      | None ->
                          let fvalue = OCaml.int_ofv field_sexp
                          in t_trace_nb_lines_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "t_shimmed" ->
                     (match !t_shimmed_field with
                      | None ->
                          let fvalue = OCaml.int_ofv field_sexp
                          in t_shimmed_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | fld ->
                     failwith ("unknown field: "^ fld)
               );
                iter tail)
           | [] -> ())
        in
          (iter field_sexps;
           if !duplicates <> []
           then Ocamlx.record_duplicate_fields _loc !duplicates sexp
           else
             if !extra <> []
             then Ocamlx.record_extra_fields _loc !extra sexp
             else
               (match ((!t_file_field), (!t_status_field), (!t_time_field),
                       (!t_memory_field), (!t_trace_nb_lines_field),
                       (!t_shimmed_field))
                with
                | (Some t_file_value, Some t_status_value, Some t_time_value,
                   Some t_memory_value, Some t_trace_nb_lines_value,
                   Some t_shimmed_value) ->
                    {
                      t_file = t_file_value;
                      t_status = t_status_value;
                      t_time = t_time_value;
                      t_memory = t_memory_value;
                      t_trace_nb_lines = t_trace_nb_lines_value;
                      t_shimmed = t_shimmed_value;
                    }
                | _ ->
                    Ocamlx.record_undefined_elements _loc sexp
                      [ ((!t_file_field = None), "t_file");
                        ((!t_status_field = None), "t_status");
                        ((!t_time_field = None), "t_time");
                        ((!t_memory_field = None), "t_memory");
                        ((!t_trace_nb_lines_field = None),
                         "t_trace_nb_lines");
                        ((!t_shimmed_field = None), "t_shimmed") ]))
    | _sexp -> failwith "was expecting a VDict"


let test_results_ofv =
  let _loc = "Xxx.test_results"
  in fun sexp -> OCaml.list_ofv test_result_ofv sexp


(* finally can now load test results from a JSON data *)
let test_results_of_json json =
  let v = Ocamlx.v_of_json json in
  let tr = test_results_ofv v in
  tr

(*****************************************************************************)
(* Regression *)
(*****************************************************************************)

let gen_regression_filename files_or_dirs =
  let files_or_dirs = files_or_dirs |> List.map Common2.chop_dirsymbol in

  let str = 
    files_or_dirs |> Common.join "___" 
    |> Str.global_replace (Str.regexp "/") "__"  
  in

  (* ext3 does not like too long filenames *)  
  if String.length str > 50
  then 
    let md = Common2.md5sum_of_string str in
    (String.sub str 0 40) ^ md
  else 
    str


let regression ~regression_file tr = 

  let newscore  = Common2.empty_score () in

  tr |> List.iter (fun t ->
    let score = 
      match t.t_status with
      | Pass _ -> Common2.Ok
      | Fail (i,j) -> Common2.Pb (spf "Fail: (%d, %d)" i j)
      | Fatal s    -> Common2.Pb (spf "Fatal: %s" s)
    in
    Hashtbl.add newscore t.t_file score;
  );
        
  pr2 "--------------------------------";
  pr2 "functional regression information";
  pr2 "--------------------------------";
  Common2.regression_testing newscore regression_file;

  ()

(*****************************************************************************)
(* Perf Regression *)
(*****************************************************************************)
let regression_perf ~regression_file tr = 
  let newscore  = Common2.empty_score () in

  tr |> List.iter (fun t ->
    let score_opt = 
      match t.t_status with
      | Pass _ -> 
          (match test_speed_of_int t.t_time, test_space_of_float t.t_memory with
          | (Fast | NormalSpeed), (SmallMem | NormalMem) ->
              Some Common2.Ok
          | Slow, _ ->
              Some (Common2.Pb "SLOW")
          | _, BigMem ->
              Some (Common2.Pb "BIGMEM")
          )
      (* prefer to not include failing tests. They will be reported already
       * by the other regressions
       *)
      | Fail (_i,_j) -> 
          None
      | Fatal _s -> 
          None
    in
    score_opt |> Common.do_option (fun score ->
      Hashtbl.add newscore t.t_file score;
    )
  );
        
  pr2 "--------------------------------";
  pr2 "performance regression  information";
  pr2 "--------------------------------";
  Common2.regression_testing newscore regression_file;
  ()


(*e: phpunit.ml *)

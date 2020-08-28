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
open Common2
open Common

open Cst_php

open Coverage_code

module V = Visitor_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * We can try to mimic Microsoft Echelon[1] project which given a patch
 * try to run the most relevant tests that could be affected by the
 * patch. It is probably easier in PHP thanks to the excellent xdebug
 * tracer. We can even run the tests and says whether the new code has
 * been covered (like in MySql test infrastructure).
 * 
 * For now we just generate given a mapping from 
 * a source code file to a list of relevant test files. 
 * 
 * See also analyze_php/coverage_(static|dynamic)_php.ml and
 * test_rank.ml which use coverage information too.
 * 
 * You first need to have xdebug ON on your machine. See xdebug.ml
 * For explanations. You can then check if xdebug is on by running 'php -v'.
 * 
 * References:
 *  [1] http://research.microsoft.com/apps/pubs/default.aspx?id=69911
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* used internally by test_coverage below *)
type test_cover_result = 
  | Cover of Common.filename * (* the test *)
      (Common.filename (* source *) * 
       int (* number of occurences in the trace *)) 
      list
  | Problem of Common.filename * string (* error message *)

(* In the past lots of tests were failing but we still wanted
 * to generate coverage data. Now, sometimes the codebase is broken which
 * makes all the tests failing but we were still generate an empty coverage
 * data. Better to return an exception when we detected something
 * went wrong.
 *)
let threshold_working_tests_percentage = ref 80.0 

let threshold_nblines_trace_too_big = ref 1_000_000

let timeout_run_test = 100

exception NotEnoughWorkingTests

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let _hnblines = Hashtbl.create 101
let nblines_with_wc_cached file = 
  Common.memoized _hnblines file (fun () ->
    Common2.nblines_with_wc file
  )
(* 
 * To print coverage statistics, we need a base number.
 * Xdebug report in the trace only function/method calls so
 * we need to extract all the possible call sites to have
 * this base number.
 * 
 * note: Xdebug uses the line number of the closing ')' so we do the same here.
 *)
let get_all_calls ?(is_directive_to_filter= (fun _ -> false)) =
  V.do_visit_with_ref (fun aref -> { V.default_visitor with

    V.kexpr = (fun (k, _) x ->
      match x with
      | New (tok, _class_name_ref, _args_opt) ->
          (* can not use ')' here, so use the token for new *)
          Common.push (None, tok) aref;

          k x;

      | Call (Id callname, (_, _args, rp)) ->
          let str = Cst_php.str_of_name callname in
          
          (* filter the require_module stuff that already skip
           * when generating the file coverage data
           *)
          if is_directive_to_filter str then ()
          else
            Common.push (Some str, rp) aref;

          k x

      | Call (ClassGet(_var, _t1, Id methname), (_lp, _args, rp))
      | Call (ObjGet(_var, _t1, Id methname), (_lp, _args, rp)) 
        ->
          let str = Cst_php.str_of_name methname in
          Common.push (Some str, rp) aref;
          k x


      | _ -> k x
    );
  })

let get_all_call_lines_with_sanity_check 
     ?is_directive_to_filter file lines_covered =

  (* don't know why but sometimes 0 is in the trace *)
  let lines_covered = lines_covered |> Common.exclude (fun x -> x = 0) in
      
  let nb_lines_covered = List.length lines_covered in
      
  let ast = 
    try Parse_php.parse_program file 
    with _exn ->
      pr2 (spf "PB: cant parse %s" file);
      []
  in
      
  let calls = get_all_calls ?is_directive_to_filter (Program ast) in
     
  let lines_calls = 
    calls 
    |> List.map (fun (_sopt, rp) -> Parse_info.line_of_info rp)
    |> Common2.set
  in
  let nb_lines_calls = List.length lines_calls in
      
  (* first sanity check *)
  if nb_lines_calls < nb_lines_covered
    (* apparently php scripts have wrong line nunber information with 
     * xdebug
     *)
    && not (Lib_parsing_php.is_php_script file)
  then begin 
    pr2 ("PB: xdebug reported more calls than there is in " ^ file);
    
    let diff = 
      lines_covered $-$ lines_calls 
    in
    let lines = 
      try 
        Common2.cat_excerpts file diff
      with
      exn -> [Common.exn_to_s exn]
    in
    lines |> List.iter pr2;
    pr2 "PB: fix get_all_calls";
    
  end;
  (* TODO: second sanity check, check that talk about same lines ? *)
  lines_calls


let killall_php_process () = 
  pr2 "Remaining php process";
  Sys.command ("ps aux |grep php") |> ignore;
  pr2 "killing";
  Sys.command ("killall php") |> ignore;
  pr2 "Still remaining php process ?";
  Sys.command ("ps aux |grep php") |> ignore;
  ()

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

(* algo:
 *  - get set of test files in testdir 
 *  - get the corresponding command to run the test
 *  - run the test with xdebug on (in light mode)
 *  - filter only working tests (don't want to blame a committer for
 *    an error made by someone else)
 *  - analyze the trace to extract file coverage
 *  - do global analysis to return for all sources the set of 
 *    relevant tests (can also be used to detect code that is not
 *    covered at all)
 *  - rank the relevant tests (using term frequency, if a file is mentionned
 *    a lot in the trace of a test, then this test is more relevant to the
 *    file)
 *  - generate JSON data so that other program can use this information
 *    (e.g. 'arc unit')
 * 
 *  - optional: parallelize and distribute the computation with MPI
 *)
let coverage_tests 
 ?(phpunit_parse_trace = Phpunit.parse_one_trace)
 ?(skip_call = (function _call -> false))
 ~php_cmd_run_test
 ~all_test_files
 ()
 = 

  (* I am now using commons/distribution.ml and the map_reduce function
   * to possibly distribute the coverage computation using MPI. Note that the
   * same program can also be used without MPI, so that all the
   * distribution/parallelisation is mostly transparent to the
   * programmer (thanks to commons/distribution.ml) *)

  (* Note that the MPI workers are started only when the code 
   * reach Distribution.map_reduce below, which means that if the 
   * program crash before, the MPI infrastructure would have not been
   * started yet and you will see no useful error output.
   * So limit the amount of code to run before the map_reduce call.
   *)

  let test_files_fn () = 
    pr2 "computing set of test files";
    let xs = all_test_files () in
    pr2 (spf "%d test files found" (List.length xs));
    Common2.index_list_and_total xs
  in

  (* using a map reduce model *)
  let (mapper: (filename * int * int) -> test_cover_result) = 
   fun (test_file, i, total) ->
    let trace_file = Common.new_temp_file "xdebug" ".xt" in
    Common.finalize (fun () ->
    pr2 (spf "processing: %s (%d/%d)" test_file i total);
    pr2 (Common2.get_mem());

    if not (Xdebug.php_has_xdebug_extension ())
    then failwith "xdebug is not properly installed";

    (* run with xdebug tracing in a "light" mode, which have less information, 
     * but which generate small traces and so leads to faster trace analysis.
     *)
    let config = { Xdebug.default_config with
      Xdebug.collect_return = false;
      Xdebug.collect_params = Xdebug.NoParam;
    }
    in

    let php_interpreter = 
      Xdebug.php_cmd_with_xdebug_on ~trace_file ~config () in
    let cmd = php_cmd_run_test ~php_interpreter test_file in
    pr2 (spf "executing: %s" cmd);

    try (
    let output_cmd = 
      Common.profile_code "Run PHP tests" (fun () ->
        Common.timeout_function timeout_run_test (fun () ->
          Common.cmd_to_list cmd 
        )
      )
    in
    let test_result = 
      phpunit_parse_trace test_file output_cmd in

    match test_result.Phpunit.t_status with
    | Phpunit.Pass _ -> 

        let h = Common2.hash_with_default (fun () -> 0) in

        let nblines = Common2.nblines_with_wc trace_file in

        pr2 (spf " trace length = %d lines, xdebug trace = %d lines" 
                (List.length output_cmd) nblines);

        if nblines > !threshold_nblines_trace_too_big
        then 
          Problem (test_file, "trace file too big")
        else begin
          trace_file |> Xdebug.iter_dumpfile 
            ~config 
            ~show_progress:false
            ~fatal_when_exn:true
            (fun call ->
              if skip_call call then ()
              else
                let file_called = call.Xdebug.f_file in 
                h#update file_called (fun old -> old + 1)
            );
          Cover (test_file, h#to_list)
        end
    | Phpunit.Fail _ | Phpunit.Fatal _ -> 
        (* I should normally print the failing test output, 
         * but some tests generates really weird binary output
         * that makes OCaml raise a Sys_blocked_io exception.
         * 
         * old: output_cmd +> List.iter pr2; 
         *)
        Problem (test_file, "failing or fataling test")
    )
   with Timeout ->
     (* those files can get huge *)
     pr2 (spf "PB with %s" test_file);
     (* todo: does not work when use MPI *)
     killall_php_process (); 
     Problem (test_file, "timeout when running test and computing coverage")
    ) 
      (fun () ->
        Common.erase_this_temp_file trace_file;
      )

  in

  (* regular_php_file -> hash_of_relevant_test_files_with_score *)
  let h = Common2.hash_with_default (fun () -> 
    Hashtbl.create 101
  )
  in
  let ok_test_files = ref 0 in
  let pb_test_files = ref [] in

  (* Right now the reducer is executed on a single machine, the master,
   * so I can (ab)use the 2 preceding globals. See the code in
   * distribution.ml.
   *)
  let reducer _acc test_cover_result = 
    match test_cover_result with
    | Cover (test_file, files_called) ->
        let total = files_called |> List.map snd |> Common2.sum in
        files_called |> List.iter (fun (file_called, nb_occurences) ->
          h#update file_called (fun hbis -> 
            Hashtbl.replace hbis test_file 
              (* "term" frequency *)
              (Common2.pourcent_float nb_occurences total);
            hbis
          );
        );
        incr ok_test_files;
    | Problem (test_file, msg) -> 
        Common.push (test_file, msg) pb_test_files;
  in
  let _res, not_done = 
    (test_files_fn, reducer, mapper) |> ignore;
    failwith "TODO: Use Features.Distribution"
(*
    Features.Distribution.map_reduce_lazy 
      ~fmap:mapper ~freduce:reducer () test_files_fn
*)
  in
  not_done |> List.iter (fun test_file ->
    Common.push (test_file, "MPI error, see the full log") pb_test_files;
  );

  pr2 "test dependencies";
  let coverage = 
    h#to_list |> List.map (fun (source_file, h) ->
      let tests = h 
      |> Common.hash_to_list 
      |> Common.sort_by_val_highfirst
      in
      (source_file, tests)
    )
  in

  (* error report *)
  !pb_test_files |> List.rev |> List.iter (fun (test_file, error) ->
    pr2 (spf "PB with %s, \n\t%s" test_file error);
  );
  let good = !ok_test_files in
  let bad = List.length (!pb_test_files) in
  let total = good + bad in
  let percent = Common2.pourcent_float good total in
  pr2 (spf "Coverage: %d/%d tests (%.02f%%)" 
          good total percent);

  if percent < !threshold_working_tests_percentage
  then raise NotEnoughWorkingTests;

  coverage, !pb_test_files
    




(* For precise file/line coverage it does make less sense to use a map/reduce
 * model and distribute computations as we need to produce a global
 * file->covered_lines assoc, which would force in each mapper to 
 * produce such an assoc and then in the reducer to unionize them which
 * would be costly. We really need a global here.
 *)
let lines_coverage_from_tests
 ?(skip_call = (function _call -> false))
 ?is_directive_to_filter (* TODO: merge with skip_call *)
 ~php_cmd_run_test
 ~all_test_files
 ~all_files
 ()
 = 
  (* file -> hashset of covered lines *)
  let h = Common2.hash_with_default (fun () -> 
    Hashtbl.create 101
  ) in

  all_test_files |> Common2.index_list_and_total |> List.iter 
  (fun (test_file, i, total) ->
    let trace_file = Common.new_temp_file "xdebug" ".xt" in
    Common.finalize (fun () ->
    pr2 (spf "processing test: %s (%d/%d)" test_file i total);

    if not (Xdebug.php_has_xdebug_extension ())
    then failwith "xdebug is not properly installed";

    (* run with xdebug tracing in a "light" mode, which have less information, 
     * but which generate small traces and so leads to faster trace analysis.
     *)
    let config = { Xdebug.default_config with
      Xdebug.collect_return = false;
      Xdebug.collect_params = Xdebug.NoParam;
    }
    in

    let php_interpreter = 
      Xdebug.php_cmd_with_xdebug_on ~trace_file ~config () in
    let cmd = php_cmd_run_test ~php_interpreter test_file in
    pr2 (spf "executing: %s" cmd);

    try (
    let output_cmd = 
      Common.profile_code "Run PHP tests" (fun () ->
        Common.timeout_function timeout_run_test (fun () ->
          Common.cmd_to_list cmd 
        )
      )
    in
    pr2 (spf " trace length = %d lines, xdebug trace = %d lines" 
            (List.length output_cmd)
            (Common2.nblines_with_wc trace_file)
    );

    trace_file |> Xdebug.iter_dumpfile 
      ~config 
      ~show_progress:false
      ~fatal_when_exn:true
      (fun call ->
        if skip_call call then ()
        else begin
          let file_called = call.Xdebug.f_file in 

          if not (Sys.file_exists file_called)
          then failwith 
            (spf "WEIRD: coverage contain reference to weird file: '%s'" 
                file_called);

          let line = call.Xdebug.f_line in

          if line <= 0 || line > nblines_with_wc_cached file_called
          then failwith ("WEIRD: call lines not in file: " ^ 
                            Xdebug.string_of_call_trace call);

          h#update file_called (fun oldh -> 
            Hashtbl.replace oldh line true; 
            oldh
          )
        end
      );
    )
   with Timeout ->
     pr2 (spf "PB with %s, timeout" test_file);
     killall_php_process (); 
  ) (fun () ->
    Common.erase_this_temp_file trace_file;
    )
  );
    
  (* some sanity checks *)
  let h_all_files = Common.hashset_of_list all_files in
  h#to_list |> List.iter (fun (file, _hset) ->
    if not (Hashtbl.mem h_all_files file)
    then pr2
      (spf "file with coverage information %s not in list of files" file);
  );

  all_files |> Common2.index_list_and_total |> List.map (fun (file, i, total) ->
    pr2 (spf "processing source: %s (%d/%d)" file i total);
    let covered = 
      try 
        let hset = h#assoc file in
        Common.hashset_to_list hset
      with Not_found -> []
    in
    file, {
      covered_sites = covered;
      all_sites = 
        get_all_call_lines_with_sanity_check ?is_directive_to_filter 
          file covered;
    }
  )


(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () = [

  (* can test via 
   *    mpirun -np 5 ./qa_test -debug_mpi -test_mpi
   * for local multi processing, or 
   *    mpirun -np 20 -H unittest006,unittest005 ./qa_test -debug_mpi -test_mpi
   * for distributed processing, or simply with
   *    ./qa_test -test_mpi 
   * for local classic ocaml single processing
   *)
  "-test_mpi", "",
  Common.mk_action_0_arg (fun () ->
    let rec fib n = 
      if n = 0 then 0
      else 
        if n = 1 then 1
      else fib (n-1) + fib (n-2)
    in
    let map_ex arg = 
      pr (spf "map: %d" arg);
      fib arg
    in
    let reduce_ex acc e = 
      pr (spf "reduce: acc=%d, e=%d" acc e);
      acc + e
    in
    let res, notdone = 
      (map_ex, reduce_ex) |> ignore;
      failwith "TODO: Use Features.Distribution"
(*
      Features.Distribution.map_reduce ~fmap:map_ex ~freduce:reduce_ex 
      0 [35;35;35;35] 
*)
  in
    pr (spf "result = %d, #not_done = %d" res (List.length notdone));
  );
]

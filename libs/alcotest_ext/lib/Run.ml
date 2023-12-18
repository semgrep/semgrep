(*
   Filter and run tests
*)

open Printf
module T = Types

type status_output_style = Short | Full

type status_stats = {
  total_tests : int;
  selected_tests : int;
  pass : int ref;
  fail : int ref;
  xfail : int ref;
  xpass : int ref;
  miss : int ref;
  needs_approval : int ref;
}

type success = OK | OK_where_no_missing_data | Not_OK

(*
   Check that no two tests have the same full name or the same ID.
*)
let check_id_uniqueness (tests : _ T.test list) =
  let id_tbl = Hashtbl.create 1000 in
  tests
  |> List.iter (fun (test : _ T.test) ->
         let id = test.id in
         let name = test.internal_full_name in
         match Hashtbl.find_opt id_tbl id with
         | None -> Hashtbl.add id_tbl id test.internal_full_name
         | Some name0 ->
             if name = name0 then
               failwith (sprintf "Two tests have the same name: %s" name)
             else
               failwith
                 (sprintf
                    "Hash collision for two tests with different names:\n\
                    \  %S\n\
                    \  %S\n\
                     These names result in the same hash ID: %s\n\
                     If this is accidental, please report the problem to the \
                     authors of\n\
                     alcotest_ext."
                    name0 name id))

let string_of_status_summary (sum : T.status_summary) =
  let approval_suffix = if sum.has_expected_output then "" else "*" in
  match sum.status_class with
  | PASS -> "PASS" ^ approval_suffix
  | FAIL -> "FAIL" ^ approval_suffix
  | XFAIL -> "XFAIL" ^ approval_suffix
  | XPASS -> "XPASS" ^ approval_suffix
  | MISS -> "MISS"

let success_of_status_summary (sum : T.status_summary) =
  match sum.status_class with
  | PASS
  | XFAIL ->
      if sum.has_expected_output then OK else OK_where_no_missing_data
  | FAIL -> Not_OK
  | XPASS -> Not_OK
  | MISS -> OK_where_no_missing_data

let style_of_status_summary (sum : T.status_summary) : Color.style =
  match success_of_status_summary sum with
  | OK -> Green
  | OK_where_no_missing_data -> Yellow
  | Not_OK -> Red

let brackets s = sprintf "[%s]" s

(* Fixed-width output: "[PASS] ", "[XFAIL]" *)
let format_status_summary (sum : T.status_summary) =
  let style = style_of_status_summary sum in
  let displayed_string = sum |> string_of_status_summary |> brackets in
  let padding = max 0 (7 - String.length displayed_string) in
  sprintf "%s%s"
    (Color.format Color style displayed_string)
    (String.make padding ' ')

let stats_of_tests tests tests_with_status =
  let stats =
    {
      total_tests = List.length tests;
      selected_tests = List.length tests_with_status;
      pass = ref 0;
      fail = ref 0;
      xfail = ref 0;
      xpass = ref 0;
      miss = ref 0;
      needs_approval = ref 0;
    }
  in
  tests_with_status
  |> List.iter (fun (_test, _status, (sum : T.status_summary)) ->
         (match sum.status_class with
         | MISS -> ()
         | _ -> if not sum.has_expected_output then incr stats.needs_approval);
         incr
           (match sum.status_class with
           | PASS -> stats.pass
           | FAIL -> stats.fail
           | XFAIL -> stats.xfail
           | XPASS -> stats.xpass
           | MISS -> stats.miss));
  stats

(* Sample output: "", " {foo, bar}" *)
let format_tags (test : _ T.test) =
  match test.tags with
  | [] -> ""
  | tags ->
      let tags = List.sort Tag.compare tags |> Helpers.list_map Tag.to_string in
      sprintf " {%s}" (String.concat ", " tags)

(*
   Group pairs by the first value of the pair, preserving the original
   order as much as possible.
*)
let group_by_key key_value_list =
  let tbl = Hashtbl.create 100 in
  key_value_list
  |> List.iteri (fun pos (k, v) ->
         let tbl_v =
           match Hashtbl.find_opt tbl k with
           | None -> (pos, [ v ])
           | Some (pos, vl) -> (pos, v :: vl)
         in
         Hashtbl.replace tbl k tbl_v);
  let clusters =
    Hashtbl.fold (fun k (pos, vl) acc -> (pos, (k, List.rev vl)) :: acc) tbl []
  in
  clusters
  |> List.sort (fun (pos1, _) (pos2, _) -> compare pos1 pos2)
  |> Helpers.list_map snd

let chdir_error (test : _ T.test) =
  if test.tolerate_chdir then None
  else
    Some
      (fun old new_ ->
        sprintf "Current working directory (cwd) wasn't restored: %s -> %s" old
          new_)

(*
   Protect against tests that mutate global variables.

   A way to do this would be to fork off a new process for each test but
   it's probably too costly and prevents the test from using hacks relying
   on globals (such as the lazy initialization of some shared resource).

   The Alcotest_ext system needs at least the relative path to its own
   files to remain valid even if a test changes the current directory
   by a call to chdir. An alternative way to do that would be to use
   absolute paths but it makes error messages a little uglier.

   Some other mutable globals could also be protected. They include:
   - environment variables
   - whether the stack backtrace should be recorded
   - terminal settings (affecting color output)
   - ...

   TODO: add options at test creation time to tolerate the failure to restore
   this or that setting.
*)
let protect_globals test (func : unit -> 'a) : unit -> 'a =
  let protect_global ?error_if_changed get set func () =
    let original_value = get () in
    (* nosemgrep: no-fun-protect *)
    Fun.protect func ~finally:(fun () ->
        let current_value = get () in
        set original_value;
        match error_if_changed with
        | Some err_msg_func when current_value <> original_value ->
            Alcotest.fail (err_msg_func original_value current_value)
        | _ -> ())
  in
  func
  |> protect_global Sys.getcwd Sys.chdir ?error_if_changed:(chdir_error test)
  |> protect_global Printexc.backtrace_status Printexc.record_backtrace
(* TODO: more universal settings to protect? *)

(* TODO: remove this code duplication by either removing support for Lwt
   (do we really need it?) or some other abtraction mechanism like a functor
   or a polymorphic record providing bind, return, catch, etc. *)
let protect_globals_lwt test (func : unit -> 'a Lwt.t) : unit -> 'a Lwt.t =
  let protect_global ?error_if_changed get set func () =
    let original_value = get () in
    Lwt.finalize func (fun () ->
        let current_value = get () in
        set original_value;
        (match error_if_changed with
        | Some err_msg_func when current_value <> original_value ->
            Alcotest.fail (err_msg_func original_value current_value)
        | _ -> ());
        Lwt.return ())
  in
  func
  |> protect_global Sys.getcwd Sys.chdir ?error_if_changed:(chdir_error test)
  |> protect_global Printexc.backtrace_status Printexc.record_backtrace
(* TODO: more universal settings to protect? *)

let to_alcotest_generic
    ~(wrap_test_function : 'a T.test -> (unit -> 'a) -> unit -> 'a)
    (tests : 'a T.test list) : _ list =
  tests
  |> Helpers.list_map (fun (test : _ T.test) ->
         let suite_name =
           match test.category with
           | [] -> test.name
           | path -> String.concat " > " path
         in
         let xfail_note =
           match test.expected_outcome with
           | Should_succeed -> ""
           | Should_fail _reason -> " [xfail]"
         in
         let suite_name =
           sprintf "%s%s%s %s" test.id xfail_note (format_tags test) suite_name
         in
         let func =
           (*
         A "skipped" test is marked as skipped in Alcotest's run output
         and leaves no trace such that Alcotest_ext thinks it never ran.
       *)
           if test.skipped then Alcotest.skip
           else wrap_test_function test test.func
         in
         (* This is the format expected by Alcotest: *)
         (suite_name, (test.name, test.speed_level, func)))
  |> group_by_key

let with_flip_xfail_outcome (test : _ T.test) func =
  match test.expected_outcome with
  | Should_succeed -> func
  | Should_fail _reason -> (
      fun () ->
        try
          func ();
          failwith "This test failed to raise an exception."
        with
        | exn ->
            eprintf "As expected, an exception was raised: %s\n"
              (Printexc.to_string exn))

let with_flip_xfail_outcome_lwt (test : _ T.test) func =
  match test.expected_outcome with
  | Should_succeed -> func
  | Should_fail _reason ->
      fun () ->
        Lwt.catch
          (fun () ->
            Lwt.bind (func ()) (fun () ->
                failwith "This test failed to raise an exception"))
          (fun exn ->
            eprintf "As expected, an exception was raised: %s\n"
              (Printexc.to_string exn);
            Lwt.return ())

let conditional_wrap condition wrapper func =
  if condition then wrapper func else func

let to_alcotest_internal ~with_storage ~flip_xfail_outcome tests =
  let wrap_test_function test func =
    func
    |> conditional_wrap flip_xfail_outcome (with_flip_xfail_outcome test)
    |> protect_globals test
    |> conditional_wrap with_storage (Store.with_result_capture test)
  in
  to_alcotest_generic ~wrap_test_function tests

let to_alcotest_lwt_internal ~with_storage ~flip_xfail_outcome tests =
  let wrap_test_function test func =
    func
    |> conditional_wrap flip_xfail_outcome (with_flip_xfail_outcome_lwt test)
    |> protect_globals_lwt test
    |> conditional_wrap with_storage (Store.with_result_capture_lwt test)
  in
  to_alcotest_generic ~wrap_test_function tests

(* Exported versions that exposes a plain Alcotest test suite that doesn't
   write test statuses and prints "OK" for XFAIL statuses. *)
let to_alcotest =
  to_alcotest_internal ~with_storage:false ~flip_xfail_outcome:true

let to_alcotest_lwt =
  to_alcotest_lwt_internal ~with_storage:false ~flip_xfail_outcome:true

let contains_regexp pat =
  let rex = Re.Pcre.regexp pat in
  fun str -> Re.execp rex str

let filter ?filter_by_substring tests =
  let tests =
    match filter_by_substring with
    | None -> tests
    | Some sub ->
        let contains_substring = contains_regexp (Re.Pcre.quote sub) in
        tests
        |> List.filter (fun (test : _ T.test) ->
               contains_substring test.internal_full_name
               || contains_substring test.id)
  in
  tests

(* Returns an exit code *)
let print_errors (xs : (_, string) Result.t list) : int =
  let error_messages =
    xs
    |> List.filter_map (function
         | Ok _ -> None
         | Error msg -> Some msg)
  in
  match error_messages with
  | [] -> 0
  | xs ->
      let n_errors = List.length xs in
      let error_str =
        if n_errors >= 2 then Color.format Color Red "Errors:\n"
        else Color.format Color Red "Error: "
      in
      let msg = String.concat "\n" error_messages in
      eprintf "%s%s\n%!" error_str msg;
      1

let is_important_status ((test : _ T.test), _status, (sum : T.status_summary)) =
  (not test.skipped)
  && ((not sum.has_expected_output)
     ||
     match success_of_status_summary sum with
     | OK -> false
     | OK_where_no_missing_data
     | Not_OK ->
         true)

let diff_output (test : _ T.test)
    (output_file_pairs : Store.output_file_pair list) =
  output_file_pairs
  |> List.iter
       (fun
         ({ short_name; path_to_expected_output; path_to_output } :
           Store.output_file_pair)
       ->
         flush stdout;
         flush stderr;
         match
           (* nosemgrep: forbid-exec *)
           Sys.command
             (sprintf "diff -u --color '%s' '%s'" path_to_expected_output
                path_to_output)
         with
         | 0 -> ()
         | _nonzero ->
             printf "  Captured %s differs from expectation for test %s %s\n"
               short_name test.id test.internal_full_name;
             printf "  Path to expected output: %s\n" path_to_expected_output;
             printf "  Path to latest output: %s\n" path_to_output);
  print_newline ()

let print_status ?(output_style = Full)
    ((test : _ T.test), (status : T.status), sum) =
  printf "%s %s%s %s\n"
    (format_status_summary sum)
    test.id (format_tags test) test.internal_full_name;

  match output_style with
  | Short -> ()
  | Full ->
      if (* Details about expectations *)
         test.skipped then printf "  always skipped\n"
      else (
        (match status.expectation.expected_outcome with
        | Should_succeed -> ()
        | Should_fail reason -> printf "  expected to fail: %S\n" reason);
        (match test.output_kind with
        | Ignore_output -> ()
        | _ ->
            let text =
              match test.output_kind with
              | Ignore_output -> assert false
              | Stdout -> "stdout"
              | Stderr -> "stderr"
              | Merged_stdout_stderr -> "merged stdout and stderr"
              | Separate_stdout_stderr -> "separate stdout and stderr"
            in
            printf "  checked output: %s\n" text);
        (* Details about results *)
        match status.expectation.expected_output with
        | Error msg ->
            printf "Missing file(s) containing the expected output: %s\n" msg
        | Ok expected_output -> (
            match status.result with
            | Error msg ->
                printf "Missing file(s) containing the test output: %s\n" msg
            | Ok result ->
                if result.captured_output <> expected_output then
                  let output_file_pairs = Store.get_output_file_pairs test in
                  diff_output test output_file_pairs))

let print_statuses ~only_important ~output_style tests_with_status =
  let tests_with_status =
    if only_important then List.filter is_important_status tests_with_status
    else tests_with_status
  in
  tests_with_status |> List.iter (print_status ~output_style)

let is_overall_success statuses =
  statuses
  |> List.for_all (fun (_test, _status, sum) ->
         match sum |> success_of_status_summary with
         | OK
         | OK_where_no_missing_data ->
             true
         | Not_OK -> false)

(*
   Status output:
   0. Introduction: explain how to read the output.
   1. Long status: for each selected test, show all the details one might
      want to know about the test.
   2. Short status: for each selected test that's considered important
      (= needs our attention), list a short summary of the test status.
   3. Summary: give the counts for each test with a particular state.

   Options:
   --full: all of the above, the default
   --short: show only short status and summary
*)
let print_status_introduction () =
  printf
    {|The status of completed tests is reported below as one of four kinds:
- PASS: a successful test that was expected to succeed (good);
- FAIL: a failing test that was expected to succeed (needs fixing);
- XFAIL: a failing test that was expected to fail (tolerated failure);
- XPASS: a successful test that was expected to fail (progress?).
Other states:
- MISS: a test that never ran;
- xxxx*: a new test for which the expected output is missing.
  In this case, you should review the test output and run the 'approve'
  subcommand once you're satisfied with the output.
|}

let print_short_status tests_with_status =
  print_endline (Color.format Color Bold "Short status");
  print_statuses ~only_important:true ~output_style:Short tests_with_status

let print_long_status tests_with_status =
  print_endline (Color.format Color Bold "Long status");
  print_statuses ~only_important:false ~output_style:Full tests_with_status

let plural num = if num >= 2 then "s" else ""

let print_status_summary tests tests_with_status =
  let stats = stats_of_tests tests tests_with_status in
  let overall_success = is_overall_success tests_with_status in
  printf
    "%i/%i selected test%s:\n\
    \  %i successful (%i pass, %i xfail),\n\
    \  %i unsuccessful (%i fail, %i xpass),\n\
     %i new test%s\n\
     %i test%s whose output needs first-time approval\n\
     overall status: %s\n"
    stats.selected_tests stats.total_tests (plural stats.total_tests)
    (!(stats.pass) + !(stats.xfail))
    !(stats.pass) !(stats.xfail)
    (!(stats.fail) + !(stats.xpass))
    !(stats.fail) !(stats.xpass) !(stats.miss) (plural !(stats.miss))
    !(stats.needs_approval)
    (plural !(stats.needs_approval))
    (if overall_success then Color.format Color Green "success"
     else Color.format Color Red "failure");
  if overall_success then 0 else 1

let print_full_status tests tests_with_status =
  print_status_introduction ();
  print_newline ();
  print_long_status tests_with_status;
  print_newline ();
  print_short_status tests_with_status;
  print_status_summary tests tests_with_status

let print_short_status tests tests_with_status =
  print_short_status tests_with_status;
  print_status_summary tests tests_with_status

let get_tests_with_status tests =
  tests
  |> Helpers.list_map (fun test ->
         let status = Store.get_status test in
         (test, status, Store.status_summary_of_status status))

(*
   Entry point for the status subcommand
*)
let list_status ?filter_by_substring ?(output_style = Full) tests =
  check_id_uniqueness tests;
  let selected_tests = filter ?filter_by_substring tests in
  let tests_with_status = get_tests_with_status selected_tests in
  let exit_code =
    match output_style with
    | Full -> print_full_status tests tests_with_status
    | Short -> print_short_status tests tests_with_status
  in
  (exit_code, tests_with_status)

(* Important: for unknown reasons, the "test" subcommand of alcotest
   force an exit after Alcotest.run, regardless of the 'and_exit' argument
   that we pass.

   '-e': synonym for '--show-errors' causing the output of all failing
         tests to be shown (rather than just one if I remember correctly).
*)
let alcotest_argv = [| ""; "-e" |]

let run_tests_with_alcotest tests =
  let alcotest_tests =
    to_alcotest_internal ~with_storage:true ~flip_xfail_outcome:false tests
  in
  (try
     Alcotest.run ~and_exit:false ~argv:alcotest_argv "test" alcotest_tests
   with
  | Alcotest.Test_error -> ());
  Format.print_flush ()

let run_tests_with_alcotest_lwt tests =
  let alcotest_tests =
    to_alcotest_lwt_internal ~with_storage:true ~flip_xfail_outcome:false tests
  in
  Lwt.finalize
    (fun () ->
      Lwt.catch
        (fun () ->
          Lwt.bind
            (Alcotest_lwt.run ~and_exit:false ~argv:alcotest_argv "test"
               alcotest_tests) (fun () -> Lwt.return ()))
        (function
          | Alcotest.Test_error -> Lwt.return ()
          | e -> raise e))
    (fun () ->
      Format.print_flush ();
      Lwt.return ())

(* Run this before a run or Lwt run. Returns the filtered tests. *)
let before_run ?filter_by_substring ?(lazy_ = false) tests =
  Store.init_workspace ();
  check_id_uniqueness tests;
  let tests =
    match lazy_ with
    | false -> tests
    | true ->
        (* Read the status of each test so we can skip them *)
        let tests_with_status = get_tests_with_status tests in
        List.filter is_important_status tests_with_status
        |> Helpers.list_map (fun (test, _, _) -> test)
  in
  print_status_introduction ();
  let selected_tests = tests |> filter ?filter_by_substring in
  (* It would probably be less confusing to report the 4-way outcome here
     (pass/fail/xfail/xpass) but we can't as long as we rely on Alcotest
     for running and printing test results. *)
  print_endline (Color.format Color Bold "Test run");
  print_endline
    "In this section, tests are reported as either OK or FAIL without looking\n\
     at the expected outcome or expected output.";
  selected_tests

(* Run this after a run or Lwt run. *)
let after_run tests selected_tests =
  let tests_with_status = get_tests_with_status selected_tests in
  let exit_code = print_full_status tests tests_with_status in
  (exit_code, tests_with_status)

(*
   Entry point for the 'run' subcommand
*)
let run_tests ?filter_by_substring ?lazy_ tests =
  let selected_tests = before_run ?filter_by_substring ?lazy_ tests in
  run_tests_with_alcotest selected_tests;
  after_run tests selected_tests

let run_tests_lwt ?filter_by_substring ?lazy_ tests =
  let selected_tests = before_run ?filter_by_substring ?lazy_ tests in
  Lwt.bind (run_tests_with_alcotest_lwt selected_tests) (fun () ->
      after_run tests selected_tests |> Lwt.return)

(*
   Entry point for the 'approve' subcommand
*)
let approve_output ?filter_by_substring tests =
  Store.init_workspace ();
  check_id_uniqueness tests;
  tests
  |> filter ?filter_by_substring
  |> Helpers.list_map Store.approve_new_output
  |> print_errors

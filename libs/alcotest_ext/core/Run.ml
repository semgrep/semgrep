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

type success = OK | OK_but_new | Not_OK

type 'unit_promise alcotest_test_case =
  string * [ `Quick | `Slow ] * (unit -> 'unit_promise)

type 'unit_promise alcotest_test =
  string * 'unit_promise alcotest_test_case list

(* Left margin for text relating to a test *)
let bullet = Style.color Faint "• "

(*
   Check that no two tests have the same full name or the same ID.
*)
let check_id_uniqueness (tests : _ T.test list) =
  (* nosemgrep: ocamllint-hashtable-dos *)
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
      if sum.has_expected_output then OK else OK_but_new
  | FAIL -> Not_OK
  | XPASS -> Not_OK
  | MISS -> OK_but_new

let color_of_status_summary (sum : T.status_summary) : Style.color =
  match success_of_status_summary sum with
  | OK -> Green
  | OK_but_new -> Yellow
  | Not_OK -> Red

let brackets s = sprintf "[%s]" s

(* Fixed-width output: "[PASS] ", "[XFAIL]" *)
let format_status_summary (sum : T.status_summary) =
  let style = color_of_status_summary sum in
  let displayed_string = sum |> string_of_status_summary |> brackets in
  Style.left_col displayed_string |> Style.color style

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
      let tags =
        List.sort Tag.compare tags
        |> Helpers.list_map (fun tag -> Style.color Bold (Tag.to_string tag))
      in
      sprintf " (%s)" (String.concat " " tags)

let format_title (test : _ T.test) : string =
  sprintf "%s%s %s" test.id (format_tags test)
    (test.category @ [ test.name ]
    |> Helpers.list_map (Style.color Cyan)
    |> String.concat " > ")

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
let protect_globals (test : _ T.test) (func : unit -> 'promise) :
    unit -> 'promise =
  let protect_global ?error_if_changed get set func () =
    let original_value = get () in
    Mona.protect test.m
      ~finally:(fun () ->
        let current_value = get () in
        set original_value;
        (match error_if_changed with
        | Some err_msg_func when current_value <> original_value ->
            Alcotest.fail (err_msg_func original_value current_value)
        | _ -> ());
        test.m.return ())
      func
  in
  func
  |> protect_global Sys.getcwd Sys.chdir ?error_if_changed:(chdir_error test)
  |> protect_global Printexc.backtrace_status Printexc.record_backtrace
(* TODO: more universal settings to protect? *)

(* TODO: simplify and run this directly without Alcotest.run *)
let to_alcotest_gen
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
         (suite_name, (test.name, `Quick, func)))
  |> group_by_key

let print_exn (test : _ T.test) exn trace =
  match test.expected_outcome with
  | Should_succeed ->
      prerr_string
        (Style.color Red
           (sprintf "FAIL: The test raised an exception: %s\n%s"
              (Printexc.to_string exn)
              (Printexc.raw_backtrace_to_string trace)))
  | Should_fail _reason ->
      prerr_string
        (Style.color Green
           (sprintf "XFAIL: As expected, the test raised an exception: %s\n%s"
              (Printexc.to_string exn)
              (Printexc.raw_backtrace_to_string trace)))

let with_print_exn (test : _ T.test) func () =
  test.m.catch func (fun exn ->
      let trace = Printexc.get_raw_backtrace () in
      print_exn test exn trace;
      Printexc.raise_with_backtrace exn trace)

let with_flip_xfail_outcome (test : _ T.test) func =
  match test.expected_outcome with
  | Should_succeed -> func
  | Should_fail _reason ->
      fun () ->
        test.m.catch
          (fun () ->
            test.m.bind (func ()) (fun () ->
                Alcotest.fail "XPASS: This test failed to raise an exception"))
          (fun exn trace ->
            eprintf "XFAIL: As expected, an exception was raised: %s\n%s\n"
              (Printexc.to_string exn)
              (Printexc.raw_backtrace_to_string trace);
            test.m.return ())

let conditional_wrap condition wrapper func =
  if condition then wrapper func else func

let wrap_test_function ~with_storage ~flip_xfail_outcome test func =
  func |> with_print_exn test
  |> conditional_wrap flip_xfail_outcome (with_flip_xfail_outcome test)
  |> protect_globals test
  |> conditional_wrap with_storage (Store.with_result_capture test)

let to_alcotest_internal ~with_storage ~flip_xfail_outcome tests =
  to_alcotest_gen
    ~wrap_test_function:(wrap_test_function ~with_storage ~flip_xfail_outcome)
    tests

(* Exported versions that exposes a plain Alcotest test suite that doesn't
   write test statuses and prints "OK" for XFAIL statuses. *)
let to_alcotest tests =
  to_alcotest_internal ~with_storage:false ~flip_xfail_outcome:true tests

let contains_regexp pat =
  let rex = Re.Pcre.regexp pat in
  fun str -> Re.execp rex str

let filter ~filter_by_substring tests =
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
        if n_errors >= 2 then Style.color Red "Errors:\n"
        else Style.color Red "Error: "
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
     | OK_but_new
     | Not_OK ->
         true)

let show_diff (output_kind : string) path_to_expected_output path_to_output =
  match
    (* Warning: the implementation of 'diff' (which is it?) available on
       BusyBox doesn't support '--color' option which is very sad.
       TODO: find a way to show color diffs
       -> use duff? https://github.com/mirage/duff *)
    (* nosemgrep: forbid-exec *)
    Sys.command
      (sprintf "diff -u '%s' '%s'" path_to_expected_output path_to_output)
  with
  | 0 -> ()
  | _nonzero ->
      printf "%sCaptured %s differs from expectation.\n" bullet output_kind

let show_output_details (sum : T.status_summary)
    (output_file_pairs : Store.output_file_pair list) =
  let success = success_of_status_summary sum in
  output_file_pairs
  |> List.iter
       (fun
         ({ short_name; path_to_expected_output; path_to_output } :
           Store.output_file_pair)
       ->
         flush stdout;
         flush stderr;
         match path_to_expected_output with
         | None ->
             printf "%sPath to unchecked output: %s\n" bullet path_to_output
         | Some path_to_expected_output ->
             (match success with
             | OK
             | OK_but_new ->
                 ()
             | Not_OK ->
                 (* TODO: only show diff if this particular file differs *)
                 show_diff short_name path_to_expected_output path_to_output);
             if success <> OK_but_new then
               printf "%sPath to expected %s: %s\n" bullet short_name
                 path_to_expected_output;
             printf "%sPath to latest %s: %s\n" bullet short_name path_to_output)

let print_error text = printf "%s%s\n" bullet (Style.color Red text)

let with_highlight_test ~highlight_test ~title func =
  if highlight_test then printf "%s" (Style.frame title)
  else printf "%s\n" title;
  func ();
  if highlight_test then print_string (Style.horizontal_line ())

let print_status ~highlight_test ~always_show_unchecked_output
    ((test : _ T.test), (status : T.status), sum) =
  let title = sprintf "%s%s" (format_status_summary sum) (format_title test) in
  with_highlight_test ~highlight_test ~title (fun () ->
      if (* Details about expectations *)
         test.skipped then printf "%sAlways skipped\n" bullet
      else (
        (match status.expectation.expected_outcome with
        | Should_succeed -> ()
        | Should_fail reason -> printf "%sExpected to fail: %s\n" bullet reason);
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
            printf "%sChecked output: %s\n" bullet text);
        (* Details about results *)
        (match status.expectation.expected_output with
        | Error [ path ] ->
            print_error
              (sprintf "Missing file containing the expected output: %s" path)
        | Error paths ->
            print_error
              (sprintf "Missing files containing the expected output: %s"
                 (String.concat ", " paths))
        | Ok _expected_output -> (
            match status.result with
            | Error [ path ] ->
                print_error
                  (sprintf "Missing file containing the test output: %s" path)
            | Error paths ->
                print_error
                  (sprintf "Missing files containing the test output: %s"
                     (String.concat ", " paths))
            | Ok _result ->
                let output_file_pairs = Store.get_output_file_pairs test in
                show_output_details sum output_file_pairs));
        match success_of_status_summary sum with
        | OK when not always_show_unchecked_output -> ()
        | OK_but_new when not always_show_unchecked_output ->
            (* TODO: show the checked output to be approved? *)
            ()
        | OK
        | OK_but_new
        | Not_OK -> (
            match Store.get_unchecked_output test with
            | Some ""
            | None ->
                ()
            | Some data ->
                printf "%sTest log (unchecked output):\n%s" bullet
                  (Style.quote_multiline_text data);
                if not (String.ends_with ~suffix:"\n" data) then print_char '\n'
            )));
  flush stdout

let print_statuses ~highlight_test ~always_show_unchecked_output
    tests_with_status =
  tests_with_status
  |> List.iter (print_status ~highlight_test ~always_show_unchecked_output)

let is_overall_success statuses =
  statuses
  |> List.for_all (fun (_test, _status, sum) ->
         match sum |> success_of_status_summary with
         | OK
         | OK_but_new ->
             true
         | Not_OK -> false)

(*
   Status output:
   0. Introduction: explain how to read the output.
   1. Long status: for each selected test, show all the details one might
      want to know about the test.
   2. Short status: show only the status of the tests that need our attention.
   3. Summary: give the counts for each test with a particular state.

   Options:
   --full: all of the above, the default
   --short: show only short status and summary
*)
let print_status_introduction () =
  printf
    {|Legend:
%s[PASS]: a successful test that was expected to succeed (good);
%s[FAIL]: a failing test that was expected to succeed (needs fixing);
%s[XFAIL]: a failing test that was expected to fail (tolerated failure);
%s[XPASS]: a successful test that was expected to fail (progress?).
%s[MISS]: a test that never ran;
%s[xxxx*]: a new test for which there's no expected output yet.
  In this case, you should review the test output and run the 'approve'
  subcommand once you're satisfied with the output.
|}
    bullet bullet bullet bullet bullet bullet

let print_short_status ~always_show_unchecked_output tests_with_status =
  let tests_with_status = List.filter is_important_status tests_with_status in
  match tests_with_status with
  | [] -> ()
  | _ ->
      print_statuses ~highlight_test:true ~always_show_unchecked_output
        tests_with_status

let print_long_status ~always_show_unchecked_output tests_with_status =
  match tests_with_status with
  | [] -> ()
  | _ ->
      print_statuses ~highlight_test:false ~always_show_unchecked_output
        tests_with_status

let plural num = if num >= 2 then "s" else ""

let print_status_summary tests tests_with_status =
  let stats = stats_of_tests tests tests_with_status in
  let overall_success = is_overall_success tests_with_status in
  printf
    "%i/%i selected test%s:\n\
    \  %i successful (%i pass, %i xfail)\n\
    \  %i unsuccessful (%i fail, %i xpass)\n"
    stats.selected_tests stats.total_tests (plural stats.total_tests)
    (!(stats.pass) + !(stats.xfail))
    !(stats.pass) !(stats.xfail)
    (!(stats.fail) + !(stats.xpass))
    !(stats.fail) !(stats.xpass);
  if !(stats.miss) > 0 then
    printf "%i new test%s\n" !(stats.miss) (plural !(stats.miss));
  if !(stats.needs_approval) > 0 then
    printf "%i test%s whose output needs first-time approval\n"
      !(stats.needs_approval)
      (plural !(stats.needs_approval));
  printf "overall status: %s\n"
    (if overall_success then Style.color Green "success"
     else Style.color Red "failure");
  if overall_success then 0 else 1

let print_full_status ~always_show_unchecked_output tests tests_with_status =
  print_status_introduction ();
  print_newline ();
  print_long_status ~always_show_unchecked_output tests_with_status;
  print_newline ();
  print_short_status ~always_show_unchecked_output tests_with_status;
  print_status_summary tests tests_with_status

let print_short_status ~always_show_unchecked_output tests tests_with_status =
  print_short_status ~always_show_unchecked_output tests_with_status;
  print_status_summary tests tests_with_status

let get_test_with_status test =
  let status = Store.get_status test in
  (test, status, Store.status_summary_of_status status)

let get_tests_with_status tests = tests |> Helpers.list_map get_test_with_status

(*
   Entry point for the status subcommand
*)
let list_status ~always_show_unchecked_output ~filter_by_substring ~output_style
    tests =
  check_id_uniqueness tests;
  let selected_tests = filter ~filter_by_substring tests in
  let tests_with_status = get_tests_with_status selected_tests in
  let exit_code =
    match output_style with
    | Full ->
        print_full_status ~always_show_unchecked_output tests tests_with_status
    | Short ->
        print_short_status ~always_show_unchecked_output tests tests_with_status
  in
  (exit_code, tests_with_status)

let run_tests_sequentially ~(mona : _ Mona.t) ~always_show_unchecked_output
    (tests : 'unit_promise T.test list) : 'unit_promise =
  List.fold_left
    (fun previous (test : _ T.test) ->
      let test_func : unit -> 'unit_promise =
        wrap_test_function ~with_storage:true ~flip_xfail_outcome:false test
          test.func
      in
      mona.bind previous (fun () ->
          if test.skipped then (
            printf "%s%s\n%!"
              (Style.left_col (Style.color Yellow "[SKIP]"))
              (format_title test);
            mona.return ())
          else (
            printf "%s%s...%!"
              (Style.left_col (Style.color Yellow "[RUN]"))
              (format_title test);
            mona.bind
              (mona.catch test_func (fun _exn _trace -> mona.return ()))
              (fun () ->
                (* Erase RUN line *)
                printf "\027[2K\r";
                get_test_with_status test
                |> print_status ~highlight_test:false
                     ~always_show_unchecked_output;
                mona.return ()))))
    (mona.return ()) tests

(* Run this before a run or Lwt run. Returns the filtered tests. *)
let before_run ~filter_by_substring ~lazy_ tests =
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
  filter ~filter_by_substring tests

(* Run this after a run or Lwt run. *)
let after_run ~always_show_unchecked_output tests selected_tests =
  let tests_with_status = get_tests_with_status selected_tests in
  let exit_code =
    print_short_status ~always_show_unchecked_output tests tests_with_status
  in
  (exit_code, tests_with_status)

(*
   Entry point for the 'run' subcommand
*)
let run_tests ~(mona : _ Mona.t) ~always_show_unchecked_output
    ~filter_by_substring ~lazy_ tests cont =
  let selected_tests = before_run ~filter_by_substring ~lazy_ tests in
  mona.bind
    (run_tests_sequentially ~mona ~always_show_unchecked_output selected_tests)
    (fun () ->
      let exit_code, tests_with_status =
        after_run ~always_show_unchecked_output tests selected_tests
      in
      cont exit_code tests_with_status |> ignore;
      (* The continuation 'cont' should exit but otherwise we exit once
         it's done. *)
      (* nosemgrep: forbid-exit *)
      exit exit_code)
  |> ignore;
  (* shouldn't be reached if 'bind' does what it's supposed to *)
  (* nosemgrep: forbid-exit *)
  exit 0

(*
   Entry point for the 'approve' subcommand
*)
let approve_output ?filter_by_substring tests =
  Store.init_workspace ();
  check_id_uniqueness tests;
  tests
  |> filter ~filter_by_substring
  |> Helpers.list_map Store.approve_new_output
  |> print_errors

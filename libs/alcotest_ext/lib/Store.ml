(*
   Manage the storage of test statuses and test results.

   We store the following:

   - result for the last run of each test: in a hidden folder
   - captured output for the last run of each test: in a hidden folder
   - expected output for each test: in a persistent folder

   We distinguish two levels of "statuses":

   - test result: the test result before comparing it to expectations:
     * Did it return or raise an exception?
     * What output did we capture?
   - test status: the test result confronted to our expectations:
     * Did the test run at all?
     * Does the test result match our expectations?
*)

open Printf
module T = Types

(**************************************************************************)
(* Helpers *)
(**************************************************************************)
(*
   Some of these helpers are provided by nice third-party libraries but we're
   not using them to minimize dependencies, this being a test framework
   that all library authors should be able to use.
*)

let list_map f xs = List.rev_map f xs |> List.rev
let ( // ) a b = if Filename.is_relative b then Filename.concat a b else b

exception Local_message of string

(* Return the first error message and drop the other messages in case
   of an error. *)
let list_result_of_result_list (xs : ('a, _) Result.t list) :
    ('a list, _) Result.t =
  try
    Ok
      (list_map
         (function
           | Ok x -> x
           | Error msg -> raise (Local_message msg))
         xs)
  with
  | Local_message msg -> Error msg

let with_file_in path f =
  if Sys.file_exists path then
    (* nosemgrep: no-open-in *)
    let ic = open_in_bin path in
    (* nosemgrep: no-fun-protect *)
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> Ok (f ic))
  else Error (sprintf "Missing file %S" path)

let with_file_out path f =
  let oc = open_out_bin path in
  (* nosemgrep: no-fun-protect *)
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> f oc)

let read_file path : (string, string) Result.t =
  with_file_in path (fun ic -> really_input_string ic (in_channel_length ic))

let write_file path data = with_file_out path (fun oc -> output_string oc data)
let remove_file path = if Sys.file_exists path then Sys.remove path

(**************************************************************************)
(* Global settings *)
(**************************************************************************)

(*
   The status workspace is a temporary folder outside of version control.
*)
let default_status_workspace_root = "_build" // "alcotest_ext" // "status"

(*
   The expectation workspace is under version control.
*)
let default_expectation_workspace_root = "tests" // "snapshots"

let not_initialized () =
  failwith "Missing initialization call: Alcotest_ext.init ()"

let already_initialized () =
  failwith "Multiple initialization calls to Alcotest_ext.init. Keep only one."

let make_late_init () =
  let var = ref None in
  let get () =
    match !var with
    | None -> not_initialized ()
    | Some x -> x
  in
  let set value =
    match !var with
    | Some _ -> already_initialized ()
    | None -> var := Some value
  in
  (get, set)

let get_status_workspace, set_status_workspace = make_late_init ()
let get_expectation_workspace, set_expectation_workspace = make_late_init ()

let init_settings
    ?(expectation_workspace_root = default_expectation_workspace_root)
    ?(status_workspace_root = default_status_workspace_root) ~project_name () =
  if status_workspace_root = expectation_workspace_root then
    invalid_arg
      "Store.init: status_workspace and expectation_workspace must be \
       different folders.";
  set_status_workspace (status_workspace_root // project_name);
  set_expectation_workspace (expectation_workspace_root // project_name)

let init_workspace () =
  Helpers.make_dir_if_not_exists ~recursive:true (get_status_workspace ());
  Helpers.make_dir_if_not_exists ~recursive:true (get_expectation_workspace ())

let get_test_status_workspace (test : _ T.test) =
  get_status_workspace () // test.id

let get_test_expectation_workspace (test : _ T.test) =
  get_expectation_workspace () // test.id

let init_test_workspace test =
  Helpers.make_dir_if_not_exists (get_test_status_workspace test);
  Helpers.make_dir_if_not_exists (get_test_expectation_workspace test)

(**************************************************************************)
(* Read/write data *)
(**************************************************************************)

let corrupted_file path =
  failwith
    (sprintf
       "Uh oh, the test framework ran into a corrupted file: %S\n\
        Remove it and retry." path)

let get_outcome_path (test : _ T.test) =
  get_test_status_workspace test // "outcome"

let string_of_outcome (outcome : T.outcome) =
  match outcome with
  | Succeeded -> "Succeeded"
  | Failed -> "Failed"

let outcome_of_string path data : T.outcome =
  match data with
  | "Succeeded" -> Succeeded
  | "Failed" -> Failed
  | _ -> corrupted_file path

let set_outcome (test : _ T.test) outcome =
  let path = get_outcome_path test in
  outcome |> string_of_outcome |> write_file path

let get_outcome (test : _ T.test) : (T.outcome, string) Result.t =
  let path = get_outcome_path test in
  read_file path |> Result.map (outcome_of_string path)

let clear_outcome (test : _ T.test) = test |> get_outcome_path |> remove_file
let stdout_filename = "stdout"
let stderr_filename = "stderr"
let stdxxx_filename = "stdxxx"

let names_of_output (output : T.output_kind) : string list =
  match output with
  | Ignore_output -> []
  | Stdout -> [ stdout_filename ]
  | Stderr -> [ stderr_filename ]
  | Merged_stdout_stderr -> [ stdxxx_filename ]
  | Separate_stdout_stderr -> [ stdout_filename; stderr_filename ]

let get_output_path (test : _ T.test) filename =
  get_status_workspace () // test.id // filename

let get_output_paths (test : _ T.test) =
  test.output_kind |> names_of_output |> list_map (get_output_path test)

let get_output (test : _ T.test) =
  test |> get_output_paths |> list_map read_file

let set_output (test : _ T.test) data =
  let paths = test |> get_output_paths in
  if List.length data <> List.length paths then
    invalid_arg "Store.set_output_data"
  else List.iter2 (fun path data -> write_file path data) paths data

let clear_output (test : _ T.test) =
  test |> get_output_paths |> List.iter remove_file

let get_expected_output_path (test : _ T.test) filename =
  get_expectation_workspace () // test.id // filename

let get_expected_output_paths (test : _ T.test) =
  test.output_kind |> names_of_output
  |> list_map (get_expected_output_path test)

let get_expected_output (test : _ T.test) =
  test |> get_expected_output_paths |> list_map read_file

let set_expected_output (test : _ T.test) (data : string list) =
  let paths = test |> get_expected_output_paths in
  if List.length data <> List.length paths then
    invalid_arg "Store.set_expected_output_data"
  else List.iter2 (fun path data -> write_file path data) paths data

let clear_expected_output (test : _ T.test) =
  test |> get_expected_output_paths |> List.iter remove_file

type output_file_pair = {
  short_name : string;
  path_to_expected_output : string;
  path_to_output : string;
}

let get_output_file_pairs (test : _ T.test) =
  test.output_kind |> names_of_output
  |> Helpers.list_map (fun name ->
         {
           short_name = name;
           path_to_expected_output = get_expected_output_path test name;
           path_to_output = get_output_path test name;
         })

(**************************************************************************)
(* Output redirection *)
(**************************************************************************)

(* Redirect e.g. stderr to stdout during the execution of the function func.
   Usage:

     with_redirect Unix.stderr Unix.stdout do_something

   redirects stderr to stdout.
*)
let with_redirect_fd ~from ~to_ func () =
  (* keep the original file alive *)
  let original = Unix.dup from in
  (* nosemgrep: no-fun-protect *)
  Fun.protect
    ~finally:(fun () -> Unix.close original)
    (fun () ->
      (* redirect to file *)
      Unix.dup2 to_ from;
      (* nosemgrep: no-fun-protect *)
      Fun.protect
        ~finally:(fun () -> (* cancel the redirect *)
                            Unix.dup2 original from)
        func)

(* Redirect stdout or stderr to a file *)
let with_redirect_fd_to_file fd filename func () =
  let file = Unix.openfile filename [ O_CREAT; O_TRUNC; O_WRONLY ] 0o666 in
  (* nosemgrep: no-fun-protect *)
  Fun.protect
    ~finally:(fun () -> Unix.close file)
    (with_redirect_fd ~from:fd ~to_:file func)

(* stdout/stderr redirect using buffered channels. We're careful about
   flushing the channel of interest (from) before any redirection. *)
let with_redirect ~from ~to_ func () =
  flush from;
  let from_fd = Unix.descr_of_out_channel from in
  let to_fd = Unix.descr_of_out_channel to_ in
  with_redirect_fd ~from:from_fd ~to_:to_fd
    (fun () ->
      (* nosemgrep: no-fun-protect *)
      Fun.protect ~finally:(fun () -> flush from) func)
    ()

(* Redirect a buffered channel to a file. *)
let with_redirect_to_file from filename func () =
  flush from;
  let from_fd = Unix.descr_of_out_channel from in
  with_redirect_fd_to_file from_fd filename
    (fun () ->
      (* nosemgrep: no-fun-protect *)
      Fun.protect ~finally:(fun () -> flush from) func)
    ()

let with_output_capture (test : unit T.test) func =
  match test.output_kind with
  | Ignore_output -> func
  | Stdout ->
      with_redirect_to_file stdout (get_output_path test stdout_filename) func
  | Stderr ->
      with_redirect_to_file stderr (get_output_path test stderr_filename) func
  | Merged_stdout_stderr ->
      (* redirect stderr to stdout, then redirect stdout to stdxxx file *)
      with_redirect ~from:stderr ~to_:stdout
        (with_redirect_to_file stdout
           (get_output_path test stdxxx_filename)
           func)
  | Separate_stdout_stderr ->
      with_redirect_to_file stdout
        (get_output_path test stdout_filename)
        (with_redirect_to_file stderr
           (get_output_path test stderr_filename)
           func)

let with_outcome_capture (test : unit T.test) func () =
  try
    let res = func () in
    set_outcome test Succeeded;
    res
  with
  | e ->
      let trace = Printexc.get_raw_backtrace () in
      set_outcome test Failed;
      Printexc.raise_with_backtrace e trace

let with_outcome_capture_lwt (test : unit Lwt.t T.test) func () =
  Lwt.catch
    (fun () ->
      Lwt.bind (func ()) (fun res ->
          set_outcome test Succeeded;
          Lwt.return res))
    (fun e ->
      let trace = Printexc.get_raw_backtrace () in
      set_outcome test Failed;
      Printexc.raise_with_backtrace e trace)

let with_output_capture_lwt (test : unit Lwt.t T.test) func =
  (* TODO: IO redirections for Lwt *)
  match test.output_kind with
  | Ignore_output -> func
  | _ ->
      failwith
        (sprintf "TODO: lwt test output capture (requested for test %s)" test.id)

let with_result_capture (test : unit T.test) func () =
  init_test_workspace test;
  let func = func |> with_output_capture test |> with_outcome_capture test in
  func ()

let with_result_capture_lwt (test : unit Lwt.t T.test) func () =
  init_test_workspace test;
  let func =
    func |> with_output_capture_lwt test |> with_outcome_capture_lwt test
  in
  func ()

(**************************************************************************)
(* High-level interface *)
(**************************************************************************)

let captured_output_of_data (kind : T.output_kind) (data : string list) :
    T.captured_output =
  match (kind, data) with
  | Ignore_output, [] -> Ignored
  | Stdout, [ out ] -> Captured_stdout out
  | Stderr, [ err ] -> Captured_stderr err
  | Merged_stdout_stderr, [ data ] -> Captured_merged data
  | Separate_stdout_stderr, [ out; err ] -> Captured_stdout_stderr (out, err)
  | ( ( Ignore_output | Stdout | Stderr | Merged_stdout_stderr
      | Separate_stdout_stderr ),
      _ ) ->
      assert false

let data_of_captured_output (output : T.captured_output) : string list =
  match output with
  | Ignored -> []
  | Captured_stdout out -> [ out ]
  | Captured_stderr err -> [ err ]
  | Captured_merged data -> [ data ]
  | Captured_stdout_stderr (out, err) -> [ out; err ]

let save_result (test : _ T.test) (res : T.result) =
  let data = data_of_captured_output res.captured_output in
  set_outcome test res.outcome;
  set_output test data

let get_expectation (test : _ T.test) : T.expectation =
  let expected_output =
    test |> get_expected_output |> list_result_of_result_list
    |> Result.map (captured_output_of_data test.output_kind)
  in
  { expected_outcome = test.expected_outcome; expected_output }

let get_result (test : _ T.test) : (T.result, string) Result.t =
  match get_outcome test with
  | Error _ as res -> res
  | Ok outcome -> (
      let opt_captured_output =
        test |> get_output |> list_result_of_result_list
        |> Result.map (captured_output_of_data test.output_kind)
      in
      match opt_captured_output with
      | Error _ as res -> res
      | Ok captured_output -> Ok { outcome; captured_output })

let get_status (test : _ T.test) : T.status =
  let expectation = get_expectation test in
  let result = get_result test in
  { expectation; result }

let delete_result (test : _ T.test) =
  clear_outcome test;
  clear_output test

let status_summary_of_status (status : T.status) : T.status_summary =
  match status.result with
  | Error _ ->
      {
        status_class = MISS;
        has_expected_output = false (* incorrect but does it matter? *);
      }
  | Ok result ->
      let expect = status.expectation in
      let has_expected_output, output_matches =
        match (expect.expected_output, result.captured_output) with
        | Ok output1, output2 when output1 = output2 -> (true, true)
        | Ok _, _ -> (true, false)
        | Error _, _ -> (false, true)
      in
      let status_class : T.status_class =
        match (expect.expected_outcome, (result.outcome, output_matches)) with
        | Should_succeed, (Succeeded, true) -> PASS
        | Should_succeed, (Failed, _ | _, false) -> FAIL
        | Should_fail _, (Succeeded, true) -> XPASS
        | Should_fail _, (Failed, _ | _, false) -> XFAIL
      in
      { status_class; has_expected_output }

let check_outcome (test : _ T.test) =
  match (test.expected_outcome, get_outcome test) with
  | Should_succeed, Ok Succeeded
  | Should_fail _, Ok Failed ->
      Ok ()
  | Should_succeed, Ok Failed ->
      Error (sprintf "Cannot approve test %S because it failed." test.id)
  | Should_fail reason, Ok Succeeded ->
      Error
        (sprintf
           "Cannot approve test %S because it succeeded but was expected to \
            fail.\n\
            The original reason given was:\n\
           \  %S" test.id reason)
  | _, Error msg -> Error msg

exception Local_error of string

let approve_new_output (test : _ T.test) =
  if test.skipped then Ok ()
  else
    match check_outcome test with
    | Error _ as res -> res
    | Ok () -> (
        clear_expected_output test;
        try
          let data =
            test |> get_output
            |> list_map (function
                 | Ok data -> data
                 | Error msg -> raise (Local_error msg))
          in
          set_expected_output test data;
          Ok ()
        with
        | Local_error msg ->
            Error (sprintf "Cannot approve output for test %s: %s" test.id msg))

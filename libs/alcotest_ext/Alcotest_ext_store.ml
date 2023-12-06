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
module T = Alcotest_ext_types

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

let rec mkdir_if_not_exists dir =
  match (Unix.stat dir).st_kind with
  | S_DIR -> ()
  | S_REG
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK ->
      failwith
        (sprintf
           "File %S already exists but is not a folder as required by the \
            testing setup."
           dir)
  | exception Unix.Unix_error (ENOENT, _, _) ->
      let parent = Filename.dirname dir in
      if parent = dir then
        failwith
          (sprintf
             "Folder %S doesn't exist and has no parent that we could create."
             dir)
      else (
        mkdir_if_not_exists parent;
        Sys.mkdir dir 0o777)

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
   The status workspace is a temporary folder outside of version control,
   and by normally out of programmer's sight.
*)
let default_status_workspace = "_build" // "_alcotest_ext"

(*
   The expectation workspace is under version control.
*)
let default_expectation_workspace = "expect"

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

let init ?(status_workspace = default_status_workspace)
    ?(expectation_workspace = default_expectation_workspace) () =
  if status_workspace = expectation_workspace then
    invalid_arg
      "Alcotest_ext_store.init: status_workspace and expectation_workspace \
       must be different folders.";
  set_status_workspace status_workspace;
  set_expectation_workspace expectation_workspace;
  mkdir_if_not_exists status_workspace;
  mkdir_if_not_exists expectation_workspace

(**************************************************************************)
(* Read/write data *)
(**************************************************************************)

let corrupted_file path =
  failwith
    (sprintf
       "Uh oh, the test framework ran into a corrupted file: %S\n\
        Remove it and retry." path)

let get_outcome_path (test : _ T.test) =
  get_status_workspace () // test.id // "outcome"

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
  outcome |> string_of_outcome |> write_file (get_outcome_path test)

let get_outcome (test : _ T.test) : (T.outcome, string) Result.t =
  let path = get_outcome_path test in
  read_file path |> Result.map (outcome_of_string path)

let clear_outcome (test : _ T.test) = test |> get_outcome_path |> remove_file

let names_of_output (output : T.output_kind) : string list =
  match output with
  | Ignore_output -> []
  | Stdout -> [ "stdout" ]
  | Stderr -> [ "stderr" ]
  | Merged_stdout_stderr -> [ "stdxxx" ]
  | Separate_stdout_stderr -> [ "stdout"; "stderr" ]

let get_output_paths (test : _ T.test) =
  test.output_kind |> names_of_output
  |> list_map (fun name -> get_status_workspace () // test.id // name)

let get_output (test : _ T.test) =
  test |> get_output_paths |> list_map read_file

let set_output (test : _ T.test) data =
  let paths = test |> get_output_paths in
  if List.length data <> List.length paths then
    invalid_arg "Alcotest_ext_store.set_output_data"
  else List.iter2 (fun path data -> write_file path data) paths data

let clear_output (test : _ T.test) =
  test |> get_output_paths |> List.iter remove_file

let get_expected_output_paths (test : _ T.test) =
  test.output_kind |> names_of_output
  |> list_map (fun name -> get_expectation_workspace () // test.id // name)

let get_expected_output (test : _ T.test) =
  test |> get_expected_output_paths |> list_map read_file

let set_expected_output (test : _ T.test) (data : string list) =
  let paths = test |> get_expected_output_paths in
  if List.length data <> List.length paths then
    invalid_arg "Alcotest_ext_store.set_expected_output_data"
  else List.iter2 (fun path data -> write_file path data) paths data

let clear_expected_output (test : _ T.test) =
  test |> get_expected_output_paths |> List.iter remove_file

(**************************************************************************)
(* High-level interface *)
(**************************************************************************)

let captured_output_of_data (kind : T.output_kind) (data : string list) :
    T.captured_output =
  match (kind, data) with
  | Ignore_output, [] -> Ignored
  | Stdout, [ out ] -> Captured_stdout out
  | Stderr, [ err ] -> Captured_stderr err
  | Merged_stdout_stderr, [ out; err ] -> Captured_stdout_stderr (out, err)
  | Separate_stdout_stderr, [ data ] -> Captured_merged data
  | ( ( Ignore_output | Stdout | Stderr | Merged_stdout_stderr
      | Separate_stdout_stderr ),
      _ ) ->
      assert false

let data_of_captured_output (output : T.captured_output) : string list =
  match output with
  | Ignored -> []
  | Captured_stdout out -> [ out ]
  | Captured_stderr err -> [ err ]
  | Captured_stdout_stderr (out, err) -> [ out; err ]
  | Captured_merged data -> [ data ]

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

let status_class_of_status ?(accept_missing_expected_output = false)
    (status : T.status) : T.status_class =
  match status.result with
  | Error _ -> MISSING
  | Ok result -> (
      let expect = status.expectation in
      let output_matches =
        match (expect.expected_output, result.captured_output) with
        | Ok output1, output2 when output1 = output2 -> true
        | Error _, _ when accept_missing_expected_output -> true
        | _ -> false
      in
      match (expect.expected_outcome, (result.outcome, output_matches)) with
      | Should_succeed, (Succeeded, true) -> PASS
      | Should_succeed, (Failed, _ | _, false) -> FAIL
      | Should_fail _, (Succeeded, true) -> XPASS
      | Should_fail _, (Failed, _ | _, false) -> XFAIL)

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

let approve_new_output (test : _ T.test) =
  match check_outcome test with
  | Error _ as res -> res
  | Ok () ->
      clear_expected_output test;
      let data =
        test |> get_expected_output
        |> list_map (function
             | Ok data -> data
             | Error msg -> failwith msg)
      in
      set_expected_output test data;
      Ok ()

(*
   Filter and run tests
*)

open Printf
module T = Types

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
  let class_string =
    match sum.status_class with
    | PASS -> "PASS"
    | FAIL -> "FAIL"
    | XFAIL -> "XFAIL"
    | XPASS -> "XPASS"
    | MISS -> "MISS"
  in
  if sum.has_expected_output then class_string else class_string ^ " (new)"

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

(* Fixed-width output: "[PASS] ", "[XFAIL]" *)
let format_status_summary (sum : T.status_summary) =
  let style = style_of_status_summary sum in
  let displayed_string = sum |> string_of_status_summary in
  let padding = max 0 (5 - String.length displayed_string) in
  sprintf "[%s]%s"
    (Color.format Color style displayed_string)
    (String.make padding ' ')

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

let to_alcotest_generic ~wrap_test_function tests : _ list =
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
           | Should_fail reason -> sprintf " [xfail: %s]" reason
         in
         let suite_name =
           sprintf "%s%s%s %s" test.id xfail_note (format_tags test) suite_name
         in
         let func =
           if test.skipped then Alcotest.skip else wrap_test_function test
         in
         (* This is the format expected by Alcotest: *)
         (suite_name, (test.name, test.speed_level, func)))
  |> group_by_key

let to_alcotest tests =
  to_alcotest_generic ~wrap_test_function:Store.with_result_capture tests

let to_alcotest_lwt tests =
  to_alcotest_generic ~wrap_test_function:Store.with_result_capture_lwt tests

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

let raise_errors (xs : (_, string) Result.t list) : unit =
  let error_messages =
    xs
    |> List.filter_map (function
         | Ok _ -> None
         | Error msg -> Some msg)
  in
  match error_messages with
  | [] -> ()
  | _ ->
      let msg = String.concat "\n" error_messages in
      failwith msg

let is_important_status (sum : T.status_summary) =
  (not sum.has_expected_output)
  ||
  match success_of_status_summary sum with
  | OK -> false
  | OK_where_no_missing_data
  | Not_OK ->
      true

let print_status ?(only_important = false) ((test : _ T.test), status) =
  let sum = Store.status_summary_of_status status in
  let show =
    match only_important with
    | true -> is_important_status sum
    | false -> true
  in
  if show then
    printf "%s %s%s %s\n"
      (format_status_summary sum)
      test.id (format_tags test) test.internal_full_name

let is_overall_success statuses =
  statuses
  |> List.for_all (fun (_test, status) ->
         match
           status |> Store.status_summary_of_status |> success_of_status_summary
         with
         | OK
         | OK_where_no_missing_data ->
             true
         | Not_OK -> false)

let print_statuses ?only_important statuses =
  print_endline (Color.format Color Bold "Summary");
  statuses |> List.iter (print_status ?only_important);
  let overall_success = is_overall_success statuses in
  (* TODO: show counts: total number of tests, number of selected tests,
     number of tests in pass/fail/xfail/xpass, percentage of success *)
  if overall_success then print_endline "All expectations were met."
  else print_endline "Some expectations were not met.";
  if overall_success then 0 else 1

(* Important: for unknown reasons, the "test" subcommand of alcotest
   force an exit after Alcotest.run, regardless of the 'and_exit' argument
   that we pass. *)
let alcotest_argv = [| ""; "-e" |]

let run_tests ?filter_by_substring tests =
  check_id_uniqueness tests;
  let alcotest_tests = tests |> filter ?filter_by_substring |> to_alcotest in
  (try
     Alcotest.run ~and_exit:false ~argv:alcotest_argv "test" alcotest_tests
   with
  | Alcotest.Test_error -> ());
  (* Alcotest doesn't print a trailing newline here but only when the program
     exits, it seems (?) *)
  print_newline ()

let run_tests_lwt ?filter_by_substring tests =
  check_id_uniqueness tests;
  let alcotest_tests =
    tests |> filter ?filter_by_substring |> to_alcotest_lwt
  in
  Lwt.catch
    (fun () ->
      Alcotest_lwt.run ~and_exit:false ~argv:alcotest_argv "test" alcotest_tests)
    (function
      | Alcotest.Test_error -> Lwt.return ()
      | e -> raise e)

let list_status ?filter_by_substring ?only_important tests =
  check_id_uniqueness tests;
  tests
  |> filter ?filter_by_substring
  |> Helpers.list_map (fun test -> (test, Store.get_status test))
  |> print_statuses ?only_important

let approve_output ?filter_by_substring tests =
  check_id_uniqueness tests;
  tests
  |> filter ?filter_by_substring
  |> Helpers.list_map Store.approve_new_output
  |> raise_errors

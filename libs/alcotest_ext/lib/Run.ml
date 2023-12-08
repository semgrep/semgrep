(*
   Filter and run tests
*)

open Printf
module T = Types

let format_test_path (test : _ T.test) =
  String.concat " > " (test.category @ [ test.name ])

let string_of_status_class (x : T.status_class) =
  match x with
  | PASS -> "PASS"
  | FAIL -> "FAIL"
  | XFAIL -> "XFAIL"
  | XPASS -> "XPASS"
  | MISS -> "MISS"

let style_of_status_class (x : T.status_class) : Color.style =
  match x with
  | PASS -> Green
  | FAIL -> Red
  | XFAIL -> Green
  | XPASS -> Red
  | MISS -> Yellow

let success_of_status_class (x : T.status_class) =
  match x with
  | PASS -> true
  | FAIL -> false
  | XFAIL -> true
  | XPASS -> false
  | MISS -> false

(* Fixed-width output: "[PASS] ", "[XFAIL]" *)
let format_status_class (status : T.status) =
  let status_class = Store.status_class_of_status status in
  let style = style_of_status_class status_class in
  let displayed_string = status_class |> string_of_status_class in
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
         let suite_name =
           sprintf "%s%s %s" test.id (format_tags test) suite_name
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
        |> List.filter (fun test ->
               contains_substring (format_test_path test)
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

let print_status ((test : _ T.test), status) =
  printf "%s %s%s %s\n"
    (format_status_class status)
    test.id (format_tags test) (format_test_path test)

let is_overall_success statuses =
  statuses
  |> List.for_all (fun (_test, status) ->
         status |> Store.status_class_of_status |> success_of_status_class)

let print_statuses statuses =
  statuses |> List.iter print_status;
  let overall_success = is_overall_success statuses in
  if overall_success then print_endline "All expectations were met."
  else print_endline "Some expectations were not met.";
  if overall_success then 0 else 1

let alcotest_argv = [| ""; "test"; "-e" |]

let run_tests ?filter_by_substring tests =
  tests
  |> filter ?filter_by_substring
  |> to_alcotest
  |> Alcotest.run ~argv:alcotest_argv "test"

let run_tests_lwt ?filter_by_substring tests =
  tests
  |> filter ?filter_by_substring
  |> to_alcotest_lwt
  |> Alcotest_lwt.run ~argv:alcotest_argv "test"

let list_status ?filter_by_substring tests =
  tests
  |> filter ?filter_by_substring
  |> Helpers.list_map (fun test -> (test, Store.get_status test))
  |> print_statuses

let approve_output ?filter_by_substring tests =
  tests
  |> filter ?filter_by_substring
  |> Helpers.list_map Store.approve_new_output
  |> raise_errors

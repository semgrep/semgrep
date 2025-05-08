(*
   Unit tests for our List_ module.
*)

open Printf

let t = Testo.create

let string_of_int_list xs =
  sprintf "[%s]" (xs |> List_.map string_of_int |> String.concat "; ")

let string_of_int_option = function
  | None -> "None"
  | Some x -> sprintf "Some %i" x

let test_iter_with_view_into_neighbor_elements name list =
  Testo.create ~category:[ "List_.iter_with_view_into_neighbor_elements" ]
    ~checked_output:(Testo.stdout ()) name (fun () ->
      printf "input list: %s\n" (string_of_int_list list);
      list
      |> List_.iter_with_view_into_neighbor_elements (fun ~prev ~cur ~next ->
             printf "prev=%s, cur=%i, next=%s\n"
               (string_of_int_option prev)
               cur
               (string_of_int_option next)))

let test_flatten () =
  let list = [ [ 1; 2; 3 ]; []; [ 4 ]; [ 5; 6 ] ] in
  (* nosemgrep is ok here as we are comparing against STD List.flatten *)
  (* nosemgrep: no-list-concat *)
  Alcotest.(check (list int)) __LOC__ (List.flatten list) (List_.flatten list)

let test_append () =
  let a = [ 1; 2; 3; 4 ] in
  let b = [ 5; 6 ] in
  Alcotest.(check (list int)) __LOC__ [ 1; 2; 3; 4; 5; 6 ] (List_.append a b)

let test_fold_right () =
  let cons a b = a :: b in
  let list = [ 1; 2; 3; 4 ] in
  Alcotest.(check (list int)) __LOC__ list (List_.fold_right cons list [])

let test_combine () =
  assert (
    List_.combine [ 1; 2; 3 ] [ "a"; "b"; "c" ]
    = [ (1, "a"); (2, "b"); (3, "c") ])

let test_split () =
  assert (
    List_.split [ (1, "a"); (2, "b"); (3, "c") ]
    = ([ 1; 2; 3 ], [ "a"; "b"; "c" ]))

let test_span () =
  assert (List_.span (fun x -> x < 3) [ 1; 2; 3 ] = ([ 1; 2 ], [ 3 ]));
  assert (List_.span (fun x -> x < 3) [ 1; 2; 3; 1 ] = ([ 1; 2 ], [ 3; 1 ]));
  assert (List_.span (fun x -> x < 3) [] = ([], []));
  assert (List_.span (fun x -> x < 3) [ 1; 2 ] = ([ 1; 2 ], []));
  assert (List_.span (fun x -> x < 3) [ 5; 1 ] = ([], [ 5; 1 ]))

let test_uniq_by () =
  Alcotest.(check (list int))
    __LOC__ [ 1; 2; 3; 0 ]
    (List_.uniq_by ( = ) [ 1; 2; 2; 1; 3; 0 ])

let test_deduplicate () =
  Alcotest.(check (list int))
    __LOC__ [ 1; 2; 3; 0 ]
    (List_.deduplicate [ 1; 2; 2; 1; 3; 0 ])

let test_deduplicate_gen () =
  Alcotest.(check (list (pair int string)))
    __LOC__
    [ (1, "a"); (2, "b"); (3, "e"); (0, "f") ]
    (List_.deduplicate_gen fst
       [ (1, "a"); (2, "b"); (2, "c"); (1, "d"); (3, "e"); (0, "f") ])

let tests =
  Testo.categorize "List_"
    [
      test_iter_with_view_into_neighbor_elements "empty" [];
      test_iter_with_view_into_neighbor_elements "one element" [ 1 ];
      test_iter_with_view_into_neighbor_elements "two elements" [ 1; 2 ];
      test_iter_with_view_into_neighbor_elements "three elements" [ 1; 2; 3 ];
      test_iter_with_view_into_neighbor_elements "four elements" [ 1; 2; 3; 4 ];
      t "safe flatten" test_flatten;
      t "safe append" test_append;
      t "safe fold_right" test_fold_right;
      t "combine" test_combine;
      t "split" test_split;
      t "span" test_span;
      t "uniq_by" test_uniq_by;
      t "deduplicate" test_deduplicate;
      t "deduplicate_gen" test_deduplicate_gen;
    ]
